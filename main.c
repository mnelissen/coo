#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
typedef uint32_t dev_t;
typedef uint64_t ino_t;
typedef uint32_t pid_t;
#define DIRSEP '\\'
#define getpid() GetProcessId()
#else
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#define DIRSEP '/'
#endif
#include "hash.h"
#include "hasho.h"

#define NEWFN_MAX          8192            /* (stack)size for included dirs+filenames */
#define OUTPR_MAX          256             /* maximum outprintf size we do */
#define STDIN_BUFSIZE      (1024 * 1024)   /* size for inputbuffer when using stdin */
#define MAX_PAREN_LEVELS   16
#define DEF_COO_INLINE \
	"#ifndef coo_inline\n" \
	"#ifdef _MSC_VER\n" \
	"#define coo_inline __forceinline\n" \
	"#else\n" \
	"#define coo_inline extern inline " \
		"__attribute__((always_inline)) __attribute__((gnu_inline))\n" \
	"#endif\n" \
	"#endif\n"

static int compare_prefix_strhash(void *user, void *key);

#define DEFINE_PTRHASH(c, h, nm, itemtype) \
	static struct itemtype *find_##nm(struct c *c, struct c *ptr) \
	{ return hash_find(&c->h, ptrhash(ptr), &ptr); } \
	static int init_##nm##_hash(struct c *c, unsigned tblsize) \
	{ return hash_init(&c->h, compare_ptrs, tblsize, \
		offsetof(struct itemtype, node), offsetof(struct itemtype, c)); } \
	static int insert_##nm(struct c *c, struct itemtype *item) \
	{ return hash_insert(&c->h, &item->node, ptrhash(item->c)); }

#define DEFINE_STRHASH_FIND(c, h, nm, itemtype) \
	static struct itemtype *find_##nm(struct c *c, char *name) \
	{ return hash_find_custom(&c->h, strhash(name), (hash_cmp_cb)strcmp, name); }
#define DEFINE_STRHASH_FIND_E(c, h, nm, itemtype) \
	static struct itemtype *find_##nm##_e(struct c *c, char *name, char *end) \
	{ return hash_find_custom(&c->h, memhash(name, end-name), \
		compare_prefix_strhash, name); }
#define strhash_init(h, tblsize, itemtype) \
	hash_init(h, (hash_cmp_cb)strcmp, tblsize, \
		offsetof(struct itemtype, node), offsetof(struct itemtype, name))
#define strhash_insert(h, item) \
	hash_insert(h, &(item)->node, strhash((item)->name))

struct file_id {
	dev_t dev_id;
	ino_t file_id;
	uint64_t mtime;
	struct hash_entry node;
};

enum parse_state {
	FINDVAR, STMTSTART, DECLVAR, ACCESSMEMBER, EXPRUNARY
};

struct dynarr {
	void **mem;
	unsigned num;
	unsigned max;
};

struct memberprops {
	unsigned char is_virtual:1;
	unsigned char is_override:1;
	unsigned char is_function:1;
	unsigned char is_static:1;
	unsigned char from_primary:1;  /* inherited from primary base class? */
	unsigned char from_virtual:1;  /* a virtual inheritance in chain? */
	unsigned char seen:1;          /* seen implementation? */
};

struct vmt;

struct class {
	struct hash members;
	struct dynarr members_arr;
	struct hasho ancestors;      /* class => ancestor, all classes inherited from */
	struct dynarr vmts;          /* struct vmt *, all applicable vmts */
	struct dynarr descendants;   /* struct parent *, inheriting from this class */
	struct vmt *vmt;             /* from primary parent (or new here)  */
	unsigned is_interface:1;
	unsigned write_vmt:1;        /* have seen virtual func implementation */
	struct hash_entry node;
	char name[];
};

struct classptr {
	struct class *class;    /* class for this declaration */
	int pointerlevel;       /* pointerlevel for this declaration */
};

struct parent {
	struct class *class;       /* parent class */
	struct class *child;       /* inheriting class */
	unsigned char is_primary;
	unsigned char is_virtual;
	unsigned char need_vmt;    /* has own vmt or overrides parent's vmt */
};

struct ancestor {   /* link to parent, or parent of parent, etc.. */
	struct parent *parent;
	unsigned from_virtual;
	char path[];   /* full path to ancestor */
};

struct vmt {
	struct class *origin;        /* where this vmt is first defined */
	unsigned char modified:1;    /* any method overriden by class */
	unsigned char is_primary:1;  /* is this the primary vmt for this class? */
	char name[];                 /* vmt struct type and variable name */
};

struct member {
	char *rettype;
	struct classptr retclassptr;
	struct class *origin;      /* origin class where member was defined */
	struct class *definition;  /* closest class virtual member overridden */
	struct vmt *vmt;           /* vmt this member is defined in */
	char *paramstext;
	struct dynarr params;
	char *funcinsert;   /* insert this string in front of function call */
	  /* for normal function:  func() ==> class_func()
	     for virtual function: func() ==> class_vmt_func() */
	char *parentname;   /* insert after function first argument expression */
	  /* for a function: x->func() ==> x_func(&x.parent) */
	char *nameinsert;   /* this member is inherited, insert this in front */
	  /* for this class: NULL or empty string
	     for inherited: field1 ==> parent.field1 */
	struct memberprops props;
	char duplicate_pr;  /* to prevent spam, already printed duplicated message */
	char parent_virtual;  /* member inherited from a virtual base class */
	struct hash_entry node;
	char name[];
};

DEFINE_STRHASH_FIND_E(class, members, member, member)

struct variable {
	struct classptr decl;
	struct dynarr params;   /* struct classptr, for function pointer variables */
	unsigned blocklevel;
	struct hash_entry node;
	char name[];
};

struct classtype {
	struct classptr decl;
	struct hash_entry node;
	char name[];
};

struct insert {   /* an insert, to insert some text in the output */
	char *flush_until;   /* flush output until here (expression start) */
	char *insert_text;   /* text to insert: e.g. 'this->' or 'vt->' */
	char *continue_at;   /* where to continue writing (perhaps skip something) */
};

struct parse_file {
	FILE *out;                /* file writing to */
	char *filename;           /* input filename */
	char *buffer;             /* input buffer */
	char *writepos;           /* input buffer written to output till here */
	char *pos;                /* parsed input buffer till here */
	char *outbuffer;          /* comparing output buffer */
	char *outpos;             /* compared output until here, NULL if writing */
	char *outfilename;        /* output filename */
	char *outfilename_end;    /* pointer to null-character of output filename */
	char *newfilename_path;   /* points to start of path (last in stack) */
	char *newfilename_file;   /* points after parsing file dir in newfilename */
	int coo_inline_defined;   /* defined the COO_INLINE macro yet? */
	int outfailed;            /* prevent error spam if creating file failed */
};

struct parser {
	struct parse_file pf;     /* parse state for currently parsing file */
	char *include_ext_in;     /* parse only include files with this ext */
	char *source_ext_out;     /* write source output files with this ext */
	char *header_ext_out;     /* write header output files with this ext */
	struct hash classes;          /* struct class pointers */
	struct hash classtypes;       /* struct classtype pointers */
	struct hash globals;          /* struct variable pointers */
	struct hash locals;           /* struct variable pointers */
	struct dynarr nested_locals;  /* struct variable pointers */
	struct dynarr inserts;        /* struct insert pointers */
	struct dynarr includepaths;   /* char pointers */
	struct dynarr file_stack;     /* struct parse_file pointers */
	struct dynarr param_classes;  /* struct classptr *, collect params of funcs */
	struct hash files_seen;       /* struct file_id pointers */
	int include_ext_in_len;       /* length of include_ext_in, optimization */
	char printbuffer[OUTPR_MAX];  /* buffer for outprintf */
	char newfilename[NEWFN_MAX];  /* (stacked) store curdir + new input filename */
	char *newfilename_end;        /* pointer to end of new input filename */
};

DEFINE_STRHASH_FIND(parser, classes, class, class)
DEFINE_STRHASH_FIND_E(parser, classes, class, class)
DEFINE_STRHASH_FIND_E(parser, classtypes, classtype, classtype)
DEFINE_STRHASH_FIND_E(parser, globals, global, variable)
DEFINE_STRHASH_FIND_E(parser, locals, local, variable)

pid_t g_pid;

static char *strchrnul(const char *str, int ch)
{
	while (*str && *str != ch)
		str++;
	return (char*)str;
}

static int islinespace(int ch)
{
	return ch == ' ' || ch == '\t';
}

/* assumes pos[0] == '/' was matched, starting a possible comment */
static char *skip_comment(char *pos)
{
	/* C style comment? */
	if (pos[1] == '*') {
		for (pos += 2;; pos++) {
			pos = strchrnul(pos, '*');
			if (pos[0] == 0)
				return pos;
			if (pos[1] == '/')
				break;
		}
		return pos + 2;
	}
	/* C++ style comment? */
	if (pos[1] == '/') {
		pos = strchrnul(pos+2, '\n');
		if (pos[0] == 0)
			return pos;
		return pos + 1;
	}
	return pos;
}

static char *skip_whitespace(char *p)
{
	for (;;) {
		if (isspace(*p))
			p++;
		else if (*p == '/')
			p = skip_comment(p);
		else if (*p == '"') {
			for (;;) {
				p = strchrnul(p + 1, '"');
				if (*p == 0)
					return p;
				if (*(p-1) != '\\')
					break;
			}

			p++;
		} else if (*p == '\'') {
			p++;
			if (*p == '\\')
				p++;
			p++;
			if (*p == '\'')
				p++;
		} else
			break;
	}
	return p;
}

/* skip whitespace and comments for this line only */
static char *skip_whiteline(char *p)
{
	for (;;) {
		if (islinespace(*p))
			p++;
		else if (*p == '/')
			p = skip_comment(p);
		else
			break;
	}
	/* skip one line ending, not all */
	if (*p == '\r')
		p++;
	if (*p == '\n')
		p++;
	return p;
}

static char *skip_word(char *p)
{
	while (isalnum(*p) || *p == '_')
		p++;
	return p;
}

char *strprefixcmp(const char *s1, const char *s2)
{
	for (;; s1++, s2++) {
		if (*s1 == 0)
			return (char*)s2;
		if (*s1 != *s2)
			return NULL;
	}
}

static int compare_prefix_strhash(void *user, void *key)
{
	return strprefixcmp(user, key) == NULL;
}

static char *stmecpy(char *dest, char *end, const char *src, const char *src_end)
{
	size_t len, maxlen;

	if (src == NULL) {
		*dest = 0;
		return dest;
	}
	len = src_end - src;
	maxlen = end - dest - 1;
	if (len > maxlen)
		len = maxlen;
	memcpy(dest, src, len);
	dest[len] = 0;
	return dest + len;
}

static char *stmcpy(char *dest, char *end, const char *src)
{
	return stmecpy(dest, end, src, src ? src + strlen(src) : NULL);
}

/* like strcpy, but return error if string full */
#define stfcpy(d,s) ((stmcpy(d,&d[sizeof(d)],s) == &d[sizeof(d)-1]) ? -1 : 0)
/* like stfcpy, but outputs pointer to end null-terminator in 'e' */
#define stfcpy_e(d,s,e) ((*(e) = stmcpy(d,&d[sizeof(d)],s), *(e) == &d[sizeof(d)-1]) ? -1 : 0)

static char *stredupto(char *dest, const char *src, const char *until)
{
	int len = until-src;
	char *newdest = realloc(dest, len+1);
	if (newdest == NULL) {
		free(dest);
		return NULL;
	}
	memcpy(newdest, src, len);
	newdest[len] = 0;
	return newdest;
}

static char *strdupto(char *dest, const char *src)
{
	return stredupto(dest, src, src + strlen(src));
}

static char *stredup(const char *src, const char *until)
{
	return stredupto(NULL, src, until);
}

static char *replace_ext_temp(char *filename, char *strend, char *bufend, char *new_ext)
{
	char *ext, *pidend;

	/* find last '.' */
	for (ext = strend;;) {
		/* if reached begin of string, paste extension to end of filename */
		if (ext == filename) {
			ext = strend;
			break;
		}
		ext--;
		if (*ext == '.')
			break;
	}
	ext = stmcpy(ext, bufend, new_ext);
	if (ext+1 == bufend)
		return NULL;
	/* prepare temporary filename already in this buffer */
	pidend = ext + snprintf(ext, bufend-ext, ".%u", g_pid);
	if (pidend >= bufend)
		return NULL;
	return ext;
}

/* scan for character set, ignoring comments, include '/' in set!
   if '/' is first in set, then skip '/' as a token */
static char *scan_token(char *pos, char *set)
{
	char *newpos;

	for (;;) {
		pos = strpbrk(pos, set);
		if (pos == NULL)
			return NULL;
		if (pos[0] != '/')
			return pos;
		newpos = skip_comment(pos);
		if (newpos[0] == 0)
			return NULL;
		if (newpos == pos && set[0] == '/')
			newpos++;
		pos = newpos;
	}
}

__attribute__((format(printf,1,2))) static char *aprintf(const char *format, ...)
{
	int size;
	va_list va_args;
	char *buffer;

	va_start(va_args, format);
	size = 1 + vsnprintf(NULL, 0, format, va_args);
	va_end(va_args);
	buffer = malloc(size);
	if (buffer == NULL)
		return NULL;

	va_start(va_args, format);
	vsnprintf(buffer, size, format, va_args);
	va_end(va_args);
	return buffer;
}

#ifdef _WIN32

static int get_file_id(FILE *fp, struct file_id *out_id)
{
	BY_HANDLE_FILE_INFORMATION file_info;

	if (!GetFileInformationByHandle(fileno(hFile), &file_info))
		return -1;

	out_id->dev_id = file_info.dwVolumeSerialNumber;
	out_id->file_id = ((uint64_t)file_info.nFileIndexHigh << 32)
			| ((uint64_t)file_info.nFileIndexLow);
	out_id->mtime = ((uint64_t)file_info.ftLastWriteTime.dwHighDateTime << 32)
			| ((uint64_t)file_info.ftLastWriteTime.dwLowDateTime);
	return 0;
}

static size_t file_size(FILE *fp)
{
	uint32_t sizeLow, sizeHigh;

	/* might fail on e.g. pipe, then use default buffer size */
	sizeLow = GetFileSize(fileno(fp), &sizeHigh);
	if (sizeLow == INVALID_FILE_SIZE && GetLastError() != NO_ERROR)
		return STDIN_BUFSIZE;
	return sizeLow | ((uint64_t)sizeHigh << 32);
}

#else

static int get_file_id(FILE *fp, struct file_id *out_id)
{
	struct stat stat;

	if (fstat(fileno(fp), &stat) < 0)
		return -1;

	out_id->dev_id = stat.st_dev;
	out_id->file_id = stat.st_ino;
	out_id->mtime = stat.st_mtim.tv_sec * 1000000ull + stat.st_mtim.tv_nsec;
	return 0;
}

static size_t file_size(FILE *fp)
{
	struct stat stat;

	/* size unknown on e.g. pipe, then use default buffer size */
	if (fstat(fileno(fp), &stat) < 0 || !S_ISREG(stat.st_mode))
		return STDIN_BUFSIZE;
	return stat.st_size;
}

#endif

static int compare_file_ids(void *a, void *b)
{
	struct file_id *file_id_a = a, *file_id_b = b;
	return file_id_a->dev_id != file_id_b->dev_id
		|| file_id_a->file_id != file_id_b->file_id;
}

static void *read_file_until(FILE *fp, char *buffer, size_t size)
{
	buffer = realloc(buffer, size+1);
	if (buffer == NULL) {
		fprintf(stderr, "No memory for file buffer\n");
		return NULL;
	}

	size = fread(buffer, 1, size, fp);
	buffer[size] = 0;
	return buffer;
}

static void *read_file(FILE *fp, char *buffer)
{
	return read_file_until(fp, buffer, file_size(fp));
}

/*** dynamic array ***/

static int grow_dynarr_to(struct dynarr *dynarr, int minimum)
{
	if (minimum >= dynarr->max) {
		unsigned newmax = minimum > dynarr->max ? minimum : dynarr->max;
		newmax = newmax >= 16 ? newmax * 2 : 16;
		void **newmem = realloc(dynarr->mem, newmax * sizeof(void*));
		if (newmem == NULL)
			return -1;
		memset(&newmem[dynarr->max], 0, (newmax - dynarr->max) * sizeof(void*));
		dynarr->mem = newmem;
		dynarr->max = newmax;
	}

	return 0;
}

static int grow_dynarr(struct dynarr *dynarr)
{
	return grow_dynarr_to(dynarr, dynarr->num);
}

typedef int (*compare_cb)(const void *key, const void *item);

/*** parser helpers ***/

static void *realloc_namestruct(void *ptr, size_t structsize, char *name, char *nameend)
{
	size_t len = nameend-name, allocsize = structsize + len + 1;
	char *ret, *dest;

	/* if never allocated yet, zero-initialize */
	if (ptr)
		ret = realloc(ptr, allocsize);
	else
		ret = calloc(1, allocsize);
	if (ret == NULL)
		return NULL;

	dest = ret + structsize;
	memcpy(dest, name, len);
	dest[len] = 0;
	return ret;
}

#define alloc_namestruct(s,n,e) realloc_namestruct(NULL, offsetof(s, name), n, e)

static struct class *addclass(struct parser *parser, char *classname, char *nameend)
{
	struct class *class;

	class = alloc_namestruct(struct class, classname, nameend);
	if (class == NULL)
		return NULL;

	strhash_init(&class->members, 8, member);
	hasho_init(&class->ancestors, 8);
	if (hash_insert(&parser->classes, &class->node, strhash(class->name)) < 0) {
		/* already exists */
		free(class);
		class = NULL;
	}
	return class;
}

static struct classtype *addclasstype(struct parser *parser,
		char *typename, char *nameend, struct classptr *decl)
{
	struct classtype *type;

	type = alloc_namestruct(struct classtype, typename, nameend);
	if (type == NULL)
		return NULL;

	type->decl = *decl;
	if (strhash_insert(&parser->classtypes, type) < 0) {
		/* already exists */
		free(type);
		type = NULL;
	}
	return type;
}

typedef void (*new_param_cb)(struct parser *parser, int position,
		struct classptr *decl, char *name, char *next);

static char *parse_parameters(struct parser *parser, char *params, new_param_cb new_param)
{
	struct classtype *classtype;
	struct classptr decl;
	char *next, *curr, *name;
	enum parse_state state;
	int position;

	decl.pointerlevel = 0;
	state = FINDVAR;
	for (position = 0, next = params;;) {
		curr = skip_whitespace(next);
		if (curr[0] == 0)
			return NULL;
		if (strprefixcmp("const ", curr)) {
			next = curr + 6;  /* "const " */
			continue;
		} else if (strprefixcmp("struct ", curr)) {
			name = skip_whitespace(curr + 7);  /* "struct " */
			next = skip_word(name);
			if (state == DECLVAR) {
				/* grammar error, ignore */
				state = FINDVAR;
			} else if ((decl.class = find_class_e(parser, name, next)) != NULL) {
				state = DECLVAR;
			}
			continue;
		} else if (*curr == ',') {
			state = FINDVAR;
			next = curr + 1;
			position++;
			decl.pointerlevel = 0;
			continue;
		} else if (*curr == ')') {
			return curr + 1;
		} else if (!isalpha(*curr)) {
			if (*curr == '*')
				decl.pointerlevel++;
			next = curr + 1;
			continue;
		}

		name = curr;
		next = skip_word(name);
		switch (state) {
		case DECLVAR:
			new_param(parser, position, &decl, name, next);
			state = FINDVAR;
			break;
		default:
			if ((classtype = find_classtype_e(parser, name, next)) != NULL) {
				decl = classtype->decl;
				state = DECLVAR;
			}
			break;
		}
	}
}

static int insertmember(struct class *class, struct member *member)
{
	if (grow_dynarr(&class->members_arr) < 0)
		return -1;
	/* check already exists */
	if (strhash_insert(&class->members, member) < 0)
		return -1;

	class->members_arr.mem[class->members_arr.num] = member;
	class->members_arr.num++;
	return 0;
}

static struct member *allocmember_e(struct class *class, char *membername, char *nameend)
{
	struct member *member;

	member = alloc_namestruct(struct member, membername, nameend);
	if (member == NULL)
		return NULL;
	if (insertmember(class, member) < 0)
		goto err;
	return member;
err:
	free(member);
	return NULL;
}

static struct member *dupmemberto(struct class *class, struct member *parentmember)
{
	int size = offsetof(struct member, name) + strlen(parentmember->name) + 1;
	struct member *member;

	member = malloc(size);
	if (member == NULL)
		return NULL;

	memcpy(member, parentmember, size);
	if (insertmember(class, member) < 0)
		goto err;

	return member;
err:
	free(member);
	return NULL;
}

static void save_param_class(struct parser *parser, int position,
		struct classptr *decl, char *name, char *next)
{
	struct dynarr *dynarr = &parser->param_classes;
	struct classptr *newclassptr;

	if (grow_dynarr_to(dynarr, position) < 0)
		return;

	newclassptr = malloc(sizeof(*newclassptr));
	if (newclassptr == NULL)
		return;

	*newclassptr = *decl;
	dynarr->mem[position] = newclassptr;
	dynarr->num = position+1;
}

static void addmember_to_children(struct class *class, struct member *member);

static void parse_parameters_to(struct parser *parser,
	char *params, struct dynarr *param_classes)
{
	/* save_param_class stores params in parser->param_classes */
	parse_parameters(parser, params, save_param_class);
	/* move the dynarr (array pointer etc) to requested array */
	memcpy(param_classes, &parser->param_classes, sizeof(*param_classes));
	memset(&parser->param_classes, 0, sizeof(parser->param_classes));
}

static struct member *addmember(struct parser *parser, struct class *class,
		struct classptr *retclassptr, char *rettype, char *retend,
		char *membername, char *nameend, char *params, char *paramend,
		struct memberprops props)
{
	struct member *member;
	char *vmt_insert;

	member = allocmember_e(class, membername, nameend);
	if (member == NULL) {
		*nameend = 0;
		fprintf(stderr, "duplicate member %s, did you mean override?\n", membername);
		return NULL;
	}

	member->origin = class;
	member->definition = class;
	member->props = props;
	member->retclassptr = *retclassptr;
	member->rettype = stredup(rettype, retend);
	if (paramend >= params) {
		member->paramstext = stredup(params, paramend);
		parse_parameters_to(parser, params, &member->params);
	}
	if (props.is_function || props.is_static) {
		vmt_insert = props.is_virtual ? "vmt_" : "";
		member->funcinsert = aprintf("%s_%s", class->name, vmt_insert);
	}
	if (props.is_virtual) {
		member->vmt = class->vmt;
		class->vmt->modified = 1;
	}

	/* in case of defining functions on the fly,
	   insert into classes inheriting from this class as well */
	addmember_to_children(class, member);
	return member;
}

static struct variable *addvariable(struct parser *parser, unsigned blocklevel,
		struct classptr *decl, int extraptr, char *membername, char *nameend,
		char *params, char *paramsend)
{
	struct dynarr *dynarr = &parser->nested_locals;
	struct variable *variable;

	if (grow_dynarr(dynarr) < 0)
		return NULL;

	variable = realloc_namestruct(dynarr->mem[dynarr->num],
		offsetof(struct variable, name), membername, nameend);
	if (variable == NULL)
		return NULL;

	dynarr->mem[dynarr->num] = variable;
	if (strhash_insert(&parser->locals, variable) < 0) {
		/* no need to free variable, ref stored in dynarr */
		return NULL;
	}

	variable->decl = *decl;
	variable->decl.pointerlevel += extraptr;  /* typedef type + extra pointers */
	variable->blocklevel = blocklevel;
	if (params) {
		parse_parameters(parser, params, save_param_class);
		memcpy(&variable->params, &parser->param_classes, sizeof(variable->params));
	}
	dynarr->num++;
	return variable;
}

static struct insert **addinsert(struct parser *parser, struct insert **after_pos,
		char *flush_until, char *insert_text, char *continue_at)
{
	struct insert *insert, **end_pos;

	if (grow_dynarr(&parser->inserts) < 0)
		return NULL;

	end_pos = (struct insert **)&parser->inserts.mem[parser->inserts.num];
	insert = *end_pos;
	if (!insert) {
		insert = calloc(1, sizeof(*insert));
		if (insert == NULL)
			return NULL;
		*end_pos = insert;
	}

	insert->flush_until = flush_until;
	insert->insert_text = insert_text;
	insert->continue_at = continue_at;
	parser->inserts.num++;
	if (!after_pos) {
		/* check flush ordering to be incremental */
		for (after_pos = end_pos - 1;; after_pos--) {
			if ((void**)after_pos < parser->inserts.mem)
				break;
			/* add new inserts after existing (so stop if equal) */
			if ((*after_pos)->flush_until <= flush_until)
				break;
		}
	}

	after_pos++;
	if (after_pos < end_pos) {
		memmove(after_pos+1, after_pos, (char*)end_pos-(char*)after_pos);
		*after_pos = insert;
	}
	return after_pos;
}

static int addincludepath(struct parser *parser, char *path)
{
	if (grow_dynarr(&parser->includepaths) < 0)
		return -1;

	/* pointer to commandline so do not have to copy */
	parser->includepaths.mem[parser->includepaths.num++] = path;
	return 0;
}

static struct parse_file *addfilestack(struct parser *parser)
{
	struct dynarr *dynarr = &parser->file_stack;
	struct parse_file *parse_file;

	if (grow_dynarr(dynarr) < 0)
		return NULL;

	parse_file = dynarr->mem[dynarr->num];
	if (!parse_file) {
		parse_file = calloc(1, sizeof(*parse_file));
		if (parse_file == NULL)
			return NULL;
		dynarr->mem[dynarr->num] = parse_file;
	}

	dynarr->num++;
	return parse_file;
}

static void remove_locals(struct parser *parser, int blocklevel)
{
	struct variable *var;
	unsigned j;

	for (j = parser->nested_locals.num; j > 0;) {
		var = parser->nested_locals.mem[--j];
		if (var->blocklevel < blocklevel)
			break;
	}

	parser->nested_locals.num = j;
}

static int find_global_e_class(struct parser *parser, char *name, char *nameend,
		struct classptr *ret_decl, struct dynarr **retparams)
{
	struct variable *var = find_global_e(parser, name, nameend);
	if (var == NULL)
		return -1;
	*ret_decl = var->decl;
	/* TODO: parse function parameters for variables (params always empty now) */
	*retparams = &var->params;
	return 0;
}

static int find_local_e_class(struct parser *parser, char *name, char *nameend,
		struct classptr *ret_decl, struct dynarr **retparams)
{
	struct variable *var = find_local_e(parser, name, nameend);
	if (var == NULL)
		return -1;
	*ret_decl = var->decl;
	*retparams = &var->params;
	return 0;
}

static int prepare_output_file(struct parser *parser)
{
	/* outfilename already has the temporary extension in buffer, but has
	 * null character in place of '.<pid>' to read output filename first */
	*parser->pf.outfilename_end = '.';
	parser->pf.out = fopen(parser->pf.outfilename, "wb");
	if (parser->pf.out == NULL) {
		fprintf(stderr, "%s: could not create\n", parser->pf.outfilename);
		parser->pf.outfailed = 1;
		return -1;
	}

	/* write everything that already did compare OK */
	fwrite(parser->pf.outbuffer, 1, parser->pf.outpos - parser->pf.outbuffer, parser->pf.out);
	return 0;
}

static void outwrite(struct parser *parser, const void *buffer, size_t size)
{
	/* if in comparing mode, compare instead of write */
	if (parser->pf.out == NULL) {
		/* do not keep complaining */
		if (parser->pf.outfailed)
			return;
		if (parser->pf.outpos && memcmp(parser->pf.outpos, buffer, size) == 0) {
			parser->pf.outpos += size;
			return;
		}
		/* input changed compared to output, start writing */
		if (prepare_output_file(parser) < 0)
			return;
	}

	fwrite(buffer, 1, size, parser->pf.out);
}

static void outputs(struct parser *parser, const char *src)
{
	return outwrite(parser, src, strlen(src));
}

static void outprintf(struct parser *parser, const char *format, ...)
{
	va_list va_args;
	unsigned size;

	va_start(va_args, format);
	size = vsnprintf(parser->printbuffer, sizeof(parser->printbuffer), format, va_args);
	va_end(va_args);
	if (size >= sizeof(parser->printbuffer)) {
		fprintf(stderr, "error: printbuffer too small\n");
		size = sizeof(parser->printbuffer)-1;
	}

	outwrite(parser, parser->printbuffer, size);
}

static void flush_until(struct parser *parser, char *until)
{
	size_t size = until - parser->pf.writepos;

	if (parser->pf.writepos > until) {
		fprintf(stderr, "internal error, flushing into past\n");
		return;
	}

	return outwrite(parser, parser->pf.writepos, size);
	/* update writepos done in caller, usually want to skip something anyway */
}

static void flush(struct parser *parser)
{
	flush_until(parser, parser->pf.pos);
	parser->pf.writepos = parser->pf.pos;
}

struct classptr parse_type(struct parser *parser, char *pos, char **retnext)
{
	struct classptr decl;
	struct classtype *classtype;
	char *name, *next;

	for (decl.class = NULL;; pos = skip_whitespace(pos)) {
		if (strprefixcmp("const ", pos) != NULL) {
			pos += 6;  /* const */
			continue;
		}
		/* not necessary to parse unsigned/signed/int properly,
		   (1) retnext is only used to parse typedef
		   (1b) retnext usage is only interested in class types
		   (2) use context like '(' and ';' to find member names */
		if (strprefixcmp("struct ", pos)) {
			name = skip_whitespace(pos + 7);  /* "struct " */
			next = skip_word(name);
			decl.class = find_class_e(parser, pos, next);
			decl.pointerlevel = 0;
			break;
		} else {
			if (isalpha(*pos)) {
				next = skip_word(pos);
				if ((classtype = find_classtype_e(parser, pos, next)) != NULL)
					decl = classtype->decl;
			}
			break;
		}
	}

	for (next = skip_whitespace(next); *next == '*'; next++)
		decl.pointerlevel++;
	*retnext = next;
	return decl;
}

static struct member *inheritmember(struct class *class,
		struct parent *parent, struct member *parentmember)
{
	struct member *member;

	member = dupmemberto(class, parentmember);
	if (member == NULL) {
		fprintf(stderr, "duplicate member %s from parent "
			"class %s to %s\n", parentmember->name,
			parentmember->origin->name, class->name);
		goto err;
	}

	/* only primary if this and inherited are all primary */
	member->props.from_primary &= parent->is_primary;
	member->props.from_virtual |= parent->is_virtual;
	if (parentmember->parentname) {
		member->parentname = aprintf("%s%s%s", parent->class->name,
			parent->is_virtual ? "->" : ".",
			parentmember->parentname);
	} else {
		member->parentname = parent->class->name;
		member->parent_virtual = parent->is_virtual;
	}
	member->nameinsert = aprintf("%s%s", member->parentname,
				member->parent_virtual ? "->" : ".");
	/* in case of defining functions on the fly,
	   insert into classes inheriting from this class as well */
	addmember_to_children(class, member);
	return member;
err:
	free(member);
	return NULL;
}

static void addmember_to_children(struct class *class, struct member *member)
{
	struct parent *parent;
	int i;

	for (i = 0; i < class->descendants.num; i++) {
		parent = class->descendants.mem[i];
		inheritmember(parent->child, parent, member);
	}
}

/* is origin a virtual base of provided class?
   -1: not a base of class
    0: is a base of class, but not virtual
    1: is a virtual base of class */
static int is_virtual_base(struct class *origin, struct class *class)
{
	struct ancestor *ancestor;

	ancestor = hasho_find(&class->ancestors, origin);
	if (ancestor == NULL)
		return -1;
	return ancestor->from_virtual;
}

static void add_ancestor(struct class *class, int from_virtual,
		struct parent *origin_parent,
		char *via_class_name, char *link, char *path)
{
	struct ancestor *ancestor;

	/* allocate struct and printf path in one alloc */
	ancestor = (void*)aprintf("%*s%s%s%s", (int)offsetof(struct ancestor, path),
			"", via_class_name, link, path);
	if (ancestor == NULL)
		return;

	ancestor->parent = origin_parent;
	ancestor->from_virtual = from_virtual;
	/* duplicates may occur for virtual bases */
	if (hasho_insert(&class->ancestors, origin_parent->class, ancestor) < 0)
		free(ancestor);
}

static void add_ancestors(struct class *class, struct parent *parent)
{
	struct class *parentclass = parent->class;
	struct hasho_entry *ancestor_entry;
	struct ancestor *ancestor;
	int from_virtual;

	add_ancestor(class, parent->is_virtual, parent, parentclass->name, "", "");
	hasho_foreach(ancestor_entry, &parentclass->ancestors) {
		ancestor = ancestor_entry->value;
		from_virtual = ancestor->from_virtual | parent->is_virtual;
		add_ancestor(class, from_virtual, ancestor->parent,
			parentclass->name, parent->is_virtual ? "->" : ".", ancestor->path);
	}
}

enum primary_vmt { SECONDARY_VMT, PRIMARY_VMT };  /* boolean compatible */

static void addvmt(struct class *class, struct class *origin, enum primary_vmt primary_vmt)
{
	struct vmt *vmt;
	char *vmt_sec_name, *vmt_sec_sep;

	if (grow_dynarr(&class->vmts) < 0)
		return;

	/* secondary vmt? append origin class name to distiniguish from primary */
	if (primary_vmt != PRIMARY_VMT) {
		vmt_sec_name = origin->name;
		vmt_sec_sep = "_";
	} else
		vmt_sec_name = vmt_sec_sep = "";
	/* trick: use padded string to allocate struct part */
	vmt = (void*)aprintf("%*s%s%s%s_vmt", (int)offsetof(struct vmt, name), "",
			class->name, vmt_sec_sep, vmt_sec_name);
	/* no need to zero vmt struct, assign all members initial value */
	vmt->origin = origin;
	vmt->modified = class == origin;
	vmt->is_primary = primary_vmt == PRIMARY_VMT;
	class->vmts.mem[class->vmts.num++] = vmt;
	if (primary_vmt == PRIMARY_VMT)
		class->vmt = vmt;
}

static void import_parent(struct class *class, struct parent *parent)
{
	struct member *parentmember;
	struct dynarr *parentmembers;
	struct class *origin, *currorigin, *ignoreorigin;
	struct class *parentclass = parent->class;
	struct vmt *vmt;
	int i, ret;

	parentmembers = &parentclass->members_arr;
	currorigin = ignoreorigin = NULL;
	for (i = 0; i < parentmembers->num; i++) {
		parentmember = parentmembers->mem[i];
		origin = parentmember->origin;
		if (origin == ignoreorigin)
			continue;
		if (origin != currorigin) {
			currorigin = origin;
			/* if we already imported this class virtual, or
			   this import is virtual, then don't inherit
			   the members again (prevent duplicates) */
			ret = is_virtual_base(origin, class);
			if (ret == 1 || (ret == 0 && parentmember->props.from_virtual)) {
				ignoreorigin = origin;
				continue;
			}
			/* on the other hand, might both be non-virtual */
			if (ret == 0 && !parentmember->props.from_virtual) {
				fprintf(stderr, "inherit duplicate class %s\n", origin->name);
				ignoreorigin = origin;
			}
		}

		inheritmember(class, parent, parentmember);
		/* write to output later, when opening brace '{' parsed */
	}

	/* only add class after origin checks */
	if (grow_dynarr(&parentclass->descendants) < 0)
		return;

	/* inherit vmts */
	for (i = 0; i < parentclass->vmts.num; i++) {
		vmt = parentclass->vmts.mem[i];
		/* if we already have this origin, then also its vmt: skip it */
		if (hasho_find(&class->ancestors, vmt->origin) != NULL)
			continue;

		/* do not reuse virtual base class' vmt, is inefficient */
		addvmt(class, vmt->origin,
			class->vmt == NULL && vmt->is_primary && !parent->is_virtual);
	}

	/* add parents after vmt duplication check */
	add_ancestors(class, parent);
	parentclass->descendants.mem[parentclass->descendants.num++] = parent;
}

static struct member *implmember(struct class *class, char *membername, char *nameend)
{
	struct class *vmt_origin;
	struct member *member;
	struct vmt *vmt;
	unsigned i;

	member = find_member_e(class, membername, nameend);
	if (member == NULL) {
		fprintf(stderr, "cannot find member to override\n");
		return NULL;
	}

	if (!member->props.is_virtual) {
		fprintf(stderr, "inherited member is not virtual\n");
		return NULL;
	}

	member->definition = class;
	vmt_origin = member->vmt->origin;
	/* find same vmt in this class (same origin) */
	for (i = 0;; i++) {
		if (i == class->vmts.num) {
			fprintf(stderr, "internal error, vmt for %s not found\n", member->name);
			break;
		}
		vmt = class->vmts.mem[i];
		if (vmt->origin == vmt_origin) {
			member->vmt = vmt;
			vmt->modified = 1;
			break;
		}
	}

	return member;
}

static void print_vmt_type(struct parser *parser, struct class *class)
{
	struct member *member;
	struct vmt *vmt;
	int i, j;

	for (i = 0; i < class->vmts.num; i++) {
		vmt = class->vmts.mem[i];
		if (!vmt->modified)
			continue;

		outprintf(parser, "\nextern struct %s {\n", vmt->name);
		for (j = 0; j < class->members_arr.num; j++) {
			member = class->members_arr.mem[j];
			if (member->vmt == NULL)
				continue;
			if (member->vmt->origin != vmt->origin)
				continue;

			outprintf(parser, "\t%s(*%s)(struct %s *%s%s%s;\n",
				member->rettype, member->name,
				member->definition->name, member->definition->name,
				member->paramstext[0] != ')' ? ", " : "",
				member->paramstext);
		}
		outprintf(parser, "} %s;\n", vmt->name);
	}
}

enum func_decltype {
	MEMBER_FUNCTION,
	VIRTUAL_WRAPPER,
};

static void print_func_decl(struct parser *parser, struct class *class,
		struct member *member, enum func_decltype emittype)
{
	char *func_prefix, *name_insert, *func_body;
	char *p, *last_word, *last_end, *vmtpath, *vmtaccess;
	struct ancestor *ancestor;
	unsigned empty;

	empty = member->paramstext[0] == ')';
	if (emittype == VIRTUAL_WRAPPER) {
		func_prefix = "coo_inline ";
		name_insert = "vmt_";
		func_body = "\n{";
	} else {
		func_prefix = name_insert = "";
		func_body = ";";   /* no body */
	}
	outprintf(parser, "\n%s%s%s_%s%s(struct %s *this%s%s%s", func_prefix,
		member->rettype, class->name, name_insert, member->name,
		class->name, empty ? "" : ", ", member->paramstext, func_body);
	if (emittype == VIRTUAL_WRAPPER) {
		ancestor = hasho_find(&class->ancestors, class->vmt->origin);
		if (ancestor) {
			vmtpath = ancestor->path;
			vmtaccess = ancestor->parent->is_virtual ? "->" : ".";
		} else
			vmtpath = vmtaccess = "";
		outprintf(parser, "\n\t((struct %s_vmt*)this->%s%svmt)->%s(this",
			class->name, vmtpath, vmtaccess, member->name);
		for (p = member->paramstext;;) {
			last_word = NULL;
			/* search for last word, assume it is parameter name */
			while (*p != ')' && *p != ',') {
				if (*p == '/')
					p = skip_comment(p);
				else if (isalpha(*p)) {
					last_word = p;
					last_end = p = skip_word(p);
				} else
					p++;
			}
			if (last_word) {
				outwrite(parser, ", ", 2);
				outwrite(parser, last_word, last_end - last_word);
			}
			if (*p == ')')
				break;
			/* go to next argument */
			p = skip_whitespace(p+1);
		}
		outprintf(parser, ");\n}\n");
	}
}

static void print_member_decls(struct parser *parser, struct class *class)
{
	struct member *member;
	unsigned i;

	if (class->vmt && !parser->pf.coo_inline_defined) {
		outputs(parser, "\n"DEF_COO_INLINE);
		parser->pf.coo_inline_defined = 1;
	}

	for (i = 0; i < class->members_arr.num; i++) {
		member = class->members_arr.mem[i];
		/* don't print inherited members (that we did not override) */
		if (member->definition != class)
			continue;
		if (member->props.is_function) {
			print_func_decl(parser, class, member, MEMBER_FUNCTION);
		} else if (member->props.is_static) {
			outprintf(parser, "\nextern %s%s_%s;",
				member->rettype, class->name, member->name);
		}
	}

	/* now generate all the virtual method call wrappers */
	for (i = 0; i < class->members_arr.num; i++) {
		member = class->members_arr.mem[i];
		/* don't inherited members */
		if (member->origin != class)
			continue;
		if (member->props.is_virtual)
			print_func_decl(parser, class, member, VIRTUAL_WRAPPER);
	}
}

static struct class *parse_struct(struct parser *parser, char *next)
{
	struct class *class, *parentclass;
	struct memberprops memberprops;
	struct classptr decl, retclassptr;
	char *declbegin, *membername, *nameend, *params, *declend, *nextdecl;
	char *classname, *retend, *retnext, *prevdeclend, *prevnext;
	int level, is_typedef, parent_primary, parent_virtual;
	int first_virtual_warn, first_vmt_warn;
	struct classtype *parentclasstype;
	struct parent *parent, *firstparent;

	is_typedef = *parser->pf.pos == 't';
	/* skip 'typedef struct ' or just 'struct ' */
	classname = skip_whitespace(parser->pf.pos + (is_typedef ? 15 : 7));
	declend = skip_word(classname);
	if (declend != classname) {
		class = addclass(parser, classname, declend);
		if (!class)
			return NULL;
		/* mimic C++, class names are also types */
		decl.class = class;
		decl.pointerlevel = 0;
		addclasstype(parser, classname, declend, &decl);
	} else
		class = NULL;

	nextdecl = skip_whitespace(declend);
	if (class && *nextdecl == ':') {
		flush_until(parser, nextdecl);
		parent_primary = 1;
		parent_virtual = 0;
		first_virtual_warn = 0;
		first_vmt_warn = 0;
		outwrite(parser, "{", 1);
		for (classname = nextdecl;;) {
			classname = skip_whitespace(classname + 1);
			if (strprefixcmp("public ", classname)) {
				classname += 6;  /* "public" */
				continue;
			}
			if (strprefixcmp("virtual ", classname)) {
				classname += 7;  /* "virtual" */
				parent_virtual = 1;
				continue;
			}

			nameend = skip_word(classname);
			parentclasstype = find_classtype_e(parser, classname, nameend);
			if (parentclasstype == NULL) {
				fprintf(stderr, "cannot find parent class\n");
				break;
			}

			parent = calloc(1, sizeof(*parent));
			if (parent == NULL)
				break;

			parentclass = parentclasstype->decl.class;
			parent->class = parentclass;
			parent->child = class;
			parent->is_primary = parent_primary;
			parent->is_virtual = parent_virtual;
			if (class->ancestors.num_entries) {
				if (!parent_virtual && firstparent->is_virtual
						&& !first_virtual_warn) {
					fprintf(stderr, "put non-virtual base class "
						"first for efficiency\n");
					first_virtual_warn = 1;
				}
				if (parentclass->vmt && !firstparent->class->vmt
						&& !parent_virtual && !first_vmt_warn) {
					fprintf(stderr, "put virtual function class "
						"first for efficiency\n");
					first_vmt_warn = 1;
				}
			} else {
				/* remember first parent for checks later (above) */
				firstparent = parent;
			}

			import_parent(class, parent);
			outprintf(parser, "\n\tstruct %s %s%s;", parent->class->name,
				parent_virtual ? "*" : "", parent->class->name);

			nextdecl = skip_whitespace(nameend);
			if (*nextdecl == '{')
				break;
			if (*nextdecl != ',') {
				fprintf(stderr, "expected comma after parent class\n");
				break;
			}
			parent_primary = parent_virtual = 0;
			classname = nextdecl;
		}

		/* already printed '{', so start after '{' */
		parser->pf.writepos = nextdecl+1;
	}

	level = 1;  /* next is at opening brace '{' */
	declbegin = NULL;
	for (;;) {
		/* search (sub)struct or end of variable or function prototype */
		prevnext = next + 1;
		next = skip_whitespace(prevnext);
		if (declbegin == NULL) {
			prevdeclend = prevnext;
			declbegin = next;
		}
		/* remember last word before '(' or ';' => member name */
		if (isalpha(*next) || *next == '*')
			membername = next;
		if (!(next = scan_token(next, "/{};( \r\n\t\v")))
			return NULL;

		/* count substruct level */
		if (*next == '{') {
			declbegin = NULL;
			level++;
			continue;
		}
		if (*next == '}') {
			declbegin = NULL;
			if (--level == 0)
				break;
			continue;
		}
		/* ignore definitions in substructs */
		if (!class || level > 1)
			continue;
		if (isspace(*next))
			continue;

		declend = params = next;
		memberprops.is_function = *params == '(';

		/* skip pointers in membername */
		for (;; membername++) {
			/* is it a pointer to function variable? */
			if (*membername == '(') {
				retend = membername;
				memberprops.is_function = 0;
				/* find real params */
				membername++;
				/* let loop skip '*' in front of function variable */
				continue;
			} else if (*membername != '*' && !isspace(*membername))
				break;
		}
		/* find nameend and real params in case of function pointer variable */
		nameend = skip_word(membername);
		if (params < nameend) {
			params = scan_token(nameend, "/(;");
			if (!params)
				return NULL;
		} else {
			/* params >= nameend means this is not function variable
			   (in that case retend is already assigned above) */
			retend = membername;
		}

		/* next is either '(' or ';', but declend must be at ';' */
		if (*params == '(' && !(declend = scan_token(declend, "/;")))
			return NULL;

		memberprops.from_primary  = 1;  /* defined here so always primary */
		memberprops.is_static = strprefixcmp("static ", declbegin) != NULL;
		memberprops.is_virtual = strprefixcmp("virtual ", declbegin) != NULL;
		memberprops.is_override = strprefixcmp("override ", declbegin) != NULL;
		memberprops.is_virtual |= memberprops.is_override;
		memberprops.is_function |= memberprops.is_override;
		if (memberprops.is_virtual && !memberprops.is_function) {
			fprintf(stderr, "Member variable cannot be virtual\n");
			continue;
		}

		/* no need to check virtual and static because cannot both at declbegin */
		if (memberprops.is_virtual) {
			/* new members need a primary vmt to put them in */
			if (!memberprops.is_override && !class->vmt) {
				addvmt(class, class, PRIMARY_VMT);
				/* flush to front of virtual function */
				flush_until(parser, declbegin);
				outputs(parser, "void *vmt;");
				/* set writepos such that flush below is no-op */
				parser->pf.writepos = prevdeclend;
			}
			declbegin += 8;  /* skip "virtual " */
		}
		if (memberprops.is_override)
			declbegin++;     /* skip "override " (diff with "virtual ") */
		if (memberprops.is_static)
			declbegin += 7;  /* skip "static " */
		if (!memberprops.is_function)
			class->is_interface = 0;

		/* do not print functions or static variables inside the struct */
		next = declend;
		if (memberprops.is_function || memberprops.is_static) {
			flush_until(parser, prevdeclend);
			parser->pf.pos = parser->pf.writepos = next + 1;
		}

		if (memberprops.is_override) {
			/* cannot add member, should have inherited already from parent */
			implmember(class, membername, nameend);
			if (declbegin != membername)
				fprintf(stderr, "membername must follow override\n");
			if (*params != ';')
				fprintf(stderr, "no parameters allowed for override\n");
		} else {
			retclassptr = parse_type(parser, declbegin, &retnext);
			params = skip_whitespace(params+1);
			addmember(parser, class, &retclassptr, declbegin, retend,
				membername, nameend, params, declend, memberprops);
		}
		declbegin = NULL;
	}

	/* check for typedef struct X {} Y, *Z; */
	for (;;) {
		decl.pointerlevel = 0;
		for (classname = skip_whitespace(next + 1); *classname == '*'; classname++)
			decl.pointerlevel++;
		next = skip_word(classname);
		declend = scan_token(next, "/,;");
		if (declend == NULL)
			return NULL;
		if (next != classname) {
			if (is_typedef)
				addclasstype(parser, classname, next, &decl);
			else {
				/* TODO: addglobal() */
			}
		}
		if (*declend == ';')
			break;
		next = declend;
	}
	parser->pf.pos = skip_whiteline(declend + 1);
	flush(parser);
	print_vmt_type(parser, class);
	print_member_decls(parser, class);
	return class;
}

int parse_member(struct parser *parser, char *exprstart, char *exprend,
	struct class *class, char *name, char *nameend,
	struct classptr *retclassptr, struct dynarr **retparams)
{
	struct member *member;
	struct insert **after_pos = NULL;
	char *args, *flush_until, *insert_text, *continue_at;
	int add_this;

	if (class == NULL)
		return -1;
	if ((member = find_member_e(class, name, nameend)) == NULL)
		return -1;

	if (exprstart && member->props.is_static) {
		fprintf(stderr, "Cannot access static member\n");
		return -1;
	}

	if (member->funcinsert)
		after_pos = addinsert(parser, after_pos,
			exprstart ?: name, member->funcinsert, name);

	continue_at = NULL;
	add_this = !exprstart && !member->props.is_static;
	if (member->props.is_function) {
		args = scan_token(nameend, "/(;");
		if (args != NULL && *args == ';')
			args = NULL;
		if (args != NULL)
			args = skip_whitespace(args+1);
		if (args == NULL)
			goto out;   /* end of file? escape */

		continue_at = args;
		if (add_this) {
			flush_until = args;
			if (member->parentname) {
				after_pos = addinsert(parser, after_pos,
					args, "&this->", args);
				if (*args != ')') {
					after_pos = addinsert(parser, after_pos,
						args, member->parentname, args);
					insert_text = ", ";
				} else
					insert_text = member->parentname;
			} else {
				if (*args == ')')
					insert_text = "this";
				else
					insert_text = "this, ";
			}
		} else {
			if (*args == ')')
				insert_text = "";
			else
				insert_text = ", ";
			if (exprstart) {
				/* after other->function(, need to jump to
				   'other' expression and back to arguments */
				addinsert(parser, after_pos, args,
					member->parentname ? "&" : "", exprstart);
				/* continue at end */
				after_pos = (struct insert **)
					&parser->inserts.mem[parser->inserts.num];
				if (member->parentname) {
					after_pos = addinsert(parser, after_pos,
						exprend, "->", args);
					/* reuse empty insert_text if possible */
					if (insert_text[0] != 0)
						after_pos = addinsert(parser, after_pos,
							args, member->parentname, args);
					else
						insert_text = member->parentname;
					flush_until = args;
				} else
					flush_until = exprend;
			} else
				flush_until = name;
		}
	} else if (add_this) {
		flush_until = name;
		continue_at = name;
		insert_text = "this->";
		if (member->nameinsert) {
			after_pos = addinsert(parser, after_pos, name, insert_text, name);
			insert_text = member->nameinsert;
		}
	}

	if (continue_at)
		addinsert(parser, after_pos, flush_until, insert_text, continue_at);

out:
	*retclassptr = member->retclassptr;
	*retparams = &member->params;
	return 0;
}

static void parse_typedef(struct parser *parser, char *declend)
{
	struct classptr decl;
	char *next;

	/* check if a class is aliased to a new name */
	decl = parse_type(parser, parser->pf.pos, &next);
	if (decl.class == NULL)
		return;

	/* note: after next cannot be '{', as in 'typedef struct X {'
	   it is handled by calling parse_struct in caller */
	parser->pf.pos = skip_whitespace(next);
	next = skip_word(parser->pf.pos);
	addclasstype(parser, parser->pf.pos, next, &decl);
	parser->pf.pos = declend + 1;
}

static void print_inserts(struct parser *parser)
{
	struct insert *insert;
	int j;

	if (parser->inserts.num == 0)
		return;

	for (j = 0; j < parser->inserts.num; j++) {
		insert = parser->inserts.mem[j];
		flush_until(parser, insert->flush_until);
		outputs(parser, insert->insert_text);
		parser->pf.writepos = insert->continue_at;
	}
	parser->inserts.num = 0;
}

static void param_to_variable(struct parser *parser, int position,
		struct classptr *decl, char *name, char *next)
{
	addvariable(parser, 0, decl, 0, name, next, NULL, NULL);
}

static void accessancestor(struct parser *parser,
		char *exprstart, char *name, char *end,
		struct classptr *target, struct classptr *expr)
{
	struct ancestor *ancestor;
	struct insert **afterpos = NULL;
	char *pre, *post;

	ancestor = hasho_find(&expr->class->ancestors, target->class);
	if (ancestor == NULL)
		return;

	/* check if need to add parentheses to surround & ... -> */
	if (exprstart != name) {
		pre = !ancestor->parent->is_virtual ? "&(" : "(";
		post = expr->pointerlevel ? ")->" : ").";
	} else {
		pre = !ancestor->parent->is_virtual ? "&" : NULL;
		post = expr->pointerlevel ? "->" : ".";
	}
	if (pre)
		afterpos = addinsert(parser, afterpos, exprstart, pre, exprstart);
	afterpos = addinsert(parser, afterpos, end, post, end);
	addinsert(parser, afterpos, end, ancestor->path, end);
}

struct paramstate {
	struct dynarr *params;
	int index;
};

void select_next_param(struct paramstate *state, struct classptr *dest)
{
	dest->class = NULL;
	state->index++;
	if (state->params && state->index < state->params->num) {
		struct classptr *source = state->params->mem[state->index];
		if (source)
			*dest = *source;   /* only two pointer copies */
	}
}

/* state for detecting function variable: (*name)(.... */
enum parse_funcvar_state { FV_NONE, FV_NAME, FV_PARENCLOSE };

static void parse_function(struct parser *parser, char *next)
{
	struct class *class;
	struct classptr exprdecl[MAX_PAREN_LEVELS], targetdecl[MAX_PAREN_LEVELS];
	struct classptr retclassptr, decl, immdecl, *target, *expr;
	struct paramstate targetparams[MAX_PAREN_LEVELS];
	struct dynarr **tgtparams;
	struct classtype *classtype;
	struct member *member;
	char *curr, *funcname, *funcnameend, *classname, *name, *argsep, *dblcolonsep;
	char *exprstart[MAX_PAREN_LEVELS], *exprend, *params, *paramend;
	char *memberstart[MAX_PAREN_LEVELS], *funcvarname, *funcvarnameend;
	enum parse_funcvar_state funcvarstate;
	enum parse_state state;
	int blocklevel, parenlevel, seqparen;

	funcname = NULL, classname = next;
	for (; classname > parser->pf.pos && !isspace(*(classname-1)); classname--) {
		if (classname[0] == ':' && classname[1] == ':') {
			dblcolonsep = classname;
			classname[0] = 0;
			funcname = skip_whitespace(classname+2);
		}
	}

	params = ++next;  /* advance after '(' */
	paramend = scan_token(params, "/)");
	if (paramend == NULL)
		return;
	if (funcname) {   /* funcname assigned means there is a classname::funcname */
		if ((class = find_class(parser, classname)) != NULL) {
			/* lookup method name */
			funcnameend = skip_word(funcname);
			member = find_member_e(class, funcname, funcnameend);
			if (member != NULL) {
				if (member->props.is_virtual)
					class->write_vmt = 1;
			} else {
				struct memberprops props = {0,};
				/* undeclared, so it's private, include static */
				flush(parser);
				outputs(parser, "static ");
				/* add as member so others can call from further down */
				retclassptr = parse_type(parser, parser->pf.pos, &curr);
				props.is_function = 1;
				props.seen = 1;
				addmember(parser, class, &retclassptr, parser->pf.pos,
					classname, funcname, funcnameend,
					params, paramend, props);
			}
			/* replace :: with _ */
			flush_until(parser, dblcolonsep);
			outwrite(parser, "_", 1);
			parser->pf.writepos = funcname;
			flush_until(parser, next);
			/* check if there are parameters */
			next = skip_whitespace(next);
			argsep = "";  /* start assumption: no params */
			if (*next != ')') {
				if (strprefixcmp("void", next) && !isalpha(next[4]))
					next += 4;  /* skip "void" if adding this param */
				else
					argsep = ", ";  /* separate this, rest params */
			}
			/* add this parameter */
			outprintf(parser, "struct %s *this%s", classname, argsep);
			parser->pf.writepos = next;
		} else {
			fprintf(stderr, "class '%s' not declared\n", classname);
		}
	} else
		class = NULL;

	next = skip_whitespace(paramend+1);
	if (*next == ';') {
		/* function prototype */
		parser->pf.pos = next + 1;
		return;
	}

	/* store parameters as variables */
	hash_clear(&parser->locals);
	parser->nested_locals.num = 0;
	next = parse_parameters(parser, params, param_to_variable);
	if (next == NULL)
		return;

	blocklevel = seqparen = parenlevel = exprdecl[0].pointerlevel = 0;
	exprdecl[0].class = targetdecl[0].class = decl.class = immdecl.class = NULL;
	exprstart[0] = memberstart[0] = NULL;
	targetparams[0].params = NULL;
	funcvarstate = FV_NONE;
	state = STMTSTART;
	for (;;) {
		curr = skip_whitespace(next);
		/* skip comments */
		if (curr[0] == 0)
			return;

		/* track block nesting level */
		if (*curr == '{') {
			blocklevel++;
			next = curr + 1;
			continue;
		}
		if (*curr == '}') {
			remove_locals(parser, blocklevel);
			if (--blocklevel == 0)
				break;
			next = curr + 1;
			continue;
		}
		if (!exprstart[parenlevel])
			exprstart[parenlevel] = curr;
		if (isdigit(*curr))
			curr = skip_word(curr);
		else if (isalpha(*curr)) {
			if (!memberstart[parenlevel])
				memberstart[parenlevel] = curr;
			if (strprefixcmp("static ", curr)) {
				next = curr + 7;  /* "static " */
				continue;   /* stay in e.g. statement-start state */
			} else if (strprefixcmp("const ", curr)) {
				next = curr + 6;  /* "const " */
				continue;   /* stay in e.g. declare-var state */
			} else if (strprefixcmp("struct ", curr)) {
				name = skip_whitespace(curr + 7);  /* "struct " */
				next = skip_word(name);
				if (state == DECLVAR || state == ACCESSMEMBER) {
					/* grammar error, ignore */
					state = FINDVAR;
				} else if ((decl.class = find_class_e(parser, name, next)) != NULL) {
					decl.pointerlevel = 0;
					goto decl_or_cast;
				}
				continue;
			}

			name = curr;
			next = skip_word(name);
			switch (state) {
			case DECLVAR:
				addvariable(parser, blocklevel, &decl,
					exprdecl[0].pointerlevel, name, next, NULL, NULL);
				state = FINDVAR;
				break;
			case ACCESSMEMBER:
				expr = immdecl.class ? &immdecl : &exprdecl[parenlevel];
				if (expr->pointerlevel == 1)
					parse_member(parser, memberstart[parenlevel], exprend,
						expr->class, name, next, expr,
						&targetparams[parenlevel].params);
				state = FINDVAR;
				break;
			default:
				/* try to find variable or member field
				 * if can't find, then maybe is a typedef to declare variable */
				tgtparams = &targetparams[parenlevel].params;
				if (find_local_e_class(parser, name, next,
						&immdecl, tgtparams) < 0
					&& parse_member(parser, NULL, NULL,
						class, name, next, &immdecl, tgtparams) < 0
					&& find_global_e_class(
						parser, name, next, &immdecl, tgtparams) < 0
					&& (classtype = find_classtype_e(
						parser, name, next)) != NULL) {
					decl = classtype->decl;
				  decl_or_cast:
					if (state == STMTSTART) {
						state = DECLVAR;
						break;
					} else if (seqparen >= 2
							&& !exprdecl[parenlevel-2].class) {
						/* this is a cast, like ((class_t*)x)->..
						   remember first detected class */
						exprdecl[parenlevel-2].class = decl.class;
						/* combine possible pointer dereference */
						exprdecl[parenlevel-2].pointerlevel +=
							decl.pointerlevel;
						decl.class = NULL;
					} else {
						/* grammar error, ignore */
						decl.class = NULL;
					}
				} else if (parenlevel == 1 && exprdecl[1].pointerlevel == -1) {
					/* perhaps this is a function pointer variable decl? */
					funcvarname = name;
					funcvarnameend = next;
					funcvarstate = FV_NAME;
				}
				state = FINDVAR;
				break;
			}

			continue;
		}

		if (funcvarstate == FV_NAME)
			funcvarstate = *curr == ')' ? FV_PARENCLOSE : FV_NONE;
		else if (funcvarstate == FV_PARENCLOSE) {
			funcvarstate = FV_NONE;
			if (*curr == '(') {
				next = scan_token(curr+1, "/);");
				if (next == NULL)
					break;
				if (*next == ')')
					addvariable(parser, blocklevel, &decl,
						exprdecl[0].pointerlevel, funcvarname,
						funcvarnameend, curr+1, next);
			}
		}

		if (*curr != '.' && (*curr != '-' || curr[1] != '>'))
			memberstart[parenlevel] = NULL;
		if (*curr == ')' || *curr == ',' || *curr == '=' || *curr == ';') {
			if (exprdecl[parenlevel].class != NULL)
				expr = &exprdecl[parenlevel];
			else
				expr = &immdecl;
			target = &targetdecl[parenlevel];
			if (target->class && expr->class && target->class != expr->class
					&& target->pointerlevel == expr->pointerlevel
					&& target->pointerlevel <= 1)
				accessancestor(parser,
					exprstart[parenlevel], name, curr, target, expr);
			exprstart[parenlevel] = NULL;
		}
		if (*curr == '(')
			seqparen++;
		else {
			seqparen = 0;
			targetparams[parenlevel].params = NULL;
		}
		if (*curr == '(') {
			memberstart[parenlevel] = curr;
			if (parenlevel < MAX_PAREN_LEVELS) {
				parenlevel++;
				memberstart[parenlevel] = NULL;  /* reset, assigned later */
				exprstart[parenlevel] = NULL;
				exprdecl[parenlevel].class = NULL;
				exprdecl[parenlevel].pointerlevel = 0;
				targetparams[parenlevel].params = NULL;
				targetparams[parenlevel-1].index = -1;
				select_next_param(&targetparams[parenlevel-1],
					&targetdecl[parenlevel]);
			} else
				fprintf(stderr, "maximum parenthesis nesting level reached\n");
			state = EXPRUNARY;
		} else if (*curr == ')') {
			if (parenlevel > 0) {
				parenlevel--;
				if (exprdecl[parenlevel].class == NULL)
					exprdecl[parenlevel] = exprdecl[parenlevel+1];
			}
		} else if (*curr == '*') {
			switch (state) {
			case DECLVAR: exprdecl[0].pointerlevel++; break;
			case STMTSTART:
			case ACCESSMEMBER:
			case EXPRUNARY: exprdecl[parenlevel].pointerlevel--; break;
			default: break;
			}
		} else if (*curr == ',') {
			if (decl.class && parenlevel == 0)
				state = DECLVAR;
			else
				state = EXPRUNARY;
			if (parenlevel)
				select_next_param(&targetparams[parenlevel-1],
					&targetdecl[parenlevel]);
			exprdecl[parenlevel].pointerlevel = 0;
		} else if (*curr == '=') {
			targetdecl[parenlevel] = immdecl;
			targetdecl[parenlevel].pointerlevel += exprdecl[parenlevel].pointerlevel;
			exprdecl[parenlevel].pointerlevel = 0;
			immdecl.class = NULL;
			state = EXPRUNARY;
		} else if (*curr == '.') {
			state = ACCESSMEMBER;
			exprend = curr;
		} else if (*curr == '-' && curr[1] == '>') {
			state = ACCESSMEMBER;
			exprend = curr;
			curr++;
		} else if (*curr == ';') {
			print_inserts(parser);
			immdecl.class = NULL;
			decl.class = NULL;
			seqparen = 0;
			parenlevel = 0;
			state = STMTSTART;
			exprdecl[0].pointerlevel = 0;
		} else if (*curr == '+' || *curr == '-' || *curr == '/' || *curr == '%'
			|| *curr == '&' || *curr == '|' || *curr == '^' || *curr == '!'
			|| *curr == '?' || *curr == ':') {
			state = EXPRUNARY;
			if (immdecl.class != NULL) {
				exprdecl[parenlevel] = immdecl;
				immdecl.class = NULL;
			}
		} else
			state = FINDVAR;

		/* advance for most of the operator/separator cases */
		if (next <= curr)
			next = curr+1;
	}

	parser->pf.pos = curr + 1;
}

static void print_vmts(struct parser *parser)
{
	struct class *class;
	struct member *member;
	struct vmt *vmt;
	unsigned i, j;

	hash_foreach(class, &parser->classes) {
		if (!class->write_vmt)
			continue;

		for (i = 0; i < class->vmts.num; i++) {
			vmt = class->vmts.mem[i];
			if (!vmt->modified)
				continue;

			/* flush if not already done so */
			if (parser->pf.writepos != parser->pf.pos)
				flush(parser);

			/* TODO: print multiple vmts in case of multiple inheritance / interfaces */
			/* TODO: for multiple inheritance, print wrappers to fix base address */
			/* TODO: fix member-defined-in-class type, make it match */
			outprintf(parser, "\nstruct %s %s = {\n", vmt->name, vmt->name);
			for (j = 0; j < class->members_arr.num; j++) {
				member = class->members_arr.mem[j];
				if (member->vmt == NULL)
					continue;
				if (member->vmt->origin != vmt->origin)
					continue;

				outprintf(parser, "\t%s_%s,\n",
					member->definition->name, member->name);
			}
			outprintf(parser, "};\n");
		}
	}
}

enum include_location {
	SEARCH_CURRENT_DIR,
	SEARCH_ONLY_PATHS,
};

static FILE *locate_include_file(struct parser *parser, char *dir, char *nameend,
		char *fullname, char **ret_newfilename_file)
{
	char *p, *bufend = &parser->newfilename[sizeof(parser->newfilename)];
	struct file_id *file_id;
	FILE *fp_inc;

	p = stmcpy(parser->pf.newfilename_file, bufend, dir);
	if (dir[0] != 0 && *(p-1) != DIRSEP)
		*p++ = DIRSEP;
	*ret_newfilename_file = p;  /* remember filename start in case we parse it */
	p = stmecpy(p, bufend, parser->pf.pos, nameend);
	if (p+1 == bufend)  /* nul-character before end, at end is overflow! */
		return NULL;

	fp_inc = fopen(fullname, "rb");
	if (fp_inc == NULL)
		return NULL;

	/* check if we have already seen/parsed this file */
	file_id = calloc(1, sizeof(*file_id));
	if (file_id == NULL)
		goto err_fp;
	if (get_file_id(fp_inc, file_id) < 0)
		goto err_fp;
	if (hash_insert(&parser->files_seen, &file_id->node,
			uint64hash(file_id->file_id)) < 0)
		goto err_hash;
	parser->newfilename_end = p;
	return fp_inc;
err_hash:
	free(file_id);
err_fp:
	fclose(fp_inc);
	return NULL;
}

enum {
	OUTPUT_FILE_WRITTEN = 0,
	OUTPUT_FILE_SKIPPED = 1,
};

static int parse_source(struct parser *parser, char *filename, FILE *in, char *ext_out);

static int try_include_file(struct parser *parser, char *dir, char *nameend)
{
	FILE *fp_inc;
	char *buffer, *outbuffer, *fullname, *filename, *outfilename;
	char *p, *new_newfilename_file;
	struct parse_file *parse_file;
	int parse_ret;

	/* if directory specified, use from there, otherwise including current dir */
	if (dir[0] != 0)
		fullname = parser->pf.newfilename_file;
	else
		fullname = parser->pf.newfilename_path;
	fp_inc = locate_include_file(parser, dir, nameend, fullname, &new_newfilename_file);
	if (fp_inc == NULL)
		return -1;

	/* allocate temporary to store current state */
	parse_file = addfilestack(parser);
	if (parse_file == NULL)
		goto err_add;

	/* make copy before overwriting parse_file, newfilename is reused later */
	filename = strdupto(parse_file->filename, fullname);
	/* reuse old buffers, if any */
	buffer = parse_file->buffer;
	outbuffer = parse_file->outbuffer;
	outfilename = parse_file->outfilename;
	/* save current parsing state */
	memcpy(parse_file, &parser->pf, sizeof(*parse_file));
	/* parser->filename assigned in parse_source */
	parser->pf.out = NULL;
	parser->pf.buffer = buffer;
	parser->pf.outbuffer = outbuffer;
	parser->pf.outfilename = outfilename;
	parser->pf.coo_inline_defined = 0;
	parser->pf.outfailed = 0;
	/* if path given then put it on stack */
	if (dir[0] != 0)
		parser->pf.newfilename_path = parser->pf.newfilename_file;
	parser->pf.newfilename_file = new_newfilename_file;
	/* recursively parse! */
	parse_ret = parse_source(parser, filename, fp_inc, parser->header_ext_out);
	/* restore parser state, swap the buffers back, we can reuse them later */
	buffer = parser->pf.buffer;
	filename = parser->pf.filename;
	outbuffer = parser->pf.outbuffer;
	outfilename = parser->pf.outfilename;
	memcpy(&parser->pf, parse_file, sizeof(parser->pf));
	parse_file->buffer = buffer;
	parse_file->filename = filename;
	parse_file->outbuffer = outbuffer;
	parse_file->outfilename = outfilename;
	parser->file_stack.num--;
	if (parse_ret == OUTPUT_FILE_WRITTEN) {
		/* write new output filename, filename generated by
		 * parse_source_size in newfilename_path for rename() target
		 * parser->pf.pos is at the filename after '"' or '<', write it as well */
		flush_until(parser, parser->pf.pos);
		p = new_newfilename_file;  /* cannot use pf.new_filename_file, moved */
		outwrite(parser, p, parser->newfilename_end - p);
		/* nameend is at '"' or '>' at end of #include statement, write it as well */
		parser->pf.writepos = nameend;
	}
	/* fp_inc was closed by parse_source */
	return 0;
err_add:
	fclose(fp_inc);
	return -1;
}

static void try_include(struct parser *parser, char *nameend, enum include_location loc)
{
	char *ext;
	unsigned j;

	parser->pf.pos++;
	/* check user enabled include extension matching */
	if (parser->include_ext_in_len) {
		/* check if name is too short to match extension at all */
		ext = nameend - parser->include_ext_in_len;
		if (ext < parser->pf.pos)
			return;
		if (strprefixcmp(parser->include_ext_in, ext) == NULL)
			return;
	}

	if (loc == SEARCH_CURRENT_DIR)
		if (try_include_file(parser, "", nameend) == 0)
			return;
	for (j = 0; j < parser->includepaths.num; j++)
		if (try_include_file(parser, parser->includepaths.mem[j], nameend) == 0)
			return;

	if (parser->include_ext_in_len) {
		char *tempname = stredup(parser->pf.pos, nameend);
		fprintf(stderr, "%s: file not found\n", tempname);
		free(tempname);
	} else {
		/* not found... assume it's a system header or so */
	}
}

static void parse_include(struct parser *parser)
{
	char *next;

	while (islinespace(*parser->pf.pos))
		parser->pf.pos++;
	next = scan_token(parser->pf.pos + 1, "/\">\r\n");
	if (*parser->pf.pos == '"' && *next == '"')
		try_include(parser, next, SEARCH_CURRENT_DIR);
	else if (*parser->pf.pos == '<' && *next == '>')
		try_include(parser, next, SEARCH_ONLY_PATHS);
	else
		fprintf(stderr, "unknown character after #include, ignored\n");

	parser->pf.pos = next+1;
}

static void parse(struct parser *parser)
{
	char *next;

	/* search for start of struct or function */
	for (;;) {
		parser->pf.pos = skip_whitespace(parser->pf.pos);
		if (*parser->pf.pos == '#') {
			parser->pf.pos++;
			if (strprefixcmp("include ", parser->pf.pos)) {
				parser->pf.pos += 8;  /* "include " */
				parse_include(parser);
			}

			next = strchr(parser->pf.pos, '\n');
			if (next == NULL)
				break;

			parser->pf.pos = next + 1;
			continue;
		}

		next = scan_token(parser->pf.pos, "/({;");
		if (next == NULL)
			break;

		if (*next == '{' && strprefixcmp("struct ", parser->pf.pos)) {
			parse_struct(parser, next);
		} else if (*next == '{' && strprefixcmp("typedef struct ", parser->pf.pos)) {
			parse_struct(parser, next);
		} else if (*next == ';' && strprefixcmp("typedef ", parser->pf.pos)) {
			parser->pf.pos += 8;  /* "typedef " */
			parse_typedef(parser, next);
		} else if (*next == '(') {
			parse_function(parser, next);
		}
		if (parser->pf.pos <= next)
			parser->pf.pos = next + 1;
	}

	/* flush in caller */
}

/* parser->pf.out == NULL => parser->newfilename contains input filename, transform
 * this filename to output filename and read it into outbuffer
 * parser->pf.out != NULL => do not scan output file, output immediately */
static int parse_source_size(struct parser *parser, char *filename,
		FILE *in, size_t size, char *ext_out)
{
	char *buffer, *outfilename, *outfilename_file, *p;
	size_t commonlen, outnamelen;
	FILE *fp_dest;

	/* read entire input file in one go to make scanning easier */
	parser->pf.filename = filename;
	buffer = read_file_until(in, parser->pf.buffer, size);
	if (buffer == NULL)
		return -1;
	fclose(in);

	/* scan (previously generated) output file for comparison, if applicable */
	if (parser->pf.out == NULL) {
		/* input filename was written into newfilename, just need to
		 * replace extension to generate output filename */
		char *bufend = &parser->newfilename[sizeof(parser->newfilename)];
		parser->newfilename_end = replace_ext_temp(parser->newfilename,
			parser->newfilename_end, bufend, ext_out);
		if (parser->newfilename_end == NULL)
			return -1;

		/* newfilename includes temporary (pid) extension, make a copy
		 * without it for outfilename, so we can reuse newfilename later */
		outfilename = strdupto(parser->pf.outfilename, parser->pf.newfilename_path);
		parser->pf.outfilename = outfilename;
		parser->pf.outfilename_end = outfilename
			+ (parser->newfilename_end - parser->pf.newfilename_path);
		*parser->pf.outfilename_end = 0;
		/* open read-only, because if we want to modify, write temporary file later */
		fp_dest = fopen(outfilename, "rb");
		if (fp_dest) {
			parser->pf.outbuffer = read_file(fp_dest, parser->pf.outbuffer);
			parser->pf.outpos = parser->pf.outbuffer;
			fclose(fp_dest);
			if (parser->pf.outbuffer == NULL)
				return -1;
		} else {
			parser->pf.outpos = NULL;
		}
	}

	/* prepare input buffer pointers for scanning */
	parser->pf.buffer = parser->pf.writepos = parser->pf.pos = buffer;
	/* make sure filename_file points to filename (after last /) */
	for (p = parser->pf.newfilename_file; *p; p++)
		if (*p == DIRSEP)
			parser->pf.newfilename_file = p+1;

	/* start parsing! */
	parse(parser);
	print_vmts(parser);
	/* if nothing changed (== no flush, writepos did not move) */
	if (parser->pf.writepos == buffer) {
		/* if parsing included file, and nothing changed, do not write any output */
		if (parser->file_stack.num)
			return OUTPUT_FILE_SKIPPED;
		/* otherwise flush now, we always want output of main file */
		flush(parser);
		return OUTPUT_FILE_WRITTEN;
	}
	/* store output filename (1) for rename below (2) for caller #include "..." */
	if (parser->pf.outfilename) {
		/* newfilename_path..newfilename_file was not modified, is common
		 * align to newfilename_path so that caller can use for #include */
		commonlen = parser->pf.newfilename_file - parser->pf.newfilename_path;
		outfilename_file = parser->pf.outfilename + commonlen;
		outnamelen = parser->pf.outfilename_end - outfilename_file;
		memcpy(parser->pf.newfilename_file, outfilename_file, outnamelen);
		parser->pf.newfilename_file[outnamelen] = 0;
	}
	if (parser->pf.out) {
		fclose(parser->pf.out);
		parser->pf.out = NULL;
		/* rename temporary output filename to final filename
		 * might not be present in case of stdout output */
		if (parser->pf.outfilename)
			rename(parser->pf.outfilename, parser->pf.newfilename_path);
	}

	return OUTPUT_FILE_WRITTEN;
}

static int parse_source(struct parser *parser, char *filename, FILE *in, char *ext_out)
{
	return parse_source_size(parser, filename, in, file_size(in), ext_out);
}

static void parse_from_file(struct parser *parser, char *filename, char *ext_out)
{
	FILE *in;
	char *strend;

	in = fopen(filename, "rb");
	if (!in) {
		fprintf(stderr, "Cannot open file '%s'\n", filename);
		return;
	}

	if (stfcpy_e(parser->newfilename, filename, &strend) < 0) {
		fprintf(stderr, "input filename too long\n");
		return;
	}

	parser->newfilename_end = strend;
	parse_source(parser, filename, in, ext_out);
}

static int parse_stdin(struct parser *parser)
{
	parser->pf.out = stdout;
	return parse_source_size(parser,
		"<stdin>", stdin, STDIN_BUFSIZE, parser->source_ext_out);
}

static void usage(void)
{
	fprintf(stderr, "coo: an Object Oriented C to plain C compiler\n"
		"usage: coo [option] [FILE] ...\n"
		"\twith no FILE, read from stdin\n"
		"options:\n"
		"\t-Ipath: search path for include files\n"
		"\t-ofile: set output filename\n"
		"\t-xsext: output source to filename with extension replaced with ext\n"
		"\t-xhext: output headers to filename with extension replaced with ext\n"
		"\t-xiext: only find includes with extension ext; error if not found\n");
}

static int parseextoption(struct parser *parser, char *option)
{
	switch (*option) {
	case 'j':
		parser->include_ext_in = option+1;
		parser->include_ext_in_len = strlen(parser->include_ext_in);
		return 0;
	case 's': parser->source_ext_out = option+1; return 0;
	case 'h': parser->header_ext_out = option+1; return 0;
	default: usage(); return -1;
	}
}

static int initparser(struct parser *parser)
{
	memset(parser, 0, sizeof(*parser));
	parser->pf.newfilename_path = parser->pf.newfilename_file =
		parser->newfilename_end = parser->newfilename;
	parser->source_ext_out = ".coo.c";
	parser->header_ext_out = ".coo.h";
	return strhash_init(&parser->classes, 64, class) < 0 ||
		strhash_init(&parser->classtypes, 64, classtype) < 0 ||
		strhash_init(&parser->globals, 64, variable) < 0 ||
		strhash_init(&parser->locals, 16, variable) < 0 ||
		hash_init(&parser->files_seen, compare_file_ids,
			64, offsetof(struct file_id, node), 0) < 0;
}

int main(int argc, char **argv)
{
	struct parser parser_s, *parser = &parser_s;

	if (initparser(parser)) {
		fprintf(stderr, "Out of memory allocating parser\n");
		return 1;
	}

	g_pid = getpid();
	argc--; argv++; /* skip our name */
	/* parse options */
	for (; argc; argc--, argv++) {
		if ((*argv)[0] == '-') {
			switch ((*argv)[1]) {
			case 'I':
				if (addincludepath(parser, *argv + 2) < 0)
					return 2;
				break;
			case 'o': /* output filename? TODO */
			case 'x':
				if (parseextoption(parser, *argv + 1) < 0)
					return 2;
				break;
			default: usage(); return 1;
			}
		} else
			parse_from_file(parser, *argv, parser->source_ext_out);
	}

	/* if no file given on commandline, then parse stdin */
	if (parser->pf.filename == NULL)
		parse_stdin(parser);

	return 0;
}
