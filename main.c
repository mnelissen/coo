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

#define container_of(ptr, type, node_var) \
  ((type *)((size_t)(ptr)-(size_t)(&((type *)0)->node_var)))

#define NEWFN_MAX          8192            /* (stack)size for included dirs+filenames */
#define OUTPR_MAX          256             /* maximum outprintf size we do */
#define STDIN_BUFSIZE      (1024 * 1024)   /* size for inputbuffer when using stdin */
#define MAX_PAREN_LEVELS   16
#define MAX_USER_ERRORS    25
#define DEF_COO_INLINE \
	"#ifndef coo_inline\n" \
	"#ifdef _MSC_VER\n" \
	"#define coo_inline __forceinline\n" \
	"#else\n" \
	"#define coo_inline extern inline " \
		"__attribute__((always_inline)) __attribute__((gnu_inline))\n" \
	"#endif\n" \
	"#endif\n" \
	"#ifndef container_of\n" \
	"#define container_of(ptr, type, node_var) \\\n" \
	"  ((type *)((size_t)(ptr)-(size_t)(&((type *)0)->node_var)))\n" \
	"#endif\n"
#define NUM_LINES_DEF_COO_INLINE 11

#ifdef __GNUC__
#define attr_format(a,b) __attribute__((format(printf,a,b)))
#else
#define attr_format(a,b)
#endif

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
#define strhash_init(tbl, tblsize, itemtype) \
	hash_init(tbl, (hash_cmp_cb)strcmp, tblsize, \
		offsetof(struct itemtype, node), offsetof(struct itemtype, name))
#define strhash_insert(tbl, item) \
	hash_insert(tbl, &(item)->node, strhash((item)->name))
#define strhash_insert_exists(tbl, item, resvar) \
	hash_insert_exists(tbl, &(item)->node, strhash((item)->name), resvar)

struct file_id {
	dev_t dev_id;
	ino_t file_id;
	uint64_t mtime;
	struct hash_entry node;
};

enum parse_state {
	FINDVAR, STMTSTART, DECLVAR, CONSTRUCT, CASTVAR,
	ACCESSMEMBER, ACCESSINHERITED, EXPRUNARY
};

struct dynarr {
	void **mem;
	unsigned num;
	unsigned max;
};

struct memberprops {
	unsigned char is_virtual:1;
	unsigned char is_abstract:1;
	unsigned char is_override:1;
	unsigned char is_function:1;
	unsigned char is_static:1;
	unsigned char from_primary:1;  /* inherited from primary base class? */
	unsigned char from_virtual:1;  /* a virtual inheritance in chain? */
};

struct class {
	struct hash members;
	struct dynarr members_arr;
	struct hasho ancestors;      /* class => ancestor, all classes inherited from */
	struct dynarr vmts;          /* struct vmt *, all applicable vmts */
	struct dynarr descendants;   /* struct parent *, inheriting from this class */
	struct vmt *vmt;             /* from primary parent (or new here)  */
	struct member *constructor;  /* (parent) constructor defined for this class */
	struct member *root_constructor;  /* root constructor defined for this class */
	struct member *destructor;   /* (parent) destructor defined for this class */
	struct member *root_destructor;  /* root destructor defined for this class */
	struct class *missing_root;  /* missing root constructor because of non-void ... */
	struct class *prim_parent;   /* primary parent to mimic constructor of */
	struct class *freer_class;   /* usable class (this or parent) to free() 'this' */
	struct rootclass *rootclass; /* root class for missing virtual base(s), if any */
	struct hash_entry node;      /* node in parser.classes hash */
	unsigned num_parent_constr;  /* number of parent constructors */
	unsigned num_parent_destr;   /* number of parent destructors */
	unsigned num_init_vars;      /* number of members needing root construction call */
	unsigned num_abstract;       /* number of abstract virtual methods */
	char declare_complete;       /* inside or after struct declaration? */
	char is_interface;
	char is_implemented;         /* have seen class implementation */
	char is_rootclass;           /* this class is a rootclass */
	char has_duplicates;         /* multiple parents override same member */
	char void_constructor;       /* constructor has return type void (can't fail) */
	char void_root_constructor;  /* root constructor has return type void */
	char need_root_constructor;  /* has virtual bases or vmts to initialize */
	char need_root_destructor;   /* has virtual bases to destruct */
	char has_constructor;        /* this class (not any parent) has constructor */
	char has_destructor;         /* this class (not any parent) has destructor */
	char gen_constructor;
	char gen_root_constructor;   /* need root constructor and not user defined */
	char gen_root_destructor;    /* need root destructor and not user defined */
	char gen_destructor;         /* need destructor and not user defined */
	char name[];                 /* class name, keep behind gen_destructor */
	                             /* temp.use name[-1] to add gen.destructor */
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
	struct ancestor *next;     /* alternate path to same origin (interface) */
	unsigned from_virtual;
	char path[];   /* full path to ancestor */
};

struct rootclass {
	struct parent parent;        /* link to class from (its) rootclass */
	struct class class;          /* rootclass definition */
};

struct vmt {
	struct class *class;         /* class this vmt belongs to */
	struct class *origin;        /* where this vmt is first defined */
	struct vmt *parent;          /* pointer to same origin vmt in parent */
	unsigned char modified;      /* any method overriden by class */
	unsigned char is_primary;    /* is this the primary vmt for this class? */
	char *name;                  /* vmt struct type and variable name */
};

struct member {
	struct hash_entry node;    /* entry in class->members */
	char *rettype;
	struct classptr retclassptr;
	struct class *origin;      /* origin class where member was defined */
	struct class *definition;  /* closest class virtual member overridden */
	struct vmt *vmt;           /* vmt this member is defined in */
	char *paramstext;          /* literal text of all params definition */
	struct dynarr params;      /* struct classptr *, params' class type */
	char *funcinsert;   /* insert this string in front of function call */
	  /* for normal function:  func() ==> class_func()
	     for virtual function: func() ==> class_vmt_func() */
	char *parentname;   /* insert after function first argument expression */
	  /* for a function: x->func() ==> x_func(&x.parent) */
	char *nameinsert;   /* this member is inherited, insert this in front */
	  /* for this class: NULL or empty string
	     for inherited: field1 ==> parent.field1 */
	char *implprefix;         /* prefix for implementation name (destructor) */
	char *implname;           /* implementation name (destructor different) */
	struct memberprops props;
	char duplicate_pr;  /* to prevent spam, already printed duplicated message */
	char parent_virtual;      /* member inherited from a virtual base class */
	char is_constructor;      /* is the constructor for this class */
	char is_root_constructor; /* is the root constructor for this class */
	char is_destructor;       /* is the destructor for this class */
	char parent_constructor;  /* is the constructor for a parent class */
	char parent_destructor;   /* is the destructor for a parent class */
	char constructor_called;  /* is a parent constructor and has been called */
	char name[];              /* declared name */
};

DEFINE_STRHASH_FIND_E(class, members, member, member)

struct variable {
	struct classptr decl;
	struct dynarr params;   /* struct classptr, for function pointer variables */
	unsigned blocklevel;
	struct hash_entry node;
	char name[];
};

enum typedef_implicit { TYPEDEF_EXPLICIT, TYPEDEF_IMPLICIT };

struct classtype {
	struct classptr decl;
	struct hash_entry node;
	char implicit;       /* implicitly declared? or user typedef'ed */
	char name[];
};

struct insert {   /* an insert, to insert some text in the output */
	char *flush_until;   /* flush output until here (expression start) */
	char *insert_text;   /* text to insert: e.g. 'this->' or 'vt->' */
	char *continue_at;   /* where to continue writing (perhaps skip something) */
	int continue_after;  /* later-insert before or after this insert? */
};

struct initializer {
	struct class *varclass; /* class (root) constructor to call, if any */
	char *params;           /* ... first parameter, to separate from "this" */
	struct dynarr inserts;  /* inserts for initialization expression */
	char *name;             /* name of variable */
	char *start;            /* start of expression (to skip originally) */
	char *end;              /* ... and its end */
	int lineno;             /* source line number where expression appeared */
};

struct disposer {
	struct class *class;    /* class to call destructor of */
	char *name;		/* name of variable */
	unsigned blocklevel;    /* { } nesting level to destuct at */
	unsigned retblocknr;    /* block number for return goto(s), disposers */
};

enum line_pragma_mode { LINE_PRAGMA_INPUT, LINE_PRAGMA_OUTPUT };

struct parse_file {
	FILE *out;                /* file writing to */
	char *filename;           /* input filename */
	char *buffer;             /* input buffer */
	char *writepos;           /* input buffer written to output till here */
	char *pos;                /* parsed input buffer till here */
	char *bufend;             /* end of input buffer */
	char *outbuffer;          /* comparing output buffer */
	char *outpos;             /* compared output until here, NULL if writing */
	char *outfilename;        /* output filename */
	char *outfilename_end;    /* pointer to null-character of output filename */
	char *newfilename_path;   /* points to start of path (last in stack) */
	char *newfilename_file;   /* points after parsing file dir in newfilename */
	char *linestart;	  /* points to line-end before currently parsing line */
	int coo_inline_defined;   /* defined the COO_INLINE macro yet? */
	int outfailed;            /* prevent error spam if creating file failed */
	int lineno;               /* line number of currently parsing line */
	int lines_coo;            /* number of (full) lines generated by this parser */
	enum line_pragma_mode line_pragma_mode;  /* refer to input file or not? */
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
	struct dynarr initializers;   /* struct initializer pointers */
	struct dynarr disposers;      /* struct disposer pointers */
	struct dynarr nested_locals;  /* struct variable pointers */
	struct dynarr inserts_arr;    /* struct insert pointers (outer layer) */
	struct dynarr includepaths;   /* char pointers */
	struct dynarr file_stack;     /* struct parse_file pointers */
	struct dynarr param_classes;  /* struct classptr *, collect params of funcs */
	struct dynarr implicit_structs;  /* char *, param positions to add "struct " */
	struct dynarr *inserts;       /* either inserts_arr or a initializer's */
	struct hash files_seen;       /* struct file_id pointers */
	int include_ext_in_len;       /* length of include_ext_in, optimization */
	int num_errors;               /* user errors, prevent spam */
	int line_pragmas;             /* print line pragmas for compiler lineno */
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

static void parser_nextline(struct parser *parser, char *pos)
{
	/* to prevent counting line-endings multiple times, or even from
	   completely different buffer (e.g. parameter parsing), compare
	   whether new position is in expected buffer range */
	if (pos > parser->pf.linestart && pos < parser->pf.bufend) {
		parser->pf.lineno++;
		parser->pf.linestart = pos;
	}
}

static void parser_check_lineend(struct parser *parser, char *pos)
{
	if (*pos == '\n')
		parser_nextline(parser, pos);
}

static char *parser_strchrnul(struct parser *parser, char *str, int ch)
{
	for (; *str; str++) {
		parser_check_lineend(parser, str);
		if (*str == ch)
			break;
	}
	return str;
}

static char *strchrnul(char *str, int ch)
{
	while (*str && *str != ch)
		str++;
	return str;
}

static int islinespace(int ch)
{
	return ch == ' ' || ch == '\t';
}

static char *rev_linestart(const char *bufstart, char *position)
{
	for (; position > bufstart; position--) {
		if (!islinespace(*(position-1)))
			break;
	}
	return position;
}

static char *rev_lineend(const char *bufstart, char *position)
{
	position = rev_linestart(bufstart, position);
	if (position == bufstart)
		return position;
	/* reverse through line ending, but only one */
	if (*(position-1) != '\n')
		return position;
	/* skip LF */
	if (--position == bufstart)
		return position;
	if (*(position-1) != '\r')
		return position;
	/* skip CR */
	return position - 1;
}

/* assumes pos[0] == '/' was matched, starting a possible comment */
static int skip_comment(struct parser *parser, char **retpos)
{
	char *pos = *retpos;
	/* C style comment? */
	if (pos[1] == '*') {
		for (pos += 2;; pos++) {
			pos = parser_strchrnul(parser, pos, '*');
			if (pos[0] == 0)
				goto moved;
			if (pos[1] == '/')
				break;
		}
		pos += 2;
		goto moved;
	}
	/* C++ style comment? */
	if (pos[1] == '/') {
		pos = parser_strchrnul(parser, pos+2, '\n');
		if (pos[0] == 0)
			goto moved;
		pos++;
		goto moved;
	}
	return 0;
moved:
	*retpos = pos;
	return 1;
}

static char *skip_whitespace(struct parser *parser, char *p)
{
	for (;;) {
		if (isspace(*p)) {
			parser_check_lineend(parser, p);
			p++;
		} else if (*p == '/') {
			if (!skip_comment(parser, &p))
				break;
		} else if (*p == '"') {
			for (;;) {
				p = parser_strchrnul(parser, p + 1, '"');
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
static char *skip_whiteline(struct parser *parser, char *p)
{
	for (;;) {
		if (islinespace(*p))
			p++;
		else if (*p == '/') {
			if (!skip_comment(parser, &p))
				break;
		} else
			break;
	}
	/* skip one line ending, not all */
	if (*p == '\r')
		p++;
	if (*p == '\n') {
		parser_nextline(parser, p);
		p++;
	}
	return p;
}

/* assumes pos[0] == '/' was matched, starting a possible comment */
static int strskip_comment(char **retpos)
{
	char *pos = *retpos;
	/* C style comment? */
	if (pos[1] == '*') {
		for (pos += 2;; pos++) {
			pos = strchrnul(pos, '*');
			if (pos[0] == 0)
				goto moved;
			if (pos[1] == '/')
				break;
		}
		pos += 2;
		goto moved;
	}
	/* C++ style comment? */
	if (pos[1] == '/') {
		pos = strchrnul(pos+2, '\n');
		if (pos[0] == 0)
			goto moved;
		pos++;
		goto moved;
	}
	return 0;
moved:
	*retpos = pos;
	return 1;
}

static char *strskip_whitespace(char *p)
{
	for (;;) {
		if (isspace(*p))
			p++;
		else if (*p == '/') {
			if (!strskip_comment(&p))
				break;
		} else
			break;
	}
	return p;
}

static char *skip_word(char *p)
{
	while (isalnum(*p) || *p == '_')
		p++;
	return p;
}

static char *skip_methodname(char *p)
{
	if (*p == '~')
		p++;
	return skip_word(p);
}

/* scan for character set, ignoring comments, include '/' and '\n' in set!
   if '/' is first in set, then skip '/' as a token
   if '\n' is second in set, then skip '\n' as token */
static char *scan_token(struct parser *parser, char *pos, char *set)
{
	for (;;) {
		pos = strpbrk(pos, set);
		if (pos == NULL)
			return NULL;
		if (pos[0] == '\n') {
			parser_nextline(parser, pos);
			if (set[1] != '\n')
				return pos;
			pos++;
			continue;
		}
		if (pos[0] != '/')
			return pos;
		if (!skip_comment(parser, &pos)) {
			/* no comment detected, do we want this '/' as token? */
			if (set[0] != '/')
				return pos;
			/* no, skip it */
			pos++;
		} else if (pos[0] == 0)
			return NULL;
	}
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

static int grow_dynarr_to(struct dynarr *dynarr, unsigned minimum)
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

void *allocdynitem_size(struct dynarr *dynarr, size_t itemsize)
{
	void *item;

	if (grow_dynarr(dynarr) < 0)
		return NULL;

	item = dynarr->mem[dynarr->num];
	if (item == NULL) {
		item = calloc(1, itemsize);
		if (item == NULL)
			return NULL;

		dynarr->mem[dynarr->num] = item;
	}

	dynarr->num++;
	return item;
}

#define allocdynitem(arr, pp_item) (*(pp_item) = allocdynitem_size(arr, sizeof(**(pp_item))))

/*** print helpers ***/

static void print_message(struct parser *parser,
		char *pos, char *severity, char *message, ...)
{
	char msgbuf[256];
	va_list va_args;
	int colnr;

	if (parser->num_errors == MAX_USER_ERRORS)
		return;

	parser->num_errors += severity[0] == 'e';
	va_start(va_args, message);
	vsnprintf(msgbuf, sizeof(msgbuf), message, va_args);
	va_end(va_args);
	if (pos > parser->pf.linestart && pos < parser->pf.bufend)
		colnr = (int)(pos - parser->pf.linestart);
	else
		colnr = 1;
	fprintf(stderr, "%s:%d:%d: %s: %s\n", parser->pf.filename,
		parser->pf.lineno, colnr, severity, msgbuf);
}

#define pr_err(pos, ...) print_message(parser, pos, "error", __VA_ARGS__)
#define pr_warn(pos, ...) print_message(parser, pos, "warning", __VA_ARGS__)

static int prepare_output_file(struct parser *parser)
{
	/* outfilename already has the temporary extension in buffer, but has
	   null character in place of '.<pid>' to buffer output first */
	*parser->pf.outfilename_end = '.';
	parser->pf.out = fopen(parser->pf.outfilename, "wb");
	if (parser->pf.out == NULL) {
		fprintf(stderr, "%s: could not create\n", parser->pf.outfilename);
		parser->pf.outfailed = 1;
		return -1;
	}

	/* restore original output filename for #line pragmas */
	*parser->pf.outfilename_end = 0;
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

static void attr_format(2,3) outprintf(struct parser *parser, const char *format, ...)
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
		parser->num_errors++;
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

static void print_implicit_struct(struct parser *parser,
		struct classtype *classtype, char *position)
{
	/* print "struct " if this type X was implicitly declared by struct X {};
	   in this case the C compiler does not know it, so we need to add "struct " */
	if (!classtype->implicit)
		return;
	/* in structs, skip return types of functions that we don't print anyway */
	if (parser->pf.writepos > position)
		return;

	parser->pf.pos = position;
	flush(parser);
	outputs(parser, "struct ");
}

void switch_line_pragma(struct parser *parser, enum line_pragma_mode newmode)
{
	char *filename;
	int lineno;

	if (!parser->line_pragmas || parser->pf.line_pragma_mode == newmode)
		return;

	parser->pf.line_pragma_mode = newmode;
	/* update lines_coo before printing because needs to match next line */
	parser->pf.lines_coo++;
	lineno = parser->pf.lineno;
	/* note that line pragmas must be relative from current directory!
	   you might expect relative from file location, but no */
	if (newmode == LINE_PRAGMA_INPUT) {
		filename = parser->pf.filename;
	} else {
		filename = parser->pf.outfilename;
		lineno += parser->pf.lines_coo;
	}
	outprintf(parser, "\n#line %d \"%s\"", lineno, filename);
}

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

static struct class *initclass(struct parser *parser, struct class *class)
{
	strhash_init(&class->members, 8, member);
	hasho_init(&class->ancestors, 8);
	if (hash_insert(&parser->classes, &class->node, strhash(class->name))) {
		/* already exists */
		free(class);
		class = NULL;
	}
	return class;
}

static struct class *addclass(struct parser *parser, char *classname, char *nameend)
{
	struct class *class;

	class = alloc_namestruct(struct class, classname, nameend);
	if (class == NULL)
		return NULL;

	return initclass(parser, class);
}

static struct classtype *addclasstype(struct parser *parser, char *typename,
		char *nameend, struct classptr *decl, enum typedef_implicit implicit)
{
	struct classtype *type;

	type = alloc_namestruct(struct classtype, typename, nameend);
	if (type == NULL)
		return NULL;

	type->decl = *decl;
	type->implicit = implicit;
	if (strhash_insert(&parser->classtypes, type)) {
		/* already exists */
		free(type);
		type = NULL;
	}
	return type;
}

typedef void (*insert_implicit_cb)(struct parser *parser, char *position);
typedef void (*new_param_cb)(struct parser *parser, int position,
		struct classptr *decl, char *name, char *next);

static char *parse_parameters(struct parser *parser, char *params,
		insert_implicit_cb new_implicit, new_param_cb new_param)
{
	struct classtype *classtype;
	struct classptr decl;
	char *next, *curr, *name;
	enum parse_state state;
	int position;

	decl.pointerlevel = 0;
	state = FINDVAR;
	for (position = 0, next = params;;) {
		curr = strskip_whitespace(next);
		if (curr[0] == 0)
			return NULL;
		if (strprefixcmp("const ", curr)) {
			next = curr + 6;  /* "const " */
			continue;
		} else if (strprefixcmp("struct ", curr)) {
			name = strskip_whitespace(curr + 7);  /* "struct " */
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
				if (classtype->implicit) {
					if (new_implicit)
						new_implicit(parser, name);
					else
						print_implicit_struct(parser, classtype, name);
				}
				decl = classtype->decl;
				state = DECLVAR;
			}
			break;
		}
	}
}

static int insertmember(struct class *class, struct member *member, struct member **dupmember)
{
	struct hash_entry *entry;

	if (grow_dynarr(&class->members_arr) < 0)
		return -1;
	/* check already exists */
	if ((entry = strhash_insert(&class->members, member))) {
		if (hash_insert_nomem(entry))
			return -1;
		*dupmember = container_of(entry, struct member, node);
		return -1;
	}

	class->members_arr.mem[class->members_arr.num] = member;
	class->members_arr.num++;
	return 0;
}

static struct member *allocmember_e(struct class *class,
		char *membername, char *nameend, struct member **dupmember)
{
	struct member *member;

	member = alloc_namestruct(struct member, membername, nameend);
	if (member == NULL)
		return NULL;
	/* translate destructor ~ character */
	member->implname = member->name;
	if (membername[0] == '~') {
		member->implprefix = "d_";
		member->implname++;
	} else {
		member->implprefix = "";
	}
	if (insertmember(class, member, dupmember) < 0)
		goto err;
	return member;
err:
	free(member);
	return NULL;
}

static struct member *dupmemberto(struct class *class,
		struct member *parentmember, struct member **dupmember)
{
	int size = offsetof(struct member, name) + strlen(parentmember->name) + 1;
	struct member *member;

	member = malloc(size);
	if (member == NULL)
		return NULL;

	memcpy(member, parentmember, size);
	if (insertmember(class, member, dupmember) < 0)
		goto err;

	return member;
err:
	free(member);
	return NULL;
}

static void save_implicit_insert(struct parser *parser, char *position)
{
	if (grow_dynarr(&parser->implicit_structs))
		return;

	parser->implicit_structs.mem[parser->implicit_structs.num++] = position;
}

static void save_param_class(struct parser *parser, int position,
		struct classptr *decl, char *name, char *next)
{	(void)name; (void)next;
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

static void addmember_to_children(struct parser *parser, char *parsepos,
		struct class *class, struct member *member);

static char *insert_implicit_structs(struct parser *parser, char *src, char *srcend)
{
	char *dest, *insertpos, *params_new;
	unsigned i, remaining;

	params_new = malloc(srcend - src + 1 + parser->implicit_structs.num * 7);  /* "struct " */
	if (params_new == NULL)
		return NULL;

	for (dest = params_new, i = 0; i < parser->implicit_structs.num; i++) {
		insertpos = parser->implicit_structs.mem[i];
		memcpy(dest, src, insertpos - src);
		dest += insertpos - src;
		memcpy(dest, "struct ", 7);
		dest += 7;
		src = insertpos;
	}
	/* src moved in meanwhile, calculate remaining */
	remaining = srcend - src;
	memcpy(dest, src, remaining);
	dest[remaining] = 0;
	parser->implicit_structs.num = 0;
	return params_new;
}

static void parse_parameters_to(struct parser *parser,
	char *params, char **params_out, struct dynarr *param_classes)
{
	char *paramsend;

	/* save_param_class stores params in parser->param_classes */
	paramsend = parse_parameters(parser, params,
		params_out ? save_implicit_insert : NULL, save_param_class);
	/* move the dynarr (array pointer etc) to requested array */
	memcpy(param_classes, &parser->param_classes, sizeof(*param_classes));
	memset(&parser->param_classes, 0, sizeof(parser->param_classes));
	if (params_out)
		*params_out = insert_implicit_structs(parser, params, paramsend);
}

static char is_void_params(char *params)
{
	params = strskip_whitespace(params);
	if (*params == ')')
		return 1;
	if (strprefixcmp("void", params) == NULL)
		return 0;
	return *strskip_whitespace(params + 4) == ')';
}

static char is_void_rettype(char *rettype)
{
	char *end = strprefixcmp("void", rettype);
	if (!end)
		return 0;
	if (!isspace(*end))
		return *end == 0;
	do {
		end++;
	} while (isspace(*end));
	return *end != '*';
}

static struct member *addmember(struct parser *parser, struct class *class,
		struct classtype *rettype, char *retstr, char *retend,
		char *membername, char *nameend, char *params, struct memberprops props)
{
	struct member *member, *dupmember;
	char *vmt_insert, *end, void_ret;

	member = allocmember_e(class, membername, nameend, &dupmember);
	if (member == NULL) {
		*nameend = 0;
		if (dupmember) {
			pr_err(membername, "duplicate member %s, inherited from %s,"
				" did you mean override?", membername,
				dupmember->origin->name);
		}
		return NULL;
	}

	member->origin = class;
	member->definition = class;
	member->props = props;
	if (props.is_abstract)
		class->num_abstract++;
	member->retclassptr = rettype->decl;
	if (params) {
		if (is_void_params(params))
			member->paramstext = ")";
		else
			parse_parameters_to(parser, params, &member->paramstext, &member->params);
	}
	if (props.is_function || props.is_static) {
		vmt_insert = props.is_virtual ? "vmt_" : "";
		member->funcinsert = aprintf("%s_%s", class->name, vmt_insert);
	}
	if (props.is_virtual) {
		member->vmt = class->vmt;
		class->vmt->modified = 1;
	}
	end = strprefixcmp(class->name, membername);
	if (end) {
		member->is_constructor = *end != '_' && !isalnum(*end);
		end = strprefixcmp("_root", end);
		member->is_root_constructor = end ? !isalnum(*end) : 0;
		if (member->is_constructor || member->is_root_constructor) {
			if (class->declare_complete)
				pr_err(membername, "(root) constructor must be declared in class");
			if (!props.is_function) {
				pr_err(membername, "constructor must be a function");
				/* don't bother anymore, prevent problems */
				class->need_root_constructor = 0;
			} else {
				void_ret = retstr == retend;
				if (!void_ret)
					void_ret = is_void_rettype(retstr);
				/* even if constructor is void in COO-speak, then in C
				   translation still return class pointer for chaining */
				if (void_ret) {
					member->rettype = aprintf("struct %s *", class->name);
					member->retclassptr.class = class;
					member->retclassptr.pointerlevel = 1;
				}
				else if (rettype->decl.class != class
						|| rettype->decl.pointerlevel != 1)
					pr_err(retstr, "constructor must be void or return "
						"pointer to its own class type");
				if (member->is_constructor) {
					class->void_constructor = void_ret;
					class->constructor = member;
					class->has_constructor = 1;
				} else {
					class->void_root_constructor = void_ret;
					class->root_constructor = member;
				}
			}
		}
	} else if (member->name[0] == '~') {
		if (!strcmp(class->name, &member->name[1])) {
			if (class->declare_complete)
				pr_err(membername, "destructor must be declared in class");
			if (retstr != retend)
				pr_err(membername, "return type is not allowed for destructor");
			member->is_destructor = 1;
			class->has_destructor = 1;
			class->destructor = member;
			class->freer_class = class;
		} else {
			pr_err(membername, "invalid member name, "
				"did you mean ~%s?", class->name);
		}
		member->rettype = "void ";
	}

	if (!member->rettype) {
		if (rettype->decl.class && rettype->implicit) {
			member->rettype = malloc(retend - retstr + 8);  /* "struct " + null-term */
			memcpy(member->rettype, "struct ", 7);
			memcpy(member->rettype + 7, retstr, retend - retstr);
			member->rettype[7 + retend - retstr] = 0;
		} else if (retend == retstr) {
			member->rettype = "";
		} else
			member->rettype = stredup(retstr, retend);
	}

	/* in case of defining functions on the fly,
	   insert into classes inheriting from this class as well */
	addmember_to_children(parser, membername, class, member);
	return member;
}

void addgenmember(struct parser *parser, struct class *class,
		struct member *mimic, char *name, char *nameend)
{
	struct memberprops constructorprops = {0,};
	struct classtype rettype;

	constructorprops.is_function = 1;
	rettype.implicit = 0;
	rettype.decl.class = NULL;
	rettype.decl.pointerlevel = 0;
	/* retstr/retend NULL is detected as not present => use void behavior */
	addmember(parser, class, &rettype, NULL, NULL,
		name, nameend, mimic ? mimic->paramstext : ")", constructorprops);
}

static struct variable *addvariable(struct parser *parser, unsigned blocklevel,
		struct classptr *decl, int extraptr,
		char *membername, char *nameend, char *params)
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
	if (strhash_insert(&parser->locals, variable)) {
		/* no need to free variable, ref stored in dynarr */
		return NULL;
	}

	variable->decl = *decl;
	variable->decl.pointerlevel += extraptr;  /* typedef type + extra pointers */
	variable->blocklevel = blocklevel;
	if (params) {
		parse_parameters(parser, params, NULL, save_param_class);
		memcpy(&variable->params, &parser->param_classes, sizeof(variable->params));
	}
	dynarr->num++;
	return variable;
}

/* compatible with struct insert.continue_after */
enum insert_continue { CONTINUE_BEFORE, CONTINUE_AFTER };

static int addinsert(struct parser *parser, int insert_index,
		char *flush_until, char *insert_text, char *continue_at,
		enum insert_continue insert_continue)
{
	struct insert *insert, **before_pos, **end_pos;

	if (grow_dynarr(parser->inserts) < 0)
		return -1;

	end_pos = (struct insert **)&parser->inserts->mem[parser->inserts->num];
	insert = *end_pos;
	if (!insert) {
		insert = calloc(1, sizeof(*insert));
		if (insert == NULL)
			return -1;
		*end_pos = insert;
	}

	insert->flush_until = flush_until;
	insert->insert_text = insert_text;
	insert->continue_at = continue_at;
	insert->continue_after = insert_continue;
	parser->inserts->num++;
	if (insert_index < 0) {
		/* check flush ordering to be incremental */
		for (before_pos = end_pos;; before_pos--) {
			if ((void**)before_pos == parser->inserts->mem)
				break;
			if (before_pos[-1]->continue_at < flush_until)
				break;
			if (before_pos[-1]->continue_at == flush_until
					&& before_pos[-1]->continue_after)
				break;
		}
	} else {
		before_pos = (struct insert **)&parser->inserts->mem[insert_index];
	}

	if (before_pos < end_pos) {
		memmove(before_pos+1, before_pos, (char*)end_pos-(char*)before_pos);
		*before_pos = insert;
	}
	/* return index of next entry to insert before */
	return (void**)before_pos + 1 - parser->inserts->mem;
}

static void addinsert_implicit_struct(struct parser *parser,
		struct classtype *classtype, char *position)
{
	/* print "struct " if this type X was implicitly declared by struct X {};
	   in this case the C compiler does not know it, so we need to add "struct " */
	if (!classtype->implicit)
		return;

	addinsert(parser, -1, position, "struct ", position, CONTINUE_BEFORE);
}

static int insert_text(struct parser *parser, int insert_index,
		char *pos, char *text, char *continue_at,
		enum insert_continue insert_continue)
{
	if (parser->inserts->num)
		return addinsert(parser, insert_index,
			pos, text, continue_at, insert_continue);
	flush_until(parser, pos);
	outputs(parser, text);
	parser->pf.writepos = continue_at;
	return 0;
}

static struct initializer *addinitializer(struct parser *parser,
		struct class *varclass, char *name, char *start)
{
	struct initializer *initializer;

	if (!allocdynitem(&parser->initializers, &initializer))
		return NULL;

	initializer->lineno = parser->pf.lineno;
	initializer->varclass = varclass;
	initializer->params = NULL;
	initializer->name = name;
	initializer->start = start;
	parser->inserts = &initializer->inserts;
	return initializer;
}

static struct disposer *adddisposer(struct parser *parser,
		struct class *class, char *name, unsigned blocklevel, unsigned retblocknr)
{
	struct disposer *disposer;

	if (!class->destructor)
		return NULL;
	if (!allocdynitem(&parser->disposers, &disposer))
		return NULL;

	disposer->class = class;
	disposer->name = name;
	disposer->blocklevel = blocklevel;
	disposer->retblocknr = retblocknr;
	return disposer;
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

static void remove_locals(struct parser *parser, unsigned blocklevel)
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

static void pr_lineno(struct parser *parser, int lineno)
{
	if (parser->line_pragmas && parser->pf.line_pragma_mode == LINE_PRAGMA_INPUT) {
		outprintf(parser, "\n#line %d", lineno);
		parser->pf.lines_coo++;
	}
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

void parse_type(struct parser *parser, char *pos,
		char **retnext, struct classtype *rettype)
{
	struct classtype *classtype;
	char *name, *next;

	rettype->implicit = 0;
	rettype->decl.pointerlevel = 0;
	for (rettype->decl.class = NULL;; pos = skip_whitespace(parser, pos)) {
		if (strprefixcmp("const ", pos) != NULL) {
			pos += 6;  /* const */
			continue;
		}
		/* not necessary to parse unsigned/signed/int properly,
		   (1) retnext is only used to parse typedef
		   (1b) retnext usage is only interested in class types
		   (2) use context like '(' and ';' to find member names */
		if (strprefixcmp("struct ", pos)) {
			name = skip_whitespace(parser, pos + 7);  /* "struct " */
			next = skip_word(name);
			rettype->decl.class = find_class_e(parser, name, next);
			rettype->decl.pointerlevel = 0;
			break;
		} else {
			if (isalpha(*pos)) {
				next = skip_word(pos);
				if ((classtype = find_classtype_e(parser, pos, next)) != NULL) {
					print_implicit_struct(parser, classtype, pos);
					rettype->decl = classtype->decl;
					rettype->implicit = classtype->implicit;
				}
			} else
				next = pos;
			break;
		}
	}

	for (next = skip_whitespace(parser, next); *next == '*'; next++)
		rettype->decl.pointerlevel++;
	*retnext = next;
}

static int is_abstract(struct parser *parser, char **pos)
{
	*pos = skip_whitespace(parser, *pos + 1);
	if (**pos != '0')
		return 0;
	*pos = skip_whitespace(parser, *pos + 1);
	return **pos == ';';
}

static struct member *inheritmember(struct parser *parser, char *parsepos,
		struct class *class, struct parent *parent, struct member *parentmember)
{
	struct member *member, *dupmember = NULL;

	member = dupmemberto(class, parentmember, &dupmember);
	if (member == NULL) {
		if (dupmember) {
			pr_err(parsepos, "duplicate member %s from parent "
				"class %s and %s", parentmember->name,
				dupmember->origin->name, parent->class->name);
		}
		goto err;
	}

	/* only primary if this and inherited are all primary */
	member->props.from_primary &= parent->is_primary;
	member->props.from_virtual |= parent->is_virtual;
	member->parent_constructor = parentmember->is_constructor;
	member->parent_destructor = parentmember->is_destructor;
	if (member->props.is_abstract)
		class->num_abstract++;
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
	addmember_to_children(parser, parsepos, class, member);
	return member;
err:
	free(member);
	return NULL;
}

static void addmember_to_children(struct parser *parser, char *parsepos,
		struct class *class, struct member *member)
{
	struct parent *parent;
	unsigned i;

	for (i = 0; i < class->descendants.num; i++) {
		parent = class->descendants.mem[i];
		inheritmember(parser, parsepos, parent->child, parent, member);
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

static void mergemember(struct parser *parser, struct class *class, struct member *parentmember)
{
	struct member *member;
	char *nameend;

	/* if new parent did not override, there is no ambiguity anyway */
	if (parentmember->definition == parentmember->origin)
		return;

	nameend = parentmember->name + strlen(parentmember->name);
	member = find_member_e(class, parentmember->name, nameend);
	if (member == NULL) {
		fprintf(stderr, "internal error, cannot find member %s in"
			"class %s for merging\n", parentmember->name, class->name);
		parser->num_errors++;
		return;
	}

	/* if both parents override this member, mark member as to-be overridden */
	if (member->definition != member->origin) {
		class->has_duplicates = 1;
		member->definition = NULL;
	} else {
		/* did not override yet, use new parent's definition */
		member->definition = parentmember->definition;
		if (member->props.is_abstract) {
			member->props.is_abstract = 0;
			class->num_abstract--;
		}
	}
}

static void add_ancestor(struct class *class, int from_virtual,
		struct parent *origin_parent,
		char *via_class_name, char *link, char *path)
{
	struct hasho_entry *orig_entry;
	struct ancestor *ancestor, *orig_ancestor;

	/* allocate struct and printf path in one alloc */
	ancestor = (void*)aprintf("%*s%s%s%s", (int)offsetof(struct ancestor, path),
			"", via_class_name, link, path);
	if (ancestor == NULL)
		return;

	ancestor->next = NULL;
	ancestor->parent = origin_parent;
	ancestor->from_virtual = from_virtual;
	/* duplicates may occur for virtual bases */
	if (hasho_insert_exists(&class->ancestors, origin_parent->class,
			ancestor, orig_entry)) {
		/* store primary base, not the virtual one */
		orig_ancestor = orig_entry->value;
		if (!origin_parent->is_virtual && orig_ancestor->parent->is_virtual) {
			/* replace old virtual one with new literal one */
			ancestor->next = orig_entry->value;
			orig_entry->value = ancestor;
		} else {
			/* either new one virtual or both literal, keep first literal
			   ancestor first, rest don't care; need all primary and all
			   virtual ancestors to print root constructor */
			ancestor->next = orig_ancestor->next;
			orig_ancestor->next = ancestor;
		}
	}
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
		do {
			from_virtual = ancestor->from_virtual | parent->is_virtual;
			add_ancestor(class, from_virtual, ancestor->parent, parentclass->name,
				parent->is_virtual ? "->" : ".", ancestor->path);
			ancestor = ancestor->next;
		} while (ancestor);
	}
}

static const char *get_vmt_name(struct vmt *vmt)
{
	char *vmt_sec_name, *vmt_sec_sep;
	struct vmt *impl_vmt;   /* implemented vmt */

	if (vmt->name)
		return vmt->name;

	/* no name yet, find most descendent class that modified the vmt */
	for (impl_vmt = vmt; !impl_vmt->modified; impl_vmt = impl_vmt->parent)
		if (!impl_vmt->parent)
			break;

	/* secondary vmt? append origin class name to distiniguish from primary */
	if (!impl_vmt->is_primary) {
		vmt_sec_name = impl_vmt->origin->name;
		vmt_sec_sep = "_";
	} else
		vmt_sec_name = vmt_sec_sep = "";

	/* generate vmt name, this vmt must be defined, because it is marked modified */
	vmt->name = aprintf("%s%s%s_vmt", impl_vmt->class->name, vmt_sec_sep, vmt_sec_name);
	return vmt->name;
}

static int vmt_this_needs_offset(struct member *member)
{
	/* for non-primary, the class does not align to start, so have
	   to use the original defining class and translate pointer in implementation */
	return member->vmt && (!member->props.from_primary || member->props.from_virtual);
}

static int impl_this_needs_offset(struct member *member)
{
	/* for virtual origin, we cannot translate, so that has to use trampoline */
	return member->vmt && !member->props.from_primary && !member->props.from_virtual;
}

static struct class *get_vmt_this_class(struct member *member)
{
	return vmt_this_needs_offset(member) ? member->vmt->origin : member->definition;
}

static struct class *get_impl_this_class(struct member *member)
{
	return impl_this_needs_offset(member) ? member->vmt->origin : member->definition;
}

enum parent_virtual { LITERAL_PARENT, VIRTUAL_PARENT };  /* boolean compatible */

static void addvmt(struct class *class, struct vmt *parent_vmt,
		struct class *origin, enum parent_virtual parent_virtual)
{
	struct vmt *vmt;

	if (grow_dynarr(&class->vmts) < 0)
		return;

	vmt = calloc(1, sizeof(*vmt));
	if (vmt == NULL)
		return;

	vmt->class = class;
	vmt->origin = origin;
	vmt->parent = parent_vmt;
	vmt->modified = class == origin;
	/* do not reuse virtual base class' vmt, is inefficient */
	vmt->is_primary = class->vmt == NULL;
	class->vmts.mem[class->vmts.num++] = vmt;
	if (parent_vmt)
		vmt->is_primary = vmt->is_primary && parent_vmt->is_primary && !parent_virtual;
	if (vmt->is_primary)
		class->vmt = vmt;
}

static void import_parent(struct parser *parser, char *parsepos,
		struct class *class, struct parent *parent)
{
	struct member *parentmember;
	struct dynarr *parentmembers;
	struct class *origin, *currorigin, *ignoreorigin, *intforigin, *mergeorigin;
	struct class *parentclass = parent->class;
	struct vmt *vmt;
	unsigned i;
	int ret;

	parentmembers = &parentclass->members_arr;
	currorigin = ignoreorigin = intforigin = mergeorigin = NULL;
	for (i = 0; i < parentmembers->num; i++) {
		parentmember = parentmembers->mem[i];
		/* hide grand-parent constructors behind parent constructor, except
		   the ones from virtual bases, those are called from root constr. */
		if (parentmember->parent_constructor && parentclass->has_constructor
				&& !parentmember->props.from_virtual)
			continue;
		if (parentmember->parent_destructor && parentclass->has_destructor
				&& !parentmember->props.from_virtual)
			continue;
		/* skip root constructors, they are specific for their own class */
		if (parentmember->is_root_constructor && parentmember->origin->rootclass)
			continue;
		origin = parentmember->origin;
		if (origin == ignoreorigin)
			continue;
		if (origin == intforigin)
			goto check_intf;
		/* count parent constructors to be called, skip constructors for virtual
		   base classes, those are to be called from root constructor */
		class->num_parent_constr +=
			parentmember->is_constructor && !parentmember->props.from_virtual;
		class->num_parent_destr +=
			parentmember->is_destructor && !parentmember->props.from_virtual;
		if (origin == mergeorigin)
			goto merge;
		if (origin != currorigin) {
			currorigin = origin;
			/* if we already imported this class virtual, or
			   this import is virtual, then don't inherit
			   the members again (prevent duplicates) */
			ret = is_virtual_base(origin, class);
			if (ret == 1 || (ret == 0 && parentmember->props.from_virtual)) {
				mergeorigin = origin;
				goto merge;
			}
			/* class inherited indirectly, e.g. B:A, C:A, D:B,C */
			if (ret == 0) {
				/* potentially an interface, prepare and check */
				intforigin = origin;
				goto check_intf;
			}
		}

		inheritmember(parser, parsepos, class, parent, parentmember);
		/* write to output later, when opening brace '{' parsed */
		continue;
	  check_intf:
		if (!parentmember->props.is_function) {
			/* statics have global lifetime, accept and skip them */
			if (parentmember->props.is_static)
				continue;
			/* duplicate member fields are unacceptable */
			goto err_duplicate;
		}
		/* for virtual methods, check where overridden */
		if (parentmember->props.is_virtual)
			goto merge;
		/* normal methods we already have, skip them */
		continue;
	  merge:
		mergemember(parser, class, parentmember);
		continue;
	  err_duplicate:
		pr_err(parsepos, "inherit duplicate class %s, non-virtual field %s",
				origin->name, parentmember->name);
		ignoreorigin = origin;
	}

	/* only add class after origin checks */
	if (grow_dynarr(&parentclass->descendants) < 0)
		return;

	/* inherit vmts */
	for (i = 0; i < parentclass->vmts.num; i++) {
		vmt = parentclass->vmts.mem[i];
		/* if we already have this origin, then also its vmt: skip it */
		if (hasho_find(&class->ancestors, vmt->origin))
			continue;

		addvmt(class, vmt, vmt->origin, parent->is_virtual);
	}

	/* add parents after vmt duplication check */
	add_ancestors(class, parent);
	parentclass->descendants.mem[parentclass->descendants.num++] = parent;
}

static struct member *implmember(struct parser *parser, char *parsepos,
		struct class *class, char *membername, char *nameend)
{
	struct class *vmt_origin;
	struct member *member;
	struct vmt *vmt;
	unsigned i;

	member = find_member_e(class, membername, nameend);
	if (member == NULL) {
		pr_err(parsepos, "cannot find member to override");
		return NULL;
	}

	if (!member->props.is_virtual) {
		pr_err(parsepos, "inherited member is not virtual");
		return NULL;
	}

	member->definition = class;
	vmt_origin = member->vmt->origin;
	if (member->props.is_abstract) {
		member->props.is_abstract = 0;
		class->num_abstract--;
	}
	/* find same vmt in this class (same origin) */
	for (i = 0;; i++) {
		if (i == class->vmts.num) {
			pr_err(parsepos, "(ierr) vmt for %s not found", member->name);
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

static void print_func_header(struct parser *parser,
		struct class *class, struct member *member)
{
	char *paramstext, *sep_or_end;

	paramstext = member->paramstext;
	sep_or_end = paramstext[0] != ')' ? ", " : "";
	outprintf(parser, "\n%s%s_%s%s(struct %s *this%s%s\n{\n",
		member->rettype, class->name, member->implprefix, member->implname,
		class->name, sep_or_end, paramstext);
}

static int is_member_lit_var(struct class *class, struct member *member)
{
	return !member->props.is_function && member->retclassptr.class
		&& member->retclassptr.pointerlevel == 0
		&& member->origin == class;
}

static int member_needs_init(struct class *class, struct member *member)
{
	return is_member_lit_var(class, member)
		&& member->retclassptr.class->root_constructor;
}

static int member_needs_dispose(struct class *class, struct member *member)
{
	return is_member_lit_var(class, member)
		&& member->retclassptr.class->destructor;
}

static struct rootclass *addrootclass(struct parser *parser, struct class *parentclass)
{
	struct rootclass *class;
	int namelen = strlen(parentclass->name) + 6;  /* "_root\0" */

	class = calloc(1, offsetof(struct rootclass, class.name) + namelen);
	if (class == NULL)
		return NULL;

	sprintf(class->class.name, "%s_root", parentclass->name);
	class->parent.class = parentclass;
	class->parent.child = &class->class;
	class->parent.is_primary = 1;
	class->class.is_rootclass = 1;
	return initclass(parser, &class->class) ? class : NULL;
}

static void print_root_classes(struct parser *parser, struct class *class)
{
	struct hasho_entry *entry;
	struct ancestor *ancestor;
	struct class *parentclass, *rootclass;
	struct parent *parent;
	char *name;

	if (class->num_abstract)
		return;

	rootclass = NULL;
	hasho_foreach(entry, &class->ancestors) {
		ancestor = entry->value;
		if (ancestor->parent->is_virtual) {
			if (rootclass == NULL) {
				name = class->name;
				switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
				outprintf(parser, "\nstruct %s_root {\n"
					"\tstruct %s %s;\n", name, name, name);
				/* 3 lines here, 1 for closing '}' */
				parser->pf.lines_coo += 4;
				class->rootclass = addrootclass(parser, class);
				if (class->rootclass == NULL)
					break;

				rootclass = &class->rootclass->class;
				/* copy root constructor for initialization stack var */
				rootclass->root_constructor = class->root_constructor;
				rootclass->void_root_constructor = class->void_root_constructor;
			}

			parent = calloc(1, sizeof(*parent));
			if (parent == NULL)
				break;

			parentclass = ancestor->parent->class;
			parent->class = parentclass;
			parent->child = rootclass;
			import_parent(parser, parser->pf.pos, rootclass, parent);
			name = parentclass->name;
			outprintf(parser, "\tstruct %s %s;\n", name, name);
			parser->pf.lines_coo++;
			if (!parentclass->void_constructor) {
				class->missing_root = parentclass;
				/* don't bother anymore */
				class->gen_root_constructor = 0;
			}
		}
	}

	if (rootclass) {
		/* import parent class last, so the literals get priority when resolving */
		import_parent(parser, parser->pf.pos, rootclass, &class->rootclass->parent);
		outputs(parser, "};\n");
		/* newline for lines_coo counted above */
	}
}

static void print_vmt_type(struct parser *parser, struct class *class)
{
	struct class *thisclass;
	struct member *member;
	struct vmt *vmt;
	unsigned i, j;

	for (i = 0; i < class->vmts.num; i++) {
		vmt = class->vmts.mem[i];
		if (!vmt->modified)
			continue;

		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		outprintf(parser, "\nextern struct %s {\n", get_vmt_name(vmt));
		/* 2 lines here, 1 to close struct */
		parser->pf.lines_coo += 3;
		for (j = 0; j < class->members_arr.num; j++) {
			member = class->members_arr.mem[j];
			if (member->vmt == NULL)
				continue;
			if (member->vmt->origin != vmt->origin)
				continue;

			thisclass = get_vmt_this_class(member);
			outprintf(parser, "\t%s(*%s%s)(struct %s *this%s%s;\n",
				member->rettype, member->implprefix, member->implname,
				thisclass->name, member->paramstext[0] != ')' ? ", " : "",
				member->paramstext);
			parser->pf.lines_coo++;
		}
		outprintf(parser, "} %s;\n", get_vmt_name(vmt));
	}
}

static struct ancestor *find_vmt_path(struct class *class, struct vmt *vmt,
			char **ret_vmtpath, char **ret_vmtaccess)
{
	struct ancestor *ancestor;

	ancestor = hasho_find(&class->ancestors, vmt->origin);
	if (!ancestor) {
		*ret_vmtpath = *ret_vmtaccess = "";
		return NULL;
	}

	*ret_vmtpath = ancestor->path;
	*ret_vmtaccess = ancestor->parent->is_virtual ? "->" : ".";
	return ancestor->next;
}

static void print_param_names(struct parser *parser, char *params)
{
	char *p, *last_word, *last_end;

	for (p = params;;) {
		last_word = NULL;
		/* search for last word, assume it is parameter name */
		while (*p != ')' && *p != ',') {
			if (*p == '/') {
				if (!skip_comment(parser, &p))
					p++;
			} else if (isalpha(*p)) {
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
		p = skip_whitespace(parser, p+1);
	}
}

enum func_decltype {
	MEMBER_FUNCTION,
	VIRTUAL_WRAPPER,
};

static void print_func_decl(struct parser *parser, struct class *class,
		struct member *member, enum func_decltype emittype)
{
	char *func_prefix, *name_insert, *func_body, *vmtpath, *vmtaccess, *rootclass;
	struct class *thisclass;
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
	thisclass = get_impl_this_class(member);
	rootclass = member->is_root_constructor && class->rootclass ? "_root" : "";
	outprintf(parser, "\n%s%s%s_%s%s%s(struct %s%s *this%s%s%s", func_prefix,
		member->rettype, class->name, name_insert,
		member->implprefix, member->implname, thisclass->name, rootclass,
		empty ? "" : ", ", member->paramstext, func_body);
	parser->pf.lines_coo++;
	if (emittype == VIRTUAL_WRAPPER) {
		find_vmt_path(class, member->vmt, &vmtpath, &vmtaccess);
		outprintf(parser, "\n\t((struct %s_vmt*)this->%s%svmt)->%s%s(this",
			class->name, vmtpath, vmtaccess, member->implprefix, member->implname);
		print_param_names(parser, member->paramstext);
		outprintf(parser, ");\n}\n");
		parser->pf.lines_coo += 4;  /* 1 in func_body, 3 here */
	}
}

static void print_member_decls(struct parser *parser, struct class *class)
{
	struct class* freer_class;
	struct ancestor *ancestor;
	struct member *member;
	char *params;
	unsigned i;

	if (class->vmt && !parser->pf.coo_inline_defined) {
		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		outputs(parser, "\n"DEF_COO_INLINE);
		parser->pf.coo_inline_defined = 1;
		parser->pf.lines_coo += NUM_LINES_DEF_COO_INLINE + 1;
	}

	if (class->root_constructor) {
		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		params = class->root_constructor->paramstext;
		params = params[0] == ')' ? "void)" : params;
		outprintf(parser, "\nstruct %s *new_%s(%s;", class->name,
			class->name, params);
		parser->pf.lines_coo++;
	}
	freer_class = class->freer_class;
	if (freer_class == class) {
		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		outprintf(parser, "\nvoid free_%s(struct %s *this);",
			class->name, class->name);
		parser->pf.lines_coo++;
	} else if (freer_class) {
		ancestor = hasho_find(&class->ancestors, freer_class);
		if (ancestor) {
			switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
			outprintf(parser, "\n#define free_%s(this) free_%s(%s(this)->%s)",
				class->name, freer_class->name,
				ancestor->parent->is_virtual ? "" : "&", ancestor->path);
			parser->pf.lines_coo++;
		}
	}

	for (i = 0; i < class->members_arr.num; i++) {
		member = class->members_arr.mem[i];
		/* don't print inherited members (that we did not override) */
		if (member->definition != class)
			continue;
		if (member->props.is_abstract)
			continue;
		if (!member->props.is_function && !member->props.is_static)
			continue;

		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		if (member->props.is_function) {
			print_func_decl(parser, class, member, MEMBER_FUNCTION);
		} else {
			outprintf(parser, "\nextern %s%s_%s;",
				member->rettype, class->name, member->name);
			parser->pf.lines_coo++;
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
	struct memberprops memberprops = {0,};
	struct classptr decl;
	char *declbegin, *retend, *membername, *nameend, *params, *declend, *nextdecl;
	char *classname, *classnameend, *parentname, *retnext, *prevdeclend, *prevnext;
	int level, is_typedef, parent_primary, parent_virtual, is_lit_var, len;
	int first_virtual_warn, first_vmt_warn, empty_line, need_destructor;
	struct classtype *parentclasstype, rettype;
	struct parent *parent, *firstparent;
	struct member *member;
	char namebuf[128];
	unsigned i;

	is_typedef = *parser->pf.pos == 't';
	/* skip 'typedef struct ' or just 'struct ' */
	classname = skip_whitespace(parser, parser->pf.pos + (is_typedef ? 15 : 7));
	classnameend = skip_word(classname);
	if (classnameend != classname) {
		class = addclass(parser, classname, classnameend);
		if (!class)
			return NULL;
		/* mimic C++, class names are also types */
		decl.class = class;
		decl.pointerlevel = 0;
		addclasstype(parser, classname, classnameend, &decl, TYPEDEF_IMPLICIT);
	} else
		class = NULL;

	need_destructor = 0;
	nextdecl = skip_whitespace(parser, classnameend);
	if (class && *nextdecl == ':') {
		flush_until(parser, nextdecl);
		parent_primary = 1;
		parent_virtual = 0;
		first_virtual_warn = 0;
		first_vmt_warn = 0;
		firstparent = NULL;
		outwrite(parser, "{", 1);
		for (parentname = nextdecl;;) {
			parentname = skip_whitespace(parser, parentname + 1);
			if (strprefixcmp("public ", parentname)) {
				parentname += 6;  /* "public" */
				continue;
			}
			if (strprefixcmp("virtual ", parentname)) {
				parentname += 7;  /* "virtual" */
				parent_virtual = 1;
				class->need_root_constructor = 1;
				continue;
			}

			nameend = skip_word(parentname);
			parentclasstype = find_classtype_e(parser, parentname, nameend);
			if (parentclasstype == NULL) {
				pr_err(parentname, "cannot find parent class");
				goto nextparent;
			}
			if (parentclasstype->decl.pointerlevel) {
				pr_err(parentname, "parent type cannot be pointer type");
				goto nextparent;
			}

			parent = calloc(1, sizeof(*parent));
			if (parent == NULL)
				break;

			parentclass = parentclasstype->decl.class;
			parent->class = parentclass;
			parent->child = class;
			parent->is_primary = parent_primary;
			parent->is_virtual = parent_virtual;
			class->need_root_constructor |= parentclass->need_root_constructor;
			class->need_root_destructor = parentclass->need_root_destructor ||
				(parent_virtual && parentclass->has_destructor);
			if (!class->constructor) {
				class->constructor = parentclass->constructor;
				class->void_constructor = parentclass->void_constructor;
				class->void_root_constructor = parentclass->void_root_constructor;
			}
			if (parent_primary)
				class->freer_class = parentclass->freer_class;
			if (!class->destructor) {
				class->destructor = parentclass->destructor;
				/* if not primary, need to prevent free of offset pointer */
				if (!parent_primary && parentclass->destructor)
					class->freer_class = class;
			}
			if (class->ancestors.num_entries) {
				if (!parent_virtual && firstparent->is_virtual
						&& !first_virtual_warn) {
					pr_warn(parentname, "put non-virtual base class "
						"first for efficiency");
					first_virtual_warn = 1;
				}
				if (parentclass->vmt && !firstparent->class->vmt
						&& !parent_virtual && !first_vmt_warn) {
					pr_warn(parentname, "put virtual function class "
						"first for efficiency");
					first_vmt_warn = 1;
				}
			} else {
				/* remember first parent for checks later (above) */
				firstparent = parent;
			}

			import_parent(parser, parentname, class, parent);
			outprintf(parser, "\n\tstruct %s %s%s;", parent->class->name,
				parent_virtual ? "*" : "", parent->class->name);
			parser->pf.lines_coo++;

		  nextparent:
			nextdecl = skip_whitespace(parser, nameend);
			if (*nextdecl == '{')
				break;
			if (*nextdecl != ',') {
				pr_err(nextdecl, "expected comma or brace after parent class");
				break;
			}
			parent_primary = parent_virtual = 0;
			parentname = nextdecl;
		}

		/* already printed '{', so start after '{' */
		parser->pf.writepos = nextdecl+1;
	}

	level = 1;  /* next is at opening brace '{' */
	declbegin = retend = NULL;
	membername = next;  /* make compiler happy */
	for (;;) {
		/* search (sub)struct or end of variable or function prototype */
		prevnext = next + 1;
		next = skip_whitespace(parser, prevnext);
		if (declbegin == NULL) {
			prevdeclend = prevnext;
			declbegin = next;
		}
		/* remember last word before '(' or ';' => member name */
		if (isalpha(*next) || *next == '*' || *next == '~')
			membername = next;
		if (!(next = scan_token(parser, next, "/{},;( \r\n\t\v")))
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
				retend = retend ?: membername;
				memberprops.is_function = 0;
				/* find real params */
				membername++;
				/* let loop skip '*' in front of function variable */
				continue;
			} else if (*membername != '*' && !isspace(*membername))
				break;
		}
		/* find nameend and real params in case of function pointer variable */
		nameend = skip_methodname(membername);
		if (params < nameend) {
			params = scan_token(parser, nameend, "/\n(,;");
			if (!params)
				return NULL;
		}

		/* retend might have been assigned above, or even before,
		   and this is second variable after a comma */
		retend = retend ?: membername;

		/* next is either '(' or ';', but declend must be at ';' */
		memberprops.is_abstract = 0;
		if (*params == '(') {
			/* scan forward, once, to prevent lineno mistakes */
			params = skip_whitespace(parser, params+1);
			declend = scan_token(parser, params, "/\n=;");
			if (declend == NULL)
				return NULL;
			if (*declend == '=') {
 				if (is_abstract(parser, &declend)) {
					memberprops.is_abstract = 1;
				} else {
					pr_err(declend, "expected ';' after declaration");
					declend = scan_token(parser, declend, "/\n;");
				}
			}
		} else
			params = NULL;

		memberprops.from_primary = 1;  /* defined here so always primary */
		memberprops.from_virtual = 0;
		memberprops.is_static = strprefixcmp("static ", declbegin) != NULL;
		memberprops.is_virtual = strprefixcmp("virtual ", declbegin) != NULL;
		memberprops.is_override = strprefixcmp("override ", declbegin) != NULL;
		memberprops.is_virtual |= memberprops.is_override;
		memberprops.is_function |= memberprops.is_override;
		if (memberprops.is_virtual && !memberprops.is_function) {
			pr_err(membername, "Member variable cannot be virtual");
			continue;
		}

		/* no need to check virtual and static because cannot both at declbegin */
		if (memberprops.is_virtual) {
			/* new members need a primary vmt to put them in */
			if (!memberprops.is_override && !class->vmt) {
				class->need_root_constructor = 1;
				addvmt(class, NULL, class, LITERAL_PARENT);
				/* flush to front of virtual function */
				flush_until(parser, declbegin);
				outputs(parser, "void *vmt;");
				/* set writepos such that flush below is no-op */
				parser->pf.writepos = prevdeclend;
				parser->pf.lines_coo++;
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
			parser->pf.lines_coo--;
		}

		if (memberprops.is_override) {
			/* cannot add member, should have inherited already from parent */
			implmember(parser, membername, class, membername, nameend);
			if (declbegin != membername)
				pr_err(membername, "membername must follow override");
			if (params)
				pr_err(params, "no parameters allowed for override");
		} else {
			/* for constructor, parse_type detects rettype wrong */
			if (retend > declbegin) {
				parse_type(parser, declbegin, &retnext, &rettype);
			} else {
				rettype.decl.class = NULL;
				rettype.decl.pointerlevel = 0;
				rettype.implicit = 0;
			}
			is_lit_var = rettype.decl.pointerlevel == 0 && rettype.decl.class;
			if (is_lit_var && rettype.decl.class->num_abstract) {
				pr_err(membername, "cannot instantiate abstract "
					"class %s", rettype.decl.class->name);
			} else {
				member = addmember(parser, class, &rettype, declbegin,
					retend, membername, nameend, params, memberprops);
				if (member && is_lit_var) {
					if (rettype.decl.class->root_constructor)
						class->num_init_vars++;
					if (rettype.decl.class->destructor)
						need_destructor = 1;
				}
			}
		}

		if (*declend == ';')
			declbegin = retend = NULL;
	}

	/* add constructor if there are literal class variables with root constructor */
	if ((class->num_init_vars || class->num_parent_constr >= 2) && !class->has_constructor) {
		member = class->prim_parent ? class->prim_parent->constructor : NULL;
		addgenmember(parser, class, member, classname, classnameend);
		class->gen_constructor = 1;
		/* avoid warnings about non-void root constructor because it does not exist */
		if (!class->root_constructor)
			class->void_root_constructor = class->void_constructor;
	}
	/* add root constructor if needed and user did not define one */
	if (!class->num_abstract && class->need_root_constructor && !class->root_constructor) {
		/* copy user's constructor signature if defined */
		len = snprintf(namebuf, sizeof(namebuf), "%s_root", class->name);
		addgenmember(parser, class, class->constructor, namebuf, namebuf+len);
		class->gen_root_constructor = 1;
	}
	/* add destructor if there are literal class variables with destructor */
	if (need_destructor && !class->has_destructor) {
		class->name[-1] = '~';  /* &name[-1] == &gen_destructor */
		addgenmember(parser, class, NULL, &class->name[-1],
			&class->name[classnameend-classname]);
		class->gen_destructor = 1;
	}
	if (class->need_root_destructor && !class->root_destructor) {
		len = snprintf(namebuf, sizeof(namebuf), "d_%s_root", class->name);
		addgenmember(parser, class, NULL, namebuf, namebuf+len);
		class->gen_root_destructor = 1;
	}

	/* if we don't need a root constructor, then use the normal constructor */
	if (!class->num_abstract && !class->root_constructor && class->constructor) {
		class->constructor->is_root_constructor = 1;
		class->root_constructor = class->constructor;
		class->void_root_constructor = class->void_constructor;
	}

	/* if we don't have a constructor, then mark it void to avoid 'must define'
	   errors later on in other classes trying to initialize this class */
	if (!class->constructor) {
		class->void_constructor = 1;
		/* class-literal members need their constructor called, and we
		   do not auto-generate the constructor, require it from user */
		if (class->num_init_vars)
			pr_err(next, "must define constructor, for class-literal members");
	}

	/* check if multiple parents override same member, without final override */
	if (class->has_duplicates) {
		for (i = 0; i < class->members_arr.num; i++) {
			member = class->members_arr.mem[i];
			if (member->definition == NULL) {
				pr_err(next, "duplicate inheritance, need override "
					"for member %s", member->name);
				/* define here anyway to prevent segfaults */
				member->definition = class;
			}
		}
	}

	/* check for typedef struct X {} Y, *Z; */
	class->declare_complete = 1;
	for (;;) {
		decl.pointerlevel = 0;
		for (classname = skip_whitespace(parser, next + 1); *classname == '*'; classname++)
			decl.pointerlevel++;
		next = skip_word(classname);
		declend = scan_token(parser, next, "/\n,;");
		if (declend == NULL)
			return NULL;
		if (next != classname) {
			if (is_typedef)
				addclasstype(parser, classname, next, &decl, TYPEDEF_EXPLICIT);
			else {
				/* TODO: addglobal() */
			}
		}
		if (*declend == ';')
			break;
		next = declend;
	}
	parser->pf.pos = skip_whiteline(parser, declend + 1);
	flush(parser);
	print_root_classes(parser, class);
	print_vmt_type(parser, class);
	print_member_decls(parser, class);
	/* skip line endings, so we put resync at next declaration line */
	for (empty_line = 0;; parser->pf.pos++) {
		if (*parser->pf.pos != '\r' && *parser->pf.pos != '\n')
			break;
		if (parser->pf.pos[0] == '\r' && parser->pf.pos[1] == '\n')
			parser->pf.pos++;
		parser->pf.lineno++;
		parser->pf.linestart = parser->pf.pos;
		empty_line = 1;
	}
	/* many reasons why lineno could have gone out of sync, always resync
	   (e.g. skipped function declaration lines) */
	if (parser->line_pragmas) {
		/* flush line endings just skipped in above loop */
		flush(parser);
		/* switch back line pragma to input mode, but hard
		   to reuse because we want to force a resync here */
		outprintf(parser, "%s#line %d \"%s\"\n", empty_line ? "" : "\n",
			parser->pf.lineno, parser->pf.filename);
		parser->pf.line_pragma_mode = LINE_PRAGMA_INPUT;
		parser->pf.lines_coo += !empty_line + 1;
	}
	return class;
}

static struct member *parse_member(struct parser *parser,
	char *exprstart, char *exprend, struct class *class, int pointerlevel,
	char *name, char *nameend, struct classptr *retclassptr, struct dynarr **retparams)
{
	struct member *member;
	char *args, *flush_until, *insert_text, *continue_at;
	enum insert_continue insert_continue;
	int add_this, has_arguments, insert_index = -1;

	if (class == NULL)
		return NULL;
	if ((member = find_member_e(class, name, nameend)) == NULL)
		return NULL;

	if (exprstart && member->props.is_static) {
		pr_err(name, "cannot access static member");
		return NULL;
	}

	if (member->funcinsert)
		insert_index = addinsert(parser, insert_index,
			exprstart ?: name, member->funcinsert, name, CONTINUE_BEFORE);

	continue_at = NULL;
	add_this = !exprstart && !member->props.is_static;
	if (member->props.is_function) {
		for (args = nameend; *args == ' '; args++)
			;
		if (*args != '(')
			goto out;

		continue_at = ++args;
		has_arguments = *strskip_whitespace(args) != ')';
		insert_continue = CONTINUE_AFTER;
		if (add_this) {
			flush_until = args;
			if (member->parentname) {
				insert_index = addinsert(parser, insert_index, args,
					member->parent_virtual ? "this->" : "&this->",
					args, CONTINUE_AFTER);
				if (has_arguments) {
					insert_index = addinsert(parser, insert_index,
						args, member->parentname, args,
						CONTINUE_AFTER);
					insert_text = ", ";
				} else
					insert_text = member->parentname;
			} else {
				insert_text = has_arguments ? "this, " : "this";
			}
		} else {
			insert_text = has_arguments ? ", " : "";
			if (exprstart) {
				/* after other->function(, need to jump to
				   'other' expression and back to arguments
				   we need to take address:
				   (1) if plain (stack) variable as is
				   (2) or taking substruct which is a virtual member */
				addinsert(parser, insert_index, args,
					( member->parentname && !member->parent_virtual) ||
					(!member->parentname && pointerlevel == 0)
					? "&" : "", exprstart, CONTINUE_AFTER);
				/* continue at end */
				insert_index = parser->inserts->num;
				if (member->parentname) {
					insert_index = addinsert(parser, insert_index,
						exprend, pointerlevel ? "->" : ".",
						args, CONTINUE_AFTER);
					/* reuse empty insert_text if possible */
					if (insert_text[0] != 0)
						insert_index = addinsert(parser, insert_index,
							args, member->parentname,
							args, CONTINUE_AFTER);
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
		insert_continue = CONTINUE_BEFORE;
		if (member->nameinsert) {
			insert_index = addinsert(parser, insert_index,
				name, insert_text, name, CONTINUE_BEFORE);
			insert_text = member->nameinsert;
		}
	} else if (member->nameinsert) {
		flush_until = name;
		continue_at = name;
		insert_text = member->nameinsert;
		insert_continue = CONTINUE_AFTER;
	}

	if (continue_at)
		addinsert(parser, insert_index,
			flush_until, insert_text, continue_at, insert_continue);

out:
	*retclassptr = member->retclassptr;
	*retparams = &member->params;
	return member;
}

static char *access_inherited(struct parser *parser, struct class *thisclass,
		struct class *tgtclass, char *name, char *next, struct dynarr **retparams)
{
	struct member *member;
	struct ancestor *ancestor;
	char *thistext, *thispos;
	int insert_index;

	member = find_member_e(thisclass, name, next);
	if (!member)
		return next;

	if (tgtclass != thisclass) {
		ancestor = hasho_find(&thisclass->ancestors, tgtclass);
		if (ancestor == NULL)
			return next;
		if (ancestor->parent->is_virtual)
			thistext = "this->";
		else
			thistext = "&this->";
	} else {
		thistext = "this";
		ancestor = NULL;
	}

	next = skip_whitespace(parser, next);
	if (*next != '(')
		return next;

	thispos = next + 1;
	insert_index = addinsert(parser, -1, thispos, thistext, thispos, CONTINUE_BEFORE);
	if (ancestor)
		insert_index = addinsert(parser, insert_index,
			thispos, ancestor->path, thispos, CONTINUE_BEFORE);
	if (member->paramstext[0] != ')')
		addinsert(parser, insert_index, thispos, ", ", thispos, CONTINUE_BEFORE);

	*retparams = &member->params;
	return next;
}

static void parse_typedef(struct parser *parser, char *declend)
{
	struct classtype type;
	char *next;

	/* check if a class is aliased to a new name */
	parse_type(parser, parser->pf.pos, &next, &type);
	if (type.decl.class == NULL)
		return;

	/* note: after next cannot be '{', as in 'typedef struct X {'
	   it is handled by calling parse_struct in caller */
	parser->pf.pos = skip_whitespace(parser, next);
	next = skip_word(parser->pf.pos);
	addclasstype(parser, parser->pf.pos, next, &type.decl, TYPEDEF_EXPLICIT);
	parser->pf.pos = declend + 1;
}

static void print_inserts(struct parser *parser, struct dynarr *inserts)
{
	struct insert *insert;
	unsigned j;

	if (inserts->num == 0)
		return;

	for (j = 0; j < inserts->num; j++) {
		insert = inserts->mem[j];
		flush_until(parser, insert->flush_until);
		outputs(parser, insert->insert_text);
		parser->pf.writepos = insert->continue_at;
	}
	inserts->num = 0;
}

static void print_initializers(struct parser *parser, char *position, unsigned blocklevel,
		unsigned retblocknr, char *retvartype, char *retvartypeend)
{
	char *linestart, *params, *sep_or_end;
	struct initializer *initializer;
	struct ancestor *ancestor;
	struct class *class;
	struct member *member;
	char *ancpath, *ancpath_sep;
	unsigned i;
	int lineno;

	if (parser->initializers.num == 0)
		return;

	/* go back to start of line, so we can print full line statements
	   copy the indentation to our added lines */
	linestart = rev_lineend(parser->pf.writepos, position);
	flush_until(parser, linestart);
	/* define a return variable if needed */
	if (retvartype) {
		outwrite(parser, linestart, position - linestart);
		outwrite(parser, retvartype, retvartypeend - retvartype);
		outwrite(parser, "__coo_ret;", 10);
		parser->pf.lines_coo++;
	}
	lineno = 0;
	for (i = 0; i < parser->initializers.num; i++) {
		initializer = parser->initializers.mem[i];
		if (initializer->lineno != lineno) {
			/* if moved to next line, only print line ending */
			if (initializer->lineno != lineno + 1)
				pr_lineno(parser, initializer->lineno);
			outwrite(parser, linestart, position - linestart);
			parser->pf.lines_coo++;
			lineno = initializer->lineno;
		} else {
			/* concatenating initializers on one line, add a space */
			outwrite(parser, " ", 1);
		}
		class = initializer->varclass;
		if (class) {
			if (initializer->params) {
				/* print what user wrote here, might be a comment */
				params = strskip_whitespace(initializer->params);
				sep_or_end = *params != ')' ? ", ": "";
				parser->pf.writepos = initializer->params;
			} else {
				sep_or_end = ")";
				parser->pf.writepos = initializer->start;
			}
			member = class->root_constructor;
			ancpath_sep = ancpath = "";
			/* origin of root constructor is the original class, not the rootclass */
			if (member->origin != class && (!member->origin->rootclass
					|| &member->origin->rootclass->class != class)) {
				ancestor = hasho_find(&class->ancestors, member->origin);
				if (ancestor) {
					ancpath_sep = ".";
					ancpath = ancestor->path;
				}
			}
			outprintf(parser, "%s_%s(&%s%s%s%s", member->origin->name,
				member->name, initializer->name, ancpath_sep, ancpath, sep_or_end);
			adddisposer(parser, class, initializer->name, blocklevel, retblocknr);
		} else {
			parser->pf.writepos = initializer->name;
		}
		print_inserts(parser, &initializer->inserts);
		flush_until(parser, initializer->end);
		outwrite(parser, ";", 1);
	}
	parser->initializers.num = 0;
	parser->pf.writepos = linestart;
	/* resync lineno in case there is an empty line between initialization section
	   and statements; linestart is before line ending, so compare with lineno + 1 */
	pr_lineno(parser, parser->pf.lineno);
}

static void print_disposers(struct parser *parser, char *position,
		unsigned blocklevel, unsigned next_retblocknr, int need_retvar)
{
	struct disposer *disposer;
	struct member *member;
	char *linestart;
	unsigned i, retblocknr;

	if ((i = parser->disposers.num) == 0) {
		/* only return if also no label to print */
		if (next_retblocknr == 0)
			return;
		retblocknr = 0;
	} else {
		disposer = parser->disposers.mem[i-1];
		if (disposer->blocklevel < blocklevel)
			return;
		retblocknr = disposer->retblocknr;
	}

	/* go back to start of line, so we can print full line statements
	   copy the indentation to our added lines */
	linestart = rev_lineend(parser->pf.writepos, position);
	flush_until(parser, linestart);
	switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
	for (;;) {
		if (next_retblocknr != retblocknr) {
			outprintf(parser, "\n__coo_out%d:", retblocknr);
			parser->pf.lines_coo++;
			next_retblocknr = retblocknr;
			/* in case no disposers, just to print label */
			if (i == 0)
				break;
		}
		outwrite(parser, linestart, position - linestart);
		member = disposer->class->destructor;
		outprintf(parser, "\t%s_%s%s(&%s);", member->origin->name,
			member->implprefix, member->implname, disposer->name);
		parser->pf.lines_coo++;
		if (--i == 0)
			break;
		disposer = parser->disposers.mem[i - 1];
		if (disposer->blocklevel < blocklevel)
			break;
		retblocknr = disposer->retblocknr;
	}

	if (blocklevel == 1 && need_retvar) {
		outwrite(parser, linestart, position - linestart);
		outprintf(parser, "\treturn __coo_ret;");
		parser->pf.lines_coo++;
	}
	parser->disposers.num = i;
	parser->pf.writepos = linestart;
	switch_line_pragma(parser, LINE_PRAGMA_INPUT);
}

static void param_to_variable(struct parser *parser, int position,
		struct classptr *decl, char *name, char *next)
{	(void)position;
	addvariable(parser, 0, decl, 0, name, next, NULL);
}

static void accessancestor(struct parser *parser,
		char *exprstart, char *name, char *end,
		struct classptr *target, struct classptr *expr)
{
	struct ancestor *ancestor;
	int insert_index = -1;
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
		insert_index = addinsert(parser, insert_index,
			exprstart, pre, exprstart, CONTINUE_BEFORE);
	insert_index = addinsert(parser, insert_index, end, post, end, CONTINUE_BEFORE);
	addinsert(parser, insert_index, end, ancestor->path, end, CONTINUE_BEFORE);
}

struct paramstate {
	struct dynarr *params;
	unsigned index;
};

static void select_next_param(struct paramstate *state, struct classptr *dest)
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
enum goto_return { GOTO_RET_NONE, GOTO_RET, GOTO_RET_BLOCK };

static int is_expr(enum parse_state state)
{
	 return state == STMTSTART || state == FINDVAR || state == EXPRUNARY;
}

static void parse_function(struct parser *parser, char *next)
{
	struct class *class;
	struct classptr exprdecl[MAX_PAREN_LEVELS], targetdecl[MAX_PAREN_LEVELS];
	struct classptr decl, immdecl, *target, *expr;
	struct paramstate targetparams[MAX_PAREN_LEVELS];
	struct dynarr **tgtparams;
	struct classtype *classtype, rettype;
	struct member *member, *submember, *constr;
	struct variable *declvar;
	struct initializer *initializer;
	struct ancestor *this_ancestor;
	char *curr, *funcname, *nameend, *classname, *name, *argsep, *dblcolonsep;
	char *exprstart[MAX_PAREN_LEVELS], *exprend, *params, *param0, *paramend;
	char *memberstart[MAX_PAREN_LEVELS], *funcvarname, *funcvarnameend;
	char *thisprefix, *thisclassname, *thisfuncret, *thisfuncretend, *str1, *str2;
	enum parse_funcvar_state funcvarstate;
	enum parse_state state, nextstate;
	enum goto_return goto_ret;
	int is_constructor, blocklevel, parenlevel, seqparen, numwords;
	int have_retvar, need_retvar, used_retvar, void_retvar;
	int retblocknr, next_retblocknr, in_delete, insert_index;
	unsigned i, num_constr_called, num_constr;

	thisfuncret = parser->pf.pos;
	funcname = NULL, classname = next;
	for (; classname > thisfuncret && !isspace(*(classname-1)); classname--) {
		if (classname[0] == ':' && classname[1] == ':') {
			dblcolonsep = classname;
			classname[0] = 0;
			funcname = skip_whitespace(parser, classname+2);
		}
	}

	thisfuncretend = classname;
	is_constructor = num_constr_called = num_constr = 0;
	params = ++next;  /* advance after '(' */
	next = param0 = skip_whitespace(parser, params);

	/* clear local variables, may add "this" as variable for class */
	hash_clear(&parser->locals);
	parser->nested_locals.num = 0;
	this_ancestor = NULL;
	if (funcname) {   /* funcname assigned means there is a classname::funcname */
		if ((class = find_class(parser, classname)) != NULL) {
			/* lookup method name */
			nameend = skip_methodname(funcname);
			member = find_member_e(class, funcname, nameend);
			if (member != NULL) {
				if (member->props.is_virtual && member->definition != class) {
					pr_warn(funcname, "overriding virtual method without "
						"override in class declaration, is "
						"invisible to descendent classes");
				}
			} else {
				struct memberprops props = {0,};
				/* undeclared, so it's private, include static */
				flush(parser);
				outputs(parser, "static ");
				/* add as member so others can call from further down */
				parse_type(parser, thisfuncret, &curr, &rettype);
				props.is_function = 1;
				member = addmember(parser, class, &rettype, thisfuncret,
					classname, funcname, nameend, params, props);
			}
			class->is_implemented = 1;
			class->gen_constructor &= ~member->is_constructor;
			class->gen_root_constructor &= ~member->is_root_constructor;
			class->gen_destructor &= ~member->is_destructor;
			/* if this is a constructor and no return type, default to void
			   no return type allowed for destructor in COO, add it now for C */
			if (((member->is_constructor || member->is_root_constructor)
						&& (thisfuncret == classname
							|| strprefixcmp("void ", thisfuncret)))
					|| member->is_destructor) {
				flush(parser);
				outputs(parser, member->rettype);
				parser->pf.writepos = classname;
			}
			/* replace :: with _ */
			flush_until(parser, dblcolonsep);
			parser->pf.writepos = funcname;
			if (funcname[0] == '~') {
				/* destructor, translate to valid C name */
				outwrite(parser, "_d_", 3);
				parser->pf.writepos++;
			} else
				outwrite(parser, "_", 1);
			flush_until(parser, params);
			/* check if there are parameters */
			argsep = ", ";  /* start assumption: separate "this", rest params */
			if (*param0 == ')') {
				argsep = "";
			} else if (strprefixcmp("void", param0)) {
				next = skip_whitespace(parser, param0 + 4);
				if (*next == ')') {
					param0 += 4;  /* skip "void" if adding "this" param */
					argsep = "";
				}
			}
			/* add this parameter, does it need offset? */
			if (impl_this_needs_offset(member)) {
				this_ancestor = hasho_find(&class->ancestors,
								member->vmt->origin);
				thisclassname = member->origin->name;
				thisprefix = "__";
			} else {
				thisclassname = classname;
				thisprefix = "";
			}
			outprintf(parser, "struct %s *%sthis%s",
						thisclassname, thisprefix, argsep);
			parser->pf.writepos = param0;
			/* add this as a variable */
			decl.class = class;
			decl.pointerlevel = 1;
			name = "this";
			nameend = name + 4;  /* "this" */
			addvariable(parser, 0, &decl, 0, name, nameend, NULL);
			/* no parents means all are initialized */
			is_constructor = member->is_constructor;
			num_constr = is_constructor
				* (class->num_parent_constr + class->num_init_vars);
		} else {
			pr_err(classname, "class '%s' not declared", classname);
		}
	} else
		class = NULL;

	paramend = scan_token(parser, next, "/\n)");
	if (paramend == NULL)
		return;

	next = skip_whitespace(parser, paramend+1);
	if (*next == ';') {
		/* function prototype */
		parser->pf.pos = next + 1;
		return;
	}

	/* save in case of error exit, or flushing + defining 'this' later */
	parser->pf.pos = next;
	if (*next != '{') {
		pr_err(next, "expected ';' or '{' after function definition");
		return;
	}

	/* skip the '{' */
	blocklevel = 1;
	next++;

	/* store parameters as variables */
	if (parse_parameters(parser, params, NULL, param_to_variable) == NULL)
		return;

	if (this_ancestor) {
		parser->pf.pos = next;
		flush(parser);
		outprintf(parser, "\tstruct %s *this = container_of(__this, struct %s, %s);",
			classname, classname, this_ancestor->path);
	}

	seqparen = parenlevel = exprdecl[0].pointerlevel = numwords = 0;
	exprdecl[0].class = targetdecl[0].class = decl.class = immdecl.class = NULL;
	have_retvar = need_retvar = used_retvar = void_retvar = 0;
	exprstart[0] = memberstart[0] = exprend = NULL;
	funcvarname = funcvarnameend = name = NULL;  /* make compiler happy */
	retblocknr = next_retblocknr = in_delete = 0;
	parser->initializers.num = 0;
	targetparams[0].params = NULL;
	funcvarstate = FV_NONE;
	goto_ret = GOTO_RET_NONE;
	state = STMTSTART;
	initializer = NULL;
	declvar = NULL;
	for (;;) {
		curr = skip_whitespace(parser, next);
		/* skip comments */
		if (curr[0] == 0)
			return;
		/* pending initializers and this looks like statement, then print */
		if (parser->initializers.num && parenlevel == 0 &&
				(*curr == '=' || *curr == '(' || *curr == '{') && numwords < 2) {
			print_initializers(parser, memberstart[0], blocklevel, next_retblocknr,
				need_retvar && !have_retvar ? thisfuncret : NULL, thisfuncretend);
			have_retvar = need_retvar;
			retblocknr = next_retblocknr;
		}

		/* track block nesting level */
		if (*curr == '{') {
			blocklevel++;
			next = curr + 1;
			continue;
		}
		if (*curr == '}') {
			print_disposers(parser, curr, blocklevel, next_retblocknr, need_retvar);
			remove_locals(parser, blocklevel);
			next = curr + 1;
			if (--blocklevel == 0)
				break;
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
				name = skip_whitespace(parser, curr + 7);  /* "struct " */
				next = skip_word(name);
				if (state == DECLVAR || state == ACCESSMEMBER) {
					/* grammar error, ignore */
					state = FINDVAR;
				} else if ((decl.class = find_class_e(parser, name, next)) != NULL) {
					decl.pointerlevel = 0;
					goto decl_or_cast;
				}
				continue;
			} else if (strprefixcmp("return", curr) && !isalnum(curr[6])) {
				next = curr + 7;  /* "return" */
				if (need_retvar) {
					insert_text(parser, -1, curr,
						state == STMTSTART ? "__coo_ret = "
							: "{ __coo_ret = ",
						next, CONTINUE_AFTER);
					next_retblocknr = retblocknr + 1;
					goto_ret = state != STMTSTART ? GOTO_RET_BLOCK : GOTO_RET;
				}
				if (parser->disposers.num) {
					struct disposer *disposer =
						parser->disposers.mem[parser->disposers.num-1];
					if (disposer->blocklevel > 1) {
						pr_err(curr, "cannot return from nested "
							"block with stack variables");
					}
				}
				state = FINDVAR;
				continue;
			} else if (is_expr(state) && strprefixcmp("new ", curr)) {
				name = skip_whitespace(parser, curr + 4);  /* "new " */
				next = skip_word(name);
				classtype = find_classtype_e(parser, name, next);
				if (classtype) {
					if (!classtype->decl.pointerlevel) {
						decl.class = classtype->decl.class;
						decl.pointerlevel = 1;
						immdecl = decl;
						if (decl.class->constructor) {
							addinsert(parser, -1,
								curr, "new_", name, CONTINUE_AFTER);
							state = CONSTRUCT;
						} else {
							if (classtype->implicit) {
								str1 = "(struct ";
								str2 = "*)malloc(sizeof(struct ";
							} else {
								str1 = "(";
								str2 = "*)malloc(sizeof(";
							}
							/* first print a cast to dest type */
							insert_index = addinsert(parser, -1,
								curr, str1, name, CONTINUE_AFTER);
							/* second the malloc, go back to name */
							insert_index = addinsert(parser,
								insert_index, next, str2,
								name, CONTINUE_AFTER);
							addinsert(parser, insert_index, next,
								"))", next, CONTINUE_AFTER);
						}
					} else
						pr_err(name, "'new type' cannot be pointer type");
				}
				continue;
 			} else if (state == STMTSTART && strprefixcmp("delete ", curr)) {
				flush_until(parser, curr);
				parser->pf.writepos = next = curr + 7;  /* "delete " */
				in_delete = 1;
				continue;
			}

			if (state == DECLVAR && decl.class && parenlevel == 0) {
				/* for 2nd variable decl.class is the rootclass */
				if (declvar && decl.class->is_rootclass
						&& (declvar->decl.pointerlevel
							|| exprdecl[0].pointerlevel)) {
					pr_err(curr, "cannot combine pointer and non-pointer "
						"declarations of root-requiring classes");
				} else if (!declvar && decl.class->rootclass
						&& exprdecl[0].pointerlevel == 0) {
					/* declaring stack variable with root class, add _root */
					flush_until(parser, next);
					outwrite(parser, "_root", 5);
					parser->pf.writepos = next;
					/* fix class type, so ancestor paths are correct */
					decl.class = &decl.class->rootclass->class;
				}
			}
			name = curr;
			next = skip_word(name);
			numwords++;
			nextstate = FINDVAR;
			switch (state) {
			case DECLVAR:
				declvar = addvariable(parser, blocklevel, &decl,
					exprdecl[0].pointerlevel, name, next, NULL);
				if (exprdecl[0].pointerlevel == 0 && decl.pointerlevel == 0
						&& decl.class) {
					if (decl.class->num_abstract) {
						pr_err(name, "cannot instantiate "
							"abstract class %s", decl.class->name);
					} else if (decl.class->root_constructor) {
						nextstate = CONSTRUCT;
						if (!need_retvar && !void_retvar &&
								decl.class->destructor) {
							void_retvar = is_void_rettype(thisfuncret);
							need_retvar = !void_retvar;
						}
						initializer = addinitializer(parser,
							decl.class, declvar->name, next);
					}
				}
				break;
			case ACCESSMEMBER:
				/* immdecl is used for same parenthesis level,
				   exprdecl in case of cast (nested in parentheses) */
				expr = immdecl.class ? &immdecl : &exprdecl[parenlevel];
				if (expr->pointerlevel <= 1) {
					submember = parse_member(parser,
						memberstart[parenlevel], exprend, expr->class,
						expr->pointerlevel, name, next, expr,
						&targetparams[parenlevel].params);
					/* to check all literal class variables initialized */
					if (is_constructor && submember
							&& submember->is_root_constructor
							&& member->retclassptr.
								pointerlevel == 0) {
						if (!member->constructor_called) {
							num_constr_called++;
							member->constructor_called = 1;
						} else {
							pr_err(memberstart[parenlevel],
								"duplicate call to member %s "
								"root constructor", member->name);
						}
					}
				}
				break;
			case ACCESSINHERITED:
				next = access_inherited(parser, class, immdecl.class,
					name, next, &targetparams[parenlevel].params);
				break;
			default:
				/* maybe it's a local (stack) variable? */
				tgtparams = &targetparams[parenlevel].params;
				if (find_local_e_class(parser, name, next,
						&immdecl, tgtparams) >= 0)
					break;
				/* maybe it's a member field? "this" has pointerlevel 1 */
				member = parse_member(parser, NULL, NULL,
						class, 1, name, next, &immdecl, tgtparams);
				if (member != NULL) {
					if (is_constructor && member->parent_constructor) {
						if (!member->constructor_called) {
							num_constr_called++;
							member->constructor_called = 1;
						} else {
							pr_err(name, "duplicate call to parent "
								"constructor %s", member->name);
						}
					}
					break;
				}
				/* maybe it's a global variable? */
				if (find_global_e_class(parser,
						name, next, &immdecl, tgtparams) >= 0)
					break;
				/* maybe it's a type, to declare variable, or a cast */
				classtype = find_classtype_e(parser, name, next);
				if (classtype != NULL) {
					/* we don't want implicit struct for class::func */
					if (*next != ':') {
						/* if we are in expression,
						   not safe to print directly */
						if (state == STMTSTART)
							print_implicit_struct(
								parser, classtype, name);
						else
							addinsert_implicit_struct(
								parser, classtype, name);
					}
					decl = classtype->decl;
				  decl_or_cast:
					if (state == STMTSTART) {
						nextstate = DECLVAR;
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
						nextstate = CASTVAR;
						break;
					} else if (*next != ':' || next[1] != ':') {
						/* grammar error, ignore */
						decl.class = NULL;
					}
				} else if (parenlevel == 1 && exprdecl[1].pointerlevel == -1) {
					/* perhaps this is a function pointer variable decl? */
					funcvarname = name;
					funcvarnameend = next;
					funcvarstate = FV_NAME;
				}
				break;
			}

			state = nextstate;
			continue;
		}

		if (funcvarstate == FV_NAME)
			funcvarstate = *curr == ')' ? FV_PARENCLOSE : FV_NONE;
		else if (funcvarstate == FV_PARENCLOSE) {
			funcvarstate = FV_NONE;
			if (*curr == '(') {
				next = scan_token(parser, curr+1, "/\n);");
				if (next == NULL)
					break;
				if (*next == ')')
					addvariable(parser, blocklevel, &decl,
						exprdecl[0].pointerlevel, funcvarname,
						funcvarnameend, curr+1);
			}
		}

		if (state == CONSTRUCT) {
			if (decl.class->missing_root) {
				pr_err(curr, "must define root constructor for %s "
					"due to non-void constructor %s",
					decl.class->name, decl.class->missing_root->name);
			}
			constr = decl.class->root_constructor;
			if (constr) {
				if (*curr == '(') {
					if (initializer) {
						/* skip parenthesis, need to add "this" parameter */
						initializer->params = curr + 1;
					}
					targetparams[parenlevel].params = &constr->params;
				} else if (constr->paramstext[0] != ')') {
					pr_err(curr, "missing call to constructor");
					parser->initializers.num -= initializer != NULL;
					initializer = NULL;
				}
				if (decl.pointerlevel == 0 && !decl.class->void_root_constructor)
					pr_warn(curr, "non-void root constructor may fail");
			}
			if (*curr != '(' && decl.pointerlevel) {
				/* allocation with 'new class', needs function call */
				addinsert(parser, -1, curr, "()", curr, CONTINUE_AFTER);
			}
			state = FINDVAR;
		}

		if (*curr != '.' && (*curr != '-' || curr[1] != '>'))
			memberstart[parenlevel] = NULL;
		if (parser->initializers.num && parenlevel == 0 && *curr == '=') {
			/* when added one initializer, then copy them all,
			   to keep order the order the same */
			initializer = addinitializer(parser, NULL, name, next);
		} else if (*curr == ')' || *curr == ',' || *curr == '=' || *curr == ';') {
			/* determine target and source classes to access ancestor of */
			if (exprdecl[parenlevel].class != NULL)
				expr = &exprdecl[parenlevel];
			else {
				expr = &immdecl;
				immdecl.pointerlevel += exprdecl[parenlevel].pointerlevel;
			}
			target = &targetdecl[parenlevel];
			if (target->class && expr->class && target->class != expr->class
					&& target->pointerlevel == expr->pointerlevel
					&& target->pointerlevel <= 1)
				accessancestor(parser,
					exprstart[parenlevel], name, curr, target, expr);
			exprstart[parenlevel] = NULL;
		}
		if (initializer && (*curr == ',' || *curr == ';')) {
			if (initializer->start != curr) {
				flush_until(parser, initializer->start);
				parser->pf.writepos = curr;
			}
			initializer->end = curr;
			parser->inserts = &parser->inserts_arr;
			initializer = NULL;
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
				pr_err(curr, "maximum parenthesis nesting level reached");
			state = EXPRUNARY;
		} else if (*curr == ')') {
			if (parenlevel > 0) {
				parenlevel--;
				if (exprdecl[parenlevel].class == NULL)
					exprdecl[parenlevel] = exprdecl[parenlevel+1];
			}
			state = FINDVAR;
		} else if (*curr == '*') {
			switch (state) {
			case DECLVAR:
			case CASTVAR: exprdecl[0].pointerlevel++; break;
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
			if (parenlevel) {
				select_next_param(&targetparams[parenlevel-1],
					&targetdecl[parenlevel]);
			}
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
			numwords--;    /* for declaration detection, merge x->y words */
		} else if (*curr == '-' && curr[1] == '>') {
			state = ACCESSMEMBER;
			exprend = curr;
			numwords--;    /* for declaration detection, merge x->y words */
			curr++;
		} else if (*curr == ':' && curr[1] == ':') {
			if (member && member->is_constructor) {
				/* inserting "this", has pointerlevel 1 */
				immdecl.class = member->definition;
				immdecl.pointerlevel = 1;
				/* parent constructor call already inserted class_ */
				parser->inserts_arr.num--;
				goto accessinherited;
			} else if (decl.class) {
				immdecl = decl;
				decl.class = NULL;
			  accessinherited:
				/* convert "class::func" to "class_func" */
				next = curr + 2;
				addinsert(parser, -1, curr, "_", next, CONTINUE_AFTER);
				state = ACCESSINHERITED;
				numwords--;    /* for declaration detection, merge x->y words */
			} else {
				pr_err(curr, "unexpected '::' encountered");
			}
		} else if (*curr == ';') {
			if (in_delete) {
				expr = immdecl.class ? &immdecl : &exprdecl[0];
				if (expr->class) {
					if (expr->class->destructor) {
						outprintf(parser, "free_%s(", expr->class->name);
					} else {
						outputs(parser, "free(");
					}
				} else {
					pr_err(curr, "unknown variable to delete");
				}
			}
			parser->inserts = &parser->inserts_arr;
			print_inserts(parser, parser->inserts);
			if (in_delete) {
				parser->pf.pos = curr;
				flush(parser);
				outwrite(parser, ")", 1);
				in_delete = 0;
			}
			immdecl.class = NULL;
			decl.class = NULL;
			declvar = NULL;
			member = NULL;
			numwords = 0;
			seqparen = 0;
			parenlevel = 0;
			state = STMTSTART;
			exprdecl[0].pointerlevel = 0;
			if (goto_ret) {
				/* look ahead to see if function ends here */
				next = skip_whitespace(parser, curr + 1);
				/* if end of function reached, no goto necessary */
				if (goto_ret != GOTO_RET || blocklevel > 1 || *next != '}') {
					parser->pf.pos = curr + 1;
					flush(parser);
					outprintf(parser, " goto __coo_out%d;", retblocknr);
					if (goto_ret == GOTO_RET_BLOCK)
						outwrite(parser, " }", 2);
					goto_ret = GOTO_RET_NONE;
				}
				continue;
			}
		} else if (*curr == '+' || *curr == '-' || *curr == '/' || *curr == '%'
			|| *curr == '&' || *curr == '|' || *curr == '^' || *curr == '!'
			|| *curr == '?' || *curr == ':') {
			if (*curr == '&' && state == EXPRUNARY) {
				exprdecl[parenlevel].pointerlevel++;
			} else {
				state = EXPRUNARY;
				if (immdecl.class != NULL) {
					exprdecl[parenlevel] = immdecl;
					immdecl.class = NULL;
				}
			}
			member = NULL;
		} else
			state = FINDVAR;

		/* advance for most of the operator/separator cases */
		if (next <= curr)
			next = curr+1;
	}

	if (num_constr_called < num_constr) {
		for (i = 0; i < class->members_arr.num; i++) {
			member = class->members_arr.mem[i];
			if (member->constructor_called)
				continue;
			if (member->parent_constructor) {
				pr_err(curr, "missing parent constructor "
					"%s call", member->name);
			} else if (member_needs_init(class, member)) {
				pr_err(curr, "missing member root constructor "
					"call %s.%s()", member->name,
					member->retclassptr.class->root_constructor->name);
			}
		}
	}

	if (is_constructor) {
		int lines_coo = 1;
		for (; curr > parser->pf.buffer && isspace(curr[-1]); curr--) {
			if (curr[-1] == '\n') {
				/* move line back, so have to compensate output lineno */
				lines_coo = 2;
				parser->pf.lines_coo--;
				curr--;
				break;
			}
		}
		flush_until(parser, curr);
		parser->pf.writepos = curr;
		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		outwrite(parser, "\n\treturn this;", 14);
		parser->pf.lines_coo += lines_coo;
		switch_line_pragma(parser, LINE_PRAGMA_INPUT);
	}
	parser->pf.pos = next;
}

static void print_class_alloc(struct parser *parser, struct class *class)
{
	struct member *rootconstr = class->root_constructor;
	char *rootsuffix, *callend1, *callend2, *callend3, *params;
	char *constr_ret, *constr_addr, *constr_arrow, *constr_path;
	struct class *constr_origin;
	struct ancestor *ancestor;

	if (!rootconstr)
		return;

	constr_origin = rootconstr->origin;
	rootsuffix = constr_ret = constr_addr = constr_arrow = constr_path = "";
	callend1 = callend2 = callend3 = "";
	if (class->rootclass) {
		rootsuffix = "_root";
		callend1 = "\treturn &this->";
		callend2 = class->name;
		callend3 = ";\n";
	} else if (constr_origin != class) {
		ancestor = hasho_find(&class->ancestors, constr_origin);
		if (!ancestor) {
			pr_err(NULL, "internal error, cannot find ancestor for class alloc");
			return;
		}
		constr_addr = ancestor->parent->is_virtual ? "" : "&";
		constr_arrow = "->";
		constr_path = ancestor->path;
		callend1 = "\treturn this;\n";
	} else
		constr_ret = "return ";
	params = rootconstr->paramstext[0] == ')' ? "void)" : rootconstr->paramstext;
	outprintf(parser, "\nstruct %s *new_%s(%s\n"
		"{\n\tstruct %s%s *this = malloc(sizeof(*this));\n"
		"\tif (this == NULL) return NULL;\n"
		"\t%s%s_%s(%sthis%s%s",
		class->name, class->name, params, class->name, rootsuffix,
		constr_ret, constr_origin->name, rootconstr->name,
		constr_addr, constr_arrow, constr_path);
	print_param_names(parser, rootconstr->paramstext);
	outprintf(parser, ");\n%s%s%s}\n", callend1, callend2, callend3);
	/* no need to count lines_coo here, end of input */
}

static void print_call_constructor(struct parser *parser, struct class *class,
		char *rootprefix, char *sep, char *membername, struct member *constructor)
{
	char *retstr, *sep_or_end;

	retstr = class->void_root_constructor ? "" : "if (!";
	sep_or_end = constructor->paramstext[0] != ')' ? ", " : "";
	outprintf(parser, "\t%s%s_%s(%sthis->%s%s%s%s", retstr,
		class->name, constructor->name,
		constructor->parent_virtual ? "" : "&",
		rootprefix, sep, membername, sep_or_end);
	print_param_names(parser, constructor->paramstext);
	if (class->void_root_constructor)
		retstr = ");\n";
	else
		retstr = "))\n\t\treturn NULL;\n";
	outputs(parser, retstr);
	/* no need to update lines_coo here, at end of input */
}

static void print_construct_parents(struct parser *parser, struct class *class,
		char *rootprefix, char *sep, enum parent_virtual virtual_parents)
{
	struct member *member;
	unsigned i;

	for (i = 0; i < class->members_arr.num; i++) {
		member = class->members_arr.mem[i];
		if (!member->parent_constructor)
			continue;
		if (virtual_parents != member->props.from_virtual)
			continue;

		print_call_constructor(parser, member->origin,
			rootprefix, sep, member->parentname, member);
	}
}

static void print_root_constructor(struct parser *parser, struct class *class)
{
	char *vmtpath, *vmtaccess, *sep_or_end, *paramstext;
	struct ancestor *ancestor, *literal_ancestor;
	struct hasho_entry *entry;
	struct class *parentclass, *rootclass;
	char *name, *rootprefix, *thisname, *sep;
	struct vmt *vmt;
	unsigned i;

	if (!class->gen_root_constructor)
		return;

	if (class->rootclass) {
		rootclass = &class->rootclass->class;
		rootprefix = class->name;
		sep = ".";
	} else {
		rootclass = class;
		rootprefix = sep = "";
	}
	paramstext = class->root_constructor->paramstext;
	sep_or_end = paramstext[0] != ')' ? ", " : "";
	/* root constructor function signature */
	outprintf(parser, "\nstruct %s *%s_%s_root(struct %s *this%s%s\n{\n",
		class->name, class->name, class->name, rootclass->name, sep_or_end, paramstext);
	/* no need to update lines_coo here, at end of input */
	/* virtual inits */
	hasho_foreach(entry, &class->ancestors) {
		ancestor = entry->value;
		do {
			if (ancestor->parent->is_virtual) {
				parentclass = ancestor->parent->class;
				outprintf(parser, "\tthis->%s%s%s = &this->",
					rootprefix, sep, ancestor->path);
				literal_ancestor = hasho_find(&rootclass->ancestors, parentclass);
				if (!literal_ancestor || literal_ancestor->parent->is_virtual)
					outputs(parser, parentclass->name);
				else
					outprintf(parser, "%s", literal_ancestor->path);
				outwrite(parser, ";\n", 2);
				/* no update lines_coo, input end */
			}
			ancestor = ancestor->next;
		} while (ancestor);
	}
	/* vmt inits */
	for (i = 0; i < class->vmts.num; i++) {
		vmt = class->vmts.mem[i];
		ancestor = find_vmt_path(rootclass, vmt, &vmtpath, &vmtaccess);
		for (;;) {
			outprintf(parser, "\tthis->%s%svmt = &%s;\n",
				vmtpath, vmtaccess, get_vmt_name(vmt));
			/* no update lines_coo, input end */
			/* loop though duplicates for this origin (literal bases) */
			for (; ancestor; ancestor = ancestor->next) {
				/* virtual parents also exist somewhere as literal, skip */
				if (!ancestor->parent->is_virtual)
					break;
			}
			if (!ancestor)
				break;
			vmtpath = ancestor->path;
			vmtaccess = ancestor->parent->is_virtual ? "->" : ".";
			ancestor = ancestor->next;
		}
	}
	/* construct classes that are used as virtual bases only */
	print_construct_parents(parser, class, rootprefix, sep, VIRTUAL_PARENT);
	/* no root class if no non-resolved virtual bases */
	if (class->rootclass) {
		thisname = "&this->";
		name = class->name;
	} else {
		thisname = "this";
		name = "";
	}
	/* class constructor call */
	if (class->has_constructor) {
		outprintf(parser, "\treturn %s_%s(%s%s",
			class->name, class->name, thisname, name);
		print_param_names(parser, paramstext);
		outwrite(parser, ");\n}\n", 5);
		/* no need to update lines_coo here, at end of input */
	} else {
		/* no constructor, need to call all parents here then */
		print_construct_parents(parser, class, rootprefix, sep, LITERAL_PARENT);
		outprintf(parser, "\treturn %s%s;\n}\n", thisname, name);
	}
}

static void print_constructor(struct parser *parser, struct class *class)
{
	struct class *memberclass;
	struct member *member;
	unsigned i;

	if (!class->gen_constructor)
		return;

	print_func_header(parser, class, class->constructor);
	print_construct_parents(parser, class, "", "", LITERAL_PARENT);
	for (i = 0; i < class->members_arr.num; i++) {
		member = class->members_arr.mem[i];
		if (!member_needs_init(class, member))
			continue;
		memberclass = member->retclassptr.class;
		print_call_constructor(parser, memberclass,
			"", "", member->name, memberclass->root_constructor);
	}
	outwrite(parser, "\treturn this;\n}\n", 16);
}

static void print_destruct_parents(struct parser *parser, struct class *class,
		enum parent_virtual virtual_parents)
{
	struct class *parentclass;
	struct member *member;
	unsigned i;

	for (i = class->members_arr.num; i > 0; i++) {
		member = class->members_arr.mem[i-1];
		if (!member->parent_destructor)
			continue;
		if (virtual_parents != member->props.from_virtual)
			continue;

		parentclass = member->origin;
		outprintf(parser, "\t%s_d_%s(&this->%s);\n",
			parentclass->name, parentclass->name, member->parentname);
		/* no need to update lines_coo here, at end of input */
	}
}

static void print_root_destructor(struct parser *parser, struct class *class)
{
	if (!class->gen_root_destructor)
		return;

	outprintf(parser, "\nvoid %s_d_%s_root(struct %s *this)\n{\n",
		class->name, class->name, class->name);
	if (class->rootclass)
		outprintf(parser, "\tstruct %s *root_this = container_of(this, struct %s, %s);",
			class->name, class->name, class->name);
	if (class->has_destructor) {
		outprintf(parser, "\t%s_d_%s(this);\n\t", class->name, class->name);
	} else {
		/* no destructor, need to call all parents here then */
		print_destruct_parents(parser, class, LITERAL_PARENT);
	}
	/* destruct classes that are used as virtual bases only */
	print_destruct_parents(parser, class, VIRTUAL_PARENT);
}

static void print_destructor(struct parser *parser, struct class *class)
{
	struct class *memberclass;
	struct member *member, *destructor;
	unsigned i;

	if (!class->gen_destructor)
		return;

	print_func_header(parser, class, class->destructor);
	for (i = class->members_arr.num; i > 0; i--) {
		member = class->members_arr.mem[i-1];
		if (!member_needs_dispose(class, member))
			continue;
		memberclass = member->retclassptr.class;
		destructor = memberclass->destructor;
		outprintf(parser, "\t%s_%s%s(&this->%s);\n",
			memberclass->name, destructor->implprefix,
			destructor->implname, member->name);
	}
	print_destruct_parents(parser, class, LITERAL_PARENT);
	outwrite(parser, "}\n", 2);
}

static void print_class_free(struct parser *parser, struct class *class)
{
	struct ancestor *ancestor;
	struct class *destrclass;
	char *addr, *arrow, *path;

	if (class->freer_class != class)
		return;

	addr = arrow = path = "";
	destrclass = class->destructor ? class->destructor->origin : class;
	if (destrclass != class) {
		ancestor = hasho_find(&class->ancestors, destrclass);
		if (ancestor) {
			addr = ancestor->parent->is_virtual ? "" : "&";
			arrow = "->";
			path = ancestor->path;
		}
	}
	outprintf(parser, "\nvoid free_%s(struct %s *this)\n{\n"
			"\t%s_d_%s%s(%sthis%s%s);\n\tfree(this);\n}\n",
			class->name, class->name, destrclass->name, destrclass->name,
			destrclass->rootclass ? "_root" : "", addr, arrow, path);
}

static void print_trampolines(struct parser *parser, struct class *class,
		struct ancestor *ancestor, struct class *origin)
{
	struct member *member;
	unsigned i;

	for (i = 0; i < class->members_arr.num; i++) {
		member = class->members_arr.mem[i];
		if (member->vmt == NULL)
			continue;
		if (member->vmt->origin != origin)
			continue;

		outprintf(parser, "\n%s%s_root_%s%s(struct %s *__this, %s\n"
			"{\tstruct %s_root *this = container_of("
				"__this, struct %s_root, %s);\n"
			"\t%s%s_%s%s(&this->%s",
			member->rettype, class->name, member->implprefix,
			member->implname, origin->name, member->paramstext,
			class->name, class->name, ancestor->path,
			is_void_rettype(member->rettype) ? "" : "return ",
			member->definition->name, member->implprefix,
			member->implname, class->name);
		print_param_names(parser, member->paramstext);
		outprintf(parser, ");\n}\n");
		/* no need to count lines_coo here, end of input */
	}
}

static void print_vmt(struct parser *parser, struct class *class,
		struct vmt *vmt, const char *rootsuffix)
{
	struct member *member;
	const char *vmt_name;
	unsigned i;

	vmt_name = get_vmt_name(vmt);
	outprintf(parser, "\nstruct %s %s = {\n", vmt_name, vmt_name);
	for (i = 0; i < class->members_arr.num; i++) {
		member = class->members_arr.mem[i];
		if (member->vmt == NULL)
			continue;
		if (member->vmt->origin != vmt->origin)
			continue;

		outprintf(parser, "\t%s%s_%s%s,\n",
			member->definition->name, rootsuffix,
			member->implprefix, member->implname);
	}
	outprintf(parser, "};\n");
	/* no need to count lines_coo here, end of input */
}

static void print_class_impl(struct parser *parser)
{
	struct class *class, *vmt_origin;
	struct ancestor *ancestor;
	const char *rootsuffix;
	struct vmt *vmt;
	unsigned i;

	hash_foreach(class, &parser->classes) {
		if (class->num_abstract || !class->is_implemented)
			continue;

		if (parser->pf.writepos != parser->pf.pos) {
			flush(parser);
			switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		}

		for (i = 0; i < class->vmts.num; i++) {
			vmt = class->vmts.mem[i];
			if (!vmt->modified)
				continue;

			/* print trampoline functions for virtual functions
			   inherited from virtual base classes, where implementation
			   cannot see literal base therefore cannot translate 'this' */
			/* note that this implies a root class for this class: if the
			   base is present as a literal base, no need for root class */
			rootsuffix = "";
			vmt_origin = vmt->origin;
			ancestor = hasho_find(&class->ancestors, vmt_origin);
			if (ancestor && ancestor->from_virtual) {
				rootsuffix = "_root";
				print_trampolines(parser, class, ancestor, vmt_origin);
			}

			/* print vmt itself */
			print_vmt(parser, class, vmt, rootsuffix);
		}

		print_class_alloc(parser, class);
		print_root_constructor(parser, class);
		print_constructor(parser, class);
		print_class_free(parser, class);
		print_root_destructor(parser, class);
		print_destructor(parser, class);
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
			uint64hash(file_id->file_id)))
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
	char *buffer, *bufend, *outbuffer, *fullname, *filename, *outfilename;
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
	bufend = parse_file->bufend;
	outbuffer = parse_file->outbuffer;
	outfilename = parse_file->outfilename;
	/* save current parsing state */
	memcpy(parse_file, &parser->pf, sizeof(*parse_file));
	/* parser->filename assigned in parse_source */
	parser->pf.out = NULL;
	parser->pf.buffer = buffer;
	parser->pf.bufend = bufend;
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
	bufend = parser->pf.bufend;
	filename = parser->pf.filename;
	outbuffer = parser->pf.outbuffer;
	outfilename = parser->pf.outfilename;
	memcpy(&parser->pf, parse_file, sizeof(parser->pf));
	parse_file->buffer = buffer;
	parse_file->bufend = bufend;
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
		pr_err(parser->pf.pos, "file not found: %s", tempname);
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
	next = scan_token(parser, parser->pf.pos + 1, "/\">\r\n");
	if (*parser->pf.pos == '"' && *next == '"')
		try_include(parser, next, SEARCH_CURRENT_DIR);
	else if (*parser->pf.pos == '<' && *next == '>')
		try_include(parser, next, SEARCH_ONLY_PATHS);
	else
		pr_warn(parser->pf.pos, "unknown character after #include, ignored");

	parser->pf.pos = next+1;
}

static void parse(struct parser *parser)
{
	char *next;

	/* write line directives for useful compiler messages */
	if (parser->line_pragmas) {
		outprintf(parser, "#line 1 \"%s\"\n", parser->pf.filename);
		/* 1 here, and switch_line_pragma always prints \n first */
		parser->pf.lines_coo = 2;
	}

	/* search for start of struct or function */
	for (;;) {
		parser->pf.pos = skip_whitespace(parser, parser->pf.pos);
		if (*parser->pf.pos == '#') {
			parser->pf.pos++;
			if (strprefixcmp("include ", parser->pf.pos)) {
				parser->pf.pos += 8;  /* "include " */
				parse_include(parser);
			}

			next = strchr(parser->pf.pos, '\n');
			if (next == NULL)
				break;

			/* do not skip lineend, let skip_whitespace count lineno */
			parser->pf.pos = next;
			continue;
		}

		next = scan_token(parser, parser->pf.pos, "/\n({;");
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
	int prev_num_errors, new_errors;
	size_t commonlen, outnamelen;
	FILE *fp_dest;

	/* read entire input file in one go to make scanning easier */
	parser->pf.filename = filename;
	buffer = read_file_until(in, parser->pf.buffer, size);
	if (buffer == NULL)
		return -1;
	fclose(in);

	/* scan (previously generated) output file for comparison, if applicable */
	parser->pf.lineno = 1;
	parser->pf.linestart = buffer - 1;  /* 1-based column index in messages */
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
	parser->pf.bufend = buffer + size;
	/* make sure filename_file points to filename (after last /) */
	for (p = parser->pf.newfilename_file; *p; p++)
		if (*p == DIRSEP)
			parser->pf.newfilename_file = p+1;

	/* start parsing! */
	prev_num_errors = parser->num_errors;
	parse(parser);
	new_errors = parser->num_errors > prev_num_errors;
	if (!new_errors)
		print_class_impl(parser);  /* vmt(s), alloc, constr. */
	/* if no syntax added by coo parser, then can keep original filename in #include */
	if (parser->pf.writepos == buffer) {
		/* if parsing included file, and nothing changed, do not write any output */
		if (parser->file_stack.num)
			return OUTPUT_FILE_SKIPPED;
		/* otherwise flush now, we always want output of main file */
		flush(parser);
		return OUTPUT_FILE_WRITTEN;
	}
	/* also write output file if it became shorter (outwrite cannot detect this)
	   if exactly equal, we expect a null-terminator at outpos (put by read_file) */
	if (parser->pf.out == NULL && !parser->pf.outfailed && parser->pf.outpos[0])
		prepare_output_file(parser);
	/* store output filename (1) for rename below (2) for caller #include "..." */
	if (parser->pf.outfilename) {
		/* newfilename_path..newfilename_file was not modified, is common
		   align to newfilename_path so that caller can use for #include */
		/* outfilename already has the temporary extension in buffer, but has
		   null character in place of '.<pid>' to write #line pragmas */
		*parser->pf.outfilename_end = '.';
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
		if (parser->pf.outfilename) {
			if (!new_errors)
				rename(parser->pf.outfilename, parser->pf.newfilename_path);
			else
				unlink(parser->pf.outfilename);
		}
	}
	if (parser->pf.outfilename && new_errors) {
		/* delete output filename, might already exist, but out-of-date now */
		unlink(parser->pf.newfilename_path);
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
		"\twithout FILE, read from stdin\n"
		"options:\n"
		"\t-Ipath: search path for include files\n"
		"\t-l:     suppress line pragmas (debugging coo output)\n"
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
	parser->line_pragmas = 1;
	parser->inserts = &parser->inserts_arr;
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
			case 'l': parser->line_pragmas = 0; break;
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

	return parser->num_errors > 0;
}
