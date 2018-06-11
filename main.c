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
	"#endif\n\n"

struct file_id {
	dev_t dev_id;
	ino_t file_id;
	uint64_t mtime;
};

enum parse_state {
	FINDVAR, STMTSTART, DECLVAR, ACCESSMEMBER,
};

struct dynarr {
	void **mem;
	unsigned num;
	unsigned max;
	unsigned sorted;
};

struct memberprops {
	unsigned char is_virtual;
	unsigned char is_function;
	unsigned char is_static;
	unsigned char seen;
};

struct class {
	char *name;
	struct dynarr members;
	unsigned num_vfuncs;
	unsigned num_vfuncs_seen;  /* to trigger generate virtual method table */
};

struct member {
	char *name;
	char *rettype;
	struct class *retclass;
	char *params;
	char *insert;   /* insert this string in front of identifier */
	  /* for virtual function: func() ==> vt->func()
	     for inherited member: field1 ==> parent.field1
	     for member, no change */
	struct memberprops props;
	char retsep;   /* separator between rettype and name, may be '*' */
	char duplicate_pr;  /* to prevent spam, already printed duplicated message */
};

struct variable {
	char *name;
	struct class *class;
	unsigned blocklevel;
};

struct classtype {
	char *name;
	struct class *class;
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
	int include_ext_in_len;   /* length of include_ext_in, optimization */
	struct dynarr classes;        /* struct class pointers */
	struct dynarr classtypes;     /* struct classtype pointers */
	struct dynarr globals;        /* struct variable pointers */
	struct dynarr locals;         /* struct variable pointers */
	struct dynarr nested_locals;  /* struct variable pointers */
	struct dynarr inserts;        /* struct insert pointers */
	struct dynarr includepaths;   /* char pointers */
	struct dynarr file_stack;     /* struct parse_file pointers */
	struct dynarr files_seen;     /* struct file_id pointers */
	char printbuffer[OUTPR_MAX];  /* buffer for outprintf */
	char newfilename[NEWFN_MAX];  /* (stacked) store curdir + new input filename */
	char *newfilename_end;        /* pointer to end of new input filename */
};

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
		else
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

char *strprefixcmp(const char *s1, const char *s2)
{
	for (;; s1++, s2++) {
		if (*s1 == 0)
			return (char*)s2;
		if (*s1 != *s2)
			return NULL;
	}
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

int get_file_id(FILE *fp, struct file_id *out_id)
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
	uint32_t sizeHigh;

	return GetFileSize(fileno(fp), &sizeHigh) | ((uint64_t)sizeHigh << 32);
}

#else

int get_file_id(FILE *fp, struct file_id *out_id)
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

	if (fstat(fileno(fp), &stat) < 0)
		return 0;
	return stat.st_size;
}

#endif

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

int grow_dynarr(struct dynarr *dynarr)
{
	if (dynarr->num == dynarr->max) {
		unsigned newmax = dynarr->max ? dynarr->max * 2 : 16;
		void **newmem = realloc(dynarr->mem, newmax * sizeof(void*));
		if (newmem == NULL)
			return -1;
		memset(&newmem[dynarr->max], 0, (newmax - dynarr->max) * sizeof(void*));
		dynarr->mem = newmem;
		dynarr->max = newmax;
	}

	return 0;
}

int insert_dynarr(struct dynarr *dynarr, void **pos)
{
	memmove(pos, pos+1, (char*)(dynarr->mem + dynarr->num) - (char*)pos);
	dynarr->num++;
	return 0;
}

typedef int (*compare_cb)(const void *key, const void *item);

int bin_search_dynarr(const void *key, struct dynarr *dynarr, compare_cb cmp, void ***ret)
{
	void **try, **base = dynarr->mem;
	unsigned count = dynarr->num;
	int sign;

	if (count == 0) {
		*ret = base;
		return 1;
	}

	for (;;) {
		try = base + count/2;
		sign = cmp(key, *try);
		if (!sign || count == 1)
			break;
		if (sign < 0) {
			count /= 2;
		} else {
			base = try;
			count -= count/2;
		}
	}

	*ret = try;
	return sign;
}

void qsort_dynarr(struct dynarr *dynarr, compare_cb cmp)
{
	qsort(dynarr->mem, dynarr->num, sizeof(*dynarr->mem), cmp);
}

static struct class *addclass(struct parser *parser, char *classname, char *nameend)
{
	struct class *class;

	/* make sure class_pos will point somewhere valid */
	if (grow_dynarr(&parser->classes) < 0)
		return NULL;

	class = calloc(1, sizeof(*class));
	if (class == NULL)
		return NULL;

	class->name = stredup(classname, nameend);
	parser->classes.mem[parser->classes.num++] = class;
	parser->classes.sorted = 0;
	return class;
}

static struct classtype *addclasstype(struct parser *parser,
		char *typename, char *nameend, struct class *class)
{
	struct classtype *type;

	/* make sure type_pos will point somewhere valid */
	if (grow_dynarr(&parser->classtypes) < 0)
		return NULL;

	type = calloc(1, sizeof(*type));
	if (type == NULL)
		return NULL;

	type->name = stredup(typename, nameend);
	type->class = class;
	parser->classtypes.mem[parser->classtypes.num++] = type;
	parser->classtypes.sorted = 0;
	return type;
}

static struct member *addmember(struct class *class, char *rettype, char retsep,
		struct class *retclass, char *membername, char *params,
		char *declend, struct memberprops props)
{
	struct member *member;
	char *vmt_insert;

	if (grow_dynarr(&class->members) < 0)
		return NULL;

	member = calloc(1, sizeof(*member));
	if (member == NULL)
		return NULL;

	member->props = props;
	member->retclass = retclass;
	member->rettype = rettype;
	member->retsep = retsep;
	member->params = params;
	member->name = membername;
	if (props.is_function || props.is_static) {
		vmt_insert = props.is_virtual ? "vmt_" : "";
		member->insert = aprintf("%s_%s", class->name, vmt_insert);
	} else
		member->insert = NULL;

	class->members.mem[class->members.num++] = member;
	class->members.sorted = 0;
	return member;
}

static struct variable *addvariable(struct parser *parser, unsigned blocklevel,
		struct class *class, char *membername, char *nameend)
{
	struct dynarr *dynarr;
	struct variable *variable;

	if (!parser->locals.sorted && blocklevel == 0)
		dynarr = &parser->locals;
	else
		dynarr = &parser->nested_locals;

	if (grow_dynarr(dynarr) < 0)
		return NULL;

	variable = dynarr->mem[dynarr->num];
	if (!variable) {
		variable = calloc(1, sizeof(*variable));
		if (variable == NULL)
			return NULL;
		dynarr->mem[dynarr->num] = variable;
	}

	variable->name = stredupto(variable->name, membername, nameend);
	variable->class = class;
	variable->blocklevel = blocklevel;
	dynarr->sorted = 0;
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
	parser->inserts.sorted = 0;
	parser->inserts.num++;
	if (!after_pos) {
		/* check flush ordering to be incremental */
		for (after_pos = end_pos - 1;; after_pos--) {
			if ((void**)after_pos < parser->inserts.mem)
				break;
			if ((*after_pos)->flush_until < flush_until)
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

	dynarr->sorted = 0;
	dynarr->num++;
	return parse_file;
}

static struct file_id *insert_file_id(struct parser *parser, void **before_pos,
		struct file_id *file_id_src)
{
	struct file_id *file_id;
	void **end_pos;

	file_id = calloc(1, sizeof(*file_id));
	if (!file_id)
		return NULL;

	memcpy(file_id, file_id_src, sizeof(*file_id));
	end_pos = parser->files_seen.mem + parser->files_seen.num;
	memmove(before_pos+1, before_pos, (char*)end_pos - (char*)before_pos);
	*before_pos = file_id;
	parser->files_seen.num++;
	return file_id;
}

static void remove_locals(struct parser *parser, int blocklevel)
{
	struct variable *var;
	unsigned i;

	for (i = parser->nested_locals.num; i > 0;) {
		var = parser->nested_locals.mem[--i];
		if (var->blocklevel < blocklevel)
			break;
	}

	parser->nested_locals.num = i;
}

static int compare_names(const void *left, const void *right)
{
	const struct member *left_member = *(void**)left, *right_member = *(void**)right;
	return strcmp(left_member->name, right_member->name);
}

static void sort_dynarr(struct dynarr *dynarr)
{
	struct member **item, **cmp_last;

	if (dynarr->sorted)
		return;

	if (dynarr->mem) {
		qsort_dynarr(dynarr, compare_names);
		cmp_last = (struct member**)(dynarr->mem + dynarr->num - 1);
		for (item = (struct member**)dynarr->mem; item < cmp_last; item++) {
			if (item[0]->duplicate_pr)
				continue;
			if (strcmp(item[0]->name, item[1]->name) == 0) {
				fprintf(stderr, "duplicate '%s' found\n", item[0]->name);
				item[0]->duplicate_pr = item[1]->duplicate_pr = 1;
			}
		}
	}
	dynarr->sorted = 1;
}

static int compare_name_to(const void *key, const void *item)
{
	const struct member *member = item;
	return strcmp(key, member->name);
}

static int find_dynarr(struct dynarr *dynarr, char *name, void **retitem)
{
	void **array_pos;
	int ret;

	sort_dynarr(dynarr);
	ret = bin_search_dynarr(name, dynarr, compare_name_to, &array_pos);
	if (array_pos)
		*retitem = *array_pos;
	else
		*retitem = NULL;
	return ret;
}

static int find_dynarr_e(struct dynarr *dynarr, char *name, char *nameend, void **retitem)
{
	char oldch = *nameend;
	int ret;

	*nameend = 0;
	ret = find_dynarr(dynarr, name, retitem);
	*nameend = oldch;
	return ret;
}

static int find_class(struct parser *parser, char *classname, struct class **retclass)
{
	return find_dynarr(&parser->classes, classname, (void**)retclass);
}

static int find_class_e(struct parser *parser,
		char *classname, char *nameend, struct class **retclass)
{
	return find_dynarr_e(&parser->classes, classname, nameend, (void**)retclass);
}

static int find_classtype_e(struct parser *parser,
		char *typename, char *nameend, struct classtype **rettype)
{
	return find_dynarr_e(&parser->classtypes, typename, nameend, (void**)rettype);
}

static int find_member_e(struct class *class,
		char *membername, char *nameend, struct member **retmember)
{
	return find_dynarr_e(&class->members, membername, nameend, (void**)retmember);
}

static int find_global(struct parser *parser,
		char *name, char *nameend, struct class **retclass)
{
	return find_dynarr_e(&parser->locals, name, nameend, (void**)retclass);
}

static int find_local(struct parser *parser,
		char *name, char *nameend, struct class **retclass)
{
	unsigned i;
	struct variable **vars, *var;
	int ret;

	/* search backwards, hoping deeper nested is more often used */
	vars = (struct variable **)parser->nested_locals.mem;
	for (i = parser->nested_locals.num; i-- > 0;) {
		if (strprefixcmp(vars[i]->name, name) == nameend) {
			*retclass = vars[i]->class;
			return 0;
		}
	}

	ret = find_dynarr_e(&parser->locals, name, nameend, (void**)&var);
	if (ret)
		return ret;

	*retclass = var->class;
	return 0;
}

static int compare_file_id_to(const void *key, const void *item)
{
	const struct file_id *key_id = key, *item_id = item;
	if (key_id->file_id > item_id->file_id)
		return 1;
	if (key_id->file_id < item_id->file_id)
		return -1;
	if (key_id->dev_id > item_id->dev_id)
		return 1;
	if (key_id->dev_id < item_id->dev_id)
		return -1;
	return 0;
}

static int find_file_id(struct dynarr *dynarr, struct file_id *file_id, void ***id_pos)
{
	return bin_search_dynarr(file_id, dynarr, compare_file_id_to, id_pos);
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
		if (memcmp(parser->pf.outpos, buffer, size) == 0) {
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

struct class *parse_type(struct parser *parser, char *pos, char **ret_next)
{
	struct class *class;
	struct classtype *classtype;
	char *name, *next;

	if (strprefixcmp("struct ", pos)) {
		name = skip_whitespace(pos + 7);  /* "struct " */
		next = skip_word(name);
		if (find_class_e(parser, pos, next, &class))
			return NULL;
	} else {
		next = skip_word(parser->pf.pos);
		if (find_classtype_e(parser, pos, next, &classtype))
			return NULL;
		class = classtype->class;
	}

	*ret_next = next;
	return class;
}

static void print_vmt_type(struct parser *parser, struct class *class)
{
	struct member *member;
	int i;

	if (class->num_vfuncs == 0)
		return;

	outprintf(parser, "extern struct %s_vmt {\n", class->name);
	for (i = 0; i < class->members.num; i++) {
		member = class->members.mem[i];
		if (!member->props.is_virtual)
			continue;

		outprintf(parser, "\t%s%c(*%s)(%s;\n",
			member->rettype, member->retsep,
			member->name, member->params);
	}
	outprintf(parser, "} %s_vmt;\n\n", class->name);
}

enum func_decltype {
	MEMBER_FUNCTION,
	VIRTUAL_WRAPPER,
};

static void print_func_decl(struct parser *parser, struct class *class,
		struct member *member, enum func_decltype emittype)
{
	char *func_prefix, *name_insert, *func_body;
	char *p, *first_param, *last_word, *last_end;
	unsigned empty;

	first_param = skip_whitespace(member->params);
	empty = *first_param == ')';
	if (emittype == VIRTUAL_WRAPPER) {
		func_prefix = "coo_inline ";
		name_insert = "vmt_";
		func_body = "\n{";
	} else {
		func_prefix = name_insert = "";
		func_body = ";";   /* no body */
	}
	outprintf(parser, "%s%s%c%s_%s%s(struct %s *this%s%s%s\n",
		func_prefix, member->rettype, member->retsep,
		class->name, name_insert, member->name, class->name,
		empty ? "" : ", ", member->params, func_body);
	if (emittype == VIRTUAL_WRAPPER) {
		outprintf(parser, "\t((struct %s_vmt*)this->vmt)->%s(this",
			class->name, member->name);
		for (p = first_param; *p != ')'; p = skip_whitespace(p)) {
			last_word = NULL;
			/* search for last word, assume it is parameter name */
			for (; *p != ',' && *p != ')';) {
				if (*p == '/')
					p = skip_comment(p);
				else if (isalpha(*p)) {
					last_word = p;
					last_end = p = skip_word(p);
				} else
					p++;
			}
			if (last_word) {
				*last_end = 0;
				outprintf(parser, ", %s", last_word);
			}
			/* go to next argument */
			if (*p != ')')
				p++;
		}
		outprintf(parser, ");\n}\n\n");
	}
}

static void print_member_decls(struct parser *parser, struct class *class)
{
	struct member *member;
	unsigned i;

	if (class->num_vfuncs && !parser->pf.coo_inline_defined) {
		outputs(parser, DEF_COO_INLINE);
		parser->pf.coo_inline_defined = 1;
	}

	for (i = 0; i < class->members.num; i++) {
		member = class->members.mem[i];
		if (member->props.is_function) {
			print_func_decl(parser, class, member, MEMBER_FUNCTION);
		} else if (member->props.is_static) {
			outprintf(parser, "extern %s%c%s_%s;\n",
				member->rettype, member->retsep,
				class->name, member->name);
		}
	}

	if (!class->num_vfuncs)
		return;

	/* now generate all the virtual method call wrappers */
	outwrite(parser, "\n", 1);
	for (i = 0; i < class->members.num; i++) {
		member = class->members.mem[i];
		if (member->props.is_virtual)
			print_func_decl(parser, class, member, VIRTUAL_WRAPPER);
	}
}

static struct class *parse_struct(struct parser *parser, char *next)
{
	struct class *class, *retclass;
	struct memberprops props;
	char *declbegin, retsep, *membername, *nameend, *params, *declend, *nextdecl;
	char *classname, *retnext;
	int level, is_typedef;

	is_typedef = *parser->pf.pos == 't';
	/* skip 'typedef struct ' or just 'struct ' */
	classname = skip_whitespace(parser->pf.pos + (is_typedef ? 15 : 7));
	declend = skip_word(classname);
	if (declend != classname) {
		class = addclass(parser, classname, declend);
		if (!class)
			return NULL;
	} else
		class = NULL;

	declend = skip_whitespace(declend);
	if (*declend == ':') {
		/* TODO: handle inheritance */
	}

	level = 1;  /* next is at opening brace '{' */
	declbegin = NULL;
	for (;;) {
		/* search (sub)struct or end of variable or function prototype */
		next = skip_whitespace(next+1);
		if (declbegin == NULL) {
		  newdecl:
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
		if (level > 1)
			continue;
		if (isspace(*next))
			continue;

		declend = params = next;
		props.is_function = *params == '(';
		props.is_static = strprefixcmp("static ", declbegin) != NULL;

		/* skip pointers in membername */
		for (;; membername++) {
			/* is it a pointer to function variable? */
			if (*membername == '(') {
				props.is_function = 0;
				/* find real params */
				membername++;
				/* let loop skip '*' in front of function variable */
				continue;
			} else if (*membername != '*' && !isspace(*membername))
				break;
		}
		/* find nameend and real params in case of function pointer variable  */
		nameend = skip_word(membername);
		if (params < nameend) {
			params = scan_token(nameend, "/(;");
			if (!params)
				return NULL;
		}

		/* next is either '(' or ';', but declend must be at ';' */
		if (*params == '(' && !(declend = scan_token(params, "/;")))
			break;

		props.is_virtual = strprefixcmp("virtual ", declbegin) != NULL;
		if (props.is_virtual && !props.is_function) {
			fprintf(stderr, "Member variable cannot be virtual\n");
			continue;
		}

		/* do not print functions or static variables inside the struct
		   for consistency, flush always to start of declaration */
		nextdecl = skip_whitespace(declend+1);
		if (props.is_function || props.is_static)
			flush_until(parser, declbegin);
		else
			flush_until(parser, nextdecl);

		/* cut in pieces to store membername, and print modified later */
		retsep = *(membername-1);
		*(membername-1) = *nameend = *declend = 0;

		/* no need to check virtual and static because cannot both at declbegin */
		if (props.is_virtual)
			declbegin += 8;  /* skip "virtual " */
		if (props.is_static)
			declbegin += 7;  /* skip "static " */

		retclass = parse_type(parser, declbegin, &retnext);
		addmember(class, declbegin, retsep, retclass,
			membername, params+1, declend, props);
		/* if variable then flushed until nextdecl,
		   for function also skip lineending => go to nextdecl too
		   except for first virtual function, then we print vmt variable */
		parser->pf.pos = parser->pf.writepos = next = nextdecl;
		if (props.is_virtual) {
			if (!class->num_vfuncs) {
				outputs(parser, "void *vmt;");
				parser->pf.writepos = declend+1;
			}
			class->num_vfuncs++;
		}
		/* small optimization to skip whitespace, already in nextdecl */
		goto newdecl;
	}

	/* check for typedef struct X {} Y; */
	classname = skip_whitespace(next + 1);
	next = skip_word(classname);
	declend = scan_token(next, "/;");
	if (declend == NULL)
		return NULL;
	if (declend != next) {
		if (is_typedef)
			addclasstype(parser, classname, next, class);
		else {
			/* TODO: addglobal() */
		}
	}
	parser->pf.pos = skip_whitespace(declend + 1);
	flush(parser);
	print_vmt_type(parser, class);
	print_member_decls(parser, class);
	return class;
}

int parse_member(struct parser *parser, char *exprstart, char *exprend,
	struct class *class, char *name, char *nameend, struct class **retclass)
{
	struct member *member;
	struct insert **after_pos = NULL;
	char *args, *flush_until, *insert_text, *continue_at;
	int add_this;

	if (class == NULL)
		return -1;
	if (find_member_e(class, name, nameend, &member))
		return -1;

	if (exprstart && member->props.is_static) {
		fprintf(stderr, "Cannot access static member\n");
		return -1;
	}

	if (member->insert)
		after_pos = addinsert(parser, after_pos,
			exprstart ?: name, member->insert, name);

	continue_at = NULL;
	add_this = !exprstart && !member->props.is_static;
	if (member->props.is_function) {
		args = scan_token(nameend, "/(;");
		if (args != NULL && *args == ';')
			args = NULL;
		if (args != NULL)
			args = skip_whitespace(args+1);
		if (args != NULL) {
			continue_at = args;
			if (add_this) {
				flush_until = args;
				if (*args == ')')
					insert_text = "this";
				else
					insert_text = "this, ";
			} else {
				if (exprstart) {
					/* after other->function(), need to jump to
					   'other' expression and back to arguments */
					addinsert(parser, after_pos,
						args, "", exprstart);
					/* continue at end */
					after_pos = (struct insert **)
						&parser->inserts.mem[parser->inserts.num];
					flush_until = exprend;
				} else
					flush_until = name;
				if (*args == ')')
					insert_text = "";
				else
					insert_text = ", ";
			}
		}
	} else if (add_this) {
		flush_until = name;
		insert_text = "this->";
		continue_at = name;
	}

	if (continue_at)
		addinsert(parser, after_pos, flush_until, insert_text, continue_at);

	*retclass = member->retclass;
	return 0;
}

static void parse_typedef(struct parser *parser, char *declend)
{
	struct class *class;
	char *next;

	/* check if a class is aliased to a new name */
	class = parse_type(parser, parser->pf.pos, &next);
	if (class == NULL)
		return;

	/* note: after next cannot be '{', as in 'typedef struct X {'
	   it is handled by calling parse_struct in caller */
	parser->pf.pos = skip_whitespace(next);
	next = skip_word(parser->pf.pos);
	addclasstype(parser, parser->pf.pos, next, class);
	parser->pf.pos = declend + 1;
}

static void print_inserts(struct parser *parser)
{
	struct insert *insert;
	int i;

	if (parser->inserts.num == 0)
		return;

	for (i = 0; i < parser->inserts.num; i++) {
		insert = parser->inserts.mem[i];
		flush_until(parser, insert->flush_until);
		outputs(parser, insert->insert_text);
		parser->pf.writepos = insert->continue_at;
	}
	parser->inserts.num = 0;
}

static void parse_function(struct parser *parser, char *next)
{
	struct class *class, **class_p, *exprclass[MAX_PAREN_LEVELS];
	struct class *newclass, *declclass;
	struct classtype *classtype;
	struct member *member;
	char *curr, *funcname, *classname, *name, *argsep, *dblcolonsep;
	char *start, *exprstart[MAX_PAREN_LEVELS], *exprend;
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

	next++;  /* advance after '(' */
	if (funcname) {
		if (find_class(parser, classname, &class) == 0) {
			/* lookup method name */
			if (find_member_e(class, funcname, skip_word(funcname),
					&member) == 0) {
				if (member->props.is_virtual)
					class->num_vfuncs_seen++;
			} else {
				/* undeclared, so it's private, include static */
				flush(parser);
				outputs(parser, "static ");
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
			class = NULL;
		}
	}

	next = scan_token(start = next, "/)");
	if (next == NULL)
		return;
	next = skip_whitespace(next+1);
	if (*next == ';') {
		/* function prototype */
		parser->pf.pos = next + 1;
		return;
	}

	/* store parameters as variables */
	parser->locals.num = 0;
	parser->nested_locals.num = 0;
	state = FINDVAR;
	for (next = start;;) {
		curr = skip_whitespace(next);
		if (curr[0] == 0)
			return;
		if (strprefixcmp("const ", curr)) {
			next = curr + 6;  /* "const " */
			continue;
		} else if (strprefixcmp("struct ", curr)) {
			name = skip_whitespace(curr + 7);  /* "struct " */
			next = skip_word(name);
			if (state == DECLVAR) {
				/* grammar error, ignore */
				state = FINDVAR;
			} else if (find_class_e(parser, name, next, &declclass) >= 0) {
				state = DECLVAR;
			}
			continue;
		} else if (*curr == ',') {
			state = FINDVAR;
			next = curr + 1;
			continue;
		} else if (*curr == ')') {
			next = curr + 1;
			break;
		} else if (*curr == '*') {
			next = curr + 1;
			continue;
		} else if (!isalpha(*curr)) {
			next = curr + 1;
			continue;
		}

		name = curr;
		next = skip_word(name);
		switch (state) {
		case DECLVAR:
			addvariable(parser, 0, declclass, name, next);
			state = FINDVAR;
			break;
		default:
			if (find_classtype_e(parser, name, next, &classtype) == 0) {
				declclass = classtype->class;
				printf("class=%p %s\n", declclass, declclass->name);
				state = DECLVAR;
			}
			break;
		}
	}

	blocklevel = seqparen = parenlevel = 0;
	exprstart[parenlevel] = NULL;
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
		while (*curr == '*')  /* skip pointers, esp in declarations */
			curr++;
		if (isdigit(*curr))
			curr = skip_word(curr);
		else if (isalpha(*curr)) {
			if (!exprstart[parenlevel])
				exprstart[parenlevel] = curr;
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
				} else if (find_class_e(parser, name, next, &newclass) == 0) {
					goto decl_or_cast;
				}
				continue;
			}

			name = curr;
			next = skip_word(name);
			switch (state) {
			case DECLVAR:
				addvariable(parser, blocklevel, declclass, name, next);
				state = FINDVAR;
				break;
			case ACCESSMEMBER:
				parse_member(parser, exprstart[parenlevel], exprend,
					exprclass[parenlevel], name, next,
					&exprclass[parenlevel]);
				state = FINDVAR;
				break;
			default:
				class_p = &exprclass[parenlevel];
				if (find_local(parser, name, next, class_p)
					&& parse_member(parser, NULL, NULL,
						class, name, next, class_p) < 0
					&& find_global(parser, name, next, class_p)
					&& find_classtype_e(parser,
							name, next, &classtype) == 0) {
					newclass = classtype->class;
				  decl_or_cast:
					if (state == STMTSTART) {
						declclass = newclass;
						state = DECLVAR;
					} else if (seqparen >= 2
							&& !exprclass[parenlevel-2]) {
						/* this is a cast, like ((class_t*)x)->..
						   remember first detected class */
						exprclass[parenlevel-2] = newclass;
					} else {
						/* grammar error, ignore */
					}
				}
				break;
			}

			continue;
		}

		if (*curr != '.' && (*curr != '-' || curr[1] != '>'))
			exprstart[parenlevel] = NULL;
		if (*curr == '(') {
			exprclass[parenlevel] = NULL;
			exprstart[parenlevel] = curr;
			parenlevel++;
			seqparen++;
			exprstart[parenlevel] = NULL;
		} else
			seqparen = 0;
		if (*curr == ')' && parenlevel)
			parenlevel--;
		if (*curr == ',' && declclass && parenlevel == 0)
			state = DECLVAR;
		if (*curr == '.') {
			state = ACCESSMEMBER;
			exprend = curr;
		}
		if (*curr == '-' && curr[1] == '>') {
			state = ACCESSMEMBER;
			exprend = curr;
			curr++;
		}
		if (*curr == ';') {
			print_inserts(parser);
			declclass = NULL;
			seqparen = 0;
			parenlevel = 0;
		}

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
	unsigned i, j;

	for (i = 0; i < parser->classes.num; i++) {
		class = parser->classes.mem[i];
		if (!class->num_vfuncs_seen)
			continue;

		outprintf(parser, "\nstruct %s_vmt %s_vmt {\n", class->name, class->name);
		for (j = 0; j < class->members.num; j++) {
			member = class->members.mem[j];
			if (!member->props.is_virtual)
				continue;

			outprintf(parser, "\t%s_%s,\n", class->name, member->name);
		}
		outputs(parser, "};\n");
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
	struct file_id file_id;
	void **file_id_pos;
	FILE *fp_inc;
	int ret;

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
	if (get_file_id(fp_inc, &file_id) < 0)
		goto err_fp;
	if (grow_dynarr(&parser->files_seen) < 0)
		goto err_fp;
	ret = find_file_id(&parser->files_seen, &file_id, &file_id_pos);
	if (ret == 0)
		goto err_fp;
	if (ret < 0)
		file_id_pos++;
	insert_file_id(parser, file_id_pos, &file_id);
	parser->newfilename_end = p;
	return fp_inc;
err_fp:
	fclose(fp_inc);
	return NULL;
}

static int parse_source(struct parser *parser, char *filename, FILE *in, char *ext_out);

static int try_include_file(struct parser *parser, char *dir, char *nameend)
{
	FILE *fp_inc;
	char *buffer, *outbuffer, *fullname, *filename, *outfilename;
	char *p, *new_newfilename_file;
	struct parse_file *parse_file;

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
	parse_source(parser, filename, fp_inc, parser->header_ext_out);
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
	/* write new output filename, filename generated by
	 * parse_source_size in newfilename_path for rename() target
	 * parser->pf.pos is at the filename after '"' or '<', write it as well */
	flush_until(parser, parser->pf.pos);
	p = new_newfilename_file;  /* cannot use pf.new_filename_file, moved */
	outwrite(parser, p, parser->newfilename_end - p);
	/* nameend is at '"' or '>' at end of #include statement, write it as well */
	parser->pf.writepos = nameend;
	/* fp_inc was closed by parse_source */
	return 0;
err_add:
	fclose(fp_inc);
	return -1;
}

static void try_include(struct parser *parser, char *nameend, enum include_location loc)
{
	char *ext;
	unsigned i;

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
	for (i = 0; i < parser->includepaths.num; i++)
		if (try_include_file(parser, parser->includepaths.mem[i], nameend) == 0)
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
		if (*parser->pf.pos == '"') {
			for (next = parser->pf.pos + 1;;) {
				next = strchr(next, '"');
				if (next == NULL)
					break;
				if (*(next-1) != '\\')
					break;
			}

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

	flush(parser);
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
			prepare_output_file(parser);
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

	return 0;
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

int parseextoption(struct parser *parser, char *option)
{
	switch (*option) {
	case 'i':
		parser->include_ext_in = option+1;
		parser->include_ext_in_len = strlen(parser->include_ext_in);
		return 0;
	case 's': parser->source_ext_out = option+1; return 0;
	case 'h': parser->header_ext_out = option+1; return 0;
	default: usage(); return -1;
	}
}

int main(int argc, char **argv)
{
	struct parser parser_s, *parser = &parser_s;

	memset(&parser_s, 0, sizeof(parser_s));
	parser->pf.newfilename_path = parser->pf.newfilename_file =
		parser->newfilename_end = parser->newfilename;
	parser->source_ext_out = ".coo.c";
	parser->header_ext_out = ".coo.h";
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
