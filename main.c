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

#define MEMBLOCK_SIZE      65536
#define NEWFN_MAX          8192            /* (stack)size for included dirs+filenames */
#define OUTPR_MAX          256             /* maximum outprintf size we do */
#define STDIN_BUFSIZE      (1024 * 1024)   /* size for inputbuffer when using stdin */
#define MAX_PAREN_LEVELS   16
#define MAX_USER_ERRORS    25

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
	FINDVAR, STMTSTART, DECLVAR, DECLMETHODVAR, CONSTRUCT, CASTVAR,
	ACCESSMEMBER, ACCESSINHERITED, EXPRUNARY
};

enum visibility {
	PUBLIC,      /* default is public, like C struct */
	PROTECTED,
	PRIVATE,
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
	enum visibility visibility:2;
};

enum { AT_UNKNOWN, AT_CLASS, AT_FUNC, AT_METHOD, AT_TEMPLPAR, AT_TEMPLINST };

struct anytype {
	union {
		struct class *class;
		struct methodptr *mp;
		struct templpar *tp;    /* template parameter type */
	} u;
	struct dynarr args;          /* struct anytype *, template arguments */
	char pointerlevel;
	unsigned char type;          /* AT_xxxx */
	unsigned char implicit;      /* for AT_CLASS, still needs "struct " */
};

struct class {
	struct hash members;
	struct dynarr members_arr;
	struct hash templpars;       /* template parameters for this class (types) */
	struct dynarr templ_arr;     /* ... in order of declaration for parent mapping */
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
	struct parent *dync_parents; /* linked list of dyncastable parents */
	struct parent *dync_lastp;   /* last dyncastable parent to add after */
	char *funcinsert;            /* insert in front of function call "class_" */
	unsigned num_parent_constr;  /* number of parent constructors */
	unsigned num_parent_destr;   /* number of parent destructors */
	unsigned num_init_vars;      /* number of members needing root construction call */
	unsigned num_abstract;       /* number of abstract virtual methods */
	unsigned num_dync_parents;   /* number of parents for dyncasting to */
	char need_dync_p0_dummy;     /* first dync.parent is not at offset 0 */
	char declare_complete;       /* inside or after struct declaration? */
	char is_final;               /* final class, cannot be inherited from */
	char no_dyncast;             /* user disabled dyncasting to this class */
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

struct parent {
	struct class *class;       /* parent class */
	struct class *child;       /* inheriting class */
	struct parent *next_dync;  /* next dyncastable parent */
	struct dynarr templ_map;   /* struct templpar of child */
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
	struct class *class;       /* vmt this class belongs to */
	struct class *origin;      /* where this vmt is first defined */
	struct vmt *modified;      /* parent most recently modified */
	struct vmt *parent;        /* pointer to same origin vmt in parent */
	struct vmt *child;         /* temp.during struct definition child */
	char *path;                /* path to reach this vmt (in ancestor) */
	char *access;              /* path->vmt or path.vmt? */
	char *sec_name;            /* name for use as secondary vmt */
	unsigned char is_primary;  /* is this the primary vmt for this class? */
	unsigned char is_diverged; /* path to origin != ancestor->path */
	unsigned char is_parent_virtual;  /* how to access parent, if any */
	unsigned char from_virtual;  /* vmt inherited from virtual origin */
	char *name;                /* vmt struct type and variable name */
};

struct templpar {
	struct hash_entry node;    /* entry in class->templpars */
	struct class *impl;        /* implementation type, NULL => "void *" */
	char *implstr;             /* representation string of impl */
	unsigned index;            /* this type's index into class->templ_arr */
	char name[];               /* declared name of template type */
};

struct member {
	struct hash_entry node;    /* entry in class->members */
	char *rettypestr;          /* literal text of return type */
	struct anytype rettype;    /* return type of member */
	struct class *origin;      /* origin class where member was defined */
	struct class *definition;  /* closest class virtual member overridden */
	struct class *visi_define; /* closest class visibility was defined */
	struct vmt *vmt;           /* vmt this member is defined in */
	char *paramstext;          /* literal text of all params definition */
	struct dynarr params;      /* struct anytype *, params' type */
	char *parentname;   /* insert after function first argument expression */
	  /* for a function: x->func() ==> x_func(&x.parent) */
	char *nameinsert;   /* insert this in front of membername when using */
	  /* for this class: NULL or empty string
	     for inherited: field1 ==> parent.field1
	     for static variable: field1 ==> classname_field1 */
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
DEFINE_STRHASH_FIND_E(class, templpars, templpar, templpar)

struct variable {
	struct anytype decl;      /* type, or return type for funcvar/methodptr */
	struct dynarr params;     /* struct anytype *, function params */
	unsigned blocklevel;
	struct hash_entry node;
	char name[];
};

enum typedef_implicit { TYPEDEF_EXPLICIT, TYPEDEF_IMPLICIT };

struct classtype {
	struct class *class;      /* class for this declaration */
	struct hash_entry node;   /* node in parser.classtypes */
	char pointerlevel;        /* pointerlevel for this declaration */
	char implicit;            /* implicitly declared? or user typedef'ed */
	char name[];
};

struct methodptr {
	struct anytype rettype;   /* return type of methods of this type */
	char *rettypestr;         /* return type string to copy for casting */
	struct dynarr params;     /* parameters in case class parsing is needed */
	struct hash_entry node;   /* node in parser.methodptrtypes */
	char name[];
};

struct insert {   /* an insert, to insert some text in the output */
	char *flush_until;   /* flush output until here (expression start) */
	char *insert_text;   /* text to insert: e.g. 'this->' or 'vt->' */
	char *continue_at;   /* where to continue writing (perhaps skip something) */
	int continue_after;  /* later-insert before or after this insert? */
	int free_insert;     /* was insert_text alloc'ed? */
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
	int outfailed;            /* prevent error spam if creating file failed */
	int lineno;               /* line number of currently parsing line */
	int lines_coo;            /* number of (full) lines generated by this parser */
	enum line_pragma_mode line_pragma_mode;  /* refer to input file or not? */
	char coo_rtl_included;    /* included coortl.h? */
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
	struct hash methodptrtypes;   /* struct methodptr pointers */
	struct dynarr initializers;   /* struct initializer pointers */
	struct dynarr disposers;      /* struct disposer pointers */
	struct dynarr nested_locals;  /* struct variable pointers */
	struct dynarr inserts_arr;    /* struct insert pointers (outer layer) */
	struct dynarr includepaths;   /* char pointers */
	struct dynarr file_stack;     /* struct parse_file pointers */
	struct dynarr param_types;    /* struct anytype *, collect params of funcs */
	struct dynarr implicit_structs;  /* char *, param positions to add "struct " */
	struct dynarr *inserts;       /* either inserts_arr or a initializer's */
	struct hash files_seen;       /* struct file_id pointers */
	char *newfilename_end;        /* pointer to end of new input filename */
	char *memblock;               /* current memory block for allocation */
	char *memptr;                 /* pointer to next available memory */
	size_t memavail;              /* available memory in current block */
	struct class *class;          /* parsing function of this class */
	char *last_parentname;        /* cache to optimize parentname init */
	int include_ext_in_len;       /* length of include_ext_in, optimization */
	int num_errors;               /* user errors, prevent spam */
	int tempscope_varnr;          /* unique nr for vars in tempscope */
	char line_pragmas;            /* print line pragmas for compiler lineno */
	char saw_nodyncast;           /* saw the nodyncast keyword */
	char saw_final;               /* saw the final keyword */
	char saw_typedef;             /* saw the typedef keyword */
	char coo_includes_pr;         /* printed necessary includes for coo class vars? */
	char newfilename[NEWFN_MAX];  /* (stacked) store curdir + new input filename */
	char printbuffer[OUTPR_MAX];  /* buffer for outprintf */
};

DEFINE_STRHASH_FIND(parser, classes, class, class)
DEFINE_STRHASH_FIND_E(parser, classes, class, class)
DEFINE_STRHASH_FIND_E(parser, classtypes, classtype, classtype)
DEFINE_STRHASH_FIND_E(parser, globals, global, variable)
DEFINE_STRHASH_FIND_E(parser, locals, local, variable)
DEFINE_STRHASH_FIND_E(parser, methodptrtypes, methodptr, methodptr)

pid_t g_pid;

static void *palloc(struct parser *parser, size_t alignmask, size_t size)
{
	char *newblock;

	/* sanity check, only meant for small blocks */
	if (size > MEMBLOCK_SIZE / 2)
		return NULL;
	if (parser->memavail < size) {
		newblock = malloc(MEMBLOCK_SIZE);
		if (!newblock)
			return NULL;
		/* simple linked list */
		*(void**)newblock = parser->memblock;
		parser->memblock = newblock;
		parser->memptr = newblock + sizeof(void*);
		parser->memavail = MEMBLOCK_SIZE - sizeof(void*);
	}

	newblock = (void*)((size_t)(parser->memptr + alignmask) & ~alignmask);
	parser->memavail -= newblock + size - parser->memptr;
	parser->memptr = newblock + size;
	return newblock;
}

#define pstralloc(p,s) palloc(p,0,s)
#define pgenalloc(p,s) palloc(p,sizeof(void*)-1,s)

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

static int iswordstart(int ch)
{
	return isalpha(ch) || ch == '_';
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

static char *pstredup(struct parser *parser, const char *src, const char *until)
{
	int len = until-src;
	char *newdest = pstralloc(parser, len+1);
	if (newdest == NULL)
		return NULL;
	memcpy(newdest, src, len);
	newdest[len] = 0;
	return newdest;
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

static char *vsmaprintf(char *buffer, size_t size, const char *format, va_list ap)
{
	va_list ap2;
	char *str;
	int len;

	va_copy(ap2, ap);
	len = vsnprintf(buffer, size, format, ap2);
	va_end(ap2);

	if ((size_t)len < size)
		return buffer;
	if ((len < 0) || (str = malloc(len+1)) == NULL)
		return NULL;
	vsnprintf(str, len+1, format, ap);
	return str;
}

static char *vsaprintf(const char *format, va_list ap)
{
	return vsmaprintf(NULL, 0, format, ap);
}

static attr_format(3,4) char *smaprintf(char *buffer, size_t size, const char *format, ...)
{
	va_list va_args;

	va_start(va_args, format);
	return vsmaprintf(buffer, size, format, va_args);
}

static attr_format(2,3) char *psaprintf(struct parser *parser, const char *format, ...)
{
	va_list va_args, va2;
	char *str;
	int len;

	va_start(va_args, format);
	va_copy(va2, va_args);
	len = vsnprintf(NULL, 0, format, va2);
	va_end(va2);
	if ((len < 0) || (str = pstralloc(parser, len+1)) == NULL)
		return NULL;
	vsnprintf(str, len+1, format, va_args);
	return str;
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

#ifdef __linux__
#define st_mtimespec st_mtim
#endif

static int get_file_id(FILE *fp, struct file_id *out_id)
{
	struct stat stat;

	if (fstat(fileno(fp), &stat) < 0)
		return -1;

	out_id->dev_id = stat.st_dev;
	out_id->file_id = stat.st_ino;
	out_id->mtime = stat.st_mtimespec.tv_sec * 1000000ull
	                + stat.st_mtimespec.tv_nsec;
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

static void flush_skipto(struct parser *parser,
		char *until, int until_lineno, char *continue_at)
{
	flush_until(parser, until);
	parser->pf.writepos = continue_at;
	parser->pf.lines_coo -= parser->pf.lineno - until_lineno;
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

static void switch_line_pragma(struct parser *parser, enum line_pragma_mode newmode)
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

static char *open_tempscope(struct parser *parser, int *varnr)
{
	/* varnr > 0 <=> scope open */
	if ((*varnr = parser->tempscope_varnr++) > 0)
		return "";
	return "{ ";
}

static void close_tempscope(struct parser *parser, char *pos)
{
	if (parser->tempscope_varnr == 0)
		return;
	parser->pf.pos = pos;
	flush(parser);
	outwrite(parser, " }", 2);
	parser->tempscope_varnr = 0;
}

/*** parser helpers ***/

static void *realloc_namestruct(struct parser *parser, void *ptr,
		size_t structsize, char *name, char *nameend)
{
	size_t len = nameend-name, allocsize = structsize + len + 1;
	char *ret, *dest;

	if (parser)
		ret = pgenalloc(parser, allocsize);
	else
		ret = realloc(ptr, allocsize);
	if (ret == NULL)
		return NULL;
	/* if never allocated yet, zero-initialize */
	if (!ptr)
		memset(ret, 0, structsize);

	dest = ret + structsize;
	memcpy(dest, name, len);
	dest[len] = 0;
	return ret;
}

#define alloc_namestruct(p,s,n,e) realloc_namestruct(p, NULL, offsetof(s, name), n, e)

static struct class *initclass(struct parser *parser, struct class *class)
{
	strhash_init(&class->members, 8, member);
	strhash_init(&class->templpars, 4, templpar);
	hasho_init(&class->ancestors, 8);
	if (hash_insert(&parser->classes, &class->node, strhash(class->name))) {
		/* already exists */
		return NULL;
	}
	return class;
}

static struct class *addclass(struct parser *parser, char *classname, char *nameend)
{
	struct class *class;

	class = alloc_namestruct(parser, struct class, classname, nameend);
	if (class == NULL)
		return NULL;

	return initclass(parser, class);
}

static char *get_class_funcinsert(struct parser *parser, struct class *class)
{
	if (!class->funcinsert)
		class->funcinsert = psaprintf(parser, "%s_", class->name);

	return class->funcinsert;
}

static struct classtype *addclasstype(struct parser *parser, char *typename,
		char *nameend, struct anytype *decl)
{
	struct classtype *type;

	type = alloc_namestruct(parser, struct classtype, typename, nameend);
	if (type == NULL)
		return NULL;

	type->class = decl->u.class;
	type->pointerlevel = decl->pointerlevel;
	type->implicit = decl->implicit;
	if (strhash_insert(&parser->classtypes, type)) {
		/* already exists, no free necessary, using parser memory */
		return NULL;
	}
	return type;
}

static struct templpar *addtemplpar(struct parser *parser, struct class *class,
		char *typename, char *nameend, struct class *impl)
{
	struct templpar *par;

	if (grow_dynarr(&class->templ_arr) < 0)
		return NULL;

	par = alloc_namestruct(parser, struct templpar, typename, nameend);
	if (par == NULL)
		return NULL;

	if (strhash_insert(&class->templpars, par)) {
		/* already exists */
		free(par);
		return NULL;
	}

	par->index = class->templ_arr.num;
	par->impl = impl;
	par->implstr = impl ? psaprintf(parser, "%s *", impl->name) : "void *";
	class->templ_arr.mem[par->index] = par;
	class->templ_arr.num++;
	return par;
}

static int copy_anytype(struct anytype *dest, struct anytype *src)
{
	unsigned i;

	/* copy members individually because we want deep copy of args (if applicable) */
	dest->u.class = src->u.class;  /* any of the pointers is OK */
	dest->pointerlevel = src->pointerlevel;
	dest->type = src->type;
	dest->implicit = src->implicit;
	if (src->type == AT_TEMPLINST) {
		if (grow_dynarr_to(&dest->args, src->args.num) < 0)
			return -1;

		dest->args.num = src->args.num;
		for (i = 0; i < src->args.num; i++) {
			if (dest->args.mem[i] == NULL) {
				dest->args.mem[i] = malloc(sizeof(struct anytype));
				if (dest->args.mem[i] == NULL)
					return -1;
			}
			if (copy_anytype(dest->args.mem[i], src->args.mem[i]) < 0)
				return -1;
		}
	}

	return 0;
}

static struct class *to_class(struct anytype *any)
{
	if (any->type == AT_CLASS || any->type == AT_TEMPLINST)
		return any->u.class;
	return NULL;
}

static struct methodptr *to_mp(struct anytype *any)
{
	return any->type == AT_METHOD ? any->u.mp : NULL;
}

#if 0
static struct templpar *to_tp(struct anytype *any)
{
	return any->type == AT_TEMPLPAR ? any->u.tp : NULL;
}
#endif

struct anytype *addargtype(struct dynarr *templ_map)
{
	struct anytype *rettype;

	if (grow_dynarr(templ_map) < 0)
		return NULL;
	rettype = templ_map->mem[templ_map->num];
	if (!rettype) {
		rettype = malloc(sizeof(struct anytype));
		if (!rettype)
			return NULL;
		templ_map->mem[templ_map->num] = rettype;
	}
	templ_map->num++;
	return rettype;
}

typedef void (*insert_implicit_cb)(struct parser *parser, char *position);
typedef void (*new_param_cb)(struct parser *parser, int position,
		struct class *class, struct anytype *decl, char *name, char *next);
typedef void (*check_name_cb)(struct parser *parser, char *name, char *next);

static char *parse_parameters(struct parser *parser, struct class *class, char *params,
		insert_implicit_cb new_implicit, new_param_cb new_param,
		check_name_cb check_name)
{
	struct classtype *classtype;
	struct anytype decl;
	char *next, *curr, *name;
	enum parse_state state;
	int position;

	name = NULL;
	state = FINDVAR;
	memset(&decl, 0, sizeof(decl));
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
			} else if ((decl.u.class = find_class_e(parser, name, next)) != NULL) {
				decl.type = AT_CLASS;
				state = DECLVAR;
			}
			continue;
		} else if (*curr == ',' || *curr == ')') {
			if (name && check_name)
				check_name(parser, name, curr);
			if (*curr == ')')
				return curr + 1;
			/* it's a comma, continue with next parameter */
			state = FINDVAR;
			next = curr + 1;
			position++;
			decl.pointerlevel = 0;
			name = NULL;
			continue;
		} else if (!iswordstart(*curr)) {
			if (*curr == '*')
				decl.pointerlevel++;
			next = curr + 1;
			continue;
		}

		name = curr;
		next = skip_word(name);
		switch (state) {
		case DECLVAR:
			new_param(parser, position, class, &decl, name, next);
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
				decl.type = AT_CLASS;
				decl.u.class = classtype->class;
				decl.pointerlevel = classtype->pointerlevel;
				state = DECLVAR;
			} else if ((decl.u.mp = find_methodptr_e(parser, name, next)) != NULL) {
				/* TODO: test this */
				decl.type = AT_METHOD;
				state = DECLVAR;
			} else if (class == NULL) {
				break;
			} else if ((decl.u.tp = find_templpar_e(class, name, next)) != NULL) {
				decl.type = AT_TEMPLPAR;
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

static struct member *allocmember_e(struct parser *parser, struct class *class,
		char *membername, char *nameend, struct member **dupmember)
{
	struct member *member;

	member = alloc_namestruct(parser, struct member, membername, nameend);
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
		return NULL;
	return member;
}

static struct member *dupmemberto(struct parser *parser, struct class *class,
		struct member *parentmember, struct member **dupmember)
{
	int size = offsetof(struct member, name) + strlen(parentmember->name) + 1;
	struct member *member;

	member = pgenalloc(parser, size);
	if (member == NULL)
		return NULL;

	memcpy(member, parentmember, size);
	if (insertmember(class, member, dupmember) < 0)
		return NULL;
	return member;
}

static char *get_member_nameinsert(struct parser *parser,
		struct class *class, struct member *member)
{
	if (member->nameinsert)
		return member->nameinsert;

	/* static variables have class_ in front, just like functions */
	if (member->props.is_static)
		member->nameinsert = get_class_funcinsert(parser, class);
	else if (member->parentname)
		member->nameinsert = psaprintf(parser, "%s%s",
			member->parentname, member->parent_virtual ? "->" : ".");
	return member->nameinsert;
}

static void save_implicit_insert(struct parser *parser, char *position)
{
	if (grow_dynarr(&parser->implicit_structs))
		return;

	parser->implicit_structs.mem[parser->implicit_structs.num++] = position;
}

static void save_param_type(struct parser *parser, int position,
		struct class *class, struct anytype *decl, char *name, char *next)
{	(void)class; (void)name; (void)next;
	struct dynarr *dynarr = &parser->param_types;
	struct anytype *newtype;

	if (grow_dynarr_to(dynarr, position) < 0)
		return;

	newtype = malloc(sizeof(*newtype));
	if (newtype == NULL)
		return;

	memcpy(newtype, decl, sizeof(*newtype));
	dynarr->mem[position] = newtype;
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

static void parse_parameters_to(struct parser *parser, struct class *class,
	char *params, char **params_out, struct dynarr *param_types)
{
	char *paramsend;

	/* save_param_class stores params in parser->param_types */
	paramsend = parse_parameters(parser, class, params,
		params_out ? save_implicit_insert : NULL, save_param_type, NULL);
	/* move the dynarr (array pointer etc) to requested array */
	memcpy(param_types, &parser->param_types, sizeof(*param_types));
	memset(&parser->param_types, 0, sizeof(parser->param_types));
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
		struct anytype *rettype, char *retstr, char *retend,
		char *membername, char *nameend, char *params, struct memberprops props)
{
	struct member *member, *dupmember;
	char *end, *parsepos = retstr, void_ret;

	member = allocmember_e(parser, class, membername, nameend, &dupmember);
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
	member->visi_define = class;
	member->props = props;
	if (props.is_abstract)
		class->num_abstract++;
	if (rettype->type == AT_METHOD && !params) {
		copy_anytype(&member->rettype, rettype);
		member->rettypestr = rettype->u.mp->rettypestr;
		member->paramstext = "";
		memcpy(&member->params, &rettype->u.mp->params, sizeof(member->params));
		goto out;
	}

	copy_anytype(&member->rettype, rettype);
	if (params) {
		if (is_void_params(params))
			member->paramstext = ")";
		else
			parse_parameters_to(parser, class,
				params, &member->paramstext, &member->params);
	}
	if (props.is_virtual) {
		member->vmt = class->vmt;
		class->vmt->modified = class->vmt;
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
					member->rettypestr = psaprintf(parser,
						"struct %s *", class->name);
					member->rettype.type = AT_CLASS;
					member->rettype.u.class = class;
					member->rettype.pointerlevel = 1;
				} else if (to_class(rettype) != class
						|| rettype->pointerlevel != 1)
					pr_err(parsepos, "constructor must be void or return "
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
		member->rettypestr = "void ";
	}

	if (!member->rettypestr) {
		if (to_class(rettype) && rettype->implicit) {
			member->rettypestr = malloc(retend - retstr + 8);  /* "struct " + null-term */
			memcpy(member->rettypestr, "struct ", 7);
			memcpy(member->rettypestr + 7, retstr, retend - retstr);
			member->rettypestr[7 + retend - retstr] = 0;
		} else if (retend == retstr) {
			member->rettypestr = "";
		} else
			member->rettypestr = pstredup(parser, retstr, retend);
	}

	/* in case of defining functions on the fly,
	   insert into classes inheriting from this class as well */
out:
	addmember_to_children(parser, membername, class, member);
	return member;
}

void addgenmember(struct parser *parser, struct class *class,
		struct member *mimic, char *name, char *nameend)
{
	struct memberprops constructorprops = {0,};
	struct anytype rettype;

	constructorprops.is_function = 1;
	rettype.type = AT_UNKNOWN;
	/* retstr/retend NULL is detected as not present => use void behavior */
	addmember(parser, class, &rettype, NULL, NULL,
		name, nameend, mimic ? mimic->paramstext : ")", constructorprops);
}

struct member *find_eqv_member(struct parser *parser,
		struct class *class, struct member *member)
{
	struct member *eqvmember;

	eqvmember = find_member_e(class, member->name,
		member->name + strlen(member->name));
	if (eqvmember == NULL)
		pr_err(NULL, "(ierr) cannot find equivalent "
			"member %s in %s", member->name, class->name);

	return eqvmember;
}

static struct variable *addvariable_common(struct parser *parser, unsigned blocklevel,
		struct anytype *decl, int extraptr, char *membername, char *nameend)
{
	struct dynarr *dynarr = &parser->nested_locals;
	struct variable *variable;

	if (grow_dynarr(dynarr) < 0)
		return NULL;

	/* variables are volatile, use reallocable memory, not parser */
	variable = realloc_namestruct(NULL, dynarr->mem[dynarr->num],
		offsetof(struct variable, name), membername, nameend);
	if (variable == NULL)
		return NULL;

	dynarr->mem[dynarr->num] = variable;
	if (strhash_insert(&parser->locals, variable)) {
		/* no need to free variable, ref stored in dynarr */
		return NULL;
	}

	copy_anytype(&variable->decl, decl);
	variable->decl.pointerlevel += extraptr;  /* typedef type + extra pointers */
	variable->blocklevel = blocklevel;
	dynarr->num++;
	return variable;
}

static struct variable *addvariable(struct parser *parser, struct class *class,
		unsigned blocklevel, struct anytype *decl, int extraptr,
		char *membername, char *nameend, char *params)
{
	struct variable *variable;

	variable = addvariable_common(parser, blocklevel, decl, extraptr, membername, nameend);
	if (variable == NULL)
		return NULL;
	if (params)
		parse_parameters_to(parser, class, params, NULL, &variable->params);
	return variable;
}

static struct variable *addclsvariable(struct parser *parser,
		unsigned blocklevel, struct anytype *decl,
		int extraptr, char *membername, char *nameend)
{
	return addvariable_common(parser, blocklevel, decl, extraptr, membername, nameend);
}

static struct variable *addmpvariable(struct parser *parser, unsigned blocklevel,
		struct anytype *mpdecl, int extraptr, char *membername, char *nameend)
{
	struct variable *variable;

	variable = addvariable_common(parser, blocklevel,
		mpdecl, extraptr, membername, nameend);
	if (variable == NULL)
		return NULL;

	memcpy(&variable->params, &mpdecl->u.mp->params, sizeof(variable->params));
	return variable;
}

static struct methodptr *allocmethodptrtype(struct parser *parser,
		struct anytype *rettype, char *name, char *nameend)
{
	struct methodptr *mptype;

	mptype = alloc_namestruct(parser, struct methodptr, name, nameend);
	if (mptype == NULL)
		return NULL;
	if (strhash_insert(&parser->methodptrtypes, mptype))
		return NULL;

	copy_anytype(&mptype->rettype, rettype);
	return mptype;
}

static struct methodptr *addmethodptrtype(struct parser *parser, struct anytype *rettype,
	char *rettypestr, char *rettypeend, char *name, char *nameend, char *params)
{
	struct methodptr *mptype;

	mptype = allocmethodptrtype(parser, rettype, name, nameend);
	if (mptype == NULL)
		return NULL;

	mptype->rettypestr = pstredup(parser, rettypestr, rettypeend);
	parse_parameters_to(parser, NULL, params, NULL, &mptype->params);
	return mptype;
}

static struct methodptr *dupmethodptrtype(struct parser *parser,
		char *name, char *nameend, struct methodptr *source)
{
	struct methodptr *mptype;

	mptype = allocmethodptrtype(parser, &source->rettype, name, nameend);
	if (mptype == NULL)
		return NULL;

	mptype->rettypestr = strdup(source->rettypestr);
	memcpy(&mptype->params, &source->params, sizeof(mptype->params));
	return mptype;
}

static int is_nop_insert(struct insert *insert)
{
	return insert->flush_until == insert->continue_at
		&& insert->insert_text[0] == 0;
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
		insert = malloc(sizeof(*insert));
		if (insert == NULL)
			return -1;
		*end_pos = insert;
	}


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

	/* print_inserts_fromto might nop-out some inserts, see if we can reuse them */
	if (before_pos < end_pos && is_nop_insert(*before_pos)) {
		/* reuse, so inserts->num doesn't increase */
		insert = *before_pos;
	} else {
		parser->inserts->num++;
		if (before_pos < end_pos) {
			memmove(before_pos+1, before_pos, (char*)end_pos-(char*)before_pos);
			*before_pos = insert;
		}
	}

	insert->flush_until = flush_until;
	insert->insert_text = insert_text;
	insert->continue_at = continue_at;
	insert->continue_after = insert_continue;
	insert->free_insert = 0;
	/* return index of next entry to insert before */
	return (void**)before_pos + 1 - parser->inserts->mem;
}

static attr_format(6,7) int addinsert_format(struct parser *parser, int insert_index,
		char *flush_until, char *continue_at,
		enum insert_continue insert_continue, char *insert_format, ...)
{
	va_list va_args;
	char *insert_text;
	int index;

	va_start(va_args, insert_format);
	insert_text = vsaprintf(insert_format, va_args);
	if (insert_text == NULL)
		return -1;

	index = addinsert(parser, insert_index,
			flush_until, insert_text, continue_at, insert_continue);
	if (index < 0)
		return index;

	/* mark the text to be freed after use, next index returned, so subtract 1 */
	((struct insert *)parser->inserts->mem[index-1])->free_insert = 1;
	return index;
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

static void addinsert_templpar(struct parser *parser,
		struct templpar *tptype, char *name, char *next)
{
	addinsert(parser, -1, name, tptype->implstr, next, CONTINUE_AFTER);
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
		struct anytype *ret_decl, struct dynarr **retparams)
{
	struct variable *var = find_global_e(parser, name, nameend);
	if (var == NULL)
		return -1;
	copy_anytype(ret_decl, &var->decl);
	/* TODO: parse function parameters for variables (params always empty now) */
	*retparams = &var->params;
	return 0;
}

static int find_local_e_class(struct parser *parser, char *name, char *nameend,
		struct anytype *ret_decl, struct dynarr **retparams)
{
	struct variable *var = find_local_e(parser, name, nameend);
	if (var == NULL)
		return -1;
	copy_anytype(ret_decl, &var->decl);
	*retparams = &var->params;
	return 0;
}

static char *replace_templpar(struct parser *parser,
		struct templpar *par, char *pos, char *next)
{
	flush_until(parser, pos);
	if (par->impl)
		outprintf(parser, "struct %s *", par->impl->name);
	else
		outwrite(parser, "void *", 6);
	if (*next == ' ')
		next++;
	return parser->pf.writepos = next;
}

static char *parse_templargs(struct parser *parser, struct class *class,
		struct class *parentclass, char *next, struct dynarr *templ_map);

enum print_implicit {
	SKIP_IMPLICIT,   /* boolean compatible */
	PRINT_IMPLICIT,
};

static char *parse_type(struct parser *parser, struct class *class, char *pos,
		struct anytype *rettype, enum print_implicit print_implicit)
{
	struct classtype *classtype;
	struct templpar *templpar;
	char *name, *next;

	rettype->implicit = 0;
	rettype->args.num = 0;
	rettype->pointerlevel = 0;
	rettype->type = AT_UNKNOWN;
	for (;; pos = skip_whitespace(parser, pos)) {
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
			rettype->u.class = find_class_e(parser, name, next);
			if (rettype->u.class)
				goto class;
			break;
		}

		if (!iswordstart(*pos)) {
			next = pos;
			break;
		}
		next = skip_word(pos);
		if ((classtype = find_classtype_e(parser, pos, next)) != NULL) {
			if (print_implicit)
				print_implicit_struct(parser, classtype, pos);
			rettype->u.class = classtype->class;
			rettype->pointerlevel = classtype->pointerlevel;
			rettype->implicit = classtype->implicit;
		  class:
			rettype->type = AT_CLASS;
			if (*next == '<') {
				 next = parse_templargs(parser, class, classtype->class,
						next, &rettype->args);
			}
			break;
		}
		if ((rettype->u.mp = find_methodptr_e(parser, pos, next)) != NULL) {
			rettype->type = AT_METHOD;
			break;
		}
		if (class == NULL)
			break;
		if ((templpar = find_templpar_e(class, pos, next)) != NULL) {
			if (print_implicit)
				next = replace_templpar(parser, templpar, pos, next);
			rettype->u.tp = templpar;
			rettype->type = AT_TEMPLPAR;
			break;
		}
		break;
	}

	for (next = skip_whitespace(parser, next); *next == '*'; next++)
		rettype->pointerlevel++;
	return next;
}

static char *parse_templargs(struct parser *parser, struct class *class,
		struct class *parentclass, char *next, struct dynarr *templ_map)
{
	struct anytype *argtype;
	char *name, *sep;

	if (!parentclass->templpars.num_entries)
		pr_err(next, "%s is not a template class",
			parentclass->name);
	for (;;) {
		name = skip_whitespace(parser, next);
		if (!iswordstart(*name)) {
			pr_err(name, "expected a name");
			break;
		}
		argtype = addargtype(templ_map);
		if (!argtype)
			goto next;

		next = parse_type(parser, class, name, argtype, PRINT_IMPLICIT);
		if (argtype->type == AT_UNKNOWN)
			pr_err(name, "unknown template type");

	  next:
		next = skip_whitespace(parser, next);
		sep = next++;
		if (*sep == ',')
			continue;
		if (*sep != '>')
			pr_err(sep, "expected '>' or ','");
		break;
	}

	return next;
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

	member = dupmemberto(parser, class, parentmember, &dupmember);
	if (member == NULL) {
		if (dupmember) {
			pr_err(parsepos, "duplicate member %s from parent "
				"class %s and %s", parentmember->name,
				dupmember->origin->name, parent->class->name);
		}
		return NULL;
	}

	/* only primary if this and inherited are all primary */
	member->props.from_primary &= parent->is_primary;
	member->props.from_virtual |= parent->is_virtual;
	member->parent_constructor = parentmember->is_constructor;
	member->parent_destructor = parentmember->is_destructor;
	if (member->props.is_abstract)
		class->num_abstract++;
	if (parentmember->parentname) {
		if (parser->last_parentname) {
			member->parentname = parser->last_parentname;
		} else {
			member->parentname = psaprintf(parser, "%s%s%s",
				parent->class->name, parent->is_virtual ? "->" : ".",
				parentmember->parentname);
			parser->last_parentname = member->parentname;
		}
	} else {
		member->parentname = parent->class->name;
		member->parent_virtual = parent->is_virtual;
	}
	/* in case of defining functions on the fly,
	   insert into classes inheriting from this class as well */
	addmember_to_children(parser, parsepos, class, member);
	return member;
}

static void addmember_to_children(struct parser *parser, char *parsepos,
		struct class *class, struct member *member)
{
	struct parent *parent;
	unsigned i;

	for (i = 0; i < class->descendants.num; i++) {
		parent = class->descendants.mem[i];
		/* possibly different parent path, clear cache */
		parser->last_parentname = NULL;
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

static void add_ancestor(struct parser *parser, struct class *class,
		int from_virtual, struct parent *origin_parent,
		char *via_class_name, char *link, char *path)
{
	struct hasho_entry *orig_entry;
	struct ancestor *ancestor, *orig_ancestor;

	/* allocate struct and printf path in one alloc */
	ancestor = (void*)psaprintf(parser, "%*s%s%s%s",
			(int)offsetof(struct ancestor, path),
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

static void add_ancestors(struct parser *parser, struct class *class, struct parent *parent)
{
	struct class *parentclass = parent->class;
	struct hasho_entry *ancestor_entry;
	struct ancestor *ancestor;
	int from_virtual;

	add_ancestor(parser, class, parent->is_virtual, parent, parentclass->name, "", "");
	hasho_foreach(ancestor_entry, &parentclass->ancestors) {
		ancestor = ancestor_entry->value;
		do {
			from_virtual = ancestor->from_virtual | parent->is_virtual;
			add_ancestor(parser, class, from_virtual, ancestor->parent,
				parentclass->name, parent->is_virtual ? "->" : ".",
				ancestor->path);
			ancestor = ancestor->next;
		} while (ancestor);
	}
}

static char *alloc_vmt_name(struct parser *parser, struct vmt *vmt)
{
	char *vmt_sec_name, *vmt_sec_sep;

	/* secondary vmt? append origin class name to distinguish from primary */
	if (!vmt->is_primary) {
		vmt_sec_name = vmt->sec_name;
		vmt_sec_sep = "_";
	} else
		vmt_sec_name = vmt_sec_sep = "";

	/* generate vmt name, this vmt must be defined, because it is marked modified */
	vmt->name = psaprintf(parser, "%s%s%s_vmt",
		vmt->class->name, vmt_sec_sep, vmt_sec_name);
	return vmt->name;
}

static char *get_vmt_name(struct parser *parser, struct vmt *vmt)
{
	struct vmt *altvmt = vmt->modified;

	if (vmt->name)
		return vmt->name;
	if (altvmt->name)
		return vmt->name = altvmt->name;

	/* go to closest modified vmt, closer ones if any, are not generated */
	return vmt->name = alloc_vmt_name(parser, altvmt);
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
	return vmt_this_needs_offset(member) ? member->origin : member->definition;
}

static struct class *get_impl_this_class(struct member *member)
{
	return impl_this_needs_offset(member) ? member->origin : member->definition;
}

enum parent_virtual { LITERAL_PARENT, VIRTUAL_PARENT };  /* boolean compatible */

static struct vmt *addvmt(struct class *class, struct vmt *parent_vmt,
		struct class *origin, enum parent_virtual parent_virtual)
{
	struct vmt *vmt;

	if (grow_dynarr(&class->vmts) < 0)
		return NULL;

	vmt = calloc(1, sizeof(*vmt));
	if (vmt == NULL)
		return NULL;

	vmt->class = class;
	vmt->origin = origin;
	vmt->parent = parent_vmt;
	/* do not reuse virtual base class' vmt, is inefficient */
	vmt->is_primary = class->vmt == NULL;
	vmt->is_parent_virtual = parent_virtual;
	vmt->from_virtual = parent_virtual;
	class->vmts.mem[class->vmts.num++] = vmt;
	if (parent_vmt) {
		vmt->is_primary = vmt->is_primary && parent_vmt->is_primary && !parent_virtual;
		vmt->from_virtual |= parent_vmt->from_virtual;
		vmt->modified = parent_vmt->modified;
	} else {
		vmt->path = vmt->access = "";
		vmt->modified = vmt;
	}
	if (vmt->is_primary)
		class->vmt = vmt;
	return vmt;
}

static struct vmt *find_vmt_origin(struct class *class, struct class *vmt_origin)
{
	struct vmt *vmt;
	unsigned i;

	/* find same vmt in this class (same origin) */
	for (i = 0; i < class->vmts.num; i++) {
		vmt = class->vmts.mem[i];
		if (vmt->origin == vmt_origin)
			return vmt;
	}

	return NULL;
}

static void import_parent(struct parser *parser, char *parsepos,
		struct class *class, struct parent *parent)
{
	struct member *parentmember, *member;
	struct dynarr *parentmembers;
	struct class *origin, *currorigin, *ignoreorigin, *intforigin, *mergeorigin;
	struct class *vmtorigin, *parentclass = parent->class;
	struct vmt *vmt, *parentvmt;
	unsigned i, diverged;
	char *sec_name;
	int ret;

	if (parentclass->is_final)
		pr_err(parsepos, "cannot inherit from final class %s", parentclass->name);

	/* inherit vmts */
	for (i = 0; i < parentclass->vmts.num; i++) {
		parentvmt = parentclass->vmts.mem[i];
		vmtorigin = parentvmt->origin;
		sec_name = vmtorigin->name;
		diverged = 0;
		/* potential conflict with duplicate origin */
		if (hasho_find(&class->ancestors, vmtorigin)) {
			/* don't add virtual bases multiple times */
			if (parentvmt->from_virtual)
				continue;
			/* find same vmt added earlier for this origin */
			vmt = find_vmt_origin(class, vmtorigin);
			if (vmt == NULL) {
				pr_err(parsepos, "(ierr) vmt for parent %s origin %s not "
					"found", parentclass->name, vmtorigin->name);
				continue;
			}
			/* prefer literal bases over virtual ones */
			if (vmt->from_virtual) {
				/* parentvmt is literal (checked above), prefer that */
				vmt->parent = parentvmt;
				vmt->is_parent_virtual = 0;
				vmt->from_virtual = 0;
				continue;
			}
			if (hasho_find(&class->ancestors, parentvmt->modified)) {
				/* parent class last modified vmt in common ancestor
				   with earlier added same-origin vmt, no conflict */
				goto connect_vmt;
			}
			if (hasho_find(&parentclass->ancestors, vmt->modified)
					&& vmt->is_primary == parentvmt->is_primary
					&& !parent->is_virtual) {
				/* same-origin vmt was last modified in common ancestor
				   of parent class, but parent class modified its vmt
				   (previous check false), use that vmt */
				vmt->parent = parentvmt;
				vmt->modified = parentvmt->modified;
				goto connect_vmt;
			}
			/* both inheriting vmts have been modified, they have diverged
			   add a new vmt with same origin below */
			diverged = 1;
			/* use parent name instead of origin name, is clearer */
			sec_name = parentclass->name;
		}
		vmt = addvmt(class, parentvmt, parentvmt->origin, parent->is_virtual);
	  connect_vmt:
		parentvmt->child = vmt;
		vmt->is_diverged = diverged;
		vmt->sec_name = sec_name;
	}

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
			/* origin changed, clear parentname cache */
			parser->last_parentname = NULL;
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

		member = inheritmember(parser, parsepos, class, parent, parentmember);
		/* point to a vmt for this class */
		if (member->vmt) {
			if (member->vmt->child == NULL)
				pr_err(parsepos, "(ierr) parent's vmt does not point to ours?");
			else
				member->vmt = member->vmt->child;
		}
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

	/* count dynamic castable parents */
	if (parentclass->vmts.num && !parentclass->no_dyncast) {
		class->num_dync_parents++;
		if (!class->dync_parents)
			class->dync_parents = parent;
		else
			class->dync_lastp->next_dync = parent;
		class->dync_lastp = parent;
	} else if (class->ancestors.num_entries == 0)
		class->need_dync_p0_dummy = 1;

	/* add parents after vmt duplication check */
	add_ancestors(parser, class, parent);
	parentclass->descendants.mem[parentclass->descendants.num++] = parent;
}

static struct member *implmember(struct parser *parser, char *parsepos,
		struct class *class, char *membername, char *nameend, struct memberprops props)
{
	struct member *member;

	member = find_member_e(class, membername, nameend);
	if (member == NULL) {
		pr_err(parsepos, "cannot find member to override");
		return NULL;
	}

	if (!member->props.is_virtual) {
		pr_err(parsepos, "inherited member %s::%s is not virtual",
			member->origin->name, member->name);
		return NULL;
	}

	if (props.visibility != member->props.visibility) {
		if (props.visibility > member->props.visibility) {
			pr_err(parsepos, "cannot decrease visibility of inherited "
				"member %s::%s", member->visi_define->name, member->name);
			return NULL;
		}
		member->props.visibility = props.visibility;
		member->visi_define = class;
	}

	member->definition = class;
	member->vmt->modified = member->vmt;
	if (member->props.is_abstract) {
		member->props.is_abstract = 0;
		class->num_abstract--;
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
		member->rettypestr, class->name, member->implprefix, member->implname,
		class->name, sep_or_end, paramstext);
}

static int is_member_lit_var(struct class *class, struct member *member)
{
	return !member->props.is_function && to_class(&member->rettype)
		&& member->rettype.pointerlevel == 0 && member->origin == class;
}

static int member_needs_init(struct class *class, struct member *member)
{
	return is_member_lit_var(class, member)
		&& member->rettype.u.class->root_constructor;
}

static int member_needs_dispose(struct class *class, struct member *member)
{
	return is_member_lit_var(class, member)
		&& member->rettype.u.class->destructor;
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

/* also cleans up vmt child pointers, only accurate during class definition */
static void print_vmt_type(struct parser *parser, struct class *class)
{
	struct class *thisclass, *prvmtclass;
	struct member *member;
	struct vmt *vmt, *prvmt;
	unsigned i, j;

	for (i = 0; i < class->vmts.num; i++) {
		vmt = class->vmts.mem[i];
		if (vmt->parent)
			vmt->parent->child = NULL;
		if (vmt->modified != vmt)
			continue;

		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		if (!parser->pf.coo_rtl_included) {
			switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
			outputs(parser, "\n#include <coortl.h>");
			parser->pf.coo_rtl_included = 1;
			parser->pf.lines_coo++;
		}

		outprintf(parser, "\nextern const struct %s {\n"
			"\tstruct coo_vmt vmt_base;\n", get_vmt_name(parser, vmt));
		/* 3 lines here, 1 to close struct */
		parser->pf.lines_coo += 4;
		/* in case of diverging, the member->vmt might point to
		   different vmt; have to print parent class' vmt */
		for (prvmt = vmt; prvmt->is_diverged; prvmt = prvmt->parent)
			;
		prvmtclass = prvmt->class;
		for (j = 0; j < prvmtclass->members_arr.num; j++) {
			member = prvmtclass->members_arr.mem[j];
			if (member->vmt != prvmt)
				continue;

			thisclass = get_vmt_this_class(member);
			outprintf(parser, "\t%s(*%s%s)(struct %s *this%s%s;\n",
				member->rettypestr, member->implprefix, member->implname,
				thisclass->name, member->paramstext[0] != ')' ? ", " : "",
				member->paramstext);
			parser->pf.lines_coo++;
		}
		outprintf(parser, "} %s;\n", get_vmt_name(parser, vmt));
	}
}

static void print_coo_class_var(struct parser *parser, struct class *class)
{
	if (class->vmts.num == 0 || class->no_dyncast)
		return;

	switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
	outprintf(parser, "\nextern const struct %s_coo_class %s_coo_class;\n",
		class->name, class->name);
	parser->pf.lines_coo += 2;
}

static void get_vmt_path(struct parser *parser,
		struct vmt *vmt, char **ret_path, char **ret_access)
{
	struct ancestor *ancestor;
	struct vmt *parent;
	char *path;

	if (!vmt->path) {
		if (!vmt->is_diverged) {
			ancestor = hasho_find(&vmt->class->ancestors, vmt->origin);
			if (ancestor) {
				vmt->path = ancestor->path;
				vmt->access = ancestor->parent->is_virtual ? "->" : ".";
			} else {
				vmt->path = vmt->access = "";
			}
		} else {
			/* diverging only happens if there is a parent */
			parent = vmt->parent;
			get_vmt_path(parser, parent, &path, &vmt->access);
			vmt->path = psaprintf(parser, "%s%s%s", parent->class->name,
				vmt->is_parent_virtual ? "->" : ".", path);
		}
	}

	*ret_path = vmt->path;
	*ret_access = vmt->access;
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
			} else if (iswordstart(*p)) {
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

static void print_func_decl(struct parser *parser,
		struct class *class, struct member *member)
{
	struct class *thisclass;
	char *rootclass;
	unsigned empty;

	empty = member->paramstext[0] == ')';
	thisclass = get_impl_this_class(member);
	rootclass = member->is_root_constructor && class->rootclass ? "_root" : "";
	outprintf(parser, "\n%s%s_%s%s(struct %s%s *this%s%s;",
		member->rettypestr, class->name, member->implprefix, member->implname,
		thisclass->name, rootclass, empty ? "" : ", ", member->paramstext);
	parser->pf.lines_coo++;
}

static void print_member_decls(struct parser *parser, struct class *class)
{
	struct class* freer_class;
	struct ancestor *ancestor;
	struct member *member;
	char *params;
	unsigned i;

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

		/* optimize virtual function calls to actual calls for final classes */
		if (class->is_final && member->props.is_virtual) {
			member->props.is_virtual = 0;
			/* note that non-primary base virtual calls translate 'this',
			   so have to pass that base, so keep original parentname */
			if (!impl_this_needs_offset(member)) {
				if (member->definition != class) {
					ancestor = hasho_find(&class->ancestors,
							member->definition);
					if (ancestor && ancestor->parent->class
								!= member->origin) {
						member->parentname = ancestor->path;
					}
				} else {
					/* definition is here, no parent path necessary anymore */
					member->parentname = NULL;
				}
			}
		}

		/* don't print inherited members (that we did not override) */
		if (member->definition != class)
			continue;
		if (member->props.is_abstract)
			continue;
		if (!member->props.is_function && !member->props.is_static)
			continue;

		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		if (member->props.is_function) {
			print_func_decl(parser, class, member);
		} else {
			outprintf(parser, "\nextern %s%s_%s;",
				member->rettypestr, class->name, member->name);
			parser->pf.lines_coo++;
		}
	}
}

static char *parse_templpars(struct parser *parser, struct class *class, char *next)
{
	char *name, *nameend, *implstr, sep;
	struct anytype impltype;

	/* flush to skip template parameters in C output */
	flush_until(parser, next);
	for (next++;;) {
		name = skip_whitespace(parser, next);
		if (!iswordstart(*name)) {
			pr_err(name, "expected a name");
			break;
		}
		nameend = skip_word(name);
		next = skip_whitespace(parser, nameend);
		if (*next == ':') {
			implstr = skip_whitespace(parser, next+1);
			if (!iswordstart(*implstr)) {
				pr_err(name, "expected a type");
				goto nextpar;
			}
			next = parse_type(parser, class, implstr, &impltype, SKIP_IMPLICIT);
			if (impltype.type != AT_CLASS) {
				pr_err(implstr, "expected a class type");
				goto nextpar;
			}
			if (impltype.pointerlevel != 1) {
				pr_err(implstr, "expected single pointer type");
				goto nextpar;
			}
		}

		addtemplpar(parser, class, name, nameend, impltype.u.class);
	  nextpar:
		sep = *next++;
		if (sep == '>')
			break;
		if (sep != ',') {
			pr_err(name, "expected '>' or ','");
			break;
		}
	}
	/* skip template parameters in C output */
	parser->pf.writepos = next;
	return next;
}

static struct class *parse_struct(struct parser *parser, char *openbrace)
{
	struct class *class, *parentclass;
	struct memberprops memberprops = {0,};
	struct anytype decl;
	char *declbegin, *retend, *membername, *nameend, *params, *declend, *next;
	char *classname, *classnameend, *parentname, *prevdeclend, *prevnext;
	int level, parent_primary, parent_virtual, is_lit_var, len, prevdeclend_lineno;
	int first_virtual_warn, first_vmt_warn, empty_line, need_destructor;
	struct classtype *parentclasstype;
	struct parent *parent, *firstparent;
	struct anytype rettype;
	struct member *member;
	char namebuf[128];
	unsigned i, numexp;

	/* skip "struct " */
	classname = strskip_whitespace(parser->pf.pos + 7);
	next = classnameend = skip_word(classname);
	if (classnameend != classname) {
		class = addclass(parser, classname, classnameend);
		if (!class)
			return NULL;
		/* mimic C++, class names are also types */
		decl.type = AT_CLASS;
		decl.u.class = class;
		decl.pointerlevel = 0;
		decl.implicit = 1;
		addclasstype(parser, classname, classnameend, &decl);
		class->is_final = parser->saw_final;
		class->no_dyncast = parser->saw_nodyncast;
		/* check for template */
		if (*next == '<')
			next = parse_templpars(parser, class, next);
	} else
		class = NULL;

	need_destructor = 0;
	next = skip_whitespace(parser, next);
	if (class && *next == ':') {
		flush_until(parser, next);
		parent_primary = 1;
		parent_virtual = 0;
		first_virtual_warn = 0;
		first_vmt_warn = 0;
		firstparent = NULL;
		outwrite(parser, "{", 1);
		for (parentname = next;;) {
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
			if (parentclasstype->pointerlevel) {
				pr_err(parentname, "parent type cannot be pointer type");
				goto nextparent;
			}

			parent = calloc(1, sizeof(*parent));
			if (parent == NULL)
				break;

			parentclass = parentclasstype->class;
			parent->class = parentclass;
			parent->child = class;
			parent->is_primary = parent_primary;
			parent->is_virtual = parent_virtual;

			/* check template types to be mapped */
			next = skip_whitespace(parser, nameend);
			if (*next == '<')
				next = parse_templargs(parser, class,
					parentclass, next, &parent->templ_map);

			numexp = parentclass->templpars.num_entries;
			if (parent->templ_map.num != numexp)
				pr_err(nameend, "expected %u template argument%s for "
					"class %s, got %u", numexp, numexp == 1 ? "" : "s",
					parentclass->name, parent->templ_map.num);

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
			next = skip_whitespace(parser, next);
			if (*next == '{')
				break;
			if (*next != ',') {
				pr_err(next, "expected comma or brace after parent class");
				break;
			}
			parent_primary = parent_virtual = 0;
			parentname = next;
		}

		/* already printed '{', so start after '{' */
		parser->pf.writepos = next+1;
	}

	level = 1;  /* next is at opening brace '{' */
	declbegin = retend = NULL;
	membername = next = openbrace;  /* make compiler happy */
	for (;;) {
		/* search (sub)struct or end of variable or function prototype */
		prevnext = next + 1;
		if (declbegin == NULL)
			prevdeclend_lineno = parser->pf.lineno;
		next = skip_whitespace(parser, prevnext);
		if (declbegin == NULL) {
			prevdeclend = prevnext;
			declbegin = next;
		}
		/* remember last word before '(' or ';' => member name */
		if (iswordstart(*next) || *next == '*' || *next == '~')
			membername = next;
		if (!(next = scan_token(parser, next, "/{},:;( \r\n\t\v")))
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
		if (*next == ':') {
			if (strprefixcmp("public:", declbegin))
				memberprops.visibility = PUBLIC;
			else if (strprefixcmp("protected:", declbegin))
				memberprops.visibility = PROTECTED;
			else if (strprefixcmp("private:", declbegin))
				memberprops.visibility = PRIVATE;
			else
				pr_err(declbegin, "unrecognized visibility modifier");
			flush_skipto(parser, prevdeclend, prevdeclend_lineno, next + 1);
			declbegin = NULL;
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
				outputs(parser, "const struct coo_vmt *vmt;");
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
		if (memberprops.is_function || memberprops.is_static)
			flush_skipto(parser, prevdeclend, prevdeclend_lineno, next + 1);

		if (memberprops.is_override) {
			/* cannot add member, should have inherited already from parent */
			implmember(parser, membername, class, membername, nameend, memberprops);
			if (declbegin != membername)
				pr_err(membername, "membername must follow override");
			if (params)
				pr_err(params, "no parameters allowed for override");
		} else {
			/* for constructor, parse_type detects rettype wrong */
			if (retend > declbegin) {
				parse_type(parser, class, declbegin,
					&rettype, PRINT_IMPLICIT);
			} else {
				rettype.type = AT_UNKNOWN;
				rettype.pointerlevel = 0;
				rettype.implicit = 0;
			}
			is_lit_var = rettype.pointerlevel == 0 && to_class(&rettype);
			if (is_lit_var && rettype.u.class->num_abstract) {
				pr_err(membername, "cannot instantiate abstract "
					"class %s", rettype.u.class->name);
			} else {
				member = addmember(parser, class, &rettype,
					declbegin, retend, membername, nameend,
					params, memberprops);
				if (member && is_lit_var) {
					if (rettype.u.class->root_constructor)
						class->num_init_vars++;
					if (rettype.u.class->destructor)
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
		class->gen_destructor = '~';  /* &name[-1] == &gen_destructor */
		addgenmember(parser, class, NULL, &class->gen_destructor,
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
			if (parser->saw_typedef)
				addclasstype(parser, classname, next, &decl);
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
	print_coo_class_var(parser, class);
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

static void print_inserts_fromto(struct parser *parser, char *from, char *until)
{
	struct dynarr *inserts = parser->inserts;
	struct insert *insert;
	unsigned j;

	parser->pf.writepos = from;
	for (j = 0; j < inserts->num; j++) {
		insert = inserts->mem[j];
		if (from > insert->flush_until)
			continue;
		if (until < insert->flush_until)
			break;
		flush_until(parser, insert->flush_until);
		outputs(parser, insert->insert_text);
		parser->pf.writepos = insert->continue_at;
		/* make insert a no-op */
		insert->flush_until = insert->continue_at;
		if (insert->free_insert)
			free(insert->insert_text);
		insert->insert_text = "";
		insert->free_insert = 0;
	}
	parser->pf.pos = until;
	flush(parser);
}

static struct ancestor *accessancestor(struct anytype *expr, char *exprstart,
		char *name, struct class *target, char **pre, char **post)
	/* assumes expr->type == AT_CLASS */
{
	struct ancestor *ancestor;

	ancestor = hasho_find(&expr->u.class->ancestors, target);
	if (ancestor == NULL)
		return NULL;

	/* check if need to add parentheses to surround & ... -> */
	if (exprstart != NULL && exprstart != name) {
		*pre = !ancestor->parent->is_virtual ? "&(" : "(";
		*post = expr->pointerlevel ? ")->" : ").";
	} else {
		*pre = !ancestor->parent->is_virtual ? "&" : "";
		*post = expr->pointerlevel ? "->" : ".";
	}
	return ancestor;
}

static void parse_method(struct parser *parser, char *stmtstart, struct methodptr *targetmp,
		char *exprstart, char *exprend, struct anytype *expr, struct class *tgtclass,
		char *name, char *nameend, struct member *member)
	/* assumes expr->type == AT_CLASS */
{
	char *maybe_this, *newscope, *sep_or_end, *params, *vmtpath, *vmtaccess;
	char *pre, *post, *ancpath, funcsourcebuf[128], *funcsource;
	int scope_varnr, free_funcsource = 0;
	struct class *memberdef = member->definition;
	struct ancestor *ancestor;

	if (targetmp == NULL) {
		pr_err(exprstart, "did not detect target method pointer type");
		return;
	}
	if (expr->u.class != memberdef) {
		ancestor = accessancestor(expr, exprstart, name, memberdef, &pre, &post);
		if (ancestor == NULL)
			return;
		ancpath = ancestor->path;
	} else {
		pre = expr->pointerlevel == 0 ? "&" : "";
		post = ancpath = "";
	}
	if (!is_void_params(member->paramstext)) {
		sep_or_end = ", ";
		params = member->paramstext;
	} else {
		sep_or_end = params = "";
	}
	if (exprstart == NULL) {
		/* expr is implicit this, flush to start of name */
		exprstart = exprend = name;
		maybe_this = "this";
	} else {
		maybe_this = "";
	}
	newscope = open_tempscope(parser, &scope_varnr);
	if (tgtclass) {
		goto hardref;
	} else if (member->props.is_virtual) {
		get_vmt_path(parser, member->vmt, &vmtpath, &vmtaccess);
		funcsource = smaprintf(funcsourcebuf, sizeof(funcsourcebuf),
			"((struct %s_vmt*)coo_obj%d->%s%svmt)->%s", memberdef->name,
			scope_varnr, vmtpath, vmtaccess, member->name);
	} else  {
		tgtclass = memberdef;
	  hardref:
		funcsource = smaprintf(funcsourcebuf, sizeof(funcsourcebuf),
			"%s_%s", tgtclass->name, member->name);
	}
	free_funcsource = funcsource != funcsourcebuf;
	parser->pf.pos = stmtstart;
	flush(parser);
	outprintf(parser, "%sstruct %s *coo_obj%d = %s",
		newscope, memberdef->name, scope_varnr, pre);
	outwrite(parser, exprstart, exprend - exprstart);
	outprintf(parser, "%s%s%s; "
		"%s(*coo_fv%d)(struct %s *this%s%s = %s; "
		"%s coo_mv%d = { coo_obj%d, (%s(*)())coo_fv%d }; ",
		maybe_this, post, ancpath,
		member->rettypestr, scope_varnr, memberdef->name, sep_or_end, params, funcsource,
		targetmp->name, scope_varnr, scope_varnr, targetmp->rettypestr, scope_varnr);
	/* replace exprstart..(exprend..name)..nameend with newly defined coo_mv var */
	addinsert_format(parser, -1, exprstart, nameend, CONTINUE_AFTER,
		"coo_mv%u", scope_varnr);
	if (free_funcsource)
		free(funcsource);
}

static struct member *parse_member(struct parser *parser, char *stmtstart,
	struct methodptr *targetmp, int expr_is_oneword, char *exprstart, char *exprend,
	struct anytype *expr, char *name, char *nameend,
	struct anytype *retdecl, struct dynarr **retparams)
{
	struct member *member;
	struct class *class = to_class(expr);
	char *args, *flush_until, *insert_text, *continue_at;
	char *newscope, insert_buf[16], *vmtpath, *vmtaccess;
	enum insert_continue insert_continue;
	int add_this, has_arguments, scope_varnr, insert_index = -1;

	if (class == NULL)
		return NULL;
	if ((member = find_member_e(class, name, nameend)) == NULL)
		return NULL;

	if (exprstart && member->props.is_static) {
		pr_err(name, "cannot access static member %s::%s",
			member->origin->name, member->name);
		return NULL;
	}

	continue_at = NULL;
	insert_continue = CONTINUE_AFTER;
	add_this = !exprstart && !member->props.is_static;
	if (member->props.is_function) {
		args = strskip_whitespace(nameend);
		if (args[0] == ':')  /* base class::constructor */
			goto out;
		if (*args != '(') {
			parse_method(parser, stmtstart, targetmp, exprstart, exprend,
				expr, NULL, name, nameend, member);
			return NULL;
		}

		if (member->props.is_virtual) {
			if (!expr_is_oneword) {
				newscope = open_tempscope(parser, &scope_varnr);
				parser->pf.pos = stmtstart;
				flush(parser);
				outprintf(parser, "%sstruct %s *coo_obj%d = %s",
					newscope, class->name, scope_varnr,
					expr->pointerlevel == 0 ? "&" : "");
				print_inserts_fromto(parser, exprstart, exprend);
				sprintf(insert_buf, "coo_obj%d->", scope_varnr);
				insert_text = insert_buf;
				flush_until = exprstart;
				continue_at = name;
			} else if (exprstart) {
				/* user supplied expression, do not skip it */
				flush_until = exprstart;
				insert_text = "";
				continue_at = exprstart;
			} else {
				flush_until = name;
				insert_text = "this->";
				continue_at = name;
			}
			/* output vmt type cast */
			insert_index = addinsert_format(parser, insert_index,
				flush_until, continue_at, CONTINUE_BEFORE,
				"((struct %s_vmt*)%s",
				member->origin->name, insert_text);
			/* output expression + vmt path + name */
			get_vmt_path(parser, member->vmt, &vmtpath, &vmtaccess);
			insert_index = addinsert_format(parser, insert_index,
				name, name, CONTINUE_BEFORE,
				"%s%svmt)->", vmtpath, vmtaccess);
		} else {
			insert_index = addinsert(parser, insert_index, exprstart ?: name,
				get_class_funcinsert(parser, member->definition),
				name, CONTINUE_BEFORE);
		}

		continue_at = ++args;
		has_arguments = *strskip_whitespace(args) != ')';
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
					(!member->parentname && expr->pointerlevel == 0)
					? "&" : "", exprstart, CONTINUE_AFTER);
				/* continue at end */
				insert_index = parser->inserts->num;
				if (member->parentname) {
					insert_index = addinsert(parser, insert_index,
						exprend, expr->pointerlevel ? "->" : ".",
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
		insert_continue = CONTINUE_BEFORE;
		insert_text = get_member_nameinsert(parser, class, member);
		if (insert_text)
			insert_index = addinsert(parser, insert_index,
				name, "this->", name, CONTINUE_BEFORE);
		else
			insert_text = "this->";
	} else if ((insert_text = get_member_nameinsert(parser, class, member)) != NULL) {
		/* static variables need to skip expression/variable name, is a global */
		flush_until = member->props.is_static && exprstart ? exprstart : name;
		continue_at = name;
	}

	if (continue_at)
		addinsert(parser, insert_index,
			flush_until, insert_text, continue_at, insert_continue);

out:
	copy_anytype(retdecl, &member->rettype);
	*retparams = &member->params;
	return member;
}

static void check_visibility(struct parser *parser, struct class *thisclass,
		struct member *member, char *errpos)
{
	char *errmsg;

	switch (member->props.visibility) {
	case PRIVATE:
		if (member->origin == thisclass)
			return;
		errmsg = "cannot access private member %s::%s";
		break;
	case PROTECTED:
		/* thisclass can be NULL when parsing global/non-class functions */
		if (thisclass && hasho_find(&thisclass->ancestors, member->origin))
			return;
		errmsg = "cannot access protected member %s::%s";
		break;
	default:  /* public always OK */
		return;
	}
	pr_err(errpos, errmsg, member->visi_define->name, member->name);
}

static int check_ancestor_visibility(struct parser *parser, struct member *member, char *errpos)
{
	if (member->props.visibility == PRIVATE) {
		pr_err(errpos, "cannot access private member %s::%s",
			member->visi_define->name, member->name);
		return -1;
	}
	return 0;
}

static char *access_inherited(struct parser *parser, char *stmtstart,
		struct methodptr *targetmp, struct class *thisclass, struct class *tgtclass,
		char *accinhname, char *colons, char *name, char *next, struct dynarr **retparams)
{
	struct member *member;
	struct ancestor *ancestor;
	char *thistext, *thispos;
	struct anytype expr;
	int insert_index;

	member = find_member_e(thisclass, name, next);
	if (!member)
		return next;

	if (tgtclass != thisclass) {
		if (check_ancestor_visibility(parser, member, name) < 0)
			return next;
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
	if (*next != '(') {
		expr.u.class = thisclass;
		expr.pointerlevel = 1;
		/* pass accinhname as 'name', consider whole X::func to be
		   name..nameend so that the X:: part is also replaced */
		parse_method(parser, stmtstart, targetmp, NULL, NULL,
			&expr, tgtclass, accinhname, next, member);
		return next;
	}

	addinsert(parser, -1, colons, "_", colons+2, CONTINUE_AFTER);
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

static char *insertmpcall(struct parser *parser, int expr_is_oneword, char *stmtstart,
		char *exprstart, char *exprend, struct methodptr *targetmp,
		char *parenpos, int pointerlevel)
{
	char *arrow, *newscope, *sep_or_end, *next;
	int insert_index, scope_varnr;

	/* look ahead to see if there arguments, then need comma separator
	   between "this" parameter and rest of user arguments */
	next = skip_whitespace(parser, parenpos+1);
	sep_or_end = *next != ')' ? ", " : "";
	if (expr_is_oneword) {
		arrow = pointerlevel ? "->" : ".";
		insert_index = addinsert_format(parser, -1, parenpos, exprstart, CONTINUE_AFTER,
			"%sfunc(", arrow);
		addinsert_format(parser, insert_index, parenpos, parenpos+1, CONTINUE_AFTER,
			"%sobj%s", arrow, sep_or_end);
	} else {
		newscope = open_tempscope(parser, &scope_varnr);
		parser->pf.pos = stmtstart;
		flush(parser);
		outprintf(parser, "%s%s coo_mv%d = ", newscope, targetmp->name, scope_varnr);
		outwrite(parser, exprstart, exprend - exprstart);
		/* temp.var has been defined, now use it to replace expression */
		addinsert_format(parser, -1, exprstart, parenpos+1, CONTINUE_AFTER,
			"; coo_mv%d.func(coo_mv%d.obj%s", scope_varnr, scope_varnr, sep_or_end);
	}

	return next;
}

/*
   method pointer type:
   typedef class_X * (*::integer_cb)(int x);
      ===translate to===>
   typedef struct {
	   void *obj;
	   struct class_X *(*func)(void *this, int x);
   } integer_cb;
*/

static void parse_methodptr_typedef(struct parser *parser,
		struct anytype *rettype, char *rettypestr, char *next, char *declend)
{
	char *params, *nameend, *opentext, *name = next + 4;   /* after '(*::' */
	struct methodptr *mptype;

	nameend = skip_word(name);
	params = skip_whitespace(parser, nameend);
	if (params == NULL || *params != ')')
		return;
	params = skip_whitespace(parser, params + 1);
	if (*params != '(')
		return;

	/* if return type has implicit struct, then "struct " was already printed */
	if (rettype && rettype->implicit) {
		opentext = "{\n\tvoid *obj;\n\tstruct ";
	} else {
		flush(parser);
		opentext = "struct {\n\tvoid *obj;\n\t";
	}
	outputs(parser, opentext);
	flush_until(parser, next + 2);    /* include '(*' */
	outputs(parser, "func)(void *this");
	params = skip_whitespace(parser, params + 1);
	if (*params != ')' && !is_void_params(params))
		outwrite(parser, ", ", 2);

	/* also parses the params and inserts implicit struct where necessary
	   so make sure printing upto params is complete */
	parser->pf.writepos = params;
	mptype = addmethodptrtype(parser, rettype, rettypestr, next, name, nameend, params);
	if (mptype == NULL)
		return;

	flush_until(parser, declend);
	outputs(parser, ";\n} ");
	parser->pf.writepos = name;
	flush_until(parser, nameend);
	parser->pf.writepos = declend;
	parser->pf.lines_coo += 3;
}

static void parse_typedef(struct parser *parser, char *declend)
{
	struct anytype type;
	char *next, *typestr;

	/* check if a class is aliased to a new name */
	typestr = parser->pf.pos;
	next = parse_type(parser, NULL, typestr, &type, PRINT_IMPLICIT);
	if (strprefixcmp("(*::", next)) {
		parse_methodptr_typedef(parser, &type, typestr, next, declend);
		return;
	}

	/* TODO: how to handle templinst? */
	if (!to_class(&type) && !to_mp(&type))
		return;

	/* note: after next cannot be '{', as in 'typedef struct X {'
	   it is handled by calling parse_struct in caller */
	parser->pf.pos = next;
	next = skip_word(parser->pf.pos);
	if (to_class(&type))
		addclasstype(parser, parser->pf.pos, next, &type);
	else  /* mptype, TODO: do something with type.decl.pointerlevel! */
		dupmethodptrtype(parser, parser->pf.pos, next, to_mp(&type));
	parser->pf.pos = declend + 1;
}

static void print_inserts(struct parser *parser, struct dynarr *inserts)
{
	struct insert *insert;
	unsigned j;

	for (j = 0; j < inserts->num; j++) {
		insert = inserts->mem[j];
		flush_until(parser, insert->flush_until);
		outputs(parser, insert->insert_text);
		parser->pf.writepos = insert->continue_at;
		if (insert->free_insert)
			free(insert->insert_text);
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
		disposer = NULL;  /* make compiler happy */
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
		struct class *class, struct anytype *decl, char *name, char *next)
{	(void)position;
	addvariable(parser, class, 0, decl, 0, name, next, NULL);
}

static void check_conflict_param_member(struct parser *parser,
		char *name, char *nameend)
{
	struct member *member;
	char *inheritmsg, *inherittext;

	if (!parser->class)
		return;

	member = find_member_e(parser->class, name, nameend);
	if (!member)
		return;

	if (member->origin != parser->class) {
		inheritmsg = " inherited from ";
		inherittext = member->origin->name;
	} else
		inheritmsg = inherittext = "";
	pr_warn(name, "class %s also has member named '%s'%s%s; change "
		"parameter name to avoid confusion", parser->class->name,
		member->name, inheritmsg, inherittext);
}

static void insertancestor(struct parser *parser,
		char *exprstart, char *name, char *end,
		struct class *target, struct anytype *expr)
	/* assumes expr->type == AT_CLASS */
{
	struct ancestor *ancestor;
	int insert_index = -1;
	char *pre, *post;

	ancestor = accessancestor(expr, exprstart, name, target, &pre, &post);
	if (ancestor == NULL)
		return;

	if (pre[0])
		insert_index = addinsert(parser, insert_index,
			exprstart, pre, exprstart, CONTINUE_BEFORE);
	insert_index = addinsert(parser, insert_index, end, post, end, CONTINUE_BEFORE);
	addinsert(parser, insert_index, end, ancestor->path, end, CONTINUE_BEFORE);
}

struct paramstate {
	struct dynarr *params;
	unsigned index;
};

static void select_next_param(struct paramstate *state, struct anytype *dest)
{
	dest->type = AT_UNKNOWN;
	state->index++;
	if (state->params && state->index < state->params->num) {
		struct anytype *source = state->params->mem[state->index];
		if (source)
			copy_anytype(dest, source);
	}
}

/* state for detecting function variable: (*name)(.... */
enum parse_funcvar_state { FV_NONE, FV_NAME, FV_PARENCLOSE };
enum goto_return { GOTO_RET_NONE, GOTO_RET, GOTO_RET_BLOCK };
enum dyncast_state { DYNCAST_NONE, DYNCAST_EXPR, DYNCAST_PAREN };

static int is_expr(enum parse_state state)
{
	 return state == STMTSTART || state == FINDVAR || state == EXPRUNARY;
}

static int is_dyncast_expr_sep(char *curr)
{
	/* all higher precedence operators do not separate dyncast expressions */
	if (curr[0] == '+' && curr[1] == '+')
		return 0;
	if (curr[0] == '(' || curr[0] == '[' || curr[0] == '.')
		return 0;
	if (curr[0] == '-' && curr[1] == '>')
		return 0;
	return 1;
}

static void parse_function(struct parser *parser, char *next)
{
	struct anytype exprdecl[MAX_PAREN_LEVELS], targetdecl[MAX_PAREN_LEVELS];
	struct anytype decl, immdecl, *expr, *target, rettype, thisptr;
	struct paramstate targetparams[MAX_PAREN_LEVELS];
	struct dynarr **tgtparams;
	struct templpar *tptype;
	struct classtype *classtype;
	struct member *member, *submember, *constr;
	struct variable *declvar;
	struct initializer *initializer;
	struct methodptr *mptype, *mp;
	char *curr, *funcname, *nameend, *classname, *name, *str1, *str2, *tmplpos;
	char *exprstart[MAX_PAREN_LEVELS], *exprend, *params, *param0, *paramend;
	char *memberstart[MAX_PAREN_LEVELS], *funcvarname, *funcvarnameend, *stmtstart;
	char *thispath, *thisprefix, *thisclassname, *thisfuncret, *thisfuncretend;
	char *argsep, *dblcolonsep, *accinhname, *accinhcolons;
	enum dyncast_state dyncast[MAX_PAREN_LEVELS];
	enum parse_funcvar_state funcvarstate;
	enum parse_state state, nextstate;
	enum goto_return goto_ret;
	int is_constructor, expr_is_oneword, blocklevel, parenlevel, seqparen, numwords;
	int have_retvar, need_retvar, used_retvar, void_retvar;
	int retblocknr, next_retblocknr, in_delete, insert_index;
	unsigned i, num_constr_called, num_constr, ptrlvl;

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
	thispath = NULL;
	thisptr.type = AT_UNKNOWN;
	thisptr.u.class = NULL;
	if (funcname) {   /* funcname assigned means there is a classname::funcname */
		if ((thisptr.u.class = find_class(parser, classname)) != NULL) {
			/* lookup method name */
			thisptr.type = AT_CLASS;
			thisptr.pointerlevel = 1;
			nameend = skip_methodname(funcname);
			member = find_member_e(thisptr.u.class, funcname, nameend);
			if (member != NULL) {
				if (member->props.is_virtual
						&& member->definition != thisptr.u.class
						&& !thisptr.u.class->is_final) {
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
				parse_type(parser, thisptr.u.class, thisfuncret,
					&rettype, PRINT_IMPLICIT);
				props.is_function = 1;
				member = addmember(parser, thisptr.u.class, &rettype,
					thisfuncret, classname, funcname, nameend, params, props);
			}
			thisptr.u.class->is_implemented = 1;
			thisptr.u.class->gen_constructor &= ~member->is_constructor;
			thisptr.u.class->gen_root_constructor &= ~member->is_root_constructor;
			thisptr.u.class->gen_destructor &= ~member->is_destructor;
			/* if this is a constructor and no return type, default to void
			   no return type allowed for destructor in COO, add it now for C */
			if (((member->is_constructor || member->is_root_constructor)
						&& (thisfuncret == classname
							|| strprefixcmp("void ", thisfuncret)))
					|| member->is_destructor) {
				flush(parser);
				outputs(parser, member->rettypestr);
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
				get_vmt_path(parser, member->vmt, &thispath, &str1);
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
			decl.type = AT_CLASS;
			decl.u.class = thisptr.u.class;
			decl.pointerlevel = 1;
			name = "this";
			nameend = name + 4;  /* "this" */
			addvariable(parser, thisptr.u.class, 0, &decl, 0, name, nameend, NULL);
			/* no parents means all are initialized */
			is_constructor = member->is_constructor;
			num_constr = is_constructor
				* (thisptr.u.class->num_parent_constr + thisptr.u.class->num_init_vars);
		} else {
			pr_err(classname, "class '%s' not declared", classname);
		}
	}

	parser->class = thisptr.u.class;
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
	if (parse_parameters(parser, thisptr.u.class, params, NULL, param_to_variable,
			check_conflict_param_member) == NULL)
		return;

	if (thispath) {
		parser->pf.pos = next;
		flush(parser);
		outprintf(parser, "\tstruct %s *this = container_of(__this, struct %s, %s);",
			classname, classname, thispath);
	}

	seqparen = parenlevel = exprdecl[0].pointerlevel = numwords = 0;
	exprdecl[0].type = targetdecl[0].type = decl.type = immdecl.type = AT_UNKNOWN;
	have_retvar = need_retvar = used_retvar = void_retvar = 0;
	stmtstart = exprstart[0] = memberstart[0] = exprend = accinhcolons = NULL;
	funcvarname = funcvarnameend = name = NULL;  /* make compiler happy */
	retblocknr = next_retblocknr = in_delete = 0;
	parser->initializers.num = 0;
	targetparams[0].params = NULL;
	memset(&dyncast, 0, sizeof(dyncast));
	funcvarstate = FV_NONE;
	goto_ret = GOTO_RET_NONE;
	state = STMTSTART;
	initializer = NULL;
	declvar = NULL;
	member = NULL;
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
		if (!stmtstart)
			stmtstart = curr;
		if (!exprstart[parenlevel]) {
			exprstart[parenlevel] = curr;
			expr_is_oneword = 1;
		}
		if (isdigit(*curr))
			curr = skip_word(curr);
		else if (iswordstart(*curr)) {
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
				} else if ((decl.u.class =
						find_class_e(parser, name, next)) != NULL) {
					decl.pointerlevel = 0;
					decl.type = AT_CLASS;
					goto decl_or_cast;
				}
				continue;
			} else if (strprefixcmp("dyn", curr)) {
				if (strprefixcmp("amic_cast<", curr+3)) {
					if (strprefixcmp(">(", curr+13)) {
						next = curr + 13;
						goto dyncast;
					}
					next = parse_type(parser, thisptr.u.class, curr+13,
						&rettype, SKIP_IMPLICIT);
					if (next[0] != '>' || next[1] != '(')
						pr_err(next, "'>' expected");
					next++;
					goto dyncast;
				} else if (curr[3] == ':') {
					next = curr + 4;
					copy_anytype(&rettype, &targetdecl[parenlevel]);
				   dyncast:
					if (rettype.type != AT_CLASS) {
						pr_err(curr, "unknown class to cast to");
						continue;
					}
					if (rettype.pointerlevel != 1) {
						pr_err(curr, "dynamic cast must be "
							"to single pointer");
						continue;
					}
					if (rettype.u.class->no_dyncast) {
						pr_err(curr, "target class '%s' is not dynamic "
							"castable", rettype.u.class->name);
						continue;
					}
					/* skip "dyn:" or "dynamic_cast<>(" */
					insert_index = addinsert(parser, -1, curr,
						"coo_dyn_cast(&", next, CONTINUE_AFTER);
					insert_index = addinsert(parser, insert_index, next,
						rettype.u.class->name, next, CONTINUE_AFTER);
					addinsert(parser, insert_index, next,
						"_coo_class, &", next, CONTINUE_AFTER);
					if (*next == '(') {
						copy_anytype(&targetdecl[parenlevel+1], &rettype);
						dyncast[parenlevel+1] = DYNCAST_PAREN;
					} else {
						dyncast[parenlevel] = DYNCAST_EXPR;
					}
					continue;
				}
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
					if (!classtype->pointerlevel) {
						immdecl.type = AT_CLASS;
						immdecl.u.class = classtype->class;
						immdecl.pointerlevel = 1;
						if (immdecl.u.class->constructor) {
							addinsert(parser, -1, curr,
								"new_", name, CONTINUE_AFTER);
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

			if (state == DECLVAR && to_class(&decl) && parenlevel == 0) {
				/* for 2nd variable decl.class is the rootclass */
				if (declvar && decl.u.class->is_rootclass
						&& (declvar->decl.pointerlevel
							|| exprdecl[0].pointerlevel)) {
					pr_err(curr, "cannot combine pointer and non-pointer "
						"declarations of root-requiring classes");
				} else if (!declvar && decl.u.class->rootclass
						&& exprdecl[0].pointerlevel == 0) {
					/* declaring stack variable with root class, add _root */
					flush_until(parser, next);
					outwrite(parser, "_root", 5);
					parser->pf.writepos = next;
					/* fix class type, so ancestor paths are correct */
					decl.u.class = &decl.u.class->rootclass->class;
				}
			}
			name = curr;
			next = skip_word(name);
			numwords++;
			nextstate = FINDVAR;
			switch (state) {
			case DECLVAR:
				declvar = addclsvariable(parser, blocklevel, &decl,
						exprdecl[0].pointerlevel, name, next);
				copy_anytype(&immdecl, &decl);
				if (exprdecl[0].pointerlevel == 0 && decl.pointerlevel == 0
						&& to_class(&decl)) {
					if (decl.u.class->num_abstract) {
						pr_err(name, "cannot instantiate "
							"abstract class %s", decl.u.class->name);
					} else if (decl.u.class->root_constructor) {
						nextstate = CONSTRUCT;
						if (!need_retvar && !void_retvar &&
								decl.u.class->destructor) {
							void_retvar = is_void_rettype(thisfuncret);
							need_retvar = !void_retvar;
						}
						initializer = addinitializer(parser,
							decl.u.class, declvar->name, next);
					}
				}
				break;
			case DECLMETHODVAR:
				addmpvariable(parser, blocklevel, &decl,
					exprdecl[0].pointerlevel, name, next);
				break;
			case ACCESSMEMBER:  /* parsing expr.member or expr->member */
				/* immdecl is used for same parenthesis level,
				   exprdecl in case of cast (nested in parentheses) */
				expr = to_class(&immdecl) ? &immdecl : &exprdecl[parenlevel];
				if (expr->pointerlevel <= 1) {
					submember = parse_member(parser, stmtstart,
						to_mp(&targetdecl[parenlevel]), expr_is_oneword,
						memberstart[parenlevel], exprend, expr,
						name, next, expr, &targetparams[parenlevel].params);
					if (submember == NULL)
						break;
					/* to check all literal class variables initialized */
					if (is_constructor && submember->is_root_constructor
						&& member && member->rettype.pointerlevel == 0) {
						if (!member->constructor_called) {
							num_constr_called++;
							member->constructor_called = 1;
						} else {
							pr_err(memberstart[parenlevel],
								"duplicate call to member %s "
								"root constructor", member->name);
						}
					}
					check_visibility(parser, thisptr.u.class, submember,
						memberstart[parenlevel]);
				}
				break;
			case ACCESSINHERITED:  /* parsing class::member */
				next = access_inherited(parser, stmtstart,
					to_mp(&targetdecl[parenlevel]), thisptr.u.class,
					to_class(&immdecl), accinhname, accinhcolons,
					name, next, &targetparams[parenlevel].params);
				break;
			default:
				/* maybe it's a local (stack) variable? */
				tgtparams = &targetparams[parenlevel].params;
				if (find_local_e_class(parser, name, next,
						&immdecl, tgtparams) >= 0)
					break;
				/* maybe it's a member field? "this" has pointerlevel 1 */
				member = parse_member(parser, stmtstart,
					to_mp(&targetdecl[parenlevel]), 0, NULL, NULL,
					&thisptr, name, next, &immdecl, tgtparams);
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
					if (member->origin != thisptr.u.class)
						check_ancestor_visibility(parser, member, name);
					break;
				}
				/* maybe it's a global variable? */
				if (find_global_e_class(parser,
						name, next, &immdecl, tgtparams) >= 0)
					break;
				/* maybe it's a type, to declare variable, or a cast */
				classtype = find_classtype_e(parser, name, next);
				if (classtype != NULL) {
					decl.type = AT_CLASS;
					decl.u.class = classtype->class;
					decl.pointerlevel = classtype->pointerlevel;
					decl.implicit = classtype->implicit;
					if (*next == '<') {
						tmplpos = next;
						decl.type = AT_TEMPLINST;
						next = parse_templargs(parser, thisptr.u.class,
							classtype->class, next, &decl.args);
					}
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
					if (decl.type == AT_TEMPLINST) {
						/* don't print template arguments */
						if (state == STMTSTART) {
							flush_until(parser, tmplpos);
							parser->pf.writepos = next;
						} else {
							addinsert(parser, -1,
								tmplpos, "", next, CONTINUE_AFTER);
						}
					}
				  decl_or_cast:
					if (state == STMTSTART) {
						nextstate = DECLVAR;
					} else if (seqparen >= 2 && exprdecl[parenlevel-2]
							.type == AT_UNKNOWN) {
						/* this is a cast, like ((class_t*)x)->..
						   remember first detected class */
						ptrlvl = exprdecl[parenlevel-2].pointerlevel;
						copy_anytype(&exprdecl[parenlevel-2], &decl);
						/* combine possible pointer dereference */
						exprdecl[parenlevel-2].pointerlevel += ptrlvl;
						decl.type = AT_UNKNOWN;
						nextstate = CASTVAR;
					} else if (*next != ':' || next[1] != ':') {
						/* grammar error, ignore */
						decl.type = AT_UNKNOWN;
					}
					break;
				}
				mptype = find_methodptr_e(parser, name, next);
				if (mptype != NULL) {
					if (state == STMTSTART) {
						expr = &decl;
						nextstate = DECLMETHODVAR;
						goto init_mp_decl;
					} else if (seqparen >= 2 && exprdecl[parenlevel-2]
							.type == AT_UNKNOWN) {
						/* cast to methodptr */
						expr = &exprdecl[parenlevel-2];
						nextstate = CASTVAR;
					  init_mp_decl:
						expr->type = AT_METHOD;
						expr->u.mp = mptype;
						expr->pointerlevel = 0;
					}
					break;
				}
				if (thisptr.u.class) {
					tptype = find_templpar_e(thisptr.u.class, name, next);
					if (tptype != NULL) {
						if (state == STMTSTART) {
							next = replace_templpar(parser,
									tptype, name, next);
							expr = &decl;
							nextstate = DECLVAR;
							goto init_tp_decl;
						} else if (seqparen >= 2 && exprdecl[parenlevel-2]
								.type == AT_UNKNOWN) {
							/* cast to template parameter type
							in expression, not safe to replace here */
							addinsert_templpar(parser,
									tptype, name, next);
							expr = &exprdecl[parenlevel-2];
							nextstate = CASTVAR;
						init_tp_decl:
							expr->type = AT_TEMPLPAR;
							expr->u.tp = tptype;
							expr->pointerlevel = 0;
						}
						break;
					}
				}
				if (parenlevel == 1 && exprdecl[1].pointerlevel == -1) {
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
				if (*next == ')') {
					addvariable(parser, NULL, blocklevel, &decl,
						exprdecl[0].pointerlevel, funcvarname,
						funcvarnameend, curr+1);
				}
			}
		}

		if (state == CONSTRUCT) {
			expr = to_class(&immdecl) ? &immdecl : &decl;
			if (expr->u.class->missing_root) {
				pr_err(curr, "must define root constructor for %s "
					"due to non-void constructor %s",
					expr->u.class->name, expr->u.class->missing_root->name);
			}
			constr = expr->u.class->root_constructor;
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
				if (expr->pointerlevel == 0
						&& !expr->u.class->void_root_constructor)
					pr_warn(curr, "non-void root constructor may fail");
			}
			if (*curr != '(' && expr->pointerlevel) {
				/* allocation with 'new class', needs function call */
				addinsert(parser, -1, curr, "()", curr, CONTINUE_AFTER);
			}
			state = FINDVAR;
		}

		if (*curr == '(') {
			mp = to_mp(&immdecl) ?: to_mp(&exprdecl[parenlevel]);
			if (mp && exprdecl[parenlevel].pointerlevel < 2) {
				next = insertmpcall(parser, expr_is_oneword, stmtstart,
					exprstart[parenlevel], exprend, mp,
					curr, exprdecl[parenlevel].pointerlevel);
			}
		}

		if (*curr != '.' && (*curr != '-' || curr[1] != '>'))
			memberstart[parenlevel] = NULL;
		if (parser->initializers.num && parenlevel == 0 && *curr == '=') {
			/* when added one initializer, then copy them all,
			   to keep order the order the same */
			initializer = addinitializer(parser, NULL, name, next);
		} else if (*curr == ')' || *curr == ',' || *curr == '=' || *curr == ';') {
			/* parsing membername in context (function argument, assignment)
			   where pointer to a membername's ancestor class is expected
			   determine target and source classes to access ancestor of */
			immdecl.pointerlevel += exprdecl[parenlevel].pointerlevel;
			if (to_class(&exprdecl[parenlevel]))
				expr = &exprdecl[parenlevel];
			else
				expr = &immdecl;
			target = &targetdecl[parenlevel];
			if (to_class(target) && to_class(expr) && target->u.class != expr->u.class
					&& target->pointerlevel == expr->pointerlevel
					&& target->pointerlevel <= 1)
				insertancestor(parser, exprstart[parenlevel],
						name, curr, target->u.class, expr);
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
		if (dyncast[parenlevel] == DYNCAST_EXPR && is_dyncast_expr_sep(curr)) {
			/* close coo_dyn_cast call */
			addinsert(parser, -1, curr, "->vmt)", curr, CONTINUE_AFTER);
			dyncast[parenlevel] = DYNCAST_NONE;
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
				exprdecl[parenlevel].type = AT_UNKNOWN;
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
				if (dyncast[parenlevel]) {
					addinsert(parser, -1,
						curr, ")->vmt", curr, CONTINUE_AFTER);
					dyncast[parenlevel] = DYNCAST_NONE;
				}
				parenlevel--;
				if (exprdecl[parenlevel].type == AT_UNKNOWN &&
						exprdecl[parenlevel+1].type != AT_UNKNOWN)
					copy_anytype(&exprdecl[parenlevel],
						&exprdecl[parenlevel+1]);
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
			if (to_class(&decl) && parenlevel == 0)
				state = DECLVAR;
			else if (to_mp(&decl) && parenlevel == 0)
				state = DECLMETHODVAR;
			else
				state = EXPRUNARY;
			if (parenlevel) {
				select_next_param(&targetparams[parenlevel-1],
					&targetdecl[parenlevel]);
			}
			exprdecl[parenlevel].pointerlevel = 0;
		} else if (*curr == '=') {
			copy_anytype(&targetdecl[parenlevel], &immdecl);
			exprdecl[parenlevel].pointerlevel = 0;
			immdecl.type = AT_UNKNOWN;
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
				immdecl.type = AT_CLASS;
				immdecl.u.class = member->definition;
				immdecl.pointerlevel = 1;
				goto accessinherited;
			} else if (to_class(&decl)) {
				immdecl = decl;
				decl.type = AT_UNKNOWN;
			  accessinherited:
				/* convert "class::func" to "class_func" */
				next = curr + 2;
				accinhname = name;
				accinhcolons = curr;
				state = ACCESSINHERITED;
				numwords--;    /* for declaration detection, merge x->y words */
			} else {
				pr_err(curr, "unexpected '::' encountered");
			}
		} else if (*curr == ';') {
			if (in_delete) {
				expr = to_class(&immdecl) ? &immdecl : &exprdecl[0];
				if (to_class(expr)) {
					if (expr->u.class->destructor) {
						outprintf(parser, "free_%s(",
							expr->u.class->name);
					} else {
						outputs(parser, "free(");
					}
				} else {
					pr_err(curr, "unknown variable to delete");
				}
			}
			print_inserts(parser, parser->inserts);
			if (in_delete) {
				parser->pf.pos = curr;
				flush(parser);
				outwrite(parser, ")", 1);
				in_delete = 0;
			}
			close_tempscope(parser, curr + 1);
			targetdecl[0].pointerlevel = exprdecl[0].pointerlevel = 0;
			targetdecl[0].type = exprdecl[0].type = AT_UNKNOWN;
			immdecl.type = decl.type = AT_UNKNOWN;
			stmtstart = NULL;
			declvar = NULL;
			member = NULL;
			numwords = 0;
			seqparen = 0;
			parenlevel = 0;
			state = STMTSTART;
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
				if (immdecl.type != AT_UNKNOWN) {
					copy_anytype(&exprdecl[parenlevel], &immdecl);
					immdecl.type = AT_UNKNOWN;
				}
			}
			member = NULL;
		} else
			state = FINDVAR;

		if (exprend == NULL && !isalnum(*curr))
			expr_is_oneword = 0;

		/* advance for most of the operator/separator cases */
		if (next <= curr)
			next = curr+1;
	}

	if (num_constr_called < num_constr) {
		for (i = 0; i < thisptr.u.class->members_arr.num; i++) {
			member = thisptr.u.class->members_arr.mem[i];
			if (member->constructor_called)
				continue;
			if (member->parent_constructor) {
				pr_err(curr, "missing parent constructor "
					"%s call", member->name);
			} else if (member_needs_init(thisptr.u.class, member)) {
				pr_err(curr, "missing member root constructor "
					"call %s.%s()", member->name,
					member->rettype.u.class->root_constructor->name);
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
	char *name, *rootprefix, *thisname, *rootsep;
	struct ancestor *ancestor, *literal_ancestor;
	struct hasho_entry *entry;
	struct class *parentclass, *rootclass;
	struct vmt *vmt;
	unsigned i;

	if (!class->gen_root_constructor)
		return;

	if (class->rootclass) {
		rootclass = &class->rootclass->class;
		rootprefix = class->name;
		rootsep = ".";
	} else {
		rootclass = class;
		rootprefix = rootsep = "";
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
					rootprefix, rootsep, ancestor->path);
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
	for (i = 0; i < rootclass->vmts.num; i++) {
		vmt = rootclass->vmts.mem[i];
		get_vmt_path(parser, vmt, &vmtpath, &vmtaccess);
		outprintf(parser, "\tthis->%s%svmt = &%s.vmt_base;\n",
			vmtpath, vmtaccess, get_vmt_name(parser, vmt));
	}
	/* construct classes that are used as virtual bases only */
	print_construct_parents(parser, class, rootprefix, rootsep, VIRTUAL_PARENT);
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
		print_construct_parents(parser, class, rootprefix, rootsep, LITERAL_PARENT);
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
		memberclass = member->rettype.u.class;
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
		memberclass = member->rettype.u.class;
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

static void print_trampolines(struct parser *parser, struct class *class, struct vmt *vmt)
{
	struct member *member;
	char *vmtpath, *vmtaccess, *funcmiddle, *structsuffix, *thisaccess, *thismember;
	struct class *prclass, *implclass, *vmtclass;
	struct vmt *prvmt;
	unsigned i;

	get_vmt_path(parser, vmt, &vmtpath, &vmtaccess);
	for (prvmt = vmt; prvmt->is_diverged; prvmt = prvmt->parent)
		;
	prclass = prvmt->class;
	if (vmt->is_diverged) {
		funcmiddle = prclass->name;
		structsuffix = "";
		thisaccess = "this";
		thismember = "";
	} else {
		funcmiddle = "root";
		structsuffix = "_root";
		thisaccess = "&this->";
		thismember = class->name;
	}
	for (i = 0; i < prclass->members_arr.num; i++) {
		member = prclass->members_arr.mem[i];
		if (member->vmt != prvmt)
			continue;

		/* need to print our members, not parent's, find it */
		vmtclass = get_vmt_this_class(member);
		if (vmt->is_diverged) {
			if ((member = find_eqv_member(parser, class, member)) == NULL)
				continue;
		}

		implclass = get_impl_this_class(member);
		if (implclass == member->origin) {
			/* may happen in case this member is inherited from a
			   diverged vmt, but is not ambiguous (not present in
			   origin): then the implementation already performs
			   the offset calculation and we don't need a trampoline */
			continue;
		}
		outprintf(parser, "\nstatic %s%s_%s_%s%s(struct %s *__this, %s\n"
			"{\tstruct %s%s *this = container_of("
				"__this, struct %s%s, %s);\n"
			"\t%s%s_%s%s(%s%s",
			member->rettypestr, class->name, funcmiddle,
			  member->implprefix, member->implname,
			  vmtclass->name, member->paramstext,
			implclass->name, structsuffix, implclass->name,
			  structsuffix, vmtpath,
			is_void_rettype(member->rettypestr) ? "" : "return ",
			  member->definition->name, member->implprefix,
			  member->implname, thisaccess, thismember);
		print_param_names(parser, member->paramstext);
		outprintf(parser, ");\n}\n");
		/* no need to count lines_coo here, end of input */
	}
}

static void print_coo_class(struct parser *parser, struct class *class)
{
	struct parent *parent;
	unsigned num_parents, first;
	struct class *rootclass;

	rootclass = class->rootclass ? &class->rootclass->class : class;
	num_parents = class->num_dync_parents;
	if (num_parents && class->need_dync_p0_dummy)
		num_parents++;
	switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
	/* disable packing so 'void* parents[]' is aligned */
	if (!parser->coo_includes_pr) {
		outputs(parser, "\n"
			"#include <stddef.h>\n"
			"#include <stdint.h>\n"
			"#pragma pack(8)");
		parser->coo_includes_pr = 1;
	}
	outprintf(parser, "\nconst struct %s_coo_class {\n"
		"\tuint32_t num_parents;\n", class->name);
	/* offsets are between parents, assmption is that first parent is located
	   at offset zero; if this does not hold, then need_dummy is set */
	if (num_parents > 1)
		outprintf(parser, "\tuint32_t offsets[%u];\n", num_parents - 1);
	if (num_parents > 0)
		outprintf(parser, "\tconst void *parents[%u];\n", num_parents);
	outprintf(parser, "} %s_coo_class = {\n"
		"\t%u,\n", class->name, num_parents);
	if (num_parents > 0) {
		parent = class->dync_parents;
		/* dummy means first parent is not at affset 0 =>
		   no dummy means first parent is at offset 0, then skip first parent */
		if (!class->need_dync_p0_dummy && parent)
			parent = parent->next_dync;
		for (first = 1; parent; parent = parent->next_dync, first = 0)
			outprintf(parser, "%s offsetof(struct %s, %s)",
				first ? "\t{" : ",\n\t ", rootclass->name, parent->class->name);
		if (!first)
			outputs(parser, " },\n");
		if (class->need_dync_p0_dummy)
			outputs(parser, "\t{ NULL");
		first = !class->need_dync_p0_dummy;
		for (parent = class->dync_parents; parent; parent = parent->next_dync) {
			outprintf(parser, "%s &%s_coo_class",
				first ? "\t{" : ",\n\t ", parent->class->name);
			first = 0;
		}
		outputs(parser, " }\n");
	}
	outputs(parser, "};\n");
	/* no need to count lines_coo here, end of input */
}

static void print_vmt(struct parser *parser, struct class *class, struct vmt *vmt)
{
	struct member *member;
	const char *vmt_name;
	char *classsep, *classsuffix, *vmtpre, *vmtpreacc, *vmtaccess, *vmtpath;
	struct ancestor *ancestor;
	struct class *rootclass, *prclass;
	struct vmt *prvmt;
	unsigned i;

	rootclass = class->rootclass ? &class->rootclass->class : class;
	vmt_name = get_vmt_name(parser, vmt);
	vmtpre = vmtpreacc = "";
	if (vmt->from_virtual) {
		/* vmt from a virtual base, rootclass will have a literal
		   (but not necessarily toplevel) */
		ancestor = hasho_find(&rootclass->ancestors, vmt->origin);
		vmtpath = ancestor->path;
		vmtaccess = ".";
	} else {
		get_vmt_path(parser, vmt, &vmtpath, &vmtaccess);
		if (class->rootclass) {
			/* vmt path is path in class, but need relative to rootclass */
			vmtpre = class->name;
			vmtpreacc = ".";
		}
	}
	classsep = "";
	classsuffix = vmt->from_virtual ? "_root" : "";
	outprintf(parser, "\nconst struct %s %s = {\n"
		"\t{ offsetof(struct %s, %s%s%s%svmt),\n"
		"\t  &%s_coo_class },\n", vmt_name, vmt_name,
		rootclass->name, vmtpre, vmtpreacc, vmtpath, vmtaccess, class->name);
	/* in case of diverging, the member->vmt might point to
	   different vmt; have to print parent class' vmt */
	for (prvmt = vmt; prvmt->is_diverged; prvmt = prvmt->parent)
		;
	prclass = prvmt->class;
	for (i = 0; i < prclass->members_arr.num; i++) {
		member = prclass->members_arr.mem[i];
		if (member->vmt != prvmt)
			continue;
		/* need to print our members, not parent's, find it */
		if (vmt->is_diverged) {
			if ((member = find_eqv_member(parser, class, member)) == NULL)
				continue;
			if (get_impl_this_class(member) == member->origin)
				classsep = classsuffix = "";
			else
				classsep = "_", classsuffix = prclass->name;
		}

		outprintf(parser, "\t%s%s%s_%s%s,\n",
			member->definition->name, classsep, classsuffix,
			member->implprefix, member->implname);
	}
	outprintf(parser, "};\n");
	/* no need to count lines_coo here, end of input */
}

static void print_class_impl(struct parser *parser)
{
	struct class *class;
	struct vmt *vmt;
	unsigned i;

	hash_foreach(class, &parser->classes) {
		if (!class->is_implemented)
			continue;

		if (parser->pf.writepos != parser->pf.pos) {
			flush(parser);
			switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		}

		/* print class def also for abstract classes */
		print_coo_class(parser, class);

		/* but no need to print vmts for abstract classes */
		if (class->num_abstract)
			continue;

		for (i = 0; i < class->vmts.num; i++) {
			vmt = class->vmts.mem[i];
			if (vmt->modified != vmt)
				continue;

			/* print trampoline functions for virtual functions
			   inherited from virtual base classes, where implementation
			   cannot see literal base therefore cannot translate 'this' */
			/* note that this implies a root class for this class: if the
			   base is present as a literal base, no need for root class */
			if (vmt->from_virtual || vmt->is_diverged)
				print_trampolines(parser, class, vmt);

			/* print vmt itself */
			print_vmt(parser, class, vmt);
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
	parser->pf.coo_rtl_included = 0;
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
		char *tempname = stredupto(NULL, parser->pf.pos, nameend);
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

static void parse_global(struct parser *parser, char *next)
{
	struct anytype vartype;
	struct variable *variable;
	char *end, *name, coo_class_var;

	/* special declaration to trigger coo class variable to be emitted */
	coo_class_var = memcmp("::coo_class", next - 11, 11) == 0;
	if (coo_class_var) {
		/* do not output this declaration; before printing implicit struct */
		flush(parser);
		parser->pf.writepos = skip_whitespace(parser, next + 1);
	}

	name = parse_type(parser, NULL, parser->pf.pos, &vartype, PRINT_IMPLICIT);
	if (!to_class(&vartype) && !to_mp(&vartype))
		return;
	if (coo_class_var) {
		if (!to_class(&vartype))
			pr_err(parser->pf.pos, "unknown class name");
		else
			vartype.u.class->is_implemented = 1;
		return;
	}

	/* global variable definition */
	end = skip_word(name);
	variable = alloc_namestruct(parser, struct variable, name, end);
	if (variable == NULL)
		return;

	memcpy(&variable->decl, &vartype, sizeof(variable->decl));
	strhash_insert(&parser->globals, variable);
}

static void parse(struct parser *parser)
{
	char *curr, *next;

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

		curr = parser->pf.pos;
		parser->saw_nodyncast = parser->saw_final = parser->saw_typedef = 0;
		if (strprefixcmp("typedef ", curr)) {
			parser->pf.pos = curr += 8;  /* "typedef " */
			parser->saw_typedef = 1;
		}

		/* do not confuse typedef function pointer with actual function */
		next = scan_token(parser, curr,
			parser->saw_typedef ? "/\n{;" : "/\n({;");
		if (next == NULL)
			break;
		for (;;) {
			if (strprefixcmp("final ", curr)) {
				curr += 6;  /* "final " */
				parser->saw_final = 1;
			} else if (strprefixcmp("nodyncast ", curr)) {
				curr += 10;  /* "nodyncast " */
				parser->saw_nodyncast = 1;
			} else
				break;
		}
		if (*next == '{' && strprefixcmp("struct ", curr)) {
			if (parser->saw_final || parser->saw_nodyncast) {
				/* skip these COO specific keywords */
				flush(parser);
				parser->pf.writepos = parser->pf.pos = curr;
			}
			parse_struct(parser, next);
		} else if (*next == ';' && parser->saw_typedef) {
			parse_typedef(parser, next);
		} else if (*next == ';') {
			parse_global(parser, next);
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
		strhash_init(&parser->methodptrtypes, 16, methodptr) < 0 ||
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
