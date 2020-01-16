/* Copyright 2019, Micha Nelissen, GPLv3 license, see README.md */

#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <alloca.h>
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
#include "list.h"
#include "hash.h"
#include "hasho.h"

#define container_of(ptr, type, node_var) \
  ((type *)((size_t)(ptr)-(size_t)(&((type *)0)->node_var)))

#define MEMBLOCK_SIZE      65536
#define NEWFN_MAX          8192            /* (stack)size for included dirs+filenames */
#define OUTPR_MAX          256             /* maximum outprintf size we do */
#define STDIN_BUFSIZE      (1024 * 1024)   /* size for inputbuffer when using stdin */
#define MAX_USER_ERRORS    25
#define PTRLVLMASK         7               /* bits 2..0 pointer level */

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
#define strhash_remove_key(tbl, name) \
	hash_remove_key(tbl, strhash(name), name)

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

enum anytyp { AT_UNKNOWN, AT_METHOD, AT_CLASS, AT_TEMPLINST, AT_TEMPLPAR };
enum fromtp { FT_NONE, FT_ARGS,  /* args.mem = declared tp anytype */
              FT_INDEX,          /* args.mem[fromtp] = declared tp anytype */
};

const struct anytype {
	union {
		struct class *class;
		struct methodptr *mp;
		struct templpar *tp;    /* template parameter type */
	} u;
	struct dynarr args;          /* struct anytype *, template arguments */
	struct anytype *next;        /* points to variation (shared type) */
	int pointerlevel:8;
	enum anytyp type:8;          /* AT_xxxx */
	enum fromtp from_tp:8;       /* type is mapped from a templpar */
	unsigned implicit:1;         /* for AT_CLASS, still needs "struct " */
	unsigned sharedptr:1;        /* type is a [shared] type pointer */
} unknown_type = { 0 };

#define unknown_typeptr to_anyptr(&unknown_type, 0)

typedef struct {} *anyptr;           /* references anytype | pointerlevel in lsbs */

struct class {
	struct anytype t;            /* type representing this class */
	struct hash members;
	flist(struct member) members_list;
	struct hash templpars;       /* template parameters for this class (types) */
	struct templpar **tparr;     /* array of all templpar pointers */
	struct hasho ancestors;      /* class => ancestor, all classes inherited from */
	flist(struct vmt) vmts;      /* vmts for this class */
	blist(struct parent) descendants;   /* classes inheriting from this class */
	flist(struct parent) dync_parents;  /* parents that can dyncast to us */
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
	char *funcinsert;            /* insert in front of function call "class_" */
	unsigned num_parent_constr;  /* number of parent constructors */
	unsigned num_parent_destr;   /* number of parent destructors */
	unsigned num_init_vars;      /* number of members needing root construction call */
	unsigned num_destr_vars;     /* number of members needing destructor call */
	unsigned num_abstract;       /* number of abstract virtual methods */
	char need_dync_p0_dummy;     /* first dync.parent is not at offset 0 */
	char declare_complete;       /* inside or after struct declaration? */
	char is_final;               /* final class, cannot be inherited from */
	char no_dyncast;             /* user disabled dyncasting to this class */
	char zeroinit;               /* automatically zero-init upon var decl/alloc */
	char refcounted;             /* has automatic reference counting */
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
	struct parent *next_desc;  /* next parent in class->descendants */
	struct parent *next_dync;  /* next dyncastable parent */
	struct dynarr templ_map;   /* map parent's index to child's templpar */
	unsigned char is_primary;
	unsigned char is_virtual;
	unsigned char need_vmt;    /* has own vmt or overrides parent's vmt */
};

struct ancestor {   /* link to parent, or parent of parent, etc.. */
	struct parent *parent;
	struct ancestor *next;     /* alternate path to same origin (interface) */
	struct dynarr templ_map;   /* map ancestor's index to child's templpar */
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
	struct vmt *next;          /* next vmt for owning class */
	struct vmt *alternate;     /* is alternate for this same-origin-vmt */
	struct member *destructor; /* class destructor in this vmt */
	char *path;                /* path to reach this vmt (in ancestor) */
	char *access;              /* path->vmt or path.vmt? */
	char *sec_name;            /* name for use as secondary vmt */
	unsigned char is_primary;  /* is this the primary vmt for this class? */
	unsigned char is_parent_virtual;  /* how to access parent, if any */
	unsigned char from_virtual;  /* vmt inherited from virtual origin */
	char *name;                /* vmt struct type and variable name */
};

struct templpar {
	struct anytype t;          /* this templpar as an anytype */
	struct hash_entry node;    /* entry in class->templpars */
	anyptr impl;               /* implementation, unknown => "void *" */
	char *implstr;             /* representation string of impl */
	unsigned index;            /* this par's index in class declaration */
	char name[];               /* declared name of template type */
};

struct member {
	struct hash_entry node;    /* entry in class->members */
	struct member *next;       /* next member in class->members_list */
	char *rettypestr;          /* literal text of return type */
	anyptr rettype;            /* return type of member */
	struct class *origin;      /* origin class where member was defined */
	struct class *definition;  /* closest class member body is */
	struct class *implemented; /* closest class virtual member overridden */
	  /* definition and implemented differs for destructors
	     they are defined in origin, but implemented in descendant */
	struct class *visi_define; /* closest class visibility was defined */
	struct vmt *vmt;           /* vmt this member is defined in */
	struct vmt *vmt_impl;      /* vmt this member is implemented in */
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
	anyptr decl;              /* type, or return type for funcvar/methodptr */
	struct dynarr params;     /* struct anytype *, function params */
	struct variable *prev;    /* towards lower blocklevel */
	struct hash_entry node;   /* node in parser->locals */
	unsigned blocklevel;
	char name[];
};

enum typedef_implicit { TYPEDEF_EXPLICIT, TYPEDEF_IMPLICIT };

struct classtype {
	struct anytype t;         /* class + ptrlvl for this declaration */
	struct hash_entry node;   /* node in parser.classtypes */
	char name[];
};

struct methodptr {
	struct anytype t;         /* this methodptr as an anytype */
	anyptr rettype;           /* return type of methods of this type */
	char *rettypestr;         /* return type string to copy for casting */
	struct dynarr params;     /* parameters in case class parsing is needed */
	struct hash_entry node;   /* node in parser.methodptrtypes */
	char name[];
};

struct insert {   /* an insert, to insert some text in the output */
	char *flush_until;   /* flush output until here (expression start) */
	char *insert_text;   /* text to insert: e.g. 'this->' or 'vt->' */
	char *continue_at;   /* where to continue writing (perhaps skip something) */
	dlist_item(struct insert) item;  /* in list (parser|initializer)->inserts */
	int continue_after;  /* later-insert before or after this insert? */
};

typedef dlist(struct insert) insert_dlist_t;

struct initializer {
	struct initializer *next;  /* next initializer in parser->initializers */
	struct class *varclass; /* class (root) constructor to call, if any */
	char *params;           /* ... first parameter, to separate from "this" */
	insert_dlist_t inserts; /* inserts for initialization expression */
	char *name;             /* name of variable */
	char *start;            /* start of expression (to skip originally) */
	char *end;              /* ... and its end */
	int lineno;             /* source line number where expression appeared */
};

struct disposer {
	struct disposer *prev;  /* prev disposer in parser->disposers */
	struct class *class;    /* class to call destructor of */
	char *name;		/* name of variable */
	unsigned blocklevel;    /* { } nesting level to destuct at */
	unsigned retblocknr;    /* block number for return goto(s), disposers */
};

enum line_pragma_mode { LINE_PRAGMA_INPUT, LINE_PRAGMA_OUTPUT, LINE_PRAGMA_INVALID };

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
	char defined_tp_impl;     /* defined template parameter impl type */
};

struct parse_file_item {
	struct parse_file_item *prev;  /* previous in parser->file_stack/avail */
	struct parse_file pf;     /* parse file state */
};

struct allocator {
	void *memory;             /* first memory block for allocation */
	void *memblock;           /* current memory block for allocation */
	size_t memptr;            /* pointer to next available memory */
	size_t memavail;          /* available memory in current block */
	int *memerror;            /* out-of-memory condition */
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
	flist(struct initializer) initializers;    /* initializers to be printed */
	blist(struct disposer) disposers;          /* disposers to be printed */
	blist(struct parse_file_item) file_stack;  /* files' state being parsed */
	blist(struct parse_file_item) file_avail;  /* memory available for reuse */
	struct dynarr includepaths;   /* char pointers */
	struct dynarr *param_types;   /* struct anytype *, collect params of funcs */
	struct dynarr type_inserts;   /* 3x char *, replace from/text/to */
	insert_dlist_t inserts_list;  /* struct insert pointers (outer layer) */
	insert_dlist_t *inserts;      /* either inserts_list or a initializer's */
	blist(struct insert) inserts_free;  /* unused, available inserts for reuse */
	struct hash files_seen;       /* struct file_id pointers */
	char *newfilename_end;        /* pointer to end of new input filename */
	struct allocator global_mem;  /* global memory for structs/global vars */
	struct allocator func_mem;    /* function memory for local vars */
	struct class *class;          /* parsing function of this class */
	struct variable *last_local;  /* last local variable */
	char *last_parentname;        /* cache to optimize parentname init */
	int include_ext_in_len;       /* length of include_ext_in, optimization */
	int num_errors;               /* user errors, prevent spam */
	int tempscope_varnr;          /* unique nr for vars in tempscope */
	int type_inserts_delta;       /* delta type_inserts text to insert - to skip */
	int memerror;                 /* allocator out of memory */
	char line_pragmas;            /* print line pragmas for compiler lineno */
	union {
		struct {
			unsigned nodyncast:1;   /* saw the nodyncast keyword */
			unsigned final:1;       /* saw the final keyword */
			unsigned typdef:1;      /* saw the typedef keyword */
			unsigned nozeroinit:1;  /* saw the nozeroinit keyword */
			unsigned zeroinit:1;    /* saw the zeroinit keyword */
			unsigned refcount:1;    /* saw the refcount keyword */
		} k;
		unsigned all;
	} saw;
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

static void init_allocator(struct allocator *alloc, int *memerror)
{
	/* init memblock to point to "memory", a pointer to NULL
	   this will trigger block allocation upon first use/alloc
	   and then initialize alloc->memory to point to first block */
	alloc->memblock = &alloc->memory;
	alloc->memerror = memerror;
}

static void deinit_allocator(struct allocator *alloc)
{
	void *memblock, *next;

	for (memblock = alloc->memory; memblock; memblock = next) {
		next = *(void**)memblock;
		free(memblock);
	}
}

/* init allocator to start using provided block */
static void *useblock(struct allocator *alloc, void *block)
{
	alloc->memblock = block;
	alloc->memptr = (size_t)block + sizeof(void*);
	alloc->memavail = MEMBLOCK_SIZE - sizeof(void*);
	return block;
}

static void *nextblock(struct allocator *alloc)
{
	char *nextblock = *(void**)alloc->memblock;

	if (nextblock == NULL) {
		nextblock = malloc(MEMBLOCK_SIZE);
		if (!nextblock) {
			*alloc->memerror = 1;
			return NULL;
		}

		/* simple linked list */
		*(void**)alloc->memblock = nextblock;
		*(void**)nextblock = NULL;
	}
	return useblock(alloc, nextblock);
}

static void areset(struct allocator *alloc)
{
	useblock(alloc, alloc->memory);
}

static void *aalloc(struct allocator *alloc, size_t alignmask, size_t size)
{
	size_t newaddr, nextptr;

	/* sanity check, only meant for small blocks */
	if (size > MEMBLOCK_SIZE / 2)
		return NULL;
	if (alloc->memavail < size && nextblock(alloc) == NULL)
		return NULL;

	newaddr = (alloc->memptr + alignmask) & ~alignmask;
	nextptr = newaddr + size;
	alloc->memavail -= nextptr - alloc->memptr;
	alloc->memptr = nextptr;
	return (void*)newaddr;
}

static void *azalloc(struct allocator *alloc, size_t alignmask, size_t size)
{
	void *mem = aalloc(alloc, alignmask, size);
	memset(mem, 0, size);
	return mem;
}

static void *arealloc(struct allocator *alloc, void *mem, size_t oldsize, size_t size)
{
	size_t deltasize;
	void *newmem;

	/* sanity check, never shrink */
	if (size <= oldsize)
		return mem;

	deltasize = size - oldsize;
	if ((size_t)mem + oldsize == alloc->memptr && deltasize <= alloc->memavail) {
		alloc->memptr += deltasize;
		alloc->memavail -= deltasize;
		return mem;
	}

	newmem = aalloc(alloc, sizeof(void*)-1, size);
	if (!newmem)
		return NULL;

	memcpy(newmem, mem, oldsize);
	return newmem;
}

static void afree(struct allocator *alloc, void *mem, size_t oldsize)
{
	/* we can only free if this chunk is the last one */
	if ((size_t)mem + oldsize != alloc->memptr)
		return;

	alloc->memavail += oldsize;
	alloc->memptr -= oldsize;
}

#define astralloc(s) aalloc(alloc,0,s)
#define pstralloc(s) aalloc(&parser->global_mem,0,s)
#define agenalloc(s) aalloc(alloc,PTRLVLMASK,s)
#define fgenalloc(s) aalloc(&parser->func_mem,PTRLVLMASK,s)
#define pgenalloc(s) aalloc(&parser->global_mem,PTRLVLMASK,s)
#define agenzalloc(s) azalloc(alloc,PTRLVLMASK,s)
#define fgenzalloc(s) azalloc(&parser->func_mem,PTRLVLMASK,s)
#define pgenzalloc(s) azalloc(&parser->global_mem,PTRLVLMASK,s)

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

static char *astredup(struct allocator *alloc, const char *src, const char *until)
{
	int len = until-src;
	char *newdest = astralloc(len+1);
	if (newdest == NULL)
		return NULL;
	memcpy(newdest, src, len);
	newdest[len] = 0;
	return newdest;
}

#define pstredup(src, until) astredup(&parser->global_mem, src, until)

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

static int vaaprintf(struct allocator *alloc, char **retbuf, const char *format, va_list ap)
{
	va_list ap2;
	char *str = (void*)alloc->memptr;
	int len;

	va_copy(ap2, ap);
	len = vsnprintf(str, alloc->memavail, format, ap2);
	va_end(ap2);
	if ((size_t)len < alloc->memavail) {
		alloc->memptr += (size_t)len+1;
		alloc->memavail -= (size_t)len+1;
		*retbuf = str;
		return len;
	}

	if ((len < 0) || (str = astralloc(len+1)) == NULL) {
		*retbuf = NULL;
		return -1;
	}
	*retbuf = str;
	return vsnprintf(str, len+1, format, ap);
}

static attr_format(3,4) int aaprintf(struct allocator *alloc,
		char **retbuf, const char *format, ...)
{
	va_list va_args;

	va_start(va_args, format);
	return vaaprintf(alloc, retbuf, format, va_args);
}

#define psaprintf(...) aaprintf(&parser->global_mem, __VA_ARGS__)

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

static int grow_dynarr_to(struct allocator *alloc, struct dynarr *dynarr, unsigned minimum)
{
	if (minimum > dynarr->max) {
		unsigned newmax = minimum >= 4 ? minimum * 2 : 4;
		void **newmem = arealloc(alloc, dynarr->mem,
				dynarr->max * sizeof(void*), newmax * sizeof(void*));
		if (newmem == NULL)
			return -1;
		memset(&newmem[dynarr->max], 0, (newmax - dynarr->max) * sizeof(void*));
		dynarr->mem = newmem;
		dynarr->max = newmax;
	}

	return 0;
}

static int grow_dynarr(struct allocator *alloc, struct dynarr *dynarr)
{
	return grow_dynarr_to(alloc, dynarr, dynarr->num+1);
}

#define pgrow_dynarr(dynarr) grow_dynarr(&parser->global_mem, dynarr)

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

static void outwrite(struct parser *parser, const char *buffer, size_t size)
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

static void print_implicit_struct(struct parser *parser, char *position)
{
	/* print "struct " if this type X was implicitly declared by struct X {};
	   in this case the C compiler does not know it, so we need to add "struct " */
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
	lineno = parser->pf.lineno;
	/* note that line pragmas must be relative from current directory!
	   you might expect relative from file location, but no */
	if (newmode == LINE_PRAGMA_INPUT) {
		filename = parser->pf.filename;
	} else {
		filename = parser->pf.outfilename;
		lineno += parser->pf.lines_coo;
	}
	outprintf(parser, "#line %d \"%s\"\n", lineno, filename);
	parser->pf.lines_coo++;
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

/*** type helpers ***/

static const struct anytype *typ(anyptr p)
{
	return (const struct anytype*)((size_t)p & ~PTRLVLMASK);
}

static struct anytype *modtyp(anyptr p)
{
	return (struct anytype*)((size_t)p & ~PTRLVLMASK);
}

static unsigned raw_ptrlvl(anyptr p)
{
	return (size_t)p & PTRLVLMASK;
}

static unsigned ptrlvl(anyptr p)
{
	return raw_ptrlvl(p) + typ(p)->pointerlevel;
}

static anyptr to_anyptr(const struct anytype *any, unsigned pointerlevel)
{
	return (void*)((size_t)any | pointerlevel);
}

static void add_ptrlvl(anyptr *p, unsigned pointerlevel)
{
	*p = (anyptr)(((size_t)*p & ~PTRLVLMASK) | ((raw_ptrlvl(*p) + pointerlevel) & PTRLVLMASK));
}

static void sub_ptrlvl(anyptr *p, unsigned pointerlevel)
{
	*p = (anyptr)(((size_t)*p & ~PTRLVLMASK) | ((raw_ptrlvl(*p) - pointerlevel) & PTRLVLMASK));
}

static void clear_ptrlvl(anyptr *p)
{
	*p = (void*)((size_t)*p & ~PTRLVLMASK);
}

static struct anytype *alloctype(struct allocator *alloc, enum anytyp type, void *class)
{
	struct anytype *newtype;

	newtype = agenzalloc(sizeof(*newtype));
	if (newtype == NULL)
		return NULL;

	newtype->type = type;
	newtype->u.class = class;
	return newtype;
}

static struct anytype *duptype(struct allocator *alloc, const struct anytype *source)
{
	struct anytype *newtype;

	if ((newtype = agenalloc(sizeof(*newtype))) == NULL)
		return NULL;
	memcpy(newtype, source, sizeof(*newtype));
	return newtype;
}

anyptr get_from_tpdecl(const struct anytype *any)
{
	switch (any->from_tp) {
	case FT_NONE: return NULL;
	case FT_ARGS: return (anyptr)any->args.mem;
	default: return any->args.mem[any->from_tp];
	}
}

void set_from_tpdecl(struct allocator *alloc, struct anytype *dest, anyptr decl)
{
	if (decl == NULL) {
		dest->from_tp = FT_NONE;
		return;
	}
	if (dest->args.max == 0) {
		dest->args.mem = (void*)decl;
		dest->from_tp = FT_ARGS;
	} else {
		if (grow_dynarr(alloc, &dest->args) < 0)
			return;
		dest->from_tp = dest->args.num >= FT_INDEX ? dest->args.num : FT_INDEX;
		dest->args.mem[dest->from_tp] = decl;
	}
}

static void init_anytype(struct anytype *dest, anyptr source)
{
	memcpy(dest, typ(source), sizeof(*dest));
	dest->pointerlevel += raw_ptrlvl(source);
}

/*** parser helpers ***/

static void *alloc_namestruct_s(struct allocator *alloc,
		size_t structsize, char *name, char *nameend)
{
	size_t len = nameend-name, allocsize = structsize + len + 1;
	char *ret, *dest;

	ret = agenzalloc(allocsize);
	if (ret == NULL)
		return NULL;

	dest = ret + structsize;
	memcpy(dest, name, len);
	dest[len] = 0;
	return ret;
}

#define alloc_namestruct(p,s,n,e) alloc_namestruct_s(p, offsetof(s, name), n, e)

static struct class *init_class(struct parser *parser, struct class *class)
{
	class->t.type = AT_CLASS;
	class->t.u.class = class;
	strhash_init(&class->members, 8, member);
	strhash_init(&class->templpars, 4, templpar);
	hasho_init(&class->ancestors, 8);
	flist_init(&class->members_list);
	flist_init(&class->vmts);
	flist_init(&class->dync_parents);
	if (hash_insert(&parser->classes, &class->node, strhash(class->name))) {
		/* already exists */
		return NULL;
	}
	return class;
}

static void deinit_class(struct class *class)
{
	hash_deinit(&class->members);
	hash_deinit(&class->templpars);
	hasho_deinit(&class->ancestors);
}

static struct class *addclass(struct parser *parser, char *classname, char *nameend)
{
	struct class *class;

	class = alloc_namestruct(&parser->global_mem, struct class, classname, nameend);
	if (class == NULL)
		return NULL;

	return init_class(parser, class);
}

static char *get_class_funcinsert(struct parser *parser, struct class *class)
{
	if (!class->funcinsert)
		psaprintf(&class->funcinsert, "%s_", class->name);

	return class->funcinsert;
}

static struct classtype *addclasstype(struct parser *parser, char *typename, char *nameend)
{
	struct classtype *type;

	type = alloc_namestruct(&parser->global_mem, struct classtype, typename, nameend);
	if (type == NULL)
		return NULL;

	if (strhash_insert(&parser->classtypes, type)) {
		/* already exists, no free necessary, using parser memory */
		return NULL;
	}
	return type;
}

static struct templpar *addtemplpar(struct parser *parser,
		struct class *class, char *typename, char *nameend)
{
	struct templpar *tp;

	tp = alloc_namestruct(&parser->global_mem, struct templpar, typename, nameend);
	if (tp == NULL)
		return NULL;

	tp->index = class->templpars.num_entries;
	if (strhash_insert(&class->templpars, tp)) {
		/* already exists (tp allocated in parser memory, can't free) */
		return NULL;
	}

	tp->t.type = AT_TEMPLPAR;
	tp->t.u.tp = tp;
	tp->implstr = "void *";
	tp->impl = unknown_typeptr;
	return tp;
}

static char *replace_templpar(struct parser *parser,
		struct templpar *tp, char *pos, char *next)
{
	flush_until(parser, pos);
	outputs(parser, tp->implstr);
	if (*next == ' ')
		next++;
	return parser->pf.writepos = next;
}

static struct class *to_lit_class(const struct anytype *any)
{
	if (any->type == AT_CLASS || any->type == AT_TEMPLINST)
		return any->u.class;
	return NULL;
}

static struct class *to_class(const struct anytype *any)
{
	if (any->type == AT_CLASS || any->type == AT_TEMPLINST)
		return any->u.class;
	if (any->type == AT_TEMPLPAR)
		return to_class(typ(any->u.tp->impl));
	return NULL;
}

static struct methodptr *to_mp(const struct anytype *any)
{
	return any->type == AT_METHOD ? any->u.mp : NULL;
}

#if 0
static struct templpar *to_tp(struct anytype *any)
{
	return any->type == AT_TEMPLPAR ? any->u.tp : NULL;
}

static unsigned sum_pointerlevel(struct anytype *any)
{
	return any->pointerlevel + (any->type == AT_TEMPLPAR ? any->u.tp->impl.pointerlevel : 0);
}
#endif

static anyptr sel_class(anyptr any)
{
	const struct anytype *anytype = typ(any);
	if (anytype->type == AT_CLASS || anytype->type == AT_TEMPLINST)
		return any;
	if (anytype->type == AT_TEMPLPAR)
		if (to_class(typ(anytype->u.tp->impl)))
			return anytype->u.tp->impl;
	return NULL;
}

static int cmp_templ_args(const struct anytype *from,
		const struct anytype *to, struct ancestor *ancestor);

static int is_assignable(const struct anytype *from, const struct anytype *to)
{
	struct ancestor *ancestor;

	/* template instance might be assignable to a class */
	if (from->type != to->type && !to_class(from))
		return 0;
	if (from->type == AT_UNKNOWN)
		return 0;
	if (from->pointerlevel != to->pointerlevel)
		return 0;
	if (from->u.class == to->u.class)
		return 1;
	if (from->type == AT_METHOD || from->type == AT_TEMPLPAR)
		return 0;

	ancestor = hasho_find(&from->u.class->ancestors, to->u.class);
	if (ancestor == NULL)
		return -1;

	/* not equal class, but ancestor is also OK */
	return cmp_templ_args(from, to, ancestor) == 0;
}

/* compare two template argument lists to be compatible, ancestor: from(child) to parent(to) */
static int cmp_templ_args(const struct anytype *from,
		const struct anytype *to, struct ancestor *ancestor)
{
	struct anytype *tmplarg, *fromtype, *totype;
	unsigned i, numargs;

	numargs = to->type == AT_TEMPLINST ? to->args.num : 0;
	if (numargs != ancestor->templ_map.num)
		return -1;
	for (i = 0; i < ancestor->templ_map.num; i++) {
		tmplarg = ancestor->templ_map.mem[i];
		if (tmplarg == NULL)
			continue;
		/* non-template types are fixed by class definition, so OK */
		if (tmplarg->type != AT_TEMPLPAR)
			continue;
		fromtype = from->args.mem[tmplarg->u.tp->index];
		totype = to->args.mem[tmplarg->u.tp->index];
		if (!is_assignable(fromtype, totype))
			return -1;
	}

	return 0;
}

typedef void (*new_insert_cb)(struct parser *parser, char *from, char *text, char *to);
typedef void (*new_param_cb)(struct parser *parser, struct allocator *alloc, int position,
		struct class *class, anyptr decl, char *name, char *next);
typedef void (*check_name_cb)(struct parser *parser, char *name, char *next);

static char *parse_templargs(struct parser *parser, struct allocator *alloc, struct class *class,
		struct class *parentclass, char *next, struct dynarr *templ_map);

static void save_type_insert(struct parser *parser, char *from, char *text, char *to)
{
	struct dynarr *dynarr = &parser->type_inserts;

	if (grow_dynarr_to(&parser->global_mem, dynarr, dynarr->num + 3))
		return;

	dynarr->mem[dynarr->num++] = from;
	dynarr->mem[dynarr->num++] = text;
	dynarr->mem[dynarr->num++] = to;
	parser->type_inserts_delta += strlen(text) - (to - from);
}

static void print_type_insert(struct parser *parser, char *from, char *text, char *to)
{
	flush_until(parser, from);
	outputs(parser, text);
	parser->pf.writepos = to;
}

static char *parse_type(struct parser *parser, struct allocator *alloc,
		struct class *ctxclass, char *pos, anyptr *rettype, new_insert_cb new_insert)
{
	struct classtype *classtype;
	struct anytype *sharedtype, *newtype = NULL;
	struct methodptr *mp;
	struct templpar *tp;
	struct class *class;
	char *name, *next;
	anyptr newptr = unknown_typeptr;
	int seen_shared = 0;

	for (;; pos = skip_whitespace(parser, pos)) {
		if (strprefixcmp("[shared]", pos) != NULL) {
			next = skip_whitespace(parser, pos + 8);  /* [shared] */
			if (new_insert)
				new_insert(parser, pos, "", next);
			pos = next;
			seen_shared = 1;
			continue;
		}
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
			class = find_class_e(parser, name, next);
			if (class)
				newptr = to_anyptr(&class->t, 0);
			break;
		}

		if (!iswordstart(*pos)) {
			next = pos;
			break;
		}
		next = skip_word(pos);
		if ((classtype = find_classtype_e(parser, pos, next)) != NULL) {
			if (new_insert && classtype->t.implicit)
				new_insert(parser, pos, "struct ", pos);
			newptr = to_anyptr(&classtype->t, 0);
			if (*next == '<') {
				/* alloc copy so we can fill in template arguments */
				newtype = duptype(alloc, &classtype->t);
				if (!newtype)
					return NULL;
				newtype->type = AT_TEMPLINST;
				next = parse_templargs(parser, alloc, ctxclass,
					newtype->u.class, next, &newtype->args);
				newptr = to_anyptr(newtype, 0);
			}
			break;
		}
		if ((mp = find_methodptr_e(parser, pos, next)) != NULL) {
			newptr = to_anyptr(&mp->t, 0);
			break;
		}
		if (ctxclass == NULL)
			break;
		if ((tp = find_templpar_e(ctxclass, pos, next)) != NULL) {
			if (*next == ' ')
				next++;
			if (new_insert)
				new_insert(parser, pos, tp->implstr, next);
			newptr = to_anyptr(&tp->t, 0);
			break;
		}
		break;
	}

	for (next = skip_whitespace(parser, next); *next == '*'; next++)
		add_ptrlvl(&newptr, 1);
	if (seen_shared) {
		if (newtype) {
			newtype->sharedptr = 1;
		} else {
			sharedtype = typ(newptr)->next;
			if (!sharedtype || !sharedtype->sharedptr) {
				sharedtype = duptype(alloc, typ(newptr));
				if (!sharedtype)
					return NULL;
				sharedtype->sharedptr = 1;
				/* (modify) connect to existing type */
				modtyp(newptr)->next = sharedtype;
			}
			newptr = to_anyptr(sharedtype, 0);
		}
	}
	*rettype = newptr;
	return next;
}

static char *parse_parameters(struct parser *parser, struct allocator *alloc,
		struct class *class, char *params, new_insert_cb new_insert,
		new_param_cb new_param, check_name_cb check_name)
{
	char *next, *curr;
	int position;
	anyptr decl;

	memset(&decl, 0, sizeof(decl));
	for (position = 0, next = params;; position++, next++) {
		curr = strskip_whitespace(next);
		if (curr[0] == 0)
			return NULL;
		curr = parse_type(parser, alloc, class, curr, &decl, new_insert);
		if (curr == NULL)
			return NULL;

		/* find parameter name */
		for (;; curr++) {
			if (iswordstart(*curr)) {
				next = skip_word(curr);
				break;
			} else if (*curr == ',' || *curr == ')' || *curr == 0) {
				/* empty name */
				next = curr;
				break;
			}
		}

		new_param(parser, alloc, position, class, decl, curr, next);
		if (check_name)
			check_name(parser, curr, next);
		if (*next == ')')
			return next + 1;
		if (*next == 0)
			return next;
	}
}

static int insertmember(struct class *class,
		struct member *member, struct member **dupmember)
{
	struct hash_entry *entry;

	/* check already exists */
	if ((entry = strhash_insert(&class->members, member))) {
		if (hash_insert_nomem(entry))
			return -1;
		*dupmember = container_of(entry, struct member, node);
		return -1;
	}

	flist_add(&class->members_list, member, next);
	return 0;
}

static struct member *allocmember_e(struct parser *parser, struct class *class,
		char *membername, char *nameend, struct member **dupmember)
{
	struct member *member;

	member = alloc_namestruct(&parser->global_mem, struct member, membername, nameend);
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

	member = pgenalloc(size);
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
		psaprintf(&member->nameinsert, "%s%s",
			member->parentname, member->parent_virtual ? "->" : ".");
	return member->nameinsert;
}

static void save_param_type(struct parser *parser, struct allocator *alloc, int position,
		struct class *class, anyptr decl, char *name, char *next)
{	(void)class; (void)name; (void)next;

	if (grow_dynarr_to(alloc, parser->param_types, position+1) < 0)
		return;

	parser->param_types->mem[position] = decl;
	parser->param_types->num = position+1;
}

static void addmember_to_children(struct parser *parser, char *parsepos,
		struct class *class, struct member *member);

static char *print_type_inserts(struct parser *parser,
		struct allocator *alloc, char *src, char *srcend)
{
	char *dest, *from, *text, *params_new;
	struct dynarr *inserts;
	size_t textlen;
	unsigned i;

	params_new = astralloc(srcend - src + 1 + parser->type_inserts_delta);
	if (params_new == NULL)
		return NULL;

	inserts = &parser->type_inserts;
	for (dest = params_new, i = 0; i < inserts->num;) {
		from = inserts->mem[i++];
		text = inserts->mem[i++];
		memcpy(dest, src, from - src);
		dest += from - src;
		textlen = strlen(text);
		memcpy(dest, text, textlen);
		dest += textlen;
		src = inserts->mem[i++];
	}
	/* src moved in meanwhile, calculate remaining */
	textlen = srcend - src;
	memcpy(dest, src, textlen);
	dest[textlen] = 0;
	inserts->num = 0;
	return params_new;
}

static void parse_parameters_to(struct parser *parser, struct allocator *alloc,
	struct class *class, char *params, char **params_out, struct dynarr *param_types)
{
	char *paramsend;

	/* save_param_class stores params in parser->param_types */
	parser->type_inserts_delta = 0;
	parser->param_types = param_types;
	paramsend = parse_parameters(parser, alloc, class, params,
		params_out ? save_type_insert : print_type_insert, save_param_type, NULL);
	parser->param_types = NULL;
	if (params_out)
		*params_out = print_type_inserts(parser, alloc, params, paramsend);
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

static struct member *addmember(struct parser *parser,
		struct class *class, anyptr rettype, char *rettypestr,
		char *membername, char *nameend, char *params, struct memberprops props)
{
	struct member *member, *dupmember;
	char *end, *parsepos = rettypestr, void_ret;

	/* special check for destructors because their names vary (== ~classname) */
	if (membername[0] == '~' && class->vmt && class->vmt->destructor) {
		pr_err(membername, "destructor must override %s",
			class->vmt->destructor->name);
		return NULL;
	}

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
	member->implemented = class;
	member->props = props;
	member->rettypestr = rettypestr;
	if (props.is_abstract)
		class->num_abstract++;
	member->rettype = rettype;
	if (typ(rettype)->type == AT_METHOD && !params) {
		member->rettypestr = typ(rettype)->u.mp->rettypestr;
		member->paramstext = "";
		memcpy(&member->params, &typ(rettype)->u.mp->params, sizeof(member->params));
		goto out;
	}

	if (params) {
		if (is_void_params(params))
			member->paramstext = ")";
		else
			parse_parameters_to(parser, &parser->global_mem, class,
				params, &member->paramstext, &member->params);
	}
	if (props.is_virtual) {
		member->vmt = member->vmt_impl = class->vmt;
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
				void_ret = rettypestr == NULL;
				if (!void_ret)
					void_ret = is_void_rettype(rettypestr);
				/* even if constructor is void in COO-speak, then in C
				   translation still return class pointer for chaining */
				if (void_ret) {
					psaprintf(&member->rettypestr,
						"struct %s *", class->name);
					member->rettype = to_anyptr(&class->t, 1);
				} else if (to_class(typ(rettype)) != class || ptrlvl(rettype) != 1)
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
			if (rettypestr)
				pr_err(membername, "return type is not allowed for destructor");
			member->is_destructor = 1;
			class->has_destructor = 1;
			class->destructor = member;
			class->freer_class = class;
			if (props.is_virtual)
				class->vmt->destructor = member;
		} else {
			pr_err(membername, "invalid member name, "
				"did you mean ~%s?", class->name);
		}
		member->rettypestr = "void ";
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
	struct memberprops constructorprops = { 0 };

	constructorprops.is_function = 1;
	/* rettypestr NULL => use void behavior */
	addmember(parser, class, unknown_typeptr, NULL,
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
		anyptr decl, int extraptr, char *membername, char *nameend)
{
	struct variable *variable;

	/* variables have lifetime of function parsing */
	variable = alloc_namestruct(&parser->func_mem,
			struct variable, membername, nameend);
	if (variable == NULL)
		return NULL;

	variable->prev = parser->last_local;
	parser->last_local = variable;
	if (strhash_insert(&parser->locals, variable)) {
		/* no need to free variable, ref stored in dynarr */
		return NULL;
	}

	variable->decl = decl;
	add_ptrlvl(&variable->decl, extraptr);  /* typedef type + extra pointers */
	variable->blocklevel = blocklevel;
	return variable;
}

static struct variable *addvariable(struct parser *parser, struct class *class,
		unsigned blocklevel, anyptr decl, int extraptr,
		char *membername, char *nameend, char *params)
{
	struct variable *variable;

	variable = addvariable_common(parser, blocklevel, decl, extraptr, membername, nameend);
	if (variable == NULL)
		return NULL;
	if (params)
		parse_parameters_to(parser, &parser->func_mem,
			class, params, NULL, &variable->params);
	return variable;
}

static struct variable *addclsvariable(struct parser *parser,
		unsigned blocklevel, anyptr decl,
		int extraptr, char *membername, char *nameend)
{
	return addvariable_common(parser, blocklevel, decl, extraptr, membername, nameend);
}

static struct variable *addmpvariable(struct parser *parser, unsigned blocklevel,
		anyptr mpdecl, int extraptr, char *membername, char *nameend)
{
	struct variable *variable;

	variable = addvariable_common(parser, blocklevel,
		mpdecl, extraptr, membername, nameend);
	if (variable == NULL)
		return NULL;

	memcpy(&variable->params, &typ(mpdecl)->u.mp->params, sizeof(variable->params));
	return variable;
}

static struct methodptr *allocmethodptrtype(struct parser *parser,
		anyptr rettype, char *name, char *nameend)
{
	struct methodptr *mptype;

	mptype = alloc_namestruct(&parser->global_mem, struct methodptr, name, nameend);
	if (mptype == NULL)
		return NULL;
	if (strhash_insert(&parser->methodptrtypes, mptype))
		return NULL;

	mptype->t.type = AT_METHOD;
	mptype->t.u.mp = mptype;
	mptype->rettype = rettype;
	return mptype;
}

static struct methodptr *addmethodptrtype(struct parser *parser, anyptr rettype,
	char *rettypestr, char *rettypeend, char *name, char *nameend, char *params)
{
	struct methodptr *mptype;

	mptype = allocmethodptrtype(parser, rettype, name, nameend);
	if (mptype == NULL)
		return NULL;

	mptype->rettypestr = pstredup(rettypestr, rettypeend);
	parse_parameters_to(parser, &parser->global_mem, NULL, params, NULL, &mptype->params);
	return mptype;
}

static struct methodptr *dupmethodptrtype(struct parser *parser,
		char *name, char *nameend, struct methodptr *source)
{
	struct methodptr *mptype;

	mptype = allocmethodptrtype(parser, source->rettype, name, nameend);
	if (mptype == NULL)
		return NULL;

	mptype->rettypestr = source->rettypestr;
	memcpy(&mptype->params, &source->params, sizeof(mptype->params));
	return mptype;
}

/* compatible with struct insert.continue_after */
enum insert_continue { CONTINUE_BEFORE, CONTINUE_AFTER };

static struct insert *addinsert(struct parser *parser, struct insert *insert_before,
		char *flush_until, char *insert_text, char *continue_at,
		enum insert_continue insert_continue)
{
	struct insert *insert;

	if (!insert_before) {
		/* check flush ordering to be incremental */
		insert_before = dlist_head(parser->inserts, item);
		dlist_foreach_rev(insert, parser->inserts, item) {
			/* if the next insert (the one before insert_before)
			   is before where to insert, then found insert_before */
			if (insert->continue_at < flush_until)
				break;
			if (insert->continue_at == flush_until
					&& insert->continue_after)
				break;
			insert_before = insert;
		}
	}

	if (!blist_pop_last(&parser->inserts_free, &insert, item.inext))
		if (!(insert = pgenalloc(sizeof(*insert))))
			return NULL;

	insert->flush_until = flush_until;
	insert->insert_text = insert_text;
	insert->continue_at = continue_at;
	insert->continue_after = insert_continue;
	dlist_insert_before(insert_before, insert, item);
	return insert_before;
}

static attr_format(6,7) struct insert *addinsert_format(struct parser *parser,
		struct insert *before_insert, char *flush_until, char *continue_at,
		enum insert_continue insert_continue, char *insert_format, ...)
{
	va_list va_args;
	char *insert_text;

	va_start(va_args, insert_format);
	if (vaaprintf(&parser->func_mem, &insert_text, insert_format, va_args) < 0)
		return NULL;

	return addinsert(parser, before_insert,
			flush_until, insert_text, continue_at, insert_continue);
}

static void addinsert_implicit_struct(struct parser *parser, char *position)
{
	/* print "struct " if this type X was implicitly declared by struct X {};
	   in this case the C compiler does not know it, so we need to add "struct " */
	addinsert(parser, NULL, position, "struct ", position, CONTINUE_BEFORE);
}

static void addinsert_templpar(struct parser *parser,
		struct templpar *tptype, char *name, char *next)
{
	addinsert(parser, NULL, name, tptype->implstr, next, CONTINUE_AFTER);
}

static struct insert *insert_text(struct parser *parser, struct insert *before_insert,
		char *pos, char *text, char *continue_at,
		enum insert_continue insert_continue)
{
	if (!dlist_empty(parser->inserts, item))
		return addinsert(parser, before_insert,
			pos, text, continue_at, insert_continue);
	flush_until(parser, pos);
	outputs(parser, text);
	parser->pf.writepos = continue_at;
	return NULL;
}

static struct initializer *addinitializer(struct parser *parser,
		struct class *varclass, char *name, char *start)
{
	struct initializer *initializer;

	initializer = fgenzalloc(sizeof *initializer);
	if (initializer == NULL)
		return NULL;

	initializer->lineno = parser->pf.lineno;
	initializer->varclass = varclass;
	initializer->name = name;
	initializer->start = start;
	flist_add(&parser->initializers, initializer, next);
	dlist_init(&initializer->inserts, item);
	parser->inserts = &initializer->inserts;
	return initializer;
}

static struct disposer *adddisposer(struct parser *parser,
		struct class *class, char *name, unsigned blocklevel, unsigned retblocknr)
{
	struct disposer *disposer;

	if (!class->destructor)
		return NULL;
	disposer = fgenalloc(sizeof *disposer);
	if (disposer == NULL)
		return NULL;

	disposer->class = class;
	disposer->name = name;
	disposer->blocklevel = blocklevel;
	disposer->retblocknr = retblocknr;
	blist_add(&parser->disposers, disposer, prev);
	return disposer;
}

static int addincludepath(struct parser *parser, char *path)
{
	if (grow_dynarr(&parser->global_mem, &parser->includepaths) < 0)
		return -1;

	/* pointer to commandline so do not have to copy */
	parser->includepaths.mem[parser->includepaths.num++] = path;
	return 0;
}

static struct parse_file_item *addfilestack(struct parser *parser)
{
	struct parse_file_item *newfile;

	if (blist_pop_last(&parser->file_avail, &newfile, prev))
		goto out;

	newfile = pgenzalloc(sizeof *newfile);
	if (newfile == NULL)
		return NULL;

out:
	blist_add(&parser->file_stack, newfile, prev);
	return newfile;
}

static void remove_locals(struct parser *parser, unsigned blocklevel)
{
	struct variable *var;

	for (var = parser->last_local; var; var = var->prev) {
		if (var->blocklevel < blocklevel)
			break;
		hash_remove(&parser->locals, &var->node);
	}
}

static void pr_lineno(struct parser *parser, int lineno)
{
	if (parser->line_pragmas && parser->pf.line_pragma_mode == LINE_PRAGMA_INPUT) {
		outprintf(parser, "#line %d\n", lineno);
		parser->pf.lines_coo++;
	}
}

static int find_global_e_class(struct parser *parser,
		char *name, char *nameend, anyptr *ret_decl, struct dynarr **retparams)
{
	struct variable *var = find_global_e(parser, name, nameend);
	if (var == NULL)
		return -1;
	*ret_decl = var->decl;
	/* TODO: parse function parameters for variables (params always empty now) */
	*retparams = &var->params;
	return 0;
}

static int find_local_e_class(struct parser *parser,
		char *name, char *nameend, anyptr *ret_decl, struct dynarr **retparams)
{
	struct variable *var = find_local_e(parser, name, nameend);
	if (var == NULL)
		return -1;
	*ret_decl = var->decl;
	*retparams = &var->params;
	return 0;
}

static int ensure_tparr(struct parser *parser, struct class *class)
{
	struct templpar *tp;

	if (class->tparr)
		return 0;

	/* create array for index-based access */
	class->tparr = pgenalloc(class->templpars.num_entries * sizeof(*class->tparr));
	if (class->tparr == NULL)
		return -1;
	hash_foreach(tp, &class->templpars)
		class->tparr[tp->index] = tp;
	return 0;
}

static char *parse_templargs(struct parser *parser, struct allocator *alloc,
		struct class *class, struct class *parentclass,
		char *next, struct dynarr *templ_map)
{
	struct class *prtclass, *argclass;
	const struct anytype *argsrc;
	struct anytype *argtype;
	struct templpar *prtp;
	unsigned numpars;
	char *name, *sep;
	anyptr argptr;

	if (!parentclass->templpars.num_entries)
		pr_err(next, "%s is not a template class", parentclass->name);
	if (ensure_tparr(parser, parentclass) < 0)
		return NULL;

	/* templ_map always points to empty dynarr, allocate simple array here */
	numpars = parentclass->templpars.num_entries;
	templ_map->mem = agenalloc(numpars * sizeof(void*));
	if (templ_map->mem == NULL)
		return NULL;

	/* next is at '<', skip */
	templ_map->max = numpars;
	for (templ_map->num = 0, next++;;) {
		name = skip_whitespace(parser, next);
		if (!iswordstart(*name)) {
			pr_err(name, "expected a name");
			break;
		}
		next = parse_type(parser, alloc, class, name, &argptr, NULL);
		argsrc = typ(argptr);
		if (argsrc->type == AT_UNKNOWN)
			pr_err(name, "unknown template type");
		if (argsrc->type != AT_TEMPLPAR && ptrlvl(argptr) == 0)
			pr_err(name, "template argument must be a pointer type");
		if (templ_map->num >= numpars) {
			pr_err(name, "too many arguments, expect %u", numpars);
			goto next;
		}
		if (argsrc->type == AT_TEMPLINST) {
			/* templinst types are always new, parse_type allocated it */
			argtype = (struct anytype *)argsrc;
		} else {
			/* make a private instance so we can set the declaration origin */
			argtype = duptype(alloc, argsrc);
			argptr = to_anyptr(argtype, raw_ptrlvl(argptr));
		}
		prtp = parentclass->tparr[templ_map->num];
		set_from_tpdecl(alloc, argtype, prtp->impl);
		templ_map->mem[templ_map->num++] = argptr;
		prtclass = to_class(typ(prtp->impl));
		if (prtclass) {
			if ((argclass = to_class(argtype)) == NULL) {
				pr_err(name, "parent template parameter expects class "
					"'%s'", prtclass->name);
			} else if (argclass != prtclass) {
				if (!hasho_find(&argclass->ancestors, prtclass))
					pr_err(name, "'%s' is not a descendant of '%s'",
						argclass->name, prtclass->name);
			}
		}
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

static int need_translate_tp(struct parser *parser, struct dynarr *templ_map, anyptr any)
{
	const struct anytype *anytype = typ(any), *prtype, *anyimpl, *primpl;

	if (any == NULL)
		return 0;
	if (anytype->type != AT_TEMPLPAR)
		return 0;
	if (anytype->u.tp->index >= templ_map->num) {
		pr_err(NULL, "(ierr) parent tp index %u out of range %u",
			anytype->u.tp->index, templ_map->num);
		return 0;
	}
	prtype = typ(templ_map->mem[anytype->u.tp->index]);
	if (prtype->type == AT_TEMPLPAR) {
		anyimpl = typ(anytype->u.tp->impl), primpl = typ(prtype->u.tp->impl);
		if (anyimpl->type != primpl->type)
			return 1;
		if (anyimpl->type == AT_UNKNOWN)
			return 0;
		if (anyimpl->type == AT_CLASS && anyimpl->u.class == primpl->u.class)
			return 0;
	}
	return 1;
}

static anyptr translate_tp(struct allocator *alloc, const struct dynarr *templ_map, anyptr src)
{
	const struct anytype *srctype = typ(src);
	struct anytype *dest;
	anyptr tpdecl = get_from_tpdecl(srctype);
	anyptr mapsrc = templ_map->mem[srctype->u.tp->index];

	if (tpdecl == NULL)
		tpdecl = srctype->u.tp->impl;
	/* allocate unique copy to not alter parentmember's param type */
	dest = duptype(alloc, typ(mapsrc));
	if (dest == NULL)
		return NULL;
	set_from_tpdecl(alloc, dest, tpdecl);
	/* keep pointerlevel, replace with newly modified type */
	return to_anyptr(dest, raw_ptrlvl(src) + raw_ptrlvl(mapsrc));
}

static struct member *inheritmember(struct parser *parser, char *parsepos,
		struct class *class, struct parent *parent, struct member *parentmember)
{
	struct member *member, *dupmember = NULL;
	unsigned i, allocsize;
	void **newmem;

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
			psaprintf(&member->parentname, "%s%s%s",
				parent->class->name, parent->is_virtual ? "->" : ".",
				parentmember->parentname);
			parser->last_parentname = member->parentname;
		}
	} else {
		member->parentname = parent->class->name;
		member->parent_virtual = parent->is_virtual;
	}
	if (need_translate_tp(parser, &parent->templ_map, member->rettype))
		if ((member->rettype = translate_tp(&parser->global_mem, &parent->templ_map,
				member->rettype)) == NULL)
			return NULL;
	/* params can't be translated in-place, the params array is still pointing
	   to parentmember's allocated memory, and so are the anytypes
	   if any param needs translation, reallocate and copy the params */
	for (i = 0, allocsize = 0; i < member->params.num; i++) {
		if (!need_translate_tp(parser, &parent->templ_map, member->params.mem[i]))
			continue;
		if (!allocsize) {
			allocsize = member->params.num * sizeof(void*);
			newmem = pgenalloc(allocsize);
			if (newmem == NULL)
				return NULL;
			memcpy(newmem, member->params.mem, allocsize);
			member->params.max = member->params.num;
			member->params.mem = newmem;
		}
		if ((member->params.mem[i] = translate_tp(&parser->global_mem,
				&parent->templ_map, member->params.mem[i])) == NULL)
			return NULL;
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

	blist_foreach_rev(parent, &class->descendants, next_desc) {
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
	struct vmt *vmt;
	char *nameend;

	/* regular non-virtual functions are always okay */
	if (!parentmember->props.is_virtual)
		return;

	nameend = parentmember->name + strlen(parentmember->name);
	member = find_member_e(class, parentmember->name, nameend);
	if (member == NULL) {
		fprintf(stderr, "(ierr) cannot find member %s in class %s "
			"for merging\n", parentmember->name, class->name);
		parser->num_errors++;
		return;
	}

	/* both agree where they are defined, ok */
	if (member->definition == parentmember->definition)
		return;

	if (member->definition == member->origin) {
		/* existing member did not override yet, use new parent's definition */
		member->definition = parentmember->definition;
		if (member->props.is_abstract) {
			member->props.is_abstract = 0;
			class->num_abstract--;
		}
	} else if (parentmember->definition == parentmember->origin) {
		/* new parent member did not override yet, update vmt for that class */
		vmt = parentmember->vmt->child;
		vmt->modified = vmt;
	} else {
		/* if both parents override this member, mark member as to-be overridden */
		class->has_duplicates = 1;
		member->definition = NULL;
	}
}

static struct ancestor *add_ancestor(struct parser *parser, struct class *class,
		int from_virtual, struct parent *origin_parent,
		char *via_class_name, char *link, char *path)
{
	struct hasho_entry *orig_entry;
	struct ancestor *ancestor, *orig_ancestor;

	/* allocate struct and printf path in one alloc */
	if (psaprintf((char**)&ancestor, "%*s%s%s%s",
			(int)offsetof(struct ancestor, path),
			"", via_class_name, link, path) < 0)
		return NULL;

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

	return ancestor;
}

static void map_ancestor_templ_pars(struct parser *parser, struct ancestor *new_ancestor,
		struct ancestor *ancestor, struct parent *parent)
{
	struct anytype *ta;
	unsigned i;

	new_ancestor->templ_map.num = 0;
	if (ancestor->templ_map.num == 0)
		return;

	new_ancestor->templ_map.mem = pgenalloc(ancestor->templ_map.num * sizeof(void*));
	if (new_ancestor->templ_map.mem == NULL)
		return;

	new_ancestor->templ_map.num = new_ancestor->templ_map.max = ancestor->templ_map.num;
	for (i = 0; i < ancestor->templ_map.num; i++) {
		/* get template argument passed by parent class */
		ta = ancestor->templ_map.mem[i];
		/* translate template type, others stay as-is */
		if (ta && ta->type == AT_TEMPLPAR) {
			/* lookup the mapping to our class */
			if (ta->u.tp->index < parent->templ_map.num)
				ta = parent->templ_map.mem[ta->u.tp->index];
			else
				ta = NULL;
		}
		new_ancestor->templ_map.mem[i] = ta;
	}
}

static void add_ancestors(struct parser *parser, struct class *class, struct parent *parent)
{
	struct class *parentclass = parent->class;
	struct hasho_entry *ancestor_entry;
	struct ancestor *ancestor, *new_ancestor;
	int from_virtual;

	new_ancestor = add_ancestor(parser, class, parent->is_virtual,
		parent, parentclass->name, "", "");
	if (new_ancestor == NULL)
		return;

	memcpy(&new_ancestor->templ_map, &parent->templ_map, sizeof(new_ancestor->templ_map));
	hasho_foreach(ancestor_entry, &parentclass->ancestors) {
		ancestor = ancestor_entry->value;
		do {
			from_virtual = ancestor->from_virtual | parent->is_virtual;
			new_ancestor = add_ancestor(parser, class, from_virtual,
				ancestor->parent, parentclass->name,
				parent->is_virtual ? "->" : ".", ancestor->path);
			if (new_ancestor == NULL)
				return;

			map_ancestor_templ_pars(parser, new_ancestor, ancestor, parent);
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
	psaprintf(&vmt->name, "%s%s%s_vmt",
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

static int vmt_this_needs_offset(struct member *member, struct vmt *vmt)
{
	/* for non-primary, the class does not align to start, so have
	   to use the original defining class and translate pointer in implementation */
	return member->vmt && (member->vmt != vmt ||
		!member->props.from_primary || member->props.from_virtual);
}

static int impl_this_needs_offset(struct member *member)
{
	/* for virtual origin, we cannot translate, so that has to use trampoline */
	return member->vmt && !member->props.from_primary && !member->props.from_virtual;
}

static struct class *get_vmt_this_class(struct member *member, struct vmt *vmt)
{
	return vmt_this_needs_offset(member, vmt) ? member->origin : member->implemented;
}

static struct class *get_impl_this_class(struct member *member)
{
	return impl_this_needs_offset(member) ? member->origin : member->implemented;
}

enum parent_virtual { LITERAL_PARENT, VIRTUAL_PARENT };  /* boolean compatible */

static struct vmt *addvmt(struct parser *parser, struct class *class, struct vmt *parent_vmt,
		struct class *origin, enum parent_virtual parent_virtual)
{
	struct vmt *vmt;

	vmt = pgenzalloc(sizeof(*vmt));
	if (vmt == NULL)
		return NULL;

	vmt->class = class;
	vmt->origin = origin;
	vmt->parent = parent_vmt;
	/* vmt->destructor is initialized in import_parent when copying members */
	/* do not reuse virtual base class' vmt, is inefficient */
	vmt->is_primary = class->vmt == NULL;
	vmt->is_parent_virtual = parent_virtual;
	vmt->from_virtual = parent_virtual;
	flist_add(&class->vmts, vmt, next);
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

	/* find same vmt in this class (same origin) */
	flist_foreach(vmt, &class->vmts, next)
		if (vmt->origin == vmt_origin)
			return vmt;

	return NULL;
}

static void import_parent(struct parser *parser, char *parsepos,
		struct class *class, struct parent *parent)
{
	struct member *parentmember, *member;
	struct class *origin, *currorigin, *ignoreorigin, *intforigin, *mergeorigin;
	struct class *vmtorigin, *parentclass = parent->class;
	struct vmt *vmt, *parentvmt, *alternate;
	char *sec_name;
	int ret;

	if (parentclass->is_final)
		pr_err(parsepos, "cannot inherit from final class %s", parentclass->name);

	/* inherit vmts */
	flist_foreach(parentvmt, &parentclass->vmts, next) {
		vmtorigin = parentvmt->origin;
		sec_name = vmtorigin->name;
		alternate = NULL;
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
			/* duplicate vmt has (at least) different offset to class, but
			   possibly also different/new members, add new vmt with same origin */
			alternate = vmt;
			/* use parent name instead of origin name, is clearer */
			sec_name = parentclass->name;
		}
		vmt = addvmt(parser, class, parentvmt, parentvmt->origin, parent->is_virtual);
		vmt->alternate = alternate;
		vmt->sec_name = sec_name;
		parentvmt->child = vmt;
	}

	currorigin = ignoreorigin = intforigin = mergeorigin = NULL;
	flist_foreach(parentmember, &parentclass->members_list, next) {
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
		if (parentmember->is_constructor && !class->constructor) {
			class->constructor = member;
			class->void_constructor = parentclass->void_constructor;
			class->void_root_constructor = parentclass->void_root_constructor;
		} else if (parentmember->is_destructor) {
			/* parse_struct detects changing destructors */
			class->destructor = member;
		}
		/* point to a vmt for this class */
		if (member->vmt) {
			if (member->vmt->child == NULL)
				pr_err(parsepos, "(ierr) parent's vmt does not point to ours?");
			else {
				if (parentmember == member->vmt->destructor) {
					member->vmt->child->destructor = member;
					/* merge destructor implementation, to
					   trigger trampoline creation for alternates */
					member->vmt_impl = class->vmt;
				} else {
					member->vmt_impl = member->vmt->child;
				}
				member->vmt = member->vmt->child;
			}
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

	/* count dynamic castable parents */
	if (!flist_empty(&parentclass->vmts) && !parentclass->no_dyncast) {
		flist_add(&class->dync_parents, parent, next_dync);
	} else if (class->ancestors.num_entries == 0)
		class->need_dync_p0_dummy = 1;

	/* add parents after vmt duplication check */
	add_ancestors(parser, class, parent);
	blist_add(&parentclass->descendants, parent, next_desc);
}

static void implmember(struct class *class, struct member *member)
{
	member->implemented = class;
	member->vmt->modified = member->vmt;
	if (member->props.is_abstract) {
		member->props.is_abstract = 0;
		class->num_abstract--;
	}
}

static void impldestructor(struct class *class)
{
	struct vmt *vmt;

	/* make sure to mark vmts that have destructor as modified to point to us */
	class->freer_class = class;
	flist_foreach(vmt, &class->vmts, next) {
		if (vmt->destructor) {
			if (vmt == class->vmt) {
				vmt->destructor->definition = class;
				class->destructor = vmt->destructor;
			}
			implmember(class, vmt->destructor);
			vmt->destructor->implname = class->name;
		}
	}
}

static void implmembername(struct parser *parser, char *parsepos,
		struct class *class, char *membername, char *nameend, struct memberprops props)
{
	struct member *member;

	/* special case for destructor because name varies (== ~classname) */
	if (membername[0] == '~') {
		if (strcmp(membername+1, class->name) != 0) {
			pr_err(membername, "destructor must be named ~%s", class->name);
			return;
		}
		return impldestructor(class);
	}

	member = find_member_e(class, membername, nameend);
	if (member == NULL) {
		pr_err(parsepos, "cannot find member to override");
		return;
	}

	if (!member->props.is_virtual) {
		pr_err(parsepos, "inherited member %s::%s is not virtual",
			member->origin->name, member->name);
		return;
	}

	if (props.visibility != member->props.visibility) {
		if (props.visibility > member->props.visibility) {
			pr_err(parsepos, "cannot decrease visibility of inherited "
				"member %s::%s", member->visi_define->name, member->name);
			return;
		}
		member->props.visibility = props.visibility;
		member->visi_define = class;
	}

	/* this class defines a new body for this function name
	   note that destructors don't do this (their name is different) */
	member->definition = class;
	implmember(class, member);
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
	return !member->props.is_function && to_lit_class(typ(member->rettype))
		&& ptrlvl(member->rettype) == 0 && member->origin == class;
}

static int member_needs_init(struct class *class, struct member *member)
{
	return is_member_lit_var(class, member)
		&& typ(member->rettype)->u.class->root_constructor;
}

static int member_needs_dispose(struct class *class, struct member *member)
{
	return is_member_lit_var(class, member)
		&& typ(member->rettype)->u.class->destructor;
}

static struct rootclass *addrootclass(struct parser *parser, struct class *parentclass)
{
	struct rootclass *class;
	int namelen = strlen(parentclass->name) + 6;  /* "_root\0" */

	class = pgenzalloc(offsetof(struct rootclass, class.name) + namelen);
	if (class == NULL)
		return NULL;

	sprintf(class->class.name, "%s_root", parentclass->name);
	class->parent.class = parentclass;
	class->parent.child = &class->class;
	class->parent.is_primary = 1;
	class->class.is_rootclass = 1;
	return init_class(parser, &class->class) ? class : NULL;
}

static struct class *open_root_class(struct parser *parser, struct class *class)
{
	char *name = class->name;
	struct class *rootclass;

	switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
	outprintf(parser, "struct %s_root {\n\tstruct %s %s;\n", name, name, name);
	/* 2 lines here, 1 for closing '}' */
	parser->pf.lines_coo += 3;
	class->rootclass = addrootclass(parser, class);
	if (class->rootclass == NULL)
		return NULL;

	rootclass = &class->rootclass->class;
	/* copy root constructor for initialization stack var */
	rootclass->root_constructor = class->root_constructor;
	rootclass->void_root_constructor = class->void_root_constructor;
	return rootclass;
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
	if (class->refcounted)
		rootclass = open_root_class(parser, class);
	hasho_foreach(entry, &class->ancestors) {
		ancestor = entry->value;
		if (ancestor->parent->is_virtual) {
			if (rootclass == NULL) {
				rootclass = open_root_class(parser, class);
				if (rootclass == NULL)
					return;
			}

			parent = pgenzalloc(sizeof(*parent));
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
		if (class->refcounted) {
			outputs(parser, "\tcoo_atomic_t refcount;\n");
			parser->pf.lines_coo++;
		}
		outputs(parser, "};\n");
		/* newline for lines_coo counted above */
	}
}

static void include_coortl(struct parser *parser)
{
	if (parser->pf.coo_rtl_included)
		return;

	switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
	outputs(parser, "#include <coortl.h>\n");
	parser->pf.coo_rtl_included = 1;
	parser->pf.lines_coo++;
}

static int is_vmt_member(struct member *member, struct vmt *vmt)
{
	if (member->vmt == NULL)
		return 0;
	if (member->vmt == vmt)
		return 1;
	/* members might also be pointing to our alternate vmt, if they
	   are not, then they are definitely also not a member of this vmt */
	if (member->vmt != vmt->alternate)
		return 0;
	/* ... however, the alternate vmt might have been expanded with more
	   members than this vmt, so confirm this member is part of this
	   specific vmt's inheritance (so via its parent) */
	return hasho_find(&vmt->parent->class->ancestors, member->origin) != NULL;
}

/* also cleans up vmt child pointers, only accurate during class definition */
static void print_vmt_type(struct parser *parser, struct class *class)
{
	struct class *thisclass;
	struct member *member;
	struct vmt *vmt;

	flist_foreach(vmt, &class->vmts, next) {
		if (vmt->parent)
			vmt->parent->child = NULL;
		if (vmt->modified != vmt)
			continue;

		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		include_coortl(parser);
		outprintf(parser, "extern const struct %s {\n"
			"\tstruct coo_vmt vmt_base;\n", get_vmt_name(parser, vmt));
		/* 2 lines here, 2 to close struct */
		parser->pf.lines_coo += 4;
		flist_foreach(member, &class->members_list, next) {
			if (!is_vmt_member(member, vmt))
				continue;

			thisclass = get_vmt_this_class(member, vmt);
			outprintf(parser, "\t%s(*%s%s)(struct %s *this%s%s;\n",
				member->rettypestr, member->implprefix, member->implname,
				thisclass->name, member->paramstext[0] != ')' ? ", " : "",
				member->paramstext);
			parser->pf.lines_coo++;
		}
		outprintf(parser, "} %s;\n\n", get_vmt_name(parser, vmt));
	}
}

static void print_coo_class_var(struct parser *parser, struct class *class)
{
	if (flist_empty(&class->vmts) || class->no_dyncast)
		return;

	switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
	outprintf(parser, "extern const struct %s_coo_class %s_coo_class;\n\n",
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
		if (!vmt->alternate) {
			ancestor = hasho_find(&vmt->class->ancestors, vmt->origin);
			if (ancestor) {
				vmt->path = ancestor->path;
				vmt->access = ancestor->parent->is_virtual ? "->" : ".";
			} else {
				vmt->path = vmt->access = "";
			}
		} else {
			/* alternate only happens if there is a parent */
			parent = vmt->parent;
			get_vmt_path(parser, parent, &path, &vmt->access);
			psaprintf(&vmt->path, "%s%s%s", parent->class->name,
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
	outprintf(parser, "%s%s_%s%s(struct %s%s *this%s%s;\n",
		member->rettypestr, class->name, member->implprefix, member->implname,
		thisclass->name, rootclass, empty ? "" : ", ", member->paramstext);
	parser->pf.lines_coo++;
}

static void print_member_decls(struct parser *parser, struct class *class)
{
	struct class *freer_class, *rootclass;
	struct ancestor *ancestor;
	struct member *member;
	char *params, *sep, *ret_pre, *ret_type, *ret_post;

	if (class->root_constructor) {
		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		params = class->root_constructor->paramstext;
		params = params[0] == ')' ? "void)" : params;
		outprintf(parser, "struct %s *new_%s(%s;\n",
			class->name, class->name, params);
		parser->pf.lines_coo++;
		if (class->zeroinit) {
			params = class->root_constructor->paramstext;
			sep = params[0] == ')' ? "" : ", ";
			if (class->void_root_constructor) {
				ret_pre = ret_post = "";
				ret_type = "void ";
			} else {
				ret_pre = "struct ";
				ret_type = class->name;
				ret_post = " *";
			}
			if (class->rootclass)
				rootclass = &class->rootclass->class;
			else
				rootclass = class;
			outprintf(parser, "%s%s%s%s_%s_zi(struct %s *this%s%s;\n",
				ret_pre, ret_type, ret_post, class->name,
				class->root_constructor->name, rootclass->name, sep, params);
			parser->pf.lines_coo++;
		}
	} else {
		if (class->zeroinit) {
			outprintf(parser, "void %s_%s_zi(struct %s *this);\n",
				class->name, class->name, class->name);
			parser->pf.lines_coo++;
		}
	}
	freer_class = class->freer_class;
	if (freer_class == class) {
		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		outprintf(parser, "void free_%s(struct %s *this);\n",
			class->name, class->name);
		parser->pf.lines_coo++;
	} else if (freer_class) {
		ancestor = hasho_find(&class->ancestors, freer_class);
		if (ancestor) {
			switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
			outprintf(parser, "#define free_%s(this) free_%s(%s(this)->%s)\n",
				class->name, freer_class->name,
				ancestor->parent->is_virtual ? "" : "&", ancestor->path);
			parser->pf.lines_coo++;
		}
	}

	flist_foreach(member, &class->members_list, next) {
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
			outprintf(parser, "extern %s%s_%s;\n",
				member->rettypestr, class->name, member->name);
			parser->pf.lines_coo++;
		}
	}
}

static char *parse_templpars(struct parser *parser, struct class *class, char *next)
{
	char *name, *nameend, *implstr, sep;
	struct templpar *tp;

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
		tp = addtemplpar(parser, class, name, nameend);
		if (tp == NULL)
			return NULL;
		if (*next == ':') {
			parser->pf.defined_tp_impl = 1;
			implstr = skip_whitespace(parser, next+1);
			if (!iswordstart(*implstr)) {
				pr_err(name, "expected a type");
				goto nextpar;
			}
			next = parse_type(parser, &parser->global_mem,
					class, implstr, &tp->impl, NULL);
			if (typ(tp->impl)->type != AT_CLASS) {
				pr_err(implstr, "expected a class type");
				goto nextpar;
			}
			if (ptrlvl(tp->impl) != 1) {
				pr_err(implstr, "expected single pointer type");
				goto nextpar;
			}
			psaprintf(&tp->implstr, "struct %s *", typ(tp->impl)->u.class->name);
		}

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
	struct class *class, *parentclass, *varclass;
	struct memberprops memberprops = { 0 };
	char *declbegin, *retend, *membername, *nameend, *params, *declend, *next;
	char *classname, *classnameend, *parentname, *prevnext;
	int level, parent_primary, parent_virtual, is_lit_var, len, prevdeclend_lineno;
	int first_virtual_warn, first_vmt_warn, need_destructor, have_retbase;
	int is_coo_class, parent_zeroinit, has_vars, flushdecl_lineno, last_linestart_lineno;
	struct classtype *parentclasstype, *classtype, *defclasstype;
	struct parent *parent, *firstparent;
	const struct anytype *retbasetype;
	struct member *member, *prev_destructor;
	char namebuf[128], *rettypestr, *last_linestart, *indent_start, *indent_end;
	unsigned numexp, extra_pointerlevel;
	new_insert_cb insert_handler;
	anyptr rettype;

	/* skip "struct " */
	classname = strskip_whitespace(parser->pf.pos + 7);
	next = classnameend = skip_word(classname);
	if (classnameend != classname) {
		class = addclass(parser, classname, classnameend);
		if (!class)
			return NULL;
		/* mimic C++, class names are also types */
		classtype = addclasstype(parser, classname, classnameend);
		if (classtype == NULL)
			return NULL;
		classtype->t.type = AT_CLASS;
		classtype->t.u.class = class;
		classtype->t.implicit = 1;
		class->is_final = parser->saw.k.final;
		class->no_dyncast = parser->saw.k.nodyncast;
		/* check for template */
		if (*next == '<')
			if ((next = parse_templpars(parser, class, next)) == NULL)
				return NULL;
	} else
		class = NULL;

	is_coo_class = parent_zeroinit = has_vars = need_destructor = 0;
	rettype = NULL, retbasetype = NULL;
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
			if (parentclasstype->t.pointerlevel) {
				pr_err(parentname, "parent type cannot be pointer type");
				goto nextparent;
			}

			parent = pgenzalloc(sizeof(*parent));
			if (parent == NULL)
				break;

			parentclass = parentclasstype->t.u.class;
			parent->class = parentclass;
			parent->child = class;
			parent->is_primary = parent_primary;
			parent->is_virtual = parent_virtual;
			if (parentclass->refcounted) {
				if (!parser->saw.k.refcount) {
					pr_err(parentname, "class %s must be refcounted "
						"because parent %s is refcounted",
						class->name, parentclass->name);
				}
			}
			if (parentclass->zeroinit) {
				if (parser->saw.k.nozeroinit)
					pr_err(parentname, "class %s must be zeroinit "
						"because parent %s is zeroinit",
						class->name, parentclass->name);
				parent_zeroinit = 1;
			}

			/* check template types to be mapped */
			next = skip_whitespace(parser, nameend);
			if (*next == '<')
				if ((next = parse_templargs(parser, &parser->global_mem,
					class, parentclass, next, &parent->templ_map)) == NULL)
					return NULL;

			numexp = parentclass->templpars.num_entries;
			if (parent->templ_map.num != numexp)
				pr_err(nameend, "expected %u template argument%s for "
					"class %s, got %u", numexp, numexp == 1 ? "" : "s",
					parentclass->name, parent->templ_map.num);

			class->need_root_constructor |= parentclass->need_root_constructor;
			class->need_root_destructor = parentclass->need_root_destructor ||
				(parent_virtual && parentclass->has_destructor);
			if (parent_primary)
				class->freer_class = parentclass->freer_class;
			if (!class->destructor) {
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

			prev_destructor = class->destructor;
			import_parent(parser, parentname, class, parent);
			/* if there are multiple (unique) parent destructors,
			   then this class needs its own destructor */
			need_destructor |= prev_destructor && class->destructor != prev_destructor;
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
	declbegin = retend = indent_start = indent_end = NULL;
	membername = next = openbrace;  /* make compiler happy */
	flushdecl_lineno = 0;
	for (;;) {
		/* search (sub)struct or end of variable or function prototype */
		prevnext = next + 1;
		if (declbegin == NULL)
			prevdeclend_lineno = parser->pf.lineno;
		next = skip_whitespace(parser, prevnext);
		if (declbegin == NULL) {
			/* last linestart is the last linestart in front of this
			   declaration, if no newline in between then end of last decl
			   note that pf.linestart has offset 1 for messages, column 1 */
			last_linestart = parser->pf.linestart + 1;
			last_linestart_lineno = parser->pf.lineno;
			if (last_linestart < prevnext) {
				last_linestart = prevnext;
				last_linestart_lineno = prevdeclend_lineno;
			} else {
				indent_start = last_linestart;
				indent_end = next;
			}
			declbegin = next;
			have_retbase = 0;
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
			if (!flushdecl_lineno) {
				flush_until(parser, last_linestart);
				flushdecl_lineno = last_linestart_lineno;
			}
			is_coo_class = 1;
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
		extra_pointerlevel = 0;

		/* skip pointers in membername */
		for (;; membername++) {
			/* is it a pointer to function variable? */
			if (*membername == '(') {
				retend = retend ?: membername;
				memberprops.is_function = 0;
				/* find real params */
				membername++;
				/* remember in case of later var in a list */
				extra_pointerlevel++;
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
		is_coo_class |= memberprops.is_static
			| memberprops.is_virtual | memberprops.is_function;
		if (memberprops.is_virtual && !memberprops.is_function) {
			pr_err(membername, "member variable cannot be virtual");
			continue;
		}

		/* do not print functions or static variables inside the struct */
		if (memberprops.is_function || memberprops.is_static) {
			insert_handler = save_type_insert;
			rettypestr = NULL;  /* to be constructed */
			if (!flushdecl_lineno) {
				flush_until(parser, last_linestart);
				flushdecl_lineno = last_linestart_lineno;
			}
			/* force emit new line pragma when switching back */
			parser->pf.line_pragma_mode = LINE_PRAGMA_INVALID;
		} else {
			insert_handler = print_type_insert;
			rettypestr = declbegin;
			if (flushdecl_lineno) {
				parser->pf.writepos = last_linestart;
				parser->pf.lines_coo -= parser->pf.lineno - flushdecl_lineno;
				flushdecl_lineno = 0;
			}
			switch_line_pragma(parser, LINE_PRAGMA_INPUT);
		}

		/* no need to check virtual and static because cannot both at declbegin */
		if (memberprops.is_virtual) {
			/* new members need a primary vmt to put them in */
			if (!memberprops.is_override && !class->vmt) {
				class->need_root_constructor = 1;
				addvmt(parser, class, NULL, class, LITERAL_PARENT);
				outwrite(parser, indent_start, indent_end - indent_start);
				outputs(parser, "const struct coo_vmt *vmt;\n");
				parser->pf.lines_coo++;
			}
			declbegin += 8;  /* skip "virtual " */
		}
		if (memberprops.is_override)
			declbegin++;     /* skip "override " (diff with "virtual ") */
		if (memberprops.is_static)
			declbegin += 7;  /* skip "static " */
		if (!memberprops.is_function)
			has_vars = 1;

		next = declend;
		if (memberprops.is_override) {
			/* cannot add member, should have inherited already from parent */
			implmembername(parser, membername, class,
				membername, nameend, memberprops);
			if (declbegin != membername)
				pr_err(membername, "membername must follow override");
			if (params)
				pr_err(params, "no parameters allowed for override");
		} else {
			/* first time parsing this return type? == first var in list? */
			if (!have_retbase) {
				/* for constructor, parse_type detects rettype wrong */
				if (retend > declbegin) {
					parse_type(parser, &parser->global_mem, class,
						declbegin, &rettype, insert_handler);
					if (rettypestr == NULL)
						rettypestr = print_type_inserts(parser,
							&parser->global_mem, declbegin, retend);
				} else {
					rettype = unknown_typeptr, rettypestr = NULL;
				}
				/* the basetype should not include local '*' */
				retbasetype = typ(rettype);
			} else {
				if (retbasetype->type == AT_TEMPLPAR) {
					parser->pf.pos = membername;
					flush(parser);
					outwrite(parser, "*****",
						ptrlvl(retbasetype->u.tp->impl));
				}
				rettype = to_anyptr(retbasetype, extra_pointerlevel);
			}
			have_retbase = *declend == ',';
			varclass = to_lit_class(retbasetype);
			is_lit_var = ptrlvl(rettype) == 0 && varclass;
			if (is_lit_var && retbasetype->u.class->num_abstract) {
				pr_err(membername, "cannot instantiate abstract "
					"class %s", retbasetype->u.class->name);
			} else {
				member = addmember(parser, class, rettype,
					rettypestr, membername, nameend,
					params, memberprops);
				if (member && is_lit_var) {
					if (retbasetype->u.class->root_constructor)
						class->num_init_vars++;
					if (retbasetype->u.class->destructor) {
						class->num_destr_vars++;
						need_destructor = 1;
					}
					if (varclass->zeroinit && parser->saw.k.nozeroinit)
						pr_err(membername, "class %s must be zeroinit "
							"because member %s is zeroinit",
							class->name, member->name);
				}
			}
		}

		if (*declend == ';')
			declbegin = retend = NULL;
	}

	if (flushdecl_lineno) {
		parser->pf.writepos = last_linestart;
		parser->pf.lines_coo -= parser->pf.lineno - flushdecl_lineno;
	}

	if (!class)
		goto skip_noclass;

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
	class->refcounted = parser->saw.k.refcount;
	if (!class->num_abstract && class->need_root_constructor && !class->root_constructor) {
		/* copy user's constructor signature if defined */
		len = snprintf(namebuf, sizeof(namebuf), "%s_root", class->name);
		addgenmember(parser, class, class->constructor, namebuf, namebuf+len);
		class->gen_root_constructor = 1;
	}
	/* add destructor if there are literal class variables with destructor */
	if (need_destructor && !class->has_destructor) {
		if (class->vmt && class->destructor) {
			impldestructor(class);
		} else {
			class->gen_destructor = '~';  /* &name[-1] == &gen_destructor */
			addgenmember(parser, class, NULL, &class->gen_destructor,
				&class->name[classnameend-classname]);
		}
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
		flist_foreach(member, &class->members_list, next) {
			if (member->definition == NULL) {
				pr_err(next, "duplicate inheritance, need override "
					"for member %s", member->name);
				/* define here anyway to prevent segfaults */
				member->definition = class;
			}
		}
	}

	/* determine automatic zero-initialization */
	class->zeroinit = parser->saw.k.zeroinit || parent_zeroinit
		|| (is_coo_class && has_vars && !parser->saw.k.nozeroinit);

	/* check for typedef struct X {} Y, *Z; */
	class->declare_complete = 1;
skip_noclass:
	for (;;) {
		extra_pointerlevel = 0;
		for (classname = skip_whitespace(parser, next + 1); *classname == '*'; classname++)
			extra_pointerlevel++;
		next = skip_word(classname);
		declend = scan_token(parser, next, "/\n,;");
		if (declend == NULL)
			return NULL;
		if (next != classname) {
			if (class && parser->saw.k.typdef) {
				defclasstype = addclasstype(parser, classname, next);
				if (defclasstype) {
					defclasstype->t.type = AT_CLASS;
					defclasstype->t.u.class = class;
					defclasstype->t.implicit = 1;
					defclasstype->t.pointerlevel = extra_pointerlevel;
				} else {
					pr_warn(classname, "class type already exists");
				}
			} else {
				/* TODO: addglobal() */
			}
		}
		if (*declend == ';')
			break;
		next = declend;
	}
	/* skip till next declaration */
	parser->pf.pos = skip_whitespace(parser, declend + 1);
	flush(parser);
	if (!class)
		return NULL;

	if (parser->pf.defined_tp_impl)
		include_coortl(parser);
	print_root_classes(parser, class);
	print_vmt_type(parser, class);
	print_coo_class_var(parser, class);
	print_member_decls(parser, class);
	switch_line_pragma(parser, LINE_PRAGMA_INPUT);
	return class;
}

static void print_inserts_fromto(struct parser *parser, char *from, char *until)
{
	insert_dlist_t *inserts = parser->inserts;
	struct insert *insert, *first, *last;

	first = NULL;
	parser->pf.writepos = from;
	dlist_foreach(insert, inserts, item) {
		if (from > insert->flush_until)
			continue;
		if (until < insert->flush_until)
			break;
		first = first ?: insert;
		flush_until(parser, insert->flush_until);
		outputs(parser, insert->insert_text);
		parser->pf.writepos = insert->continue_at;
		last = insert;
	}
	if (first) {
		dlist_remove_section(first, last, item);
		blist_add_section(&parser->inserts_free, first, last, item.inext);
	}
	parser->pf.pos = until;
	flush(parser);
}

static struct ancestor *accessancestor(anyptr expr, char *exprstart,
		char *name, struct class *target, char **pre, char **post)
	/* assumes expr->type == AT_CLASS */
{
	struct ancestor *ancestor;

	ancestor = hasho_find(&typ(expr)->u.class->ancestors, target);
	if (ancestor == NULL)
		return NULL;

	/* check if need to add parentheses to surround & ... -> */
	if (exprstart != NULL && exprstart != name) {
		*pre = !ancestor->parent->is_virtual ? "&(" : "(";
		*post = ptrlvl(expr) ? ")->" : ").";
	} else {
		*pre = !ancestor->parent->is_virtual ? "&" : "";
		*post = ptrlvl(expr) ? "->" : ".";
	}
	return ancestor;
}

static void parse_method(struct parser *parser, char *stmtstart, struct methodptr *targetmp,
		char *exprstart, char *exprend, anyptr expr, struct class *tgtclass,
		char *name, char *nameend, struct member *member)
	/* assumes expr->type == AT_CLASS */
{
	char *maybe_this, *newscope, *sep_or_end, *params, *vmtpath, *vmtaccess;
	char *pre, *post, *ancpath, *funcsource;
	struct class *memberdef = member->definition;
	struct ancestor *ancestor;
	int scope_varnr, fslen;

	if (targetmp == NULL) {
		pr_err(exprstart, "did not detect target method pointer type");
		return;
	}
	if (typ(expr)->u.class != memberdef) {
		ancestor = accessancestor(expr, exprstart, name, memberdef, &pre, &post);
		if (ancestor == NULL)
			return;
		ancpath = ancestor->path;
	} else {
		pre = ptrlvl(expr) == 0 ? "&" : "";
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
		fslen = aaprintf(&parser->func_mem, &funcsource,
				"((struct %s_vmt*)coo_obj%d->%s%svmt)->%s", memberdef->name,
				scope_varnr, vmtpath, vmtaccess, member->name);
	} else  {
		tgtclass = memberdef;
	  hardref:
		fslen = aaprintf(&parser->func_mem, &funcsource,
				"%s_%s", tgtclass->name, member->name);
	}
	if (fslen < 0)
		return;
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
	/* free funcsource from heap end before allocating new insert item/text */
	afree(&parser->func_mem, funcsource, fslen+1);
	/* replace exprstart..(exprend..name)..nameend with newly defined coo_mv var */
	addinsert_format(parser, NULL, exprstart, nameend, CONTINUE_AFTER,
		"coo_mv%u", scope_varnr);
}

static void map_template_par(struct parser *parser, char *pos,
		anyptr expr, anyptr src, anyptr *target)
{
	if (typ(src)->type == AT_TEMPLPAR && typ(expr)->type == AT_TEMPLINST) {
		if (typ(src)->u.tp->index >= typ(expr)->args.num) {
			pr_err(pos, "(ierr) tp index %u out of range %u",
				typ(src)->u.tp->index, typ(expr)->args.num);
			*target = NULL;
		} else {
			/* TODO: fail on error? */
			*target = translate_tp(&parser->func_mem, &typ(expr)->args, src);
		}
	} else
		*target = src;
}

static void cast_tp_expr(struct parser *parser, char *exprstart, char *exprend,
		const struct anytype *expr)
{
	struct class *tpclass, *class = to_class(expr);
	struct ancestor *ancestor;
	anyptr tpdecl;

	if (!expr->from_tp)
		return;

	/* cast from template parameter declared type to implementation type */
	tpdecl = get_from_tpdecl(expr);
	/* inserts might not be consecutive, surround e.g. (this->)expr */
	if (tpdecl == NULL || typ(tpdecl)->type == AT_UNKNOWN) {
		/* declared type is void *, so use direct cast */
		addinsert_format(parser, NULL, exprstart, exprstart,
			CONTINUE_BEFORE, "((struct %s*)", class->name);
		addinsert(parser, NULL, exprend, ")", exprend, CONTINUE_AFTER);
	} else if ((tpclass = to_class(typ(tpdecl))) == class) {
		/* nothing to do, already the right class */
	} else if ((ancestor = hasho_find(&class->ancestors, tpclass))) {
		/* declared type is a class, let compiler calculate offset */
		addinsert(parser, NULL, exprstart,
			"container_of(", exprstart, CONTINUE_BEFORE);
		addinsert_format(parser, NULL, exprend, exprend, CONTINUE_AFTER,
			", struct %s, %s)", class->name, ancestor->path);
	} else {
		pr_err(exprstart, "(ierr) template parameter class '%s' is "
			"not an ancestor of '%s'?", tpclass->name, class->name);
	}
}

static struct member *parse_member(struct parser *parser, char *stmtstart,
	struct methodptr *targetmp, int expr_is_oneword, char *exprstart, char *exprend,
	anyptr expr, char *name, char *nameend, anyptr *retdecl, struct dynarr **retparams)
{
	struct member *member;
	struct class *class = to_class(typ(expr));
	char *args, *flush_until, *insert_text, *continue_at;
	char *newscope, insert_buf[16], *vmtpath, *vmtaccess;
	enum insert_continue insert_continue;
	int add_this, has_arguments, scope_varnr;
	struct insert *insert_before;

	if (class == NULL)
		return NULL;
	if ((member = find_member_e(class, name, nameend)) == NULL) {
		/* without expr, looking up any name in 'this' causes many false positives */
		if (exprstart)
			pr_err(name, "cannot find member in class '%s'", class->name);
		return NULL;
	}

	if (exprstart) {
		if (member->props.is_static) {
			pr_err(name, "cannot access static member %s::%s",
				member->origin->name, member->name);
			return NULL;
		}
		/* if expr is template, cast declared to implemented type */
		cast_tp_expr(parser, exprstart, exprend, typ(expr));
	}

	insert_before = NULL;
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
					ptrlvl(expr) == 0 ? "&" : "");
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
			insert_before = addinsert_format(parser, insert_before,
				flush_until, continue_at, CONTINUE_BEFORE,
				"((struct %s_vmt*)%s",
				member->origin->name, insert_text);
			/* output expression + vmt path + name */
			get_vmt_path(parser, member->vmt, &vmtpath, &vmtaccess);
			insert_before = addinsert_format(parser, insert_before,
				name, name, CONTINUE_BEFORE,
				"%s%svmt)->", vmtpath, vmtaccess);
		} else {
			insert_before = addinsert(parser, insert_before, exprstart ?: name,
				get_class_funcinsert(parser, member->definition),
				name, CONTINUE_BEFORE);
		}

		continue_at = ++args;
		has_arguments = *strskip_whitespace(args) != ')';
		if (add_this) {
			flush_until = args;
			if (member->parentname) {
				insert_before = addinsert(parser, insert_before, args,
					member->parent_virtual ? "this->" : "&this->",
					args, CONTINUE_AFTER);
				if (has_arguments) {
					insert_before = addinsert(parser, insert_before,
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
				addinsert(parser, insert_before, args,
					( member->parentname && !member->parent_virtual) ||
					(!member->parentname && ptrlvl(expr) == 0)
					? "&" : "", exprstart, CONTINUE_BEFORE);
				/* continue at end */
				insert_before = dlist_head(parser->inserts, item);
				if (member->parentname) {
					insert_before = addinsert(parser, insert_before,
						exprend, ptrlvl(expr) ? "->" : ".",
						args, CONTINUE_AFTER);
					/* reuse empty insert_text if possible */
					if (insert_text[0] != 0)
						insert_before = addinsert(parser, insert_before,
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
			insert_before = addinsert(parser, insert_before,
				name, "this->", name, CONTINUE_BEFORE);
		else
			insert_text = "this->";
	} else if ((insert_text = get_member_nameinsert(parser, class, member)) != NULL) {
		/* static variables need to skip expression/variable name, is a global */
		flush_until = member->props.is_static && exprstart ? exprstart : name;
		continue_at = name;
	}

	if (continue_at)
		addinsert(parser, insert_before,
			flush_until, insert_text, continue_at, insert_continue);

out:
	map_template_par(parser, name, expr, member->rettype, retdecl);
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
	struct insert *insert_before;
	char *thistext, *thispos;
	anyptr expr;

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
		expr = to_anyptr(&thisclass->t, 1);
		/* pass accinhname as 'name', consider whole X::func to be
		   name..nameend so that the X:: part is also replaced */
		parse_method(parser, stmtstart, targetmp, NULL, NULL,
			expr, tgtclass, accinhname, next, member);
		return next;
	}

	addinsert(parser, NULL, colons, "_", colons+2, CONTINUE_AFTER);
	thispos = next + 1;
	insert_before = addinsert(parser, NULL, thispos, thistext, thispos, CONTINUE_BEFORE);
	if (ancestor)
		insert_before = addinsert(parser, insert_before,
			thispos, ancestor->path, thispos, CONTINUE_BEFORE);
	if (member->paramstext[0] != ')')
		addinsert(parser, insert_before, thispos, ", ", thispos, CONTINUE_BEFORE);

	*retparams = &member->params;
	return next;
}

static char *insertmpcall(struct parser *parser, int expr_is_oneword, char *stmtstart,
		char *exprstart, char *exprend, struct methodptr *targetmp,
		char *parenpos, int pointerlevel)
{
	char *arrow, *newscope, *sep_or_end, *next;
	struct insert *insert_before;
	int scope_varnr;

	/* look ahead to see if there arguments, then need comma separator
	   between "this" parameter and rest of user arguments */
	next = skip_whitespace(parser, parenpos+1);
	sep_or_end = *next != ')' ? ", " : "";
	if (expr_is_oneword) {
		arrow = pointerlevel ? "->" : ".";
		insert_before = addinsert_format(parser, NULL,
			parenpos, exprstart, CONTINUE_AFTER, "%sfunc(", arrow);
		addinsert_format(parser, insert_before,
			parenpos, parenpos+1, CONTINUE_AFTER, "%sobj%s", arrow, sep_or_end);
	} else {
		newscope = open_tempscope(parser, &scope_varnr);
		parser->pf.pos = stmtstart;
		flush(parser);
		outprintf(parser, "%s%s coo_mv%d = ", newscope, targetmp->name, scope_varnr);
		outwrite(parser, exprstart, exprend - exprstart);
		/* temp.var has been defined, now use it to replace expression */
		addinsert_format(parser, NULL, exprstart, parenpos+1, CONTINUE_AFTER,
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
		anyptr rettype, char *rettypestr, char *next, char *declend)
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
	if (typ(rettype) && typ(rettype)->implicit) {
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
	struct classtype *classtype;
	char *next, *typestr;
	anyptr typeptr;

	/* check if a class is aliased to a new name */
	typestr = parser->pf.pos;
	next = parse_type(parser, &parser->global_mem, NULL, typestr, &typeptr, print_type_insert);
	if (strprefixcmp("(*::", next)) {
		parse_methodptr_typedef(parser, typeptr, typestr, next, declend);
		return;
	}

	if (!to_class(typ(typeptr)) && !to_mp(typ(typeptr)))
		return;

	/* note: after next cannot be '{', as in 'typedef struct X {'
	   it is handled by calling parse_struct in caller */
	parser->pf.pos = next;
	next = skip_word(parser->pf.pos);
	if (to_class(typ(typeptr))) {
		classtype = addclasstype(parser, parser->pf.pos, next);
		if (classtype)
			init_anytype(&classtype->t, typeptr);
	} else  /* mptype, TODO: do something with type.decl.pointerlevel! */
		dupmethodptrtype(parser, parser->pf.pos, next, to_mp(typ(typeptr)));
	parser->pf.pos = declend + 1;
}

static void print_inserts(struct parser *parser, insert_dlist_t *inserts)
{
	struct insert *insert;

	if (dlist_empty(inserts, item))
		return;
	dlist_foreach(insert, inserts, item) {
		flush_until(parser, insert->flush_until);
		outputs(parser, insert->insert_text);
		parser->pf.writepos = insert->continue_at;
	}
	blist_add_section(&parser->inserts_free,
		dlist_first(inserts), dlist_last(inserts), item.inext);
	dlist_init(inserts, item);
}

static void print_initializers(struct parser *parser, char *position, unsigned blocklevel,
		unsigned retblocknr, char *retvartype, char *retvartypeend)
{
	char *linestart, *params, *sep_or_end, *zeroinit_suffix;
	struct initializer *initializer;
	struct ancestor *ancestor;
	struct class *class;
	struct member *member;
	char *ancpath, *ancpath_sep;
	int lineno;

	if (flist_empty(&parser->initializers))
		return;

	/* copy the indentation to our added lines, note parser linestart has
	   offset -1 so that diagnostic messages start counting at column 1 */
	linestart = parser->pf.linestart + 1;
	flush_until(parser, linestart);
	/* define a return variable if needed */
	if (retvartype) {
		outwrite(parser, linestart, position - linestart);
		outwrite(parser, retvartype, retvartypeend - retvartype);
		outwrite(parser, "__coo_ret;\n", 11);
		parser->pf.lines_coo++;
	}
	lineno = 0;
	flist_foreach(initializer, &parser->initializers, next) {
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
			zeroinit_suffix = class->zeroinit ? "_zi" : "";
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
			outprintf(parser, "%s_%s%s(&%s%s%s%s", member->origin->name,
				member->name, zeroinit_suffix, initializer->name,
				ancpath_sep, ancpath, sep_or_end);
			adddisposer(parser, class, initializer->name, blocklevel, retblocknr);
		} else {
			parser->pf.writepos = initializer->name;
		}
		print_inserts(parser, &initializer->inserts);
		flush_until(parser, initializer->end);
		outwrite(parser, ";\n", 2);
	}
	flist_clear(&parser->initializers);
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
	unsigned retblocknr;

	if (blist_empty(&parser->disposers)) {
		/* only return if also no label to print */
		if (next_retblocknr == 0)
			return;
		retblocknr = 0;
		disposer = NULL;
	} else {
		disposer = parser->disposers.rlast;
		if (disposer->blocklevel < blocklevel)
			return;
		retblocknr = disposer->retblocknr;
	}

	/* copy the indentation to our added lines, note parser linestart has
	   offset -1 so that diagnostic messages start counting at column 1 */
	linestart = parser->pf.linestart + 1;
	flush_until(parser, linestart);
	switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
	for (;;) {
		if (next_retblocknr != retblocknr) {
			outprintf(parser, "__coo_out%d:\n", retblocknr);
			parser->pf.lines_coo++;
			next_retblocknr = retblocknr;
			/* in case no disposers, just to print label */
			if (disposer == NULL)
				break;
		}
		outwrite(parser, linestart, position - linestart);
		member = disposer->class->destructor;
		outprintf(parser, "\t%s_%s%s(&%s);\n", member->definition->name,
			member->implprefix, member->implname, disposer->name);
		parser->pf.lines_coo++;
		disposer = disposer->prev;
		if (disposer == NULL)
			break;
		if (disposer->blocklevel < blocklevel)
			break;
		retblocknr = disposer->retblocknr;
	}

	if (blocklevel == 1 && need_retvar) {
		outwrite(parser, linestart, position - linestart);
		outprintf(parser, "\treturn __coo_ret;\n");
		parser->pf.lines_coo++;
	}
	parser->disposers.rlast = disposer;
	parser->pf.writepos = linestart;
	switch_line_pragma(parser, LINE_PRAGMA_INPUT);
}

static void param_to_variable(struct parser *parser, struct allocator *alloc, int position,
		struct class *class, anyptr decl, char *name, char *next)
{	(void)position; (void)alloc;
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

static void insertancestor(struct parser *parser, char *exprstart,
		char *name, char *end, const struct anytype *target, anyptr expr)
	/* assumes expr->type == AT_CLASS, AT_TEMPLINST */
{
	struct ancestor *ancestor;
	struct insert *insert_before = NULL;
	const struct anytype *tpdecl;
	struct class *tpclass;
	char *pre, *post;

	cast_tp_expr(parser, exprstart, end, typ(expr));
	if (typ(expr)->u.class != target->u.class) {
		ancestor = accessancestor(expr, exprstart, name, target->u.class, &pre, &post);
		if (ancestor == NULL) {
			pr_err(exprstart, "incompatible types, expect '%s', got '%s'",
				target->u.class->name, typ(expr)->u.class->name);
			return;
		}
		if (cmp_templ_args(typ(expr), target, ancestor) < 0) {
			pr_err(name, "incompatible template types");
			return;
		}
	} else
		ancestor = NULL;
	/* if target is a template parameter type, then we need to cast to declared
	   type of that parameter, because that's what the C code sees
	   the above check is also necessary to check whether the usage is valid as-is */
	if ((tpdecl = typ(get_from_tpdecl(target))) != NULL && tpdecl->type != AT_UNKNOWN) {
		if ((tpclass = to_class(tpdecl)) == NULL) {
			pr_err(exprstart, "non-void template parameter must be a class");
		} else if (tpclass == typ(expr)->u.class) {
			/* already correct class, nothing to insert */
			return;
		} else if ((ancestor = accessancestor(expr, exprstart,
						name, tpclass, &pre, &post)) == NULL) {
			pr_err(exprstart, "expression class '%s' must descend from template "
				"parameter class '%s'", typ(expr)->u.class->name, tpclass->name);
		}
	}
	if (ancestor == NULL)
		return;

	if (pre[0])
		insert_before = addinsert(parser, insert_before,
			exprstart, pre, exprstart, CONTINUE_BEFORE);
	insert_before = addinsert(parser, insert_before, end, post, end, CONTINUE_BEFORE);
	addinsert(parser, insert_before, end, ancestor->path, end, CONTINUE_BEFORE);
	return;
}

struct paramstate {
	struct dynarr *params;
	unsigned index;
};

static void select_next_param(struct parser *parser, char *pos, anyptr expr,
		struct paramstate *state, anyptr *dest)
{
	*dest = unknown_typeptr;
	state->index++;
	if (state->params && state->index < state->params->num) {
		anyptr source = state->params->mem[state->index];
		if (source)
			map_template_par(parser, pos, expr, source, dest);
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

struct parenlvl_info {
	struct parenlvl_info *prev, *next;     /* to upper/deeper parenthesis levels */
	anyptr exprdecl;                       /* expression type at current level */
	anyptr targetdecl;                     /* context type (assign to or parameter) */
	char *exprstart;                       /* where expression started */
	char *memberstart;                     /* where last word/identifier started */
	struct paramstate targetparams;        /* iterating through parameter types */
	enum dyncast_state dyncast:8;          /* dynamic casting at this level */
	unsigned target_isparam:8;             /* targetdecl is a parameter */
};

static void parse_function(struct parser *parser, char *next)
{
	anyptr immdecl, expr, *p_expr, target, rettype, thisptr;
	struct parenlvl_info *pareninfo, *parenprev, *parencast, *parenlvl0;
	const struct anytype *decl;
	struct anytype *newtype;
	struct dynarr **tgtparams;
	struct templpar *tptype;
	struct classtype *classtype;
	struct class *declclass, *thisclass;
	struct member *member, *submember, *constr;
	struct variable *declvar;
	struct initializer *initializer;
	struct methodptr *mptype, *mp;
	struct insert *insert_before;
	char *curr, *funcname, *nameend, *classname, *name, *str1, *str2, *tmplpos;
	char *exprend, *params, *param0, *paramend;
	char *funcvarname, *funcvarnameend, *stmtstart;
	char *thispath, *thisprefix, *thisclassname, *thisfuncret, *thisfuncretend;
	char *argsep, *dblcolonsep, *accinhname, *accinhcolons, *rettypestr;
	enum parse_funcvar_state funcvarstate;
	enum parse_state state, nextstate;
	enum goto_return goto_ret;
	int is_constructor, expr_is_oneword, blocklevel, seqparen, numwords;
	int have_retvar, need_retvar, used_retvar, eval_retvar;
	int retblocknr, next_retblocknr, unused_retblocknr, in_delete;
	unsigned num_constr_called, num_constr;

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
	thispath = NULL;
	thisclass = NULL;
	thisptr = unknown_typeptr;
	if (funcname) {   /* funcname assigned means there is a classname::funcname */
		if ((thisclass = find_class(parser, classname)) != NULL) {
			flush(parser);
			thisptr = to_anyptr(&thisclass->t, 1);
			/* lookup method name */
			nameend = skip_methodname(funcname);
			member = find_member_e(thisclass, funcname, nameend);
			if (member != NULL) {
				if (member->props.is_virtual
						&& member->definition != thisclass
						&& !thisclass->is_final) {
					pr_warn(funcname, "overriding virtual method without "
						"override in class declaration, is "
						"invisible to descendent classes");
				}
				if (member->definition != thisclass) {
					pr_err(funcname, "function '%s' is defined in '%s'",
						member->name, member->definition->name);
				}
			} else {
				struct memberprops props = {0,};
				/* undeclared, so it's private, include static */
				outputs(parser, "static ");
				/* add as member so others can call from further down */
				parse_type(parser, &parser->func_mem, thisclass,
					thisfuncret, &rettype, save_type_insert);
				props.is_function = 1;
				/* construct proper rettypestr in case used for methodptr */
				rettypestr = print_type_inserts(parser, &parser->global_mem,
					thisfuncret, thisfuncretend);
				member = addmember(parser, thisclass, rettype,
					rettypestr, funcname, nameend, params, props);
				if (member == NULL)
					return;
			}
			thisclass->is_implemented = 1;
			thisclass->gen_constructor &= ~member->is_constructor;
			thisclass->gen_root_constructor &= ~member->is_root_constructor;
			thisclass->gen_destructor &= ~member->is_destructor;
			/* member's rettypestr has correct type for constructor, destructor,
			   implicit struct and translated template parameters so always
			   use that one instead of original text for simplicity */
			outputs(parser, member->rettypestr);
			parser->pf.writepos = classname;
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
			name = "this";
			nameend = name + 4;  /* "this" */
			addvariable(parser, thisclass, 0, thisptr, 0, name, nameend, NULL);
			/* no parents means all are initialized */
			is_constructor = member->is_constructor;
			num_constr = is_constructor
				* (thisclass->num_parent_constr + thisclass->num_init_vars);
		} else {
			pr_err(classname, "class '%s' not declared", classname);
		}
	}

	parser->class = thisclass;
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
	if (parse_parameters(parser, &parser->func_mem, thisclass, params,
			print_type_insert, param_to_variable,
			check_conflict_param_member) == NULL)
		return;

	if (thispath) {
		parser->pf.pos = next;
		flush(parser);
		outprintf(parser, "\tstruct %s *this = container_of(__this, struct %s, %s);",
			classname, classname, thispath);
	}

	/* alloc for 2 parentheses levels, always have a next level to store target type */
	parenlvl0 = pareninfo = fgenzalloc(2*sizeof(*pareninfo));
	if (pareninfo == NULL)
		return;

	seqparen = numwords = 0;
	parencast = parenprev = NULL;
	pareninfo->next = &pareninfo[1], pareninfo[1].prev = pareninfo;
	pareninfo->exprdecl = pareninfo->targetdecl = immdecl = unknown_typeptr;
	have_retvar = need_retvar = used_retvar = eval_retvar = 0;
	stmtstart = exprend = accinhcolons = NULL;
	funcvarname = funcvarnameend = name = NULL;  /* make compiler happy */
	accinhname = tmplpos = NULL;  /* make compiler happy */
	retblocknr = next_retblocknr = unused_retblocknr = in_delete = 0;
	flist_clear(&parser->initializers);
	funcvarstate = FV_NONE;
	goto_ret = GOTO_RET_NONE;
	decl = &unknown_type;
	state = STMTSTART;
	expr_is_oneword = 1;  /* make compiler happy */
	initializer = NULL;
	declvar = NULL;
	member = NULL;
	for (;;) {
		curr = skip_whitespace(parser, next);
		/* skip comments */
		if (curr[0] == 0)
			return;
		/* pending initializers and this looks like statement, then print */
		if (!flist_empty(&parser->initializers) && pareninfo->prev == NULL &&
				(*curr == '=' || *curr == '(' || *curr == '{') && numwords < 2) {
			print_initializers(parser, pareninfo->memberstart, blocklevel, next_retblocknr,
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
		if (!pareninfo->exprstart) {
			pareninfo->exprstart = curr;
			expr_is_oneword = 1;
		}
		if (isdigit(*curr))
			curr = skip_word(curr);
		else if (iswordstart(*curr)) {
			if (!pareninfo->memberstart)
				pareninfo->memberstart = curr;
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
				} else if ((declclass =
						find_class_e(parser, name, next)) != NULL) {
					decl = alloctype(&parser->func_mem, AT_CLASS, declclass);
					goto decl_or_cast;
				}
				continue;
			} else if (strprefixcmp("dyn", curr)) {
				if (strprefixcmp("amic_cast<", curr+3)) {
					if (strprefixcmp(">(", curr+13)) {
						next = curr + 13;
						goto dyncast;
					}
					next = parse_type(parser, &parser->func_mem,
						thisclass, curr+13, &rettype, NULL);
					if (next[0] != '>' || next[1] != '(')
						pr_err(next, "'>' expected");
					next++;
					goto dyncast;
				} else if (curr[3] == ':') {
					next = curr + 4;
					rettype = pareninfo->targetdecl;
				   dyncast:
					if (typ(rettype)->type != AT_CLASS) {
						pr_err(curr, "unknown class to cast to");
						continue;
					}
					if (ptrlvl(rettype) + typ(rettype)->pointerlevel != 1) {
						pr_err(curr, "dynamic cast must be "
							"to single pointer");
						continue;
					}
					if (typ(rettype)->u.class->no_dyncast) {
						pr_err(curr, "target class '%s' is not dynamic "
							"castable", typ(rettype)->u.class->name);
						continue;
					}
					/* skip "dyn:" or "dynamic_cast<>(" */
					insert_before = addinsert(parser, NULL, curr,
						"coo_dyn_cast(&", next, CONTINUE_AFTER);
					insert_before = addinsert(parser, insert_before, next,
						typ(rettype)->u.class->name, next, CONTINUE_AFTER);
					addinsert(parser, insert_before, next,
						"_coo_class, &", next, CONTINUE_AFTER);
					if (*next == '(') {
						pareninfo->next->targetdecl = rettype;
						pareninfo->next->dyncast = DYNCAST_PAREN;
					} else {
						pareninfo->dyncast = DYNCAST_EXPR;
					}
					if (typ(pareninfo->exprdecl)->type == AT_UNKNOWN)
						pareninfo->exprdecl = rettype;
					continue;
				}
			} else if (strprefixcmp("return", curr) && !isalnum(curr[6])) {
				next = curr + 7;  /* "return" */
				if (need_retvar) {
					insert_text(parser, NULL, curr,
						state == STMTSTART ? "__coo_ret = "
							: "{ __coo_ret = ",
						next, CONTINUE_AFTER);
					next_retblocknr = retblocknr + 1;
					goto_ret = state != STMTSTART ? GOTO_RET_BLOCK : GOTO_RET;
				}
				if (parser->disposers.rlast) {
					if (parser->disposers.rlast->blocklevel > 1) {
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
				if (classtype == NULL)
					continue;
				if (classtype->t.pointerlevel) {
					pr_err(name, "'new <type>' cannot be pointer type");
					continue;
				}
				if (classtype->t.u.class->num_abstract) {
					pr_err(name, "cannot instantiate abstract class %s",
						classtype->t.u.class->name);
					continue;
				}

				immdecl = to_anyptr(&classtype->t, 1);
				if (classtype->t.u.class->constructor) {
					addinsert(parser, NULL, curr,
						"new_", name, CONTINUE_AFTER);
					state = CONSTRUCT;
				} else {
					if (classtype->t.implicit) {
						str1 = "(struct ";
						str2 = "*)malloc(sizeof(struct ";
					} else {
						str1 = "(";
						str2 = "*)malloc(sizeof(";
					}
					/* first print a cast to dest type */
					insert_before = addinsert(parser, NULL,
						curr, str1, name, CONTINUE_AFTER);
					/* second the malloc, go back to name */
					insert_before = addinsert(parser, insert_before,
						next, str2, name, CONTINUE_AFTER);
					addinsert(parser, insert_before, next,
						"))", next, CONTINUE_AFTER);
				}
				if (*next == '<') {
					tmplpos = next;
					newtype = duptype(&parser->func_mem, typ(immdecl));
					if (newtype == NULL)
						return;
					newtype->type = AT_TEMPLINST;
					next = parse_templargs(parser, &parser->func_mem,
						thisclass, newtype->u.class,
						next, &newtype->args);
					immdecl = to_anyptr(newtype, 1);
					/* don't print template arguments */
					addinsert(parser, NULL, tmplpos, "", next, CONTINUE_AFTER);
				}
				continue;
 			} else if (state == STMTSTART && strprefixcmp("delete ", curr)) {
				flush_until(parser, curr);
				parser->pf.writepos = next = curr + 7;  /* "delete " */
				in_delete = 1;
				continue;
			}

			if (state == DECLVAR && to_class(decl) && pareninfo->prev == NULL) {
				/* for 2nd variable decl.class is the rootclass */
				if (declvar && decl->u.class->is_rootclass
						&& (ptrlvl(declvar->decl)
							|| ptrlvl(pareninfo->exprdecl))) {
					pr_err(curr, "cannot combine pointer and non-pointer "
						"declarations of root-requiring classes");
				} else if (!declvar && decl->u.class->rootclass
						&& ptrlvl(pareninfo->exprdecl) == 0) {
					/* declaring stack variable with root class, add _root */
					flush_until(parser, next);
					outwrite(parser, "_root", 5);
					parser->pf.writepos = next;
					/* fix class type, so ancestor paths are correct
					   template instantions are non-shared types so we
					   can modify them and want to keep the templ args */
					if (decl->type != AT_TEMPLINST)
						decl = &decl->u.class->rootclass->class.t;
					else
						((struct anytype *)decl)->u.class =
							&decl->u.class->rootclass->class;
				}
			}
			name = curr;
			next = skip_word(name);
			numwords++;
			nextstate = FINDVAR;
			switch (state) {
			case DECLVAR:
				immdecl = to_anyptr(decl, 0);
				declvar = addclsvariable(parser, blocklevel, immdecl,
						ptrlvl(pareninfo->exprdecl), name, next);
				if (ptrlvl(pareninfo->exprdecl) == 0 && decl->pointerlevel == 0
						&& to_class(decl)) {
					if (decl->u.class->num_abstract) {
						pr_err(name, "cannot instantiate "
							"abstract class %s", decl->u.class->name);
					} else if (decl->u.class->root_constructor) {
						nextstate = CONSTRUCT;
						if (decl->u.class->destructor && !eval_retvar) {
							/* need retvar also when this constructor
							   is void, because a 'return X;' halfway
							   needs a retvar to store X in */
							need_retvar = !is_void_rettype(thisfuncret);
							eval_retvar = 1;
						}
					}
				}
				break;
			case DECLMETHODVAR:
				addmpvariable(parser, blocklevel, to_anyptr(decl, 0),
					ptrlvl(pareninfo->exprdecl), name, next);
				break;
			case ACCESSMEMBER:  /* parsing expr.member or expr->member */
				/* immdecl is used for same parenthesis level,
				   exprdecl in case of cast (nested in parentheses) */
				p_expr = to_class(typ(immdecl)) ? &immdecl : &pareninfo->exprdecl;
				if (ptrlvl(*p_expr) <= 1) {
					submember = parse_member(parser, stmtstart,
						to_mp(typ(pareninfo->targetdecl)), expr_is_oneword,
						pareninfo->memberstart, exprend, *p_expr,
						name, next, p_expr,
						&pareninfo->targetparams.params);
					if (submember == NULL)
						break;
					/* to check all literal class variables initialized */
					if (is_constructor && submember->is_root_constructor
						&& member && ptrlvl(member->rettype) == 0) {
						if (!member->constructor_called) {
							num_constr_called++;
							member->constructor_called = 1;
						} else {
							pr_err(pareninfo->memberstart,
								"duplicate call to member %s "
								"root constructor", member->name);
						}
					}
					check_visibility(parser, thisclass, submember,
						pareninfo->memberstart);
				}
				break;
			case ACCESSINHERITED:  /* parsing class::member */
				next = access_inherited(parser, stmtstart,
					to_mp(typ(pareninfo->targetdecl)), thisclass,
					to_class(typ(immdecl)), accinhname, accinhcolons,
					name, next, &pareninfo->targetparams.params);
				break;
			default:
				/* maybe it's a local (stack) variable? */
				tgtparams = &pareninfo->targetparams.params;
				if (find_local_e_class(parser, name, next,
						&immdecl, tgtparams) >= 0)
					break;
				/* maybe it's a member field? "this" has pointerlevel 1 */
				member = parse_member(parser, stmtstart,
					to_mp(typ(pareninfo->targetdecl)), 0, NULL, NULL,
					thisptr, name, next, &immdecl, tgtparams);
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
					if (member->origin != thisclass)
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
					decl = &classtype->t;
				  decl_or_cast:
					if (*next == '<') {
						tmplpos = next;
						/* alloc copy to be modified */
						decl = newtype = duptype(&parser->func_mem, decl);
						if (newtype == NULL)
							break;
						newtype->type = AT_TEMPLINST;
						next = parse_templargs(parser, &parser->func_mem,
							thisclass, decl->u.class,
							next, &newtype->args);
					}
					/* we don't want implicit struct for class::func
					   note that classtype is not valid coming from
					   "struct " path, but then decl.implicit == 0 */
					if (*next != ':' && decl->implicit) {
						/* unsafe to print directly in expression */
						if (state == STMTSTART)
							print_implicit_struct(parser, name);
						else
							addinsert_implicit_struct(parser, name);
					}
					if (decl->type == AT_TEMPLINST) {
						/* don't print template arguments */
						if (state == STMTSTART) {
							flush_until(parser, tmplpos);
							parser->pf.writepos = next;
						} else {
							addinsert(parser, NULL,
								tmplpos, "", next, CONTINUE_AFTER);
						}
					}
					if (state == STMTSTART) {
						nextstate = DECLVAR;
					} else if (seqparen >= 2 && parencast->exprdecl) {
						/* this is a cast, like ((class_t*)x)->..
						   remember first detected class */
						/* combine possible pointer dereference */
						parencast->exprdecl = to_anyptr(decl,
							ptrlvl(parencast->exprdecl));
						decl = &unknown_type;
						nextstate = CASTVAR;
					} else if (*next != ':' || next[1] != ':') {
						/* grammar error, ignore */
						decl = &unknown_type;
					}
					break;
				}
				mptype = find_methodptr_e(parser, name, next);
				if (mptype != NULL) {
					if (state == STMTSTART) {
						decl = &mptype->t;
						nextstate = DECLMETHODVAR;
					} else if (seqparen >= 2 && !parencast->exprdecl) {
						/* cast to methodptr */
						parencast->exprdecl = to_anyptr(&mptype->t, 0);
						nextstate = CASTVAR;
					}
					break;
				}
				if (thisclass && (tptype = find_templpar_e(thisclass,
						name, next)) != NULL) {
					if (state == STMTSTART) {
						next = replace_templpar(parser,
								tptype, name, next);
						decl = &tptype->t;
						nextstate = DECLVAR;
					} else if (seqparen >= 2
							&& !typ(parencast->exprdecl)) {
						/* cast to template parameter type
						   in expression, not safe to replace here */
						addinsert_templpar(parser,
								tptype, name, next);
						parencast->exprdecl = to_anyptr(&tptype->t,
							raw_ptrlvl(parencast->exprdecl));
						nextstate = CASTVAR;
					}
					break;
				}
				/* are we at parenthesis level 1? */
				if (pareninfo->prev == parenlvl0
						&& ptrlvl(pareninfo->exprdecl) == PTRLVLMASK) {
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
					addvariable(parser, NULL, blocklevel, to_anyptr(decl, 0),
						ptrlvl(pareninfo->exprdecl), funcvarname,
						funcvarnameend, curr+1);
				}
			}
		}

		if (state == CONSTRUCT) {
			expr = to_class(typ(immdecl)) ? immdecl : to_anyptr(decl, 0);
			if (typ(expr)->u.class->missing_root) {
				pr_err(curr, "must define root constructor for %s "
					"due to non-void constructor %s",
					typ(expr)->u.class->name,
					typ(expr)->u.class->missing_root->name);
			}
			constr = typ(expr)->u.class->root_constructor;
			if (constr) {
				if (*curr != '(' && constr->paramstext[0] != ')') {
					pr_err(curr, "missing call to constructor");
				} else {
					/* no pointer means stack-based constructor */
					if (ptrlvl(expr) == 0) {
						initializer = addinitializer(parser,
							decl->u.class, declvar->name, next);
						if (*curr == '(') {
							/* skip parenthesis,
							   need to add "this" parameter */
							initializer->params = curr + 1;
						}
					}
					if (*curr == '(') {
						pareninfo->exprdecl = expr;
						pareninfo->targetparams.params = &constr->params;
					}
				}
				if (!ptrlvl(expr) && !typ(expr)->u.class->void_root_constructor)
					pr_warn(curr, "non-void root constructor may fail");
			}
			if (*curr != '(' && ptrlvl(expr)) {
				/* allocation with 'new class', needs function call */
				addinsert(parser, NULL, curr, "()", curr, CONTINUE_AFTER);
			}
			state = FINDVAR;
		} else if (*curr == '(') {
			mp = to_mp(typ(immdecl)) ?: to_mp(typ(pareninfo->exprdecl));
			if (mp && ptrlvl(pareninfo->exprdecl) < 2) {
				next = insertmpcall(parser, expr_is_oneword, stmtstart,
					pareninfo->exprstart, exprend, mp,
					curr, ptrlvl(pareninfo->exprdecl));
			}
		}

		if (*curr != '.' && (*curr != '-' || curr[1] != '>'))
			pareninfo->memberstart = NULL;
		if (!flist_empty(&parser->initializers) && parenprev == NULL && *curr == '=') {
			/* when added one initializer, then copy them all,
			   to keep order the order the same */
			initializer = addinitializer(parser, NULL, name, next);
		} else if (!pareninfo->dyncast && (*curr == ')' || *curr == ','
						|| *curr == '=' || *curr == ';')) {
			/* parsing membername in context (function argument, assignment)
			   where pointer to a membername's ancestor class is expected
			   determine target and source classes to access ancestor of */
			add_ptrlvl(&immdecl, ptrlvl(pareninfo->exprdecl));
			expr = sel_class(pareninfo->exprdecl) ?: sel_class(immdecl);
			target = sel_class(pareninfo->targetdecl);
			if (target && expr && ptrlvl(target) <= 1)
				insertancestor(parser, pareninfo->exprstart,
						name, curr, typ(target), expr);
		}
		if (*curr == ')' || *curr == ',' || *curr == '=' || *curr == ';')
			pareninfo->exprstart = NULL;
		if (initializer && parenprev == NULL && (*curr == ',' || *curr == ';')) {
			if (initializer->start != curr) {
				flush_until(parser, initializer->start);
				parser->pf.writepos = curr;
			}
			initializer->end = curr;
			parser->inserts = &parser->inserts_list;
			initializer = NULL;
		}
		if (pareninfo->dyncast == DYNCAST_EXPR && is_dyncast_expr_sep(curr)) {
			/* close coo_dyn_cast call */
			addinsert(parser, NULL, curr, "->vmt)", curr, CONTINUE_AFTER);
			pareninfo->dyncast = DYNCAST_NONE;
		}
		if (*curr == '(')
			seqparen++;
		else {
			seqparen = 0;
			pareninfo->targetparams.params = NULL;
		}
		if (*curr == '(') {
			pareninfo->memberstart = curr;
			if (typ(immdecl)->type != AT_UNKNOWN) {
				pareninfo->exprdecl = immdecl;
				immdecl = unknown_typeptr;
			}
			parencast = pareninfo->prev;
			parenprev = pareninfo;
			pareninfo = pareninfo->next;
			/* always have a next parenthesis level ready for parameter target */
			if (!pareninfo->next) {
				pareninfo->next = fgenzalloc(sizeof(*pareninfo->next));
				if (!pareninfo->next)
					return;
				pareninfo->next->prev = pareninfo;
			}
			pareninfo->memberstart = NULL;  /* reset, assigned later */
			pareninfo->exprstart = NULL;
			pareninfo->exprdecl = unknown_typeptr;
			pareninfo->targetparams.params = NULL;
			parenprev->targetparams.index = -1;
			select_next_param(parser, curr, parenprev->exprdecl,
				&parenprev->targetparams, &pareninfo->targetdecl);
			state = EXPRUNARY;
		} else if (*curr == ')') {
			if (pareninfo->prev) {
				if (pareninfo->dyncast) {
					addinsert(parser, NULL,
						curr, ")->vmt", curr, CONTINUE_AFTER);
					pareninfo->dyncast = DYNCAST_NONE;
				}
				if (parenprev->targetparams.params) {
					/* reset paraminfo if was parsing function arguments
					   for functions, don't copy the last type upwards */
					parenprev->targetparams.params = NULL;
				} else if (typ(parenprev->exprdecl)->type == AT_UNKNOWN) {
					if (typ(pareninfo->exprdecl)->type != AT_UNKNOWN)
						parenprev->exprdecl = pareninfo->exprdecl;
					else
						parenprev->exprdecl = immdecl;
				}
				pareninfo = parenprev;
				parenprev = parencast;
				parencast = parencast ? parencast->prev : NULL;
				immdecl = unknown_typeptr;
			}
			state = FINDVAR;
		} else if (*curr == '*') {
			switch (state) {
			case DECLVAR: add_ptrlvl(&pareninfo->exprdecl, 1); break;
			case CASTVAR: add_ptrlvl(&parencast->exprdecl, 1); break;
			case STMTSTART:
			case ACCESSMEMBER:
			case EXPRUNARY: sub_ptrlvl(&pareninfo->exprdecl, 1); break;
			default: break;
			}
		} else if (*curr == ',') {
			if (to_class(decl) && !pareninfo->prev)
				state = DECLVAR;
			else if (to_mp(decl) && !pareninfo->prev)
				state = DECLMETHODVAR;
			else
				state = EXPRUNARY;
			if (pareninfo->prev) {
				select_next_param(parser, curr, parenprev->exprdecl,
					&parenprev->targetparams, &pareninfo->targetdecl);
			}
			clear_ptrlvl(&pareninfo->exprdecl);
		} else if (*curr == '=') {
			pareninfo->targetdecl = immdecl;
			clear_ptrlvl(&pareninfo->exprdecl);
			immdecl = unknown_typeptr;
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
				/* recognized a class name as constructor member */
				immdecl = to_anyptr(&member->origin->t, 0);
				goto accessinherited;
			} else if (to_class(decl)) {
				immdecl = to_anyptr(decl, 0);
				decl = &unknown_type;
			  accessinherited:
				/* convert "class::func" to "class_func" */
				clear_ptrlvl(&immdecl);
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
				expr = to_class(typ(immdecl)) ? immdecl : parenlvl0->exprdecl;
				if (to_class(typ(expr))) {
					if (typ(expr)->u.class->destructor) {
						outprintf(parser, "free_%s(",
							typ(expr)->u.class->name);
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
			decl = &unknown_type;
			stmtstart = NULL;
			declvar = NULL;
			member = NULL;
			numwords = 0;
			seqparen = 0;
			pareninfo = parenlvl0;
			pareninfo->targetdecl = pareninfo->exprdecl = immdecl = unknown_typeptr;
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
					unused_retblocknr = next_retblocknr;
					goto_ret = GOTO_RET_NONE;
				} else {
					/* prevent adding unused retblock label */
					next_retblocknr = unused_retblocknr;
				}
				continue;
			}
		} else if (*curr == '+' || *curr == '-' || *curr == '/' || *curr == '%'
			|| *curr == '&' || *curr == '|' || *curr == '^' || *curr == '!'
			|| *curr == '?' || *curr == ':') {
			if (*curr == '&' && state == EXPRUNARY) {
				add_ptrlvl(&pareninfo->exprdecl, 1);
			} else {
				state = EXPRUNARY;
				if (immdecl) {
					pareninfo->exprdecl = immdecl;
					immdecl = unknown_typeptr;
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
		flist_foreach(member, &thisclass->members_list, next) {
			if (member->constructor_called)
				continue;
			if (member->parent_constructor) {
				pr_err(curr, "missing parent constructor "
					"%s call", member->name);
			} else if (member_needs_init(thisclass, member)) {
				pr_err(curr, "missing member root constructor "
					"call %s.%s()", member->name,
					typ(member->rettype)->u.class->root_constructor->name);
			}
		}
	}

	if (is_constructor) {
		flush_until(parser, curr);
		parser->pf.writepos = curr;
		switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		outwrite(parser, "\treturn this;\n", 14);
		parser->pf.lines_coo++;
		switch_line_pragma(parser, LINE_PRAGMA_INPUT);
	}

	/* free this function's variable/type memory */
	areset(&parser->func_mem);
	parser->last_local = NULL;
	parser->pf.pos = next;
}

static void print_class_alloc(struct parser *parser, struct class *class)
{
	struct member *rootconstr = class->root_constructor;
	char *rootsuffix, *callend1, *callend2, *callend3, *params, alloctyp, *allocarg;
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
			pr_err(NULL, "(ierr) no ancestor for class alloc");
			return;
		}
		constr_addr = ancestor->parent->is_virtual ? "" : "&";
		constr_arrow = "->";
		constr_path = ancestor->path;
		callend1 = "\treturn this;\n";
	} else
		constr_ret = "return ";
	if (class->zeroinit)
		alloctyp = 'c', allocarg = "1, ";
	else
		alloctyp = 'm', allocarg = "";
	params = rootconstr->paramstext[0] == ')' ? "void)" : rootconstr->paramstext;
	outprintf(parser, "\nstruct %s *new_%s(%s\n"
		"{\n\tstruct %s%s *this = %calloc(%ssizeof(*this));\n"
		"\tif (this == NULL) return NULL;\n"
		"\t%s%s_%s(%sthis%s%s",
		class->name, class->name, params,
		class->name, rootsuffix, alloctyp, allocarg,
		constr_ret, constr_origin->name, rootconstr->name,
		constr_addr, constr_arrow, constr_path);
	print_param_names(parser, rootconstr->paramstext);
	outprintf(parser, ");\n%s%s%s}\n", callend1, callend2, callend3);
	/* no need to count lines_coo here, end of input */
}

static void print_class_zeroinit(struct parser *parser, struct class *class)
{
	struct member *rootconstr = class->root_constructor;
	char *params, *sep, *ret_pre, *ret_type, *ret_post, *ret_oper;
	char *constr_addr, *constr_arrow, *constr_path;
	struct ancestor *ancestor;
	struct class *rootclass;

	if (!class->zeroinit)
		return;

	if (class->root_constructor) {
		rootconstr = class->root_constructor;
		params = class->root_constructor->paramstext;
		sep = params[0] == ')' ? "" : ", ";
		if (class->void_root_constructor) {
			ret_pre = ret_post = ret_oper = "";
			ret_type = "void ";
		} else {
			ret_pre = "struct ";
			ret_type = class->name;
			ret_post = " *";
			ret_oper = "return ";
		}
		if (class->rootclass)
			rootclass = &class->rootclass->class;
		else
			rootclass = class;
		if (rootconstr->definition != class) {
			ancestor = hasho_find(&class->ancestors, rootconstr->definition);
			if (!ancestor) {
				pr_err(NULL, "(ierr) no ancestor for zeroinit constructor");
				return;
			}
			constr_addr = ancestor->parent->is_virtual ? "" : "&";
			constr_arrow = "->";
			constr_path = ancestor->path;
		} else {
			constr_addr = constr_arrow = constr_path = "";
		}
		outprintf(parser, "\n%s%s%s%s_%s_zi(struct %s *this%s%s\n"
			"{\n\tmemset(this, 0, sizeof(*this));\n"
			"\t%s%s_%s(%sthis%s%s", ret_pre, ret_type, ret_post, class->name,
			rootconstr->name, rootclass->name, sep, params,
			ret_oper, rootconstr->definition->name, rootconstr->name,
			constr_addr, constr_arrow, constr_path);
		print_param_names(parser, rootconstr->paramstext);
		outputs(parser, ");\n}\n");
	} else {
		outprintf(parser, "\nvoid %s_%s_zi(struct %s *this)\n{\n"
			"\tmemset(this, 0, sizeof(*this));\n}\n",
			class->name, class->name, class->name);
	}
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

	flist_foreach(member, &class->members_list, next) {
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
	flist_foreach(vmt, &rootclass->vmts, next) {
		get_vmt_path(parser, vmt, &vmtpath, &vmtaccess);
		outprintf(parser, "\tthis->%s%svmt = &%s.vmt_base;\n",
			vmtpath, vmtaccess, get_vmt_name(parser, vmt));
	}
	/* init refcount */
	if (class->refcounted && !class->zeroinit)
		outputs(parser, "\tthis->refcount = 0;\n");
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

	if (!class->gen_constructor)
		return;

	print_func_header(parser, class, class->constructor);
	print_construct_parents(parser, class, "", "", LITERAL_PARENT);
	flist_foreach(member, &class->members_list, next) {
		if (!member_needs_init(class, member))
			continue;
		memberclass = typ(member->rettype)->u.class;
		print_call_constructor(parser, memberclass,
			"", "", member->name, memberclass->root_constructor);
	}
	outwrite(parser, "\treturn this;\n}\n", 16);
}

static void print_destruct_parents(struct parser *parser, struct class *class,
		enum parent_virtual virtual_parents)
{
	struct class *parentclass;
	struct member *member, **destruct_parents;
	unsigned i = 0;

	/* first gather parent destructors */
	destruct_parents = alloca(class->num_parent_destr * sizeof(member));
	flist_foreach(member, &class->members_list, next) {
		if (!member->parent_destructor)
			continue;
		if (virtual_parents != member->props.from_virtual)
			continue;
		destruct_parents[i++] = member;
	}
	/* reverse walk our gathered array */
	while (i--) {
		member = destruct_parents[i];
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

	/* TODO: change this pointer to %s_root? */
	outprintf(parser, "void %s_d_%s_root(struct %s *this)\n{\n",
		class->name, class->name, class->name);
	if (class->rootclass)
		outprintf(parser, "\tstruct %s *root_this = container_of(this, struct %s, %s);",
			class->name, class->name, class->name);
	if (class->has_destructor) {
		outprintf(parser, "\t%s_d_%s(&this->%s);\n\t",
			class->name, class->name, class->name);
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
	struct member *member, *destructor, **destruct_members;
	unsigned i = 0;

	if (!class->gen_destructor)
		return;

	/* first gather member destructors */
	destruct_members = alloca(class->num_destr_vars * sizeof(member));
	print_func_header(parser, class, class->destructor);
	flist_foreach(member, &class->members_list, next) {
		if (!member_needs_dispose(class, member))
			continue;
		destruct_members[i++] = member;
	}
	/* reverse walk our gathered array */
	while (i--) {
		member = destruct_members[i];
		memberclass = typ(member->rettype)->u.class;
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
	destrclass = class->destructor ? class->destructor->definition : class;
	if (destrclass != class) {
		ancestor = hasho_find(&class->ancestors, destrclass);
		if (ancestor) {
			addr = ancestor->parent->is_virtual ? "" : "&";
			arrow = "->";
			path = ancestor->path;
		}
	}
	outprintf(parser, "\nvoid free_%s(struct %s *this)\n{\n", class->name, class->name);
	if (class->refcounted)
		outputs(parser, "\tif (coo_atomic_fetch_dec(&this->refcount) != 0)\n"
			"\t\treturn;\n");
	outprintf(parser, "\t%s_d_%s%s(%sthis%s%s);\n\tfree(this);\n}\n",
			destrclass->name, destrclass->name,
			destrclass->rootclass ? "_root" : "", addr, arrow, path);
}

static void print_trampolines(struct parser *parser, struct class *class, struct vmt *vmt)
{
	struct member *member;
	char *vmtpath, *vmtaccess, *funcmiddle, *sep, *structsuffix;
	char *funcprefix, *implprefix, *implname, *thisaccess, *thismember;
	struct class *implclass, *previmplclass, *vmtclass;
	struct ancestor *ancestor;

	get_vmt_path(parser, vmt, &vmtpath, &vmtaccess);
	if (vmt->from_virtual) {
		funcmiddle = "root";
		structsuffix = "_root";
		thisaccess = "&this->";
		thismember = class->name;
	} else {
		funcmiddle = vmt->parent->class->name;
		structsuffix = "";
		thisaccess = "this";
		thismember = "";
	}
	previmplclass = NULL;
	flist_foreach(member, &class->members_list, next) {
		if (!is_vmt_member(member, vmt))
			continue;

		/* need trampoline for calls from virtual base */
		if (!vmt->from_virtual) {
			/* ... and for alternate vmts */
			if (member->vmt_impl == vmt)
				continue;

			/* non-primary parent-inherited destructor is special because it isn't
			   merged with our destructor so get_impl_this_class does not work */
			implclass = member != vmt->destructor ? get_impl_this_class(member)
								: member->implemented;
			if (implclass != previmplclass) {
				previmplclass = implclass;
				if (implclass != class) {
					ancestor = hasho_find(&class->ancestors, implclass);
					if (ancestor == NULL) {
						pr_err(NULL, "(ierr) cannot find ancestor "
							"%s for trampoline of %s::%s",
							implclass->name,
							class->name, member->name);
						continue;
					}
					thisaccess = "&this->";
					thismember = ancestor->path;
				} else {
					thisaccess = "this";
					thismember = "";
				}
			}
		}
		vmtclass = get_vmt_this_class(member, vmt);
		sep = member->paramstext[0] == ')' ? "" : ", ";
		if (!member->is_destructor) {
			funcprefix = class->name;
			implprefix = member->implemented->name;
			implname = member->name;
		} else {
			funcprefix = implprefix = "free";
			implname = member->vmt->destructor->implemented->name;
		}
		outprintf(parser, "\nstatic %s%s_%s_%s(struct %s *__this%s%s\n"
			"{\tstruct %s%s *this = container_of("
				"__this, struct %s%s, %s);\n"
			"\t%s%s_%s(%s%s",
			member->rettypestr, funcprefix, funcmiddle,
			  member->implname, vmtclass->name, sep, member->paramstext,
			class->name, structsuffix, class->name,
			  structsuffix, vmtpath,
			is_void_rettype(member->rettypestr) ? "" : "return ",
			  implprefix, implname, thisaccess, thismember);
		print_param_names(parser, member->paramstext);
		outprintf(parser, ");\n}\n");
		/* no need to count lines_coo here, end of input */
	}
}

static void print_coo_class(struct parser *parser, struct class *class)
{
	struct parent *parent;
	unsigned num_dync_parents, first;
	struct class *rootclass;

	rootclass = class->rootclass ? &class->rootclass->class : class;
	num_dync_parents = 0;
	flist_foreach(parent, &class->dync_parents, next_dync)
		num_dync_parents++;
	if (num_dync_parents && class->need_dync_p0_dummy)
		num_dync_parents++;
	switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
	/* disable packing so 'void* parents[]' is aligned */
	if (!parser->coo_includes_pr) {
		outputs(parser,
			"#include <stddef.h>\n"
			"#include <stdint.h>\n"
			"#include <stdlib.h>\n"
			"#include <string.h>\n"
			"#pragma pack(8)");
		parser->coo_includes_pr = 1;
	}
	outprintf(parser, "\nconst struct %s_coo_class {\n"
		"\tuint32_t num_parents;\n", class->name);
	/* offsets are between parents, assmption is that first parent is located
	   at offset zero; if this does not hold, then need_dummy is set */
	if (num_dync_parents > 1)
		outprintf(parser, "\tuint32_t offsets[%u];\n", num_dync_parents - 1);
	if (num_dync_parents > 0)
		outprintf(parser, "\tconst void *parents[%u];\n", num_dync_parents);
	outprintf(parser, "} %s_coo_class = {\n"
		"\t%u,\n", class->name, num_dync_parents);
	if (num_dync_parents > 0) {
		parent = class->dync_parents.first;
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
		flist_foreach(parent, &class->dync_parents, next_dync) {
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
	char *classsep, *classsuffix, *implprefix, *vmtpre, *vmtpreacc, *vmtaccess, *vmtpath;
	struct ancestor *ancestor;
	struct class *rootclass, *implclass;

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
	flist_foreach(member, &class->members_list, next) {
		if (!is_vmt_member(member, vmt))
			continue;

		/* if from virtual base, then trampoline via root class, suffix set above */
		implclass = member->implemented;
		if (!vmt->from_virtual) {
			/* choose to use the trampoline or not (see print trampolines) */
			if (member->vmt_impl == vmt)
				classsep = classsuffix = "";
			else {
				classsep = "_";
				classsuffix = (vmt->parent ?: vmt)->class->name;
				/* point to trampoline where alternate vmt is implemented */
				implclass = vmt->modified->class;
			}
		}
		implprefix = !member->is_destructor ? implclass->name : "free";
		outprintf(parser, "\t%s%s%s_%s,\n",
			implprefix, classsep, classsuffix, member->implname);
	}
	outprintf(parser, "};\n");
	/* no need to count lines_coo here, end of input */
}

static void print_class_impl(struct parser *parser)
{
	struct class *class;
	struct vmt *vmt;

	hash_foreach(class, &parser->classes) {
		if (!class->is_implemented)
			continue;

		if (parser->pf.writepos != parser->pf.pos) {
			flush(parser);
			/* separation newline between user and coo-generated block */
			outwrite(parser, "\n", 1);
			parser->pf.lines_coo++;
			switch_line_pragma(parser, LINE_PRAGMA_OUTPUT);
		}

		/* print class def also for abstract classes */
		print_coo_class(parser, class);

		/* but no need to print vmts for abstract classes */
		if (class->num_abstract)
			continue;

		flist_foreach(vmt, &class->vmts, next) {
			if (vmt->modified != vmt)
				continue;

			/* print trampoline functions for virtual functions
			   inherited from virtual base classes, where implementation
			   cannot see literal base therefore cannot translate 'this' */
			/* note that this implies a root class for this class: if the
			   base is present as a literal base, no need for root class */
			if (vmt->from_virtual || vmt != class->vmt)
				print_trampolines(parser, class, vmt);

			/* print vmt itself */
			print_vmt(parser, class, vmt);
		}

		print_class_alloc(parser, class);
		print_class_zeroinit(parser, class);
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
	file_id = pgenzalloc(sizeof(*file_id));
	if (file_id == NULL)
		goto err_fp;
	if (get_file_id(fp_inc, file_id) < 0)
		goto err_fp;
	if (hash_insert(&parser->files_seen, &file_id->node,
			uint64hash(file_id->file_id)))
		goto err_fp;
	parser->newfilename_end = p;
	return fp_inc;
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
	struct parse_file_item *parse_file_item;
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
	parse_file_item = addfilestack(parser);
	if (parse_file_item == NULL)
		goto err_add;

	/* make copy before overwriting parse_file, newfilename is reused later */
	parse_file = &parse_file_item->pf;
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
	parser->pf.defined_tp_impl = 0;
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
	blist_remove_last(&parser->file_stack, prev);
	blist_add(&parser->file_avail, parse_file_item, prev);
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
	anyptr typeptr;
	struct variable *variable;
	char *end, *name, coo_class_var;
	new_insert_cb insert_handler = print_type_insert;

	/* special declaration to trigger coo class variable to be emitted */
	coo_class_var = memcmp("::coo_class", next - 11, 11) == 0;
	if (coo_class_var) {
		/* do not output this declaration; don't print implicit struct */
		flush(parser);
		parser->pf.writepos = skip_whitespace(parser, next + 1);
		insert_handler = NULL;
	}

	name = parse_type(parser, &parser->global_mem, NULL,
			parser->pf.pos, &typeptr, insert_handler);
	if (!to_class(typ(typeptr)) && !to_mp(typ(typeptr)))
		return;
	if (coo_class_var) {
		if (!to_class(typ(typeptr)))
			pr_err(parser->pf.pos, "unknown class name");
		else
			typ(typeptr)->u.class->is_implemented = 1;
		return;
	}

	/* global variable definition */
	end = skip_word(name);
	variable = alloc_namestruct(&parser->global_mem, struct variable, name, end);
	if (variable == NULL)
		return;

	variable->decl = typeptr;
	strhash_insert(&parser->globals, variable);
}

static void parse(struct parser *parser)
{
	char *curr, *next;

	/* write line directives for useful compiler messages */
	parser->pf.linestart = parser->pf.pos - 1;
	if (parser->line_pragmas) {
		outprintf(parser, "#line 1 \"%s\"\n", parser->pf.filename);
		/* 1 here, and start at 1 */
		parser->pf.lines_coo = 2;
	}

	/* search for start of struct or function */
	while (!parser->memerror) {
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
		parser->saw.all = 0;
		if (strprefixcmp("typedef ", curr)) {
			parser->pf.pos = curr += 8;  /* "typedef " */
			parser->saw.k.typdef = 1;
		}

		/* do not confuse typedef function pointer with actual function */
		next = scan_token(parser, curr,
			parser->saw.k.typdef ? "/\n{;" : "/\n({;");
		if (next == NULL)
			break;
		for (;;) {
			if (strprefixcmp("final ", curr)) {
				curr += 6;  /* "final " */
				parser->saw.k.final = 1;
			} else if (strprefixcmp("nodyncast ", curr)) {
				curr += 10;  /* "nodyncast " */
				parser->saw.k.nodyncast = 1;
			} else if (strprefixcmp("nozeroinit ", curr)) {
				curr += 11;  /* "nozeroinit " */
				parser->saw.k.nozeroinit = 1;
			} else if (strprefixcmp("zeroinit ", curr)) {
				curr += 9;  /* "zeroinit " */
				parser->saw.k.zeroinit = 1;
			} else if (strprefixcmp("refcount ", curr)) {
				curr += 9;  /* "refcount " */
				parser->saw.k.refcount = 1;
			} else
				break;
		}
		if (*next == '{' && strprefixcmp("struct ", curr)) {
			if (parser->saw.k.zeroinit || parser->saw.k.nozeroinit
				|| parser->saw.k.final || parser->saw.k.nodyncast) {
				/* skip these COO specific keywords */
				flush(parser);
				parser->pf.writepos = parser->pf.pos = curr;
			}
			parse_struct(parser, next);
		} else if (*next == ';' && parser->saw.k.typdef) {
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
	if (parser->memerror)
		return -1;
	new_errors = parser->num_errors > prev_num_errors;
	if (!new_errors)
		print_class_impl(parser);  /* vmt(s), alloc, constr. */
	/* if no syntax added by coo parser, then can keep original filename in #include */
	if (parser->pf.writepos == buffer) {
		/* if parsing included file, and nothing changed, do not write any output */
		if (!blist_empty(&parser->file_stack)) {
			/* probably written first #line pragma already, remove file */
			if (parser->pf.out) {
				fclose(parser->pf.out);
				*parser->pf.outfilename_end = '.';
				unlink(parser->pf.outfilename_end);
			}
			return OUTPUT_FILE_SKIPPED;
		}
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
		fprintf(stderr, "cannot open file '%s'\n", filename);
		return;
	}

	if (stfcpy_e(parser->newfilename, filename, &strend) < 0) {
		fprintf(stderr, "input filename too long\n");
		return;
	}

	parser->newfilename_end = strend;
	if (parse_source(parser, filename, in, ext_out) < 0)
		fprintf(stderr, "out of memory\n");
}

static void parse_stdin(struct parser *parser)
{
	parser->pf.out = stdout;
	if (parse_source_size(parser, "<stdin>", stdin, STDIN_BUFSIZE,
			parser->source_ext_out) < 0)
		fprintf(stderr, "out of memory\n");
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
	parser->inserts = &parser->inserts_list;
	init_allocator(&parser->global_mem, &parser->memerror);
	init_allocator(&parser->func_mem, &parser->memerror);
	flist_init(&parser->initializers);
	dlist_init(&parser->inserts_list, item);
	return strhash_init(&parser->classes, 64, class) < 0 ||
		strhash_init(&parser->classtypes, 64, classtype) < 0 ||
		strhash_init(&parser->globals, 64, variable) < 0 ||
		strhash_init(&parser->locals, 16, variable) < 0 ||
		strhash_init(&parser->methodptrtypes, 16, methodptr) < 0 ||
		hash_init(&parser->files_seen, compare_file_ids,
			64, offsetof(struct file_id, node), 0) < 0;
}

static void free_file(struct parse_file *pf)
{
	free(pf->buffer);
	free(pf->filename);
	free(pf->outbuffer);
	free(pf->outfilename);
}

static void deinit_parser(struct parser *parser)
{
	struct parse_file_item *pfitem;
	void *item;

	hash_foreach(item, &parser->classes)
		deinit_class(item);
	hash_deinit(&parser->classes);
	hash_deinit(&parser->classtypes);
	hash_deinit(&parser->globals);
	hash_deinit(&parser->locals);
	hash_deinit(&parser->methodptrtypes);
	hash_deinit(&parser->files_seen);
	blist_foreach_rev(pfitem, &parser->file_avail, prev)
		free_file(&pfitem->pf);
	blist_foreach_rev(pfitem, &parser->file_stack, prev)
		free_file(&pfitem->pf);
	free(parser->pf.buffer);
	free(parser->pf.outfilename);
	free(parser->pf.outbuffer);
	deinit_allocator(&parser->global_mem);
	deinit_allocator(&parser->func_mem);
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

	deinit_parser(parser);
	return parser->memerror + parser->num_errors > 0;
}
