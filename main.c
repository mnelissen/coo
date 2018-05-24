#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

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

struct parser {
	FILE *out;
	char *buffer;
	char *writepos, *pos;
	int coo_inline_defined;
	struct dynarr classes;        /* struct class pointers */
	struct dynarr classtypes;     /* struct classtype pointers */
	struct dynarr globals;        /* struct variable pointers */
	struct dynarr locals;         /* struct variable pointers */
	struct dynarr nested_locals;  /* struct variable pointers */
	struct dynarr inserts;        /* struct insert pointers */
};

static char *strchrnul(const char *str, int ch)
{
	while (*str && *str != ch)
		str++;
	return (char*)str;
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

static char *strprefixcmp(const char *prefix, const char *str)
{
        for (;; prefix++, str++) {
                if (*prefix == 0)
                        return (char*)str;
                if (*prefix != *str)
                        return NULL;
        }
}

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

static char *stredup(const char *src, const char *until)
{
	return stredupto(NULL, src, until);
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
	if (dynarr->sorted)
		return;

	qsort_dynarr(dynarr, compare_names);
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

static void flush_until(struct parser *parser, char *until)
{
	if (parser->writepos > until) {
		fprintf(stderr, "internal error, flushing into past\n");
		return;
	}
	fwrite(parser->writepos, 1, until - parser->writepos, parser->out);
	/* update writepos done in caller, usually want to skip something anyway */
}

static void flush(struct parser *parser)
{
	flush_until(parser, parser->pos);
	parser->writepos = parser->pos;
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
		next = skip_word(parser->pos);
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

	fprintf(parser->out, "extern struct %s_vmt {\n", class->name);
	for (i = 0; i < class->members.num; i++) {
		member = class->members.mem[i];
		if (!member->props.is_virtual)
			continue;

		fprintf(parser->out, "\t%s%c(*%s)(%s;\n",
			member->rettype, member->retsep,
			member->name, member->params);
	}
	fprintf(parser->out, "} %s_vmt;\n\n", class->name);
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
	fprintf(parser->out, "%s%s%c%s_%s%s(struct %s *this%s%s%s\n",
		func_prefix, member->rettype, member->retsep,
		class->name, name_insert, member->name, class->name,
		empty ? "" : ", ", member->params, func_body);
	if (emittype == VIRTUAL_WRAPPER) {
		fprintf(parser->out, "\t((struct %s_vmt*)this->vmt)->%s(this",
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
				fprintf(parser->out, ", %s", last_word);
			}
			/* go to next argument */
			if (*p != ')')
				p++;
		}
		fprintf(parser->out, ");\n}\n\n");
	}
}

static void print_member_decls(struct parser *parser, struct class *class)
{
	struct member *member;
	unsigned i;

	if (class->num_vfuncs && !parser->coo_inline_defined) {
		fputs(DEF_COO_INLINE, parser->out);
		parser->coo_inline_defined = 1;
	}

	for (i = 0; i < class->members.num; i++) {
		member = class->members.mem[i];
		if (member->props.is_function) {
			print_func_decl(parser, class, member, MEMBER_FUNCTION);
		} else if (member->props.is_static) {
			fprintf(parser->out, "extern %s%c%s_%s;\n",
				member->rettype, member->retsep,
				class->name, member->name);
		}
	}

	if (!class->num_vfuncs)
		return;

	/* now generate all the virtual method call wrappers */
	fputs("\n", parser->out);
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

	is_typedef = *parser->pos == 't';
	/* skip 'typedef struct ' or just 'struct ' */
	classname = skip_whitespace(parser->pos + (is_typedef ? 15 : 7));
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
		parser->pos = parser->writepos = next = nextdecl;
		if (props.is_virtual) {
			if (!class->num_vfuncs) {
				fputs("void *vmt;", parser->out);
				parser->writepos = declend+1;
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
	parser->pos = skip_whitespace(declend + 1);
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
	class = parse_type(parser, parser->pos, &next);
	if (class == NULL)
		return;

	/* note: after next cannot be '{', as in 'typedef struct X {'
	   it is handled by calling parse_struct in caller */
	parser->pos = skip_whitespace(next);
	next = skip_word(parser->pos);
	addclasstype(parser, parser->pos, next, class);
	parser->pos = declend + 1;
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
		fputs(insert->insert_text, parser->out);
		parser->writepos = insert->continue_at;
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
	for (; classname > parser->pos && !isspace(*(classname-1)); classname--) {
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
				fputs("static ", parser->out);
			}
			/* replace :: with _ */
			flush_until(parser, dblcolonsep);
			fputc('_', parser->out);
			parser->writepos = funcname;
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
			fprintf(parser->out, "struct %s *this%s", classname, argsep);
			parser->writepos = next;
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
		parser->pos = next + 1;
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

	parser->pos = curr + 1;
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

		fprintf(parser->out, "\nstruct %s_vmt %s_vmt {\n", class->name, class->name);
		for (j = 0; j < class->members.num; j++) {
			member = class->members.mem[j];
			if (!member->props.is_virtual)
				continue;

			fprintf(parser->out, "\t%s_%s,\n", class->name, member->name);
		}
		fputs("};\n", parser->out);
	}
}

#define LEFT_PAREN  ((void*)(size_t)'(')

static void parse(char *filename, char *buffer)
{
	struct parser parser_s, *parser = &parser_s;
	char *next;

	memset(&parser_s, 0, sizeof(parser_s));
	parser->writepos = parser->pos = buffer;
	parser->out = stdout;
	/* search for start of struct or function */
	for (;;) {
		parser->pos = skip_whitespace(parser->pos);
		next = scan_token(parser->pos, "/({;");
		if (next == NULL)
			break;

		if (*next == '{' && strprefixcmp("struct ", parser->pos) != NULL) {
			parse_struct(parser, next);
		}
		if (*next == '{' && strprefixcmp("typedef struct ", parser->pos) != NULL) {
			parse_struct(parser, next);
		}
		if (*next == ';' && strprefixcmp("typedef ", parser->pos) != NULL) {
			parser->pos += 8;  /* "typedef " */
			parse_typedef(parser, next);
		}
		if (*next == '(') {
			parse_function(parser, next);
		}
		if (parser->pos <= next)
			parser->pos = next + 1;
	}

	flush(parser);
	print_vmts(parser);
}

int main(int argc, char **argv)
{
	FILE *in;
	char *filename, *inbuffer;
	size_t size;

	if (argc >= 2) {
		filename = argv[1];
		in = fopen(argv[1], "r");
		if (!in) {
			fprintf(stderr, "Cannot open file '%s'\n", filename);
			return 1;
		}

		fseek(in, 0, SEEK_END);
		size = ftell(in) + 1;
		fseek(in, 0, SEEK_SET);
	} else {
		filename = "<stdin>";
		size = STDIN_BUFSIZE;
		in = stdin;
	}

	inbuffer = malloc(size);
	if (inbuffer == NULL) {
		fprintf(stderr, "No memory for input buffer\n");
		return 2;
	}

	size = fread(inbuffer, 1, size, in);
	inbuffer[size] = 0;
	fclose(in);

	parse(filename, inbuffer);

	return 0;
}
