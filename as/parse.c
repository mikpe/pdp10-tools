/*
 * parse.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "emalloc.h"
#include "parse.h"
#include "scan.h"
#include "token.h"

static void error(const char *msg, enum token token, const union token_attribute *token_attr)
{
    fprintf(stderr, "Syntax error on line %u: %s; current token is ", scan_linenr, msg);
    token_print(stderr, token, token_attr);
    fprintf(stderr, "\n");
}

static enum token save_token = T_EOF;
static union token_attribute save_token_attr;

static enum token token_get(union token_attribute *token_attr)
{
    enum token token;

    token = save_token;
    if (token != T_EOF) {
	*token_attr = save_token_attr;
	save_token = T_EOF;
	return token;
    }
    return scan(token_attr);
}

static void token_unget(enum token token, const union token_attribute *token_attr)
{
    save_token = token;
    save_token_attr = *token_attr;
}

/*
 * Expression grammar:
 * The productions are listed from highest to lowest precedence.
 * All infix operators are left-associative.
 *
 * E	-> '(' E ')' | <uinteger> | <symbol> | -E | ~E
 *	 | E MOP E
 *	 | E BOP E
 *	 | E AOP E
 *	 | E && E
 *	 | E || E
 *
 * MOP	-> * | / | % | << | >>
 * BOP	-> '|' | & | ^ | !
 * AOP	-> + | - | == | <> | != | < | > | >= | <=
 */

static struct expr *make_expr(enum expr_tag tag)
{
    struct expr *expr;

    expr = emalloc(sizeof *expr);
    expr->tag = tag;
    return expr;
}

static void free_expr(struct expr *expr)
{
    if (expr) {
	switch (expr->tag) {
	case E_UNARY:
	    free_expr(expr->u.e_unary.expr);
	    break;
	case E_BINARY:
	    free_expr(expr->u.e_binary.expr1);
	    free_expr(expr->u.e_binary.expr2);
	    break;
	default:
	    break;
	}
	free(expr);
    }
}

static struct expr *make_uinteger_expr(pdp10_uint36_t val)
{
    struct expr *expr;

    expr = make_expr(E_UINTEGER);
    expr->u.e_uinteger.val = val;
    return expr;
}

static struct expr *make_symbol_expr(const struct strnode *name)
{
    struct expr *expr;

    expr = make_expr(E_SYMBOL);
    expr->u.e_symbol.name = name;
    return expr;
}

static struct expr *make_unary_expr(enum expr_unop unop, struct expr *expr1)
{
    struct expr *expr;

    expr = make_expr(E_UNARY);
    expr->u.e_unary.unop = unop;
    expr->u.e_unary.expr = expr1;
    return expr;
}

static struct expr *make_binary_expr(struct expr *expr1, enum expr_binop binop, struct expr *expr2)
{
    struct expr *expr;

    expr = make_expr(E_BINARY);
    expr->u.e_binary.expr1 = expr1;
    expr->u.e_binary.binop = binop;
    expr->u.e_binary.expr2 = expr2;
    return expr;
}

static struct expr *parse_expr(void);	/* forward */

static struct expr *parse_atomic_expr(void)
{
    enum token token;
    union token_attribute token_attr;
    struct expr *expr;

    token = token_get(&token_attr);
    switch (token) {
    case T_UINTEGER:
	return make_uinteger_expr(token_attr.uint);
    case T_SYMBOL:
	return make_symbol_expr(strtab_enter(token_attr.text));
    case T_LPAREN:
	expr = parse_expr();
	token = token_get(&token_attr);
	if (token != T_RPAREN) {
	    error("junk after expression, expected right parenthesis", token, &token_attr);
	    return NULL;
	}
	return expr;
    case T_MINUS:
	return make_unary_expr(E_UMINUS, parse_atomic_expr());
    case T_TILDE:
	return make_unary_expr(E_NOT, parse_atomic_expr());
    default:
	error("bad expression, expected integer, symbol, unary operator, or left parenthesis", token, &token_attr);
	return NULL;
    }
}

static struct expr *parse_multiplicative_expr(void)
{
    struct expr *expr1;
    struct expr *expr2;
    enum token token;
    union token_attribute token_attr;
    enum expr_binop binop;

    expr1 = parse_atomic_expr();
    if (!expr1)
	return NULL;

    for (;;) {
	token = token_get(&token_attr);
	switch (token) {
	case T_MUL:
	    binop = E_MUL;
	    break;
	case T_DIV:
	    binop = E_DIV;
	    break;
	case T_REM:
	    binop = E_REM;
	    break;
	case T_LSHIFT:
	    binop = E_LSHIFT;
	    break;
	case T_RSHIFT:
	    binop = E_RSHIFT;
	    break;
	default:
	    token_unget(token, &token_attr);
	    return expr1;
	}
	expr2 = parse_atomic_expr();
	if (!expr2)
	    return NULL;
	expr1 = make_binary_expr(expr1, binop, expr2);
    }
}

static struct expr *parse_bitwise_expr(void)
{
    struct expr *expr1;
    struct expr *expr2;
    enum token token;
    union token_attribute token_attr;
    enum expr_binop binop;

    expr1 = parse_multiplicative_expr();
    if (!expr1)
	return NULL;

    for (;;) {
	token = token_get(&token_attr);
	switch (token) {
	case T_OR:
	    binop = E_OR;
	    break;
	case T_AND:
	    binop = E_AND;
	    break;
	case T_CARET:
	    binop = E_XOR;
	    break;
	case T_BANG:
	    binop = E_ORNOT;
	    break;
	default:
	    token_unget(token, &token_attr);
	    return expr1;
	}
	expr2 = parse_multiplicative_expr();
	if (!expr2)
	    return NULL;
	expr1 = make_binary_expr(expr1, binop, expr2);
    }
}

static struct expr *parse_additive_expr(void)
{
    struct expr *expr1;
    struct expr *expr2;
    enum token token;
    union token_attribute token_attr;
    enum expr_binop binop;

    expr1 = parse_bitwise_expr();
    if (!expr1)
	return NULL;

    for (;;) {
	token = token_get(&token_attr);
	switch (token) {
	case T_PLUS:
	    binop = E_ADD;
	    break;
	case T_MINUS:
	    binop = E_SUB;
	    break;
	case T_EQEQ:
	    binop = E_EQ;
	    break;
	case T_NEQ:
	    binop = E_NE;
	    break;
	case T_LT:
	    binop = E_LT;
	    break;
	case T_GT:
	    binop = E_GT;
	    break;
	case T_GE:
	    binop = E_GE;
	    break;
	case T_LE:
	    binop = E_LE;
	    break;
	default:
	    token_unget(token, &token_attr);
	    return expr1;
	}
	expr2 = parse_bitwise_expr();
	if (!expr2)
	    return NULL;
	expr1 = make_binary_expr(expr1, binop, expr2);
    }
}

static struct expr *parse_andand_expr(void)
{
    struct expr *expr1;
    struct expr *expr2;
    enum token token;
    union token_attribute token_attr;

    expr1 = parse_additive_expr();
    if (!expr1)
	return NULL;

    for (;;) {
	token = token_get(&token_attr);
	if (token != T_ANDAND) {
	    token_unget(token, &token_attr);
	    return expr1;
	}
	expr2 = parse_additive_expr();
	if (!expr2)
	    return NULL;
	expr1 = make_binary_expr(expr1, E_ANDAND, expr2);
    }
}

static struct expr *parse_oror_expr(void)
{
    struct expr *expr1;
    struct expr *expr2;
    enum token token;
    union token_attribute token_attr;

    expr1 = parse_andand_expr();
    if (!expr1)
	return NULL;

    for (;;) {
	token = token_get(&token_attr);
	if (token != T_OROR) {
	    token_unget(token, &token_attr);
	    return expr1;
	}
	expr2 = parse_andand_expr();
	if (!expr2)
	    return NULL;
	expr1 = make_binary_expr(expr1, E_OROR, expr2);
    }
}

static struct expr *parse_expr(void)
{
    return parse_oror_expr();
}

static int parse_expr_opt(struct expr **exprp)
{
    enum token token;
    union token_attribute token_attr;

    token = token_get(&token_attr);
    token_unget(token, &token_attr);
    switch (token) {
    case T_UINTEGER:
    case T_SYMBOL:
    case T_LPAREN:
    case T_MINUS:
    case T_TILDE:
	*exprp = parse_expr();
	return *exprp == NULL ? -1 : 1;
    default:
	return 0;
    }
}

static int parse_dot_align(struct stmt *stmt, unsigned int flags)
{
    enum token token;
    union token_attribute token_attr;
    struct expr *expr;

    stmt->tag = S_ALIGN;
    stmt->u.s_align.flags = flags;
    expr = parse_expr();
    if (!expr)
	return -1;
    stmt->u.s_align.balign = expr;

    token = token_get(&token_attr);
    if (token == T_COMMA) {
	expr = parse_expr();
	if (!expr)
	    return -1;
	stmt->u.s_align.fill = expr;
	token = token_get(&token_attr);
	if (token == T_COMMA) {
	    expr = parse_expr();
	    if (!expr)
		return -1;
	    stmt->u.s_align.maxskip = expr;
	    token = token_get(&token_attr);
	}
    }
    if (token != T_NEWLINE) {
	error("junk after .{,b,p2}align{,w,l} directive", token, &token_attr);
	return -1;
    }
    return 1;
}

static struct string_list *make_string_list(const struct strnode *string, struct string_list *next)
{
    struct string_list *list;

    list = emalloc(sizeof *list);
    list->string = string;
    list->next = next;
    return list;
}

static int parse_string_list(struct stmt *stmt, enum stmt_tag tag)
{
    enum token token;
    union token_attribute token_attr;
    struct string_list **nextp;
    struct string_list *temp;

    stmt->tag = tag;
    stmt->u.s_string_list.list = NULL;
    nextp = &stmt->u.s_string_list.list;

    token = token_get(&token_attr);
    if (token == T_STRING)
	for (;;) {
	    temp = make_string_list(strtab_enter(token_attr.text), NULL);
	    *nextp = temp;
	    nextp = &temp->next;
	    token = token_get(&token_attr);
	    if (token != T_COMMA)
		break;
	    token = token_get(&token_attr);
	    if (token != T_STRING) {
		error("junk after ',' in .asci{i,z} directive", token, &token_attr);
		return -1;
	    }
	}

    if (token != T_NEWLINE) {
	error("junk after .asci{i,z} directive", token, &token_attr);
	return -1;
    }
    return 1;
}

static struct expr_list *make_expr_list(struct expr *expr, struct expr_list *next)
{
    struct expr_list *list;

    list = emalloc(sizeof *list);
    list->expr = expr;
    list->next = next;
    return list;
}

static int parse_expr_list(struct stmt *stmt, enum stmt_tag tag)
{
    enum token token;
    union token_attribute token_attr;
    struct expr_list **nextp;
    struct expr_list *temp;
    struct expr *expr;
    int status;

    stmt->tag = tag;
    stmt->u.s_expr_list.list = NULL;
    nextp = &stmt->u.s_expr_list.list;

    status = parse_expr_opt(&expr);
    if (status < 0)
	return -1;
    else if (status == 0)
	token = token_get(&token_attr);
    else
	for (;;) {
	    temp = make_expr_list(expr, NULL);
	    *nextp = temp;
	    nextp = &temp->next;
	    token = token_get(&token_attr);
	    if (token != T_COMMA)
		break;
	    expr = parse_expr();
	    if (!expr)
		return -1;
	}

    if (token != T_NEWLINE) {
	error("junk after .{byte,long,short} directive", token, &token_attr);
	return -1;
    }
    return 1;
}

static int parse_string(struct stmt *stmt, enum stmt_tag tag)
{
    enum token token;
    union token_attribute token_attr;

    token = token_get(&token_attr);
    if (token == T_STRING) {
	stmt->u.s_string.string = strtab_enter(token_attr.text);
	stmt->tag = tag;
	token = token_get(&token_attr);
	if (token == T_NEWLINE)
	    return 1;
    }
    error("junk after .file/.ident directive", token, &token_attr);
    return -1;
}

static int parse_dot_popsection(struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;

    token = token_get(&token_attr);
    if (token == T_NEWLINE) {
	stmt->tag = S_POPSECTION;
	return 1;
    }
    error("junk after .popsection directive", token, &token_attr);
    return -1;
}

static int parse_dot_previous(struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;

    token = token_get(&token_attr);
    if (token == T_NEWLINE) {
	stmt->tag = S_PREVIOUS;
	return 1;
    }
    error("junk after .previous directive", token, &token_attr);
    return -1;
}

static int parse_dot_subsection(struct stmt *stmt)
{
    struct expr *expr;
    enum token token;
    union token_attribute token_attr;

    expr = parse_expr();
    if (!expr)
	return -1;
    stmt->u.s_subsection.expr = expr;

    token = token_get(&token_attr);
    if (token == T_NEWLINE) {
	stmt->tag = S_SUBSECTION;
	return 1;
    }
    error("junk after .subsection directive", token, &token_attr);
    return -1;
}

static int parse_name_list(struct stmt *stmt, enum stmt_tag tag)
{
    enum token token;
    union token_attribute token_attr;
    struct string_list **nextp;
    struct string_list *temp;

    stmt->tag = tag;
    stmt->u.s_string_list.list = NULL;
    nextp = &stmt->u.s_string_list.list;

    token = token_get(&token_attr);
    if (token == T_SYMBOL)
	for (;;) {
	    temp = make_string_list(strtab_enter(token_attr.text), NULL);
	    *nextp = temp;
	    nextp = &temp->next;
	    token = token_get(&token_attr);
	    if (token != T_COMMA)
		break;
	    token = token_get(&token_attr);
	    if (token != T_SYMBOL) {
		error("junk after ',' in .globl/.hidden/.internal/.local/.protected directive", token, &token_attr);
		return -1;
	    }
	}

    if (token != T_NEWLINE) {
	error("junk after .globl/.hidden/.internal/.local/.protected directive", token, &token_attr);
	return -1;
    }
    return 1;
}

static int parse_dot_comm(struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;
    struct expr *expr;

    token = token_get(&token_attr);
    if (token == T_SYMBOL) {
	stmt->u.s_comm.name = strtab_enter(token_attr.text);
	token = token_get(&token_attr);
	if (token == T_COMMA) {
	    expr = parse_expr();
	    if (!expr)
		return -1;
	    stmt->u.s_comm.length = expr;
	    token = token_get(&token_attr);
	    if (token == T_COMMA) {
		expr = parse_expr();
		if (!expr)
		    return -1;
		stmt->u.s_comm.balign = expr;
		token = token_get(&token_attr);
	    } else
		stmt->u.s_comm.balign = NULL;
	    if (token == T_NEWLINE) {
		stmt->tag = S_COMM;
		return 1;
	    }
	}
    }
    error("junk after .comm directive", token, &token_attr);
    return -1;
}

static int parse_dot_org(struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;
    struct expr *expr;

    expr = parse_expr();
    if (!expr)
	return -1;
    stmt->u.s_org.newlc = expr;
    token = token_get(&token_attr);
    if (token == T_COMMA) {
	expr = parse_expr();
	if (!expr)
	    return -1;
	stmt->u.s_org.fill = expr;
	token = token_get(&token_attr);
    } else
	stmt->u.s_org.fill = NULL;
    if (token == T_NEWLINE) {
	stmt->tag = S_ORG;
	return 1;
    }
    error("junk after .org directive", token, &token_attr);
    return -1;
}

static int parse_setsize(struct stmt *stmt, enum stmt_tag tag)
{
    enum token token;
    union token_attribute token_attr;
    struct expr *expr;

    token = token_get(&token_attr);
    if (token == T_SYMBOL) {
	stmt->u.s_setsize.name = strtab_enter(token_attr.text);
	token = token_get(&token_attr);
	if (token == T_COMMA) {
	    expr = parse_expr();
	    if (!expr)
		return -1;
	    stmt->u.s_setsize.expr = expr;
	    token = token_get(&token_attr);
	    if (token == T_NEWLINE) {
		stmt->tag = tag;
		return 1;
	    }
	}
    }
    error("junk after .set/.size directive", token, &token_attr);
    return -1;
}

static int parse_sh_flags(Elf36_Word *sh_flags)
{
    enum token token;
    union token_attribute token_attr;
    const char *f;
    Elf36_Word flag;

    token = token_get(&token_attr);
    if (token == T_STRING) {
	f = token_attr.text;
	for (; ; *sh_flags |= flag, ++f) {
	    switch (*f) {
	    case 'a':
		flag = SHF_ALLOC;
		continue;
	    case 'e':
		flag = SHF_EXCLUDE;
		continue;
	    case 'w':
		flag = SHF_WRITE;
		continue;
	    case 'M':
		flag = SHF_MERGE;
		continue;
	    case 'S':
		flag = SHF_STRINGS;
		continue;
	    case 'G':
		if (*sh_flags & (1 << 19))
		    break;
		flag = SHF_GROUP;
		continue;
	    case 'T':
		flag = SHF_TLS;
		continue;
	    case '?':
		if (*sh_flags & SHF_GROUP)
		    break;
		flag = 1 << 19;	/* XXX: FAKE FAKE FAKE */
		continue;
	    case '\0':
		/* XXX: free the string? */
		return 1;
	    default:
		break;
	    }
	    break;
	}
    }
    error("invalid section flags", token, &token_attr);
    return -1;
}

static int parse_sh_type(Elf36_Word *sh_type)
{
    enum token token;
    union token_attribute token_attr;
    Elf36_Word type;

    token = token_get(&token_attr);
    if (token == T_AT) {
	token = token_get(&token_attr);
	if (token == T_SYMBOL) {
	    do {
		if (strcmp(token_attr.text, "progbits") == 0)
		    type = SHT_PROGBITS;
		else if (strcmp(token_attr.text, "nobits") == 0)
		    type = SHT_NOBITS;
		else if (strcmp(token_attr.text, "note") == 0)
		    type = SHT_NOTE;
		else if (strcmp(token_attr.text, "init_array") == 0)
		    type = SHT_INIT_ARRAY;
		else if (strcmp(token_attr.text, "fini_array") == 0)
		    type = SHT_FINI_ARRAY;
		else if (strcmp(token_attr.text, "preinit_array") == 0)
		    type = SHT_PREINIT_ARRAY;
		else
		    break;
		*sh_type = type;
		return 1;
	    } while (0);
	}
    }
    error("invalid section type", token, &token_attr);
    return -1;
}

static int parse_dot_section(struct stmt *stmt, int is_pushsection)
{
    enum token token;
    union token_attribute token_attr;

    stmt->u.s_section.subsectnr = NULL;
    stmt->u.s_section.sh_flags = 0;
    stmt->u.s_section.sh_type = 0;
    stmt->u.s_section.sh_entsize = NULL;
    stmt->u.s_section.groupname = NULL;
    stmt->u.s_section.linkage = NULL;

    token = token_get(&token_attr);
    if (token == T_SYMBOL) {
	stmt->u.s_section.name = strtab_enter(token_attr.text);
	token = token_get(&token_attr);
	do {
	    if (is_pushsection) {
		if (token != T_COMMA)
		    break;
		stmt->u.s_section.subsectnr = parse_expr();
		if (!stmt->u.s_section.subsectnr)
		    return -1;
		token = token_get(&token_attr);
	    }
	    if (token != T_COMMA)
		break;
	    if (parse_sh_flags(&stmt->u.s_section.sh_flags) < 1)
		return -1;
	    token = token_get(&token_attr);
	    if (token != T_COMMA) {
		if (stmt->u.s_section.sh_flags & (SHF_MERGE | SHF_GROUP)) {
		    error("section flags with M and/or G not followed by ,@type", token, &token_attr);
		    return -1;
		}
		break;
	    }
	    if (parse_sh_type(&stmt->u.s_section.sh_type) < 0)
		return -1;
	    token = token_get(&token_attr);
	    if (stmt->u.s_section.sh_flags & SHF_MERGE) {
		if (token != T_COMMA) {
		    error("section flags with M not followed by entsize", token, &token_attr);
		    return -1;
		}
		stmt->u.s_section.sh_entsize = parse_expr();
		if (!stmt->u.s_section.sh_entsize)
		    return -1;
		token = token_get(&token_attr);
	    }
	    if (stmt->u.s_section.sh_flags & SHF_GROUP) {
		if (token != T_COMMA) {
		    error("section flags with G not followed by groupname", token, &token_attr);
		    return -1;
		}
		token = token_get(&token_attr);
		if (token != T_SYMBOL) {
		    error("section group with invalid groupname", token, &token_attr);
		    return -1;
		}
		stmt->u.s_section.groupname = strtab_enter(token_attr.text);
		token = token_get(&token_attr);
		if (token != T_COMMA)
		    break;
		token = token_get(&token_attr);
		if (token != T_SYMBOL) {
		    error("section group with invalid linkage", token, &token_attr);
		    return -1;
		}
		stmt->u.s_section.linkage = strtab_enter(token_attr.text);
		token = token_get(&token_attr);
	    }
	} while (0);
	if (token == T_NEWLINE) {
	    stmt->tag = is_pushsection ? S_PUSHSECTION : S_SECTION;
	    return 1;
	}
    }
    error("junk after .section/.pushsection directive", token, &token_attr);
    return -1;
}

static int parse_oldstyle_section(struct stmt *stmt, const char *name)
{
    enum token token;
    union token_attribute token_attr;

    stmt->u.s_section.subsectnr = NULL;
    stmt->u.s_section.sh_flags = 0;
    stmt->u.s_section.sh_type = 0;
    stmt->u.s_section.sh_entsize = NULL;
    stmt->u.s_section.groupname = NULL;
    stmt->u.s_section.linkage = NULL;

    stmt->u.s_section.name = strtab_enter(name);

    token = token_get(&token_attr);
    if (token == T_COMMA) {
	stmt->u.s_section.subsectnr = parse_expr();
	if (!stmt->u.s_section.subsectnr)
	    return -1;
	token = token_get(&token_attr);
    }
    if (token == T_NEWLINE) {
	stmt->tag = S_SECTION;
	return 1;
    }
    error("junk after old-style section directive", token, &token_attr);
    return -1;
}

/*
 * Recognize:
 *
 * <symbol> ":"
 * <symbol> "=" <expr> "\n"
 * <symbol> ((<uinteger> | <register>) ",")? ("@"? <expr> ("(" (<uinteger> | <register>) ")")?)? "\n"
 * <symbol> <register> "\n"		[equivalent to <symbol> <register> "," "\n"]
 */
static int make_insn(struct stmt *stmt, union token_attribute *symbol_attr, unsigned int accumulator, int at, struct expr *expr, unsigned int indexreg)
{
    stmt->tag = S_INSN;
    stmt->u.s_insn.name = strtab_enter(symbol_attr->text);
    stmt->u.s_insn.accumulator = accumulator;
    stmt->u.s_insn.at = at;
    stmt->u.s_insn.expr = expr;
    stmt->u.s_insn.indexreg = indexreg;
    return 1;
}

static int parse_insn_after_lparen(struct stmt *stmt, union token_attribute *symbol_attr, unsigned int accumulator, int at, struct expr *expr)
{
    enum token token;
    union token_attribute token_attr;

    token = token_get(&token_attr);
    if (token == T_UINTEGER || token == T_REGISTER) {
	unsigned int indexreg;

	indexreg = token_attr.uint;

	token = token_get(&token_attr);
	if (token == T_RPAREN) {
	    token = token_get(&token_attr);
	    if (token == T_NEWLINE)
		return make_insn(stmt, symbol_attr, accumulator, at, expr, indexreg);
	}
    }
    error("junk in of after index expression", token, &token_attr);
    return -1;
}

static int parse_insn_expr(struct stmt *stmt, union token_attribute *symbol_attr, unsigned int accumulator, int at)
{
    struct expr *expr;
    enum token token;
    union token_attribute token_attr;

    expr = parse_expr();
    if (!expr)
	return -1;
    
    token = token_get(&token_attr);
    switch (token) {
    case T_NEWLINE:
	return make_insn(stmt, symbol_attr, accumulator, at, expr, 0);
    case T_LPAREN:
	return parse_insn_after_lparen(stmt, symbol_attr, accumulator, at, expr);
    default:
	error("junk after <symbol> <expr>", token, &token_attr);
	return -1;
    }
}

static int parse_insn_after_comma(struct stmt *stmt, union token_attribute *symbol_attr, unsigned int accumulator)
{
    enum token token;
    union token_attribute token_attr;

    token = token_get(&token_attr);
    switch (token) {
    case T_NEWLINE:
	return make_insn(stmt, symbol_attr, accumulator, 0, NULL, 0);
    case T_AT:
	return parse_insn_expr(stmt, symbol_attr, accumulator, 1);
    case T_UINTEGER:
    case T_LPAREN:
    case T_MINUS:
    case T_TILDE:
	token_unget(token, &token_attr);
	return parse_insn_expr(stmt, symbol_attr, accumulator, 0);
    default:
	error("junk after <symbol> <register> ','", token, &token_attr);
	return -1;
    }
}

static int parse_symbol(struct stmt *stmt, union token_attribute *symbol_attr)
{
    enum token token;
    union token_attribute token_attr;

    token = token_get(&token_attr);
    switch (token) {
    case T_COLON:
	stmt->u.s_string.string = strtab_enter(symbol_attr->text);
	stmt->tag = S_LABEL;
	return 1;
    case T_EQ:	/* <symbol>=<expr>\n --> .set <symbol>,<expr>\n */
    {
	stmt->u.s_setsize.expr = parse_expr();
	if (stmt->u.s_setsize.expr != NULL) {
	    token = token_get(&token_attr);
	    if (token == T_NEWLINE) {
		stmt->u.s_setsize.name = strtab_enter(symbol_attr->text);
		stmt->tag = S_SET;
		return 1;
	    }
	}
	error("junk after <symbol>=", token, &token_attr);
	return -1;
    }
    case T_AT:
	/* the <expr> part needs to be present to avoid ambiguities with the "(<uinteger>)" index part */
	return parse_insn_expr(stmt, symbol_attr, 0, 1);
    case T_REGISTER:
    {
	union token_attribute token_attr2;

	token = token_get(&token_attr2);
	if (token == T_COMMA)
	    return parse_insn_after_comma(stmt, symbol_attr, token_attr.uint);
	if (token == T_NEWLINE)
	    return make_insn(stmt, symbol_attr, token_attr.uint, 0, NULL, 0);
	error("junk after <opcode> <register>", token, &token_attr2);
	return -1;
    }
    case T_NEWLINE:
	return make_insn(stmt, symbol_attr, 0, 0, NULL, 0);
    case T_UINTEGER:
    {
	/*
	 * This <uinteger> may be the <accumulator> part of the instruction,
	 * or the start of the <expr> part.  In the first case it's followed
	 * by ",", and in the second case it's followed by <operator>, "(",
	 * or "\n", while the entire <expr> is followed by "(" or "\n".
	 *
	 * We could distinguish between these two cases by scanning another token,
	 * but then in the <expr> case we'd have to either push back two tokens,
	 * or rewrite parse_expr() so that it could be entered with the left-most
	 * <atomic expr> already parsed.  Both options are doable, but inefficient
	 * or requiring ugly code.
	 *
	 * Instead we push back the initial <uinteger> and parse <expr>.  Afterwards
	 * we inspect the parse tree of <expr> and the next token, and from that we
	 * can identify which case applied, and complete the parse.
	 */
	struct expr *expr;
	union token_attribute token_attr2;

	token_unget(token, &token_attr);

	expr = parse_expr();
	if (!expr)
	    return -1;

	token = token_get(&token_attr2);
	switch (token) {
	case T_COMMA:
	    if (expr->tag == E_UINTEGER) {	/* <symbol> <uinteger> "," ... */
		free_expr(expr);
		return parse_insn_after_comma(stmt, symbol_attr, token_attr.uint);
	    }
	    break;	/* <symbol> <expr> "," ... is a syntax error */
	case T_NEWLINE:	/* <symbol> <expr> "\n" */
	    return make_insn(stmt, symbol_attr, 0, 0, expr, 0);
	case T_LPAREN:	/* <symbol> <expr> "(" ... */
	    return parse_insn_after_lparen(stmt, symbol_attr, 0, 0, expr);
	default:
	    break;
	}
	error("junk after <symbol> <expr>", token, &token_attr2);
	return -1;
    }
	/* the following four are FIRST(<expr>) \ {T_UINTEGER}, see parse_expr_opt() */
    case T_SYMBOL:
    case T_LPAREN:
    case T_MINUS:
    case T_TILDE:
	token_unget(token, &token_attr);
	return parse_insn_expr(stmt, symbol_attr, 0, 0);
    default:
	error("junk after <symbol>", token, &token_attr);
	return -1;
    }
}

int parse_stmt(struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;

    for (;;) {
	token = token_get(&token_attr);
	switch (token) {
	    /*
	     * directives
	     */
	case T_DOT_ALIGN:
	    return parse_dot_align(stmt, 0 | (0 << 1));
	case T_DOT_BALIGN:
	    return parse_dot_align(stmt, 0 | (0 << 1));
	case T_DOT_P2ALIGN:
	    return parse_dot_align(stmt, 1 | (0 << 1));
	case T_DOT_ASCII:
	    return parse_string_list(stmt, S_ASCII);
	case T_DOT_ASCIZ:
	    return parse_string_list(stmt, S_ASCIZ);
	case T_DOT_BYTE:
	    return parse_expr_list(stmt, S_BYTE);
	case T_DOT_LONG:
	    return parse_expr_list(stmt, S_LONG);
	case T_DOT_SHORT:
	    return parse_expr_list(stmt, S_SHORT);
	case T_DOT_FILE:
	    return parse_string(stmt, S_FILE);
	case T_DOT_IDENT:
	    return parse_string(stmt, S_IDENT);
	case T_DOT_POPSECTION:
	    return parse_dot_popsection(stmt);
	case T_DOT_PREVIOUS:
	    return parse_dot_previous(stmt);
	case T_DOT_SUBSECTION:
	    return parse_dot_subsection(stmt);
	case T_DOT_GLOBL:
	    return parse_name_list(stmt, S_GLOBL);
	case T_DOT_HIDDEN:
	    return parse_name_list(stmt, S_HIDDEN);
	case T_DOT_INTERNAL:
	    return parse_name_list(stmt, S_INTERNAL);
	case T_DOT_LOCAL:
	    return parse_name_list(stmt, S_LOCAL);
	case T_DOT_PROTECTED:
	    return parse_name_list(stmt, S_PROTECTED);
	case T_DOT_WEAK:
	    return parse_name_list(stmt, S_WEAK);
	case T_DOT_COMM:
	    return parse_dot_comm(stmt);
	case T_DOT_ORG:
	    return parse_dot_org(stmt);
	case T_DOT_SET:
	    return parse_setsize(stmt, S_SET);
	case T_DOT_SIZE:
	    return parse_setsize(stmt, S_SIZE);
	case T_DOT_SECTION:
	    return parse_dot_section(stmt, 0);
	case T_DOT_PUSHSECTION:
	    return parse_dot_section(stmt, 1);
	case T_DOT_BSS:
	    return parse_oldstyle_section(stmt, ".bss");
	case T_DOT_DATA:
	    return parse_oldstyle_section(stmt, ".data");
	case T_DOT_RODATA:
	    return parse_oldstyle_section(stmt, ".rodata");
	case T_DOT_TEXT:
	    return parse_oldstyle_section(stmt, ".text");
#if 0
	case T_DOT_SYMVER:
	case T_DOT_TYPE:
	case T_DOT_WEAKREF:
#endif
	    /*
	     * other symbols
	     */
	case T_SYMBOL:	/* start of label, insn, or symbol assignment */
	    return parse_symbol(stmt, &token_attr);

	    /*
	     * literals
	     */
#if 0
	case T_UINTEGER:	/* start of local label defn */
#endif
	    /*
	     * misc
	     */
	case T_ERROR:
	    return -1;	/* diagnostics already emitted by scan.c */
	case T_EOF:
	    return 0;
	case T_NEWLINE:
	    continue;
	default:
	    error("expected directive, label, or instruction", token, &token_attr);
	    return -1;
	}
    }
}
