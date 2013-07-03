/*
 * expr.h
 */
#ifndef EXPR_H
#define EXPR_H

#include "pdp10-elf36.h"
#include "section.h"
#include "strtab.h"

enum expr_tag {
    E_UINTEGER,
    E_SYMBOL,
    E_UNARY,
    E_BINARY,
};

enum expr_unop {
    E_UMINUS,
    E_NOT,
};

enum expr_binop {
    E_MUL,
    E_DIV,
    E_REM,
    E_LSHIFT,
    E_RSHIFT,
    E_OR,
    E_AND,
    E_XOR,
    E_ORNOT,
    E_ADD,
    E_SUB,
    E_EQ,
    E_NE,
    E_LT,
    E_GT,
    E_GE,
    E_LE,
    E_ANDAND,
    E_OROR,
};

struct expr {
    enum expr_tag tag;
    union {
	struct {
	    pdp10_uint36_t val;
	} e_uinteger;
	struct {
	    const struct strnode *name;
	} e_symbol;
	struct {
	    enum expr_unop unop;
	    struct expr *expr;
	} e_unary;
	struct {
	    enum expr_binop binop;
	    struct expr *expr1;
	    struct expr *expr2;
	} e_binary;
    } u;
};

struct value {
    struct section *section;
    pdp10_uint36_t offset;
};

int eval(const struct expr *expr, struct value *value, int quiet);
int eval_abs(const struct expr *expr, pdp10_uint36_t *offset, int quiet);
int eval_abs_verbose(const struct expr *expr, pdp10_uint36_t *offset);
int eval_abs_quiet(const struct expr *expr, pdp10_uint36_t *offset);

#endif /* EXPR_H */
