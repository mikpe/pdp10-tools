/*
 * expr.c
 */
#include <stdio.h>
#include "pdp10-arith.h"
#include "expr.h"

int eval(const struct expr *expr, struct value *value, int quiet)
{
    switch (expr->tag) {
    case E_UINTEGER:
	value->section = SECTION_ABS;
	value->offset = expr->u.e_uinteger.val;
	break;
    case E_SYMBOL:
	xxx;
	break;
    case E_UNARY:
	if (eval(expr->u.e_unary.expr, value, quiet) < 0)
	    return -1;
	if (value->section != SECTION_ABS) {
	    if (!quiet)
		fprintf(stderr, "as: %s(): unary operand is not absolute\n", __FUNCTION__);
	    return -1;
	}
	switch (expr->u.e_unary.unop) {
	case E_UMINUS:
	    value->offset = pdp10_neg_int36(value->offset);
	    break;
	case E_NOT:
	    value->offset = pdp10_not_int36(value->offset);
	    break;
	}
	break;
    case E_BINARY:
    {
	struct value value2;

	if (eval(expr->u.e_binary.expr1, value, quiet) < 0
	    || eval(expr->u.e_binary.expr2, &value2, quiet) < 0)
	    return -1;

	switch (expr->u.e_binary.binop) {
	case E_ADD:
	    if (value->section == SECTION_ABS)
		value->section = value2.section;
	    else if (value2.section == SECTION_ABS)
		;
	    else if (value->section == value2.section)
		;
	    else {
		if (!quiet)
		    fprintf(stderr, "as: %s(): adding operands from different sections\n", __FUNCTION__);
		return -1;
	    }
	    value->offset = pdp10_add_int36(value->offset, value2.offset);
	    return 0;
	case E_SUB:
	    if (value2.section == SECTION_ABS)
		;
	    else if (value->section == value2.section)
		value->section = SECTION_ABS;
	    else {
		if (!quiet)
		    fprintf(stderr, "as: %s(): subtracting operands from different sections\n", __FUNCTION__);
		return -1;
	    }
	    value->offset = pdp10_sub_int36(value->offset, value2.offset);
	    return 0;
	default:
	    break;
	}	    
	if (value->section != SECTION_ABS
	    || value2.section != SECTION_ABS) {
	    if (!quiet)
		fprintf(stderr, "as: %s(): binary sub-<expr> is not absolute\n", __FUNCTION__);
	    return -1;
	}
	switch (expr->u.e_binary.binop) {
	case E_MUL:
	    value->offset = pdp10_mul_int36(value->offset, value2.offset);
	    break;
	case E_DIV:
	    /* XXX: div-by-zero check */
	    value->offset = pdp10_div_int36(value->offset, value2.offset);
	    break;
	case E_REM:
	    /* XXX: div-by-zero check */
	    value->offset = pdp10_rem_int36(value->offset, value2.offset);
	    break;
	case E_LSHIFT:
	    /* XXX: range check */
	    value->offset = pdp10_asl_int36(value->offset, value2.offset);
	    break;
	case E_RSHIFT:
	    /* XXX: range check */
	    value->offset = pdp10_asr_int36(value->offset, value2.offset);
	    break;
	case E_OR:
	    value->offset = pdp10_or_int36(value->offset, value2.offset);
	    break;
	case E_AND:
	    value->offset = pdp10_and_int36(value->offset, value2.offset);
	    break;
	case E_XOR:
	    value->offset = pdp10_xor_int36(value->offset, value2.offset);
	    break;
	case E_ORNOT:
	    value->offset = pdp10_or_int36(value->offset, pdp10_not_int36(value2.offset));
	    break;
	case E_EQ:
	    value->offset = pdp10_eq_int36(value->offset, value2.offset) ? PDP10_UINT36_MAX : 0;
	    break;
	case E_NE:
	    value->offset = pdp10_ne_int36(value->offset, value2.offset) ? PDP10_UINT36_MAX : 0;
	    break;
	case E_LT:
	    value->offset = pdp10_lt_int36(value->offset, value2.offset) ? PDP10_UINT36_MAX : 0;
	    break;
	case E_GT:
	    value->offset = pdp10_gt_int36(value->offset, value2.offset) ? PDP10_UINT36_MAX : 0;
	    break;
	case E_GE:
	    value->offset = pdp10_ge_int36(value->offset, value2.offset) ? PDP10_UINT36_MAX : 0;
	    break;
	case E_LE:
	    value->offset = pdp10_le_int36(value->offset, value2.offset) ? PDP10_UINT36_MAX : 0;
	    break;
	case E_ANDAND:
	    value->offset = (pdp10_nonzero_int36(value->offset) && pdp10_nonzero_int36(value2.offset)) ? 1 : 0;
	    break;
	case E_OROR:
	    value->offset = (pdp10_nonzero_int36(value->offset) || pdp10_nonzero_int36(value2.offset)) ? 1 : 0;
	    break;
	default:
	    /* E_ADD / E_SUB cannot occur here, but the compiler may not see that */
	    break;
	}
	break;
    }
    }
    return 0;
}

int eval_abs(const struct expr *expr, pdp10_uint36_t *offset, int quiet)
{
    struct value value;

    if (eval(expr, &value, quiet) < 0)
	return -1;
    if (value.section != SECTION_ABS) {
	if (!quiet)
	    fprintf(stderr, "as: non-absolute expression\n");
	return -1;
    }
    *offset = value.offset;
    return 0;
}

int eval_abs_verbose(const struct expr *expr, pdp10_uint36_t *offset)
{
    return eval_abs(expr, offset, 0);
}

int eval_abs_quiet(const struct expr *expr, pdp10_uint36_t *offset)
{
    return eval_abs(expr, offset, 1);
}
