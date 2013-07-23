/*
 * parse.c
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pdp10-opcodes.h"
#include "input.h"	/* for struct stmt */
#include "scan.h"
#include "token.h"

static int error(struct scan_state *scan_state, const char *msg, enum token token, const union token_attribute *token_attr)
{
    fprintf(stderr, "%s: %s line %u: syntax error: %s; current token is ",
	    scan_state->progname, scan_state->filename, scan_state->linenr, msg);
    token_print(stderr, token, token_attr);
    fprintf(stderr, "\n");
    return -1;
}

static int parse_dot_globl(struct scan_state *scan_state, struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;

    token = scan_token(scan_state, &token_attr);
    if (token == T_SYMBOL) {
	stmt->u.symbol.name = token_attr.text;
	token = scan_token(scan_state, &token_attr);
	if (token == T_NEWLINE) {
	    stmt->tag = S_DOT_GLOBL;
	    return 1;
	}
    }
    return error(scan_state, "junk after .globl directive", token, &token_attr);
}

static int parse_dot_text(struct scan_state *scan_state, struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;

    token = scan_token(scan_state, &token_attr);
    if (token == T_NEWLINE) {
	stmt->tag = S_DOT_TEXT;
	return 1;
    }
    return error(scan_state, "junk after .text directive", token, &token_attr);
}

/*
 * Recognize:
 *
 * <label> ::= <symbol> ":"
 *
 * <insn> ::= <symbol> (<accumulator> ",")? <address> <newline>
 *
 * <accumulator> ::= <uinteger> [uint <= 0xF]
 *
 * <address> ::= "@"? <displacement>? <index>?
 *
 * <displacement> ::= <uinteger> [uint <= 1^18 - 1]
 * <displacement> ::= "(" <uinteger> ")" [uint <= 1^18 - 1]
 *
 * <index> ::= "(" <indexreg> ")"
 * <indexreg> ::= <uinteger> [uint <= 0xF]
 *
 * Examples:
 * foo:
 * popj 17,
 * pushj 17,bar
 * movei 1,@fum(2)
 *
 * Ambiguous examples:
 *
 * <symbol> (<uinteger>) <newline>
 *
 * This is ambigouous since we have no special notation for <register>, and the same kind of
 * parentheses are used for expression grouping in the displacement as for the index register.
 *
 * This might denote an insn with a parenthesized displacement and no index,
 * or it might denote an insn with an index but no displacement.
 *
 * However, the uinteger in an indexreg cannot be > 0xF, and it rarely makes sense to form
 * an effective address with a displacement <= 0xF and no index.
 *
 * Therefore, if the uinteger is <= 0xF this is an index with no displacement,
 * otherwise it is a displacement without an index.
 */

static int parse_insn_index_after_lparen(struct scan_state *scan_state, struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;

    token = scan_token(scan_state, &token_attr);
    if (token != T_UINTEGER
	|| token_attr.uint > 0xF)
	return error(scan_state, "invalid <indexreg>", token, &token_attr);

    stmt->u.insn.indexreg = token_attr.uint;

    token = scan_token(scan_state, &token_attr);
    if (token != T_RPAREN)
	return error(scan_state, "junk after '(' <indexreg>", token, &token_attr);

    token = scan_token(scan_state, &token_attr);
    if (token != T_NEWLINE)
	return error(scan_state, "junk after '(' <indexreg> ')'", token, &token_attr);

    return 1;
}

static int parse_insn_address_after_lparen_uinteger_rparen(struct scan_state *scan_state, struct stmt *stmt, union token_attribute *uinteger_attr)
{
    enum token token;
    union token_attribute token_attr;

    token = scan_token(scan_state, &token_attr);
    switch (token) {
    case T_NEWLINE:	/* might be <displacement> or <index>, inspect the <uinteger>'s value to disambiguate */
	if (uinteger_attr->uint > PDP10_UINT18_MAX)
	    return error(scan_state, "invalid <displacement>", T_UINTEGER, uinteger_attr);
	if (uinteger_attr->uint <= 0xF)	/* it's the <index> */
	    stmt->u.insn.indexreg = uinteger_attr->uint;
	else				/* it's the <displacement> */
	    stmt->u.insn.address = uinteger_attr->uint;
	return 1;
    case T_LPAREN:	/* the <uinteger> is the <displacement>, followed by <index> */
	if (uinteger_attr->uint > PDP10_UINT18_MAX)
	    return error(scan_state, "invalid <displacement>", T_UINTEGER, uinteger_attr);
	stmt->u.insn.address = uinteger_attr->uint;
	return parse_insn_index_after_lparen(scan_state, stmt);
    default:
	return error(scan_state, "junk in <address> after '(' <uinteger> ')'", token, &token_attr);
    }
}

static int parse_insn_address_after_lparen_uinteger(struct scan_state *scan_state, struct stmt *stmt, union token_attribute *uinteger_attr)
{
    enum token token;
    union token_attribute token_attr;

    token = scan_token(scan_state, &token_attr);
    switch (token) {
    case T_RPAREN:	/* might be <displacement> or <index> */
	return parse_insn_address_after_lparen_uinteger_rparen(scan_state, stmt, uinteger_attr);
    default:
	return error(scan_state, "junk in <address> after '(' <uinteger>", token, &token_attr);
    }
}

static int parse_insn_address_after_lparen(struct scan_state *scan_state, struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;

    token = scan_token(scan_state, &token_attr);
    switch (token) {
    case T_UINTEGER:	/* might be <displacement> or <index> */
	return parse_insn_address_after_lparen_uinteger(scan_state, stmt, &token_attr);
    default:
	return error(scan_state, "junk in <address> after '('", token, &token_attr);
    }
}

static int parse_insn_after_displacement(struct scan_state *scan_state, struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;

    token = scan_token(scan_state, &token_attr);
    switch (token) {
    case T_NEWLINE:	/* no <index> */
	return 1;
    case T_LPAREN:	/* need <index> */
	return parse_insn_index_after_lparen(scan_state, stmt);
    default:
	return error(scan_state, "junk in <address> after <displacement>", token, &token_attr);
    }
}

static int parse_insn_address_after_at(struct scan_state *scan_state, struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;

    token = scan_token(scan_state, &token_attr);
    switch (token) {
    case T_NEWLINE:
	return 1;
    case T_LPAREN:	/* might be <displacement> or <index> */
	return parse_insn_address_after_lparen(scan_state, stmt);
    case T_UINTEGER:
	if (token_attr.uint > PDP10_UINT18_MAX)
	    return error(scan_state, "invalid <displacement>", token, &token_attr);
	stmt->u.insn.address = token_attr.uint;
	return parse_insn_after_displacement(scan_state, stmt);
    default:
	return error(scan_state, "invalid <address>", token, &token_attr);
    }
}

static int parse_insn_address(struct scan_state *scan_state, struct stmt *stmt, const struct pdp10_instruction *insndesc)
{
    enum token token;
    union token_attribute token_attr;

    token = scan_token(scan_state, &token_attr);
    if (token == T_NEWLINE)
	return 1;

    if (insndesc->type & PDP10_E_UNUSED)
	return error(scan_state, "<address> not allowed in this instruction", token, &token_attr);

    switch (token) {
    case T_LPAREN:	/* might be <displacement> or <index> */
	return parse_insn_address_after_lparen(scan_state, stmt);
    case T_UINTEGER:
	if (token_attr.uint > PDP10_UINT18_MAX)
	    return error(scan_state, "invalid <displacement>", token, &token_attr);
	stmt->u.insn.address = token_attr.uint;
	return parse_insn_after_displacement(scan_state, stmt);
    case T_AT:
	stmt->u.insn.at = 1;
	return parse_insn_address_after_at(scan_state, stmt);
    default:
	return error(scan_state, "invalid <address>", token, &token_attr);
    }
}

static int parse_insn_after_symbol_uinteger(
    struct scan_state *scan_state, struct stmt *stmt, const struct pdp10_instruction *insndesc, union token_attribute *uinteger_attr)
{
    enum token token;
    union token_attribute token_attr;

    token = scan_token(scan_state, &token_attr);
    if (token == T_COMMA) {	/* the <uinteger> is the <accumulator> */
	if (uinteger_attr->uint > 0xF)
	    return error(scan_state, "invalid <accumulator>", T_UINTEGER, uinteger_attr);
	if (insndesc->type & (PDP10_A_OPCODE | PDP10_A_UNUSED))
	    return error(scan_state, "<accumulator> not allowed in this instruction", T_UINTEGER, uinteger_attr);
	stmt->u.insn.accumulator = uinteger_attr->uint;
	return parse_insn_address(scan_state, stmt, insndesc);
    }

    if (insndesc->type & PDP10_E_UNUSED)
	return error(scan_state, "<address> not allowed in this instruction", token, &token_attr);

    switch (token) {
    case T_LPAREN:	/* the <uinteger> is the <displacement>, followed by <index> */
	if (uinteger_attr->uint > PDP10_UINT18_MAX)
	    return error(scan_state, "invalid <displacement>", T_UINTEGER, uinteger_attr);
	stmt->u.insn.address = uinteger_attr->uint;
	return parse_insn_index_after_lparen(scan_state, stmt);
    case T_NEWLINE:	/* the <uinteger> is the <displacement>, there is no <accumulator> or <index> */
	if (uinteger_attr->uint > PDP10_UINT18_MAX)
	    return error(scan_state, "invalid <displacement>", T_UINTEGER, uinteger_attr);
	stmt->u.insn.address = uinteger_attr->uint;
	return 1;
    default:
	return error(scan_state, "junk after <symbol> <uinteger>", token, &token_attr);
    }
}

static int parse_after_symbol(struct scan_state *scan_state, struct stmt *stmt, union token_attribute *symbol_attr)
{
    enum token token;
    union token_attribute token_attr;
    const struct pdp10_instruction *insndesc;

    token = scan_token(scan_state, &token_attr);
    if (token == T_COLON) {
	stmt->u.symbol.name = symbol_attr->text;
	stmt->tag = S_LABEL;
	return 1;
    }

    insndesc = pdp10_instruction_from_name(symbol_attr->text);
    if (!insndesc)
	return error(scan_state, "invalid instruction name", T_SYMBOL, symbol_attr);

    stmt->tag = S_INSN;
    stmt->u.insn.at = 0;
    stmt->u.insn.address = 0;
    stmt->u.insn.indexreg = 0;

    if (insndesc->type & PDP10_A_OPCODE) {
	/* XXX: this is too intimate with quirky ->opcode representation */
	stmt->u.insn.opcode = (insndesc->opcode >> 6) & 0x1FF;
	stmt->u.insn.accumulator = (insndesc->opcode >> 2) & 0xF;
    } else {
	stmt->u.insn.opcode = insndesc->opcode & 0x1FF;
	stmt->u.insn.accumulator = 0;
    }

    switch (token) {
    case T_NEWLINE:
	return 1;
    case T_UINTEGER:	/* might be <accumulator> or <displacement> */
	return parse_insn_after_symbol_uinteger(scan_state, stmt, insndesc, &token_attr);
    default:
	break;
    }

    if (insndesc->type & PDP10_E_UNUSED)
	return error(scan_state, "<address> not allowed in this instruction", token, &token_attr);

    switch (token) {
    case T_AT:
	stmt->u.insn.at = 1;
	return parse_insn_address_after_at(scan_state, stmt);
    case T_LPAREN:	/* might be <displacement> or <index> */
	return parse_insn_address_after_lparen(scan_state, stmt);
    default:
	return error(scan_state, "junk after instruction name", token, &token_attr);
    }
}

int parse_stmt(struct scan_state *scan_state, struct stmt *stmt)
{
    enum token token;
    union token_attribute token_attr;

    for (;;) {
	token = scan_token(scan_state, &token_attr);
	switch (token) {
	    /*
	     * directives
	     */
	case T_DOT_GLOBL:
	    return parse_dot_globl(scan_state, stmt);
	case T_DOT_TEXT:
	    return parse_dot_text(scan_state, stmt);
	    /*
	     * other symbols
	     */
	case T_SYMBOL:	/* start of label, insn, or symbol assignment */
	    return parse_after_symbol(scan_state, stmt, &token_attr);
	    /*
	     * synthetic symbols
	     */
	case T_ERROR:
	    return -1;	/* diagnostics already emitted by scan.c */
	case T_EOF:
	    return 0;
	case T_NEWLINE:
	    continue;
	default:
	    return error(scan_state, "expected directive, label, or instruction", token, &token_attr);
	}
    }
}
