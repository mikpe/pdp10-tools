/*
 * charbuf.c
 */
#include <stdlib.h>
#include <string.h>
#include "charbuf.h"
#include "emalloc.h"

void charbuf_init(struct charbuf *charbuf)
{
    charbuf->head.next = NULL;
    charbuf->tail = &charbuf->head;
    charbuf->pos = 0;
}

void charbuf_fini(struct charbuf *charbuf)
{
    struct charbuf_chunk *chunk, *next;

    chunk = charbuf->head.next;
    while (chunk != NULL) {
	next = chunk->next;
	free(chunk);
	chunk = next;
    }
}

void charbuf_append(struct charbuf *charbuf, int ch)
{
    struct charbuf_chunk *tail, *next;
    unsigned int pos;

    tail = charbuf->tail;
    pos = charbuf->pos;
    if (pos >= sizeof tail->buf) {
	next = emalloc(sizeof *next);
	next->next = NULL;
	tail->next = next;
	tail = next;
	charbuf->tail = tail;
	pos = 0;
    }
    tail->buf[pos] = ch;
    charbuf->pos = pos + 1;
}

int charbuf_strcmp(const struct charbuf *charbuf, const char *string)
{
    const struct charbuf_chunk *chunk;
    int cmp;

    chunk = &charbuf->head;
    while (chunk->next != NULL) {
	cmp = strncmp(chunk->buf, string, sizeof chunk->buf);
	if (cmp)
	    return cmp;
	string += sizeof chunk->buf;
	chunk = chunk->next;
    }
    return strncmp(chunk->buf, string, charbuf->pos);
}

char *charbuf_string(const struct charbuf *charbuf)
{
    const struct charbuf_chunk *chunk;
    size_t nrbytes;
    char *string, *strp;

    chunk = &charbuf->head;
    nrbytes = 0;
    while (chunk->next != NULL) {
	nrbytes += sizeof chunk->buf;
	chunk = chunk->next;
    }
    nrbytes = nrbytes + charbuf->pos + 1;

    string = emalloc(nrbytes);

    chunk = &charbuf->head;
    strp = string;
    while (chunk->next != NULL) {
	memcpy(strp, chunk->buf, sizeof chunk->buf);
	strp += sizeof chunk->buf;
	chunk = chunk->next;
    }
    memcpy(strp, chunk->buf, charbuf->pos);
    strp[charbuf->pos] = '\0';

    return string;
}
