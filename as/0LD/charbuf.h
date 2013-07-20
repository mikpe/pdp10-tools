/*
 * charbuf.h
 */
#ifndef CHARBUF_H
#define CHARBUF_H

struct charbuf_chunk {
    char buf[128 - sizeof(struct charbuf_chunk*)];
    struct charbuf_chunk *next;
};

struct charbuf {
    struct charbuf_chunk head;
    struct charbuf_chunk *tail;	/* INV: tail->next == NULL */
    unsigned int pos;		/* in tail chunk */
};

void charbuf_init(struct charbuf *charbuf);
void charbuf_fini(struct charbuf *charbuf);
void charbuf_append(struct charbuf *charbuf, int ch);
int charbuf_strcmp(const struct charbuf *charbuf, const char *string);
char *charbuf_string(const struct charbuf *charbuf);

#endif /* CHARBUF_H */
