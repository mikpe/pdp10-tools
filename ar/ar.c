/*
 * ar.c
 *
 * ar clone for PDP10 files with 9-bit bytes
 */
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "pdp10-ar.h"
#include "pdp10-inttypes.h"
#include "pdp10-stdio.h"

#define VERSION "pdp10-tools ar version 0.1, built " __DATE__ " " __TIME__ "\n"

enum modifier {
    MOD_create = 1 << 0,	/* 'c' */
    MOD_newer = 1 << 1,		/* 'u' */
    MOD_verbose = 1 << 2,	/* 'v' */
};

struct strtab {
    char *bytes;
    unsigned long len;
};

struct arhdr {
    char ar_name[16];		/* NUL-terminated unless ar_name[0] == '/' */
    time_t ar_date;		/* File date, decimal seconds since Epoch.  */
    unsigned int ar_uid;	/* User ID.  */
    unsigned int ar_gid;	/* Group ID.  */
    unsigned int ar_mode;	/* File mode.  */
    unsigned long ar_size;	/* File size.  */
};

struct member {
    struct member **prev_next, *next;
    char *name;			/* points into ->arhdr.ar_name, params->strtab, or argv[] */
    unsigned long strtaboffset;	/* >=0: offset into strtab, -1U: short name in arhdr.ar_name, -2UL: short or long name in argv[] */
    unsigned long srcoffset;	/* 0: in external file, >0: in old archive */
    struct arhdr arhdr;
};

struct params {
    const char *progname;	/* argv[0] */
    char operation;		/* 'd', 'q', 'r', 't', 'x' */
    unsigned int modifiers;	/* bitmask indexed by enum modifier */
    char *arname;
    PDP10_FILE *pdp10fp;
    struct strtab strtab;
    struct member *members_head, **members_tail;
};

static void params_init(struct params *params)
{
    memset(params, '\0', sizeof *params);
    params->progname = NULL;
    params->arname = NULL;
    params->pdp10fp = NULL;
    params->strtab.bytes = NULL;
    params->members_head = NULL;
    params->members_tail = &params->members_head;
}

static struct member *lookup_member(struct params *params, const char *name)
{
    struct member *member;

    for (member = params->members_head; member; member = member->next)
	if (strcmp(member->name, name) == 0)
	    return member;

    return NULL;
}

static void unlink_member(struct member *member)
{
    *member->prev_next = member->next;
    if (member->next)
	member->next->prev_next = member->prev_next;
}

static void append_member(struct params *params, struct member *member)
{
    member->next = NULL;
    member->prev_next = params->members_tail;
    *params->members_tail = member;
    params->members_tail = &member->next;
}

static int ar_fgetc(struct params *params, const char *srcname, PDP10_FILE *srcfp)
{
    int ch;

    ch = pdp10_fgetc(srcfp);
    if (ch == EOF) {
	fprintf(stderr, "%s: %s: premature EOF on read\n", params->progname, srcname);
	return EOF;
    }
    return ch;
}

static int ar_fputc(struct params *params, const char *dstname, PDP10_FILE *dstfp, uint16_t ch)
{
    if (pdp10_fputc(ch, dstfp) < 0) {
	fprintf(stderr, "%s: %s: failed to write: %s\n",
		params->progname, dstname, strerror(errno));
	return -1;
    }
    return 0;
}

static int ar_fputvec(struct params *params, const char *dstname, PDP10_FILE *dstfp, const pdp10_uint9_t *v, size_t n)
{
    size_t i;

    for (i = 0; i < n; ++i)
	if (ar_fputc(params, dstname, dstfp, v[i]) < 0)
	    return -1;

    return 0;
}

static int copy_file_data(
    struct params *params, unsigned long nrbytes, PDP10_FILE *srcfp, const char *srcname, PDP10_FILE *dstfp, const char *dstname)
{
    unsigned int i;

    for (i = 0; i < nrbytes; ++i) {
	int ch = ar_fgetc(params, srcname, srcfp);
	if (ch == EOF)
	    return -1;
	if (ar_fputc(params, dstname, dstfp, ch) < 0)
	    return -1;
    }
    return 0;
}

static const pdp10_uint9_t armag[PDP10_SARMAG] = PDP10_ARMAG;

static int read_armag(struct params *params)
{
    unsigned int i;

    for (i = 0; i < PDP10_SARMAG; ++i) {
	int ch = ar_fgetc(params, params->arname, params->pdp10fp);
	if (ch == EOF)
	    return -1;
	if (ch != armag[i]) {
	    fprintf(stderr, "%s: %s: file format not recognized\n", params->progname, params->arname);
	    return -1;
	}
    }
    return 0;
}

static int write_armag(struct params *params, const char *tmparname, PDP10_FILE *tmparfp)
{
    return ar_fputvec(params, tmparname, tmparfp, armag, PDP10_SARMAG);
}

static const pdp10_uint9_t arfmag[2] = PDP10_ARFMAG;

static int read_arhdr(struct params *params, struct arhdr *arhdr)
{
    union {
	struct pdp10_ar_hdr arhdr;
	pdp10_uint9_t buf[PDP10_ARHDR_SIZEOF];
    } u;
    unsigned int i;
    char buf[16];

    for (i = 0; i < PDP10_ARHDR_SIZEOF; ++i) {
	int ch = pdp10_fgetc(params->pdp10fp);
	if (ch == EOF) {
	    if (i == 0)
		return 0;
	    fprintf(stderr, "%s: %s: premature EOF in ar_hdr\n", params->progname, params->arname);
	    return -1;
	}
	u.buf[i] = ch;
    }

    if (u.arhdr.ar_fmag[0] != arfmag[0] || u.arhdr.ar_fmag[1] != arfmag[1]) {
	fprintf(stderr, "%s: %s: wrong value in ar_fmag\n", params->progname, params->arname);
	return -1;
    }

    for (i = 0; i < 10; ++i)
	buf[i] = (char)(unsigned char)u.arhdr.ar_size[i];
    buf[i] = '\0';
    arhdr->ar_size = strtoul(buf, NULL, 10);

    for (i = 0; i < 8; ++i)
	buf[i] = (char)(unsigned char)u.arhdr.ar_mode[i];
    buf[i] = '\0';
    arhdr->ar_mode = strtoul(buf, NULL, 8);

    for (i = 0; i < 6; ++i)
	buf[i] = (char)(unsigned char)u.arhdr.ar_gid[i];
    buf[i] = '\0';
    arhdr->ar_gid = strtoul(buf, NULL, 10);

    for (i = 0; i < 6; ++i)
	buf[i] = (char)(unsigned char)u.arhdr.ar_uid[i];
    buf[i] = '\0';
    arhdr->ar_uid = strtoul(buf, NULL, 10);

    for (i = 0; i < 12; ++i)
	buf[i] = (char)(unsigned char)u.arhdr.ar_date[i];
    buf[i] = '\0';
    arhdr->ar_date = strtoul(buf, NULL, 10);

    for (i = 0; i < 16; ++i)
	arhdr->ar_name[i] = (char)(unsigned char)u.arhdr.ar_name[i];

    if (arhdr->ar_name[0] != '/') {
	for (i = 1; i < 15; ++i)
	    if (arhdr->ar_name[i] == '/')
		break;
	arhdr->ar_name[i] = '\0';
    }

    return 1;
}

static int write_arhdr(struct params *params, const char *tmparname, PDP10_FILE *tmparfp, const struct arhdr *arhdr)
{
    union {
	struct pdp10_ar_hdr arhdr;
	pdp10_uint9_t buf[PDP10_ARHDR_SIZEOF];
    } u;
    unsigned int i;
    char buf[16];

    u.arhdr.ar_fmag[0] = arfmag[0];
    u.arhdr.ar_fmag[1] = arfmag[1];

    sprintf(buf, "%lu", arhdr->ar_size);
    for (i = 0; i < 10 && buf[i] != '\0'; ++i)
	u.arhdr.ar_size[i] = (unsigned char)buf[i];
    for (; i < 10; ++i)
	u.arhdr.ar_size[i] = ' ';

    sprintf(buf, "%o", arhdr->ar_mode);
    for (i = 0; i < 8 && buf[i] != '\0'; ++i)
	u.arhdr.ar_mode[i] = (unsigned char)buf[i];
    for (; i < 8; ++i)
	u.arhdr.ar_mode[i] = ' ';

    sprintf(buf, "%u", arhdr->ar_gid);
    for (i = 0; i < 6 && buf[i] != '\0'; ++i)
	u.arhdr.ar_gid[i] = (unsigned char)buf[i];
    for (; i < 6; ++i)
	u.arhdr.ar_gid[i] = ' ';

    sprintf(buf, "%u", arhdr->ar_uid);
    for (i = 0; i < 6 && buf[i] != '\0'; ++i)
	u.arhdr.ar_uid[i] = (unsigned char)buf[i];
    for (; i < 6; ++i)
	u.arhdr.ar_uid[i] = ' ';

    sprintf(buf, "%llu", (unsigned long long)arhdr->ar_date);
    for (i = 0; i < 12 && buf[i] != '\0'; ++i)
	u.arhdr.ar_date[i] = (unsigned char)buf[i];
    for (; i < 12; ++i)
	u.arhdr.ar_date[i] = ' ';

    if (arhdr->ar_name[0] != '/') {
	for (i = 0; i < 15 && arhdr->ar_name[i] != '\0'; ++i)
	    u.arhdr.ar_name[i] = (unsigned char)arhdr->ar_name[i];
	u.arhdr.ar_name[i] = '/';
	++i;
    } else {
	for (i = 0; i < 16 && arhdr->ar_name[i] != '\0'; ++i)
	    u.arhdr.ar_name[i] = (unsigned char)arhdr->ar_name[i];
    }
    for (; i < 16; ++i)
	u.arhdr.ar_name[i] = ' ';

    if (ar_fputvec(params, tmparname, tmparfp, u.buf, PDP10_ARHDR_SIZEOF) < 0)
	return -1;

    return 0;
}

static int read_arstrtab(struct params *params, const struct arhdr *arhdr)
{
    unsigned int i;

    params->strtab.len = arhdr->ar_size;
    params->strtab.bytes = malloc(params->strtab.len);
    if (!params->strtab.bytes) {
	fprintf(stderr, "%s: %s: failed to allocate %zu bytes for string table: %s\n",
		params->progname, params->arname, params->strtab.len, strerror(errno));
	return -1;
    }

    for (i = 0; i < params->strtab.len; ++i) {
	int ch = ar_fgetc(params, params->arname, params->pdp10fp);
	if (ch == EOF)
	    return -1;
	if (ch == '/')
	    ch = '\0';
	params->strtab.bytes[i] = ch;
    }

    if ((i & 1) && ar_fgetc(params, params->arname, params->pdp10fp) == EOF)
	return -1;

    return 0;
}

static int write_arstrtab(struct params *params, const char *tmparname, PDP10_FILE *tmparfp)
{
    struct arhdr arhdr;
    unsigned int i;

    if (!params->strtab.len)
	return 0;

    memset(&arhdr, 0, sizeof arhdr);
    arhdr.ar_name[0] = '/';
    arhdr.ar_name[1] = '/';
    arhdr.ar_size = params->strtab.len;
    if (write_arhdr(params, tmparname, tmparfp, &arhdr) < 0)
	return -1;

    for (i = 0; i < params->strtab.len; ++i) {
	int ch = params->strtab.bytes[i];
	if (ch == '\0')
	    ch = '/';
	if (ar_fputc(params, tmparname, tmparfp, ch) < 0)
	    return -1;
    }

    if ((i & 1) && ar_fputc(params, tmparname, tmparfp, '\n') < 0)
	return -1;

    return 0;
}

static int skip_member(struct params *params, struct arhdr *arhdr)
{
    if (pdp10_fseeko(params->pdp10fp, (arhdr->ar_size + 1) & ~(unsigned long)1, PDP10_SEEK_CUR) < 0) {
	fprintf(stderr, "%s: %s: failed to fseek to next member: %s\n",
		params->progname, params->arname, strerror(errno));
	return -1;
    }
    return 0;
}

static int read_arsymtab(struct params *params, struct arhdr *arhdr)
{
    /* XXX: symtab is NYI so just seek past this member */
    fprintf(stderr, "%s: %s: Warning: skipping symbol table\n", params->progname, params->arname);
    return skip_member(params, arhdr);
}

static int read_archive(struct params *params)
{
    struct arhdr arhdr;
    int status;
    struct member *member;

    if (read_armag(params) < 0)
	return -1;

    status = read_arhdr(params, &arhdr);
    if (status <= 0)
	return status;

    if (arhdr.ar_name[0] == '/' && arhdr.ar_name[1] == ' ') {
	status = read_arsymtab(params, &arhdr);
	if (status < 0)
	    return -1;
	status = read_arhdr(params, &arhdr);
	if (status <= 0)
	    return status;
    }

    if (arhdr.ar_name[0] == '/' && arhdr.ar_name[1] == '/' && arhdr.ar_name[2] == ' ') {
	status = read_arstrtab(params, &arhdr);
	if (status < 0)
	    return -1;
	status = read_arhdr(params, &arhdr);
	if (status <= 0)
	    return status;
    }

    do {
	member = malloc(sizeof *member);
	if (!member) {
	    fprintf(stderr, "%s: %s: failed to allocate %zu bytes for new member\n",
		    params->progname, params->arname, sizeof *member);
	    return -1;
	}
	member->arhdr = arhdr;
	member->srcoffset = pdp10_ftello(params->pdp10fp);
	if (arhdr.ar_name[0] == '/') {
	    char buf[16];
	    unsigned int i;

	    if (arhdr.ar_name[1] < '0' || arhdr.ar_name[1] > '9') {
		fprintf(stderr, "%s: %s: invalid member name '%.*s'\n",
			params->progname, params->arname, 16, arhdr.ar_name);
		return -1;
	    }
	    for (i = 0; i < 15; ++i)
		buf[i] = arhdr.ar_name[i + 1];
	    buf[i] = '\0';
	    member->strtaboffset = strtoul(buf, NULL, 10);
	    if (!params->strtab.bytes || member->strtaboffset >= params->strtab.len) {
		fprintf(stderr, "%s: %s: ar_name '%.*s' out of bounds\n",
			params->progname, params->arname, 16, arhdr.ar_name);
		return -1;
	    }
	} else {
	    member->strtaboffset = -1UL;
	    member->name = member->arhdr.ar_name;
	}
	append_member(params, member);
	if (skip_member(params, &arhdr) < 0)
	    return -1;
	status = read_arhdr(params, &arhdr);
    } while (status > 0);

    return status;
}

/*
 * ar t/x code
 */

static int ar_tx_should_process_member(struct params *params, struct member *member, char **files, int nrfiles)
{
    int i;

    for (i = 0; i < nrfiles; ++i)
	if (files[i] && strcmp(member->name, files[i]) == 0) {
	    files[i] = NULL;
	    return 1;
	}

    return nrfiles <= 0;
}

static char *rwx_string(unsigned int m, char *buf)
{
    buf[0] = (m & 4) ? 'r' : '-';
    buf[1] = (m & 2) ? 'w' : '-';
    buf[2] = (m & 1) ? 'x' : '-';
    buf[3] = '\0';
    return buf;
}

static char *date_string(time_t t, char *buf)
{
    struct tm *tm;

    tm = gmtime(&t);
    if (!tm) {
	fprintf(stderr, "gmtime(%lu) failed: %s\n", t, strerror(errno));
	exit(1);
    }
    /* Mon Day HH:MM YYYY */
    strftime(buf, 64, "%b %d %H:%M %Y", tm);
    return buf;
}

static int ar_t_member(struct params *params, struct member *member)
{
    char mode_u_buf[4], mode_g_buf[4], mode_o_buf[4], date_buf[64];

    if (params->modifiers & MOD_verbose)
	printf("%s%s%s %d/%d %10lu %s ",
	       rwx_string(member->arhdr.ar_mode >> 6, mode_u_buf),
	       rwx_string(member->arhdr.ar_mode >> 3, mode_g_buf),
	       rwx_string(member->arhdr.ar_mode, mode_o_buf),
	       member->arhdr.ar_uid,
	       member->arhdr.ar_gid,
	       member->arhdr.ar_size,
	       date_string(member->arhdr.ar_date, date_buf));
    printf("%s\n", member->name);
    return 0;
}

static int ar_x_copy(struct params *params, struct member *member, PDP10_FILE *memberfp)
{
    if (pdp10_fseeko(params->pdp10fp, member->srcoffset, PDP10_SEEK_SET) < 0) {
	fprintf(stderr, "%s: %s: failed to fseek to member %s: %s\n",
		params->progname, params->arname, member->name, strerror(errno));
	return -1;
    }
    return copy_file_data(params, member->arhdr.ar_size, params->pdp10fp, params->arname, memberfp, member->name);
}

static int ar_x_member(struct params *params, struct member *member)
{
    PDP10_FILE *memberfp;
    int status;

    if (params->modifiers & MOD_verbose)
	printf("x - %s\n", member->name);

    memberfp = pdp10_fopen(member->name, "wb");
    if (!memberfp) {
	fprintf(stderr, "%s: %s: %s: failed to open: %s\n",
		params->progname, params->arname, member->name, strerror(errno));
	return -1;
    }
    status = ar_x_copy(params, member, memberfp);
    pdp10_fclose(memberfp);
    if (status < 0)
	return -1;

    if (chmod(member->name, member->arhdr.ar_mode & 0777) < 0) {
	fprintf(stderr, "%s: %s: %s: failed to set mode %o: %s\n",
		params->progname, params->arname, member->name, member->arhdr.ar_mode & 0777, strerror(errno));
	return -1;
    }

    return 0;
}

static int ar_tx(struct params *params, char **files, int nrfiles)
{
    struct member *member;
    int status;
    int i;

    if (nrfiles < 1) {
	fprintf(stderr, "%s: archive name missing\n", params->progname);
	return -1;
    }
    params->arname = files[0];
    ++files;
    --nrfiles;

    params->pdp10fp = pdp10_fopen(params->arname, "rb");
    if (!params->pdp10fp) {
	fprintf(stderr, "%s: %s: failed to open: %s\n",
		params->progname, params->arname, strerror(errno));
	return -1;
    }

    if (read_archive(params) < 0)
	return -1;

    for (member = params->members_head; member; member = member->next)
	if (ar_tx_should_process_member(params, member, files, nrfiles)) {
	    switch (params->operation) {
	    case 't':
		status = ar_t_member(params, member);
		break;
	    case 'x':
		status = ar_x_member(params, member);
		break;
	    default:
		fprintf(stderr, "%s: %s: unexpected operation '%c'\n", params->progname, __FUNCTION__, params->operation);
		return -1;
	    }
	    if (status < 0)
		return -1;
	}

    if (status < 0)
	return status;

    for (i = 0; i < nrfiles; ++i)
	if (files[i]) {
	    status = -1;
	    fprintf(stderr, "no entry %s in archive\n", files[i]);
	}

    return status;
}

/*
 * ar d/q/r code
 */

static int fixup_arstrtab(struct params *params)
{
    unsigned long new_strtab_len;
    struct member *member;
    size_t namlen;
    char *new_strtab_bytes;
    unsigned long curpos;

    new_strtab_len = 0;
    for (member = params->members_head; member; member = member->next) {
	if (member->strtaboffset == -1UL)
	    continue;
	namlen = strlen(member->name);
	if (namlen < 16) {
	    strcpy(member->arhdr.ar_name, member->name);
	    member->name = member->arhdr.ar_name;
	    member->strtaboffset = -1UL;
	    continue;
	}
	new_strtab_len += namlen + 2;	/* for "\0\n" which is output as "/\n" */
    }

    if (new_strtab_len == 0) {
	free(params->strtab.bytes);
	params->strtab.bytes = NULL;
	params->strtab.len = 0;
	return 0;
    }

    new_strtab_bytes = malloc(new_strtab_len);
    if (!new_strtab_bytes) {
	fprintf(stderr, "%s: %s: failed to allocate %lu bytes for updated string table: %s\n",
		params->progname, params->arname, new_strtab_len, strerror(errno));
	return -1;
    }

    curpos = 0;
    for (member = params->members_head; member; member = member->next) {
	if (member->strtaboffset == -1UL)
	    continue;
	namlen = strlen(member->name);
	member->strtaboffset = curpos;
	strcpy(new_strtab_bytes + curpos, member->name);
	member->name = new_strtab_bytes + curpos;
	*(new_strtab_bytes + curpos + namlen + 1) = '\n';
	curpos += namlen + 2;
    }

    free(params->strtab.bytes);
    params->strtab.bytes = new_strtab_bytes;
    params->strtab.len = new_strtab_len;

    return 0;
}

static void update_arhdr(struct arhdr *arhdr, struct stat *stbuf)
{
    arhdr->ar_date = stbuf->st_mtime;
    arhdr->ar_uid = stbuf->st_uid;
    arhdr->ar_gid = stbuf->st_gid;
    arhdr->ar_mode = stbuf->st_mode;

    /* stbuf->st_size is the file size in octets, convert it to the size in nonets;
       see lib/pdp10-stdio.c:pdp10_fseeko() for the derivation of this formula */
    arhdr->ar_size = (stbuf->st_size / 9) * 8 + ((stbuf->st_size % 9) * 8) / 9;
}

static int ar_d_process_files(struct params *params, char **files, int nrfiles)
{
    struct member *member;
    int i;
    char code;

    code = 0;

    for (i = 0; i < nrfiles; ++i) {
	member = lookup_member(params, files[i]);
	if (!member) {
	    fprintf(stderr, "%s: %s: member %s not found\n",
		    params->progname, params->arname, files[i]);
	    return -1;
	}
	unlink_member(member);
	free(member);
	code = 'd';
	if (params->modifiers & MOD_verbose)
	    printf("%c - %s\n", code, files[i]);
    }

    return code;	/* >0 if changes, 0 if no changes, -1 if error above */
}

static int ar_qr_process_files(struct params *params, char **files, int nrfiles)
{
    struct stat stbuf;
    struct member *member;
    int i;
    char code;

    code = 0;

    for (i = 0; i < nrfiles; ++i) {
	if (stat(files[i], &stbuf) < 0) {
	    fprintf(stderr, "%s: %s: failed to stat: %s\n",
		    params->progname, files[i], strerror(errno));
	    return -1;
	}
	member = lookup_member(params, files[i]);
	if (member && params->operation != 'q') {
	    if ((params->modifiers & MOD_newer) && stbuf.st_mtime <= member->arhdr.ar_date)
		continue;
	    code = 'r';
	} else {
	    member = malloc(sizeof *member);
	    if (!member) {
		fprintf(stderr, "%s: %s: failed to allocate %zu bytes for new member\n",
			params->progname, params->arname, sizeof *member);
		return -1;
	    }
	    member->name = files[i];
	    member->strtaboffset = -2UL;
	    append_member(params, member);
	    code = 'a';
	}
	if (params->modifiers & MOD_verbose)
	    printf("%c - %s\n", code, files[i]);
	member->srcoffset = 0;
	update_arhdr(&member->arhdr, &stbuf);
    }

    return code;	/* >0 if changes, 0 if no changes, -1 if error above */
}

static char *make_tmparname(struct params *params)
{
    char *last_slash;
    unsigned int preflen;
    char *tmparname;

    if (!params->pdp10fp)
	return params->arname;

    last_slash = strrchr(params->arname, '/');
    if (last_slash)
	preflen = last_slash + 1 - params->arname;
    else
	preflen = 0;

    tmparname = malloc(preflen + 5 + 6 + 1);	/* <prefix>artmpXXXXXX\0 */
    if (!tmparname) {
	fprintf(stderr, "%s: %s: failed to allocate %u bytes for temporary archive name: %s\n",
		params->progname, params->arname, preflen + 5 + 6 + 1, strerror(errno));
	return NULL;
    }

    sprintf(tmparname, "%.*sartmpXXXXXX", preflen, params->arname);
    return tmparname;
}

static PDP10_FILE *make_tmparfp(struct params *params, char *tmparname)
{
    int tmparfd;
    PDP10_FILE *tmparfp;

    if (!params->pdp10fp) {
	tmparfp = pdp10_fopen(params->arname, "wb");
	if (!tmparfp)
	    fprintf(stderr, "%s: %s: failed to create: %s\n", params->progname, params->arname, strerror(errno));
	if (!(params->modifiers & MOD_create))
	    printf("%s: creating %s\n", params->progname, params->arname);
	return tmparfp;
    }

    tmparfd = mkstemp(tmparname);
    if (tmparfd < 0) {
	fprintf(stderr, "%s: %s: failed to create temporary file: %s\n",
		params->progname, tmparname, strerror(errno));
	return NULL;
    }

    tmparfp = pdp10_fdopen(tmparfd, "wb");
    if (!tmparfp)
	fprintf(stderr, "%s: fdopen failed: %s\n", params->progname, strerror(errno));

    return tmparfp;
}

static int write_member(struct params *params, const char *tmparname, PDP10_FILE *tmparfp, struct member *member)
{
    PDP10_FILE *srcfp;
    const char *srcname;
    int status;

    if (member->srcoffset == 0) {
	srcname = member->name;
	srcfp = pdp10_fopen(srcname, "rb");
	if (!srcfp) {
	    fprintf(stderr, "%s: %s: failed to open: %s\n",
		    params->progname, srcname, strerror(errno));
	    return -1;
	}
    } else {
	srcname = params->arname;
	srcfp = params->pdp10fp;
	if (pdp10_fseeko(srcfp, member->srcoffset, PDP10_SEEK_SET) < 0) {
	    fprintf(stderr, "%s: %s: failed to fseek to member %s: %s\n",
		    params->progname, srcname, member->name, strerror(errno));
	    return -1;
	}
    }

    if (member->strtaboffset != -1UL)
	sprintf(member->arhdr.ar_name, "/%lu", member->strtaboffset);

    status = -1;
    do {
	if (write_arhdr(params, tmparname, tmparfp, &member->arhdr) < 0)
	    break;

	if (copy_file_data(params, member->arhdr.ar_size, srcfp, srcname, tmparfp, tmparname) < 0)
	    break;

	if ((member->arhdr.ar_size & 1) && ar_fputc(params, tmparname, tmparfp, '\n') < 0)
	    break;

	status = 0;
    } while (0);

    if (member->srcoffset == 0)
	pdp10_fclose(srcfp);

    return status;
}

static int write_archive(struct params *params, const char *tmparname, PDP10_FILE *tmparfp)
{
    struct member *member;
    struct stat stbuf;

    if (write_armag(params, tmparname, tmparfp) < 0)
	return -1;

    if (write_arstrtab(params, tmparname, tmparfp) < 0)
	return -1;

    for (member = params->members_head; member; member = member->next)
	if (write_member(params, tmparname, tmparfp, member) < 0)
	    return -1;

    if (!params->pdp10fp)
	return 0;

    if (stat(params->arname, &stbuf) < 0) {
	fprintf(stderr, "%s: %s: failed to stat: %s\n",
		params->progname, params->arname, strerror(errno));
	return -1;
    }

    if (chmod(tmparname, stbuf.st_mode) < 0) {
	fprintf(stderr, "%s: %s: failed to chmod 0%o: %s\n",
		params->progname, tmparname, stbuf.st_mode, strerror(errno));
	return -1;
    }

    if (chown(tmparname, stbuf.st_uid, stbuf.st_gid) < 0) {
	fprintf(stderr, "%s: %s: failed to chown %u/%u: %s\n",
		params->progname, tmparname, stbuf.st_uid, stbuf.st_gid, strerror(errno));
	return -1;
    }

    if (unlink(params->arname) < 0) {
	fprintf(stderr, "%s: %s: failed to unlink: %s\n",
		params->progname, params->arname, strerror(errno));
	return -1;
    }

    if (link(tmparname, params->arname) < 0) {
	fprintf(stderr, "%s: failed to link %s to %s: %s\n",
		params->progname, tmparname, params->arname, strerror(errno));
	return -1;
    }

    return 0;
}

static int ar_dqr(struct params *params, char **files, int nrfiles)
{
    char *tmparname;
    PDP10_FILE *tmparfp;
    int status;

    if (nrfiles < 1) {
	fprintf(stderr, "%s: archive name missing\n", params->progname);
	return -1;
    }
    params->arname = files[0];
    ++files;
    --nrfiles;

    params->pdp10fp = pdp10_fopen(params->arname, "rb");
    if (params->pdp10fp != NULL) {
	if (read_archive(params) < 0)
	    return -1;
    } else if (params->operation == 'd') {
	fprintf(stderr, "%s: %s: failed to open: %s\n",
		params->progname, params->arname, strerror(errno));
	return -1;
    }

    if (params->operation == 'd')
	status = ar_d_process_files(params, files, nrfiles);
    else
	status = ar_qr_process_files(params, files, nrfiles);

    if (status <= 0)
	return status;

    if (fixup_arstrtab(params) < 0)
	return -1;

    tmparname = make_tmparname(params);
    if (!tmparname)
	return -1;

    tmparfp = make_tmparfp(params, tmparname);
    if (!tmparfp)
	return -1;

    status = write_archive(params, tmparname, tmparfp);

    pdp10_fclose(tmparfp);

    if (params->pdp10fp) {
	unlink(tmparname);
	free(tmparname);
    }

    return status;
}

/*
 * ar d/q/r/t/x dispatcher
 */

static int ar(struct params *params, char **files, int nrfiles)
{
    switch (params->operation) {
    case 'd':
    case 'q':
    case 'r':
	return ar_dqr(params, files, nrfiles);
    case 't':
    case 'x':
	return ar_tx(params, files, nrfiles);
    default:
	fprintf(stderr, "%s: NYI: operation '%c'\n", params->progname, params->operation);
	return -1;
    }
}

/*
 * Command-line interface.
 */

static void usage(const char *progname)

{
    fprintf(stderr,
	    "Usage: %s [-]{d,q,r,t,x}[cuvV] archive [member...]\n",
	    progname);
}

int main(int argc, char **argv)
{
    struct params params;
    char *opts;

    params_init(&params);
    params.progname = argv[0];

    if (argc < 2) {
	usage(params.progname);
	return 1;
    }

    opts = argv[1];
    if (*opts == '-')
	++opts;
    for (;; ++opts) {
	switch (*opts) {
	case 'd':
	case 'q':
	case 'r':
	case 't':
	case 'x':
	    params.operation = *opts;
	    continue;
	case 'c':
	    params.modifiers |= MOD_create;
	    continue;
	case 'u':
	    params.modifiers |= MOD_newer;
	    continue;
	case 'v':
	    params.modifiers |= MOD_verbose;
	    continue;
	case 'V':
	    printf(VERSION);
	    return 0;
	case '\0':
	    break;
	default:
	    fprintf(stderr, "%s: invalid option: %c\n", params.progname, *opts);
	    usage(params.progname);
	    return 1;
	}
	break;
    }

    return ar(&params, &argv[2], argc - 2) == 0 ? 0 : 1;
}
