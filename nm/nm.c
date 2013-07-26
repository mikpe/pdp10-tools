/*
 * nm.c
 *
 * nm clone for PDP10 Elf36 files.
 */
#define _GNU_SOURCE	/* for getopt_long() */
#include <errno.h>
#include <getopt.h>	/* for getopt_long() */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pdp10-elf36.h"
#include "pdp10-inttypes.h"
#include "pdp10-stdio.h"

#define VERSION "pdp10-tools nm version 0.1, built " __DATE__ " " __TIME__ "\n"

struct options {
    unsigned char print_file_name;
    unsigned char dynamic;
    unsigned char format;
    unsigned char extern_only;
    unsigned char numeric_sort;
    unsigned char no_sort;
    unsigned char print_size;
    unsigned char reverse_sort;
    unsigned char radix;
    unsigned char undefined_only;
    unsigned char defined_only;
};

struct params {
    const char *progname;
    struct options opts;

    int several_files;

    const char *filename;
    PDP10_FILE *pdp10fp;

    Elf36_Ehdr ehdr;
    Elf36_Shdr *shtab;
    pdp10_uint36_t shnum;
    pdp10_uint9_t *shstrtab;
    pdp10_uint36_t shstrtablen;

    Elf36_Sym *symtab;
    pdp10_uint36_t symtabndx;
    pdp10_uint36_t symnum;
    pdp10_uint9_t *strtab;
    pdp10_uint36_t strtablen;
};

static void params_init(struct params *params)
{
    memset(params, 0, sizeof *params);
}

static void params_file_init(struct params *params)
{
    params->pdp10fp = NULL;
    params->shtab = NULL;
    params->shnum = 0;
    params->shstrtab = NULL;
    params->shstrtablen = 0;
    params->symtab = NULL;
    params->symtabndx = 0;
    params->symnum = 0;
    params->strtab = NULL;
    params->strtablen = 0;
}

static void params_file_fini(struct params *params)
{
    free(params->strtab);
    free(params->symtab);
    free(params->shstrtab);
    free(params->shtab);
    pdp10_fclose(params->pdp10fp);
}

static void ehdr_unpack_ident(const Elf36_Ehdr *ehdr, unsigned char *e_ident)
{
    pdp10_uint36_t wident[4];
    unsigned int w, i, b;

    for (w = 0; w < 4; ++w)
	wident[w] = ehdr->e_wident[w];

    for (i = 0; i < EI_NIDENT; ++i) {
	for (w = 0; w < 4; ++w) {
	    b = (wident[w] >> (36 - 8)) & 0xff;
	    if (w == 0)
		e_ident[i] = b;
	    else
		wident[w - 1] |= b;
	    wident[w] = (wident[w] & ((1 << (36 - 8)) - 1)) << 8;
	}
    }
}

static int check_eident(struct params *params, const unsigned char *e_ident)
{
    if (e_ident[EI_MAG0] != ELFMAG0
	|| e_ident[EI_MAG1] != ELFMAG1
	|| e_ident[EI_MAG2] != ELFMAG2
	|| e_ident[EI_MAG3] != ELFMAG3) {
	fprintf(stderr, "%s: %s: not an ELF file: wrong magic\n", params->progname, params->filename);
	return -1;
    }

    if (e_ident[EI_CLASS] != ELFCLASS36) {
	fprintf(stderr, "%s: %s: not an ELF36 file: wrong class %u\n",
		params->progname, params->filename, e_ident[EI_CLASS]);
	return -1;
    }

    if (e_ident[EI_DATA] != ELFDATA2MSB) {
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong data %u\n",
		params->progname, params->filename, e_ident[EI_DATA]);
	return -1;
    }

    if (e_ident[EI_VERSION] != EV_CURRENT) {
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong version %u\n",
		params->progname, params->filename, e_ident[EI_VERSION]);
	return -1;
    }

    switch (e_ident[EI_OSABI]) {
    case ELFOSABI_NONE:
    case ELFOSABI_LINUX:
	break;
    default:
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong osabi %u\n",
		params->progname, params->filename, e_ident[EI_OSABI]);
	return -1;
    }

    if (e_ident[EI_ABIVERSION] != 0) {
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong abiversion %u\n",
		params->progname, params->filename, e_ident[EI_ABIVERSION]);
	return -1;
    }

    return 0;
}

static int check_ehdr(struct params *params)
{
    Elf36_Ehdr *ehdr = &params->ehdr;

    switch (ehdr->e_type) {
    case ET_REL:
    case ET_EXEC:
    case ET_DYN:
    case ET_CORE:
	break;
    default:
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong type %u\n",
		params->progname, params->filename, ehdr->e_type);
	return -1;
    }

    if (ehdr->e_machine != EM_PDP10) {
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong machine %u\n",
		params->progname, params->filename, ehdr->e_machine);
	return -1;
    }

    if (ehdr->e_version != EV_CURRENT) {
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong version %" PDP10_PRIu36 "\n",
		params->progname, params->filename, ehdr->e_version);
	return -1;
    }

    if (ehdr->e_ehsize != ELF36_EHDR_SIZEOF) {
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong ehsize %u\n",
		params->progname, params->filename, ehdr->e_ehsize);
	return -1;
    }

    if (ehdr->e_shoff != 0 && ehdr->e_shentsize != ELF36_SHDR_SIZEOF) {
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong shentsize %u\n",
		params->progname, params->filename, ehdr->e_shentsize);
	return -1;
    }

    return 0;
}

static int read_strtab(struct params *params, pdp10_uint36_t i, pdp10_uint9_t **strtab_ptr, pdp10_uint36_t *strtablen_ptr, const char *kind)
{
    pdp10_uint9_t *strtab;
    pdp10_uint36_t strtablen;

    if (i == 0 || i >= params->shnum) {
	fprintf(stderr, "%s: %s: invalid index %" PDP10_PRIu36 " for %s string table\n",
		params->progname, params->filename, i, kind);
	return -1;
    }
    if (params->shtab[i].sh_type != SHT_STRTAB) {
	fprintf(stderr, "%s: %s: %s string table at index %" PDP10_PRIu36 " has wrong type %" PDP10_PRIu36 "\n",
		params->progname, params->filename, kind, i, params->shtab[i].sh_type);
	return -1;
    }
    *strtablen_ptr = strtablen = params->shtab[i].sh_size;
    *strtab_ptr = strtab = malloc(strtablen * sizeof(pdp10_uint9_t));
    if (!strtab) {
	fprintf(stderr, "%s: %s: failed to allocate %zu bytes for %s string table: %s\n",
		params->progname, params->filename, strtablen * sizeof(pdp10_uint9_t), kind, strerror(errno));
	return -1;
    }
    if (pdp10_fseeko(params->pdp10fp, params->shtab[i].sh_offset, PDP10_SEEK_SET) < 0) {
	fprintf(stderr, "%s: %s: failed to seek to %s string table at %" PDP10_PRIu36 ": %s\n",
		params->progname, params->filename, kind, params->shtab[i].sh_offset, strerror(errno));
	return -1;
    }
    for (i = 0; i < strtablen; ++i)
	if (pdp10_elf36_read_uint9(params->pdp10fp, &strtab[i]) < 0) {
	    fprintf(stderr, "%s: %s: failed to read %s string table at index %" PDP10_PRIu36 ": %s\n",
		    params->progname, params->filename, kind, i, strerror(errno));
	    return -1;
	}
    return 0;
}

static int read_shtab(struct params *params)
{
    Elf36_Shdr shdr0;
    pdp10_uint36_t i;

    if (params->ehdr.e_shoff == 0)
	return 0;

    params->shnum = params->ehdr.e_shnum;

    if (pdp10_fseeko(params->pdp10fp, params->ehdr.e_shoff, PDP10_SEEK_SET) < 0) {
	fprintf(stderr, "%s: %s: failed to seek to section header table at %" PDP10_PRIu36 ": %s\n",
		params->progname, params->filename, params->ehdr.e_shoff, strerror(errno));
	return -1;
    }
    if (pdp10_elf36_read_shdr(params->pdp10fp, &shdr0) < 0) {
	fprintf(stderr, "%s: %s: failed to read section header index 0: %s\n",
		params->progname, params->filename, strerror(errno));
	return -1;
    }
    if (params->shnum == 0)
	params->shnum = shdr0.sh_size;
    params->shtab = malloc(params->shnum * sizeof(Elf36_Shdr));
    if (!params->shtab) {
	fprintf(stderr, "%s: %s: failed to allocate %zu bytes for section header table: %s\n",
		params->progname, params->filename, params->shnum * sizeof(Elf36_Shdr), strerror(errno));
	return -1;
    }
    params->shtab[0] = shdr0;
    for (i = 1; i < params->shnum; ++i)
	if (pdp10_elf36_read_shdr(params->pdp10fp, &params->shtab[i]) < 0) {
	    fprintf(stderr, "%s: %s: failed to read section header index %" PDP10_PRIu36 ": %s\n",
		    params->progname, params->filename, i, strerror(errno));
	    return -1;
	}

    i = params->ehdr.e_shstrndx;
    if (i == SHN_UNDEF)
	return 0;
    if (i == SHN_XINDEX)
	i = shdr0.sh_link;
    if (read_strtab(params, i, &params->shstrtab, &params->shstrtablen, "section header") < 0)
	return -1;
    return 0;
}

static int print_name(struct params *params, pdp10_uint9_t *strtab, pdp10_uint36_t strtablen, pdp10_uint36_t name, int say_empty)
{
    pdp10_uint36_t i;

    if (name >= strtablen) {
	fprintf(stderr, "%s: %s: name index %" PDP10_PRIu36 " is larger than the string table\n",
		params->progname, params->filename, name);
	return -1;
    }
    i = name;
    while (strtab[i] != '\0') {
	printf("%c", strtab[i]);
	++i;
	if (i >= strtablen) {
	    fprintf(stderr, "%s: %s: name string at index %" PDP10_PRIu36 " is not NUL-terminated\n",
		    params->progname, params->filename, name);
	    return -1;
	}
    }
    if (i == name && say_empty)
	printf("(empty)");
    return 0;
}

static int read_symtab(struct params *params)
{
    pdp10_uint36_t i;

    for (i = 1; i < params->shnum; ++i)
	if (params->shtab[i].sh_type == SHT_SYMTAB)
	    break;

    if (i >= params->shnum)
	return 0;

    params->symtabndx = i;

    if (read_strtab(params, params->shtab[i].sh_link, &params->strtab, &params->strtablen, "symtab") < 0)
	return -1;

    if (params->shtab[i].sh_entsize != ELF36_SYM_SIZEOF) {
	fprintf(stderr, "%s: %s: bogus sh_entsize %" PDP10_PRIu36 " in symbol table section header at index %" PDP10_PRIu36 "\n",
		params->progname, params->filename, params->shtab[i].sh_entsize, i);
	return -1;
    }

    if ((params->shtab[i].sh_size % ELF36_SYM_SIZEOF) != 0) {
	fprintf(stderr, "%s: %s: bogus sh_size %" PDP10_PRIu36 " in symbol table section header at index %" PDP10_PRIu36 "\n",
		params->progname, params->filename, params->shtab[i].sh_size, i);
	return -1;
    }

    params->symnum = params->shtab[i].sh_size / ELF36_SYM_SIZEOF;
    if (params->symnum == 0)
	return 0;

    params->symtab = malloc(params->symnum * sizeof(Elf36_Sym));
    if (!params->symtab) {
	fprintf(stderr, "%s: %s: failed to allocate %zu bytes for symbol table: %s\n",
		params->progname, params->filename, params->symnum * sizeof(Elf36_Sym), strerror(errno));
	return -1;
    }

    if (pdp10_fseeko(params->pdp10fp, params->shtab[i].sh_offset, PDP10_SEEK_SET) < 0) {
	fprintf(stderr, "%s: %s: failed to seek to symbol table at %" PDP10_PRIu36 ": %s\n",
		params->progname, params->filename, params->shtab[i].sh_offset, strerror(errno));
	return -1;
    }

    for (i = 0; i < params->symnum; ++i)
	if (pdp10_elf36_read_sym(params->pdp10fp, &params->symtab[i]) < 0) {
	    fprintf(stderr, "%s: %s: failed to read symbol table index %" PDP10_PRIu36 ": %s\n",
		    params->progname, params->filename, i, strerror(errno));
	    return -1;
	}

    return 0;
}

static int print_sym_name(struct params *params, Elf36_Sym *sym)
{
    return print_name(params, params->strtab, params->strtablen, sym->st_name, 1);
}

static char sym_type_letter(struct params *params, Elf36_Sym *sym)
{
    unsigned int st_type, st_bind;
    Elf36_Shdr *shdr;

    st_type = ELF36_ST_TYPE(sym->st_info);
    st_bind = ELF36_ST_BIND(sym->st_info);

    if (sym->st_shndx == SHN_ABS) {
	switch (st_type) {
	case STT_NOTYPE:
	case STT_OBJECT:
	case STT_FUNC:
	    if (st_bind == STB_GLOBAL)
		return 'A';
	    if (st_bind == STB_LOCAL)
		return 'a';
	}
	return '\0';
    }

    if (sym->st_shndx == SHN_UNDEF) {
	if (st_bind == STB_GLOBAL)
	    return 'U';
	return '\0';
    }

    if (sym->st_shndx == SHN_COMMON) {
	switch (st_type) {
	case STT_NOTYPE:
	case STT_OBJECT:
	    if (st_bind == STB_GLOBAL)
		return 'C';
	    if (st_bind == STB_LOCAL)
		return 'c';
	}
	return '\0';
    }

    if (st_type == STT_GNU_IFUNC)
	return 'i';

    if (sym->st_shndx >= params->shnum)
	return '\0';

    shdr = &params->shtab[sym->st_shndx];

    if (shdr->sh_type == SHT_NOBITS
	&& ((shdr->sh_flags & (SHF_ALLOC | SHF_WRITE)) == (SHF_ALLOC | SHF_WRITE))) {
	if (st_bind == STB_GLOBAL)
	    return 'B';
	if (st_bind == STB_LOCAL)
	    return 'b';
	return '\0';
    }

    if (shdr->sh_type == SHT_PROGBITS) {
	if ((shdr->sh_flags & (SHF_ALLOC | SHF_EXECINSTR)) == (SHF_ALLOC | SHF_EXECINSTR)) {
	    if (st_bind == STB_GLOBAL)
		return 'T';
	    if (st_bind == STB_LOCAL)
		return 't';
	    return '\0';
	}
	if ((shdr->sh_flags & (SHF_ALLOC | SHF_WRITE)) == (SHF_ALLOC | SHF_WRITE)) {
	    if (st_bind == STB_GLOBAL)
		return 'D';
	    if (st_bind == STB_LOCAL)
		return 'd';
	    return '\0';
	}
	if (shdr->sh_flags & SHF_ALLOC) {
	    if (st_bind == STB_GLOBAL)
		return 'R';
	    if (st_bind == STB_LOCAL)
		return 'r';
	    return '\0';
	}
	return '\0';
    }

    return '\0';
}

static pdp10_uint36_t sym_value(const struct params *params, const Elf36_Sym *sym)
{
    pdp10_uint36_t base;

    base = 0;
    if (sym->st_shndx < params->shnum && sym->st_shndx != SHN_ABS && sym->st_shndx != SHN_COMMON) {
	Elf36_Shdr *shdr = &params->shtab[sym->st_shndx];
	if (shdr->sh_type == SHT_PROGBITS
	    && (shdr->sh_flags & SHF_ALLOC))
	    base = shdr->sh_addr;
    }
    return base + sym->st_value;
}

static int print_sym(struct params *params, pdp10_uint36_t i)
{
    Elf36_Sym *sym;
    char type;
    pdp10_uint36_t value;

    sym = &params->symtab[i];

    type = sym_type_letter(params, sym);
    if (type == '\0')	/* ignored */
	return 0;

    /* XXX: handle --extern-only, --undefined-only, --defined-only */
    /* XXX: handle --format={bsd,sysv,posix} */

    if (params->opts.print_file_name)
	printf("%s:", params->filename);

    value = sym_value(params, sym);
    switch (params->opts.radix) {
    case 'x':
	printf("%0*" PDP10_PRIx36, 9, value);
	break;
    case 'd':
	printf("%0*" PDP10_PRIu36, 11, value);
	break;
    case 'o':
	printf("%0*" PDP10_PRIo36, 12, value);
	break;
    }
    printf(" %c ", type);
    if (print_sym_name(params, sym) < 0)
	return -1;
    printf("\n");

    return 0;
}

static int sym_cmp_value(const struct params *params, const Elf36_Sym *sym1, const Elf36_Sym *sym2)
{
    pdp10_uint36_t val1, val2;

    val1 = sym_value(params, sym1);
    val2 = sym_value(params, sym2);

    if (val1 < val2)
	return -1;
    if (val1 > val2)
	return 1;
    return 0;
}

static int sym_cmp_name(const struct params *params, const Elf36_Sym *sym1, const Elf36_Sym *sym2)
{
    pdp10_uint36_t i;

    i = 0;
    for (;; ++i) {
	pdp10_uint9_t c1, c2;

	if (sym1->st_name + i >= params->strtablen)
	    c1 = 0;
	else
	    c1 = params->strtab[sym1->st_name + i];

	if (sym2->st_name + i >= params->strtablen)
	    c2 = 0;
	else
	    c2 = params->strtab[sym2->st_name + i];

	if (c1 < c2)
	    return -1;
	else if (c1 > c2)
	    return 1;
	else if (c1 == 0)
	    return 0;
    }
}

static struct params *the_params;	/* XXX: kludge */

static int sym_cmp(const void *p1, const void *p2)
{
    Elf36_Sym *sym1 = (Elf36_Sym*)p1;
    Elf36_Sym *sym2 = (Elf36_Sym*)p2;
    int cmp;

    if (the_params->opts.numeric_sort)
	cmp = sym_cmp_value(the_params, sym1, sym2);
    else
	cmp = sym_cmp_name(the_params, sym1, sym2);

    if (the_params->opts.reverse_sort) {
	if (cmp < 0)
	    cmp = 1;
	else if (cmp > 0)
	    cmp = -1;
    }

    return cmp;
}

static void symtab_sort(struct params *params)
{
    if (params->opts.no_sort)
	return;

    /* We need access to `params' in the comparison routines.
       Ideally qsort() should take an optional context parameter,
       but it doesn't.  A function closure could be used instead,
       but that's non-standard C.  So temporarily store `params'
       in a global variable.  */
    the_params = params;
    qsort(params->symtab, params->symnum, sizeof params->symtab[0], sym_cmp);
    the_params = NULL;
}

static int print_symtab(struct params *params)
{
    pdp10_uint36_t i;

    if (params->several_files)
	printf("\n%s:\n", params->filename);

    symtab_sort(params);

    for (i = 0; i < params->symnum; ++i)
	if (print_sym(params, i) < 0) {
	    fprintf(stderr, "%s: %s: failed to print symbol table entry %" PDP10_PRIu36 "\n",
		    params->progname, params->filename, i);
	    return -1;
	}

    return 0;
}

static int nm1(struct params *params)
{
    unsigned char e_ident[EI_NIDENT];

    if (pdp10_elf36_read_ehdr(params->pdp10fp, &params->ehdr) < 0) {
	fprintf(stderr, "%s: %s: failed to read ELF header: %s\n",
		params->progname, params->filename, strerror(errno));
	return -1;
    }
    ehdr_unpack_ident(&params->ehdr, e_ident);
    if (check_eident(params, e_ident) < 0
	|| check_ehdr(params) < 0)
	return -1;

    if (read_shtab(params) < 0) {
	fprintf(stderr, "%s: %s: failed to read section header table\n",
		params->progname, params->filename);
	return -1;
    }

    if (read_symtab(params) < 0) {
	fprintf(stderr, "%s: %s: read to read symbol table\n",
		params->progname, params->filename);
	return -1;
    }

    return print_symtab(params);
}

static int nm(struct params *params, char **files, int nrfiles)
{
    char fake_file[6];
    char *fake_files[1];
    int i, status;

    if (nrfiles <= 0) {
	fake_file[0] = 'a';
	fake_file[1] = '.';
	fake_file[2] = 'o';
	fake_file[3] = 'u';
	fake_file[4] = 't';
	fake_file[5] = '\0';
	fake_files[0] = fake_file;
	files = fake_files;
	nrfiles = 1;
    }

    if (nrfiles > 1)
	params->several_files = 1;

    for (i = 0; i < nrfiles; ++i) {
	params_file_init(params);

	params->filename = files[i];
	params->pdp10fp = pdp10_fopen(params->filename, "rb");
	if (!params->pdp10fp) {
	    fprintf(stderr, "%s: %s: failed to open: %s\n",
		    params->progname, params->filename, strerror(errno));
	    return -1;
	}
	status = nm1(params);

	params_file_fini(params);

	if (status < 0)
	    return -1;
    }

    return 0;
}

/*
 * Command-line interface.
 */
enum lopt {
    LOPT_no_demangle = 1,
    LOPT_special_syms = 2,
    LOPT_defined_only = 3,
};

static const struct option long_options[] = {
    /*
     * Long-only options:
     */
    { "no-demangle",		no_argument,		0,	LOPT_no_demangle },
    { "special-syms",		no_argument,		0,	LOPT_special_syms },
    { "defined-only",		no_argument,		0, 	LOPT_defined_only },
    /*
     * Long aliases for short options:
     */
    { "print-file-name",	no_argument,		0,	'A' },
    { "dynamic",		no_argument,		0,	'D' },
    { "format",			required_argument,	0,	'f' },
    { "extern-only",		no_argument,		0,	'g' },
    { "numeric-sort",		no_argument,		0,	'v' },
    { "no-sort",		no_argument,		0,	'p' },
    { "portability",		no_argument,		0,	'P' },
    { "print-size",		no_argument,		0,	'S' },
    { "reverse-sort",		no_argument,		0,	'r' },
    { "radix",			required_argument,	0,	't' },
    { "undefined-only",		no_argument,		0,	'u' },
    { "version",		no_argument,		0,	'V' },
    /*
     * NYI options:
     * -a / --debug-syms
     * --demangle
     * --plugin <name>
     * -l / --line-numbers
     * --size-sort
     * -s / --print-armap [TODO]
     * --target=<bfdname>
     * -X 32_64
     * --help [TODO?]
     * @file
     */
    { 0,			0,			0,	0 }
};

static void usage(const char *progname)
{
    fprintf(stderr, "Usage: %s [options..] [files..]\n", progname);
}

int main(int argc, char **argv)
{
    struct params params;

    params_init(&params);
    params.progname = argv[0];
    params.opts.radix = 'x';

    for (;;) {
	int ch;

	ch = getopt_long(argc, argv, "AoBDf:gnvpPSrt:uV", long_options, NULL);
	switch (ch) {
	case 'A':
	case 'o':
	    params.opts.print_file_name = 1;
	    continue;
	case 'B':
	    params.opts.format = 'b';
	    continue;
	case LOPT_no_demangle:
	    /* default */
	    continue;
	case 'D':
	    params.opts.dynamic = 1;
	    continue;
	case 'f':
	    switch (optarg[0]) {
	    case 'b':
	    case 's':
	    case 'p':
		params.opts.format = optarg[0];
		continue;
	    default:
		fprintf(stderr, "%s: invalid format '%s'\n", params.progname, optarg);
		return 1;
	    }
	case 'g':
	    params.opts.extern_only = 1;
	    continue;
	case 'n':
	case 'v':
	    params.opts.numeric_sort = 1;
	    params.opts.no_sort = 0;
	    continue;
	case 'p':
	    params.opts.no_sort = 1;
	    params.opts.numeric_sort = 0;
	    params.opts.reverse_sort = 0;
	    continue;
	case 'P':
	    params.opts.format = 'p';
	    continue;
	case 'S':
	    params.opts.print_size = 1;
	    continue;
	case 'r':
	    params.opts.reverse_sort = 1;
	    params.opts.no_sort = 0;
	    continue;
	case LOPT_special_syms:
	    /* nothing to do, ignore */
	    continue;
	case 't':
	    switch (optarg[0]) {
	    case 'd':
	    case 'o':
	    case 'x':
		if (optarg[1] == '\0') {
		    params.opts.radix = optarg[0];
		    continue;
		}
		/*FALLTHROUGH*/
	    default:
		fprintf(stderr, "%s: invalid radix '%s'\n", params.progname, optarg);
		return 1;
	    }
	case 'u':
	    params.opts.undefined_only = 1;
	    continue;
	case LOPT_defined_only:
	    params.opts.defined_only = 1;
	    continue;
	case 'V':
	    printf(VERSION);
	    return 0;
	case -1:
	    break;
	default:
	    usage(params.progname);
	    return 1;
	}
	break;
    }

    if (nm(&params, &argv[optind], argc - optind) < 0)
	return 1;

    return 0;
}
