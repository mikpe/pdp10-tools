/*
 * readelf.c
 *
 * readelf/objdump clone for PDP10 Elf36 files.
 */
#define _GNU_SOURCE	/* for getopt_long() */
#include <errno.h>
#include <getopt.h>	/* for getopt_long() */
#include <stdio.h>
#include <string.h>
#include "pdp10-elf36.h"
#include "pdp10-inttypes.h"
#include "pdp10-stdio.h"

struct options {
    unsigned char file_header;
    unsigned char segments;
    unsigned char sections;
    unsigned char section_groups;
    unsigned char section_details;
    unsigned char symbols;
    unsigned char dyn_syms;
    unsigned char notes;
    unsigned char relocs;
    unsigned char unwind;
    unsigned char dynamic;
    unsigned char version_info;
    unsigned char arch_specific;
    unsigned char use_dynamic;
    unsigned char archive_index;
    unsigned char disassemble;		/* local extension */
};

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

static int check_eident(const unsigned char *e_ident, const char *filename)
{
    if (e_ident[EI_MAG0] != ELFMAG0
	|| e_ident[EI_MAG1] != ELFMAG1
	|| e_ident[EI_MAG2] != ELFMAG2
	|| e_ident[EI_MAG3] != ELFMAG3
	|| e_ident[EI_VERSION] != EV_CURRENT) {
	fprintf(stderr, "readelf: %s: not an ELF file: wrong magic\n", filename);
	return -1;
    }

    if (e_ident[EI_CLASS] != ELFCLASS36) {
	fprintf(stderr, "readelf: %s: not an ELF36 file: wrong class %u\n", filename, e_ident[EI_CLASS]);
	return -1;
    }

    if (e_ident[EI_DATA] != ELFDATA2MSB) {
	fprintf(stderr, "readelf: %s: not a PDP10 ELF36 file: wrong data %u\n", filename, e_ident[EI_DATA]);
	return -1;
    }

    switch (e_ident[EI_OSABI]) {
    case ELFOSABI_NONE:
    case ELFOSABI_LINUX:
	break;
    default:
	fprintf(stderr, "readelf: %s: not a PDP10 ELF36 file: wrong osabi %u\n", filename, e_ident[EI_OSABI]);
	return -1;
    }

    if (e_ident[EI_ABIVERSION] != 0) {
	fprintf(stderr, "readelf: %s: not a PDP10 ELF36 file: wrong abiversion %u\n", filename, e_ident[EI_ABIVERSION]);
	return -1;
    }

    return 0;
}

static int check_ehdr(const Elf36_Ehdr *ehdr, const char *filename)
{
    switch (ehdr->e_type) {
    case ET_REL:
    case ET_EXEC:
    case ET_DYN:
    case ET_CORE:
	break;
    default:
	fprintf(stderr, "readelf: %s: not a PDP10 ELF36 file: wrong type %u\n", filename, ehdr->e_type);
	return -1;
    }

    if (ehdr->e_machine != EM_PDP10) {
	fprintf(stderr, "readelf: %s: not a PDP10 ELF36 file: wrong machine %u\n", filename, ehdr->e_machine);
	return -1;
    }

    if (ehdr->e_version != EV_CURRENT) {
	fprintf(stderr, "readelf: %s: not a PDP10 ELF36 file: wrong version %" PDP10_PRIu36 "\n", filename, ehdr->e_version);
	return -1;
    }

    if (ehdr->e_ehsize != ELF36_EHDR_SIZEOF) {
	fprintf(stderr, "readelf: %s: not a PDP10 ELF36 file: wrong ehsize %u\n", filename, ehdr->e_ehsize);
	return -1;
    }

    return 0;
}

static const char *class_name(unsigned int ei_class)
{
    switch (ei_class) {
    case ELFCLASS32:
	return "ELF32";
    case ELFCLASS64:
	return "ELF64";
    case ELFCLASS36:
	return "ELF36";
    default:
	return "?";
    }
}

static const char *data_name(unsigned int ei_data)
{
    switch (ei_data) {
    case ELFDATA2LSB:
	return "2's complement, little endian";
    case ELFDATA2MSB:
	return "2's complement, big endian";
    default:
	return "?";
    }
}

static const char *version_name(unsigned int ei_version)
{
    return (ei_version == EV_CURRENT) ? "current" : "?";
}

static const char *osabi_name(unsigned int ei_osabi)
{
    switch (ei_osabi) {
    case ELFOSABI_NONE:
	return "Generic";
    case ELFOSABI_LINUX:
	return "Linux";
    default:
	return "?";
    }
}

static const char *type_name(unsigned int e_type)
{
    switch (e_type) {
    case ET_REL:
	return "Relocatable file";
    case ET_EXEC:
	return "Executable file";
    case ET_DYN:
	return "Shared object file";
    case ET_CORE:
	return "Core file";
    default:
	return "?";
    }
}

static const char *machine_name(unsigned int e_machine)
{
    switch (e_machine) {
    case EM_PDP10:
	return "Digital Equipment Corp. PDP-10";
    default:
	return "?";
    }
}

static void print_ehdr(const Elf36_Ehdr *ehdr, const unsigned char *e_ident)
{
    unsigned int i;

    printf("ELF Header:\n");
    printf("  Magic:\t\t\t\t");
    for (i = 0; i < EI_NIDENT; ++i) {
	if (i > 0)
	    printf(" ");
	printf("%02x", e_ident[i]);
    }
    printf("\n");
    printf("  Class:\t\t\t\t%u (%s)\n", e_ident[EI_CLASS], class_name(e_ident[EI_CLASS]));
    printf("  Data:\t\t\t\t\t%u (%s)\n", e_ident[EI_DATA], data_name(e_ident[EI_DATA]));
    printf("  Version:\t\t\t\t%u (%s)\n", e_ident[EI_VERSION], version_name(e_ident[EI_VERSION]));
    printf("  OS/ABI:\t\t\t\t%u (%s)\n", e_ident[EI_OSABI], osabi_name(e_ident[EI_OSABI]));
    printf("  ABI Version:\t\t\t\t%u\n", e_ident[EI_ABIVERSION]);
    printf("  Type:\t\t\t\t\t%u (%s)\n", ehdr->e_type, type_name(ehdr->e_type));
    printf("  Machine:\t\t\t\t%u (%s)\n", ehdr->e_machine, machine_name(ehdr->e_machine));
    printf("  Version:\t\t\t\t%" PDP10_PRIu36 " (%s)\n", ehdr->e_version, version_name(ehdr->e_version));
    printf("  Entry point address:\t\t\t0x%" PDP10_PRIx36 "\n", ehdr->e_entry);
    printf("  Start of program headers:\t\t%" PDP10_PRIu36 "\n", ehdr->e_phoff);
    printf("  Start of section headers:\t\t%" PDP10_PRIu36 "\n", ehdr->e_shoff);
    printf("  Flags:\t\t\t\t0x%" PDP10_PRIx36 "\n", ehdr->e_flags);
    printf("  Size of this header:\t\t\t%u\n", ehdr->e_ehsize);
    printf("  Size of program headers:\t\t%u\n", ehdr->e_phentsize);
    printf("  Number of program headers:\t\t%u\n", ehdr->e_phnum);
    printf("  Size of section headers:\t\t%u\n", ehdr->e_shentsize);
    printf("  Number of section headers:\t\t%u\n", ehdr->e_shnum);
    printf("  Section header string table index:\t%u\n", ehdr->e_shstrndx);
    printf("\n");
}

static int do_readelf_fp(const struct options *options, PDP10_FILE *pdp10fp, const char *filename)
{
    Elf36_Ehdr ehdr;
    unsigned char e_ident[EI_NIDENT];

    if (pdp10_elf36_read_ehdr(pdp10fp, &ehdr) < 0) {
	fprintf(stderr, "readelf: %s: failed to read ELF header: %s\n", filename, strerror(errno));
	return -1;
    }
    ehdr_unpack_ident(&ehdr, e_ident);
    if (check_eident(e_ident, filename) < 0
	|| check_ehdr(&ehdr, filename) < 0)
	return -1;

    if (options->file_header)
	print_ehdr(&ehdr, e_ident);

    return 0;
}

static int do_readelf(const struct options *options, const char *filename)
{
    PDP10_FILE *pdp10fp;
    int status;

    pdp10fp = pdp10_fopen(filename, "rb");
    if (!pdp10fp) {
	fprintf(stderr, "%s: failed to open %s: %s\n", __FUNCTION__, filename, strerror(errno));
	return -1;
    }
    status = do_readelf_fp(options, pdp10fp, filename);
    pdp10_fclose(pdp10fp);
    return status;
}

/*
 * Command-line interface.
 */
enum lopt {
    LOPT_dyn_syms = 1,
    LOPT_disassemble = 2,		/* local extension */
};

static const struct option long_options[] = {
    /* long-only options */
    { "dyn-syms",		no_argument,	0,	LOPT_dyn_syms },
    { "disassemble",		no_argument,	0,	LOPT_disassemble },	/* local extension */
    /* long aliases for short options */
    { "all",			no_argument,	0,	'a' },
    { "file-header",		no_argument,	0,	'h' },
    { "program-headers",	no_argument,	0,	'l' },
    { "segments",		no_argument,	0,	'l' },
    { "section-groups",		no_argument,	0,	'g' },
    { "section-details",	no_argument,	0,	't' },
    { "headers",		no_argument,	0,	'e' },
    { "symbols",		no_argument,	0,	's' },
    { "syms",			no_argument,	0,	's' },
    { "notes",			no_argument,	0,	'n' },
    { "relocs",			no_argument,	0,	'r' },
    { "unwind",			no_argument,	0,	'u' },
    { "version-info",		no_argument,	0,	'V' },
    { "arch-specific",		no_argument,	0,	'A' },
    { "use-dynamic",		no_argument,	0,	'D' },
    { "archive-index",		no_argument,	0,	'c' },
    { "version",		no_argument,	0,	'v' },
    /* --{hex,string,relocated}-dump: NYI */
    /* -wide: NYI */
    /* --debug-dump: NYI */
    /* --dwarf-{depth,start}: NYI */
    /* --histogram: NYI */
    { 0,			0,		0,	0 }
};

static void usage(const char *progname)
{
    fprintf(stderr, "Usage: %s [options..] [files..]\n", progname);
}

int main(int argc, char **argv)
{
    struct options options;
    int opt_version;
    int i;

    memset(&options, 0, sizeof options);
    opt_version = 0;

    for (;;) {
	int ch;

	ch = getopt_long(argc, argv, "ahlSgtesnrudVADcv", long_options, NULL);
	switch (ch) {
	case 'a':
	    options.symbols = 1;
	    options.relocs = 1;
	    options.dynamic = 1;
	    options.notes = 1;
	    options.version_info = 1;
	    /*FALLTHROUGH*/
	case 'e':
	    options.file_header = 1;
	    options.segments = 1;
	    options.sections = 1;
	    continue;
	case 'h':
	    options.file_header = 1;
	    continue;
	case 'l':
	    options.segments = 1;
	    continue;
	case 't':
	    options.section_details = 1;
	    /*FALLTHROUGH*/
	case 'S':
	    options.sections = 1;
	    continue;
	case 'g':
	    options.section_groups = 1;
	    continue;
	case 's':
	    options.symbols = 1;
	    continue;
	case 'n':
	    options.notes = 1;
	    continue;
	case 'r':
	    options.relocs = 1;
	    continue;
	case 'u':
	    options.unwind = 1;
	    continue;
	case 'd':
	    options.dynamic = 1;
	    continue;
	case 'V':
	    options.version_info = 1;
	    continue;
	case 'A':
	    options.arch_specific = 1;
	    continue;
	case 'D':
	    options.use_dynamic = 1;
	    continue;
	case 'c':
	    options.archive_index = 1;
	    continue;
	case 'v':
	    opt_version = 1;
	    continue;
	case LOPT_dyn_syms:
	    options.dyn_syms = 1;
	    continue;
	case LOPT_disassemble:	/* local extension */
	    options.disassemble = 1;
	    continue;
	case -1:
	    break;
	default:
	    usage(argv[0]);
	    return 1;
	}
	break;
    }

    if (optind >= argc && !opt_version) {
	usage(argv[0]);
	return 1;
    }

    if (opt_version)
	printf("pdp10-tools readelf version 0.0 " __DATE__ " " __TIME__ "\n");

    for (i = optind; i < argc; ++i)
	if (do_readelf(&options, argv[i]) != 0)
	    return 1;

    return 0;
}
