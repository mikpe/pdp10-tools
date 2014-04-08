/*
 * readelf.c
 *
 * readelf/objdump clone for PDP10 Elf36 files.
 */
#define _GNU_SOURCE	/* for getopt_long() */
#include <errno.h>
#include <getopt.h>	/* for getopt_long() */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pdp10-elf36.h"
#include "pdp10-inttypes.h"
#include "pdp10-opcodes.h"
#include "pdp10-stdio.h"

#define VERSION "pdp10-tools readelf version 0.1, built " __DATE__ " " __TIME__ "\n"

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

struct params {
    const char *progname;
    struct options opts;

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

    Elf36_Sym **labels;		/* pointers into ->symtab, only current .text section, sorted on increasing ->st_value */
    pdp10_uint36_t labelsnum;
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
    params->labels = NULL;
    params->labelsnum = 0;
}

static void params_file_fini(struct params *params)
{
    free(params->labels);
    free(params->strtab);
    free(params->symtab);
    free(params->shstrtab);
    free(params->shtab);
    pdp10_fclose(params->pdp10fp);
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

static int check_ehdr(struct params *params)
{
    const Elf36_Ehdr *ehdr = &params->ehdr;
    const Elf36_Uchar *e_ident = ehdr->e_ident;

    if (e_ident[EI_MAG0] != ELFMAG0
	|| e_ident[EI_MAG1] != ELFMAG1
	|| e_ident[EI_MAG2] != ELFMAG2
	|| e_ident[EI_MAG3] != ELFMAG3) {
	fprintf(stderr, "%s: %s: not an ELF file: wrong magic\n", params->progname, params->filename);
	return -1;
    }

    if (e_ident[EI_CLASS] != ELFCLASS36) {
	fprintf(stderr, "%s: %s: not an ELF36 file: wrong class %u (%s)\n",
		params->progname, params->filename, e_ident[EI_CLASS], class_name(e_ident[EI_CLASS]));
	return -1;
    }

    if (e_ident[EI_DATA] != ELFDATA2MSB) {
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong data %u (%s)\n",
		params->progname, params->filename, e_ident[EI_DATA], data_name(e_ident[EI_DATA]));
	return -1;
    }

    if (e_ident[EI_VERSION] != EV_CURRENT) {
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong version %u (%s)\n",
		params->progname, params->filename, e_ident[EI_VERSION], version_name(e_ident[EI_VERSION]));
	return -1;
    }

    switch (e_ident[EI_OSABI]) {
    case ELFOSABI_NONE:
    case ELFOSABI_LINUX:
	break;
    default:
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong osabi %u (%s)\n",
		params->progname, params->filename, e_ident[EI_OSABI], osabi_name(e_ident[EI_OSABI]));
	return -1;
    }

    if (e_ident[EI_ABIVERSION] != 0) {
	fprintf(stderr, "%s: %s: not a PDP10 ELF36 file: wrong abiversion %u\n",
		params->progname, params->filename, e_ident[EI_ABIVERSION]);
	return -1;
    }

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

static void print_ehdr(const Elf36_Ehdr *ehdr)
{
    const Elf36_Uchar *e_ident = ehdr->e_ident;
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

static const char *sh_type_name(Elf36_Word sh_type, char *buf)
{
    switch (sh_type) {
    case SHT_NULL:
	return "NULL";
    case SHT_PROGBITS:
	return "PROGBITS";
    case SHT_SYMTAB:
	return "SYMTAB";
    case SHT_STRTAB:
	return "STRTAB";
    case SHT_RELA:
	return "RELA";
    case SHT_HASH:
	return "HASH";
    case SHT_DYNAMIC:
	return "DYNAMIC";
    case SHT_NOTE:
	return "NOTE";
    case SHT_NOBITS:
	return "NOBITS";
    case SHT_REL:
	return "REL";
    case SHT_SHLIB:
	return "SHLIB";
    case SHT_DYNSYM:
	return "DYNSYM";
    case SHT_INIT_ARRAY:
	return "INIT_ARRAY";
    case SHT_FINI_ARRAY:
	return "FINI_ARRAY";
    case SHT_PREINIT_ARRAY:
	return "PREINIT_ARRAY";
    case SHT_GROUP:
	return "GROUP";
    case SHT_SYMTAB_SHNDX:
	return "SYMTAB_SHNDX";
    case SHT_GNU_INCREMENTAL_INPUTS:
	return "GNU_INCREMENTAL_INPUTS";
    case SHT_GNU_ATTRIBUTES:
	return "GNU_ATTRIBUTES";
    case SHT_GNU_HASH:
	return "GNU_HASH";
    case SHT_GNU_LIBLIST:
	return "GNU_LIBLIST";
    case SHT_GNU_verdef:
	return "GNU_verdef";
    case SHT_GNU_verneed:
	return "GNU_verneed";
    case SHT_GNU_versym:
	return "GNU_versym";
    default:
	sprintf(buf, "%" PDP10_PRIu36, sh_type);
	return buf;
    }
}

static int read_strtab(struct params *params, pdp10_uint36_t i, pdp10_uint9_t **strtab_ptr, pdp10_uint36_t *strtablen_ptr, const char *kind)
{
    pdp10_uint9_t *strtab;
    pdp10_uint36_t strtablen;
    char sh_type_buf[16];

    if (i == 0 || i >= params->shnum) {
	fprintf(stderr, "%s: %s: invalid index %" PDP10_PRIu36 " for %s string table\n",
		params->progname, params->filename, i, kind);
	return -1;
    }
    if (params->shtab[i].sh_type != SHT_STRTAB) {
	fprintf(stderr, "%s: %s: %s string table at index %" PDP10_PRIu36 " has wrong type %s\n",
		params->progname, params->filename, kind, i,
		sh_type_name(params->shtab[i].sh_type, sh_type_buf));
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

static char *sh_flags_name(Elf36_Word sh_flags, char *buf)
{
    const char flagnames[12] = "WAXxMSILOGTZ";
    char *p;
    unsigned int i;
    Elf36_Word tmp;

    p = buf;
    tmp = sh_flags;
    if (tmp == 0) {
	*p++ = '-';
    } else {
	for (i = 0; i < 12; ++i)
	    if (i != 3 && tmp & (1 << i)) {
		tmp ^= (1 << i);
		*p++ = flagnames[i];
	    }
	if (tmp & SHF_EXCLUDE) {
	    tmp ^= SHF_EXCLUDE;
	    *p++ = 'E';
	}
    }
    if (tmp != 0)
	sprintf(buf, "0x%" PDP10_PRIx36, sh_flags);
    else
	*p = '\0';
    return buf;
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

static int print_sh_name(struct params *params, pdp10_uint36_t i, int say_empty)
{
    return print_name(params, params->shstrtab, params->shstrtablen, params->shtab[i].sh_name, say_empty);
}

static int print_shdr(struct params *params, pdp10_uint36_t i)
{
    Elf36_Shdr *shdr;
    char sh_type_buf[16], sh_flags_buf[16];

    printf("  [%" PDP10_PRIu36 "] ", i);
    if (print_sh_name(params, i, 1) < 0)
	return -1;
    shdr = &params->shtab[i];
    printf(" %s 0x%" PDP10_PRIx36 " %" PDP10_PRIu36 " %" PDP10_PRIu36 " %" PDP10_PRIu36
	   " %s %" PDP10_PRIu36 " %" PDP10_PRIu36 " %" PDP10_PRIu36 "\n",
	   sh_type_name(shdr->sh_type, sh_type_buf),
	   shdr->sh_addr, shdr->sh_offset, shdr->sh_size, shdr->sh_entsize,
	   sh_flags_name(shdr->sh_flags, sh_flags_buf),
	   shdr->sh_link, shdr->sh_info, shdr->sh_addralign);
    return 0;
}

static int print_shtab(struct params *params)
{
    pdp10_uint36_t i;

    printf("Section Headers:\n");
    printf("  [Nr] Name Type Addr Off Size ES Flg Lk Inf Al\n");
    for (i = 0; i < params->shnum; ++i)
	if (print_shdr(params, i) < 0) {
	    fprintf(stderr, "%s: %s: failed to print section header index %" PDP10_PRIu36 "\n",
		    params->progname, params->filename, i);
	    return -1;
	}
    printf("Key to Flags:\n");
    printf("  W (write), A (alloc), X (execute), M (merge), S (strings), Z (compressed)\n");
    printf("  I (info), L (link order), G (group), T (TLS), E (exclude), x (unknown)\n");
    printf("  O (extra OS processing required), o (OS specific), p (processor specific)\n");
    printf("\n");
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

static const char *st_type_name(unsigned int st_type, char *buf)
{
    switch (st_type) {
    case STT_NOTYPE:
	return "NOTYPE";
    case STT_OBJECT:
	return "OBJECT";
    case STT_FUNC:
	return "FUNC";
    case STT_SECTION:
	return "SECTION";
    case STT_FILE:
	return "FILE";
    case STT_COMMON:
	return "COMMON";
    case STT_TLS:
	return "TLS";
    case STT_RELC:
	return "RELC";
    case STT_SRELC:
	return "SRELC";
    case STT_GNU_IFUNC:
	return "GNU_IFUNC";
    default:
	sprintf(buf, "%u", st_type);
	return buf;
    }
}

static const char *st_bind_name(unsigned int st_bind, char *buf)
{
    switch (st_bind) {
    case STB_LOCAL:
	return "LOCAL";
    case STB_GLOBAL:
	return "GLOBAL";
    case STB_WEAK:
	return "WEAK";
    case STB_GNU_UNIQUE:
	return "GNU_UNIQUE";
    default:
	sprintf(buf, "%u", st_bind);
	return buf;
    }
}

static const char *st_vis_name(unsigned int st_vis, char *buf)
{
    switch (st_vis) {
    case STV_DEFAULT:
	return "DEFAULT";
    case STV_INTERNAL:
	return "INTERNAL";
    case STV_HIDDEN:
	return "HIDDEN";
    case STV_PROTECTED:
	return "PROTECTED";
    default:
	sprintf(buf, "%u", st_vis);
	return buf;
    }
}

static int print_sym(struct params *params, pdp10_uint36_t i)
{
    Elf36_Sym *sym;
    char st_type_buf[16], st_bind_buf[16], st_vis_buf[16];

    sym = &params->symtab[i];
    printf("  %" PDP10_PRIu36 " 0x%" PDP10_PRIx36 " %" PDP10_PRIu36 " %s %s %s %" PDP10_PRIu18 " ",
	   i, sym->st_value, sym->st_size,
	   st_type_name(ELF36_ST_TYPE(sym->st_info), st_type_buf),
	   st_bind_name(ELF36_ST_BIND(sym->st_info), st_bind_buf),
	   st_vis_name(ELF36_ST_VISIBILITY(sym->st_other), st_vis_buf),
	   sym->st_shndx);
    if (print_sym_name(params, sym) < 0)
	return -1;
    printf("\n");
    return 0;
}

static int print_symtab(struct params *params)
{
    pdp10_uint36_t i;

    printf("Symbol table '");
    if (print_sh_name(params, params->symtabndx, 0) < 0)
	return -1;
    printf("' in section %" PDP10_PRIu36 " contains %" PDP10_PRIu36 " entries:\n",
	   params->symtabndx, params->symnum);
    printf("  Num Value Size Type Bind Vis Ndx Name\n");
    for (i = 0; i < params->symnum; ++i)
	if (print_sym(params, i) < 0) {
	    fprintf(stderr, "%s: %s: failed to print symbol table entry %" PDP10_PRIu36 "\n",
		    params->progname, params->filename, i);
	    return -1;
	}
    printf("\n");
    return 0;
}

static int sym_matches_textshndx(struct params *params, pdp10_uint36_t textshndx, Elf36_Sym *sym)
{
    return
	sym->st_shndx == textshndx	/* XXX: NYI: SHN_XINDEX */
	&& ELF36_ST_TYPE(sym->st_info) == STT_FUNC	/* XXX: labels? */
	&& (ELF36_ST_BIND(sym->st_info) == STB_GLOBAL
	    || ELF36_ST_BIND(sym->st_info) == STB_LOCAL
	    || ELF36_ST_BIND(sym->st_info) == STB_WEAK);
}

static int labelscmp(const void *p1, const void *p2)
{
    Elf36_Sym *sym1 = *(Elf36_Sym**)p1;
    Elf36_Sym *sym2 = *(Elf36_Sym**)p2;

    if (sym1->st_value < sym2->st_value)
	return -1;
    if (sym1->st_value > sym2->st_value)
	return 1;
    return 0;
}

static int init_labels(struct params *params, pdp10_uint36_t textshndx)
{
    pdp10_uint36_t symndx, labelsnum;

    free(params->labels);
    params->labels = NULL;

    labelsnum = 0;
    for (symndx = 0; symndx < params->symnum; ++symndx)
	if (sym_matches_textshndx(params, textshndx, &params->symtab[symndx]))
	    ++labelsnum;

    params->labels = malloc(labelsnum * sizeof(Elf36_Sym*));
    if (!params->labels) {
	fprintf(stderr, "%s: %s: failed to allocate %zu bytes for labels table: %s\n",
		params->progname, params->filename, labelsnum * sizeof(Elf36_Sym*), strerror(errno));
	return -1;
    }
    params->labelsnum = labelsnum;

    labelsnum = 0;
    for (symndx = 0; symndx < params->symnum; ++symndx)
	if (sym_matches_textshndx(params, textshndx, &params->symtab[symndx])) {
	    params->labels[labelsnum] = &params->symtab[symndx];
	    ++labelsnum;
	}

    qsort(params->labels, params->labelsnum, sizeof params->labels[0], labelscmp);
    return 0;
}

static int disassemble_section(struct params *params, pdp10_uint36_t shndx)
{
    Elf36_Shdr *shdr;
    pdp10_uint36_t labelnr;
    pdp10_uint36_t i;
    pdp10_uint36_t insnword;
    const struct pdp10_instruction *insndesc;

    shdr = &params->shtab[shndx];
    if (shdr->sh_type != SHT_PROGBITS)
	return 0;
    if (shdr->sh_flags != (SHF_ALLOC | SHF_EXECINSTR))
	return 0;

    printf("Disassembly of section nr %" PDP10_PRIu36 " ", shndx);
    if (print_sh_name(params, shndx, 1) < 0)
	return -1;
    printf(":\n\n");

    if (pdp10_fseeko(params->pdp10fp, shdr->sh_offset, PDP10_SEEK_SET) < 0) {
	fprintf(stderr, "%s: %s: failed to seek to section %" PDP10_PRIu36 " at %" PDP10_PRIu36 ": %s\n",
		params->progname, params->filename, shndx, shdr->sh_offset, strerror(errno));
	return -1;
    }

    if (init_labels(params, shndx) < 0)
	return -1;
    labelnr = 0;

    for (i = 0; i < shdr->sh_size / 4; ++i) {
	if (pdp10_elf36_read_uint36(params->pdp10fp, &insnword) < 0) {
	    fprintf(stderr, "%s: %s: failed to read instruction at 0x%" PDP10_PRIx36 ": %s\n",
		    params->progname, params->filename, i * 4, strerror(errno));
	    return -1;
	}

	if (labelnr < params->labelsnum
	    && (i * 4) == params->labels[labelnr]->st_value) {
	    printf("0x%09" PDP10_PRIx36 " <", i * 4);
	    if (print_sym_name(params, params->labels[labelnr]) < 0)
		return -1;
	    printf(">:\n");
	    ++labelnr;
	}

	printf(" 0x%" PDP10_PRIx36 ":\t0%" PDP10_PRIo36 "\t", i * 4, insnword);
	insndesc = pdp10_instruction_from_high13(insnword >> (36 - 13));
	if (!insndesc) {
	    printf("(bad)\n");
	    continue;
	}
	printf("%s ", insndesc->name);

	if (!((insndesc->type & PDP10_A_OPCODE)
	      || (((insnword >> 23) & 0xF) == 0
		  && (insndesc->type & PDP10_A_UNUSED))))
	    printf("%u,", (unsigned int)(insnword >> 23) & 0xF);

	if (insnword & (1 << 22))
	    printf("@");

	printf("%u", (unsigned int)insnword & ((1 << 18) - 1));

	if (((insnword >> 18) & 0xF) != 0)
	    printf("(%u)", (unsigned int)(insnword >> 18) & 0xF);

	printf("\n");
    }

    return 1;
}

static int disassemble_all(struct params *params)
{
    pdp10_uint36_t shndx;

    for (shndx = 0; shndx < params->shnum; ++shndx)
	if (disassemble_section(params, shndx) < 0)
	    return -1;

    return 0;
}

static int readelf(struct params *params)
{
    if (pdp10_elf36_read_ehdr(params->pdp10fp, &params->ehdr) < 0) {
	fprintf(stderr, "%s: %s: failed to read ELF header: %s\n",
		params->progname, params->filename, strerror(errno));
	return -1;
    }
    if (check_ehdr(params) < 0)
	return -1;

    if (params->opts.file_header)
	print_ehdr(&params->ehdr);

    if (read_shtab(params) < 0) {
	fprintf(stderr, "%s: %s: failed to read section header table\n",
		params->progname, params->filename);
	return -1;
    }

    if (params->opts.sections
	&& print_shtab(params) < 0)
	return -1;

    if (read_symtab(params) < 0) {
	fprintf(stderr, "%s: %s: read to read symbol table\n",
		params->progname, params->filename);
	return -1;
    }

    if (params->opts.symbols
	&& print_symtab(params) < 0)
	return -1;

    if (params->opts.disassemble
	&& disassemble_all(params) < 0)
	return -1;

    return 0;
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
    struct params params;
    int opt_version;
    int i;

    params_init(&params);
    params.progname = argv[0];
    opt_version = 0;

    for (;;) {
	int ch;

	ch = getopt_long(argc, argv, "ahlSgtesnrudVADcv", long_options, NULL);
	switch (ch) {
	case 'a':
	    params.opts.symbols = 1;
	    params.opts.relocs = 1;
	    params.opts.dynamic = 1;
	    params.opts.notes = 1;
	    params.opts.version_info = 1;
	    /*FALLTHROUGH*/
	case 'e':
	    params.opts.file_header = 1;
	    params.opts.segments = 1;
	    params.opts.sections = 1;
	    continue;
	case 'h':
	    params.opts.file_header = 1;
	    continue;
	case 'l':
	    params.opts.segments = 1;
	    continue;
	case 't':
	    params.opts.section_details = 1;
	    /*FALLTHROUGH*/
	case 'S':
	    params.opts.sections = 1;
	    continue;
	case 'g':
	    params.opts.section_groups = 1;
	    continue;
	case 's':
	    params.opts.symbols = 1;
	    continue;
	case 'n':
	    params.opts.notes = 1;
	    continue;
	case 'r':
	    params.opts.relocs = 1;
	    continue;
	case 'u':
	    params.opts.unwind = 1;
	    continue;
	case 'd':
	    params.opts.dynamic = 1;
	    continue;
	case 'V':
	    params.opts.version_info = 1;
	    continue;
	case 'A':
	    params.opts.arch_specific = 1;
	    continue;
	case 'D':
	    params.opts.use_dynamic = 1;
	    continue;
	case 'c':
	    params.opts.archive_index = 1;
	    continue;
	case 'v':
	    opt_version = 1;
	    continue;
	case LOPT_dyn_syms:
	    params.opts.dyn_syms = 1;
	    continue;
	case LOPT_disassemble:	/* local extension */
	    params.opts.disassemble = 1;
	    continue;
	case -1:
	    break;
	default:
	    usage(params.progname);
	    return 1;
	}
	break;
    }

    if (optind >= argc && !opt_version) {
	usage(params.progname);
	return 1;
    }

    if (opt_version)
	printf(VERSION);

    for (i = optind; i < argc; ++i) {
	int status;

	params_file_init(&params);

	params.filename = argv[i];
	params.pdp10fp = pdp10_fopen(params.filename, "rb");
	if (!params.pdp10fp) {
	    fprintf(stderr, "%s: failed to open %s: %s\n",
		    params.progname, params.filename, strerror(errno));
	    return -1;
	}
	status = readelf(&params);

	params_file_fini(&params);

	if (status < 0)
	    return 1;
    }

    return 0;
}
