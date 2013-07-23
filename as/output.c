/*
 * output.c
 */
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pdp10-elf36.h"
#include "pdp10-stdint.h"
#include "pdp10-stdio.h"
#include "assemble.h"
#include "output.h"

struct strtab_entry {
    struct strtab_entry *next;
    const char *string;
    unsigned int nrbytes;	/* strlen(string) + 1 */
};

struct strtab {
    struct strtab_entry *head;
    unsigned int nrbytes;
};

static void strtab_init(struct strtab *strtab)
{
    strtab->head = NULL;
    strtab->nrbytes = 0;
}

static pdp10_uint36_t strtab_enter(const char *progname, struct strtab *strtab, const char *name)
{
    struct strtab_entry *prev, *here;
    pdp10_uint36_t index;

    index = 1;
    prev = NULL;
    here = strtab->head;
    while (here != NULL) {
	if (strcmp(name, here->string) == 0)
	    return index;
	index += here->nrbytes;
	prev = here;
	here = here->next;
    }

    here = malloc(sizeof *here);
    if (!here) {
	fprintf(stderr, "%s: failed to allocate %zu bytes for a strtab_entry: %s\n",
		progname, sizeof *here, strerror(errno));
	return 0;
    }
    here->next = NULL;
    here->string = name;
    here->nrbytes = strlen(name) + 1;

    if (prev) {
	prev->next = here;
    } else {
	strtab->head = here;
	index = 1;
	strtab->nrbytes = 1;
    }

    strtab->nrbytes += here->nrbytes;

    return index;
}

static int strtab_write(PDP10_FILE *pdp10fp, const struct strtab *strtab)
{
    struct strtab_entry *here;
    unsigned int i;

    if (pdp10_elf36_write_uint9(pdp10fp, '\0') < 0)
	return -1;

    for (here = strtab->head; here; here = here->next)
	for (i = 0; i < here->nrbytes; ++i)
	    if (pdp10_elf36_write_uint9(pdp10fp, here->string[i]) < 0)
		return -1;

    i = (4 - (strtab->nrbytes & 3)) & 3;
    while (i != 0) {
	if (pdp10_elf36_write_uint9(pdp10fp, '\0') < 0)
	    return -1;
	--i;
    }

    return 0;
}

int output(const char *progname, struct aunit *aunit, const char *outfile)
{
    pdp10_uint36_t shnum, text_shndx, symtab_shndx, strtab_shndx, shstrtab_shndx;
    pdp10_uint36_t text_shstrndx, symtab_shstrndx, strtab_shstrndx, shstrtab_shstrndx;
    Elf36_Sym *symtab;
    pdp10_uint36_t symnum;
    struct strtab strtab, shstrtab;
    struct aunit_symbol *asym;
    pdp10_uint36_t i;
    Elf36_Shdr *shtab;
    pdp10_uint36_t offset;
    Elf36_Ehdr ehdr;
    PDP10_FILE *pdp10fp;

    shnum = 0;
    shstrtab_shndx = 0;
    text_shndx = 0;
    symtab_shndx = 0;
    strtab_shndx = 0;
    symtab = NULL;
    symnum = 0;
    strtab_init(&strtab);
    strtab_init(&shstrtab);
    shtab = NULL;

    shnum = 1;	/* tentative */

    if (aunit->text_nr_words != 0) {
	text_shstrndx = strtab_enter(progname, &shstrtab, ".text");
	if (text_shstrndx == 0)
	    return -1;
	text_shndx = shnum;
	++shnum;
    }

    for (asym = aunit->symbols; asym; asym = asym->next)
	++symnum;
    if (symnum != 0) {
	symtab_shstrndx = strtab_enter(progname, &shstrtab, ".symtab");
	if (symtab_shstrndx == 0)
	    return -1;
	strtab_shstrndx = strtab_enter(progname, &shstrtab, ".strtab");
	if (strtab_shstrndx == 0)
	    return -1;
	symtab_shndx = shnum;
	strtab_shndx = shnum + 1;
	shnum += 2;
    }

    if (shnum == 1) {
	shstrtab_shndx = 0;
	shnum = 0;
    } else {
	shstrtab_shstrndx = strtab_enter(progname, &shstrtab, ".shstrtab");
	if (shstrtab_shstrndx == 0)
	    return -1;
	shstrtab_shndx = shnum;
	++shnum;
    }

    if (symnum) {
	++symnum;	/* for initial stub entry */
	symtab = malloc(symnum * sizeof(Elf36_Sym));
	if (!symtab) {
	    fprintf(stderr, "%s: failed to allocate %zu bytes for Elf36 symbol table: %s\n",
		    progname, symnum * sizeof(Elf36_Sym), strerror(errno));
	    return -1;
	}

	symtab[0].st_name = 0;
	symtab[0].st_value = 0;
	symtab[0].st_size = 0;
	symtab[0].st_info = ELF36_ST_INFO(STB_LOCAL, STT_NOTYPE);
	symtab[0].st_other = 0;
	symtab[0].st_shndx = SHN_UNDEF;

	for (i = 1, asym = aunit->symbols; asym; ++i, asym = asym->next) {
	    symtab[i].st_name = strtab_enter(progname, &strtab, asym->name);
	    if (symtab[i].st_name == 0)
		return -1;
	    symtab[i].st_value = asym->text_offset;
	    symtab[i].st_size = 0;
	    if (asym->is_global)
		symtab[i].st_info = ELF36_ST_INFO(STB_GLOBAL, STT_NOTYPE);
	    else
		symtab[i].st_info = ELF36_ST_INFO(STB_LOCAL, STT_NOTYPE);
	    symtab[i].st_other = STV_DEFAULT;
	    symtab[i].st_shndx = text_shndx;
	}
    }

    if (shnum) {
	shtab = malloc(shnum * sizeof(Elf36_Shdr));
	if (!shtab) {
	    fprintf(stderr, "%s: failed to allocate %zu bytes for Elf36 section header table: %s\n",
		    progname, shnum * sizeof(Elf36_Shdr), strerror(errno));
	    return -1;
	}

	shtab[0].sh_name = 0;
	shtab[0].sh_type = SHT_NULL;
	shtab[0].sh_flags = 0;
	shtab[0].sh_addr = 0;
	shtab[0].sh_offset = 0;
	shtab[0].sh_size = 0;
	shtab[0].sh_link = 0;
	shtab[0].sh_info = 0;
	shtab[0].sh_addralign = 0;
	shtab[0].sh_entsize = 0;

	offset = ELF36_EHDR_SIZEOF;

	if (text_shndx) {
	    shtab[text_shndx].sh_name = text_shstrndx;
	    shtab[text_shndx].sh_type = SHT_PROGBITS;
	    shtab[text_shndx].sh_flags = SHF_ALLOC | SHF_EXECINSTR;
	    shtab[text_shndx].sh_addr = 0;
	    shtab[text_shndx].sh_offset = offset;
	    shtab[text_shndx].sh_size = aunit->text_nr_words * 4;
	    shtab[text_shndx].sh_link = 0;
	    shtab[text_shndx].sh_info = 0;
	    shtab[text_shndx].sh_addralign = 4;
	    shtab[text_shndx].sh_entsize = 0;
	    offset += aunit->text_nr_words * 4;
	}

	if (symtab_shndx) {
	    shtab[symtab_shndx].sh_name = symtab_shstrndx;
	    shtab[symtab_shndx].sh_type = SHT_SYMTAB;
	    shtab[symtab_shndx].sh_flags = 0;
	    shtab[symtab_shndx].sh_addr = 0;
	    shtab[symtab_shndx].sh_offset = offset;
	    shtab[symtab_shndx].sh_size = symnum * ELF36_SYM_SIZEOF;
	    shtab[symtab_shndx].sh_link = strtab_shndx;
	    shtab[symtab_shndx].sh_info = 0 + 1;	/* XXX: LAST_LOCAL + 1 */
	    shtab[symtab_shndx].sh_addralign = 4;
	    shtab[symtab_shndx].sh_entsize = ELF36_SYM_SIZEOF;
	    offset += symnum * ELF36_SYM_SIZEOF;
	}

	if (strtab_shndx) {
	    shtab[strtab_shndx].sh_name = strtab_shstrndx;
	    shtab[strtab_shndx].sh_type = SHT_STRTAB;
	    shtab[strtab_shndx].sh_flags = 0;
	    shtab[strtab_shndx].sh_addr = 0;
	    shtab[strtab_shndx].sh_offset = offset;
	    shtab[strtab_shndx].sh_size = strtab.nrbytes;
	    shtab[strtab_shndx].sh_link = 0;
	    shtab[strtab_shndx].sh_info = 0;
	    shtab[strtab_shndx].sh_addralign = 1;
	    shtab[strtab_shndx].sh_entsize = 0;
	    offset += (strtab.nrbytes + 3) & ~3;
	}

	if (shstrtab_shndx) {
	    shtab[shstrtab_shndx].sh_name = shstrtab_shstrndx;
	    shtab[shstrtab_shndx].sh_type = SHT_STRTAB;
	    shtab[shstrtab_shndx].sh_flags = 0;
	    shtab[shstrtab_shndx].sh_addr = 0;
	    shtab[shstrtab_shndx].sh_offset = offset;
	    shtab[shstrtab_shndx].sh_size = shstrtab.nrbytes;
	    shtab[shstrtab_shndx].sh_link = 0;
	    shtab[shstrtab_shndx].sh_info = 0;
	    shtab[shstrtab_shndx].sh_addralign = 1;
	    shtab[shstrtab_shndx].sh_entsize = 0;
	    offset += (shstrtab.nrbytes + 3) & ~3;
	}

	/* offset is now the offset of the section header table, which is last in the file */
    } else
	offset = 0;

    ehdr.e_wident[0] = (((pdp10_uint36_t)ELFMAG0 << 28)
			| (ELFMAG1 << 20)
			| (ELFMAG2 << 12)
			| (ELFMAG3 << 4)
			| (ELFCLASS36 >> 4));
    ehdr.e_wident[1] = (((pdp10_uint36_t)(ELFCLASS36 & 0x0f) << 32)
			| (ELFDATA2MSB << 24)
			| (EV_CURRENT << 16)
			| (ELFOSABI_NONE << 8)
			| 0);	/* EI_ABIVERSION */
    ehdr.e_wident[2] = 0;
    ehdr.e_wident[3] = 0;
    ehdr.e_type = ET_REL;
    ehdr.e_machine = EM_PDP10;
    ehdr.e_version = EV_CURRENT;
    ehdr.e_entry = 0;
    ehdr.e_phoff = 0;
    ehdr.e_shoff = offset;
    ehdr.e_flags = 0;
    ehdr.e_ehsize = ELF36_EHDR_SIZEOF;
    ehdr.e_phentsize = 0;
    ehdr.e_phnum = 0;
    ehdr.e_shentsize = ELF36_SHDR_SIZEOF;
    ehdr.e_shnum = shnum;
    ehdr.e_shstrndx = shstrtab_shndx;

    pdp10fp = pdp10_fopen(outfile, "wb");
    if (!pdp10fp) {
	fprintf(stderr, "%s: failed to open %s: %s\n", progname, outfile, strerror(errno));
	return -1;
    }

    if (pdp10_elf36_write_ehdr(pdp10fp, &ehdr) < 0)
	return -1;

    if (text_shndx)
	for (i = 0; i < aunit->text_nr_words; ++i)
	    if (pdp10_elf36_write_uint36(pdp10fp, aunit->text_words[i]) < 0)
		return -1;

    if (symtab_shndx)
	for (i = 0; i < symnum; ++i)
	    if (pdp10_elf36_write_sym(pdp10fp, &symtab[i]) < 0)
		return -1;

    if (strtab_shndx)
	if (strtab_write(pdp10fp, &strtab) < 0)
	    return -1;

    if (shstrtab_shndx)
	if (strtab_write(pdp10fp, &shstrtab) < 0)
	    return -1;

    if (shnum)
	for (i = 0; i < shnum; ++i)
	    if (pdp10_elf36_write_shdr(pdp10fp, &shtab[i]) < 0)
		return -1;

    pdp10_fclose(pdp10fp);
    return 0;
}
