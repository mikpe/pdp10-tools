/*
 * gen-test1.c
 *
 * Generate test1.o for PDP10 Elf36 readelf.
 */
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include "pdp10-elf36.h"
#include "pdp10-stdio.h"

/*
 * Layout (mirroring a minimal x86 ELF32 file with empty .data and .bss omitted):
 *
 * 0:
 * - ELF Header (52 nonets, 13 words)
 * 52:
 * - Contents of .text (12 nonets, 3 words)
 * 64:
 * - Contents of .shstrtab: "", ".symtab", ".strtab", ".shstrtab", ".text"
 *   (33 nonets padded to 36 nonets, 9 words)
 * 100:
 * - Section headers:
 *   + [0] empty
 *   + [1] .text
 *   + [2] .shstrtab
 *   + [3] .symtab
 *   + [4] .strtab
 *   (200 (5 * 40) nonets, 50 words)
 * 300:
 * - Contents of .symtab: (undef), function global .text "start"
 *   (32 (2 * 16) nonets, 8 words)
 * 332:
 * - Contents of .strtab: "", "start"
 *   (7 nonets padded to 8 nonets, 2 words)
 * 340:
 * - end of file
 */

static int write_elf(PDP10_FILE *pdp10fp)
{
    enum {
	/* size of start and .text */
	TEXT_SIZE_start = 12,
	TEXT_SIZE = TEXT_SIZE_start,

	/* .shstrtab indices and size */
	SHSTRTAB_IX_symtab = 1,
	SHSTRTAB_IX_strtab = 9,
	SHSTRTAB_IX_shstrtab = 17,
	SHSTRTAB_IX_text = 27,
	SHSTRTAB_SIZE = 36,

	/* .strtab indices and size */
	STRTAB_IX_start = 1,
	STRTAB_SIZE = 8,

	/* section header indices and size */
	SHTAB_IX_text = 1,
	SHTAB_IX_shstrtab = 2,
	SHTAB_IX_strtab = 3,
	SHTAB_NR = 5,

	/* .symtab indices and size */
	SYMTAB_IX_LAST_LOCAL = 0,
	SYMTAB_NR = 2,
	SYMTAB_SIZE = SYMTAB_NR * ELF36_SYM_SIZEOF,

	/* file offsets */
	FILE_OFFSET_TEXT = ELF36_EHDR_SIZEOF,
	FILE_OFFSET_SHSTRTAB = FILE_OFFSET_TEXT + TEXT_SIZE,
	FILE_OFFSET_SHTAB = FILE_OFFSET_SHSTRTAB + SHSTRTAB_SIZE,
	FILE_OFFSET_SYMTAB = FILE_OFFSET_SHTAB + (SHTAB_NR * ELF36_SHDR_SIZEOF),
	FILE_OFFSET_STRTAB = FILE_OFFSET_SYMTAB + SYMTAB_SIZE,
    };
    static const pdp10_uint36_t text[3] = {
	/* start: */
	PDP10_UINT36_C(0201040000000),	/* MOVEI 1,0 ; exit status = 0 */
	PDP10_UINT36_C(0104000000136),	/* JSYS 0136 ; SYS_exit_group */
	PDP10_UINT36_C(0254200000000),	/* HALT */
    };
    static const pdp10_uint9_t shstrtab[36] = {
	'\0', '.', 's', 'y', 'm', 't', 'a', 'b', '\0', '.', 's', 't', 'r', 't', 'a', 'b',
	'\0', '.', 's', 'h', 's', 't', 'r', 't', 'a', 'b', '\0', '.', 't', 'e', 'x', 't',
	'\0', '\0', '\0', '\0',
    };
    static const pdp10_uint9_t strtab[8] = {
	'\0', 's', 't', 'a', 'r', 't', '\0', '\0'
    };
    static const Elf36_Shdr shtab[5] = {
        [0] = {	/* (empty) */
	    .sh_name = 0,
	    .sh_type = SHT_NULL,
	    .sh_flags = 0,
	    .sh_addr = 0,
	    .sh_offset = 0,
	    .sh_size = 0,
	    .sh_link = 0,
	    .sh_info = 0,
	    .sh_addralign = 0,
	    .sh_entsize = 0,
	},
	[1] = {	/* .text */
	    .sh_name = SHSTRTAB_IX_text,
	    .sh_type = SHT_PROGBITS,
	    .sh_flags = SHF_ALLOC | SHF_EXECINSTR,
	    .sh_addr = 0,
	    .sh_offset = FILE_OFFSET_TEXT,
	    .sh_size = TEXT_SIZE,
	    .sh_link = 0,
	    .sh_info = 0,
	    .sh_addralign = 4,
	    .sh_entsize = 0,
	},
	[2] = {	/* .shstrtab */
	    .sh_name = SHSTRTAB_IX_shstrtab,
	    .sh_type = SHT_STRTAB,
	    .sh_flags = 0,
	    .sh_addr = 0,
	    .sh_offset = FILE_OFFSET_SHSTRTAB,
	    .sh_size = SHSTRTAB_SIZE,
	    .sh_link = 0,
	    .sh_info = 0,
	    .sh_addralign = 1,
	    .sh_entsize = 0,
	},
	[3] = {	/* .strtab */
	    .sh_name = SHSTRTAB_IX_strtab,
	    .sh_type = SHT_STRTAB,
	    .sh_flags = 0,
	    .sh_addr = 0,
	    .sh_offset = FILE_OFFSET_STRTAB,
	    .sh_size = STRTAB_SIZE,
	    .sh_link = 0,
	    .sh_info = 0,
	    .sh_addralign = 1,
	    .sh_entsize = 0,
	},
	[4] = {	/* .symtab */
	    .sh_name = SHSTRTAB_IX_symtab,
	    .sh_type = SHT_SYMTAB,
	    .sh_flags = 0,
	    .sh_addr = 0,
	    .sh_offset = FILE_OFFSET_SYMTAB,
	    .sh_size = SYMTAB_SIZE,
	    .sh_link = SHTAB_IX_strtab,
	    .sh_info = SYMTAB_IX_LAST_LOCAL + 1,
	    .sh_addralign = 4,
	    .sh_entsize = ELF36_SYM_SIZEOF,
	},
    };
    static const Elf36_Sym symtab[2] = {
	[0] = {	/* (empty) */
	    .st_name = 0,
	    .st_value = 0,
	    .st_size = 0,
	    .st_info = ELF36_ST_INFO(STB_LOCAL, STT_NOTYPE),
	    .st_other = 0,
	    .st_shndx = SHN_UNDEF,
	},
	[1] = {	/* start */
	    .st_name = STRTAB_IX_start,
	    .st_value = 0,
	    .st_size = TEXT_SIZE_start,
	    .st_info = ELF36_ST_INFO(STB_GLOBAL, STT_FUNC),
	    .st_other = STV_DEFAULT,
	    .st_shndx = SHTAB_IX_text,
	},
    };
    static const Elf36_Ehdr ehdr = {
	.e_ident[EI_MAG0] = ELFMAG0,
	.e_ident[EI_MAG1] = ELFMAG1,
	.e_ident[EI_MAG2] = ELFMAG2,
	.e_ident[EI_MAG3] = ELFMAG3,
	.e_ident[EI_CLASS] = ELFCLASS36,
	.e_ident[EI_DATA] = ELFDATA2MSB,
	.e_ident[EI_VERSION] = EV_CURRENT,
	.e_ident[EI_OSABI] = ELFOSABI_NONE,
	.e_ident[EI_ABIVERSION] = 0,
	.e_ident[EI_PAD ... EI_NIDENT - 1] = 0,
	.e_type = ET_REL,
	.e_machine = EM_PDP10,
	.e_version = EV_CURRENT,
	.e_entry = 0,
	.e_phoff = 0,
	.e_shoff = FILE_OFFSET_SHTAB,
	.e_flags = 0,
	.e_ehsize = ELF36_EHDR_SIZEOF,
	.e_phentsize = 0,
	.e_phnum = 0,
	.e_shentsize = ELF36_SHDR_SIZEOF,
	.e_shnum = SHTAB_NR,
	.e_shstrndx = SHTAB_IX_shstrtab,
    };
    unsigned int i;

    if (pdp10_elf36_write_ehdr(pdp10fp, &ehdr) < 0)
	return 1;

    for (i = 0; i < 3; ++i)
	if (pdp10_elf36_write_uint36(pdp10fp, text[i]) < 0)
	    return -1;

    for (i = 0; i < 36; ++i)
	if (pdp10_elf36_write_uint9(pdp10fp, shstrtab[i]) < 0)
	    return -1;

    for (i = 0; i < 5; ++i)
	if (pdp10_elf36_write_shdr(pdp10fp, &shtab[i]) < 0)
	    return -1;

    for (i = 0; i < 2; ++i)
	if (pdp10_elf36_write_sym(pdp10fp, &symtab[i]) < 0)
	    return -1;

    for (i = 0; i < 8; ++i)
	if (pdp10_elf36_write_uint9(pdp10fp, strtab[i]) < 0)
	    return -1;

    return 0;
}

int main(void)
{
    PDP10_FILE *pdp10fp;

    pdp10fp = pdp10_fopen("test1.o", "wb");
    if (!pdp10fp) {
	fprintf(stderr, "failed to open %s: %s\n", "test1.o", strerror(errno));
	return 1;
    }

    if (write_elf(pdp10fp) < 0) {
	fprintf(stderr, "failed to write ELF data: %s\n", strerror(errno));
	return 1;
    }

    pdp10_fclose(pdp10fp);
    return 0;
}
