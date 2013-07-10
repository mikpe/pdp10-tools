/*
 * pdp10-elf36.c
 *
 * Procedures for reading and writing ELf36 files.
 */
#include "pdp10-elf36.h"
#include "pdp10-extint.h"
#include "pdp10-stdio.h"

int pdp10_elf36_write_uint9(PDP10_FILE *pdp10fp, pdp10_uint9_t val)
{
    return pdp10_fputc(val, pdp10fp);
}

int pdp10_elf36_read_uint9(PDP10_FILE *pdp10fp, pdp10_uint9_t *dst)
{
    int val;

    val = pdp10_fgetc(pdp10fp);
    if (val < 0)
	return -1;
    *dst = val;
    return 0;
}

int pdp10_elf36_write_uint18(PDP10_FILE *pdp10fp, pdp10_uint18_t val)
{
    struct pdp10_ext_uint18 ext18;
    unsigned int i;

    pdp10_uint18_to_ext(val, &ext18);

    for (i = 0; i < 2; ++i)
	if (pdp10_elf36_write_uint9(pdp10fp, ext18.nonet[i]) < 0)
	    return -1;

    return 0;
}

int pdp10_elf36_read_uint18(PDP10_FILE *pdp10fp, pdp10_uint18_t *dst)
{
    unsigned int i;
    struct pdp10_ext_uint18 ext18;

    for (i = 0; i < 2; ++i)
	if (pdp10_elf36_read_uint9(pdp10fp, &ext18.nonet[i]) < 0)
	    return -1;

    *dst = pdp10_uint18_from_ext(&ext18);
    return 0;
}

int pdp10_elf36_write_uint36(PDP10_FILE *pdp10fp, pdp10_uint36_t val)
{
    struct pdp10_ext_uint36 ext36;
    unsigned int i;

    pdp10_uint36_to_ext(val, &ext36);

    for (i = 0; i < 4; ++i)
	if (pdp10_elf36_write_uint9(pdp10fp, ext36.nonet[i]) < 0)
	    return -1;

    return 0;
}

int pdp10_elf36_read_uint36(PDP10_FILE *pdp10fp, pdp10_uint36_t *dst)
{
    unsigned int i;
    struct pdp10_ext_uint36 ext36;

    for (i = 0; i < 4; ++i)
	if (pdp10_elf36_read_uint9(pdp10fp, &ext36.nonet[i]) < 0)
	    return -1;

    *dst = pdp10_uint36_from_ext(&ext36);
    return 0;
}

int pdp10_elf36_write_sint36(PDP10_FILE *pdp10fp, pdp10_int36_t val)
{
    return pdp10_elf36_write_uint36(pdp10fp, val);
}

int pdp10_elf36_read_sint36(PDP10_FILE *pdp10fp, pdp10_int36_t *dst)
{
    pdp10_uint36_t tmp;

    if (pdp10_elf36_read_uint36(pdp10fp, &tmp) < 0)
	return -1;

    *dst = tmp;
    return 0;
}

int pdp10_elf36_write_ehdr(PDP10_FILE *pdp10fp, const Elf36_Ehdr *ehdr)
{
    if (pdp10_elf36_write_uint36(pdp10fp, ehdr->e_wident[0]) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, ehdr->e_wident[1]) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, ehdr->e_wident[2]) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, ehdr->e_wident[3]) < 0
	|| pdp10_elf36_write_uint18(pdp10fp, ehdr->e_type) < 0
	|| pdp10_elf36_write_uint18(pdp10fp, ehdr->e_machine) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, ehdr->e_version) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, ehdr->e_entry) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, ehdr->e_phoff) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, ehdr->e_shoff) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, ehdr->e_flags) < 0
	|| pdp10_elf36_write_uint18(pdp10fp, ehdr->e_ehsize) < 0
	|| pdp10_elf36_write_uint18(pdp10fp, ehdr->e_phentsize) < 0
	|| pdp10_elf36_write_uint18(pdp10fp, ehdr->e_phnum) < 0
	|| pdp10_elf36_write_uint18(pdp10fp, ehdr->e_shentsize) < 0
	|| pdp10_elf36_write_uint18(pdp10fp, ehdr->e_shnum) < 0
	|| pdp10_elf36_write_uint18(pdp10fp, ehdr->e_shstrndx) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_read_ehdr(PDP10_FILE *pdp10fp, Elf36_Ehdr *ehdr)
{
    if (pdp10_elf36_read_uint36(pdp10fp, &ehdr->e_wident[0]) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &ehdr->e_wident[1]) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &ehdr->e_wident[2]) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &ehdr->e_wident[3]) < 0
	|| pdp10_elf36_read_uint18(pdp10fp, &ehdr->e_type) < 0
	|| pdp10_elf36_read_uint18(pdp10fp, &ehdr->e_machine) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &ehdr->e_version) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &ehdr->e_entry) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &ehdr->e_phoff) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &ehdr->e_shoff) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &ehdr->e_flags) < 0
	|| pdp10_elf36_read_uint18(pdp10fp, &ehdr->e_ehsize) < 0
	|| pdp10_elf36_read_uint18(pdp10fp, &ehdr->e_phentsize) < 0
	|| pdp10_elf36_read_uint18(pdp10fp, &ehdr->e_phnum) < 0
	|| pdp10_elf36_read_uint18(pdp10fp, &ehdr->e_shentsize) < 0
	|| pdp10_elf36_read_uint18(pdp10fp, &ehdr->e_shnum) < 0
	|| pdp10_elf36_read_uint18(pdp10fp, &ehdr->e_shstrndx) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_write_shdr(PDP10_FILE *pdp10fp, const Elf36_Shdr *shdr)
{
    if (pdp10_elf36_write_uint36(pdp10fp, shdr->sh_name) < 0
    	|| pdp10_elf36_write_uint36(pdp10fp, shdr->sh_type) < 0
    	|| pdp10_elf36_write_uint36(pdp10fp, shdr->sh_flags) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, shdr->sh_addr) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, shdr->sh_offset) < 0
    	|| pdp10_elf36_write_uint36(pdp10fp, shdr->sh_size) < 0
    	|| pdp10_elf36_write_uint36(pdp10fp, shdr->sh_link) < 0
    	|| pdp10_elf36_write_uint36(pdp10fp, shdr->sh_info) < 0
    	|| pdp10_elf36_write_uint36(pdp10fp, shdr->sh_addralign) < 0
    	|| pdp10_elf36_write_uint36(pdp10fp, shdr->sh_entsize) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_read_shdr(PDP10_FILE *pdp10fp, Elf36_Shdr *shdr)
{
    if (pdp10_elf36_read_uint36(pdp10fp, &shdr->sh_name) < 0
    	|| pdp10_elf36_read_uint36(pdp10fp, &shdr->sh_type) < 0
    	|| pdp10_elf36_read_uint36(pdp10fp, &shdr->sh_flags) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &shdr->sh_addr) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &shdr->sh_offset) < 0
    	|| pdp10_elf36_read_uint36(pdp10fp, &shdr->sh_size) < 0
    	|| pdp10_elf36_read_uint36(pdp10fp, &shdr->sh_link) < 0
    	|| pdp10_elf36_read_uint36(pdp10fp, &shdr->sh_info) < 0
    	|| pdp10_elf36_read_uint36(pdp10fp, &shdr->sh_addralign) < 0
    	|| pdp10_elf36_read_uint36(pdp10fp, &shdr->sh_entsize) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_write_sym(PDP10_FILE *pdp10fp, const Elf36_Sym *sym)
{
    if (pdp10_elf36_write_uint36(pdp10fp, sym->st_name) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, sym->st_value) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, sym->st_size) < 0
	|| pdp10_elf36_write_uint9(pdp10fp, sym->st_info) < 0
	|| pdp10_elf36_write_uint9(pdp10fp, sym->st_other) < 0
	|| pdp10_elf36_write_uint18(pdp10fp, sym->st_shndx) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_read_sym(PDP10_FILE *pdp10fp, Elf36_Sym *sym)
{
    if (pdp10_elf36_read_uint36(pdp10fp, &sym->st_name) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &sym->st_value) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &sym->st_size) < 0
	|| pdp10_elf36_read_uint9(pdp10fp, &sym->st_info) < 0
	|| pdp10_elf36_read_uint9(pdp10fp, &sym->st_other) < 0
	|| pdp10_elf36_read_uint18(pdp10fp, &sym->st_shndx) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_write_rel(PDP10_FILE *pdp10fp, const Elf36_Rel *rel)
{
    if (pdp10_elf36_write_uint36(pdp10fp, rel->r_offset) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, rel->r_info) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_read_rel(PDP10_FILE *pdp10fp, Elf36_Rel *rel)
{
    if (pdp10_elf36_read_uint36(pdp10fp, &rel->r_offset) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &rel->r_info) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_write_rela(PDP10_FILE *pdp10fp, const Elf36_Rela *rela)
{
    if (pdp10_elf36_write_uint36(pdp10fp, rela->r_offset) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, rela->r_info) < 0
	|| pdp10_elf36_write_sint36(pdp10fp, rela->r_addend) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_read_rela(PDP10_FILE *pdp10fp, Elf36_Rela *rela)
{
    if (pdp10_elf36_read_uint36(pdp10fp, &rela->r_offset) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &rela->r_info) < 0
	|| pdp10_elf36_read_sint36(pdp10fp, &rela->r_addend) < 0)
	return -1;
    return 0;
}

/* XXX: I/O of Elf36_Note: NYI */

int pdp10_elf36_write_phdr(PDP10_FILE *pdp10fp, const Elf36_Phdr *phdr)
{
    if (pdp10_elf36_write_uint36(pdp10fp, phdr->p_type) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, phdr->p_offset) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, phdr->p_vaddr) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, phdr->p_paddr) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, phdr->p_filesz) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, phdr->p_memsz) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, phdr->p_flags) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, phdr->p_align) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_read_phdr(PDP10_FILE *pdp10fp, Elf36_Phdr *phdr)
{
    if (pdp10_elf36_read_uint36(pdp10fp, &phdr->p_type) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &phdr->p_offset) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &phdr->p_vaddr) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &phdr->p_paddr) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &phdr->p_filesz) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &phdr->p_memsz) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &phdr->p_flags) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &phdr->p_align) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_write_dyn(PDP10_FILE *pdp10fp, const Elf36_Dyn *dyn)
{
    if (pdp10_elf36_write_sint36(pdp10fp, dyn->d_tag) < 0
	|| pdp10_elf36_write_uint36(pdp10fp, dyn->d_un.d_val) < 0)
	return -1;
    return 0;
}

int pdp10_elf36_read_dyn(PDP10_FILE *pdp10fp, Elf36_Dyn *dyn)
{
    if (pdp10_elf36_read_sint36(pdp10fp, &dyn->d_tag) < 0
	|| pdp10_elf36_read_uint36(pdp10fp, &dyn->d_un.d_val) < 0)
	return -1;
    return 0;
}
