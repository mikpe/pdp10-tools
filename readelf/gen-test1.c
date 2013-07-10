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

int main(void)
{
    PDP10_FILE *pdp10fp;
    Elf36_Ehdr ehdr;

    pdp10fp = pdp10_fopen("test1.o", "wb");
    if (!pdp10fp) {
	fprintf(stderr, "failed to open %s: %s\n", "test1.o", strerror(errno));
	return 1;
    }

    ehdr.e_wident[0] =
	(((pdp10_uint36_t)ELFMAG0 << 28)
	 | (ELFMAG1 << 20)
	 | (ELFMAG2 << 12)
	 | (ELFMAG3 << 4)
	 | (ELFCLASS36 >> 4));
    ehdr.e_wident[1] =
	(((pdp10_uint36_t)(ELFCLASS36 & 0x0f) << 32)
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
    ehdr.e_shoff = 0;
    ehdr.e_flags = 0;
    ehdr.e_ehsize = ELF36_EHDR_SIZEOF;
    ehdr.e_phentsize = 0;
    ehdr.e_phnum = 0;
    ehdr.e_shentsize = 0;
    ehdr.e_shnum = 0;
    ehdr.e_shstrndx = SHN_UNDEF;

    if (pdp10_elf36_write_ehdr(pdp10fp, &ehdr) < 0) {
	fprintf(stderr, "writer to write ELF header: %s\n", strerror(errno));
	return 1;
    }
    pdp10_fclose(pdp10fp);
    return 0;
}
