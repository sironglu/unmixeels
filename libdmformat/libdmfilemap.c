#include "libdmfilemap.h"

int DM_file_mmap (void** addr, char** filename, int* fdout) {
	int fd;
	fd = open(*filename, O_RDONLY);
	if (fd == -1) {
		printf("Error: open file %s.\n", *filename);
		return -1;
	} 
	*fdout = fd;
	struct stat sb;
	if (fstat(fd, &sb) == -1) {
		printf("Error: fstat of file %s.\n", *filename);
		return -1;
	}
	if (sb.st_size > 800*1024*1024) {
		printf("Note: mapping file size greater than 800MB. trying mmap()\n");
	}
	*addr = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (*addr == MAP_FAILED) {
		perror("Error: mmap");
		return -1;
	}
	return 0;
}

int DM_file_munmap (void** addr, int* fd) {
	struct stat sb;
	if (fstat(*fd, &sb) == -1) {
		printf("Error: fstat of fd %d failed.\n", *fd);
		return -1;
	}
	if ( -1 == munmap(*addr, sb.st_size)) {
		perror("Error: munmap");
	}
	if ( -1 == close(*fd) ) {
		perror("Error: close");
	}
	return 0;
}

uint32_t DM_test_version (void* addr) {
	return be32toh(*(uint32_t*)(addr));
}

void DM_test_version_R (void** addr, uint32_t* version) {
	*version = be32toh(*(uint32_t*)(*addr));
}
