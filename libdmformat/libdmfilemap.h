#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdio.h>
#include <inttypes.h>
#include <endian.h>

int DM_file_mmap (void** addr, char** filename, int* fdout);
int DM_file_munmap (void** addr, int *fd);
uint32_t DM_test_version (void* addr);
void DM_test_version_R (void** addr, uint32_t* version);
