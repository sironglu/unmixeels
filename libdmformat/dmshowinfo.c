#include "libdm3format.h"
#include "libdm4format.h"
#include "libdmfilemap.h"

int main(int argc, char** argv) {
	if (argc != 2) {
		printf("Useage: %s DM_filename\n", argv[0]);
	} else {
		int fd;
		void* addr;
		DM_file_mmap (&addr, &(argv[1]), &fd);
		int dmversion = DM_test_version(addr);
		void* next;
		if (dmversion == 3) {
			DM3_node* root3 = DM3_create_root(addr, &next);
			DM3_level levels_shown3[] = {
				DM3_level_ROOT, 
				DM3_level_TAG_GROUP, 
				DM3_level_TAG_ENTRY,
				DM3_level_TAG_CONTAINER,
				DM3_level_TAG_INFO_SIMPLE,
				DM3_level_TAG_INFO_STRING,
				DM3_level_TAG_INFO_ARRAY,
				DM3_level_TAG_INFO_STRUCT,
				DM3_level_TAG_INFO_FIELD
			};
			DM3_print_nodes_all_recursively(root3, levels_shown3, 9, 0);
			DM3_node_tree_delete(root3);
		} else if (dmversion == 4) {
			DM4_node* root4 = DM4_create_root(addr, &next);
			DM4_level levels_shown4[] = {
				DM4_level_ROOT, 
				DM4_level_TAG_GROUP, 
				DM4_level_TAG_ENTRY,
				DM4_level_TAG_CONTAINER,
				DM4_level_TAG_INFO_SIMPLE,
				DM4_level_TAG_INFO_STRING,
				DM4_level_TAG_INFO_ARRAY,
				DM4_level_TAG_INFO_STRUCT,
				DM4_level_TAG_INFO_FIELD
			};
			DM4_print_nodes_all_recursively(root4, levels_shown4, 9, 0);
			DM4_node_tree_delete(root4);
		} else {
			printf("Wrong DM version : %d", dmversion);
		}
		DM_file_munmap (&addr, &fd);
	}
	return 0;
}

