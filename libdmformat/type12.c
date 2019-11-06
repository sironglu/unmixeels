#include "libdm3format.h"
#include "libdm4format.h"
#include "libdmfilemap.h"

int main(int argc, char** argv) {
	if (argc != 2) {
		printf("Useage: %s DM_filename\n", argv[0]);
	} else {
		int fd;
		void* addr = DM_file_mmap (argv[1], &fd);
		int dmversion = DM_test_version(addr);
		void* next;
		if (dmversion == 4) {
			DM4_node* root = DM4_create_root(addr, &next);
			DM4_node_list *output_nodes = NULL;
			printf("Got %d nodes\n", DM4_get_nodes_by_tag_name(root, &output_nodes, "ImageSource", 11));
			DM4_idm_h a[2] = {0,0};
			DM4_node* simple_node = DM4_get_node_by_index((output_nodes[0]).node, a, 2);
			printf( "Level: %d\n",simple_node->level);
			printf("%#016x", *(uint64_t*)(simple_node->data));
			DM4_nodelist_delete(output_nodes);
			DM4_node_tree_delete(root);
		} else {
			printf("DM version error!\n");
		}
	}
	return 0;
}
