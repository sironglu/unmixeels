# if DMVERSION == 3
#  include "libdm3format.h"
# elif DMVERSION == 4
#  include "libdm4format.h"
# endif

#include <locale.h>
#include <iconv.h>
#include <string.h>

DM_node* DM_get_first_entry_by_name(DM_node* root, char* tag_name){
	DM_node_list* output_nodes = NULL;
	int cnt = DM_get_nodes_by_tag_name(root, &output_nodes, tag_name, strlen(tag_name));
	if (cnt == 0) {
		printf("Error: tag %s not found.\n", tag_name);
		return NULL;
	}
	if (cnt != 1) {
		printf("Warn: More than one %s tag is found. Return first one.\n", tag_name);
	}
	DM_node* output_node = output_nodes->node;
	DM_nodelist_delete(output_nodes);
	return output_node;
}


DM_node* DM_get_first_node_has_successor_with_tag_name(DM_node_list* node_list, char* tag_name) {
	DM_node_list* tmp_nodes = node_list;
	DM_node* successor_with_tag_name = NULL;
	for (;tmp_nodes!=NULL;) {
		DM_node* test_node	= tmp_nodes->node;
		tmp_nodes		= tmp_nodes->next;
		DM_node_list* successor_nodes;
		successor_nodes		= NULL;
		int hit_cnt;
		hit_cnt = DM_get_nodes_by_tag_name(test_node, &successor_nodes, tag_name, strlen(tag_name));
		DM_nodelist_delete(successor_nodes);
		if ( hit_cnt > 0 ){
			if ( ((hit_cnt > 1) && (successor_with_tag_name == NULL)) || (successor_with_tag_name != NULL) ) 
				printf("Warn: More than one node with %s found. Return first one.\n", tag_name);
			if ( successor_with_tag_name == NULL )
				successor_with_tag_name = test_node;
			else break;
		}
	}
	return successor_with_tag_name;
}

int64_t DM_get_int64_from_simple(DM_node* simple_node) {
	DM_tag_info_simple_header*	base_TAG_INFO_SIMPLE;
	base_TAG_INFO_SIMPLE = (DM_tag_info_simple_header*)(simple_node->header);
	int64_t simple_content;
	DM_node* node = simple_node;
	switch ( DM_idmbetoh(base_TAG_INFO_SIMPLE->simple_type) ){
		case DM_encoded_type_SHORT:
			simple_content = *(int16_t*)(node->data);
			break;
		case DM_encoded_type_LONG:
			simple_content = *(int32_t*)(node->data);
			break;
		case DM_encoded_type_USHORT:
			simple_content = *(uint16_t*)(node->data);
			break;
		case DM_encoded_type_ULONG:
			simple_content = *(uint32_t*)(node->data);
			break;
		case DM_encoded_type_BOOLEAN:
			simple_content = *(int8_t*)(node->data);
			break;
		case DM_encoded_type_CHAR:
			simple_content = *(int8_t*)(node->data);
			break;
		case DM_encoded_type_OCTET:
			simple_content = *(int8_t*)(node->data);
			break;
		case DM_encoded_type_LONGLONG:
			simple_content = *(int64_t*)(node->data);
			break;
		case DM_encoded_type_UNKNOWN_8B:
			simple_content = *(uint64_t*)(node->data);
			break;
		default:
			printf("Error: force type %" DM_PRId " to int64!\n", DM_idmbetoh(base_TAG_INFO_SIMPLE->simple_type));
			simple_content = *(int64_t*)(node->data);
	}
	return simple_content;
}

double DM_get_double_from_simple(DM_node* simple_node) {
	DM_tag_info_simple_header*	base_TAG_INFO_SIMPLE;
	base_TAG_INFO_SIMPLE = (DM_tag_info_simple_header*)(simple_node->header);
	double simple_content;
	DM_node* node = simple_node;
	switch ( DM_idmbetoh(base_TAG_INFO_SIMPLE->simple_type) ){
		case DM_encoded_type_FLOAT:
			simple_content = *(float*)(node->data);
			break;
		case DM_encoded_type_DOUBLE:
			simple_content = *(double*)(node->data);
			break;
		default:
			printf("Error: force type %" DM_PRId " to double!\n", DM_idmbetoh(base_TAG_INFO_SIMPLE->simple_type));
			simple_content = *(double*)(node->data);
	}
	return simple_content;
}

int DM_get_image1(DM_node** root, char** with_name, double* Origin, double* Scale, int64_t* dimensions, int* dimension_len, DM_idm_h* data_length, DM_idm_h* data_type) {
	DM_node* ImageList_node = DM_get_first_entry_by_name(*root, "ImageList");
	if (ImageList_node == NULL) {
		printf("Error: ImageList not found.\n");
		return -1;
	}
	DM_node_list* ImageTags_nodes = NULL;
	int ImageTags_cnt = DM_get_nodes_by_tag_name(ImageList_node, &ImageTags_nodes, "ImageTags", 9);
	if (ImageTags_cnt < 1) {
		printf("Error: ImageTags not found.\n");
		return -1;
	}
	DM_node* ImageTags_with_EELS = DM_get_first_node_has_successor_with_tag_name(ImageTags_nodes, *with_name);
	if (ImageTags_with_EELS == NULL) {
		printf("Error: ImageTag with EELS not found.\n");
		return -1;
	}
	DM_node* EELS_tag_group = ImageTags_with_EELS->parent;
	DM_node* ImageData_tag_entry = DM_get_first_entry_by_name(EELS_tag_group, "ImageData");
	if (ImageData_tag_entry == NULL) {
		printf("Error: ImageData not found.\n");
		return -1;
	}
	DM_node* Calibrations_tag_entry	= DM_get_first_entry_by_name(ImageData_tag_entry, "Calibrations");
	if (Calibrations_tag_entry == NULL) {
		printf("Error: Calibrations not found.\n");
		return -1;
	}
	DM_node* Dimention_tag_entry	= DM_get_first_entry_by_name(Calibrations_tag_entry, "Dimension");
	if (Dimention_tag_entry == NULL) {
		printf("Error: Dimension not found.\n");
		return -1;
	}
	*dimension_len = (Dimention_tag_entry->child[0])->nchild;
	int i;
	for (i = 0; i < (*dimension_len); i++) {
		DM_node* i_dim_entry	= (Dimention_tag_entry->child[0])->child[i];
		DM_node* i_dim_origin	= DM_get_first_entry_by_name(i_dim_entry, "Origin");
		if (i_dim_origin == NULL) {
			printf("Error: Origin not found.\n");
			return -1;
		}
		Origin[i]		= DM_get_double_from_simple(i_dim_origin->child[0]->child[0]);
		DM_node* i_dim_scale	= DM_get_first_entry_by_name(i_dim_entry, "Scale");
		if (i_dim_scale == NULL) {
			printf("Error: Scale not found.\n");
			return -1;
		}
		Scale[i]		= DM_get_double_from_simple(i_dim_scale->child[0]->child[0]);
	}
	DM_node* Dimention_s_tag_entry = DM_get_first_entry_by_name(ImageData_tag_entry, "Dimensions");
	if (Dimention_s_tag_entry == NULL) {
		printf("Error: Dimensions not found.\n");
		return -1;
	}
	*dimension_len = (Dimention_s_tag_entry->child[0])->nchild;
	for (i = 0; i < (*dimension_len); i++) {
		DM_node* i_dims_entry	= Dimention_s_tag_entry->child[0]->child[i]->child[0]->child[0];
		dimensions[i]		= DM_get_int64_from_simple(i_dims_entry);
	}
	DM_node* data_tag_entry = DM_get_first_entry_by_name(ImageData_tag_entry, "Data");
	*data_length	= DM_idmbetoh(**(DM_array_length**)(data_tag_entry->child[0]->child[0]->misc));
	*data_type	= DM_idmbetoh(((DM_tag_info_array_header*)(data_tag_entry->child[0]->child[0]->header))->array_type);
	return 0;
}

int DM_get_image2(DM_node** root, char** with_name, char** Units, int* dimension_len, void* data) {
	DM_node* ImageList_node = DM_get_first_entry_by_name(*root, "ImageList");
	if (ImageList_node == NULL) {
		printf("Error: ImageList not found.\n");
		return -1;
	}
	DM_node_list* ImageTags_nodes = NULL;
	int ImageTags_cnt = DM_get_nodes_by_tag_name(ImageList_node, &ImageTags_nodes, "ImageTags", 9);
	if (ImageTags_cnt < 1) {
		printf("Error: ImageTags not found.\n");
		return -1;
	}
	DM_node* ImageTags_with_EELS = DM_get_first_node_has_successor_with_tag_name(ImageTags_nodes, *with_name);
	DM_node* EELS_tag_group = ImageTags_with_EELS->parent;
	DM_node* ImageData_tag_entry = DM_get_first_entry_by_name(EELS_tag_group, "ImageData");
	DM_node* Dimention_tag_entry = DM_get_first_entry_by_name(DM_get_first_entry_by_name(ImageData_tag_entry, "Calibrations"), "Dimension");
	*dimension_len = (Dimention_tag_entry->child[0])->nchild;
	int i;
	for (i = 0; i < (*dimension_len); i++) {
		DM_node* i_dim_entry	= (Dimention_tag_entry->child[0])->child[i];
		DM_node* i_dim_units	= DM_get_first_entry_by_name(i_dim_entry, "Units");
		setlocale(LC_ALL, "");
		iconv_t cd; 
		cd = iconv_open ("UTF-8", "UCS-2");
		if (cd == (iconv_t) -1) {
			perror("iconv_open");
		} else {
			char* inptr	= (char*)(i_dim_units->child[0]->child[0]->data);
			DM_idm_h arraylength= DM_idmbetoh(**(DM_array_length**)(i_dim_units->child[0]->child[0]->misc));
			size_t insize	= 2*arraylength;
#define UNITS_BUFSIZE 20
			size_t avail	= UNITS_BUFSIZE;
			char* wrptr	= Units[i];
//			char* wrptr_orig= wrptr;
			size_t nconv	= iconv (cd, &inptr, &insize, &wrptr, &avail);
			if (nconv == (size_t) -1) {
				perror("iconv");
			} else {
				wrptr[nconv]	= 0;
			}
		}
	}
	DM_node* data_tag_entry = DM_get_first_entry_by_name(ImageData_tag_entry, "Data");
	int encoded_type_length	= DM_get_encoded_type_length(((DM_tag_info_array_header*)(data_tag_entry->child[0]->child[0]->header))->array_type);
	DM_idm_h data_length	= DM_idmbetoh(**(DM_array_length**)(data_tag_entry->child[0]->child[0]->misc));
//	printf("len:%d %d\n", data_length, encoded_type_length);
	memcpy(data, data_tag_entry->child[0]->child[0]->data, data_length*encoded_type_length);
	return 0;
}

int R_DM_create_root(DM_node** root, DM_file_header** addr ){
	void* next = NULL;
	*root = DM_create_root(*addr, &next);
	return 0;
}
