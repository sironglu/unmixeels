#ifndef LIBDM_FORMAT_H
#define LIBDM_FORMAT_H

#include <inttypes.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*for sed to test version*/
#undef DM3_
#undef DM4_
#undef DMVERSION
#define DM_
#ifdef DM3_
# define DMVERSION 3
#endif
#ifdef DM4_
# define DMVERSION 4
#endif

#ifndef DMVERSION
# error "DMVERSION not defined!"
#elif /*DMVERSION == 4 && */!defined(INT64_MAX)
# error "DMVERSION == 4 must be run on platform that supports int64_t!"
#endif

#if DMVERSION == 4
# define DM_idmbe	DM_i8be
# define DM_idm_h	int64_t
# define DM_PRId	PRId64
#elif DMVERSION == 3
# define DM_idmbe DM_i4be
# define DM_idm_h int32_t
# define DM_PRId	PRId32
#else
# error "Requested DMVERSION not supported."
#endif

typedef int8_t i1;
typedef struct{char x[2];}DM_i2be;
typedef struct{char x[4];}DM_i4be;
typedef struct{char x[8];}DM_i8be;

typedef struct{
	DM_i4be	DM_version;
	DM_idmbe	rootlen;
	DM_i4be	byteorder;
}DM_file_header;

typedef struct{
	i1	is_sorted;
	i1	is_open;
	DM_idmbe	number_of_tags;
}DM_tag_group_header;

typedef enum{
	DM_tag_entry_is_data	= 21,
	DM_tag_entry_is_group	= 20,
}DM_tag_entry_is_data_or_group;

typedef struct{
	i1	data_or_group;
	DM_i2be	tag_name_length;
}DM_tag_entry_header;
typedef struct{
	char*	tag_name;
#if DMVERSION == 4
	DM_idmbe*	tag_data_size;
#endif
}DM_tag_entry_misc;

typedef struct{
	char	percentage_sign[4];
	DM_idmbe	info_length;
}DM_tag_container_header;

typedef enum{
    DM_encoded_type_SHORT   = 2,
    DM_encoded_type_LONG    = 3,
    DM_encoded_type_USHORT  = 4,
    DM_encoded_type_ULONG   = 5,
    DM_encoded_type_FLOAT   = 6,
    DM_encoded_type_DOUBLE  = 7,
    DM_encoded_type_BOOLEAN = 8,
    DM_encoded_type_CHAR    = 9,
    DM_encoded_type_OCTET   = 10,
    DM_encoded_type_LONGLONG= 11,
    DM_encoded_type_UNKNOWN_8B=12,
    DM_encoded_type_STRUCT  = 15,
    DM_encoded_type_STRING  = 18,
    DM_encoded_type_ARRAY   = 20,
    DM_encoded_type_count   = 21
}DM_encoded_type;
typedef struct{
	DM_encoded_type type;
	int		length;
}DM_encoded_type_length;

typedef struct{
	DM_idmbe	simple_type;
}DM_tag_info_simple_header;

typedef struct{
	DM_idmbe	type_string;
	DM_idmbe	string_length;
}DM_tag_info_string_header;

typedef struct{
	DM_idmbe	type_array;
	DM_idmbe	array_type;
}DM_tag_info_array_header;
typedef	DM_idmbe	DM_array_length;

typedef struct{
	DM_idmbe	type_struct;
	DM_idmbe	struct_name_length;
	DM_idmbe	number_of_fields;
}DM_tag_info_struct_header;

typedef struct{
	DM_idmbe	field_namelength;
	DM_idmbe	field_type;
}DM_tag_info_struct_field_header;

typedef enum{
	DM_level_ROOT, 
	DM_level_TAG_GROUP, 
	DM_level_TAG_ENTRY,
	DM_level_TAG_CONTAINER,
	DM_level_TAG_INFO_SIMPLE,
	DM_level_TAG_INFO_STRING,
	DM_level_TAG_INFO_ARRAY,
	DM_level_TAG_INFO_STRUCT,
	DM_level_TAG_INFO_FIELD
}DM_level;

/*DM data structure will be represented as a tree with node defined below*/
typedef struct DM_node{
	void *header;
	void *misc; /**/
	void *data;
	DM_level level;
	DM_idm_h nchild;
	struct DM_node **child;
	struct DM_node *parent;
}DM_node;

/*a list of node will be created when reading from a data tree, see below*/
typedef struct DM_node_list{
	DM_node*		node;
	struct DM_node_list*	next;
}DM_node_list;

/*Read file and create data tree*/
DM_node* DM_create_root(DM_file_header *base, void** next);
DM_node* DM_create_tag_group(DM_tag_group_header *base, DM_node *parent, void** next);
DM_node* DM_create_tag_entry(DM_tag_entry_header *base, DM_node *parent, void** next);
DM_node* DM_create_tag_container(DM_tag_container_header *base, DM_node *parent, void** next);
DM_node* DM_create_tag_info_simple(DM_tag_info_simple_header *base, DM_node *parent,
	void* data, void** next_header, void** next_data);
int DM_get_encoded_type_length(DM_idmbe typebe);
DM_node* DM_create_tag_info_string(DM_tag_info_string_header *base, DM_node *parent,
	void* data, void** next_header, void** next_data);
DM_node* DM_create_tag_info_any(DM_idmbe* base, DM_node* parent,
	void* data, void** next_header, void** next_data);
DM_node* DM_create_tag_info_array(DM_tag_info_array_header *base, DM_node *parent,
	void* data, void** next_header, void** next_data);
DM_node* DM_create_tag_info_struct(DM_tag_info_struct_header *base, DM_node *parent,
	void* data, void** next_header, void** next_data);
DM_node* DM_create_tag_info_struct_field(DM_tag_info_struct_field_header *base, DM_node *parent,
	void* data, void** next_header, void** next_data);

/*Convert from big endian to host byte order*/
DM_idm_h DM_idmbetoh(DM_idmbe big_endian_dm_int);/*idm depends on version to be 3 or 4*/
uint16_t DM_i2betoh(DM_i2be DM_i2be_int);
uint32_t DM_i4betoh(DM_i4be DM_i4be_int);
uint64_t DM_i8betoh(DM_i8be DM_i8be_int);

/*mmap a dm file*/
//DM_file_header *DM_file_mmap (char* filename, int* fdout);
//int DM_file_munmap (void* addr, int fd);

/*get node or node list*/
DM_node* DM_get_node_by_index(DM_node* root_node, DM_idm_h* node_seq, int depth);
int DM_nodelist_push(DM_node_list** nodes, DM_node* node);
int DM_nodelist_pop(DM_node_list** nodes);
int DM_nodelist_delete(DM_node_list* nodes);
DM_idm_h DM_node_tree_delete(DM_node* parent);
DM_idm_h DM_get_child_nodes_all_recursively(DM_node* parent, DM_node_list** child_nodes);
DM_idm_h DM_get_nodes_by_level(DM_node* parent, DM_node_list** output_nodes, DM_level level);
DM_idm_h DM_get_nodes_by_tag_name(DM_node* parent, DM_node_list** output_nodes, char* name, int namelength);

/*print nodes info*/
int DM_print_node(DM_node* node);
int DM_print_nodes_all_recursively(DM_node* root, DM_level* levels_shown, int nlevel, int indentation);

#endif /* libdmformat.h */

