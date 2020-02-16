# if DMVERSION == 3
#  include "libdm3format.h"
# elif DMVERSION == 4
#  include "libdm4format.h"
# endif

#include <locale.h>
#include <iconv.h>

DM_node* DM_create_root(DM_file_header *base, void** next){
	DM_node* root = (DM_node*)malloc(sizeof(DM_node));
	*root = (DM_node){
		.header	= (void *)base,
		.misc	= NULL,
		.data	= NULL,
		.level	= DM_level_ROOT,
		.nchild	= 1,
		.parent= NULL
		};
	root->child = malloc(1*sizeof(DM_node*));
	root->child[0] = DM_create_tag_group(
		(DM_tag_group_header*)(base + 1), root, next);
	for (int i=0;i<4;i++) {
		char* eofchar = (char*)(*next+i);
		if ( (*eofchar) != 0 ) {
			printf("Error:EOF error. Read %02x\n", *eofchar);
		}
	}
	return root;
}

DM_node* DM_create_tag_group(DM_tag_group_header *base, DM_node *parent, void** next){
	DM_node* group = (DM_node*)malloc(sizeof(DM_node));
	*group = (DM_node){
		.header	= (void *)base,
		.misc	= NULL,
		.data	= NULL,
		.level	= DM_level_TAG_GROUP,
		.parent= parent
	};
	group->nchild = DM_idmbetoh(base->number_of_tags);
	group->child  = malloc( (group->nchild) * sizeof(DM_node*) );
	DM_idm_h i;
	DM_tag_entry_header *entry_base = (DM_tag_entry_header*)(base + 1);
	DM_tag_entry_header *entry_next = entry_base;
	for (i=0;i<(group->nchild);i++) {
		group->child[i] = DM_create_tag_entry(entry_base, group, (void**)(&entry_next));
		entry_base	= entry_next;
	}
	*next = entry_next;
	return group;
}

DM_node* DM_create_tag_entry(DM_tag_entry_header *base, DM_node *parent, void** next) {
	DM_node* entry = (DM_node*)malloc(sizeof(DM_node));
	*entry = (DM_node) {
		.header	= (void *)base,
		.level	= DM_level_TAG_ENTRY,
		.nchild = 1,
		.parent=parent
	};
	DM_tag_entry_misc* entry_misc	= malloc(sizeof(DM_tag_entry_misc));
	entry_misc->tag_name	= (char*)(base + 1);
#if DMVERSION == 4
	entry_misc->tag_data_size
		= (DM_idmbe*)( entry_misc->tag_name + DM_i2betoh(base->tag_name_length));
#endif
	entry->misc	= entry_misc;
/* entry->data is container or lower group instance
   Using .data because length of tag name cannot be predefined.
*/
#if DMVERSION == 3
	entry->data	= entry_misc->tag_name + DM_i2betoh(base->tag_name_length);
#elif DMVERSION == 4
	entry->data	= entry_misc->tag_name + DM_i2betoh(base->tag_name_length)
		+ sizeof(*(entry_misc->tag_data_size));
#else
# error "Requested DMVERSION not supported."
#endif
	entry->child	= malloc(1*sizeof(DM_node*));
	if (base->data_or_group == DM_tag_entry_is_data) {
		entry->child[0]	= DM_create_tag_container(entry->data, entry, next);
	} else if (base->data_or_group == DM_tag_entry_is_group) {
		entry->child[0]	= DM_create_tag_group(entry->data, entry, next);
	}
#if DMVERSION == 4
/*Verify tag_data_size */
	DM_idm_h tlen = DM_idmbetoh(*(entry_misc->tag_data_size));
	if ( tlen != *next - entry->data) {
		printf("Error: Actual tag entry size doesn't match tag_data_size field! \n");
	}
#endif
	return entry;
}

DM_node* DM_create_tag_container(DM_tag_container_header *base, DM_node *parent, void** next) {
	DM_node* container= (DM_node*)malloc(sizeof(DM_node));
	*container	= (DM_node) {
		.header	= (void*)base,
		.misc	= NULL,
		.data	= NULL,
		.level	= DM_level_TAG_CONTAINER,
		.nchild	= 1,
		.parent	= parent
	};
	/*verify %%%%*/
	if (strncmp(base->percentage_sign, "%%%%", 4)) {
		printf("Error: Expect %%%% but read %.8s\n", base->percentage_sign);
	}
	container->child = malloc(1*sizeof(DM_node*));
	DM_idm_h ninfo		= DM_idmbetoh(base->info_length);
	DM_idmbe* any_header	= (DM_idmbe*)(base + 1);
	void* any_data		= (DM_idmbe*)any_header + ninfo;
	void* next_header; void* next_data;
	container->child[0]	= DM_create_tag_info_any(any_header, container, 
		any_data, &next_header, &next_data);
	*next = next_data;
	if ( any_data != next_header ) {
		printf("Error: info_length lengths differ. Read: %" DM_PRId ". Actual: %" PRIdPTR ".\n",
			ninfo, ((DM_idmbe*)next_header - any_header));
	}
	return container;
}

DM_node* DM_create_tag_info_simple(DM_tag_info_simple_header *base, DM_node *parent, void* data, void** next_header, void** next_data){
	DM_node* simple = (DM_node*)malloc(sizeof(DM_node));
	*simple	= (DM_node) {
		.header	= (void*)base,
		.misc	= NULL,
		.data	= data,
		.level	= DM_level_TAG_INFO_SIMPLE,
		.nchild = 0,
		.child	= NULL,
		.parent = parent
	};
	*next_header	= base + 1;
	*next_data	= (char*)data + DM_get_encoded_type_length(base->simple_type);
	return simple;
}

int DM_get_encoded_type_length(DM_idmbe typebe){
	DM_encoded_type	typeh = DM_idmbetoh(typebe);
	/*Table is in host endian (usually le)*/
	DM_encoded_type_length DM_encoded_type_length_table[DM_encoded_type_count];
//	static int initialized = 0;
//	if (!initialized) {
		bzero(DM_encoded_type_length_table, sizeof(DM_encoded_type_length_table));
		DM_encoded_type_length_table[2]
			= (DM_encoded_type_length){DM_encoded_type_SHORT, 2};
		DM_encoded_type_length_table[3]
			= (DM_encoded_type_length){DM_encoded_type_LONG, 4};
		DM_encoded_type_length_table[4]
			= (DM_encoded_type_length){DM_encoded_type_USHORT, 2};
		DM_encoded_type_length_table[5]
			= (DM_encoded_type_length){DM_encoded_type_ULONG, 4};
		DM_encoded_type_length_table[6]
			= (DM_encoded_type_length){DM_encoded_type_FLOAT, 4};
		DM_encoded_type_length_table[7]
			= (DM_encoded_type_length){DM_encoded_type_DOUBLE, 8};
		DM_encoded_type_length_table[8]
			= (DM_encoded_type_length){DM_encoded_type_BOOLEAN, 1};
		DM_encoded_type_length_table[9]
			= (DM_encoded_type_length){DM_encoded_type_CHAR, 1};
		DM_encoded_type_length_table[10]
			= (DM_encoded_type_length){DM_encoded_type_OCTET, 1};
		DM_encoded_type_length_table[11]
			= (DM_encoded_type_length){DM_encoded_type_LONGLONG, 8};
		DM_encoded_type_length_table[12]
			= (DM_encoded_type_length){DM_encoded_type_UNKNOWN_8B, 8};
//		initialized = 1;
//	}
	/*We don't have to lookup table if the key is the same as the index*/
	if (DM_encoded_type_length_table[typeh].type == typeh) {
		return DM_encoded_type_length_table[typeh].length;
	} else {
		DM_idm_h i;
		for (i=0;i<DM_encoded_type_count;i++) {
			if (DM_encoded_type_length_table[i].type == typeh) {
				return DM_encoded_type_length_table[i].length;
			}
		}
	}
	return -1;
}

DM_node* DM_create_tag_info_string(DM_tag_info_string_header *base, DM_node *parent, void* data, void** next_header, void** next_data) {
	DM_node* string = (DM_node*)malloc(sizeof(DM_node));
	*string	= (DM_node) {
		.header	= (void*)base,
		.misc	= NULL,
		.data	= data,
		.level	= DM_level_TAG_INFO_STRING,
		.nchild = 0,
		.child	= NULL,
		.parent = parent
	};
	*next_header	= base + 1;
	*next_data	= (char*)data + 2 * DM_idmbetoh(base->string_length);
	return string;
}

DM_node* DM_create_tag_info_any(DM_idmbe* base, DM_node* parent, void* data, void** next_header, void** next_data) {
	DM_node* info;
	DM_encoded_type	typeh = DM_idmbetoh(*base);
	if (typeh == DM_encoded_type_ARRAY) {
		info = DM_create_tag_info_array((DM_tag_info_array_header*)base,
			parent, data, next_header, next_data);
	} else if (typeh == DM_encoded_type_STRUCT) {
		info = DM_create_tag_info_struct((DM_tag_info_struct_header*)base,
			parent, data, next_header, next_data);
	} else if (typeh == DM_encoded_type_STRING) {
		info = DM_create_tag_info_string((DM_tag_info_string_header*)base,
			parent, data, next_header, next_data);
	} else if ( typeh >= DM_encoded_type_SHORT && typeh <= /*DM_encoded_type_LONGLONG*/DM_encoded_type_UNKNOWN_8B ) {
		/*simple type*/
		info = DM_create_tag_info_simple((DM_tag_info_simple_header*)base,
			parent, data, next_header, next_data);
	} else {
		printf("Error: Unknown encoded type %d\n", typeh);
		info = NULL;
	}
	return info;
}

DM_node* DM_create_tag_info_array(DM_tag_info_array_header *base, DM_node *parent, void* data, void** next_header, void** next_data) {
	DM_node* array = (DM_node*)malloc(sizeof(DM_node));
	*array	= (DM_node) {
		.header	= (void*)base,
		.data	= data,
		.level	= DM_level_TAG_INFO_ARRAY,
		.parent = parent
	};
	DM_encoded_type	typeh	= DM_idmbetoh(base->array_type);
	DM_array_length* misc_arraylength;
	if ( typeh >= DM_encoded_type_SHORT && typeh <= DM_encoded_type_LONGLONG ) {
		/*For simple type we dont call DM_create_tag_info_any in case too many*/
		misc_arraylength = (DM_array_length*)(base + 1);
		*next_data = data
			+ DM_get_encoded_type_length(base->array_type) * 
			DM_idmbetoh(*misc_arraylength);
	} else {
		if (typeh != DM_encoded_type_STRUCT) /*complex array*/ {
			printf("Warning: Unsupporetd array of type %d, still trying...\n", typeh);
		}
		DM_idm_h i = 0;
		void* next_element_data = data;
		array->nchild		= 1;
		array->child		= malloc(1*sizeof(DM_node*));
		do {
			array->child[i]	= DM_create_tag_info_any(&(base->array_type),
				array, next_element_data, (void**)(&misc_arraylength), &next_element_data);
			if ( i == 0 ) {
				array->nchild	= DM_idmbetoh(*misc_arraylength);
				array->child	= realloc(array->child,
					(array->nchild)*sizeof(DM_node*));
				if (array->nchild < 1) {
					printf(
						"Note: complex array of length is %" DM_PRId "!\n",
						array->nchild);
					free(array->child);
				}
			}
			i++;
		} while (i < (array->nchild) );
		*next_data	= next_element_data;
	}
	array->misc	= malloc(sizeof(DM_array_length*));
	*(DM_array_length**)(array->misc)	= misc_arraylength;
	*next_header	= (char*)misc_arraylength + sizeof(DM_array_length);
	return array;
}

DM_node* DM_create_tag_info_struct(DM_tag_info_struct_header *base, DM_node *parent, void* data, void** next_header, void** next_data) {
	DM_node* structu = (DM_node*)malloc(sizeof(DM_node));
	*structu = (DM_node) {
		.header	= (void*)base,
//		.misc	= NULL,
		.data	= data,
		.level	= DM_level_TAG_INFO_STRUCT,
		.parent	= parent
	};
	DM_idm_h struct_name_length = DM_idmbetoh(base->struct_name_length);
	if ( struct_name_length != 0 ) {
		printf("Note: struct_name_length is not zero!\n");
		structu->misc			= malloc(sizeof(char**));
		*(char**)(structu->misc)	= data;
	} else {
		structu->misc = NULL;
	}
	structu->nchild	= DM_idmbetoh(base->number_of_fields);
	structu->child	= malloc( (structu->nchild) * sizeof(DM_node*) );
	DM_idm_h i;
	DM_tag_info_struct_field_header* field_base_header
		=(DM_tag_info_struct_field_header *)(base + 1);
	void* field_base_data = (char*)data + struct_name_length;
	for (i=0;i<(structu->nchild);i++) {
		structu->child[i] = DM_create_tag_info_struct_field(field_base_header,
			structu, field_base_data, (void**)(&field_base_header), &field_base_data);
	}
	*next_header	= field_base_header;
	*next_data	= field_base_data;
	return structu;
}

DM_node* DM_create_tag_info_struct_field(DM_tag_info_struct_field_header *base, DM_node *parent, void* data, void** next_header, void** next_data) {
	DM_node* field = (DM_node*)malloc(sizeof(DM_node));
	*field = (DM_node) {
		.header	= (void*)base,
//		.misc	= NULL,
		.data	= data,
		.level	= DM_level_TAG_INFO_FIELD,
		.nchild = 1,
		.parent = parent
	};
	DM_idm_h field_namelength = DM_idmbetoh(base->field_namelength);
	if ( field_namelength != 0 ) {
		printf("Note: field_namelength is not zero!\n");
		field->misc		= malloc(sizeof(char**));
		*(char**)(field->misc)	= data;
	} else {
		field->misc = NULL;
	}
	field->child	= malloc(1*sizeof(DM_node*));
	field->child[0]	= DM_create_tag_info_simple(
		(DM_tag_info_simple_header*)(&(base->field_type)), 
		field, (char*)data + field_namelength, next_header, next_data);
	/*next_header has already been set above. Just do it again.*/
	*next_header	= base + 1;
	return field;
}

DM_idm_h DM_idmbetoh(DM_idmbe big_endian_dm_int) {
#if DMVERSION == 4
	return be64toh(*(DM_idm_h*)(&big_endian_dm_int));
#elif DMVERSION == 3
	return be32toh(*(DM_idm_h*)(&big_endian_dm_int));
#else
# error "Requested DMVERSION not supported."
#endif
}

uint16_t DM_i2betoh(DM_i2be DM_i2be_int) {
	return be16toh(*(uint16_t*)(&DM_i2be_int));
}


uint32_t DM_i4betoh(DM_i4be DM_i4be_int) {
	return be32toh(*(uint32_t*)(&DM_i4be_int));
}

uint64_t DM_i8betoh(DM_i8be DM_i8be_int) {
	return be64toh(*(uint64_t*)(&DM_i8be_int));
}

DM_node* DM_get_node_by_index(DM_node* root_node, DM_idm_h* node_seq, int depth) {
	DM_idm_h i;
	DM_node* res_node = root_node;
	for (i=0;i<depth;i++) {
		if (node_seq[i] >= res_node->nchild) {
			printf("Error: Want child no.%" DM_PRId ". However, nchild=%" DM_PRId "\n" ,
				node_seq[i], res_node->nchild);
			return NULL;
		}
		res_node = res_node->child[node_seq[i]];
	}
	return res_node;
}

/* *nodes needs to be set to NULL when initialization. */
int DM_nodelist_push(DM_node_list** nodes, DM_node* node) {
	DM_node_list* tmp	= *nodes;
	*nodes			= malloc(sizeof(DM_node_list));
	(*nodes)->next		= tmp;
	(*nodes)->node		= node;
	return 0;
}

int DM_nodelist_pop(DM_node_list** nodes) {
	DM_node_list* tmp	= (*nodes)->next;
	free(*nodes);
	*nodes			= tmp;
	return 0;
}

int DM_nodelist_delete(DM_node_list* nodes){
	while (nodes != NULL)
		DM_nodelist_pop(&nodes);
	return 0;
}

DM_idm_h DM_node_tree_delete(DM_node* parent) {
	DM_node tmp = *parent;
	free(parent->misc);
	free(parent);
	if ((tmp.nchild) != 0) {
		for (int i=0;i<(tmp.nchild);i++){
			DM_node_tree_delete(tmp.child[i]);
		}
	}
	return 0;
}

DM_idm_h DM_get_child_nodes_all_recursively(DM_node* parent, DM_node_list** output_nodes){
	DM_idm_h count	= 1;
	DM_nodelist_push(output_nodes, parent);
	if ((parent->nchild) != 0) {
		for (int i=0;i<(parent->nchild);i++){
			count += DM_get_child_nodes_all_recursively(
				parent->child[i], output_nodes);
		}
	}
	return count;
}

DM_idm_h DM_get_nodes_by_level(DM_node* parent, DM_node_list** output_nodes, DM_level level){
	DM_idm_h count	= 1;
	if ( parent->level == level ) {
		DM_nodelist_push(output_nodes, parent);
	}
	if ((parent->nchild) != 0) {
		for (int i=0;i<(parent->nchild);i++){
			count += DM_get_child_nodes_all_recursively(
				parent->child[i], output_nodes);
		}
	}
	return count;
}

DM_idm_h DM_get_nodes_by_tag_name(DM_node* parent, DM_node_list** output_nodes, char* name, int namelength){
	DM_idm_h count	= 0;
	if ( parent->level == DM_level_TAG_ENTRY) {
		char *tag_name		= ((DM_tag_entry_misc*)(parent->misc))->tag_name;
		uint16_t tag_name_length
			= DM_i2betoh(((DM_tag_entry_header*)(parent->header))->tag_name_length);
		if ( namelength == tag_name_length && 0 == (strncmp(name, tag_name, namelength)) ){
			DM_nodelist_push(output_nodes, parent);
			count = 1;
		}		
	}
	/*recursion*/
	if ((parent->nchild) != 0) {
		for (int i=0;i<(parent->nchild);i++){
			count += DM_get_nodes_by_tag_name(
				parent->child[i], output_nodes, name, namelength);
		}
	}
	return count;
}



int DM_print_node(DM_node* node) {
//	void* base;
	DM_file_header*			base_ROOT;
	DM_tag_group_header*		base_TAG_GROUP;
	DM_tag_entry_header*		base_TAG_ENTRY;
	DM_tag_container_header*	base_TAG_CONTAINER;
	DM_tag_info_simple_header*	base_TAG_INFO_SIMPLE;
	DM_tag_info_string_header*	base_TAG_INFO_STRING;
	DM_tag_info_array_header*	base_TAG_INFO_ARRAY;
	DM_tag_info_struct_header*	base_TAG_INFO_STRUCT;
	DM_tag_info_struct_field_header* base_TAG_INFO_FIELD;
	switch (node->level){
		case DM_level_ROOT :
			base_ROOT = (DM_file_header*)(node->header);
			printf("ROOT ver=%d,rootlen=%" DM_PRId ",byteorder=%d(%s)\n",
				DM_i4betoh(base_ROOT->DM_version),
				DM_idmbetoh(base_ROOT->rootlen),
				DM_i4betoh(base_ROOT->byteorder),
				DM_i4betoh((base_ROOT->byteorder))?("LE"):("BE"));
			break;
		case DM_level_TAG_GROUP:
			base_TAG_GROUP = (DM_tag_group_header*)(node->header);
			printf("TAG_GROUP IsSorted=%d,IsOpen=%d,#OfTags=%" DM_PRId "\n",
				base_TAG_GROUP->is_sorted,
				base_TAG_GROUP->is_open,
				DM_idmbetoh(base_TAG_GROUP->number_of_tags));
			break;
		case DM_level_TAG_ENTRY:
			base_TAG_ENTRY = (DM_tag_entry_header*)(node->header);
			uint16_t tag_name_length_print	= DM_i2betoh(base_TAG_ENTRY->tag_name_length);
			char* tag_name_print		= malloc(1 + sizeof(char) * tag_name_length_print);
			strncpy(tag_name_print, ((DM_tag_entry_misc*)(node->misc))->tag_name, tag_name_length_print);
			tag_name_print[tag_name_length_print]= 0;
#if DMVERSION == 4
			printf("TAG_ENTRY DataOrGroup=%d(%s),TagNameLength=%d,TagName=%s,TagDataSize=%" PRId64 "\n",
#elif DMVERSION == 3
			printf("TAG_ENTRY DataOrGroup=%d(%s),TagNameLength=%d,TagName=%s\n",
#endif
				base_TAG_ENTRY->data_or_group, 
				( base_TAG_ENTRY->data_or_group == DM_tag_entry_is_data
					|| base_TAG_ENTRY->data_or_group == DM_tag_entry_is_group )
					?((base_TAG_ENTRY->data_or_group == DM_tag_entry_is_data)?"data":"group"):"error", 
				tag_name_length_print,
				tag_name_print
#if DMVERSION == 4
				, DM_idmbetoh((*((DM_tag_entry_misc*)(node->misc))->tag_data_size))
#endif
				);
			break;
		case DM_level_TAG_CONTAINER:
			base_TAG_CONTAINER = (DM_tag_container_header*)(node->header);
			printf("TAG_CONTAINER ninfo:%" DM_PRId "\n", 
				DM_idmbetoh(base_TAG_CONTAINER->info_length));
			break;
		case DM_level_TAG_INFO_SIMPLE:
			base_TAG_INFO_SIMPLE = (DM_tag_info_simple_header*)(node->header);
			char* simple_type;
			#define SIMPLE_MAX_LENGTH 48
			char simple_content[SIMPLE_MAX_LENGTH];
			switch ( DM_idmbetoh(base_TAG_INFO_SIMPLE->simple_type) ){
				case DM_encoded_type_SHORT:
					simple_type = "SHORT";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "%" PRId16, *(int16_t*)(node->data));
					break;
				case DM_encoded_type_LONG:
					simple_type = "LONG";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "%" PRId32, *(int32_t*)(node->data));
					break;
				case DM_encoded_type_USHORT:
					simple_type = "USHORT";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "%" PRIu16, *(uint16_t*)(node->data));
					break;
				case DM_encoded_type_ULONG:
					simple_type = "ULONG";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "%" PRIu32, *(uint32_t*)(node->data));
					break;
				case DM_encoded_type_FLOAT:
					simple_type = "FLOAT";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "%e", *(float*)(node->data));
					break;
				case DM_encoded_type_DOUBLE:
					simple_type = "DOUBLE";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "%le", *(double*)(node->data));
					break;
				case DM_encoded_type_BOOLEAN:
					simple_type = "BOOLEAN";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "%" PRId8, *(int8_t*)(node->data));
					break;
				case DM_encoded_type_CHAR:
					simple_type = "CHAR";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "%c, %" PRId8, *(char*)(node->data), *(int8_t*)(node->data) );
					break;
				case DM_encoded_type_OCTET:
					simple_type = "OCTET";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "%" PRId8, *(int8_t*)(node->data));
					break;
				case DM_encoded_type_LONGLONG:
					simple_type = "LONGLONG";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "%" PRId64, *(int64_t*)(node->data));
					break;
				case DM_encoded_type_UNKNOWN_8B:
					simple_type = "UNKNOWN_8B";
					snprintf(simple_content, SIMPLE_MAX_LENGTH, "0x%" PRIx64, *(uint64_t*)(node->data));
					break;
				default:
					simple_type = "WRONG";
			}
			printf("TAG_INFO_SIMPLE type=%" DM_PRId "(%s)", 
				DM_idmbetoh(base_TAG_INFO_SIMPLE->simple_type), simple_type);
			printf(",content=%s\n", simple_content);
			break;
		case DM_level_TAG_INFO_STRING:
			base_TAG_INFO_STRING = (DM_tag_info_string_header*)(node->header);
			printf("TAG_INFO_STRING type=%" DM_PRId ",length=%" DM_PRId "\n",
				DM_idmbetoh(base_TAG_INFO_STRING->type_string),
				DM_idmbetoh(base_TAG_INFO_STRING->string_length));
			break;
		case DM_level_TAG_INFO_ARRAY:
			base_TAG_INFO_ARRAY = (DM_tag_info_array_header*)(node->header);
			DM_idm_h arraylength= DM_idmbetoh(**(DM_array_length**)(node->misc));
			printf("TAG_INFO_ARRAY typeArray=%" DM_PRId ",arrayType=%" DM_PRId ",arrayLength=%" DM_PRId ,
				DM_idmbetoh(base_TAG_INFO_ARRAY->type_array),
				DM_idmbetoh(base_TAG_INFO_ARRAY->array_type),
				arraylength);
			if (arraylength && DM_idmbetoh(base_TAG_INFO_ARRAY->array_type) == DM_encoded_type_USHORT) {
				setlocale(LC_ALL, "");
				iconv_t cd; 
				cd = iconv_open ("", "UCS-2");
				if (cd == (iconv_t) -1) {
					perror("iconv_open");
				} else {
					char* inptr	= (char*)(node->data);
					size_t insize	= 2*arraylength;
					size_t avail	= insize*3;
					char* wrptr	= malloc(avail);
					char* wrptr_orig= wrptr;
					size_t nconv	= iconv (cd, &inptr, &insize, &wrptr, &avail);
					if (nconv == (size_t) -1) {
						perror("iconv");
					} else {
						wrptr[nconv]	= 0;
						printf(",content=%s", wrptr_orig);
					}
					free(wrptr_orig);
				}
			}
			printf("\n");
			break;
		case DM_level_TAG_INFO_STRUCT:
			base_TAG_INFO_STRUCT = (DM_tag_info_struct_header*)(node->header);
			printf("TAG_INFO_STRUCT typeStruct=%" DM_PRId ",structNamelength=%" DM_PRId ",#OfFields=%" DM_PRId "\n",
				DM_idmbetoh(base_TAG_INFO_STRUCT->type_struct),
				DM_idmbetoh(base_TAG_INFO_STRUCT->struct_name_length),
				DM_idmbetoh(base_TAG_INFO_STRUCT->number_of_fields));
			break;
		case DM_level_TAG_INFO_FIELD:
			base_TAG_INFO_FIELD = (DM_tag_info_struct_field_header*)(node->header);
			printf("TAG_INFO_FIELD FieldNameLength=%" DM_PRId ",FieldType=%" DM_PRId "\n",
				DM_idmbetoh(base_TAG_INFO_FIELD->field_namelength),
				DM_idmbetoh(base_TAG_INFO_FIELD->field_type));
			break;
		default:
			printf("Error: Unknown level %d\n", node->level);
	}
	return 0;
}

int DM_print_nodes_all_recursively(DM_node* root, DM_level* levels_shown, int nlevel, int indentation){
	int i,j;
	DM_level level = root->level;
	for (i=0;i<nlevel;i++) {
		if (levels_shown[i] == level) {
//			printf("%2d ", indentation);
			printf("%2d 0x%016" PRIxPTR " 0x%016" PRIxPTR " ", indentation, (uintptr_t)(root->header), (uintptr_t)(root->data));
			for (j=0;j<indentation;j++) printf(" ");
			DM_print_node(root);
		}
	}
	if ((root->nchild) != 0) {
		for (int i=0;i<(root->nchild);i++){
			DM_print_nodes_all_recursively(root->child[i],
				levels_shown, nlevel, indentation+1);
		}
	}
	return 0;
}


