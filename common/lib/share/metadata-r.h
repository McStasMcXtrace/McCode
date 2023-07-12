//
// Created by gst on 07/07/23.
//

#ifndef MCCODE_metadata_R_H
#define MCCODE_metadata_R_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct metadata_table_struct { /* stores literal strings from components */
  char * source;  // component name which provided the literal string
  char * name;  // the name of the literal
  char * type;  // the type of the literal (free form, valid identifier)
  char * value;  // the literal string contents
};
typedef struct metadata_table_struct metadata_table_t;
char * metadata_table_key_component(char* key);
char * metadata_table_key_literal(char * key);
int metadata_table_defined(int, metadata_table_t *, char *);
char * metadata_table_type(int, metadata_table_t *, char *);
char * metadata_table_literal(int, metadata_table_t *, char *);
void metadata_table_print_all_keys(int no, metadata_table_t * tab);
int metadata_table_print_all_components(int no, metadata_table_t * tab);
int metadata_table_print_component_keys(int no, metadata_table_t * tab, char * key);

#endif //MCCODE_metadata_R_H
