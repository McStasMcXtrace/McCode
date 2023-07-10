//
// Created by gst on 07/07/23.
//

#ifndef MCCODE_LITERALS_R_H
#define MCCODE_LITERALS_R_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct literals_table_struct { /* stores literal strings from components */
  char * source;  // component name which provided the literal string
  char * name;  // the name of the literal
  char * type;  // the type of the literal (free form, valid identifier)
  char * value;  // the literal string contents
};
typedef struct literals_table_struct literals_table_t;
char * literals_table_key_component(char* key);
char * literals_table_key_literal(char * key);
int literals_table_defined(int, literals_table_t *, char *);
char * literals_table_type(int, literals_table_t *, char *);
char * literals_table_literal(int, literals_table_t *, char *);
void literals_table_print_all_keys(int no, literals_table_t * tab);
int literals_table_print_all_components(int no, literals_table_t * tab);
int literals_table_print_component_keys(int no, literals_table_t * tab, char * key);

#endif //MCCODE_LITERALS_R_H
