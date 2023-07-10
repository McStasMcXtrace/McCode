/** --- Contents of  literals-r.c ---------------------------------------------------------------------------------- */
// Created by Gregory Tucker, Data Management Software Centre, European Spallation Source ERIC on 07/07/23.
#ifndef MCCODE_NAME
#include "literals-r.h"
#endif

char * literals_table_key_component(char* key){
  if (strlen(key) == 0) return NULL;
  char sep[2] = ":\0"; // matches any number of repeated colons
  // look for the separator in the provided key; strtok is allowed to modify the string, so copy it
  char * tokkey = strdup(key);
  char * pch = strtok(tokkey, sep); // this *is* the component name (if provided) -- but we need to move the pointer
  char * comp = malloc((1 + strlen(pch)) * sizeof(char));
  strcpy(comp, pch);
  if (tokkey) free(tokkey);
  return comp;
}
char * literals_table_key_literal(char * key){
  if (strlen(key) == 0) return NULL;
  char sep[3] = ":\0";
  char * tokkey = strdup(key);
  char * pch = strtok(tokkey, sep); // this *is* the component name (if provided)
  if (pch) pch = strtok(NULL, sep); // either NULL or the literal name
  char * name = NULL;
  if (pch) {
    name = malloc((1 + strlen(pch)) * sizeof(char));
    strcpy(name, pch);
  }
  if (tokkey) free(tokkey);
  return name;
}
int literals_table_defined(int no, literals_table_t * tab, char * key){
  if (strlen(key) == 0){
    return no;
  }
  char * comp = literals_table_key_component(key);
  char * name = literals_table_key_literal(key);
  // look through the table for the matching component and literal names
  int number = 0;
  for (int i=0; i<no; ++i){
    if (!strcmp(comp, tab[i].source)){
      if (name == NULL || !strcmp(name, tab[i].name)) ++number;
    }
  }
  if (comp) free(comp);
  if (name) free(name);
  return number;
}
char * literals_table_type(int no, literals_table_t * tab, char * key){
  if (strlen(key) == 0) {
    fprintf(stderr, "Unable to check type of non-existent key\n");
    exit(1);
  }
  char * comp = literals_table_key_component(key);
  char * name = literals_table_key_literal(key);
  if (name == NULL){
    fprintf(stderr, "Unable to check type of literal for component %s without its name\n", comp);
    free(comp);
    exit(1);
  }
  char * type = NULL;
  for (int i=0; i<no; ++i){
    if (!strcmp(comp, tab[i].source) && !strcmp(name, tab[i].name)) type = tab[i].type;
  }
  if (comp) free(comp);
  if (name) free(name);
  return type;
}

char * literals_table_literal(int no, literals_table_t * tab, char * key){
  if (strlen(key) == 0) {
    fprintf(stderr, "Unable to retrieve literal for non-existent key\n");
    exit(1);
  }
  char * comp = literals_table_key_component(key);
  char * name = literals_table_key_literal(key);
  if (name == NULL){
    fprintf(stderr, "Unable to retrieve literal for component %s without its name\n", comp);
    free(comp);
    exit(1);
  }
  char * type = NULL;
  for (int i=0; i<no; ++i){
    if (!strcmp(comp, tab[i].source) && !strcmp(name, tab[i].name)) type = tab[i].value;
  }
  if (comp) free(comp);
  if (name) free(name);
  return type;
}
void literals_table_print_all_keys(int no, literals_table_t * tab){
  for (int i=0; i<no; ++i){
    printf("%s::%s ", tab[i].source, tab[i].name);
  }
  printf("\n");
}
int literals_table_print_all_components(int no, literals_table_t * tab){
  int count = 0;
  char ** used = malloc(no + sizeof(char*));
  for (int i=0; i<no; ++i){
    int unused = 1;
    for (int j=0; j<count; ++j){
      if (!strcmp(tab[i].source, used[j])) {
        unused = 0;
        break;
      }
    }
    if (unused) {
      printf("%s ", tab[i].source);
      used[count++] = tab[i].source;
    }
  }
  printf("\n");
  if (used) free(used);
  return count;
}
int literals_table_print_component_keys(int no, literals_table_t * tab, char * key){
  char * comp = literals_table_key_component(key);
  char * name = literals_table_key_literal(key);
  int count = 0;
  for (int i=0; i<no; ++i) if (!strcmp(tab[i].source, comp) && (name == NULL || !strcmp(tab[i].name, name))) {
    if (name == NULL) printf("%s ", tab[i].name);
    ++count;
  }
  if (name != NULL) printf("%d", count); // replace count by strlen(tab[i].value)?
  printf("\n");
  return count;
}
/* -------------------------------------------------------------------------------------Contents of  literals-r.c --- */