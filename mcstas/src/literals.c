//
// Created by gst on 07/07/23.
//
#include <string.h>
#include "mccode.h"


/* post-parsing, combine the list(s) of literal strings ********************* */
/** @brief Assign component literals to the instrument literals list
 *
 * @param inst the instrument_definition
 * @return 0 if successful, non-zero if not
 */
//int literals_construct_table(instr_ptr_t inst){
//  printf("Start literals_construct_table\n");
//  Symtab_handle comp_iter;
//  struct Symtab_entry * comp_entry;
//  Symtab_handle lit_iter;
//  struct Symtab_entry * lit_entry;
//  struct comp_inst * comp_ptr;
//  struct literal_struct * l_ptr;
//  char * key;
//  char sep[] = "::\0";
//  Symtab lit_map = symtab_create();
//
//  if (inst->compmap == NULL) return 1;
//  if (inst->literals == NULL || list_undef(inst->literals)) {
//    fprintf(stderr, "No literals to check because its null!\n");
//    return 1;
//  }
//
//  // iterate through all components
//  comp_iter = symtab_iterate(inst->compmap);
//  while ((comp_entry = symtab_next(comp_iter))) {
//    // iterate through all literal entries in the component in order
//    if (comp_entry->val == NULL){
//      fprintf(stderr, "Component table entry %s has NULL val?!\n", comp_entry->name);
//      return 1;
//    }
//    comp_ptr = (struct comp_inst*) comp_entry->val;
//    if (list_undef(comp_ptr->literals)) {
//      printf("How did component %s get here without literals?\n", comp_ptr->name);
//      continue;
//    }
//    printf("Component %s has %d literals\n", comp_ptr->name, list_len(comp_ptr->literals));
//
//    for (int i=0; i<list_len(comp_ptr->literals); ++i){
//      // get a pointer to the next literal structure
//      l_ptr = (struct literal_struct*) list_access(comp_ptr->literals, i);
//      // assign the component name, since we didn't bother doing so earlier
//      l_ptr->source = str_dup(comp_ptr->name);
//      // construct the key for insertion into the symbol table
//      key = str_cat(comp_ptr->name, sep, l_ptr->name);
//      // check if the same-keyed literal was already provided
//      if ((lit_entry = symtab_lookup(lit_map, key))){
//        // This key already exists! What do we do? Trust users to know what they're doing!
//        // Component-definition literals came before Component-declaration literals -- so overwrite!
//        lit_entry->val = (void *) l_ptr;
//      } else {
//        if (l_ptr->source == NULL) printf("literal source undefined?!\n");
//        if (l_ptr->name == NULL) printf("literal name undefined?!\n");
//        if (l_ptr->type == NULL) printf("literal type undefined?!\n");
//        symtab_add(lit_map, key, (void *) l_ptr);
//      }
//      // str_cat allocated new memory, and symtab_add only copied the key; so free it now
//      if (key) memfree(key);
//    }
//  }
//  symtab_iterate_end(comp_iter);
//
//  // Literals table construction complete, now copy it to the instrument literals list:
//  inst->literals = list_create();
//  lit_iter = symtab_iterate(lit_map);
//  while ((lit_entry = symtab_next(lit_iter))){
//    list_add(inst->literals, lit_entry->val);
//  }
//  symtab_iterate_end(lit_iter);
//
//  // Free the Symtab *but not the literal structures pointed at!
//  // symtab_free(lit_map, NULL);
//  printf("End literals_construct_table\n");
//  return 0;
//}

int literals_construct_table(instr_ptr_t inst){
  printf("Start literals_construct_table\n");
  Symtab_handle lit_iter;
  struct Symtab_entry * lit_entry;
  struct comp_inst * comp_ptr;
  struct literal_struct * l_ptr;
  char sep[3] = "::\0";
  char * key = malloc(2048 * sizeof(char));
  char * start = malloc(1024 * sizeof(char));
  Symtab lit_map = symtab_create();

  if (inst->compmap == NULL) return 1;
  if (inst->literals == NULL || list_undef(inst->literals)) {
    fprintf(stderr, "No literals to check because its null!\n");
    return 1;
  }

  // iterate through all components
  for (int c=0; c<list_len(inst->complist); ++c) {
    comp_ptr = (struct comp_inst *) list_access(inst->complist, c);
    if (list_undef(comp_ptr->literals)){
      printf("How did component %s get produced without literals?\n", comp_ptr->name);
      continue;
    }
    printf("Component %s has %d literals\n", comp_ptr->name, list_len(comp_ptr->literals));

    strcpy(start, comp_ptr->name);
    strcat(start, sep);

    for (int i=0; i<list_len(comp_ptr->literals); ++i){
      // get a pointer to the next literal structure
      l_ptr = (struct literal_struct*) list_access(comp_ptr->literals, i);
      // assign the component name, since we didn't bother doing so earlier
      l_ptr->source = str_dup(comp_ptr->name);
      // construct the key for insertion into the symbol table
      strcpy(key, start);
      strcat(key, l_ptr->name); // using the McCode str_cat caused a segmentation fault?!
      // check if the same-keyed literal was already provided
      if ((lit_entry = symtab_lookup(lit_map, key))){
        // This key already exists! What do we do? Trust users to know what they're doing!
        // Component-definition literals came before Component-declaration literals -- so overwrite!
        lit_entry->val = (void *) l_ptr;
      } else {
        if (l_ptr->source == NULL) printf("literal source undefined?!\n");
        if (l_ptr->name == NULL) printf("literal name undefined?!\n");
        if (l_ptr->type == NULL) printf("literal type undefined?!\n");
        symtab_add(lit_map, key, (void *) l_ptr);
      }
      // str_cat allocated new memory, and symtab_add only copied the key; so free it now

    }

  }
  if (key) free(key);
  if (start) free(start);

  // Literals table construction complete, now copy it to the instrument literals list:
  inst->literals = list_create();
  lit_iter = symtab_iterate(lit_map);
  while ((lit_entry = symtab_next(lit_iter))){
    list_add(inst->literals, lit_entry->val);
  }
  symtab_iterate_end(lit_iter);

  // Free the Symtab *but not the literal structures pointed at!
  // symtab_free(lit_map, NULL);
  printf("End literals_construct_table\n");
  return 0;
}