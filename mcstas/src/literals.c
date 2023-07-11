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

int literals_construct_table(instr_ptr_t inst){
  Symtab_handle lit_iter;
  struct Symtab_entry * lit_entry;
  struct comp_inst * comp_ptr;
  struct literal_struct * l_ptr;
  struct literal_struct * t_ptr;
  char sep[3] = "::\0";
  char * key = malloc(2048 * sizeof(char));
  char * start = malloc(1024 * sizeof(char));
  Symtab lit_map = symtab_create();
  int added = 0;

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

    strcpy(start, comp_ptr->name);
    strcat(start, sep);

    for (int i=0; i<list_len(comp_ptr->literals); ++i){
      // get a pointer to the next literal structure
      l_ptr = (struct literal_struct*) list_access(comp_ptr->literals, i);
      // Assign the component instance name, which has not been set yet
      if (l_ptr->source == NULL) l_ptr->source = comp_ptr->name;
      // construct the key for insertion into the symbol table
      strcpy(key, start);
      strcat(key, t_ptr->name); // using the McCode str_cat caused a segmentation fault?!
      // check if the same-keyed literal was already provided
      if ((lit_entry = symtab_lookup(lit_map, key))){
        // This key already exists! What do we do? Trust users to know what they're doing!
        // Component-definition literals came before Component-declaration literals -- so overwrite!
        lit_entry->val = (void *) l_ptr;
      } else {
        symtab_add(lit_map, key, (void *) l_ptr);
      }
    }
  }
  // str_cat allocated new memory, and symtab_add only copied the key; so free it now
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
  symtab_free(lit_map, NULL);
  return 0;
}

static void _literals_assign_source(List literals, char * source, int overwrite){
  struct literal_struct * l_ptr;
  for (int i=0; i<list_len(literals); ++i){
    l_ptr = (struct literal_struct *) list_access(literals, i);
    if (overwrite || l_ptr->source == NULL) {
      l_ptr->source = str_dup(source);
    } else {
      fprintf(stderr, "Literal already has source defined as %s!\n", l_ptr->source);
    }
  }
}

static void _literals_assign_instance_field(List literals, int value){
  struct literal_struct * l_ptr;
  for (int i=0; i<list_len(literals); ++i){
    l_ptr = (struct literal_struct *) list_access(literals, i);
    l_ptr->instance = value;
  }
}

void literals_assign_source(List literals, char * source){
  _literals_assign_source(literals, source, 0);
}
void literals_overwrite_source(List literals, char * source){
  _literals_assign_source(literals, source, 1);
}

void literals_assign_from_definition(List literals){
  _literals_assign_instance_field(literals, 0);
}
void literals_assign_from_instance(List literals){
  _literals_assign_instance_field(literals, 1);
}


Symtab literals_separate_by_source(List literals, int source_type){
  Symtab sources = symtab_create();
  struct Symtab_entry * s_ptr;
  struct literal_struct * l_ptr;
  for (int i=0; i<list_len(literals); ++i){
    l_ptr = list_access(literals, i);
    if (source_type == l_ptr->instance) {
      s_ptr = symtab_lookup(sources, l_ptr->source == NULL ? "null" : l_ptr->source);
      if (s_ptr == NULL) {
        s_ptr = symtab_add(sources, l_ptr->source == NULL ? "null" : l_ptr->source, (void *) list_create());
      }
      list_add(s_ptr->val, l_ptr);
    }
  }
  return sources;
}