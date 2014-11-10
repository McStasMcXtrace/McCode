/* IPA reference lists.
   Copyright (C) 2010-2014 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

struct cgraph_node;
class varpool_node;
class symtab_node;


/* How the reference is done.  */
enum GTY(()) ipa_ref_use
{
  IPA_REF_LOAD,
  IPA_REF_STORE,
  IPA_REF_ADDR,
  IPA_REF_ALIAS
};

/* Record of reference in callgraph or varpool.  */
struct GTY(()) ipa_ref
{
  symtab_node *referring;
  symtab_node *referred;
  gimple stmt;
  unsigned int lto_stmt_uid;
  unsigned int referred_index;
  ENUM_BITFIELD (ipa_ref_use) use:2;
  unsigned int speculative:1;
};

typedef struct ipa_ref ipa_ref_t;
typedef struct ipa_ref *ipa_ref_ptr;


/* List of references.  This is stored in both callgraph and varpool nodes.  */
struct GTY(()) ipa_ref_list
{
  /* Store actual references in references vector.  */
  vec<ipa_ref_t, va_gc> *references;
  /* Referring is vector of pointers to references.  It must not live in GGC space
     or GGC will try to mark middle of references vectors.  */
  vec<ipa_ref_ptr>  GTY((skip)) referring;
};

struct ipa_ref * ipa_record_reference (symtab_node *,
				       symtab_node *,
				       enum ipa_ref_use, gimple);
struct ipa_ref * ipa_maybe_record_reference (symtab_node *, tree,
					     enum ipa_ref_use, gimple);

void ipa_remove_reference (struct ipa_ref *);
void ipa_remove_all_references (struct ipa_ref_list *);
void ipa_remove_all_referring (struct ipa_ref_list *);
void ipa_dump_references (FILE *, struct ipa_ref_list *);
void ipa_dump_referring (FILE *, struct ipa_ref_list *);
void ipa_clone_references (symtab_node *, struct ipa_ref_list *);
void ipa_clone_referring (symtab_node *, struct ipa_ref_list *);
struct ipa_ref * ipa_clone_ref (struct ipa_ref *, symtab_node *, gimple);
bool ipa_ref_cannot_lead_to_return (struct ipa_ref *);
bool ipa_ref_has_aliases_p (struct ipa_ref_list *);
struct ipa_ref * ipa_find_reference (symtab_node *, symtab_node *, gimple, unsigned int);
void ipa_remove_stmt_references (symtab_node *, gimple);
void ipa_clear_stmts_in_references (symtab_node *);
