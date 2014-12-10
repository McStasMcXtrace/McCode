/* Callgraph handling code.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.
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

#ifndef GCC_CGRAPH_H
#define GCC_CGRAPH_H

#include "is-a.h"
#include "plugin-api.h"
#include "vec.h"
#include "basic-block.h"
#include "function.h"
#include "ipa-ref.h"

/* Symbol table consists of functions and variables.
   TODO: add labels and CONST_DECLs.  */
enum symtab_type
{
  SYMTAB_SYMBOL,
  SYMTAB_FUNCTION,
  SYMTAB_VARIABLE
};

/* Base of all entries in the symbol table.
   The symtab_node is inherited by cgraph and varpol nodes.  */
class GTY((desc ("%h.type"), tag ("SYMTAB_SYMBOL"),
	   chain_next ("%h.next"), chain_prev ("%h.previous")))
  symtab_node
{
public:
  /* Return name.  */
  const char *name () const;

  /* Return asm name.  */
  const char * asm_name () const;

  /* Type of the symbol.  */
  ENUM_BITFIELD (symtab_type) type : 8;

  /* The symbols resolution.  */
  ENUM_BITFIELD (ld_plugin_symbol_resolution) resolution : 8;

  /*** Flags representing the symbol type.  ***/

  /* True when symbol corresponds to a definition in current unit.
     set via cgraph_finalize_function or varpool_finalize_decl  */
  unsigned definition : 1;
  /* True when symbol is an alias.  
     Set by assemble_alias.  */
  unsigned alias : 1;
  /* True when alias is a weakref.  */
  unsigned weakref : 1;
  /* C++ frontend produce same body aliases and extra name aliases for
     virtual functions and vtables that are obviously equivalent.
     Those aliases are bit special, especially because C++ frontend
     visibility code is so ugly it can not get them right at first time
     and their visibility needs to be copied from their "masters" at
     the end of parsing.  */
  unsigned cpp_implicit_alias : 1;
  /* Set once the definition was analyzed.  The list of references and
     other properties are built during analysis.  */
  unsigned analyzed : 1;


  /*** Visibility and linkage flags.  ***/

  /* Set when function is visible by other units.  */
  unsigned externally_visible : 1;
  /* The symbol will be assumed to be used in an invisible way (like
     by an toplevel asm statement).  */
  unsigned force_output : 1;
  /* Like FORCE_OUTPUT, but in the case it is ABI requiring the symbol to be
     exported.  Unlike FORCE_OUTPUT this flag gets cleared to symbols promoted
     to static and it does not inhibit optimization.  */
  unsigned forced_by_abi : 1;
  /* True when the name is known to be unique and thus it does not need mangling.  */
  unsigned unique_name : 1;
  /* True when body and other characteristics have been removed by
     symtab_remove_unreachable_nodes. */
  unsigned body_removed : 1;

  /*** WHOPR Partitioning flags.
       These flags are used at ltrans stage when only part of the callgraph is
       available. ***/

  /* Set when variable is used from other LTRANS partition.  */
  unsigned used_from_other_partition : 1;
  /* Set when function is available in the other LTRANS partition.  
     During WPA output it is used to mark nodes that are present in
     multiple partitions.  */
  unsigned in_other_partition : 1;



  /*** other flags.  ***/

  /* Set when symbol has address taken. */
  unsigned address_taken : 1;


  /* Ordering of all symtab entries.  */
  int order;

  /* Declaration representing the symbol.  */
  tree decl;

  /* Linked list of symbol table entries starting with symtab_nodes.  */
  symtab_node *next;
  symtab_node *previous;

  /* Linked list of symbols with the same asm name.  There may be multiple
     entries for single symbol name during LTO, because symbols are renamed
     only after partitioning.

     Because inline clones are kept in the assembler name has, they also produce
     duplicate entries.

     There are also several long standing bugs where frontends and builtin
     code produce duplicated decls.  */
  symtab_node *next_sharing_asm_name;
  symtab_node *previous_sharing_asm_name;

  /* Circular list of nodes in the same comdat group if non-NULL.  */
  symtab_node *same_comdat_group;

  /* Vectors of referring and referenced entities.  */
  struct ipa_ref_list ref_list;

  /* Alias target. May be either DECL pointer or ASSEMBLER_NAME pointer
     depending to what was known to frontend on the creation time.
     Once alias is resolved, this pointer become NULL.  */
  tree alias_target;

  /* File stream where this node is being written to.  */
  struct lto_file_decl_data * lto_file_data;

  PTR GTY ((skip)) aux;
};

enum availability
{
  /* Not yet set by cgraph_function_body_availability.  */
  AVAIL_UNSET,
  /* Function body/variable initializer is unknown.  */
  AVAIL_NOT_AVAILABLE,
  /* Function body/variable initializer is known but might be replaced
     by a different one from other compilation unit and thus needs to
     be dealt with a care.  Like AVAIL_NOT_AVAILABLE it can have
     arbitrary side effects on escaping variables and functions, while
     like AVAILABLE it might access static variables.  */
  AVAIL_OVERWRITABLE,
  /* Function body/variable initializer is known and will be used in final
     program.  */
  AVAIL_AVAILABLE,
  /* Function body/variable initializer is known and all it's uses are explicitly
     visible within current unit (ie it's address is never taken and it is not
     exported to other units).
     Currently used only for functions.  */
  AVAIL_LOCAL
};

/* This is the information that is put into the cgraph local structure
   to recover a function.  */
struct lto_file_decl_data;

extern const char * const cgraph_availability_names[];
extern const char * const ld_plugin_symbol_resolution_names[];

/* Information about thunk, used only for same body aliases.  */

struct GTY(()) cgraph_thunk_info {
  /* Information about the thunk.  */
  HOST_WIDE_INT fixed_offset;
  HOST_WIDE_INT virtual_value;
  tree alias;
  bool this_adjusting;
  bool virtual_offset_p;
  /* Set to true when alias node is thunk.  */
  bool thunk_p;
};

/* Information about the function collected locally.
   Available after function is analyzed.  */

struct GTY(()) cgraph_local_info {
  /* Set when function function is visible in current compilation unit only
     and its address is never taken.  */
  unsigned local : 1;

  /* False when there is something makes versioning impossible.  */
  unsigned versionable : 1;

  /* False when function calling convention and signature can not be changed.
     This is the case when __builtin_apply_args is used.  */
  unsigned can_change_signature : 1;

  /* True when the function has been originally extern inline, but it is
     redefined now.  */
  unsigned redefined_extern_inline : 1;

  /* True if the function may enter serial irrevocable mode.  */
  unsigned tm_may_enter_irr : 1;
};

/* Information about the function that needs to be computed globally
   once compilation is finished.  Available only with -funit-at-a-time.  */

struct GTY(()) cgraph_global_info {
  /* For inline clones this points to the function they will be
     inlined into.  */
  struct cgraph_node *inlined_to;
};

/* Information about the function that is propagated by the RTL backend.
   Available only for functions that has been already assembled.  */

struct GTY(()) cgraph_rtl_info {
   unsigned int preferred_incoming_stack_boundary;
};

/* Represent which DECL tree (or reference to such tree)
   will be replaced by another tree while versioning.  */
struct GTY(()) ipa_replace_map
{
  /* The tree that will be replaced.  */
  tree old_tree;
  /* The new (replacing) tree.  */
  tree new_tree;
  /* Parameter number to replace, when old_tree is NULL.  */
  int parm_num;
  /* True when a substitution should be done, false otherwise.  */
  bool replace_p;
  /* True when we replace a reference to old_tree.  */
  bool ref_p;
};
typedef struct ipa_replace_map *ipa_replace_map_p;

struct GTY(()) cgraph_clone_info
{
  vec<ipa_replace_map_p, va_gc> *tree_map;
  bitmap args_to_skip;
  bitmap combined_args_to_skip;
};

enum cgraph_simd_clone_arg_type
{
  SIMD_CLONE_ARG_TYPE_VECTOR,
  SIMD_CLONE_ARG_TYPE_UNIFORM,
  SIMD_CLONE_ARG_TYPE_LINEAR_CONSTANT_STEP,
  SIMD_CLONE_ARG_TYPE_LINEAR_VARIABLE_STEP,
  SIMD_CLONE_ARG_TYPE_MASK
};

/* Function arguments in the original function of a SIMD clone.
   Supplementary data for `struct simd_clone'.  */

struct GTY(()) cgraph_simd_clone_arg {
  /* Original function argument as it originally existed in
     DECL_ARGUMENTS.  */
  tree orig_arg;

  /* orig_arg's function (or for extern functions type from
     TYPE_ARG_TYPES).  */
  tree orig_type;

  /* If argument is a vector, this holds the vector version of
     orig_arg that after adjusting the argument types will live in
     DECL_ARGUMENTS.  Otherwise, this is NULL.

     This basically holds:
       vector(simdlen) __typeof__(orig_arg) new_arg.  */
  tree vector_arg;

  /* vector_arg's type (or for extern functions new vector type.  */
  tree vector_type;

  /* If argument is a vector, this holds the array where the simd
     argument is held while executing the simd clone function.  This
     is a local variable in the cloned function.  Its content is
     copied from vector_arg upon entry to the clone.

     This basically holds:
       __typeof__(orig_arg) simd_array[simdlen].  */
  tree simd_array;

  /* A SIMD clone's argument can be either linear (constant or
     variable), uniform, or vector.  */
  enum cgraph_simd_clone_arg_type arg_type;

  /* For arg_type SIMD_CLONE_ARG_TYPE_LINEAR_CONSTANT_STEP this is
     the constant linear step, if arg_type is
     SIMD_CLONE_ARG_TYPE_LINEAR_VARIABLE_STEP, this is index of
     the uniform argument holding the step, otherwise 0.  */
  HOST_WIDE_INT linear_step;

  /* Variable alignment if available, otherwise 0.  */
  unsigned int alignment;
};

/* Specific data for a SIMD function clone.  */

struct GTY(()) cgraph_simd_clone {
  /* Number of words in the SIMD lane associated with this clone.  */
  unsigned int simdlen;

  /* Number of annotated function arguments in `args'.  This is
     usually the number of named arguments in FNDECL.  */
  unsigned int nargs;

  /* Max hardware vector size in bits for integral vectors.  */
  unsigned int vecsize_int;

  /* Max hardware vector size in bits for floating point vectors.  */
  unsigned int vecsize_float;

  /* The mangling character for a given vector size.  This is is used
     to determine the ISA mangling bit as specified in the Intel
     Vector ABI.  */
  unsigned char vecsize_mangle;

  /* True if this is the masked, in-branch version of the clone,
     otherwise false.  */
  unsigned int inbranch : 1;

  /* True if this is a Cilk Plus variant.  */
  unsigned int cilk_elemental : 1;

  /* Doubly linked list of SIMD clones.  */
  struct cgraph_node *prev_clone, *next_clone;

  /* Original cgraph node the SIMD clones were created for.  */
  struct cgraph_node *origin;

  /* Annotated function arguments for the original function.  */
  struct cgraph_simd_clone_arg GTY((length ("%h.nargs"))) args[1];
};


/* The cgraph data structure.
   Each function decl has assigned cgraph_node listing callees and callers.  */

struct GTY((tag ("SYMTAB_FUNCTION"))) cgraph_node : public symtab_node {
public:
  struct cgraph_edge *callees;
  struct cgraph_edge *callers;
  /* List of edges representing indirect calls with a yet undetermined
     callee.  */
  struct cgraph_edge *indirect_calls;
  /* For nested functions points to function the node is nested in.  */
  struct cgraph_node *origin;
  /* Points to first nested function, if any.  */
  struct cgraph_node *nested;
  /* Pointer to the next function with same origin, if any.  */
  struct cgraph_node *next_nested;
  /* Pointer to the next clone.  */
  struct cgraph_node *next_sibling_clone;
  struct cgraph_node *prev_sibling_clone;
  struct cgraph_node *clones;
  struct cgraph_node *clone_of;
  /* For functions with many calls sites it holds map from call expression
     to the edge to speed up cgraph_edge function.  */
  htab_t GTY((param_is (struct cgraph_edge))) call_site_hash;
  /* Declaration node used to be clone of. */
  tree former_clone_of;

  /* If this is a SIMD clone, this points to the SIMD specific
     information for it.  */
  struct cgraph_simd_clone *simdclone;
  /* If this function has SIMD clones, this points to the first clone.  */
  struct cgraph_node *simd_clones;

  /* Interprocedural passes scheduled to have their transform functions
     applied next time we execute local pass on them.  We maintain it
     per-function in order to allow IPA passes to introduce new functions.  */
  vec<ipa_opt_pass> GTY((skip)) ipa_transforms_to_apply;

  struct cgraph_local_info local;
  struct cgraph_global_info global;
  struct cgraph_rtl_info rtl;
  struct cgraph_clone_info clone;
  struct cgraph_thunk_info thunk;

  /* Expected number of executions: calculated in profile.c.  */
  gcov_type count;
  /* How to scale counts at materialization time; used to merge
     LTO units with different number of profile runs.  */
  int count_materialization_scale;
  /* Unique id of the node.  */
  int uid;
  /* ID assigned by the profiling.  */
  unsigned int profile_id;
  /* Time profiler: first run of function.  */
  int tp_first_run;

  /* Set when decl is an abstract function pointed to by the
     ABSTRACT_DECL_ORIGIN of a reachable function.  */
  unsigned used_as_abstract_origin : 1;
  /* Set once the function is lowered (i.e. its CFG is built).  */
  unsigned lowered : 1;
  /* Set once the function has been instantiated and its callee
     lists created.  */
  unsigned process : 1;
  /* How commonly executed the node is.  Initialized during branch
     probabilities pass.  */
  ENUM_BITFIELD (node_frequency) frequency : 2;
  /* True when function can only be called at startup (from static ctor).  */
  unsigned only_called_at_startup : 1;
  /* True when function can only be called at startup (from static dtor).  */
  unsigned only_called_at_exit : 1;
  /* True when function is the transactional clone of a function which
     is called only from inside transactions.  */
  /* ?? We should be able to remove this.  We have enough bits in
     cgraph to calculate it.  */
  unsigned tm_clone : 1;
  /* True if this decl is a dispatcher for function versions.  */
  unsigned dispatcher_function : 1;
  /* True if this decl calls a COMDAT-local function.  This is set up in
     compute_inline_parameters and inline_call.  */
  unsigned calls_comdat_local : 1;
};


typedef struct cgraph_node *cgraph_node_ptr;


/* Function Multiversioning info.  */
struct GTY(()) cgraph_function_version_info {
  /* The cgraph_node for which the function version info is stored.  */
  struct cgraph_node *this_node;
  /* Chains all the semantically identical function versions.  The
     first function in this chain is the version_info node of the
     default function.  */
  struct cgraph_function_version_info *prev;
  /* If this version node corresponds to a dispatcher for function
     versions, this points to the version info node of the default
     function, the first node in the chain.  */
  struct cgraph_function_version_info *next;
  /* If this node corresponds to a function version, this points
     to the dispatcher function decl, which is the function that must
     be called to execute the right function version at run-time.

     If this cgraph node is a dispatcher (if dispatcher_function is
     true, in the cgraph_node struct) for function versions, this
     points to resolver function, which holds the function body of the
     dispatcher. The dispatcher decl is an alias to the resolver
     function decl.  */
  tree dispatcher_resolver;
};

/* Get the cgraph_function_version_info node corresponding to node.  */
struct cgraph_function_version_info *
  get_cgraph_node_version (struct cgraph_node *node);

/* Insert a new cgraph_function_version_info node into cgraph_fnver_htab
   corresponding to cgraph_node NODE.  */
struct cgraph_function_version_info *
  insert_new_cgraph_node_version (struct cgraph_node *node);

/* Record that DECL1 and DECL2 are semantically identical function
   versions.  */
void record_function_versions (tree decl1, tree decl2);

/* Remove the cgraph_function_version_info and cgraph_node for DECL.  This
   DECL is a duplicate declaration.  */
void delete_function_version (tree decl);

/* A cgraph node set is a collection of cgraph nodes.  A cgraph node
   can appear in multiple sets.  */
struct cgraph_node_set_def
{
  struct pointer_map_t *map;
  vec<cgraph_node_ptr> nodes;
};

class varpool_node;
typedef varpool_node *varpool_node_ptr;


/* A varpool node set is a collection of varpool nodes.  A varpool node
   can appear in multiple sets.  */
struct varpool_node_set_def
{
  struct pointer_map_t * map;
  vec<varpool_node_ptr> nodes;
};

typedef struct cgraph_node_set_def *cgraph_node_set;


typedef struct varpool_node_set_def *varpool_node_set;


/* Iterator structure for cgraph node sets.  */
struct cgraph_node_set_iterator
{
  cgraph_node_set set;
  unsigned index;
};

/* Iterator structure for varpool node sets.  */
struct varpool_node_set_iterator
{
  varpool_node_set set;
  unsigned index;
};

#define DEFCIFCODE(code, type, string)	CIF_ ## code,
/* Reasons for inlining failures.  */
enum cgraph_inline_failed_t {
#include "cif-code.def"
  CIF_N_REASONS
};

enum cgraph_inline_failed_type_t
{
  CIF_FINAL_NORMAL = 0,
  CIF_FINAL_ERROR
};

/* Structure containing additional information about an indirect call.  */

struct GTY(()) cgraph_indirect_call_info
{
  /* When polymorphic is set, this field contains offset where the object which
     was actually used in the polymorphic resides within a larger structure.
     If agg_contents is set, the field contains the offset within the aggregate
     from which the address to call was loaded.  */
  HOST_WIDE_INT offset;
  /* OBJ_TYPE_REF_TOKEN of a polymorphic call (if polymorphic is set).  */
  HOST_WIDE_INT otr_token;
  /* Type of the object from OBJ_TYPE_REF_OBJECT. */
  tree otr_type, outer_type;
  /* Index of the parameter that is called.  */
  int param_index;
  /* ECF flags determined from the caller.  */
  int ecf_flags;
  /* Profile_id of common target obtrained from profile.  */
  int common_target_id;
  /* Probability that call will land in function with COMMON_TARGET_ID.  */
  int common_target_probability;

  /* Set when the call is a virtual call with the parameter being the
     associated object pointer rather than a simple direct call.  */
  unsigned polymorphic : 1;
  /* Set when the call is a call of a pointer loaded from contents of an
     aggregate at offset.  */
  unsigned agg_contents : 1;
  /* Set when this is a call through a member pointer.  */
  unsigned member_ptr : 1;
  /* When the previous bit is set, this one determines whether the destination
     is loaded from a parameter passed by reference. */
  unsigned by_ref : 1;
  unsigned int maybe_in_construction : 1;
  unsigned int maybe_derived_type : 1;
};

struct GTY((chain_next ("%h.next_caller"), chain_prev ("%h.prev_caller"))) cgraph_edge {
  /* Expected number of executions: calculated in profile.c.  */
  gcov_type count;
  struct cgraph_node *caller;
  struct cgraph_node *callee;
  struct cgraph_edge *prev_caller;
  struct cgraph_edge *next_caller;
  struct cgraph_edge *prev_callee;
  struct cgraph_edge *next_callee;
  gimple call_stmt;
  /* Additional information about an indirect call.  Not cleared when an edge
     becomes direct.  */
  struct cgraph_indirect_call_info *indirect_info;
  PTR GTY ((skip (""))) aux;
  /* When equal to CIF_OK, inline this call.  Otherwise, points to the
     explanation why function was not inlined.  */
  enum cgraph_inline_failed_t inline_failed;
  /* The stmt_uid of call_stmt.  This is used by LTO to recover the call_stmt
     when the function is serialized in.  */
  unsigned int lto_stmt_uid;
  /* Expected frequency of executions within the function.
     When set to CGRAPH_FREQ_BASE, the edge is expected to be called once
     per function call.  The range is 0 to CGRAPH_FREQ_MAX.  */
  int frequency;
  /* Unique id of the edge.  */
  int uid;
  /* Whether this edge was made direct by indirect inlining.  */
  unsigned int indirect_inlining_edge : 1;
  /* Whether this edge describes an indirect call with an undetermined
     callee.  */
  unsigned int indirect_unknown_callee : 1;
  /* Whether this edge is still a dangling  */
  /* True if the corresponding CALL stmt cannot be inlined.  */
  unsigned int call_stmt_cannot_inline_p : 1;
  /* Can this call throw externally?  */
  unsigned int can_throw_external : 1;
  /* Edges with SPECULATIVE flag represents indirect calls that was
     speculatively turned into direct (i.e. by profile feedback).
     The final code sequence will have form:

     if (call_target == expected_fn)
       expected_fn ();
     else
       call_target ();

     Every speculative call is represented by three components attached
     to a same call statement:
     1) a direct call (to expected_fn)
     2) an indirect call (to call_target)
     3) a IPA_REF_ADDR refrence to expected_fn.

     Optimizers may later redirect direct call to clone, so 1) and 3)
     do not need to necesarily agree with destination.  */
  unsigned int speculative : 1;
};

#define CGRAPH_FREQ_BASE 1000
#define CGRAPH_FREQ_MAX 100000

typedef struct cgraph_edge *cgraph_edge_p;


/* The varpool data structure.
   Each static variable decl has assigned varpool_node.  */

class GTY((tag ("SYMTAB_VARIABLE"))) varpool_node : public symtab_node {
public:
  /* Set when variable is scheduled to be assembled.  */
  unsigned output : 1;

  /* Set if the variable is dynamically initialized, except for
     function local statics.   */
  unsigned dynamically_initialized : 1;
};

/* Every top level asm statement is put into a asm_node.  */

struct GTY(()) asm_node {
  /* Next asm node.  */
  struct asm_node *next;
  /* String for this asm node.  */
  tree asm_str;
  /* Ordering of all cgraph nodes.  */
  int order;
};

/* Report whether or not THIS symtab node is a function, aka cgraph_node.  */

template <>
template <>
inline bool
is_a_helper <cgraph_node>::test (symtab_node *p)
{
  return p->type == SYMTAB_FUNCTION;
}

/* Report whether or not THIS symtab node is a vriable, aka varpool_node.  */

template <>
template <>
inline bool
is_a_helper <varpool_node>::test (symtab_node *p)
{
  return p->type == SYMTAB_VARIABLE;
}

extern GTY(()) symtab_node *symtab_nodes;
extern GTY(()) int cgraph_n_nodes;
extern GTY(()) int cgraph_max_uid;
extern GTY(()) int cgraph_edge_max_uid;
extern bool cgraph_global_info_ready;
enum cgraph_state
{
  /* Frontend is parsing and finalizing functions.  */
  CGRAPH_STATE_PARSING,
  /* Callgraph is being constructed.  It is safe to add new functions.  */
  CGRAPH_STATE_CONSTRUCTION,
  /* Callgraph is being at LTO time.  */
  CGRAPH_LTO_STREAMING,
  /* Callgraph is built and IPA passes are being run.  */
  CGRAPH_STATE_IPA,
  /* Callgraph is built and all functions are transformed to SSA form.  */
  CGRAPH_STATE_IPA_SSA,
  /* Functions are now ordered and being passed to RTL expanders.  */
  CGRAPH_STATE_EXPANSION,
  /* All cgraph expansion is done.  */
  CGRAPH_STATE_FINISHED
};
extern enum cgraph_state cgraph_state;
extern bool cgraph_function_flags_ready;
extern cgraph_node_set cgraph_new_nodes;

extern GTY(()) struct asm_node *asm_nodes;
extern GTY(()) int symtab_order;
extern bool cpp_implicit_aliases_done;

/* Classifcation of symbols WRT partitioning.  */
enum symbol_partitioning_class
{
   /* External declarations are ignored by partitioning algorithms and they are
      added into the boundary later via compute_ltrans_boundary.  */
   SYMBOL_EXTERNAL,
   /* Partitioned symbols are pur into one of partitions.  */
   SYMBOL_PARTITION,
   /* Duplicated symbols (such as comdat or constant pool references) are
      copied into every node needing them via add_symbol_to_partition.  */
   SYMBOL_DUPLICATE
};


/* In symtab.c  */
void symtab_register_node (symtab_node *);
void symtab_unregister_node (symtab_node *);
void symtab_remove_from_same_comdat_group (symtab_node *);
void symtab_remove_node (symtab_node *);
symtab_node *symtab_get_node (const_tree);
symtab_node *symtab_node_for_asm (const_tree asmname);
void symtab_insert_node_to_hashtable (symtab_node *);
void symtab_add_to_same_comdat_group (symtab_node *, symtab_node *);
void symtab_dissolve_same_comdat_group_list (symtab_node *node);
void dump_symtab (FILE *);
void debug_symtab (void);
void dump_symtab_node (FILE *, symtab_node *);
void debug_symtab_node (symtab_node *);
void dump_symtab_base (FILE *, symtab_node *);
void verify_symtab (void);
void verify_symtab_node (symtab_node *);
bool verify_symtab_base (symtab_node *);
bool symtab_used_from_object_file_p (symtab_node *);
void symtab_make_decl_local (tree);
symtab_node *symtab_alias_ultimate_target (symtab_node *,
					  enum availability *avail = NULL);
bool symtab_resolve_alias (symtab_node *node, symtab_node *target);
void fixup_same_cpp_alias_visibility (symtab_node *node, symtab_node *target);
bool symtab_for_node_and_aliases (symtab_node *,
				  bool (*) (symtab_node *, void *),
				  void *,
				  bool);
symtab_node *symtab_nonoverwritable_alias (symtab_node *);
enum availability symtab_node_availability (symtab_node *);
bool symtab_semantically_equivalent_p (symtab_node *, symtab_node *);
enum symbol_partitioning_class symtab_get_symbol_partitioning_class (symtab_node *);

/* In cgraph.c  */
void dump_cgraph (FILE *);
void debug_cgraph (void);
void dump_cgraph_node (FILE *, struct cgraph_node *);
void debug_cgraph_node (struct cgraph_node *);
void cgraph_remove_edge (struct cgraph_edge *);
void cgraph_remove_node (struct cgraph_node *);
void cgraph_release_function_body (struct cgraph_node *);
void release_function_body (tree);
void cgraph_node_remove_callees (struct cgraph_node *node);
struct cgraph_edge *cgraph_create_edge (struct cgraph_node *,
					struct cgraph_node *,
					gimple, gcov_type, int);
struct cgraph_edge *cgraph_create_indirect_edge (struct cgraph_node *, gimple,
						 int, gcov_type, int);
struct cgraph_indirect_call_info *cgraph_allocate_init_indirect_info (void);
struct cgraph_node * cgraph_create_node (tree);
struct cgraph_node * cgraph_create_empty_node (void);
struct cgraph_node * cgraph_get_create_node (tree);
struct cgraph_node * cgraph_same_body_alias (struct cgraph_node *, tree, tree);
struct cgraph_node * cgraph_add_thunk (struct cgraph_node *, tree, tree, bool, HOST_WIDE_INT,
				       HOST_WIDE_INT, tree, tree);
struct cgraph_node *cgraph_node_for_asm (tree);
struct cgraph_edge *cgraph_edge (struct cgraph_node *, gimple);
void cgraph_set_call_stmt (struct cgraph_edge *, gimple, bool update_speculative = true);
void cgraph_update_edges_for_call_stmt (gimple, tree, gimple);
struct cgraph_local_info *cgraph_local_info (tree);
struct cgraph_global_info *cgraph_global_info (tree);
struct cgraph_rtl_info *cgraph_rtl_info (tree);
struct cgraph_node *cgraph_create_function_alias (tree, tree);
void cgraph_call_node_duplication_hooks (struct cgraph_node *,
					 struct cgraph_node *);
void cgraph_call_edge_duplication_hooks (struct cgraph_edge *,
				         struct cgraph_edge *);

void cgraph_redirect_edge_callee (struct cgraph_edge *, struct cgraph_node *);
struct cgraph_edge *cgraph_make_edge_direct (struct cgraph_edge *, struct cgraph_node *);
bool cgraph_only_called_directly_p (struct cgraph_node *);

bool cgraph_function_possibly_inlined_p (tree);
void cgraph_unnest_node (struct cgraph_node *);

enum availability cgraph_function_body_availability (struct cgraph_node *);
void cgraph_add_new_function (tree, bool);
const char* cgraph_inline_failed_string (cgraph_inline_failed_t);
cgraph_inline_failed_type_t cgraph_inline_failed_type (cgraph_inline_failed_t);

void cgraph_set_nothrow_flag (struct cgraph_node *, bool);
void cgraph_set_const_flag (struct cgraph_node *, bool, bool);
void cgraph_set_pure_flag (struct cgraph_node *, bool, bool);
bool cgraph_node_cannot_return (struct cgraph_node *);
bool cgraph_edge_cannot_lead_to_return (struct cgraph_edge *);
bool cgraph_will_be_removed_from_program_if_no_direct_calls
  (struct cgraph_node *node);
bool cgraph_can_remove_if_no_direct_calls_and_refs_p
  (struct cgraph_node *node);
bool cgraph_can_remove_if_no_direct_calls_p (struct cgraph_node *node);
bool resolution_used_from_other_file_p (enum ld_plugin_symbol_resolution);
bool cgraph_for_node_thunks_and_aliases (struct cgraph_node *,
			                 bool (*) (struct cgraph_node *, void *),
			                 void *,
					 bool);
bool cgraph_for_node_and_aliases (struct cgraph_node *,
		                  bool (*) (struct cgraph_node *, void *),
			          void *, bool);
vec<cgraph_edge_p>  collect_callers_of_node (struct cgraph_node *node);
void verify_cgraph (void);
void verify_cgraph_node (struct cgraph_node *);
void cgraph_mark_address_taken_node (struct cgraph_node *);

typedef void (*cgraph_edge_hook)(struct cgraph_edge *, void *);
typedef void (*cgraph_node_hook)(struct cgraph_node *, void *);
typedef void (*varpool_node_hook)(varpool_node *, void *);
typedef void (*cgraph_2edge_hook)(struct cgraph_edge *, struct cgraph_edge *,
				  void *);
typedef void (*cgraph_2node_hook)(struct cgraph_node *, struct cgraph_node *,
				  void *);
struct cgraph_edge_hook_list;
struct cgraph_node_hook_list;
struct varpool_node_hook_list;
struct cgraph_2edge_hook_list;
struct cgraph_2node_hook_list;
struct cgraph_edge_hook_list *cgraph_add_edge_removal_hook (cgraph_edge_hook, void *);
void cgraph_remove_edge_removal_hook (struct cgraph_edge_hook_list *);
struct cgraph_node_hook_list *cgraph_add_node_removal_hook (cgraph_node_hook,
							    void *);
void cgraph_remove_node_removal_hook (struct cgraph_node_hook_list *);
struct varpool_node_hook_list *varpool_add_node_removal_hook (varpool_node_hook,
							      void *);
void varpool_remove_node_removal_hook (struct varpool_node_hook_list *);
struct cgraph_node_hook_list *cgraph_add_function_insertion_hook (cgraph_node_hook,
							          void *);
void cgraph_remove_function_insertion_hook (struct cgraph_node_hook_list *);
struct varpool_node_hook_list *varpool_add_variable_insertion_hook (varpool_node_hook,
							            void *);
void varpool_remove_variable_insertion_hook (struct varpool_node_hook_list *);
void cgraph_call_function_insertion_hooks (struct cgraph_node *node);
struct cgraph_2edge_hook_list *cgraph_add_edge_duplication_hook (cgraph_2edge_hook, void *);
void cgraph_remove_edge_duplication_hook (struct cgraph_2edge_hook_list *);
struct cgraph_2node_hook_list *cgraph_add_node_duplication_hook (cgraph_2node_hook, void *);
void cgraph_remove_node_duplication_hook (struct cgraph_2node_hook_list *);
gimple cgraph_redirect_edge_call_stmt_to_callee (struct cgraph_edge *);
struct cgraph_node * cgraph_function_node (struct cgraph_node *,
					   enum availability *avail = NULL);
bool cgraph_get_body (struct cgraph_node *node);
struct cgraph_edge *
cgraph_turn_edge_to_speculative (struct cgraph_edge *,
				 struct cgraph_node *,
				 gcov_type, int);
void cgraph_speculative_call_info (struct cgraph_edge *,
				   struct cgraph_edge *&,
				   struct cgraph_edge *&,
				   struct ipa_ref *&);
extern bool gimple_check_call_matching_types (gimple, tree, bool);

/* In cgraphunit.c  */
struct asm_node *add_asm_node (tree);
extern FILE *cgraph_dump_file;
void cgraph_finalize_function (tree, bool);
void finalize_compilation_unit (void);
void compile (void);
void init_cgraph (void);
void cgraph_process_new_functions (void);
void cgraph_process_same_body_aliases (void);
void fixup_same_cpp_alias_visibility (symtab_node *, symtab_node *target, tree);
/*  Initialize datastructures so DECL is a function in lowered gimple form.
    IN_SSA is true if the gimple is in SSA.  */
basic_block init_lowered_empty_function (tree, bool);
void cgraph_reset_node (struct cgraph_node *);
bool expand_thunk (struct cgraph_node *, bool);

/* In cgraphclones.c  */

struct cgraph_edge * cgraph_clone_edge (struct cgraph_edge *,
					struct cgraph_node *, gimple,
					unsigned, gcov_type, int, bool);
struct cgraph_node * cgraph_clone_node (struct cgraph_node *, tree, gcov_type,
					int, bool, vec<cgraph_edge_p>,
					bool, struct cgraph_node *, bitmap);
tree clone_function_name (tree decl, const char *);
struct cgraph_node * cgraph_create_virtual_clone (struct cgraph_node *old_node,
			                          vec<cgraph_edge_p>,
			                          vec<ipa_replace_map_p, va_gc> *tree_map,
			                          bitmap args_to_skip,
						  const char *clone_name);
struct cgraph_node *cgraph_find_replacement_node (struct cgraph_node *);
bool cgraph_remove_node_and_inline_clones (struct cgraph_node *, struct cgraph_node *);
void cgraph_set_call_stmt_including_clones (struct cgraph_node *, gimple, gimple,
					    bool update_speculative = true);
void cgraph_create_edge_including_clones (struct cgraph_node *,
					  struct cgraph_node *,
					  gimple, gimple, gcov_type, int,
					  cgraph_inline_failed_t);
void cgraph_materialize_all_clones (void);
struct cgraph_node * cgraph_copy_node_for_versioning (struct cgraph_node *,
		tree, vec<cgraph_edge_p>, bitmap);
struct cgraph_node *cgraph_function_versioning (struct cgraph_node *,
						vec<cgraph_edge_p>,
						vec<ipa_replace_map_p, va_gc> *,
						bitmap, bool, bitmap,
						basic_block, const char *);
void tree_function_versioning (tree, tree, vec<ipa_replace_map_p, va_gc> *,
			       bool, bitmap, bool, bitmap, basic_block);
struct cgraph_edge *cgraph_resolve_speculation (struct cgraph_edge *, tree);

/* In cgraphbuild.c  */
unsigned int rebuild_cgraph_edges (void);
void cgraph_rebuild_references (void);
int compute_call_stmt_bb_frequency (tree, basic_block bb);
void record_references_in_initializer (tree, bool);
void ipa_record_stmt_references (struct cgraph_node *, gimple);

/* In ipa.c  */
bool symtab_remove_unreachable_nodes (bool, FILE *);
cgraph_node_set cgraph_node_set_new (void);
cgraph_node_set_iterator cgraph_node_set_find (cgraph_node_set,
					       struct cgraph_node *);
void cgraph_node_set_add (cgraph_node_set, struct cgraph_node *);
void cgraph_node_set_remove (cgraph_node_set, struct cgraph_node *);
void dump_cgraph_node_set (FILE *, cgraph_node_set);
void debug_cgraph_node_set (cgraph_node_set);
void free_cgraph_node_set (cgraph_node_set);
void cgraph_build_static_cdtor (char which, tree body, int priority);

varpool_node_set varpool_node_set_new (void);
varpool_node_set_iterator varpool_node_set_find (varpool_node_set,
						 varpool_node *);
void varpool_node_set_add (varpool_node_set, varpool_node *);
void varpool_node_set_remove (varpool_node_set, varpool_node *);
void dump_varpool_node_set (FILE *, varpool_node_set);
void debug_varpool_node_set (varpool_node_set);
void free_varpool_node_set (varpool_node_set);
void ipa_discover_readonly_nonaddressable_vars (void);
bool varpool_externally_visible_p (varpool_node *);

/* In predict.c  */
bool cgraph_maybe_hot_edge_p (struct cgraph_edge *e);
bool cgraph_optimize_for_size_p (struct cgraph_node *);

/* In varpool.c  */
varpool_node *varpool_create_empty_node (void);
varpool_node *varpool_node_for_decl (tree);
varpool_node *varpool_node_for_asm (tree asmname);
void varpool_mark_needed_node (varpool_node *);
void debug_varpool (void);
void dump_varpool (FILE *);
void dump_varpool_node (FILE *, varpool_node *);

void varpool_finalize_decl (tree);
enum availability cgraph_variable_initializer_availability (varpool_node *);
void cgraph_make_node_local (struct cgraph_node *);
bool cgraph_node_can_be_local_p (struct cgraph_node *);


void varpool_remove_node (varpool_node *node);
void varpool_finalize_named_section_flags (varpool_node *node);
bool varpool_output_variables (void);
bool varpool_assemble_decl (varpool_node *node);
void varpool_analyze_node (varpool_node *);
varpool_node * varpool_extra_name_alias (tree, tree);
varpool_node * varpool_create_variable_alias (tree, tree);
void varpool_reset_queue (void);
tree ctor_for_folding (tree);
bool varpool_for_node_and_aliases (varpool_node *,
		                   bool (*) (varpool_node *, void *),
			           void *, bool);
void varpool_add_new_variable (tree);
void symtab_initialize_asm_name_hash (void);
void symtab_prevail_in_asm_name_hash (symtab_node *node);
void varpool_remove_initializer (varpool_node *);

/* In cgraph.c */
extern void change_decl_assembler_name (tree, tree);

/* Return callgraph node for given symbol and check it is a function. */
static inline struct cgraph_node *
cgraph (symtab_node *node)
{
  gcc_checking_assert (!node || node->type == SYMTAB_FUNCTION);
  return (struct cgraph_node *)node;
}

/* Return varpool node for given symbol and check it is a variable.  */
static inline varpool_node *
varpool (symtab_node *node)
{
  gcc_checking_assert (!node || node->type == SYMTAB_VARIABLE);
  return (varpool_node *)node;
}

/* Return callgraph node for given symbol and check it is a function. */
static inline struct cgraph_node *
cgraph_get_node (const_tree decl)
{
  gcc_checking_assert (TREE_CODE (decl) == FUNCTION_DECL);
  return cgraph (symtab_get_node (decl));
}

/* Return varpool node for given symbol and check it is a function. */
static inline varpool_node *
varpool_get_node (const_tree decl)
{
  gcc_checking_assert (TREE_CODE (decl) == VAR_DECL);
  return varpool (symtab_get_node (decl));
}

/* Walk all symbols.  */
#define FOR_EACH_SYMBOL(node) \
   for ((node) = symtab_nodes; (node); (node) = (node)->next)


/* Return first variable.  */
static inline varpool_node *
varpool_first_variable (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    if (varpool_node *vnode = dyn_cast <varpool_node> (node))
      return vnode;
  return NULL;
}

/* Return next variable after NODE.  */
static inline varpool_node *
varpool_next_variable (varpool_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    if (varpool_node *vnode1 = dyn_cast <varpool_node> (node1))
      return vnode1;
  return NULL;
}
/* Walk all variables.  */
#define FOR_EACH_VARIABLE(node) \
   for ((node) = varpool_first_variable (); \
        (node); \
	(node) = varpool_next_variable ((node)))

/* Return first reachable static variable with initializer.  */
static inline varpool_node *
varpool_first_static_initializer (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    {
      varpool_node *vnode = dyn_cast <varpool_node> (node);
      if (vnode && DECL_INITIAL (node->decl))
	return vnode;
    }
  return NULL;
}

/* Return next reachable static variable with initializer after NODE.  */
static inline varpool_node *
varpool_next_static_initializer (varpool_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    {
      varpool_node *vnode1 = dyn_cast <varpool_node> (node1);
      if (vnode1 && DECL_INITIAL (node1->decl))
	return vnode1;
    }
  return NULL;
}

/* Walk all static variables with initializer set.  */
#define FOR_EACH_STATIC_INITIALIZER(node) \
   for ((node) = varpool_first_static_initializer (); (node); \
        (node) = varpool_next_static_initializer (node))

/* Return first reachable static variable with initializer.  */
static inline varpool_node *
varpool_first_defined_variable (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    {
      varpool_node *vnode = dyn_cast <varpool_node> (node);
      if (vnode && vnode->definition)
	return vnode;
    }
  return NULL;
}

/* Return next reachable static variable with initializer after NODE.  */
static inline varpool_node *
varpool_next_defined_variable (varpool_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    {
      varpool_node *vnode1 = dyn_cast <varpool_node> (node1);
      if (vnode1 && vnode1->definition)
	return vnode1;
    }
  return NULL;
}
/* Walk all variables with definitions in current unit.  */
#define FOR_EACH_DEFINED_VARIABLE(node) \
   for ((node) = varpool_first_defined_variable (); (node); \
        (node) = varpool_next_defined_variable (node))

/* Return first function with body defined.  */
static inline struct cgraph_node *
cgraph_first_defined_function (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    {
      cgraph_node *cn = dyn_cast <cgraph_node> (node);
      if (cn && cn->definition)
	return cn;
    }
  return NULL;
}

/* Return next function with body defined after NODE.  */
static inline struct cgraph_node *
cgraph_next_defined_function (struct cgraph_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    {
      cgraph_node *cn1 = dyn_cast <cgraph_node> (node1);
      if (cn1 && cn1->definition)
	return cn1;
    }
  return NULL;
}

/* Walk all functions with body defined.  */
#define FOR_EACH_DEFINED_FUNCTION(node) \
   for ((node) = cgraph_first_defined_function (); (node); \
        (node) = cgraph_next_defined_function ((node)))

/* Return first function.  */
static inline struct cgraph_node *
cgraph_first_function (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    if (cgraph_node *cn = dyn_cast <cgraph_node> (node))
      return cn;
  return NULL;
}

/* Return next function.  */
static inline struct cgraph_node *
cgraph_next_function (struct cgraph_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    if (cgraph_node *cn1 = dyn_cast <cgraph_node> (node1))
      return cn1;
  return NULL;
}
/* Walk all functions.  */
#define FOR_EACH_FUNCTION(node) \
   for ((node) = cgraph_first_function (); (node); \
        (node) = cgraph_next_function ((node)))

/* Return true when NODE is a function with Gimple body defined
   in current unit.  Functions can also be define externally or they
   can be thunks with no Gimple representation.

   Note that at WPA stage, the function body may not be present in memory.  */

static inline bool
cgraph_function_with_gimple_body_p (struct cgraph_node *node)
{
  return node->definition && !node->thunk.thunk_p && !node->alias;
}

/* Return first function with body defined.  */
static inline struct cgraph_node *
cgraph_first_function_with_gimple_body (void)
{
  symtab_node *node;
  for (node = symtab_nodes; node; node = node->next)
    {
      cgraph_node *cn = dyn_cast <cgraph_node> (node);
      if (cn && cgraph_function_with_gimple_body_p (cn))
	return cn;
    }
  return NULL;
}

/* Return next reachable static variable with initializer after NODE.  */
static inline struct cgraph_node *
cgraph_next_function_with_gimple_body (struct cgraph_node *node)
{
  symtab_node *node1 = node->next;
  for (; node1; node1 = node1->next)
    {
      cgraph_node *cn1 = dyn_cast <cgraph_node> (node1);
      if (cn1 && cgraph_function_with_gimple_body_p (cn1))
	return cn1;
    }
  return NULL;
}

/* Walk all functions with body defined.  */
#define FOR_EACH_FUNCTION_WITH_GIMPLE_BODY(node) \
   for ((node) = cgraph_first_function_with_gimple_body (); (node); \
        (node) = cgraph_next_function_with_gimple_body (node))

/* Create a new static variable of type TYPE.  */
tree add_new_static_var (tree type);

/* Return true if iterator CSI points to nothing.  */
static inline bool
csi_end_p (cgraph_node_set_iterator csi)
{
  return csi.index >= csi.set->nodes.length ();
}

/* Advance iterator CSI.  */
static inline void
csi_next (cgraph_node_set_iterator *csi)
{
  csi->index++;
}

/* Return the node pointed to by CSI.  */
static inline struct cgraph_node *
csi_node (cgraph_node_set_iterator csi)
{
  return csi.set->nodes[csi.index];
}

/* Return an iterator to the first node in SET.  */
static inline cgraph_node_set_iterator
csi_start (cgraph_node_set set)
{
  cgraph_node_set_iterator csi;

  csi.set = set;
  csi.index = 0;
  return csi;
}

/* Return true if SET contains NODE.  */
static inline bool
cgraph_node_in_set_p (struct cgraph_node *node, cgraph_node_set set)
{
  cgraph_node_set_iterator csi;
  csi = cgraph_node_set_find (set, node);
  return !csi_end_p (csi);
}

/* Return number of nodes in SET.  */
static inline size_t
cgraph_node_set_size (cgraph_node_set set)
{
  return set->nodes.length ();
}

/* Return true if iterator VSI points to nothing.  */
static inline bool
vsi_end_p (varpool_node_set_iterator vsi)
{
  return vsi.index >= vsi.set->nodes.length ();
}

/* Advance iterator VSI.  */
static inline void
vsi_next (varpool_node_set_iterator *vsi)
{
  vsi->index++;
}

/* Return the node pointed to by VSI.  */
static inline varpool_node *
vsi_node (varpool_node_set_iterator vsi)
{
  return vsi.set->nodes[vsi.index];
}

/* Return an iterator to the first node in SET.  */
static inline varpool_node_set_iterator
vsi_start (varpool_node_set set)
{
  varpool_node_set_iterator vsi;

  vsi.set = set;
  vsi.index = 0;
  return vsi;
}

/* Return true if SET contains NODE.  */
static inline bool
varpool_node_in_set_p (varpool_node *node, varpool_node_set set)
{
  varpool_node_set_iterator vsi;
  vsi = varpool_node_set_find (set, node);
  return !vsi_end_p (vsi);
}

/* Return number of nodes in SET.  */
static inline size_t
varpool_node_set_size (varpool_node_set set)
{
  return set->nodes.length ();
}

/* Uniquize all constants that appear in memory.
   Each constant in memory thus far output is recorded
   in `const_desc_table'.  */

struct GTY(()) constant_descriptor_tree {
  /* A MEM for the constant.  */
  rtx rtl;

  /* The value of the constant.  */
  tree value;

  /* Hash of value.  Computing the hash from value each time
     hashfn is called can't work properly, as that means recursive
     use of the hash table during hash table expansion.  */
  hashval_t hash;
};

/* Return true if set is nonempty.  */
static inline bool
cgraph_node_set_nonempty_p (cgraph_node_set set)
{
  return !set->nodes.is_empty ();
}

/* Return true if set is nonempty.  */
static inline bool
varpool_node_set_nonempty_p (varpool_node_set set)
{
  return !set->nodes.is_empty ();
}

/* Return true when function NODE is only called directly or it has alias.
   i.e. it is not externally visible, address was not taken and
   it is not used in any other non-standard way.  */

static inline bool
cgraph_only_called_directly_or_aliased_p (struct cgraph_node *node)
{
  gcc_assert (!node->global.inlined_to);
  return (!node->force_output && !node->address_taken
	  && !node->used_from_other_partition
	  && !DECL_VIRTUAL_P (node->decl)
	  && !DECL_STATIC_CONSTRUCTOR (node->decl)
	  && !DECL_STATIC_DESTRUCTOR (node->decl)
	  && !node->externally_visible);
}

/* Return true when function NODE can be removed from callgraph
   if all direct calls are eliminated.  */

static inline bool
varpool_can_remove_if_no_refs (varpool_node *node)
{
  if (DECL_EXTERNAL (node->decl))
    return true;
  return (!node->force_output && !node->used_from_other_partition
  	  && ((DECL_COMDAT (node->decl)
	       && !node->forced_by_abi
	       && !symtab_used_from_object_file_p (node))
	      || !node->externally_visible
	      || DECL_HAS_VALUE_EXPR_P (node->decl)));
}

/* Return true when all references to VNODE must be visible in ipa_ref_list.
   i.e. if the variable is not externally visible or not used in some magic
   way (asm statement or such).
   The magic uses are all summarized in force_output flag.  */

static inline bool
varpool_all_refs_explicit_p (varpool_node *vnode)
{
  return (vnode->definition
	  && !vnode->externally_visible
	  && !vnode->used_from_other_partition
	  && !vnode->force_output);
}

/* Constant pool accessor function.  */
htab_t constant_pool_htab (void);

/* FIXME: inappropriate dependency of cgraph on IPA.  */
#include "ipa-ref-inline.h"

/* Return node that alias N is aliasing.  */

static inline symtab_node *
symtab_alias_target (symtab_node *n)
{
  struct ipa_ref *ref;
  ipa_ref_list_reference_iterate (&n->ref_list, 0, ref);
  gcc_checking_assert (ref->use == IPA_REF_ALIAS);
  return ref->referred;
}

static inline struct cgraph_node *
cgraph_alias_target (struct cgraph_node *n)
{
  return dyn_cast <cgraph_node> (symtab_alias_target (n));
}

static inline varpool_node *
varpool_alias_target (varpool_node *n)
{
  return dyn_cast <varpool_node> (symtab_alias_target (n));
}

/* Given NODE, walk the alias chain to return the function NODE is alias of.
   Do not walk through thunks.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.  */

static inline struct cgraph_node *
cgraph_function_or_thunk_node (struct cgraph_node *node,
			       enum availability *availability = NULL)
{
  struct cgraph_node *n;

  n = dyn_cast <cgraph_node> (symtab_alias_ultimate_target (node,
							    availability));
  if (!n && availability)
    *availability = AVAIL_NOT_AVAILABLE;
  return n;
}
/* Given NODE, walk the alias chain to return the function NODE is alias of.
   Do not walk through thunks.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.  */

static inline varpool_node *
varpool_variable_node (varpool_node *node,
		       enum availability *availability = NULL)
{
  varpool_node *n;

  if (node)
    n = dyn_cast <varpool_node> (symtab_alias_ultimate_target (node,
							       availability));
  else
    n = NULL;

  if (!n && availability)
    *availability = AVAIL_NOT_AVAILABLE;
  return n;
}

/* Return true when the edge E represents a direct recursion.  */
static inline bool
cgraph_edge_recursive_p (struct cgraph_edge *e)
{
  struct cgraph_node *callee = cgraph_function_or_thunk_node (e->callee, NULL);
  if (e->caller->global.inlined_to)
    return e->caller->global.inlined_to->decl == callee->decl;
  else
    return e->caller->decl == callee->decl;
}

/* Return true if the TM_CLONE bit is set for a given FNDECL.  */
static inline bool
decl_is_tm_clone (const_tree fndecl)
{
  struct cgraph_node *n = cgraph_get_node (fndecl);
  if (n)
    return n->tm_clone;
  return false;
}

/* Likewise indicate that a node is needed, i.e. reachable via some
   external means.  */

static inline void
cgraph_mark_force_output_node (struct cgraph_node *node)
{
  node->force_output = 1;
  gcc_checking_assert (!node->global.inlined_to);
}

/* Return true when the symbol is real symbol, i.e. it is not inline clone
   or abstract function kept for debug info purposes only.  */

static inline bool
symtab_real_symbol_p (symtab_node *node)
{
  struct cgraph_node *cnode;

  if (DECL_ABSTRACT (node->decl))
    return false;
  if (!is_a <cgraph_node> (node))
    return true;
  cnode = cgraph (node);
  if (cnode->global.inlined_to)
    return false;
  return true;
}

/* Return true if NODE can be discarded by linker from the binary.  */

static inline bool
symtab_can_be_discarded (symtab_node *node)
{
  return (DECL_EXTERNAL (node->decl)
	  || (DECL_ONE_ONLY (node->decl)
	      && node->resolution != LDPR_PREVAILING_DEF
	      && node->resolution != LDPR_PREVAILING_DEF_IRONLY
	      && node->resolution != LDPR_PREVAILING_DEF_IRONLY_EXP));
}

/* Return true if NODE is local to a particular COMDAT group, and must not
   be named from outside the COMDAT.  This is used for C++ decloned
   constructors.  */

static inline bool
symtab_comdat_local_p (symtab_node *node)
{
  return (node->same_comdat_group && !TREE_PUBLIC (node->decl));
}

/* Return true if ONE and TWO are part of the same COMDAT group.  */

static inline bool
symtab_in_same_comdat_p (symtab_node *one, symtab_node *two)
{
  return DECL_COMDAT_GROUP (one->decl) == DECL_COMDAT_GROUP (two->decl);
}
#endif  /* GCC_CGRAPH_H  */
