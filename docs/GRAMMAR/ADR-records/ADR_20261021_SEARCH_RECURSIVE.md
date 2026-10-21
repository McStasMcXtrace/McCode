# Decision record template by Michael Nygard

This is the template in [Documenting architecture decisions - Michael Nygard](http://thinkrelevance.com/blog/2011/11/15/documenting-architecture-decisions).
You can use [adr-tools](https://github.com/npryce/adr-tools) for managing the ADR files.

In each ADR file, write these sections:

# Fix in SEARCH path to bring back `%include instr` functionality

## Status

_What is the status, such as proposed, accepted, rejected, deprecated, superseded, etc.?_

required for `%include "instr"` to work again - this is a bug.

## Context

_What is the issue that we're seeing that is motivating this decision or change?_

In the past, all example instruments where located inside the "MCCODE/examples"
directory. The `%include "instr"` would then locate a "MCCODE/examples/instr" file.
The "example" directory is now a nested structure, but the search mechanism 
in `open_file_search` has not be adapted to reflect this state, so that no 
`%include "instr"` actually works (all `.instr` files reside in subdirectories).

## Decision

_What is the change that we're proposing and/or doing?_

The `open_file_search` in `mccode/file.c` can now handle recursive search.
However, the default behaviour is a single level search. To trigger a
recursive search, the path should simply end with a '/' character.

The default search for "examples" is now `examples/` (see beginning of `file.c`). 
Then, `%include "instr"` can properly find nested instrument files burried 
inside the "examples" tree structure.

## Consequences

_What becomes easier or more difficult to do because of this change?_

The `%include "instr"` is now functional again. 
It was lost at some point in the past.

The changes have been documented in the grammar and command line handling.

## Behaviour

_Describe important aspects of the new/changed behaviour_

Nothing changes for the user. 
In case a recusrive path specification is given, the search will still work as
the directory root is searched first.

