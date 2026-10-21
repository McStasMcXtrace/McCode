# Decision record template by Michael Nygard

This is the template in [Documenting architecture decisions - Michael Nygard](http://thinkrelevance.com/blog/2011/11/15/documenting-architecture-decisions).
You can use [adr-tools](https://github.com/npryce/adr-tools) for managing the ADR files.

In each ADR file, write these sections:

# Title

## Status

_What is the status, such as proposed, accepted, rejected, deprecated, superseded, etc.?_

proposed, and I bet accepted.

## Context

_What is the issue that we're seeing that is motivating this decision or change?_

Until now, the use of JUMP did not check for the existence of the target component instance.

## Decision

_What is the change that we're proposing and/or doing?_

A check was first added in the `instrument.y:jumpname` rule, looking the name up in `comp_instances`. It was removed again: at that point in parsing only the components declared before the JUMP are known, so it rejected valid forward JUMPs and JUMPs to the component itself by name. Unknown targets are reported by `cogen.c:detect_skipable_transforms` ("target ... is not a component of this instrument"), after the whole TRACE has been parsed, as mccode-antlr does in `set_jump_absolute_targets`.

In addition, I also add two `exit` statements in `read_component`, that would bring SEGV when an autoload error was triggered, but returned an NULL pointer further accessed/freed.

## Consequences

_What becomes easier or more difficult to do because of this change?_

When an undef JUMP reference is used in an instrument file, an error is reported (by cogen). Forward JUMP targets remain allowed.

## Behaviour

_Describe important aspects of the new/changed behaviour_

See above
