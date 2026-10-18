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

A check has been added in the `instrument.y:jumpname` rule to search the the component name in the `comp_instances`list, and prints error message when not found.

In addition, I also add two `exit` statements in `read_component`, that would bring SEGV when an autoload error was triggered, but returned an NULL pointer further accessed/freed.

## Consequences

_What becomes easier or more difficult to do because of this change?_

When an undef JUMP reference is used in an instrument file, an error is reported.

## Behaviour

_Describe important aspects of the new/changed behaviour_

See above
