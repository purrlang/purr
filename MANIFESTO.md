THE PURR MANIFESTO
=================

purr exists to eliminate footguns for both humans and AI.

Not by adding rules.
By removing ambiguity.

See `KEYWORDS.md` for the frozen, canonical keyword set and short rationale.

-----------------
CORE BELIEF
-----------------
If a program can be written in two different “reasonable” ways,
the language has already failed.

Correctness must be structural, not cultural.

-----------------
DESIGN GOAL
-----------------
Make the wrong thing:
- hard to express
- obvious when attempted
- impossible to make look correct

A purr program should read like a protocol, not a puzzle.

-----------------
NON-NEGOTIABLE PRINCIPLES
-----------------

1. NO HIDDEN STATES
- No null
- No implicit defaults
- No ambient context
- No invisible mutation

If something can fail, it must be typed.
If something can change, it must be explicit.

2. ONE WAY TO DO THINGS
- One string
- One list
- One map
- One numeric widening rule
- One concurrency model

Choice is a footgun disguised as freedom.

3. EXPLICIT OVER CONVENIENT
- No implicit conversions
- No truthy / falsy
- No magical async
- No silent allocation

Every cost must be visible in the code.

4. IMMUTABILITY BY DEFAULT
- Time moves forward
- Data does not change behind your back
- Updates create new values, not surprises

Mutation, if it exists, must be opt-in and obvious.

5. TYPES DESCRIBE REALITY
- Absence is `T?`, never null
- Errors are `Result`, never exceptions
- Time is integers, never floats
- Identity is explicit, never inferred

If it matters at runtime, it belongs in the type system.

6. LOWERING IS OBVIOUS
- Every construct maps cleanly to LLVM
- No “clever” semantics
- No compiler magic

If it’s hard to explain how it compiles, it doesn’t belong.

-----------------
AI FIRST, HUMAN FRIENDLY
-----------------

purr is designed so that:
- an AI cannot guess wrong silently
- a human does not need folklore to be correct
- reading the code explains the behavior

The language collapses the solution space
so that “the obvious thing” is also the correct thing.

-----------------
WHAT PURR IS NOT
-----------------
- Not a clever language
- Not a flexible language
- Not a kitchen-sink language
- Not a playground for abstractions

-----------------
WHAT PURR IS
-----------------
- A compiler-enforced playbook
- A protocol-shaped language
- A pit of success for AI-assisted systems
- A foundation for reliable, long-running software

-----------------
FINAL RULE
-----------------
If the language lets you get it wrong,
someone eventually will.

purr exists to make that impossible.
