# `.purr` (Purr v1 project file) — Normative Specification

This document defines the **Purr v1** project file format.

The project file describes a build at a high level. It is **not** a general-purpose configuration language.

## 1) Core principles

The format MUST be:
- Text-based
- UTF-8
- Line-oriented
- One directive per line
- No nesting
- No arrays
- No inference
- Trivial to parse in a bootstrap compiler
- Safe and predictable for AI generation

The following formats are explicitly forbidden for Purr v1 project configuration:
- JSON
- YAML
- TOML
- Any nested syntax

## 2) File format

- Filename: `.purr`
- Encoding: UTF-8
- Newlines: either `\n` or `\r\n`
- Blank lines are allowed
- Comments start with `#` and extend to end-of-line
- Order of lines does not matter
- Unknown directives are errors

## 3) Project root semantics (tooling)

These rules define **project discovery** for Purr tooling.

### 3.1 Core rule

- A directory containing a file named exactly `.purr` is a project root.
- The filename is exactly `.purr`.
  - No alternatives.
  - No configuration via file naming.

### 3.2 Project discovery

- Tooling searches upward from the working directory.
- The first `.purr` file encountered defines the project root.
- All compilation occurs relative to that root.

### 3.3 No nested projects

- A `.purr` file MUST NOT exist inside a directory that is already under another project root.
- Nested `.purr` files are a hard error.
- Tooling MUST refuse to proceed if nesting is detected.

### 3.4 Sibling projects

- A repository may contain multiple sibling projects.
- Each project has its own `.purr` file.
- Projects do not inherit configuration or dependencies.

Valid layout:

```text
repo/
  compiler/
    .purr
  runtime/
    .purr
  cli/
    .purr
```

Invalid layout (nested project):

```text
repo/
  .purr
  subproject/
    .purr
```

### 3.5 Language vs tooling

- The Purr language does not care about directories.
- The `.purr` file is consumed only by tooling.
- Modules, imports, and namespaces are unaffected by filesystem layout.

## 4) Lexical rules

### 4.1 Line normalization

For parsing, the implementation MUST process the file as a sequence of lines.

For each line:
- Strip any trailing `\r`
- Remove comments: if `#` appears, the `#` and all characters after it are ignored
- Trim leading/trailing ASCII whitespace
- If the resulting line is empty, it is ignored

### 4.2 Tokenization

- Tokens are separated by one or more ASCII whitespace characters.
- There is no quoting in v1.
- Directives MUST be parseable by splitting into whitespace-delimited tokens.

## 5) Directives (v1)

Each non-empty line MUST be exactly one directive.

### 5.1 `project <name>`

Declares the project name.

Rules:
- REQUIRED
- MUST appear exactly once
- `<name>` MUST be a single token

### 5.2 `license <identifier>`

Declares a license identifier.

Rules:
- MAY appear multiple times
- `<identifier>` MUST be a single token
- The identifier is intended to be SPDX-style, but v1 only requires it to be a single token

### 5.3 `author <name>`

Declares an author.

Rules:
- MAY appear multiple times
- `<name>` is **free text without quotes**
- `<name>` MUST consist of one or more tokens

### 5.4 `dep <module-ref>`

Declares a dependency.

Rules:
- MAY appear multiple times
- `<module-ref>` MUST be a single token
- Module references are logical identifiers, not filesystem paths

## 6) Dependency references

### 6.1 Grammar

A module reference is either a module identifier alone, or a module identifier plus a version.

`<module-ref> ::= <module> | <module>@<version>`

`<module> ::= domain/path[/subpath]`

`<version> ::= vMAJOR.MINOR.PATCH | branch | commit`

### 6.2 Examples

Valid:
- `dep github.com/org/reponame`
- `dep github.com/org/repo/libxyz@v1.9.0`
- `dep github.com/org/repo@main`

Rules:
- No registries
- No implicit resolution
- No globbing

## 7) Validation rules

- All directives are explicit.
- Repetition is the only form of plurality.
- Duplicate conflicting directives are errors.
- Unknown directives are errors.

### 7.1 Conflicts and duplicates

The following are errors:
- More than one `project` directive
- `project` directive missing

The following are permitted:
- Multiple `license` directives (even if identical)
- Multiple `author` directives (even if identical)
- Multiple `dep` directives (even if identical)

## 8) Valid examples

### 8.1 Minimal

```text
project hello_world
```

### 8.2 With license and authors

```text
# Example project file
project cat_service
license MIT
license Apache-2.0
author Ada Lovelace
author Grace Hopper
```

### 8.3 Dependencies

```text
project compiler_tools
license Apache-2.0
author Purr Team

dep github.com/xyzcorp/compilerlib@v1.9.0
dep github.com/abccorp/compilerlib@main
dep github.com/org/repo/subpkg
```

### 8.4 Order does not matter

```text
# Same as above, different order
license Apache-2.0
project compiler_tools

dep github.com/xyzcorp/compilerlib@v1.9.0
author Purr Team
```

## 9) Error cases (required diagnostics)

An implementation MUST report clear errors for:

- Unknown directive:
  - `projects myapp`
- Missing required directive:
  - (no `project` line)
- Duplicate conflicting directives:
  - Two `project` lines
- Wrong arity:
  - `project` (missing name)
  - `license` (missing identifier)
  - `dep` (missing module-ref)
  - `import x` (unknown directive)
- Invalid tokenization requiring quotes (quotes are not supported in v1):
  - `project "my app"`
- Invalid dependency token shape:
  - `dep` value split across tokens (e.g., `dep github.com/org repo`)
  - `dep ./local/path` (paths are not allowed)

- Project discovery failures:
  - No `.purr` file found when searching upward from the working directory

- Nested project roots:
  - A `.purr` file found inside an existing project root
  - Tooling MUST refuse to proceed

Required error message content for nested project roots:
- MUST mention both detected roots:
  - the outer project root directory
  - the nested `.purr` path
- MUST state that nested `.purr` files are forbidden

Notes:
- The spec does not define an exhaustive validator for `<module>` or `<version>` beyond the token grammar above; however, the implementation MUST treat parsing failures as errors.
