# AGENTS.md - Dumb Jump Project Context

## Project Overview

**Dumb Jump** is an Emacs Lisp package providing "jump to definition" functionality for 60+ programming languages. It uses regex-based search with external tools (ag, rg, git-grep, grep) to find definitions without requiring configuration, indexes (TAGS), or background processes.

- **Language**: Emacs Lisp
- **Main File**: `dumb-jump.el` (5,243 lines)
- **Test File**: `test/dumb-jump-test.el`
- **Min Emacs Version**: 26.1
- **Dependencies**: None

## Architecture

### High-Level Flow
```
User presses M-. (xref-find-definitions)
    → dumb-jump-xref-activate
    → Detect language (major-mode or file extension)
    → Get rules for language from dumb-jump-find-rules
    → Populate regex with symbol
    → Execute search tool (ag/rg/git-grep/grep)
    → Filter/rank results
    → Present to user
```

### Key Data Structures (all in dumb-jump.el)

1. **`dumb-jump-find-rules`** (lines 348-2705)
   - Regex patterns for finding definitions per language
   - Uses `JJJ` as placeholder for symbol, `\\j` for word boundary
   - 433 rule entries total

2. **`dumb-jump-language-file-exts`** (lines 2705-2852)
   - Maps file extensions to language names
   - Includes ag type and rg type for each extension

3. **`dumb-jump-language-aliases-alist`** (lines 3471-3488)
   - Maps Emacs major-mode names to language names
   - e.g., `"emacs-lisp" → "elisp"`

4. **`dumb-jump-language-contexts`** (lines 2867-2903)
   - Context hints for filtering (left/right patterns)

5. **`dumb-jump-language-comments`** (lines 3844-3902)
   - Comment syntax per language for filtering false positives

## File Structure

```
dumb-jump/
├── dumb-jump.el              # Main implementation (5,243 lines)
├── test/
│   ├── dumb-jump-test.el     # ERT tests
│   ├── test-helper.el        # Test utilities
│   └── data/                 # Test fixtures
│       ├── proj1/            # Multi-language test project
│       ├── proj2-elisp/      # Emacs Lisp test project
│       ├── proj3-clj/        # Clojure test project
│       └── multiproj/        # Multi-project scenarios
├── docs/
│   ├── DOCUMENTATION_INDEX.md            # Documentation navigation
│   ├── LANGUAGE_SUPPORT_ARCHITECTURE.md  # Detailed architecture
│   ├── QUICK_REFERENCE_LANGUAGE_SUPPORT.md  # Quick reference
│   └── LANGUAGE_SUPPORT_VISUAL.txt      # Visual diagrams
├── AGENTS.md                 # Agent context (this file)
└── README.md                 # User documentation
```

## Supported Languages (60+)

apex, bash, c/c++, c#, clojure, cobol, coffeescript, common lisp, coq, crystal, dart, d, elisp, elixir, erlang, f#, faust, fennel, fortran, go, groovy, haskell, jai, java, javascript, julia, kotlin, latex, lua, matlab, nim, nix, objective-c, ocaml, odin, openscad, org mode, pascal, pascal, perl, php, protocol buffers, purescript, python, r, racket, ruby, rust, sass, scala, scheme, sml, solidity, sql, swift, systemverilog, tcl, terraform/hcl, typescript, vala, vhdl, zig

## Testing Commands

```bash
# Run all tests (requires Cask)
make test

# Run specific tests by name pattern
make test-this clojure org

# Run in Docker (no Cask needed)
make test-docker
make test-docker EMACS_VERSION=28.2

# Run specific tests in Docker
make test-this-docker clojure org

# Run tests concurrently (requires Go)
make test-concurrent
```

## Adding a New Language

1. **Add file extension mapping** to `dumb-jump-language-file-exts`:
   ```elisp
   (:language "mylang" :ext "mlang" :agtype nil :rgtype nil)
   ```

2. **Add regex rules** to `dumb-jump-find-rules`:
   ```elisp
   (:language "mylang" :type "function"
     :supports ("ag" "grep" "rg" "git-grep")
     :regex "\\bfunction\\s+JJJ\\s*\\("
     :tests ("function myfunc()")
     :not ("functioncall()"))
   ```

3. **Add comment syntax** to `dumb-jump-language-comments`:
   ```elisp
   (:comment "//" :language "mylang")
   ```

4. **Add tests** in `test/dumb-jump-test.el`

## Key Functions

| Function | Line | Purpose |
|----------|------|---------|
| `dumb-jump-get-language` | ~3388 | Detect language from file/mode |
| `dumb-jump-get-rules-by-language` | ~4884 | Get rules for a language |
| `dumb-jump-populate-regex` | ~4546 | Replace JJJ with symbol |
| `dumb-jump-get-language-by-filename` | ~3499 | Map extension to language |

## Regex Placeholder Conventions

- `JJJ` - Placeholder for symbol (replaced with actual name)
- `\\j` - Word boundary (tool-specific: `\b`, `(?![...])`, etc.)
- `\\s` - Whitespace (may become `[[:space:]]` on Windows)

## Code Conventions

- Uses built-in `seq` and `mapcar` for functional operations (`seq-filter`, `seq-map`, `mapcar`, etc.)
- Property lists for data structures (`:keyword value`)
- `defcustom` for user-configurable variables
- Comprehensive test cases in rules (`:tests` for positive, `:not` for negative)
- **Always run `make test` after making changes to verify nothing is broken**

## Common Development Tasks

### Find rules for a language
```elisp
;; In dumb-jump.el, search for:
(:language "python"
```

### Test rules interactively
```elisp
M-x dumb-jump-test-ag-rules     ; Test ag rules
M-x dumb-jump-test-rg-rules     ; Test rg rules
M-x dumb-jump-test-grep-rules   ; Test grep rules
```

### Debug a jump
1. `M-x set-variable dumb-jump-debug t`
2. Try to jump
3. Check `*Messages*` buffer

## Documentation Reference

- **Quick start**: `README.md`
- **Architecture details**: `docs/LANGUAGE_SUPPORT_ARCHITECTURE.md`
- **Quick reference**: `docs/QUICK_REFERENCE_LANGUAGE_SUPPORT.md`
- **Navigation**: `docs/DOCUMENTATION_INDEX.md`
