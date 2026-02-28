# Dumb-Jump Language Support - Quick Reference Guide

## The 5 Key Data Structures

| Structure | Location | Purpose | # Entries |
|-----------|----------|---------|-----------|
| `dumb-jump-find-rules` | Lines 348-2705 | Regex patterns for finding definitions | 433 |
| `dumb-jump-language-file-exts` | Lines 2705-2852 | File extension → language mapping | ~120 |
| `dumb-jump-language-aliases-alist` | Lines 3471-3488 | Major-mode → language aliases | 5 |
| `dumb-jump-language-contexts` | Lines 2867-2903 | Context hints for filtering | ~20 |
| `dumb-jump-language-comments` | Lines 3844-3902 | Comment syntax per language | ~50 |

---

## Adding a New Language: 5-Minute Checklist

### 1. File Extension Mapping
```elisp
; Edit: dumb-jump-language-file-exts (line 2705)
(:language "LANGUAGE" :ext "EXT" :agtype "TYPE" :rgtype "TYPE")
```

### 2. Language Rules  
```elisp
; Edit: dumb-jump-find-rules (line 348)
; Need AT LEAST these rules per language:
; - One for function definitions
; - One for variable definitions

(:language "LANGUAGE" :type "function"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "REGEX_WITH_JJJ"
  :tests ("test case 1" "test case 2")
  :not ("negative test 1"))

(:language "LANGUAGE" :type "variable"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "REGEX_WITH_JJJ"
  :tests ("test case 1" "test case 2")
  :not ("negative test 1"))
```

### 3. Comment Syntax (Optional)
```elisp
; Edit: dumb-jump-language-comments (line 3844)
(:comment "COMMENT_CHAR" :language "LANGUAGE")
```

### 4. Test Rules
```bash
M-x dumb-jump-test-ag-rules
M-x dumb-jump-test-rg-rules
M-x dumb-jump-test-grep-rules
M-x dumb-jump-test-git-grep-rules
```

### 5. Add Tests (Recommended)
```bash
cd /path/to/dumb-jump
make test-this LANGUAGE
```

---

## Regex Pattern Placeholders

| Placeholder | Meaning | Replaced With |
|------------|---------|----------------|
| `JJJ` | Symbol being searched | The actual symbol name (regexp-quoted) |
| `\\j` | Word boundary | Tool-specific: `\b`, `(?![...])`, `($\|[^...])`, etc. |
| `\\s` | Whitespace | Stays as `\s` or replaced with `[[:space:]]` on Windows |

---

## Rule Definition Anatomy

```elisp
(:language "rust"           ; Language name (REQUIRED)
  :type "variable"         ; Type: function, variable, type, class, etc. (REQUIRED)
  :supports ("ag" "rg" "grep" "git-grep")  ; Search tools that support it (REQUIRED)
  :regex "\\blet\\s+JJJ"   ; Regex pattern with JJJ placeholder (REQUIRED)
  :tests ("let x = 42")    ; Positive test cases - MUST MATCH (REQUIRED)
  :not ("let_x = 42")      ; Negative test cases - MUST NOT MATCH (REQUIRED)
  :tags ("async"))         ; Optional variant tags
```

---

## Key Functions Quick Map

### Language Detection
```elisp
(dumb-jump-get-language "file.rs")          → "rust"
(dumb-jump-get-language-by-filename "file.js") → "javascript"
(dumb-jump-get-language-from-aliases "emacs-lisp") → "elisp"
```

### Rule Management
```elisp
(dumb-jump-get-rules-by-language "rust" 'ag) → List of rules
(dumb-jump-get-contextual-regexes "rust" "function" 'ag) → List of regexes
(dumb-jump-populate-regex TEMPLATE "symbol" 'ag) → Ready-to-use regex
```

### Extension/Type Info
```elisp
(dumb-jump-get-file-exts-by-language "rust") → ("rs")
(dumb-jump-get-ag-type-by-language "rust") → ("rust")
(dumb-jump-get-rg-type-by-language "rust") → ("rust")
```

### Comments
```elisp
(dumb-jump-get-comment-by-language "python") → ("#")
(dumb-jump-filter-no-start-comments RESULTS "python") → Filtered results
```

---

## Testing Your Rules

### Built-in Test Functions (Emacs)
```elisp
M-x dumb-jump-test-ag-rules       ; Test all ag rules, show passed count
M-x dumb-jump-test-ag-rules RET t ; Test all ag rules, show failures
M-x dumb-jump-test-rg-rules       ; Test all rg rules
M-x dumb-jump-test-grep-rules     ; Test all grep rules
M-x dumb-jump-test-git-grep-rules ; Test all git-grep rules
```

### Command Line Tests
```bash
make test                    # Run all tests
make test-this language      # Run tests matching "language"
make test-concurrent         # Run tests in parallel
make test-docker             # Run in Docker container
```

---

## Supported Languages (54+)

```
Functional: elisp, commonlisp, racket, scheme, janet, clojure
Imperative: c/c++, c#, java, csharp, vala, objective-c, pascal
Scripting:  python, ruby, perl, php, shell, tcl
Web:        javascript, typescript, coffeescript
Compiled:   rust, go, haskell, scala, kotlin, swift
ML-Family:  ocaml, fsharp, sml, coq
Markup:     tex, latex, org-mode
Other:      apex, bash, cobol, crystal, dart, elixir, erlang, faust, fennel,
            fortran, groovy, jai, julia, lua, matlab, nim, nix, odin, 
            openscad, protobuf, r, solidity, sql, systemverilog, terraform,
            vhdl, zig, scss, sass
```

---

## PureScript Example

```elisp
; 1. Add extension mapping
(:language "purescript" :ext "purs" :agtype nil :rgtype nil)

; 2. Add function rule
(:language "purescript" :type "function"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "^JJJ\\s*::"
  :tests ("test :: Int -> String")
  :not ("other :: Int -> String"))

; 3. Add variable rule
(:language "purescript" :type "variable"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "\\blet\\s+JJJ\\s*="
  :tests ("let test = 42")
  :not ("let testing = 42"))

; 4. Add comment syntax
(:comment "--" :language "purescript")

; 5. Test
M-x dumb-jump-test-ag-rules
```

---

## Common Regex Patterns

### Function Definitions
```
; Simple form
\\bfn\\s+JJJ\\s*\\(

; With type annotations  
^JJJ\\s*::\\s*

; Class/struct method
\\bdef\\s+JJJ\\s*\\(
```

### Variable Definitions
```
; Assignment
\\bJJJ\\s*=

; Let binding
\\blet\\s+JJJ\\s*=

; Function parameter
\\(\\s*JJJ\\s*[:)]
```

### Type Definitions
```
; Struct/class
\\b(struct|class)\\s+JJJ\\b

; Type alias
\\btype\\s+JJJ\\s*=

; Interface
\\b(interface|protocol)\\s+JJJ\\b
```

---

## Word Boundary Special Handling

Dumb-jump supports tool-specific word boundaries via `\\j`:

### ag (The Silver Searcher)
```
Replaces \\j with: (?![a-zA-Z0-9\\?\\*-])
Example: fn\\s+test(?![a-zA-Z0-9\\?\\*-])
```

### rg (ripgrep)
```
Replaces \\j with: ($|[^a-zA-Z0-9\\?\\*-])
Example: fn\\s+test($|[^a-zA-Z0-9\\?\\*-])
```

### grep (GNU grep)
```
Replaces \\j with: ($|[^a-zA-Z0-9\\?\\*-])
Example: fn\\s+test($|[^a-zA-Z0-9\\?\\*-])
```

### git-grep
```
Replaces \\j with: ($|[^a-zA-Z0-9\\?\\*-])
Example: fn\\s+test($|[^a-zA-Z0-9\\?\\*-])
```

---

## File Structure for Adding Language

```
dumb-jump.el                          ← Main file with data structures
├─ Line 348-2705: dumb-jump-find-rules
├─ Line 2705-2852: dumb-jump-language-file-exts
├─ Line 2867-2903: dumb-jump-language-contexts
├─ Line 3471-3488: dumb-jump-language-aliases-alist
└─ Line 3844-3902: dumb-jump-language-comments

test/
├─ dumb-jump-test.el                 ← Add ERT tests here
├─ data/
│   └─ proj-YOUR-LANGUAGE/           ← Add test fixtures (optional)
│       ├─ src/
│       │   ├─ file1.EXT
│       │   └─ file2.EXT
│       └─ .dumbjump
```

---

## Debugging a Language Rule

### 1. See what regex is generated
```elisp
(dumb-jump-populate-regex "\\bfn\\s+JJJ\\s*\\(" "test" 'ag)
→ "\\bfn\\s+test(?![a-zA-Z0-9\\?\\*-])\\s*\\("
```

### 2. Test manually with ag/rg
```bash
ag "fn\s+test\s*\(" /path/to/project
rg "fn\s+test\s*\(" /path/to/project
grep -r "fn\s+test\s*(" /path/to/project
```

### 3. Enable debug mode in Emacs
```elisp
(setq dumb-jump-debug t)
; Then jump - check *Messages* buffer
(setq dumb-jump-debug nil)
```

### 4. Check rule test cases
```elisp
M-x dumb-jump-test-ag-rules RET t
; Shows which test cases failed and why
```

---

## Performance Tips

1. **Searcher Performance**: ag > rg > grep (when available)
2. **Limit Project Scope**: Use `.dumbjump` file to exclude directories
3. **Use ag or rg**: Much faster than grep for large codebases
4. **Specific Rules**: More specific rules = fewer false positives = faster result filtering

---

## Extension Type Reference

### ag (Silver Searcher) File Types
```
c, cc, cpp, csharp, delphi, elixir, elisp, erlang, fortran, go, groovy, haskell,
java, javascript, julia, kotlin, lisp, lua, matlab, nim, nix, objc, ocaml, perl,
php, protobuf, python, r, racket, ruby, rust, sass, scala, scheme, shell, sml,
sql, swift, tcl, tex, typescript, vala, verilog
```

### rg (ripgrep) File Types
```
agda, bash, c, clojure, coffeescript, cpp, csharp, crystal, css, dart, delphi,
elixir, elisp, erlang, fortran, fsharp, go, groovy, haskell, html, java,
javascript, json, julia, kotlin, latex, lisp, lua, markdown, matlab, nim, nix,
objc, ocaml, org, pascal, perl, php, protobuf, py, python, r, racket, rst,
ruby, rust, sass, scala, scheme, shell, sml, sql, swift, tex, texinfo,
toml, tsx, typescript, vala, verilog, vhdl, vim, wix, xml, zig
```

---

## Customization Examples

```elisp
; Use ag instead of default searcher
(setq dumb-jump-prefer-searcher 'ag)

; Only search for functions
(setq dumb-jump-functions-only t)

; Don't filter results by context
(setq dumb-jump-ignore-context t)

; Add custom rules
(add-to-list 'dumb-jump-find-rules
  '(:language "mylang" :type "function"
    :supports ("ag" "grep" "rg" "git-grep")
    :regex "\\bfunc\\s+JJJ\\s*\\("
    :tests ("func test()" "func foo(){")
    :not ("func testing()")))

; Add custom file extension
(add-to-list 'dumb-jump-language-file-exts
  '(:language "mylang" :ext "ml" :agtype nil :rgtype nil))

; Add comment syntax
(add-to-list 'dumb-jump-language-comments
  '(:comment "//" :language "mylang"))
```

---

## Architecture Quick View

```
File with symbol "test"
      ↓
dumb-jump-get-language() → "rust"
      ↓
dumb-jump-get-rules-by-language("rust", searcher) → Rules
      ↓
dumb-jump-get-contextual-regexes() → Filtered rules
      ↓
dumb-jump-populate-regexes(symbol, rules, searcher) → Ready-to-use regexes
      ↓
Generate search command (ag/rg/grep/git-grep)
      ↓
Execute and parse results
      ↓
Filter (remove comments, rank by heuristics)
      ↓
Show to user
```

---

## Notes

- All `dumb-jump-*` customizable variables are in `defcustom` blocks
- Rules are tested both positive (`:tests`) and negative (`:not`) 
- `JJJ` placeholder is replaced with `(regexp-quote symbol)`
- `\\j` is replaced based on search tool being used
- Each rule can support multiple search tools
- Multiple rules per language type (function, variable, etc.) are allowed
- Tags (`:tags`) can distinguish rule variants (e.g., "es6" for modern JavaScript)

