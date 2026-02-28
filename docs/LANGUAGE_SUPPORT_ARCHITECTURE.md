# Dumb-Jump Codebase Overview: Language Support Architecture

## Project Purpose & Summary

**Dumb Jump** is an Emacs package that provides "jump to definition" functionality for 50+ programming languages without requiring configuration, indexes (TAGS), or background processes. It works by:

1. Using regex patterns to search for definitions
2. Leveraging external search tools (ag, rg, git-grep, or grep)
3. Applying heuristics to rank and select the best match

The project is 5,019 lines of Emacs Lisp (main file) + 2,064 lines of tests.

---

## Overall Project Structure

```
dumb-jump/
├── dumb-jump.el           # Main 5,019-line implementation file
├── test/
│   ├── dumb-jump-test.el  # 2,064 lines of tests
│   ├── test-helper.el     # Test utilities
│   └── data/              # Test fixtures
│       ├── proj1/         # Test project with multiple language samples
│       ├── proj2-elisp/   # Emacs Lisp test project
│       ├── proj3-clj/     # Clojure test project
│       └── multiproj/     # Multi-project test scenarios
├── README.md              # Documentation (353 lines)
└── Makefile               # Test orchestration
```

---

## How Languages Are Supported

### High-Level Architecture

Dumb-jump supports languages through **three main data structures**:

1. **`dumb-jump-find-rules`** - Regular expressions for finding definitions
2. **`dumb-jump-language-file-exts`** - File extension to language mapping
3. **`dumb-jump-language-aliases-alist`** - Mode name aliases (e.g., "emacs-lisp" → "elisp")

Plus supporting structures:
- **`dumb-jump-language-contexts`** - Context hints for filtering results
- **`dumb-jump-language-comments`** - Comment syntax per language

### 1. Core Rules Definition: `dumb-jump-find-rules`

**Location**: Lines 348-2705 in dumb-jump.el

**Purpose**: Defines regex patterns to find definitions for each language

**Structure**: Each rule is a property list with:
```elisp
(:language "LANGUAGE-NAME"
  :type "TYPE"                    ; "function", "variable", "type", etc.
  :supports ("ag" "grep" "rg" "git-grep")  ; Which search tools support it
  :regex "REGEX-PATTERN"          ; Pattern with JJJ placeholder for symbol
  :tests (...)                    ; Positive test cases
  :not (...))                     ; Negative test cases (false positives)
```

**Key Patterns**:
- `JJJ` - Placeholder for the symbol being searched (replaced with actual name)
- `\\j` - Word boundary (tool-specific: \b, (?![...]), ($|[^...]), etc.)
- `\\s` - Whitespace (may be replaced with [[:space:]] on Windows)

**Example: Rust Variable Definition**
```elisp
(:language "rust" :type "variable"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "\\blet\\s+(\\([^=\\n]*)?(mut\\s+)?JJJ([^=\\n]*\\))?(:\\s*[^=\\n]+)?\\s*=\\s*[^=\\n]+"
  :tests ("let test = 1234;"
          "let test: u32 = 1234;"
          "let test: Vec<u32> = Vec::new();"
          "let (a, test, b) = (1, 2, 3);"))
```

**Statistics**:
- **433 language rule entries** defined in `dumb-jump-find-rules`
- Multiple rules per language (different types: functions, variables, types, etc.)
- Rules organize languages: elisp, commonlisp, racket, scheme, janet, c++, clojure, etc.

### 2. File Extension Mapping: `dumb-jump-language-file-exts`

**Location**: Lines 2705-2852 in dumb-jump.el

**Purpose**: Maps file extensions to languages and search tool types

**Structure**: Each entry is a property list:
```elisp
(:language "LANGUAGE-NAME"
  :ext "FILE-EXTENSION"
  :agtype "AG-TYPE"               ; Type for 'ag' search tool (or nil)
  :rgtype "RG-TYPE")              ; Type for 'rg' search tool (or nil)
```

**Example Mappings**:
```elisp
(:language "javascript" :ext "js"   :agtype "js" :rgtype "js")
(:language "javascript" :ext "jsx"  :agtype "js" :rgtype "js")
(:language "typescript" :ext "ts"   :agtype "ts" :rgtype "ts")
(:language "python"     :ext "py"   :agtype "python" :rgtype "py")
(:language "rust"       :ext "rs"   :agtype "rust" :rgtype "rust")
(:language "haskell"    :ext "hs"   :agtype "haskell" :rgtype "haskell")
```

**Supported Extensions**: ~120 entries covering all 50+ languages

### 3. Language Mode Aliases: `dumb-jump-language-aliases-alist`

**Location**: Lines 3471-3488 in dumb-jump.el

**Purpose**: Maps Emacs major mode names to dumb-jump language names

**Current Aliases**:
```elisp
(("sh" . "shell")
 ("cperl" . "perl")
 ("octave" . "matlab")
 ("emacs-lisp" . "elisp")
 ("R" . "r"))
```

### 4. Language Contexts: `dumb-jump-language-contexts`

**Location**: Lines 2867-2903 in dumb-jump.el

**Purpose**: Provide context hints to filter results (e.g., "if something ends with (" it's likely a function call)

**Example**:
```elisp
(:language "javascript" :type "function" :left nil :right "^(")
(:language "javascript" :type "variable" :left "($" :right nil)
(:language "elisp" :type "function" :left "($" :right nil)
```

### 5. Language Comments: `dumb-jump-language-comments`

**Location**: Lines 3844-3902 in dumb-jump.el

**Purpose**: Define comment syntax for filtering out results in comments

**Example**:
```elisp
(:comment "//" :language "c++")
(:comment "//" :language "javascript")
(:comment "#" :language "python")
(:comment ";" :language "elisp")
```

---

## Complete List of Supported Languages

(54+ languages as of current version)

```
apex, bash, c/c++, c#, clojure, cobol, coffeescript, common lisp, coq, crystal,
dart, elisp, elixir, erlang, f#, faust, fennel, fortran, go, groovy, haskell, jai,
java, javascript, julia, kotlin, latex, lua, matlab, nim, nix, objective-c, ocaml,
odin, openscad, org mode, pascal, perl, php, protocol buffers, python, r, racket,
ruby, rust, sass, scala, scheme, sml, solidity, sql, swift, systemverilog, tcl,
terraform/hcl, typescript, vala, vhdl, zig
```

---

## Language Detection & Resolution Flow

### 1. Get Language for Current File

**Function**: `dumb-jump-get-language` (line 3388)

**Algorithm**:
1. Check if major-mode maps to a language via `dumb-jump-get-language-from-mode`
2. If not, get language by file extension via `dumb-jump-get-language-by-filename`
3. For org-mode files, check embedded language blocks
4. Return the detected language string

**Helper Functions**:
- `dumb-jump-get-language-from-mode` (3494) - Extract language from major-mode
- `dumb-jump-get-language-from-aliases` (3485) - Resolve aliases
- `dumb-jump-get-language-by-filename` (3499) - Match file extension
- `dumb-jump-get-mode-base-name` (3490) - Get mode name without "-mode" suffix

### 2. Get Rules for Language

**Function**: `dumb-jump-get-rules-by-language` (line 4884)

```elisp
(defun dumb-jump-get-rules-by-language (language searcher)
  "Return a list of rules for the LANGUAGE by SEARCHER."
  (let* ((searcher-str (cond ((eq 'git-grep searcher) "git-grep")
                             ((eq 'rg searcher) "rg")
                             ((eq 'ag searcher) "ag")
                             (t "grep")))
         (results (--filter (and
                             (string= (plist-get it ':language) language)
                             (member searcher-str (plist-get it ':supports)))
                            dumb-jump-find-rules)))
    (if dumb-jump-functions-only
        (--filter (string= (plist-get it ':type) "function") results)
      results)))
```

**Returns**: List of rules for the given language that the searcher supports

### 3. Get Contextual Regexes

**Function**: `dumb-jump-get-contextual-regexes` (line 4529)

**Purpose**: Filter rules by context type (function, variable, etc.) if available

```elisp
(defun dumb-jump-get-contextual-regexes (lang ctx-type searcher)
  "Get list of search regular expressions by LANG and CTX-TYPE."
  ;; Filter rules by context type if ctx-type is specified
  ;; Otherwise return all rules for the language
```

### 4. Populate Regex with Symbol

**Function**: `dumb-jump-populate-regex` (line 4546)

**Purpose**: Replace placeholders in regex pattern with actual search term and tool-specific syntax

**Transformations**:
- Replace `JJJ` with the symbol being searched (regexp-quoted)
- Replace `\\j` with tool-specific word boundary regex
- Replace `\\s` with `[[:space:]]` on Windows if needed

**Example**:
- Template: `\\bfn\\s+JJJ\\s*\\(`
- Symbol: `test_func`
- Result (ag): `\bfn\s+test_func(?![a-zA-Z0-9\?\*-])\s*\(`

---

## How to Add Support for a New Language

### Step 1: Add File Extension Mapping

**Edit**: `dumb-jump-language-file-exts` (line 2705)

**Add entries like**:
```elisp
(:language "purescript"  :ext "purs"  :agtype nil  :rgtype nil)
```

**Notes**:
- Find the agtype/rgtype values by checking ag and rg documentation
- Set to nil if the tool doesn't support the language
- Multiple extensions can map to the same language

### Step 2: Add Language Rules

**Edit**: `dumb-jump-find-rules` (line 348)

**Add rule entries like**:
```elisp
(:language "purescript" :type "function"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "^[a-z_][a-zA-Z0-9_]*\\s*::\\s*.*->.*\n^JJJ\\s*:.*"
  :tests ("test :: Int -> String\ntest x = show x")
  :not ("other :: Int -> String"))

(:language "purescript" :type "variable"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "\\blet\\s+JJJ\\s*="
  :tests ("let test = 42")
  :not ("let testing = 42"))
```

**Rule Creation Guidelines**:
- Regex must use `JJJ` as placeholder for the symbol
- Use `\\j` for word boundaries instead of `\\b`
- Include `:tests` - positive test cases that MUST match
- Include `:not` - negative test cases that MUST NOT match
- Support all major searchers if possible ("ag", "grep", "rg", "git-grep")
- Add multiple rules for different definition types

### Step 3: Add Comment Syntax (Optional)

**Edit**: `dumb-jump-language-comments` (line 3844)

**Add entries like**:
```elisp
(:comment "--" :language "purescript")
(:comment "{-" :language "purescript")
```

### Step 4: Add Mode Alias (If Needed)

**Edit**: `dumb-jump-language-aliases-alist` (line 3471)

**Add if the major-mode name differs from language name**:
```elisp
("purescript" . "purescript")
```

### Step 5: Add Context Rules (Optional)

**Edit**: `dumb-jump-language-contexts` (line 2867)

**Add to improve filtering**:
```elisp
(:language "purescript" :type "function" :left nil :right "^(")
(:language "purescript" :type "variable" :left nil :right "^\\.")
```

### Step 6: Test Rules

**Run tests**:
```bash
# Test ag rules
M-x dumb-jump-test-ag-rules

# Test rg rules  
M-x dumb-jump-test-rg-rules

# Test grep rules
M-x dumb-jump-test-grep-rules

# Test git-grep rules
M-x dumb-jump-test-git-grep-rules
```

**Or from command line**:
```bash
cd /path/to/dumb-jump
make test
make test-this purescript
```

### Step 7: Add ERT Tests (Recommended)

**Add tests** in `test/dumb-jump-test.el`:
```elisp
(ert-deftest dumb-jump-purescript-ext-test ()
  (let ((result (dumb-jump-get-language-by-filename "blah.purs")))
    (should (string= result "purescript"))))
```

---

## JavaScript/TypeScript Support Example

### File Extensions
```elisp
(:language "javascript" :ext "js"   :agtype "js" :rgtype "js")
(:language "javascript" :ext "jsx"  :agtype "js" :rgtype "js")
(:language "javascript" :ext "vue"  :agtype "js" :rgtype "js")
(:language "typescript" :ext "ts"   :agtype "ts" :rgtype "ts")
(:language "typescript" :ext "tsx"  :agtype "ts" :rgtype "ts")
```

### Rules (ES6 Example)
```elisp
(:language "javascript" :tags ("es6") :type "function"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "\\bJJJ\\s*[=:]\\s*\\([^\\)]*\\)\\s*=>"
  :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))

(:language "javascript" :type "function"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "function\\s*JJJ\\s*\\("
  :tests ("function test()" "function test ())

(:language "javascript" :type "variable"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "\\s*\\bJJJ\\s*=[^=\\n]+"
  :tests ("test = 1234" "const test = props =>")
  :not ("if (test === 1234)"))
```

### Context Rules
```elisp
(:language "javascript" :type "function" :left nil :right "^(")
(:language "javascript" :type "variable" :left "($" :right nil)
(:language "typescript" :type "function" :left nil :right "^(")
```

### Comment Syntax
```elisp
(:comment "//" :language "javascript")
(:comment "//" :language "typescript")
```

---

## Testing Infrastructure

### Built-in Test Functions

**Location**: Lines 3146-3241 in dumb-jump.el

**Available Functions**:
- `dumb-jump-test-grep-rules` - Test all grep rules
- `dumb-jump-test-ag-rules` - Test all ag rules
- `dumb-jump-test-rg-rules` - Test all rg rules
- `dumb-jump-test-git-grep-rules` - Test all git-grep rules

**Usage**:
```elisp
M-x dumb-jump-test-ag-rules          ; See passed/failed
M-x dumb-jump-test-ag-rules RET t    ; Show which tests failed with details
```

### ERT Test Suite

**Location**: test/dumb-jump-test.el (2,064 lines)

**Test Categories**:
- Language detection tests
- Extension mapping tests
- Rule generation tests (grep, ag, rg, git-grep)
- Command generation tests
- Result parsing tests
- Configuration tests

**Running Tests**:
```bash
make test                          # Run all tests
make test-this purescript          # Run tests matching "purescript"
make test-concurrent               # Run tests in parallel
make test-docker                   # Run in Docker container
make test-docker EMACS_VERSION=29.4
```

---

## Key Functions Reference

### Language Detection
- `dumb-jump-get-language` - Get language for current file
- `dumb-jump-get-language-from-mode` - Extract from major-mode name
- `dumb-jump-get-language-by-filename` - Match file extension
- `dumb-jump-get-language-from-aliases` - Resolve mode aliases

### Rule Management
- `dumb-jump-get-rules-by-language` - Get all rules for a language
- `dumb-jump-get-contextual-regexes` - Get rules filtered by type
- `dumb-jump-populate-regex` - Replace JJJ and tool-specific patterns
- `dumb-jump-populate-regexes` - Batch populate multiple regexes

### File Extensions & Types
- `dumb-jump-get-file-exts-by-language` - Get file extensions for language
- `dumb-jump-get-ag-type-by-language` - Get ag search type for language
- `dumb-jump-get-rg-type-by-language` - Get rg search type for language

### Comment Handling
- `dumb-jump-get-comment-by-language` - Get comment syntax for language
- `dumb-jump-filter-no-start-comments` - Filter results in comments

---

## Customization Points

### User-Configurable Variables

```elisp
(defcustom dumb-jump-find-rules ...)           ; Add/modify rules
(defcustom dumb-jump-language-file-exts ...)   ; Add/modify extensions
(defcustom dumb-jump-language-aliases-alist .) ; Add/modify aliases
(defcustom dumb-jump-language-contexts ...)    ; Add/modify context hints
(defcustom dumb-jump-language-comments ...)    ; Add/modify comment syntax
(defcustom dumb-jump-functions-only nil)       ; Search only functions
(defcustom dumb-jump-ignore-context nil)       ; Ignore context filtering
```

### Search Tool Configuration

```elisp
(setq dumb-jump-prefer-searcher 'ag)           ; Prefer ag over rg/grep
(setq dumb-jump-ag-search-args "")             ; Custom ag arguments
(setq dumb-jump-rg-search-args "--pcre2")      ; Custom rg arguments
```

---

## What Would Need to Be Added for PureScript Support

### 1. File Extension Mapping
```elisp
(:language "purescript" :ext "purs" :agtype nil :rgtype nil)
```

### 2. Language Rules

Minimum needed:
```elisp
; Function definitions
(:language "purescript" :type "function"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "^JJJ\\s*::"
  :tests ("test :: Int -> String")
  :not ("other :: Int -> String"))

; Top-level let bindings
(:language "purescript" :type "variable"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "\\blet\\s+JJJ\\s*="
  :tests ("let test = 42")
  :not ("let testing = 42"))

; Where clause bindings
(:language "purescript" :type "variable"
  :supports ("ag" "grep" "rg" "git-grep")
  :regex "\\bwhere\\s+JJJ\\s*="
  :tests ("where test = 42")
  :not ("where testing = 42"))
```

### 3. Comment Syntax
```elisp
(:comment "--" :language "purescript")
(:comment "{-" :language "purescript")
```

### 4. Context Rules (Optional)
```elisp
(:language "purescript" :type "function" :left nil :right "\\s+::")
(:language "purescript" :type "variable" :left nil :right "\\s*=")
```

### 5. Tests
```bash
; Run rule tests to validate
M-x dumb-jump-test-ag-rules
M-x dumb-jump-test-rg-rules
```

---

## Architecture Summary

```
User presses M-.
    ↓
dumb-jump-get-results
    ↓
dumb-jump-fetch-file-results
    ↓
dumb-jump-get-language(current-file)
    ├─ Try major-mode aliases → Language name
    └─ Try file extension → Language name
    ↓
dumb-jump-fetch-results
    ↓
dumb-jump-get-rules-by-language(language, searcher)
    ↓
dumb-jump-get-contextual-regexes(language, ctx-type, searcher)
    ↓
dumb-jump-populate-regexes(symbol, regexes, searcher)
    ├─ Replace JJJ with symbol
    ├─ Replace \\j with tool-specific word boundary
    └─ Replace \\s with [[:space:]] if needed
    ↓
Generate command (dumb-jump-generate-ag-command, etc.)
    ↓
Execute search tool (ag/rg/git-grep/grep)
    ↓
Parse results
    ↓
Filter/rank results using heuristics
    ↓
Show to user (popup/helm/ivy/xref)
```

---

## Performance Considerations

1. **Searcher Performance**: ag > rg > grep (when available)
2. **Regex Compilation**: Happens per-search, not cached
3. **Rule Matching**: Linear search through all rules for language
4. **Search Scope**: Limited by .dumbjump config and project root detection
5. **Filtering**: Post-search filtering by comments, context, and heuristics

---

## Code Quality & Testing

- **5,019 lines** of well-organized Emacs Lisp
- **2,064 lines** of comprehensive tests
- **433 rule entries** with positive and negative test cases
- **8 major search tools** tested (grep, gnu-grep, ag, rg, git-grep, git-grep-plus-ag, ugrep)
- **Multiple test runners**: ERT, make targets, Docker support
- **CI/CD**: GitHub Actions on Linux and macOS

