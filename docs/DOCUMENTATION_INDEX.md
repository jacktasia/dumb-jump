# Dumb-Jump Language Support Documentation Index

This directory contains comprehensive documentation about how dumb-jump implements language support and how to add new languages.

## Documentation Files

### 1. **LANGUAGE_SUPPORT_ARCHITECTURE.md** (604 lines)
   **For**: Understanding the complete architecture and implementation details
   
   **Contains**:
   - Complete project overview (structure, purpose, scope)
   - Detailed explanation of 5 core data structures
   - List of all 54+ supported languages
   - Language detection and resolution algorithms
   - Comprehensive step-by-step guide for adding new languages
   - JavaScript/TypeScript support as a detailed example
   - Testing infrastructure overview
   - Key functions reference with line numbers
   - Customization points for users
   - PureScript support requirements
   - Complete architecture diagrams
   - Performance considerations
   
   **Best For**: Getting a complete understanding of how the system works

### 2. **LANGUAGE_SUPPORT_VISUAL.txt** (340 lines)
   **For**: Visual learners who prefer diagrams and flowcharts
   
   **Contains**:
   - ASCII diagrams of all 5 data structures
   - Language detection flow diagram
   - Rule resolution flow diagram
   - Regex population flow diagram
   - Supported languages in grid format
   - Step-by-step visual guide for adding languages
   - Key functions in diagram format
   - Code statistics and metrics
   - Rule structure examples with JavaScript
   - Complete 9-phase workflow diagram
   
   **Best For**: Quick visual understanding of data flow and processes

### 3. **QUICK_REFERENCE_LANGUAGE_SUPPORT.md** (405 lines)
   **For**: Quick lookup and implementation guide
   
   **Contains**:
   - 5-minute language addition checklist
   - Quick reference tables for all 5 data structures
   - Regex pattern placeholder reference
   - Rule definition anatomy with annotations
   - Key functions quick map
   - Testing your rules guide
   - PureScript implementation example (complete)
   - Common regex patterns for function/variable/type definitions
   - Word boundary special handling per search tool
   - Debugging guide with concrete examples
   - Performance tips
   - Customization examples with code
   - Architecture quick view
   
   **Best For**: Developers actively adding language support

### 4. **README.md** (353 lines)
   **For**: Project overview and user guide
   
   **Contains**:
   - Feature overview
   - List of supported languages
   - Installation instructions
   - Usage guide
   - Configuration options
   - Debugging instructions
   - Contributing guidelines
   - Alternatives
   
   **Best For**: Users and newcomers to the project

## Quick Navigation

### I want to...

**Understand how dumb-jump works:**
→ Start with: `LANGUAGE_SUPPORT_ARCHITECTURE.md`

**Add support for a new language:**
→ Start with: `QUICK_REFERENCE_LANGUAGE_SUPPORT.md` 
→ Then reference: `LANGUAGE_SUPPORT_ARCHITECTURE.md` (detailed sections)

**Understand data flow visually:**
→ See: `LANGUAGE_SUPPORT_VISUAL.txt`

**Implement PureScript support:**
→ See: Section "What Would Need to Be Added for PureScript Support" in `LANGUAGE_SUPPORT_ARCHITECTURE.md`
→ Or: "PureScript Example" in `QUICK_REFERENCE_LANGUAGE_SUPPORT.md`

**Look up a specific function:**
→ See: "Key Functions Reference" in `LANGUAGE_SUPPORT_ARCHITECTURE.md`
→ Or: "Key Functions Quick Map" in `QUICK_REFERENCE_LANGUAGE_SUPPORT.md`

**Understand language detection:**
→ See: "Language Detection & Resolution Flow" in `LANGUAGE_SUPPORT_ARCHITECTURE.md`
→ Or: "LANGUAGE DETECTION FLOW" diagram in `LANGUAGE_SUPPORT_VISUAL.txt`

**Add regex rules for a language:**
→ See: "How to Add Support for a New Language > Step 2" in `LANGUAGE_SUPPORT_ARCHITECTURE.md`
→ Or: "Common Regex Patterns" in `QUICK_REFERENCE_LANGUAGE_SUPPORT.md`

**Test your language rules:**
→ See: "Testing Infrastructure" in `LANGUAGE_SUPPORT_ARCHITECTURE.md`
→ Or: "Testing Your Rules" in `QUICK_REFERENCE_LANGUAGE_SUPPORT.md`

**Understand how JavaScript/TypeScript support works:**
→ See: "JavaScript/TypeScript Support Example" in `LANGUAGE_SUPPORT_ARCHITECTURE.md`

**Debug a language rule:**
→ See: "Debugging a Language Rule" in `QUICK_REFERENCE_LANGUAGE_SUPPORT.md`

## File Locations in Codebase

All language support configuration is in the main implementation file:

- **File**: `dumb-jump.el` (5,019 lines)
  - Lines 348-2705: `dumb-jump-find-rules` - Core regex patterns
  - Lines 2705-2852: `dumb-jump-language-file-exts` - File extension mapping
  - Lines 2867-2903: `dumb-jump-language-contexts` - Context hints
  - Lines 3471-3488: `dumb-jump-language-aliases-alist` - Mode aliases
  - Lines 3844-3902: `dumb-jump-language-comments` - Comment syntax
  - Lines 3146-3241: Rule testing functions
  - Lines 3388-4898: Language detection and rule management functions
  - Lines 4529-4627: Regex population and search command generation

- **Tests**: `test/dumb-jump-test.el` (2,064 lines)
- **Test Fixtures**: `test/data/` directory

## Key Concepts

### The 5 Core Data Structures

1. **dumb-jump-find-rules** (433 entries)
   - Regex patterns for finding definitions
   - Multiple rules per language (function, variable, type, etc.)
   - Each rule includes positive and negative test cases

2. **dumb-jump-language-file-exts** (~120 entries)
   - Maps file extensions to languages
   - Includes search tool type information (ag, rg)

3. **dumb-jump-language-aliases-alist** (5 entries)
   - Maps Emacs major-mode names to dumb-jump language names
   - Example: "emacs-lisp" → "elisp"

4. **dumb-jump-language-contexts** (~20 entries)
   - Context hints for better filtering (left/right patterns)
   - Used to distinguish functions from variables

5. **dumb-jump-language-comments** (~50 entries)
   - Comment syntax per language
   - Used to filter out false positives in comments

### Language Support Workflow

```
User presses M-.
    ↓
Detect language (via major-mode or file extension)
    ↓
Get rules for language from dumb-jump-find-rules
    ↓
Filter rules by context type if available
    ↓
Populate regex patterns with symbol being searched
    ↓
Generate and execute search command (ag/rg/grep/git-grep)
    ↓
Parse and filter results (remove comments, apply heuristics)
    ↓
Present to user (popup/helm/ivy/xref)
```

## Statistics

- **Total Code**: 7,083 lines (5,019 + 2,064)
- **Language Rules**: 433 entries
- **Supported Languages**: 54+
- **File Extensions**: ~120 entries
- **Search Tools**: 8 (grep, gnu-grep, ag, rg, git-grep, git-grep-plus-ag, ugrep)
- **Test Cases**: 40+ ERT tests
- **Documentation**: 1,349 lines (this documentation set)

## Implementation Complexity

Adding a new language requires:
- **Minimum**: 2 data structure entries (extension mapping, 1-2 rules)
- **Recommended**: 5-6 entries (extension, rules, comment syntax, context hints, tests)
- **Complete**: 10+ entries (all of above plus context rules, multiple rule types, test fixtures)

## Common Tasks

### Add a simple language
See: `QUICK_REFERENCE_LANGUAGE_SUPPORT.md` - "Adding a New Language: 5-Minute Checklist"

### Understand how a language is supported
1. Search for `:language "LANGUAGE"` in `dumb-jump.el`
2. Review the rules, contexts, and comments defined
3. Check the test fixtures in `test/data/`

### Test language rules
```bash
cd /path/to/dumb-jump
make test-this LANGUAGE      # Run tests matching language name
M-x dumb-jump-test-ag-rules  # Test all ag rules interactively
```

### Debug a rule
1. Run `(dumb-jump-populate-regex TEMPLATE "symbol" 'ag)` in Emacs
2. Test the regex manually with: `ag "REGEX" /path/to/project`
3. Enable debug mode: `(setq dumb-jump-debug t)` then jump
4. Check `*Messages*` buffer for details

## Contributing

To add support for a new language:

1. Follow the step-by-step guide in `LANGUAGE_SUPPORT_ARCHITECTURE.md`
2. Use the checklist in `QUICK_REFERENCE_LANGUAGE_SUPPORT.md`
3. Reference common patterns in same document
4. Test thoroughly using built-in test functions
5. Create test fixtures in `test/data/proj-LANGUAGE/`
6. Add ERT tests in `test/dumb-jump-test.el`
7. Run full test suite: `make test`

## Related Files

- `dumb-jump.el` - Main implementation (5,019 lines)
- `test/dumb-jump-test.el` - ERT tests (2,064 lines)
- `test/test-helper.el` - Test utilities
- `test/data/` - Test fixtures
- `README.md` - User documentation
- `Makefile` - Test orchestration

## Questions?

Refer to the documentation files above:
1. Quick answer needed? → `QUICK_REFERENCE_LANGUAGE_SUPPORT.md`
2. Complete understanding? → `LANGUAGE_SUPPORT_ARCHITECTURE.md`
3. Need a diagram? → `LANGUAGE_SUPPORT_VISUAL.txt`
4. User guide? → `README.md`

---

**Documentation Created**: February 23, 2026
**Last Updated**: February 28, 2026
