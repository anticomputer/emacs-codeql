;;; codeql.el --- a tree-sitter based ql major mode for emacs -*- lexical-binding: t -*-

;; this mode is packaged with codeql.el and not recommended for stand-alone use.

;; see the codeql.el documentation for further details

(require 'cl-lib)
(require 'cl-extra)
(require 'seq)

;; based on csharp-mode's trickery to enable clean byte compile
(when t
  ;; melpa packages we depend on
  (require 'aggressive-indent)
  (require 'tree-sitter)
  (require 'tree-sitter-hl)
  (require 'tree-sitter-indent)
  (require 'tree-sitter-langs))

(defvar tree-sitter-major-mode-language-alist)
(declare-function aggressive-indent-mode "ext:agressive-indent")
(declare-function tree-sitter-indent-mode "ext:tree-sitter-indent")
(declare-function tree-sitter-indent-line "ext:tree-sitter-indent")
(declare-function tree-sitter-hl-mode "ext:tree-sitter-hl")
(declare-function tsc-node-end-position "ext:tree-sitter")
(declare-function tsc-node-start-position "ext:tree-sitter")
(declare-function tree-sitter-node-at-point "ext:tree-sitter")

;; highlights.scm
(defconst ql-mode-tree-sitter-patterns
  [
   [
    "as"
    "asc"
    "by"
    "class"
    "desc"
    "extends"
    "forall"
    "forex"
    "from"
    "implies"
    "in"
    "module"
    "newtype"
    "order"
    "select"
    "where"

    (predicate)
    (result)
    (specialId)
    ] @keyword

   [
    "and"
    "not"
    "or"
    ] @keyword.operator

   [
    "avg"
    "any"
    "count"
    "concat"
    "exists"
    "max"
    "min"
    "instanceof"
    "rank"
    "sum"
    "strictconcat"
    "strictcount"
    "strictsum"
    ] @function.builtin

   [
    "import"
    ] @include

   [
    "if"
    "then"
    "else"
    ] @conditional

   [
    (true)
    (false)
    ] @boolean

   [
    (this)
    (super)
    ] @variable.builtin

   [
    "boolean"
    "float"
    "int"
    "date"
    "string"
    ] @type.builtin

   (annotName) @attribute

   [
    "<"
    "<="
    "="
    ">"
    ">="
    "-"
    "!="
    "/"
    "*"
    "%"
    "+"
    "::"
    ] @operator

   [
    "("
    ")"
    "{"
    "}"
    "["
    "]"
    ] @punctuation.bracket

   [
    ","
    "|"
    ] @punctuation.delimiter

   (moduleExpr (simpleId) @namespace)
   (module name: (moduleName) @namespace)

   (dataclass name: (className) @type)
   (datatype name: (className) @type)
   (typeExpr name: (className) @type)

   (importModuleExpr name: (simpleId) @variable)
   (qualModuleExpr name: (simpleId) @variable)
   (varName) @variable

   (integer) @number
   (float) @float

   (string) @string

   (aritylessPredicateExpr (literalId) @function)
   (memberPredicate name: (predicateName) @function)
   (classlessPredicate name: (predicateName) @function)
   (charpred (className) @function)

   [
    (line_comment)
    (block_comment)
    (qldoc)
    ] @comment

   ])

;; indents.scm
(defvar tree-sitter-indent-ql-tree-sitter-scopes
  '((indent-all
     . ())
    (indent-body
     . (dataclass
        charpred
        classlessPredicate
        memberPredicate
        quantified
        body))
    (indent-rest
     . (if_term))
    (paren-indent
     . (par_expr))
    (aligned-siblings
     . (varDecl))
    (multi-line-text
     . (line_comment
        block_comment
        qldoc))
    (outdent
     . ("then"
        "else")))
  "Scopes for indenting in QL.")

(defvar ql-tree-sitter-indent-offset 2
  "Indent offset for ql-tree-sitter-mode.")

;;;###autoload
(define-derived-mode ql-tree-sitter-mode prog-mode "CodeQL"
  "Major mode for editing CodeQL files.
\\{ql-tree-sitter-mode-map}"

  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))

  ;; https://github.com/github/codeql/blob/main/docs/ql-style-guide.md
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)

  (setq-local indent-line-function #'tree-sitter-indent-line)

  ;; comment / uncomment courtesy of @esbena
  (modify-syntax-entry ?_ "w" (syntax-table))
  ;; // Comments (style a)
  (modify-syntax-entry ?\/ ". 124" (syntax-table))
  (modify-syntax-entry ?\n "> " (syntax-table))
  ;; /* Comments (style b) */
  (modify-syntax-entry ?* ". 23b" (syntax-table))
  (modify-syntax-entry ?@ "_" (syntax-table))

  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  (setq-local tree-sitter-hl-default-patterns ql-mode-tree-sitter-patterns)

  (tree-sitter-hl-mode)
  (tree-sitter-indent-mode)
  (aggressive-indent-mode))

(add-to-list 'tree-sitter-major-mode-language-alist '(ql-tree-sitter-mode . ql))
(add-to-list 'auto-mode-alist '("\\.qll?\\'" . ql-tree-sitter-mode))

(provide 'ql-tree-sitter)
