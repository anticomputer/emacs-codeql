;;; ql-tree-sitter-mode.el --- tree-sitter support for QL -*- lexical-binding: t -*-

;; Author: Bas Alberts <bas@anti.computer>
;; Maintainer: Bas Alberts <bas@anti.computer>
;; Created: January 2023
;; Keywords: ql languages tree-sitter

;;; Commentary

;; This mode is packaged with https://github.com/anticomputer/emacs-codeql
;; and requires you to install the ql tree-sitter grammar via the
;; `treesit-install-language-grammar' function, when prompted you can
;; point it at https://github.com/tree-sitter/tree-sitter-ql and accept
;; the default values to build and install the ql tree-sitter artifact.

;;; Code

(require 'treesit)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'cl-extra))
(eval-when-compile (require 'seq))

(declare-function treesit-parser-create "treesit.c")

(defcustom ql-tree-sitter-mode-indent-offset 2
  "Number of spaces for each indentation step in `ql-tree-sitter-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'ql)

(defvar ql-tree-sitter-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; comment / uncomment courtesy of @esbena
    (modify-syntax-entry ?_ "w" table)
    ;; // Comments (style a)
    (modify-syntax-entry ?\/ ". 124" table)
    (modify-syntax-entry ?\n "> " table)
    ;; /* Comments (style b) */
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?@ "_" table)
    table)
  "Syntax table for `ql-tree-sitter-mode'.")

;; highlights.scm
(defvar ql-tree-sitter-mode--font-lock-settings
  (treesit-font-lock-rules

   :language 'ql
   :feature 'comment
   '([(line_comment) (block_comment) (qldoc)] @font-lock-comment-face)

   :language 'ql
   :feature 'string
   '((string) @font-lock-string-face)

   :language 'ql
   :feature 'keyword
   '(["as"
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
      (specialId)]
     @font-lock-keyword-face)

   :language 'ql
   :feature 'import
   '(["import"] @font-lock-keyword-face)

   :language 'ql
   :feature 'keyword-operator
   '(["and" "or" "not"] @font-lock-keyword-face)

   :language 'ql
   :feature 'attribute
   '((annotName) @font-lock-keyword-face)

   :language 'ql
   :feature 'conditional
   '(["if" "then" "else"] @font-lock-keyword-face)

   :language 'ql
   :feature 'function
   '(["avg"
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
      "strictsum"]
     @font-lock-function-name-face
     (aritylessPredicateExpr (literalId) @font-lock-function-name-face)
     (memberPredicate name: (predicateName) @font-lock-function-name-face)
     (classlessPredicate name: (predicateName) @font-lock-function-name-face)
     (charpred (className) @font-lock-function-name-face))

   :language 'ql
   :feature 'type-builtin
   '(["boolean" "float" "int" "date" "string"] @font-lock-type-face)

   :language 'ql
   :feature 'type
   '((dataclass name: (className) @font-lock-type-face)
     (datatype name: (className) @font-lock-type-face)
     (typeExpr name: (className) @font-lock-type-face))

   :language 'ql
   :feature 'variable
   '((varName) @font-lock-variable-name-face)

   :language 'ql
   :feature 'variable-builtin
   '([(this) (super)] @font-lock-variable-name-face)

   :language 'ql
   :feature 'boolean
   '([(true) (false)] @font-lock-variable-name-face)

   :language 'ql
   :feature 'namespace
   '((importModuleExpr qualName: (simpleId) @font-lock-variable-name-face)
     (moduleExpr (simpleId) @font-lock-variable-name-face)
     (module name: (moduleName) @font-lock-variable-name-face))

   ;; these are all non-standard font lock faces, only used in level 4

   :language 'ql
   :feature 'operator
   '(["<" "<=" "=" ">" ">=" "-" "!=" "/" "*" "%" "+" "::"]
     @font-lock-operator-face)

   :language 'ql
   :feature 'number
   '([ (float) (integer)] @font-lock-number-face)

   :language 'ql
   :feature 'punctuation-bracket
   '(["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face)

   :language 'ql
   :feature 'punctuation-delimiter
   '(["," "|"] @font-lock-delimiter-face)))

(defvar ql-tree-sitter-mode--indent-rules
  '((ql
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is "then") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ((parent-is "dataclass") parent-bol ql-tree-sitter-mode-indent-offset)
     ((parent-is "if_term") parent-bol ql-tree-sitter-mode-indent-offset)
     ((parent-is "line_comment") parent-bol 0)
     ((parent-is "block_comment") parent-bol 1)
     ((parent-is "qldoc") parent-bol 1)
     ((parent-is "body") parent-bol ql-tree-sitter-mode-indent-offset)
     ((parent-is "par_expr") parent-bol ql-tree-sitter-mode-indent-offset)
     ((parent-is "quantifier") parent-bol ql-tree-sitter-mode-indent-offset)
     ;; ((parent-is "comp_term") parent-bol ql-tree-sitter-mode-indent-offset)
     ;; ((parent-is "add_expr") parent-bol ql-tree-sitter-mode-indent-offset)
     ;; ((parent-is "orderBy") parent-bol ql-tree-sitter-mode-indent-offset)
     ;; no node
     (no-node parent-bol 0)))
  "Tree-sitter indent rules for `ql-tree-sitter-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qll?\\'" . ql-tree-sitter-mode))

;;;###autoload
(define-derived-mode ql-tree-sitter-mode prog-mode "CodeQL"
  "Major mode for editing CodeQL files, powered by tree-sitter.
\\{ql-tree-sitter-mode-map}"
  :group 'ql
  :syntax-table ql-tree-sitter-mode--syntax-table

  (when (treesit-ready-p 'ql)
    (treesit-parser-create 'ql)

    ;; https://github.com/github/codeql/blob/main/docs/ql-style-guide.md
    (setq-local tab-width ql-tree-sitter-mode-indent-offset)
    (setq-local indent-tabs-mode nil)
    (setq-local treesit-simple-indent-rules ql-tree-sitter-mode--indent-rules)

    (setq-local comment-start "// ")
    (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
    (setq-local comment-end "")

    ;; re-indent on these
    (setq-local electric-indent-chars
                (append "{}()" electric-indent-chars))

    (setq-local treesit-font-lock-settings ql-tree-sitter-mode--font-lock-settings)
    ;; 4 feature levels required, 3 is the default
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword
                   string
                   type
                   type-builtin)
                  ;; default feature level (3)
                  (conditional
                   variable
                   variable-builtin
                   boolean
                   namespace
                   conditional
                   function
                   attribute
                   keyword-operator)
                  ;; all features enabled (4)
                  (import
                   number
                   operator
                   punctuation-bracket
                   punctuation-delimiter)))
    ;; navigation
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("dataclass"
                              "classlessPredicate"
                              "memberPredicate"
                              "charpred")))

    ;; TODO: imenu crud

    (treesit-major-mode-setup)))

(provide 'ql-tree-sitter-builtin)

;;; ql-tree-sitter-mode.el ends here
