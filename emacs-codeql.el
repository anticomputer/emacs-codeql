;;; emacs-codeql.el --- codeql support for emacs -*- lexical-binding: t -*-

;; Author: Bas Alberts
;; Maintainer: Bas Alberts <bas@anti.computer>
;; Version: 0.1-pre-alpha
;; Package-Requires: ((emacs "27.1") (seq "2.23") (transient "0.3.7") (jsonrpc "1.0.15") (tree-sitter "0.18.0") (tree-sitter-langs "0.11.4") (tree-sitter-indent) (aggressive-indent) (eglot) (projectile) (org))
;; Homepage: https://github.com/anticomputer/codeql.el
;; Keywords: dev

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a very early version of modern CodeQL support for emacs.
;;
;; It operates two major components:
;;
;; ql-tree-sitter.el
;;
;; A tree-sitter based QL highlighting and indentation mode. It requires
;; emacs-tree-sitter, tree-sitter-langs, tree-sitter-indent, tree-sitter-hl
;; and aggressive-indent to function.
;;
;; emacs-codeql.el
;;
;; A utility layer that enhances ql-tree-sitter-mode with modern codeql
;; ide extension functionality. It requires transient and json-pointer
;; to function in a basic capacity but projectile and eglot are highly
;; recommended for an optimal UX.
;;
;; While ql-tree-sitter.el does function without emacs-codeql.el, the two
;; are assumed to operate in tandem and considered part of the same package.

;; For `emacs-codeql' configuration examples and advice:
;;
;; https://github.com/anticomputer/emacs-codeql

;;; Disclaimers

;; This package has not been tested with lsp-mode and earlier experiments
;; with lsp-mode and QL language support suggest a less than ideal experience
;; when compared with eglot based lsp support for QL.

;;; Acknowledgements

;; This package was heavily inspired by, and in some cases directly ported
;; from, Alvaro Muñoz's codeql.nvim (https://github.com/pwntester/codeql.nvim)
;; I'd like to thank him for dragging me into a continuous, yet good natured,
;; editor arms race. He solved most of the hard problems in lua, and a lot of
;; this package stands on the shoulders of his prior art.

;; A lot of the fundamental ideas behind this package were both inspired and
;; outlined by Esben Sparre Andreasen (https://github.com/esbena). Esben is
;; a CodeQL engineer and his deep QL knowledge and early work on emacs codeql
;; support was a big inspiration to revive this effort. While this package
;; no longer contains Esben's original major mode and org rendering, the very
;; first iteration was heavily drafted on top of his work, and his early
;; design ideas are very much carried forward in this implementation.

;; While I was writing this code I found myself doing an inordinate amount
;; of destructuring binds to chew through the various json objects. In search
;; of a cleaner solution, I came across https://github.com/syohex/emacs-json-pointer
;; which I've found to be infinitely useful for clear json object access in elisp.
;; Since it is not available on MELPA, we bundle it as part of this package.
;; All original copyright and licensing as designated by Shohei YOSHIDA applies,
;; and I'd like to thank them for their continued contributions across the elisp
;; ecosystem.

;;; Support

;; There is none. This package is wholly unsupported and is focused solely on
;; my own workflows and requirements for codeql use. However, if you find it
;; useful and do run into problems, do please file an issue :)

;;; Code:

;; part of emacs
(require 'jsonrpc)
(require 'cl-lib)
(require 'url-util)
(require 'url-parse)
(require 'org)
(require 'ol)

;; bundled
(require 'ql-tree-sitter)
(require 'json-pointer)

;; not part of emacs
(require 'transient)

;;; place our artifacts if they don't exist yet (very hacky!)
(when t
  (require 'tree-sitter-langs))
(declare-function tree-sitter-langs--bin-dir "ext:tree-sitter-langs")
(unless
    (member t
            (cl-map 'list
                    (lambda (f)
                      (file-exists-p (concat (tree-sitter-langs--bin-dir) f)))
                    '("/ql.dylib" "/ql.so")))
  (if (yes-or-no-p "tree-sitter-langs ql support not found, install artifacts now?")
      (let ((cmd (format "cp %s/bin/ql.* %s"
                         (file-name-directory (or load-file-name (buffer-file-name)))
                         (tree-sitter-langs--bin-dir))))
        (message "Placing tree-sitter artifacts with %s" cmd)
        (cl-assert (eql 0 (shell-command cmd))))
    (error "emacs-codeql does not work without tree-sitter-langs support.")))

;;; global user configuration options

(defgroup emacs-codeql nil
  "Emacs CodeQL support configuration."
  :prefix "emacs-codeql"
  :group 'emacs-codeql)

(defcustom codeql-ast-sync-highlighting t
  "Enable fancy AST viewer synchronized highlighting."
  :type 'boolean
  :group 'emacs-codeql)

(defcustom codeql-query-server-timeout most-positive-fixnum
  "Query server compile/run timeout in seconds.

Normally we do not timeout, since queries can be long running.
Users may cancel queries instead through the transient UI."
  :type 'integer
  :group 'emacs-codeql)

(defcustom codeql-transient-binding "C-c C-d"
  "The keybinding to start the emacs-codeql transient UI."
  :type 'key-sequence
  :group 'emacs-codeql)

(defcustom codeql-enable-xrefs t
  "Enable CodeQL xref support for database source archives."
  :type 'boolean
  :group 'emacs-codeql)

(defcustom codeql-enable-remote-xrefs t
  "Enable CodeQL xref support for database source archives in remote sessions."
  :type 'boolean
  :group 'emacs-codeql)

(defcustom codeql-configure-eglot-lsp t
  "Use eglot for LSP support by default.

emacs-codeql requires eglot 20220326.2143 or newer from MELPA."
  :type 'boolean
  :group 'emacs-codeql)

(defcustom codeql-verbose-commands nil
  "Be verbose about which commands we're running at any given time."
  :type 'boolean
  :group 'emacs-codeql)

(defcustom codeql-configure-projectile t
  "Configure project by default."
  :type 'boolean
  :group 'emacs-codeql)

(defcustom codeql-state-dir "~/.config/codeql/emacs"
  "Base directory for codeql emacs state maintenance.

It is important to keep this as a ~/relative path so that it will resolve both
in local and remote contexts to something that readily exists."
  :type 'directory
  :group 'emacs-codeql)

(defcustom codeql-max-raw-results 2000
  "The max amount of raw result tuples to render in an org-table."
  :type 'integer
  :group 'emacs-codeql)

(defcustom codeql-use-gh-codeql-extension-when-available t
  "If emacs-codeql detects the presence of a codeql enabled gh cli, use it.

See https://github.com/github/gh-codeql for more information."
  :type 'boolean
  :group 'emacs-codeql)

(defcustom codeql-query-server-events-buffer-size 2000000
  "The scrollback size for query server event buffers."
  :type 'integer
  :group 'emacs-codeql)

;; globals that the user normally shouldn't have to touch, but may

(defvar codeql-cli (executable-find "codeql")
  "Path to codeql-cli")

(defvar codeql-search-paths
  ;; file expansion happens buffer-local so that these work remotely as well
  (list "./")
  "codeql cli library search paths that require search precedence.

This provides you with search path precedence control and should not be used for
standard search path configurations.

The default value of . ensures that either the project root, or the CWD of the
current query file is part of your search path at a higher precedence than the
configured paths.

Please configure your codeql cli search paths through ~/.config/codeql/config as
per:

https://codeql.github.com/docs/codeql-cli/specifying-command-options-in-a-codeql-configuration-file/")

;; internal variables and configurations

(defvar codeql--run-query-always-save t
  "Whether you always want to save prior to query evaluation.

Evaluation requires a synced position in the query file on disk, such that the
query server can grab the right contents.

This applies to both normal evaluation and quick evaluation.")

(defvar codeql--query-server-max-ram nil
  "The max amount of RAM to use for query server evaluations.

Leave nil for default.")

;; we use buffer-local copies of these so we can play context dependent tricks
(defvar-local codeql--cli-buffer-local nil)
(defvar-local codeql--search-paths-buffer-local nil)

(defun codeql--gh-codeql-cli-available-p ()
  (condition-case nil
      (eql 0 (with-temp-buffer
               (process-file "gh" nil `(,(current-buffer) nil) nil "codeql")))
    (error nil)))

(defvar-local codeql--use-gh-cli nil)

(defun codeql--search-paths-from-codeql-config ()
  "Grab search paths from the codeql cli configuration file."
  (let ((config-path "~/.config/codeql/config"))
    (when-let ((search-paths-string
                (and (codeql--file-exists-p config-path)
                     (codeql--shell-command-to-string
                      ;; deal with both = and " " syntaxes
                      (format "grep '^--search-path' %s" config-path)))))
      (let ((path-config
             (cadr (split-string (string-trim-right search-paths-string) " +\\|="))))
        ;; make sure all the search paths exist
        (cl-loop with search-paths = (split-string path-config ":")
                 for path in search-paths
                 unless (codeql--file-exists-p path) do
                 (error (format "Non-existing search path in configuration: %s" path))
                 collect
                 (let ((local-search-path
                        (codeql--tramp-unwrap
                         (expand-file-name (codeql--tramp-wrap path)))))
                   (message "Adding %s to search path from codeql cli config." local-search-path)
                   local-search-path))))))

(defun codeql--search-paths-from-emacs-config ()
  "Grab search paths from our emacs configuration."
  (cl-loop for path in codeql-search-paths
           unless (codeql--file-exists-p path) do
           (error (format "Non-existing search path in configuration: %s" path))
           collect (let ((local-search-path
                          (codeql--tramp-unwrap
                           (expand-file-name (codeql--tramp-wrap path)))))
                     (message "Adding %s to search path from emacs config." local-search-path)
                     local-search-path)))

(defun codeql--search-path ()
  "Return any currently configured codeql cli search paths."
  (mapconcat #'identity codeql--search-paths-buffer-local ":"))

;; Projectile configuration

(when codeql-configure-projectile
  (require 'projectile)
  ;; use projectile for project root management
  (projectile-register-project-type
   'ql '("qlpack.yml")
   ;; TODO: fill these in with codeql commands
   ;; :compile ""
   ;; :test ""
   ;; :run ""
   ;; :test-suffix ""
   :project-file "qlpack.yml"))

;; Eglot configuration

(defun codeql--lang-server-contact (_i)
  "Return the currently configured LSP server parameters."

  (cl-assert codeql--cli-buffer-local t)

  (let ((lsp-server-cmd
         (append
          (when codeql--use-gh-cli '("gh"))
          (list
           codeql--cli-buffer-local
           "execute" "language-server"
           (format "--search-path=%s" (codeql--search-path))
           "--check-errors" "ON_CHANGE"
           "-q"))))
    (message "Using LSP server cmd: %s" lsp-server-cmd)
    lsp-server-cmd))

(when codeql-configure-eglot-lsp
  (require 'eglot)

  ;; only configure eglot for codeql when we have the cli available in PATH
  (add-to-list 'eglot-server-programs
               `(ql-tree-sitter-mode . ,#'codeql--lang-server-contact)))

(defun codeql--get-cli-version ()
  ;; used in setup for ql-tree-sitter-mode so we can not assert the mode here yet.
  (let* ((cmd (format "%s version" codeql--cli-buffer-local)))
    (codeql--shell-command-to-string
     (if codeql--use-gh-cli
         (format "gh %s" cmd) cmd))))

(defun codeql--buffer-local-init-hook ()
  "Set up a codeql buffer context correctly."
  ;; use whatever the current config for codeql-cli and codeql-search-paths is
  (setq codeql--cli-buffer-local codeql-cli)

  ;; decide whether we want to use the gh cli to run our codeql commands
  (when (and codeql-use-gh-codeql-extension-when-available
             (codeql--gh-codeql-cli-available-p))
    (message "Enabling gh cli codeql extension use.")
    (setq codeql--use-gh-cli t)
    (setq codeql--cli-buffer-local "codeql"))

  ;; ensure we have somewhere to store result data in both local and remote contexts
  (codeql--init-state-dirs)

  ;; if we're in a remote context, make absolutely sure we know where the cli lives
  (when (and (file-remote-p default-directory))
    (unless codeql--use-gh-cli
      ;; no gh cli available ... we still need a path
      (let ((remote-path
             (string-trim-right
              (or (executable-find "codeql" t)
                  (read-file-name "Need remote path to codeql cli bin: "
                                  nil default-directory t)))))
        (message "Setting remote codeql cli path: %s" remote-path)
        (setq codeql--cli-buffer-local (codeql--tramp-unwrap remote-path)))))

  ;; ensure we were able to resolve _A_ path for the codeql cli local|remote execution
  (cl-assert codeql--cli-buffer-local t)

  ;; ensure we have the eglot LSP client setup if folks want it
  ;; catch any errors so we don't fail just because the LSP fails
  (condition-case nil
      ;; without eglot we don't get default-directory set from project-root
      ;; so we back up to a reasonable alternative before resolving paths

      (if (and codeql-configure-eglot-lsp
               (or (and (file-remote-p (buffer-file-name))
                        (yes-or-no-p "[WARNING] Do you want to use LSP remotely? This can be very slow!"))
                   (not (file-remote-p (buffer-file-name)))))
          ;; turn eglot on if local or we really want it remote
          (eglot-ensure)
        ;; set default-directory to a sane alternative if not
        (setq default-directory (file-name-directory (buffer-file-name))))

    ;; no LSP for us due to error
    (error (progn
             (message "Ignoring failed LSP initialization and plowing ahead!")
             (setq default-directory (file-name-directory (buffer-file-name))))))

  ;; now that default-directory points where it should, resolve our search paths
  (message "Resolving search paths from configuration.")
  (setq codeql--search-paths-buffer-local
        (append
         ;; emacs-codeql configs always have precedence
         (codeql--search-paths-from-emacs-config)
         ;; only add new paths that weren't already configured to prevent double-hits
         (cl-loop with config-paths = (codeql--search-paths-from-codeql-config)
                  for path in config-paths
                  unless (member path codeql-search-paths)
                  collect path)))

  (when (and (<= (length codeql--search-paths-buffer-local) 1)
             (yes-or-no-p "Found <= 1 search path entry, did you forget to configure ~/.config/codeql/config?"))
    (message "Search paths: %s" codeql--search-paths-buffer-local)
    (error "See https://github.com/anticomputer/emacs-codeql for configuration examples.")))

;; add a hook that sets up all the things we need to be available in the buffer-local context
(add-hook 'ql-tree-sitter-mode-hook #'codeql--buffer-local-init-hook)

;; backup formatting utilities for when ql-tree-sitter is not available
(defun codeql-format (&optional min max)
  "Format a QL region, use when ql-tree-sitter + agressive-indent is not available."
  (interactive)
  (shell-command-on-region
   (if min min (point-min))
   (if max max (point-max))
   (format "%s query format --no-syntax-errors -- -" codeql--cli-buffer-local) t t))

(defun codeql-format-region (min max)
  "Format the current CodeQL region if active, or entire buffer if not."
  (interactive "r")
  (codeql-format min max))

;;; storage / persistence

(defvar codeql-results-dir (concat codeql-state-dir "/results"))
(defvar codeql-tmp-dir (concat codeql-state-dir "/tmp"))

;; Eglot lsp-to-point routines that deal with utf-16 code points
;;
;; START LICENSING: https://github.com/joaotavora/eglot/blob/master/LICENSE

;;; deal with utf-16 code points for column calculations, modified from eglot
(defun codeql--lsp-abiding-column ()
  "Calculate current COLUMN as defined by the LSP spec."
  ;; codeql query server uses 1-based column values and utf-16 code points
  (/ (length
      (encode-coding-region
       (line-beginning-position)
       (min (point) (point-max)) 'utf-16 t))
     2))

;; note: this isn't the same as codeql-lsp-abiding column which is 1-based
(defun codeql--eglot-lsp-abiding-column (&optional lbp)
  "Calculate current COLUMN as defined by the LSP spec.
LBP defaults to `line-beginning-position'."
  (/ (- (length
         (encode-coding-region
          (or lbp (line-beginning-position))
          ;; Fix github#860
          (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(cl-defmacro codeql--eglot--widening (&rest body)
  "Save excursion and restriction.  Widen.  Then run BODY." (declare (debug t))
  `(save-excursion (save-restriction (widen) ,@body)))

(defun codeql--eglot-move-to-lsp-abiding-column (column)
  "Move to COLUMN abiding by the LSP spec."
  (save-restriction
    (cl-loop
     with lbp = (line-beginning-position)
     initially
     (narrow-to-region lbp (line-end-position))
     (move-to-column column)
     for diff = (- column
                   (codeql--eglot-lsp-abiding-column lbp))
     until (zerop diff)
     do (condition-case eob-err
            (forward-char (/ (if (> diff 0) (1+ diff) (1- diff)) 2))
          (end-of-buffer (cl-return eob-err))))))

(defun codeql--eglot--lsp-position-to-point (pos-plist &optional marker)
  "Convert LSP position POS-PLIST to Emacs point.
If optional MARKER, return a marker instead"
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (min most-positive-fixnum
                         (plist-get pos-plist :line)))
      (unless (eobp) ;; if line was excessive leave point at eob
        (let ((tab-width 1)
              (col (plist-get pos-plist :character)))
          (unless (wholenump col)
            (message "Caution: LSP server sent invalid character position %s. Using 0 instead."
                     col)
            (setq col 0))
          (codeql--eglot-move-to-lsp-abiding-column col)))
      (if marker (copy-marker (point-marker)) (point)))))

;; END LICENSING: https://github.com/joaotavora/eglot/blob/master/LICENSE

;; Helper functions to deal with tramp contexts

(defun codeql--tramp-wrap (file)
  "Wrap a tramp prefix onto file if in a remote context (if needed)."
  (cond ((file-remote-p file)
         file)
        ((file-remote-p default-directory)
         (let* ((v (tramp-dissect-file-name default-directory))
                (method (tramp-file-name-method v))
                (user (tramp-file-name-user v))
                (host (tramp-file-name-host v)))
           (format "/%s:%s%s:%s"
                   method
                   (if user (format "%s@" user) "")
                   host
                   file)))
        (t
         file)))

(defun codeql--tramp-unwrap (file)
  "Strip the tramp prefix from a file if it's a remote file."
  (cond
   ((file-remote-p file)
    (file-local-name file))
   (t
    file)))

(defun codeql--file-truename (file)
  (if (file-remote-p default-directory)
      (codeql--tramp-unwrap file)
    (file-truename file)))

(defun codeql--file-exists-p (file)
  (if (file-remote-p default-directory)
      (file-exists-p (codeql--tramp-wrap file))
    (file-exists-p (codeql--tramp-unwrap file))))

(defun codeql--init-state-dirs ()
  "Create our various storage directories if they do not exist yet."
  (mapcar (lambda (path)
            (unless (codeql--file-exists-p path)
              (mkdir (codeql--tramp-wrap path) t)))
          (list codeql-state-dir
                codeql-results-dir
                codeql-tmp-dir)))

;;; Request and notification handling

(cl-defgeneric codeql--query-server-handle-request (_server method &rest params)
  "Handle SERVER's METHOD request with PARAMS."
  (message "handle request: %s %s" method params))

(cl-defgeneric codeql--query-server-handle-notification (_server _method &rest _params)
  "Handle SERVER's METHOD notification with PARAMS."
  (message "handle notification"))

(cl-defmethod codeql--query-server-handle-notification
  (_server (method (eql ql/progressUpdated)) &rest params)
  "Handle SERVER's ql/progressUpdated notification with PARAMS."
  ;;(message "%s %s" method params)
  (cl-destructuring-bind ((&key step maxStep message &allow-other-keys)) params
    (unless (string= message "")
      (message "[%s] step %s/%s -> %s" method step maxStep message))))

(cl-defmethod codeql--query-server-handle-request
  (_server (method (eql evaluation/queryCompleted)) &rest params)
  "Handle SERVER's evaluation/queryCompleted request with PARAMS."
  ;;(message "%s %s" method params)
  (cl-destructuring-bind ((&key resultType queryId evaluationTime message &allow-other-keys)) params
    (message "[%s] resultType %s evaluationTime %s" method resultType evaluationTime)
    (cond ((eql resultType 0)
           '())
          ((eql resultType 1)
           ;; oh the joys of function binding vs variable binding :P
           (if message
               (message message)
             (message "ERROR: Other")))
          ((eql resultType 2)
           (if message
               (message message)
             (message "ERROR: OOM")))
          ((eql resultType 3)
           (if message
               (message message)
             (message "ERROR: Timeout")))
          ((eql resultType 4)
           (if message
               (message message)
             (message "ERROR: Query was canceled"))))))

;;; emacs-wide state tracking variables and functions

(defvar codeql--registered-database-history nil)

;; global caches for database source archive files defs/refs/ast
(defvar codeql--definitions-cache (make-hash-table :test #'equal))
(defvar codeql--references-cache (make-hash-table :test #'equal))
(defvar codeql--ast-cache (make-hash-table :test #'equal))

(defvar codeql--ast-backwards-definitions (make-hash-table :test #'equal)
  "A hash table of src-filename -> ast-table.

Provides backwards references into the AST buffer from the source file.")

(defun codeql-clear-refs-defs-ast-cache ()
  "Clear global refs/defs/ast caches in case you need a clean slate."
  (interactive)
  (setq codeql--ast-backwards-definitions (make-hash-table :test #'equal))
  (setq codeql--definitions-cache (make-hash-table :test #'equal))
  (setq codeql--references-cache (make-hash-table :test #'equal))
  (setq codeql--ast-cache (make-hash-table :test #'equal))
  (message "Cleared refs/defs/ast caches."))

(defvar codeql--active-source-roots-with-buffers nil
  "A hash table of currently active archive source roots and their query buffers.")

(defvar codeql--active-datasets nil
  "A hash table of all registered databases across all sessions.")

(defun codeql--active-datasets-init ()
  (setq codeql--active-datasets (make-hash-table :test #'equal)))

(defun codeql--active-datasets-add (database-dataset buffer)
  (puthash database-dataset buffer codeql--active-datasets))

(defun codeql--active-datasets-del (database-dataset)
  (remhash database-dataset codeql--active-datasets))

(defun codeql--active-datasets-get (database-dataset)
  (gethash database-dataset codeql--active-datasets))

(codeql--active-datasets-init)

;;; buffer-local state tracking variables and functions

;; query server config state
(defvar-local codeql--path-problem-max-paths 10)

;; local connection state
(defvar-local codeql--query-server nil
  "CodeQL query server rpc connection.")

;; local database state
(defvar-local codeql--active-database nil)
(defvar-local codeql--active-database-language nil)
(defvar-local codeql--database-dataset-folder nil)
(defvar-local codeql--database-source-location-prefix nil)
(defvar-local codeql--database-source-archive-zip nil)
(defvar-local codeql--database-source-archive-root nil)
(defvar-local codeql--library-path nil)
(defvar-local codeql--dbscheme nil)

;; this expects to run inside a query buffer-local context
(defun codeql--reset-database-state ()
  "Clear out all the buffer-local database state."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode))
  (when codeql--database-source-archive-root
    (codeql--database-unmount-source-archive-zip codeql--database-source-archive-root)
    (remhash (codeql--tramp-wrap codeql--database-source-archive-root)
             codeql--active-source-roots-with-buffers))
  (setq codeql--active-database nil)
  (setq codeql--active-database-language nil)
  (setq codeql--database-dataset-folder nil)
  (setq codeql--database-source-location-prefix nil)
  (setq codeql--database-source-archive-root nil)
  (setq codeql--database-source-archive-zip nil))

;; local query id state
(defvar-local codeql--query-server-client-id 0)
(defvar-local codeql--query-server-progress-id 0)
(defvar-local codeql--query-server-evaluate-id 0)

;; local query history
(defvar-local codeql--completed-query-history nil)

(defun codeql--query-server-on-shutdown (_obj buffer-context)
  ;; remove any active database from global database state
  (when codeql--database-dataset-folder
    (codeql--active-datasets-del codeql--database-dataset-folder))
  ;; clear out the buffer-local server and database state
  (with-current-buffer buffer-context
    (setq codeql--query-server nil)
    (codeql--reset-database-state))
  (message "Shut down query server and cleared active database."))

(defun codeql--query-server-next-client-id ()
  (setq codeql--query-server-client-id
        (1+ codeql--query-server-client-id))
  codeql--query-server-client-id)

(defun codeql--query-server-next-progress-id ()
  (setq codeql--query-server-progress-id
        (1+ codeql--query-server-progress-id))
  codeql--query-server-progress-id)

(defun codeql--query-server-next-evaluate-id ()
  (setq codeql--query-server-evaluate-id
        (1+ codeql--query-server-evaluate-id))
  codeql--query-server-evaluate-id)

(defun codeql--query-server-current-or-error ()
  "Check that we have an active CodeQL query server connection, or error."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (or codeql--query-server
      (jsonrpc-error "No active JSON-RPC connection.")))

;;; utility functions for running queries

(defun codeql--shell-command-to-string (cmd &optional verbose keep-stdout)
  "A shell command to string that gives us explicit control over stdout and stderr."
  (when (or verbose codeql-verbose-commands)
    (message "Running %s cmd: %s" (if (file-remote-p default-directory) "remote" "local") cmd))
  (condition-case nil
      (with-temp-buffer
        (let* ((cmd (if codeql--use-gh-cli (format "gh %s" cmd) cmd))
               ;;(stderr-buffer (get-buffer-create "* codeql--shell-command-to-string stderr *"))
               (stdout-buffer (generate-new-buffer (format "* stdout: %s *" cmd)))
               ;; process-file will work in remote context pending default-directory
               (exit-code
                (apply 'process-file
                       ;; rely on something that's ALWAYS there (hopefully)
                       "/bin/sh"
                       nil
                       ;; redirect stderr here somewhere if we need to
                       `(,stdout-buffer nil)
                       nil
                       "-c"
                       (list cmd)))
               (stdout-data
                (when (eql exit-code 0)
                  (with-current-buffer stdout-buffer (buffer-string)))))
          (unless keep-stdout
            (kill-buffer stdout-buffer))
          ;; return stdout or signal error
          (if stdout-data stdout-data (error "failed to execute cmd"))))
    (error (progn (message "Error in codeql--shell-command-to-string: %s" cmd) nil))))

(defun codeql--resolve-query-paths (query-path)
  "Resolve and set buffer-local library-path and dbscheme for QUERY-PATH."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  ;; only resolve once for current query buffer
  (unless (and codeql--library-path codeql--dbscheme)
    (message "Resolving query paths.")
    ;; we don't need to use --additional-packs since we already offer search path precedence control
    (let* ((cmd (format "%s resolve library-path -v --log-to-stderr --format=json --search-path=%s --query=%s"
                        codeql--cli-buffer-local (codeql--search-path)
                        (codeql--tramp-unwrap query-path)))
           (json (codeql--shell-command-to-string
                  (if codeql--use-gh-cli
                      (format "gh %s" cmd) cmd))))
      (when json
        (condition-case nil
            (cl-destructuring-bind (&key libraryPath dbscheme &allow-other-keys)
                (json-parse-string json :object-type 'plist)
              (setq codeql--library-path libraryPath)
              (setq codeql--dbscheme dbscheme))
          (error (progn (message "error parsing json: %s" json) nil))))))
  ;; nil indicates there was an error resolving these
  (and codeql--library-path codeql--dbscheme))

(defun codeql--database-info (database-path)
  "Resolve info for database at DATABASE-PATH."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (let* ((cmd (format "%s resolve database -v --log-to-stderr --format=json -- %s"
                      codeql--cli-buffer-local (codeql--tramp-unwrap database-path)))
         (json (codeql--shell-command-to-string
                (if codeql--use-gh-cli
                    (format "gh %s" cmd) cmd))))
    (when json
      (condition-case nil
          (json-parse-string json :object-type 'alist)
        (error (progn (message "error parsing json: %s" json) nil))))))

(defun codeql--database-upgrades (database-scheme)
  "Resolve upgrades for DATABASE-SCHEME."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (let* ((cmd (format "%s resolve upgrades -v --log-to-stderr --format=json -- %s"
                      codeql--cli-buffer-local (codeql--tramp-unwrap database-scheme)))
         (json (codeql--shell-command-to-string
                (if codeql--use-gh-cli
                    (format "gh %s" cmd) cmd))))
    (when json
      (condition-case nil
          (json-parse-string json :object-type 'alist)
        (error (progn (message "error parsing json: %s" json) nil))))))

(defun codeql--query-info (query-path)
  "Retrieve metadata for QUERY-PATH."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (let* ((cmd (format "%s resolve metadata -v --log-to-stderr --format=json -- %s"
                      codeql--cli-buffer-local (codeql--tramp-unwrap query-path)))
         (json (codeql--shell-command-to-string
                (if codeql--use-gh-cli
                    (format "gh %s" cmd) cmd))))
    (when json
      (condition-case nil
          (json-parse-string json :object-type 'alist)
        (error (progn (message "error parsing json: %s" json) nil))))))

(defun codeql--bqrs-info (bqrs-path)
  "Retrieve info for BQRS-PATH."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (let* ((cmd (format "%s bqrs info -v --log-to-stderr --format=json -- %s"
                      codeql--cli-buffer-local (codeql--tramp-unwrap bqrs-path)))
         (json (codeql--shell-command-to-string
                (if codeql--use-gh-cli
                    (format "gh %s" cmd) cmd))))
    (when json
      (condition-case nil
          (json-parse-string json :object-type 'alist)
        (error (progn (message "error parsing json: %s" json) nil))))))

(defun codeql--bqrs-to-csv (bqrs-path entities)
  (let* ((csv-file (concat bqrs-path ".csv"))
         (cmd (format "%s bqrs decode --output=%s --format=csv --entities=%s -- %s"
                      codeql--cli-buffer-local
                      (codeql--tramp-unwrap csv-file) entities (codeql--tramp-unwrap bqrs-path))))
    (when (codeql--shell-command-to-string
           (if codeql--use-gh-cli
               (format "gh %s" cmd) cmd))
      (with-temp-buffer (insert-file-contents csv-file) (buffer-string)))))

(defun codeql--bqrs-to-json (bqrs-path entities)
  (let* ((json-file (concat bqrs-path ".json"))
         (cmd (format "%s bqrs decode --output=%s --format=json --entities=%s -- %s"
                      codeql--cli-buffer-local
                      (codeql--tramp-unwrap json-file) entities (codeql--tramp-unwrap bqrs-path))))
    (when (codeql--shell-command-to-string
           (if codeql--use-gh-cli
               (format "gh %s" cmd) cmd))
      (with-temp-buffer (insert-file-contents json-file) (buffer-string)))))

(defun codeql--bqrs-to-sarif (bqrs-path id kind &optional max-paths)
  (cl-assert (and id kind) t)
  (let* ((sarif-file (concat bqrs-path ".sarif"))
         (cmd (format "%s bqrs interpret -v --log-to-stderr -t=id=%s -t=kind=%s --output=%s --format=sarif-latest --max-paths=%s -- %s"
                      codeql--cli-buffer-local id kind
                      (codeql--tramp-unwrap sarif-file) (or max-paths codeql--path-problem-max-paths)
                      (codeql--tramp-unwrap bqrs-path))))
    (when (codeql--shell-command-to-string
           (if codeql--use-gh-cli
               (format "gh %s" cmd) cmd))
      (with-temp-buffer (insert-file-contents sarif-file) (buffer-string)))))

(defun codeql--database-archive-zipinfo ()
  "Return extraction-relative file listing for source archive zip."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (let ((zipinfo (codeql--shell-command-to-string
                  (format "%s -1 %s"
                          (executable-find "zipinfo" (file-remote-p default-directory))
                          codeql--database-source-archive-zip))))
    (when zipinfo
      (let ((zipinfo-relative-paths
             (cl-map 'list (lambda (file) file) (split-string zipinfo "\n"))))
        zipinfo-relative-paths))))

(defun codeql--database-extract-source-archive-zip (trusted-root)
  "Return extraction-relative file listing for source archive zip."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (message "archive-root: %s" (codeql--file-truename codeql--database-source-archive-root))
  (message "trusted-root: %s" (codeql--file-truename trusted-root))
  (if (not (codeql--file-exists-p (codeql--file-truename codeql--database-source-archive-root)))
      (when (eql (string-match (codeql--file-truename trusted-root)
                               (codeql--file-truename codeql--database-source-archive-root)) 0)
        (message "Source root verified to exist in trusted path ... extracting|mounting.")
        (cond
         ;; I like to use https://github.com/google/mount-zip so selfishly check for it here
         ((executable-find "mount-zip")
          (message "Mounting source archive with mount-zip.")
          (when (codeql--shell-command-to-string
                 (format "%s -o nospecials -o nosymlinks -o nohardlinks %s %s"
                         (executable-find "mount-zip" (file-remote-p default-directory))
                         (codeql--file-truename codeql--database-source-archive-zip)
                         (codeql--file-truename codeql--database-source-archive-root)))
            (message "Mounted source archive with mount-zip.")))
         ;; fall back to regular full extraction here
         ((executable-find "unzip")
          (message "Extracting source archive with unzip.")
          (when (codeql--shell-command-to-string
                 (format "%s %s -d %s"
                         (executable-find "unzip" (file-remote-p default-directory))
                         (codeql--file-truename codeql--database-source-archive-zip)
                         (codeql--file-truename codeql--database-source-archive-root)))
            (message "Extracted source archive with unzip.")))))
    (message "Source archive already extracted|mounted.")
    t))

(defun codeql--database-unmount-source-archive-zip (source-zip-root)
  "Unmount a src zip archive if it was mounted."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (cl-assert source-zip-root t)
  (when-let ((true-root (codeql--file-truename source-zip-root))
             (umount (executable-find "umount"))
             (mountpoint (executable-find "mountpoint"))
             (mount-zip (executable-find "mount-zip")))
    (message "archive-root: %s" true-root)
    ;; we only need to do this if mount-zip is available, since that's the only time src will be mounted
    (when (and (codeql--file-exists-p true-root)
               (codeql--shell-command-to-string (format "%s %s" mountpoint true-root))
               (codeql--shell-command-to-string (format "%s %s" umount true-root)))
      (message "Umounted %s" true-root))))

;;; jsonrpc interactions with the buffer-local query server

(cl-defmethod jsonrpc-connection-ready-p
  (_server (_method (eql :evaluation/registerDatabases)))
  "Provide synchronization for register/unregister."
  ;; wait until any buffer local deregistration has completed
  (if codeql--active-database
      nil
    t))

(defun codeql-query-server-register-database (database-path)
  "Register a database with the query server."
  (interactive (list (read-file-name "Database: " nil default-directory t)))
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)

  ;; if no server is running yet, offer to start one
  (unless codeql--query-server
    (transient-setup 'codeql-transient-query-server-init-start)
    (error "No query server running, please start one and re-select database."))

  ;; init the database history if need be
  (unless codeql--registered-database-history
    (setq codeql--registered-database-history (make-hash-table :test #'equal)))

  ;; init active source roots map if need be
  (unless codeql--active-source-roots-with-buffers
    (setq codeql--active-source-roots-with-buffers (make-hash-table :test #'equal)))

  ;; resolve and set the dataset folder, we need this when running queries
  (let* ((database-info (codeql--database-info database-path))
         (database-language (json-pointer-get database-info "/languages/0"))
         (source-location-prefix (json-pointer-get database-info "/sourceLocationPrefix"))
         (source-archive-zip (json-pointer-get database-info "/sourceArchiveZip"))
         (source-archive-root (json-pointer-get database-info "/sourceArchiveRoot"))
         (database-dataset-folder (json-pointer-get database-info "/datasetFolder")))

    (cl-assert codeql--query-server t)
    (cl-assert database-dataset-folder t)

    ;; perform any local deregistration first
    (when (and codeql--active-database codeql--database-dataset-folder)
      (codeql-query-server-deregister-database codeql--database-dataset-folder))

    ;; check that target dataset is not registered in a different session
    (when-let ((buffer (codeql--active-datasets-get database-dataset-folder)))
      (message "Database is registered in other session! Deregistering there.")
      ;; check if we need to do remote deregister
      (with-current-buffer buffer
        (when (and codeql--active-database codeql--database-dataset-folder)
          (codeql-query-server-deregister-database codeql--database-dataset-folder))))

    (message "Registering: %s" database-dataset-folder)
    (jsonrpc-async-request
     (codeql--query-server-current-or-error)
     :evaluation/registerDatabases
     `(:body
       (:databases
        [(:dbDir ,database-dataset-folder :workingSet "default")])
       :progressId ,(codeql--query-server-next-progress-id))
     :success-fn
     (lexical-let ((buffer (current-buffer))
                   (database-language database-language)
                   (database-path database-path)
                   (database-dataset-folder database-dataset-folder)
                   (source-location-prefix source-location-prefix)
                   (source-archive-root source-archive-root)
                   (source-archive-zip source-archive-zip))
       (jsonrpc-lambda (&key registeredDatabases &allow-other-keys)
         ;;(message "Success: %s" registeredDatabases)
         (with-current-buffer buffer
           ;; global addition can be asynchronous, so we do that on success only
           (codeql--active-datasets-add database-dataset-folder (current-buffer))
           ;; associate the source root with the query buffer globally for xref support
           (puthash (codeql--tramp-wrap source-archive-root) (current-buffer) codeql--active-source-roots-with-buffers)
           ;; save in database selection history
           (puthash database-path database-path codeql--registered-database-history)
           ;; these variables are buffer-local to ql-tree-sitter-mode
           (setq codeql--active-database database-path)
           (setq codeql--active-database-language database-language)
           (setq codeql--database-dataset-folder database-dataset-folder)
           (setq codeql--database-source-location-prefix source-location-prefix)
           (setq codeql--database-source-archive-root source-archive-root)
           (setq codeql--database-source-archive-zip source-archive-zip)
           (message "Registered: %s" database-dataset-folder)
           ;; might change codeql--active-database semantics at some point
           ;; so use the database path value we _KNOW_ is legit
           (codeql--database-extract-source-archive-zip database-path))))
     :error-fn
     (jsonrpc-lambda (&key code message data &allow-other-keys)
       (message "Error %s: %s %s" code message data))
     ;; synchronize to only register when deregister has completed in global state
     :deferred :evaluation/registerDatabases
     )))

(defun codeql-query-server-deregister-database (database-dataset-folder)
  "Deregister database associated to DATABASE-DATASET-FOLDER."
  (message "Deregistering: %s" database-dataset-folder)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  ;; we need global removal to be synchronous, so just pull the entry here
  (codeql--active-datasets-del database-dataset-folder)
  (jsonrpc-async-request
   (codeql--query-server-current-or-error)
   :evaluation/deregisterDatabases
   `(:body
     (:databases
      [(:dbDir ,database-dataset-folder :workingSet "default")])
     :progressId ,(codeql--query-server-next-progress-id))
   :success-fn
   (lexical-let
       ((buffer (current-buffer))
        (database-dataset-folder database-dataset-folder))
     (jsonrpc-lambda (&key registeredDatabases &allow-other-keys)
       ;;(message "Success: %s" registeredDatabases)
       (with-current-buffer buffer
         (codeql--reset-database-state)
         (message "Deregistered: %s" database-dataset-folder))))
   :error-fn
   (jsonrpc-lambda (&key code message data &allow-other-keys)
     (message "Error %s: %s %s" code message data))
   :deferred :evaluation/deregisterDatabases
   ))

(defun codeql-query-server-active-database ()
  "Get a short identifier for the currently active database."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (if codeql--active-database
      (file-name-base
       (directory-file-name
        codeql--active-database))
    "select"))

;; XXX: this feels a little crummy still, revisit
(defun codeql--uri-to-filename (uri)
  "Turn URI into a file system location."
  (cl-assert (stringp uri) t)
  (let ((template "%%SRCROOT%%")
        (resolved-path uri))
    ;; do any source prefixing required based on global db info
    (when (string-match template uri)
      (setq resolved-path (replace-regexp-in-string
                           template
                           (or codeql--database-source-location-prefix "")
                           uri)))
    (let* ((url (url-generic-parse-url (url-unhex-string resolved-path)))
           (type (url-type url)))
      (if (and type (not (string= type "file")))
          (error "Unsupported file scheme: %s" resolved-path))
      (url-filename url))))

(cl-defstruct (codeql--result-node
               (:constructor codeql--result-node-create)
               (:copier nil))
  (label             nil :read-only t)
  (mark              nil :read-only t)
  (filename          nil :read-only t)
  (line              nil :read-only t)
  (column            nil :read-only t)
  (visitable         nil :read-only t)
  (url               nil :read-only t)
  (code-flow         nil :read-only t)
  (rule-id           nil :read-only t)
  (code-flows        nil :read-only t)
  (locations         nil :read-only t)
  (related-locations nil :read-only t))

;;; Org-mode based result rendering, so we can turn results into audit notes \o/

(defun codeql--csv-to-org-table (csv-data &optional set-header-line)
  "Transform CSV-DATA into an org-table, optionally setting a header line."
  (with-temp-buffer
    (insert csv-data)
    (org-mode)
    (condition-case nil
        (let ((org-table-convert-region-max-lines
               codeql-max-raw-results))
          (org-table-convert-region (point-min) (point-max) '(4))
          ;; seal off the top
          (goto-char (point-min))
          (when set-header-line
            (save-excursion
              ;; as below
              (org-table-insert-hline)))
          ;; so above
          (org-table-insert-hline t)
          ;; seal off the bottom
          (goto-char (point-max))
          (insert "|-")
          (goto-char (point-min))
          (org-table-align)
          (buffer-string))
      (error
       (progn
         (message "Rendering failed (too many results?) Returning raw CSV.")
         (buffer-string))))))

(defun codeql--escape-org-description (description &optional br-open br-close)
  "Replace [] with something harmless in org link descriptions."
  (replace-regexp-in-string "\\[\\|\\]"
                            (lambda (x) (if (string= x "[") (or br-open "|") (or br-close "|"))) description))

(defun codeql--result-node-to-org (node &optional custom-description br-open br-close)
  "Transform a result node into an org compatible link|string representation."
  ;; https://orgmode.org/guide/Hyperlinks.html
  (if (codeql--result-node-visitable node)
      (progn
        (cl-assert codeql--database-source-archive-zip t)
        (cl-assert (codeql--file-exists-p codeql--database-source-archive-root) t)
        (let ((filename (codeql--result-node-filename node)))
          ;; we implement a custom org link type so we can add extra sauce on link follow
          (when filename (cl-assert (not (string-match ":" filename)))))
        ;; since we have our own link type, also add in a column
        (let ((link (format "codeql:%s%s::%s:%s"
                            ;; we've extracted to the expected location
                            (codeql--tramp-wrap codeql--database-source-archive-root)
                            (codeql--result-node-filename node)
                            (codeql--result-node-line node)
                            (codeql--result-node-column node)))
              (desc (codeql--result-node-label node)))
          (format "[[%s][%s]]" (org-link-escape link)
                  (codeql--escape-org-description
                   (or custom-description desc) br-open br-close))))
    ;; not visitable, just return the label
    (format "%s" (codeql--result-node-label node))))

(defun codeql--org-list-from-nodes (parent-buffer nodes &optional header)
  "Turn a given list of PARENT-BUFFER context location NODES into an org-list.

Sets an optional HEADER."
  (with-temp-buffer
    (when header
      (insert header))
    (cl-loop for node in nodes
             for i below (length nodes) do
             ;; (codeql--print-node node)
             (let* ((prefix
                     (format
                      "%s. %s %s" (1+ i)
                      (or (codeql--result-node-mark node) " ")
                      ;; make sure we have our buffer-local db state
                      (with-current-buffer parent-buffer
                        (codeql--result-node-to-org node))))
                    (suffix
                     (if-let ((url (codeql--result-node-url node)))
                         (format "%s:%s:%s"
                                 ;; set a custom description for this link
                                 (with-current-buffer parent-buffer
                                   (codeql--result-node-to-org node
                                                               (file-name-nondirectory
                                                                (codeql--uri-to-filename
                                                                 (json-pointer-get url "/uri")))))
                                 ;; org links don't support column offsets
                                 ;; so this is mostly just eye candy :P
                                 (json-pointer-get url "/startLine")
                                 (json-pointer-get url "/startColumn"))
                       ;; get ahead, but don't say nothing.
                       ""))
                    ;; do some basic ballpark alignment based on prefix label length
                    (align (- 40 (+ 5 (length (codeql--escape-org-description
                                               (codeql--result-node-label node)))))))
               ;; yolo alignment
               (insert (concat prefix (make-string align ?\s) suffix "\n"))))
    ;; return our nodes as org list
    (buffer-string)))

(defun codeql--query-results-to-org (query-results results-type &optional columns)
  "Turn QUERY-RESULTS of RESULTS-TYPE into an org data."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (let ((parent-buffer (current-buffer)))
    (with-temp-buffer
      (goto-char (point-min))
      (pcase results-type
        ;; raw result parsing of tuples
        ('raw
         ;; insert the final org data to return for rendering
         (insert
          ;; do the inner transform from bqrs results to csv, and csv to org table
          (with-temp-buffer
            ;; add a table header if needed
            (when columns
              ;; so we go from our json back to csv, I know this seems dumb but
              ;; going direct to csv from bqrs means we have to write/maintain
              ;; a ton of dense regex crud to transform into org links, using
              ;; the json parsing, we get all that data nicely handed to us and
              ;; we can then compose back into the data we'd like to represent
              (let ((table-header (mapconcat
                                   #'identity
                                   ;; columns are a vector
                                   (cl-map 'vector (lambda (col)
                                                     (let ((name (json-pointer-get col "/name"))
                                                           (kind (json-pointer-get col "/kind")))
                                                       (format "%s" (or name kind "(unknown)"))))
                                           ;; columns comes direct from the json, no need to reverse
                                           columns) ",")))
                (insert (concat table-header "\n"))))

            ;; add the main body, we pushed result tuples to head of list, so reverse for parsing
            (let* ((query-results (reverse query-results))
                   (table-body (mapconcat
                                #'identity
                                (cl-map 'list
                                        ;; return csv'd row strings here
                                        (lambda (row-data)
                                          (let ((row-data (reverse row-data)))
                                            (mapconcat #'identity
                                                       (cl-map 'list
                                                               (lexical-let ((parent-buffer parent-buffer))
                                                                 (lambda (node)
                                                                   ;; we use buffer-local variables here
                                                                   (with-current-buffer parent-buffer
                                                                     (codeql--result-node-to-org node))))
                                                               row-data) ",")))
                                        query-results) "\n")))
              ;; this returns raw org data, does NOT render to a display buffer
              (insert table-body)
              (codeql--csv-to-org-table (buffer-string) (if columns t nil))))))

        ;; kind: problem and path-problem use the exact same SARIF renderer
        ((or 'problem 'path-problem)
         ;; generate an org tree off of a list structure
         (cl-loop for issue in query-results
                  for locations = (codeql--result-node-locations issue)
                  for related-locations = (codeql--result-node-related-locations issue)
                  for code-flows = (codeql--result-node-code-flows issue)
                  do
                  ;; add the locations for this issue, according to spec this can be up to 10
                  ;; but for code scanning etc. it can only have 1, nest flows either way.
                  (cl-loop for location in locations do
                           (insert
                            (concat
                             (if-let ((url (codeql--result-node-url location)))
                                 (format "* %s [%s] %s ... %s:%s:%s"
                                         (or (codeql--result-node-mark issue) " ")
                                         (codeql--result-node-rule-id issue)
                                         (with-current-buffer parent-buffer
                                           (codeql--result-node-to-org
                                            location
                                            (codeql--result-node-label issue)))
                                         (with-current-buffer parent-buffer
                                           (codeql--result-node-to-org
                                            location
                                            (file-name-nondirectory
                                             (codeql--uri-to-filename
                                              (json-pointer-get url "/uri")))))
                                         (json-pointer-get url "/startLine")
                                         (json-pointer-get url "/startColumn"))
                               (format "* %s ... [location unknown]"
                                       (codeql--result-node-label issue)))
                             "\n"))
                           ;; add any codeflow paths and their associated nodes for this issue
                           (when code-flows
                             (cl-loop for path in code-flows
                                      for i below (length code-flows) do
                                      (cl-loop for nodes in path do
                                               ;; add the location|path nodes to the tree :)
                                               (insert (codeql--org-list-from-nodes
                                                        parent-buffer
                                                        nodes
                                                        (format "** Path #%s\n" i)))))))
                  ;; add related locations as a top level
                  (when related-locations
                    (insert (codeql--org-list-from-nodes parent-buffer related-locations "** Related Locations\n"))))))

      ;; return final org data
      (buffer-string))))

(defun codeql--org-render-sarif-results (org-data &optional footer marker)
  "Render ORG-DATA including an optional FOOTER."
  ;; sarif results are org trees
  (let ((buffer
         (generate-new-buffer
          (format "* codeql-org-results:%s *" (if marker marker "")))))
    (with-current-buffer buffer
      (insert org-data)
      (org-mode)
      (when footer
        (goto-char (point-max))
        (insert "\n* Results Footer\n")
        (insert footer))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (save-excursion
        (switch-to-buffer-other-window buffer))
      (buffer-string))))

(defun codeql--org-render-raw-query-results (org-data &optional footer marker)
  "Render ORG-DATA including an optional FOOTER."
  ;; raw results are org tables
  (let ((buffer
         (generate-new-buffer
          (format "* codeql-org-results:%s *" (if marker marker "")))))
    (with-current-buffer buffer
      (insert org-data)
      (org-mode)
      (goto-char (point-min))
      (org-table-align)
      ;; only add footer after alignment is done
      (when footer
        (goto-char (point-max))
        (insert "\n* Results Footer\n")
        (insert footer))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (save-excursion
        (switch-to-buffer-other-window buffer))
      (buffer-string))))

(defun codeql--print-node (node)
  ;; just for debugging
  (message "--- NODE SNIP START ---")
  (message ":label %s" (codeql--result-node-label node))
  (message ":mark %s" (codeql--result-node-mark node))
  (message ":filename %s" (codeql--result-node-filename node))
  (message ":line %s" (codeql--result-node-line node))
  (message ":visitable %s" (codeql--result-node-visitable node))
  (message ":url %s" (codeql--result-node-url node))
  (message "-- NODE SNIP STOP ---"))

(defun codeql--issue-with-nodes (code-flows locations related-locations message rule-id)
  "Returns an issue node with all its associated nodes out of a SARIF result."
  (codeql--result-node-create
   :label message
   :mark "≔"
   :code-flows
   ;; this returns a list of a list of code-flow paths
   (when code-flows
     (cl-loop for code-flow across code-flows
              for thread-flows = (json-pointer-get code-flow "/threadFlows")
              collect
              (codeql--paths-from-thread-flows thread-flows)))
   :rule-id rule-id
   :locations
   ;; this returns a list of location nodes
   (codeql--nodes-from-locations locations message)
   :related-locations
   ;; this returns a list of related-location nodes
   (codeql--nodes-from-locations related-locations message)))

(defun codeql--paths-from-thread-flows (thread-flows)
  "Returns all paths out of THREAD-FLOWS."
  ;; collect all paths out of thread-flows
  (cl-loop for thread-flow across thread-flows
           for locations = (json-pointer-get thread-flow "/locations")
           ;; collect a path
           collect (codeql--nodes-from-locations locations nil t)))

(defun codeql--nodes-from-locations (locations &optional message is-thread-flow-location)
  "Returns a list of location nodes from result LOCATIONS."
  ;; note, this operates on an array of location objects, this is not the same
  ;; as the top level locations which are passed for a result ...

  ;; collect all path nodes out of locations
  (cl-loop
   ;; if it's a threadFlowLocation, the actual location is nested one deeper
   with prefix             = (if is-thread-flow-location "/location"  "")
   with uri-index          = (format "%s%s" prefix "/physicalLocation/artifactLocation/uri")
   with uri-base-id-index  = (format "%s%s" prefix"/physicalLocation/artifactLocation/uriBaseId")
   with region-index       = (format "%s%s" prefix "/physicalLocation/region")
   with message-index      = (format "%s%s" prefix "/message/text")
   for location across locations
   for i below (length locations)
   for uri                 = (json-pointer-get location uri-index)
   for uri-base-id         = (json-pointer-get location uri-base-id-index)
   for region              = (json-pointer-get location region-index)
   ;; collect a node in a path
   collect
   (let* ((uri (if uri-base-id
                   (format "file:%s/%s" uri-base-id uri)
                 uri))
          (node
           (codeql--result-node-create
            :label (or (json-pointer-get location message-index) message)
            :mark (if is-thread-flow-location
                      ;; mark as a path node
                      (cond ((eql i 0)
                             ;; source for a flow
                             "src")
                            ((eql i (1- (length locations)))
                             ;; sink for a flow
                             "snk")
                            (t
                             ;; step for a flow
                             "..."))
                    ;; mark as a result node
                    "-->")
            :filename (or (codeql--uri-to-filename uri) uri)
            :line  (json-pointer-get region "/startLine")
            :column (or (json-pointer-get region "/startColumn") 1)
            :visitable region
            ;; build a json alist to parse out for url
            :url `(,(cons 'uri uri)
                   ,(cons 'startLine (json-pointer-get region "/startLine"))
                   ,(cons 'endLine (or (json-pointer-get region "/endLine")
                                       (json-pointer-get region "/startLine")))
                   ,(cons 'startColumn (or (json-pointer-get region "/startColumn") 1))
                   ,(cons 'endColumn (json-pointer-get region "/endColumn"))
                   ;; throw these in here just in case they exist and we need them
                   ,(cons 'charOffset (json-pointer-get region "/charOffset"))
                   ,(cons 'charLength (json-pointer-get region "/charLength"))
                   ,(cons 'snippet (json-pointer-get region "/snippet"))))))
     node)))

;; template queries for definitions, references, and AST

(defvar codeql--templated-query-formats
  (list :c           "cpp/ql/src/%s.ql"
        :cpp         "cpp/ql/src/%s.ql"
        :java        "java/ql/src/%s.ql"
        :cs          "csharp/ql/src/%s.ql"
        :javascript  "javascript/ql/src/%s.ql"
        :python      "python/ql/src/%s.ql"
        :ql          "ql/ql/src/ide-contextual-queries/%s.ql"
        :ruby        "ruby/ql/src/ide-contextual-queries/%s.ql"
        :go          "ql/lib/%s.ql")
  "A format list for finding templated queries by name")

(defun codeql--templated-query-path (language query-name)
  "Return the relative path for a QUERY-NAME of type LANGUAGE."
  (when-let (fmt(plist-get codeql--templated-query-formats language))
    (format fmt query-name)))

(defun codeql--archive-path-from-org-filename (filename)
  ;; do a dance to normalize back to the archive root relative path for this filename
  (format "/%s" (cadr (split-string filename (format "%s/*" codeql--database-source-archive-root)))))

(defun codeql--run-templated-query (language query-name src-filename src-buffer)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (cl-assert codeql--active-database t)

  (when-let ((query-path (codeql--templated-query-path language query-name)))
    (cl-loop for search-path in codeql--search-paths-buffer-local
             for full-path = (format "%s/%s" search-path query-path)
             when (codeql--file-exists-p full-path)
             do
             (message "Resolved templated query %s to %s" query-name full-path)
             (let ((template-values
                    `(:selectedSourceFile
                      (:values
                       (:tuples
                        [(:stringValue
                          ,(codeql--archive-path-from-org-filename
                            (codeql--tramp-unwrap src-filename)))])))))
               (codeql--query-server-run-query-from-path
                full-path
                nil
                template-values
                src-filename
                src-buffer))
             ;; respect search precedence and only return from first find
             (cl-return)
             ;; if we did not return, that means we weren't able to find the thing.
             finally
             (error "Did not find templated queries in search paths, did you not configure ~/.config/codeql/config?"))))

(defun codeql--database-src-path-p (path)
  (let ((prefix (format "^%s/*" codeql--database-source-archive-root)))
    (when (string-match prefix (codeql--tramp-unwrap path))
      t)))

;; mutual highlighting at point for src/ast buffers

(defvar codeql--ast-to-src-buffer (make-hash-table :test #'equal))
(defvar codeql--src-to-ast-buffer (make-hash-table :test #'equal))

(defvar-local codeql--ast-last-point nil)
(defvar-local codeql--src-last-point nil)

(defvar-local codeql--ast-overlay nil)
(defvar-local codeql--src-overlay nil)

;; not the fastest, but makes life easier for now
(defvar-local codeql--ast-lookup-cache nil)

;; do expensive calc once, then reuse after that
(defun codeql--ast-lookup-cache-init (ast-lookup-vector)
  (message "Building AST lookup cache ... please hold.")
  ;; disable hooks while yolo-ing around with point in src buffer
  (remove-hook 'post-command-hook #'codeql--sync-src-to-ast-overlay :local)
  (setq codeql--ast-lookup-cache
        (cl-loop for ast-def-seq across ast-lookup-vector
                 for i below (length ast-lookup-vector)
                 for src-start-line = (aref ast-def-seq 0)
                 for src-end-line = (aref ast-def-seq 1)
                 for src-start-column = (aref ast-def-seq 2)
                 for src-end-column = (aref ast-def-seq 3)
                 for ast-line = (aref ast-def-seq 4)
                 for lsp-start-point = (codeql--eglot--lsp-position-to-point `(:line ,(1- src-start-line) :character ,(1- src-start-column)))
                 for lsp-end-point = (codeql--eglot--lsp-position-to-point `(:line ,(1- src-end-line) :character ,(1- src-end-column)))
                 collect
                 (vector ast-line lsp-start-point lsp-end-point)))
  (message "AST lookup cache complete ... navigate away.")
  (add-hook 'post-command-hook #'codeql--sync-src-to-ast-overlay 99 :local))

(defun codeql--ast-line-at-src-point (ast-lookup-vector &optional need-ast-buffer)
  (unless codeql--ast-lookup-cache
    (codeql--ast-lookup-cache-init ast-lookup-vector))
  (let ((line-candidates
         ;; we can optimize this much more if we also build a line to candidates index cache
         (cl-loop for ast-def-seq in codeql--ast-lookup-cache
                  for ast-line = (aref ast-def-seq 0)
                  for lsp-start-point = (aref ast-def-seq 1)
                  for lsp-end-point = (aref ast-def-seq 2)
                  when (and (>= (point) lsp-start-point)
                            (<= (point) lsp-end-point))
                  collect (list (- lsp-end-point lsp-start-point) ast-line))))
    ;; renable hook before we do anything else
    (when line-candidates
      (let ((nearest-match (car (sort line-candidates (lambda (a b) (< (car a) (car b)))))))
        (when nearest-match
          (list (cadr nearest-match)
                (when need-ast-buffer
                  (gethash (current-buffer) codeql--src-to-ast-buffer))))))))

(defun codeql--src-point-to-ast-region-highlight (ast-buffer)
  "Return the closest matching AST match available for the thing at src point."
  (when (and (buffer-live-p ast-buffer)
             ;; only do things if ast-buffer is visible and/or focused
             (or (eq ast-buffer (window-buffer (selected-window)))
                 (get-buffer-window ast-buffer)))
    (when-let* ((ast-lookup-vector (gethash (buffer-file-name) codeql--ast-backwards-definitions)))
      (cl-multiple-value-bind (ast-line _) (codeql--ast-line-at-src-point ast-lookup-vector)
        (when (and ast-line (buffer-live-p ast-buffer)
                   ;; only do things if ast-buffer is visible and/or focused
                   (or (eq ast-buffer (window-buffer (selected-window)))
                       (get-buffer-window ast-buffer)))
          (with-current-buffer ast-buffer
            ;; we don't want to save-excursion here
            (widen)
            (cond
             ;; IMPORTANT:
             ;; if we ever do something expensive in our ast buffer hooks
             ;; remember to disable/enable the command hooks them as needed

             ;; we're the focused window for some inexplicable reason
             ((eq ast-buffer (window-buffer (selected-window)))
              (goto-char (point-min)) (forward-line (1- ast-line)) (move-end-of-line nil)
              (recenter))
             ;; we're an unfocused window but visible as expected
             ((get-buffer-window ast-buffer)
              (with-selected-window (get-buffer-window ast-buffer)
                (goto-char (point-min)) (forward-line (1- ast-line)) (move-end-of-line nil)
                (recenter))))
            ;; XXX: I could make this overlay the entire subtree, but that's a little resource hoggy
            (if (overlayp codeql--ast-overlay)
                ;; move overlay if we already have one
                (unless (= (overlay-start codeql--ast-overlay)
                           (line-beginning-position))
                  (move-overlay
                   codeql--ast-overlay
                   (line-beginning-position)
                   (line-end-position)))
              ;; make an overlay if we don't
              (setq codeql--ast-overlay
                    (make-overlay
                     (line-beginning-position)
                     (line-end-position)))
              (overlay-put codeql--ast-overlay 'font-lock-face 'highlight))
            ;; I prefer having a visual ping for each event in the AST
            (pulse-momentary-highlight-one-line (point))))))))

;; XXX: TODO: placeholder
(defun codeql--ast-point-to-src-region-highlight (_src-buffer))

;; post-command hooks that run locally in ast/src buffers
(defun codeql--sync-src-to-ast-overlay ()
  (let ((ast-buffer (gethash (current-buffer) codeql--src-to-ast-buffer)))
    (when (buffer-live-p ast-buffer)
      (unless (= (point) codeql--src-last-point)
        (setq codeql--src-last-point (point))
        ;; do a thing
        (codeql--src-point-to-ast-region-highlight ast-buffer)))))

;; XXX: TODO: I find ast->src less useful than src->ast
;; XXX: but we depend on codeql--ast-last-point being available
;; XXX: so still keep this code around
(defun codeql--sync-ast-to-src-overlay ()
  (let ((src-buffer (gethash (current-buffer) codeql--ast-to-src-buffer)))
    (when (buffer-live-p src-buffer)
      (unless (= (point) codeql--ast-last-point)
        (setq codeql--ast-last-point (point))))))

;; xref backend for our global ref/def caches

(defvar-local codeql--xref-has-bindings nil)

(defun codeql--xref-thing-at-point ()
  "Return the thing at point."
  (let ((current-symbol (symbol-at-point)))
    (when current-symbol
      (symbol-name current-symbol))))

;; not what I meant when I said I was looking for closure :/
(defun codeql--set-local-xref-bindings ()
  (unless codeql--xref-has-bindings
    (use-local-map (copy-keymap (symbol-value (intern (format "%s-map" major-mode)))))
    (local-set-key (kbd "M->")
                   ;; hail to the guardians of the watch towers of the west.
                   (lambda (identifier)
                     (interactive (list (codeql--xref-thing-at-point)))
                     (cl-letf (((symbol-value 'xref-backend-functions) (list (lambda () 'codeql-ast))))
                       ;; for AST we generally want a side by side
                       (xref-find-definitions identifier))))
    (local-set-key (kbd "M-.")
                   (lambda (identifier)
                     ;;(interactive (list (xref--read-identifier "Find definitions of: ")))
                     ;; skip the completion crud, since lookups are point/location based
                     (interactive (list (codeql--xref-thing-at-point)))
                     (cl-letf (((symbol-value 'xref-backend-functions) '(codeql-xref-backend)))
                       ;; for actual definitions we want to follow
                       (xref-find-definitions identifier))))
    (local-set-key (kbd "M-?")
                   (lambda (identifier)
                     ;;(interactive (list (xref--read-identifier "Find references of: ")))
                     ;; skip the completion crud, since lookups are point/location based
                     (interactive (list (codeql--xref-thing-at-point)))
                     (cl-letf (((symbol-value 'xref-backend-functions) '(codeql-xref-backend)))
                       (xref-find-references identifier))))
    (local-set-key (kbd "M-,") #'xref-pop-marker-stack)
    ;; add a local post command hook so we can sync to ast region highlights
    (when codeql-ast-sync-highlighting
      (setq codeql--src-last-point (point))
      (add-hook 'post-command-hook #'codeql--sync-src-to-ast-overlay 99 :local))
    ;; source archive files are for browsing only!
    (unless buffer-read-only
      (setq buffer-read-only t))
    (message "Activated CodeQL source archive xref bindings in %s" (file-name-nondirectory (buffer-file-name)))
    (setq codeql--xref-has-bindings t)))

(defun codeql-xref-backend ()
  "CodeQL backend for Xref.

This function serves two purposes. You can bind it as a command so that
you have a shortcut to enabling the codeql xref backend for a file you
know is a source archive file, overriding any default major-mode bindings.

It also serves as the xref-backend-functions function. So even if you
don't call this yourself, when you invoke any of the standard xref
functions, this will run and replace any existing xref bindings in
the local buffer when appropriate, i.e. when the buffer is a file inside
a codeql database source archive."
  (interactive)
  (when codeql-enable-xrefs
    (unless (and (file-remote-p (buffer-file-name))
                 (not codeql-enable-remote-xrefs))
      (if (and (gethash (buffer-file-name) codeql--references-cache)
               (gethash (buffer-file-name) codeql--definitions-cache))
          (progn
            (codeql--set-local-xref-bindings)
            'codeql)
        ;; see if this file SHOULD have codeql refs and defs and the local bindings
        ;; XXX: this is not very performant, make this a faster lookup
        (cl-loop for source-root in (hash-table-keys codeql--active-source-roots-with-buffers)
                 with src-filename = (buffer-file-name)
                 with src-buffer = (current-buffer)
                 when (string-match source-root src-filename)
                 do
                 ;; hail to the guardians of the watch towers of the east
                 (message "Cooking up CodeQL xrefs for %s, please hold." (file-name-nondirectory src-filename))
                 (with-current-buffer (gethash source-root codeql--active-source-roots-with-buffers)
                   (let ((language (intern (format ":%s" codeql--active-database-language))))
                     (codeql--run-templated-query language "localDefinitions" src-filename src-buffer)
                     (codeql--run-templated-query language "localReferences" src-filename src-buffer)))
                 ;; we want our bindings available
                 (codeql--set-local-xref-bindings)
                 (cl-return 'codeql))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql codeql)))
  "Return the thing at point."
  ;; we get away with this because really our codeql xref lookups are based on position
  (codeql--xref-thing-at-point))

;; implement a special backend definition just for AST definitions
(cl-defmethod xref-backend-definitions ((_backend (eql codeql-ast)) _symbol)
  "Show any AST definitions available for the thing at point."
  (if-let ((ast-lookup-vector (gethash (buffer-file-name) codeql--ast-backwards-definitions)))
      (cl-multiple-value-bind (ast-line ast-buffer) (codeql--ast-line-at-src-point ast-lookup-vector t)
        ;; sort the candidates by diff and return the closest match as an xref
        (if (and ast-line ast-buffer (buffer-live-p ast-buffer))
            (list
             (xref-make "[AST] show thing-at-point in AST buffer."
                        (xref-make-buffer-location
                         ast-buffer
                         (save-excursion
                           (with-current-buffer ast-buffer
                             (goto-char (point-min)) (forward-line (1- ast-line)) (move-end-of-line nil)
                             (pulse-momentary-highlight-one-line (point))
                             (point))))))
          ;; buffer must have disappeared, yank it from the cache
          (remhash (buffer-file-name) codeql--ast-backwards-definitions) nil
          (error "No AST buffer available! Please M-x codeql-view-ast on the source buffer.")))
    (error "No AST buffer available yet! Please M-x codeql-view-ast on the source buffer.")))

(cl-defmethod xref-backend-definitions ((_backend (eql codeql)) _symbol)
  "Get known definitions for location at point."
  (cl-multiple-value-bind (json src-root) (gethash (buffer-file-name) codeql--definitions-cache)
    ;; re-parsing the json for every definition is terribly inefficient
    ;; so once this works, move into more performant datastructures
    (when (and json src-root)
      (cl-loop with tuples = (json-pointer-get json "/#select/tuples")
               for tuple across tuples
               for src = (seq-elt tuple 0)
               for dst = (seq-elt tuple 1)
               for src-start-line = (json-pointer-get src "/url/startLine")
               for src-start-column = (or (json-pointer-get src "/url/startColumn") 1)
               for src-end-column = (json-pointer-get src "/url/endColumn")
               for filename = (format "%s%s" src-root (codeql--uri-to-filename (json-pointer-get dst "/url/uri")))
               for dst-line = (json-pointer-get dst "/url/startLine")
               for dst-column = (json-pointer-get dst "/url/startColumn")
               for dst-desc = (json-pointer-get dst "/label")
               when
               ;; columns in emacs are 0-based, columns in codeql are 1 based
               ;; columns in codeql are utf-16 code points, not visual column
               (let ((point-line (line-number-at-pos))
                     (point-column (codeql--lsp-abiding-column)))
                 (and (eql src-start-line point-line)
                      (>= point-column src-start-column)
                      (<= point-column src-end-column)))
               ;; if point is at a ref that we know about, collect the def
               collect
               (xref-make dst-desc (xref-make-file-location (codeql--tramp-wrap filename) dst-line (1- dst-column)))))))

(cl-defmethod xref-backend-references ((_backend (eql codeql)) _symbol)
  "Get known references for location at point."
  (cl-multiple-value-bind (json src-root) (gethash (buffer-file-name) codeql--references-cache)
    (when (and json src-root)
      (cl-loop with tuples = (json-pointer-get json "/#select/tuples")
               for tuple across tuples
               for src = (seq-elt tuple 0)
               for dst = (seq-elt tuple 1)
               for dst-start-line = (json-pointer-get dst "/url/startLine")
               for dst-start-column = (json-pointer-get dst "/url/startColumn")
               for dst-end-column = (json-pointer-get dst "/url/endColumn")
               for filename = (format "%s%s" src-root (codeql--uri-to-filename (json-pointer-get src "/url/uri")))
               for src-line = (json-pointer-get src "/url/startLine")
               for src-column = (json-pointer-get src "/url/startColumn")
               for src-desc = (json-pointer-get src "/label")
               when
               ;; columns in emacs are 0-based, columns in codeql are 1 based
               ;; columns in codeql are utf-16 code points, not visual column
               (let ((point-line (line-number-at-pos))
                     (point-column (codeql--lsp-abiding-column)))
                 (and (eql dst-start-line point-line)
                      (>= point-column dst-start-column)
                      (<= point-column dst-end-column)))
               ;; if point is at a def that we know about, collect the ref
               collect
               (xref-make
                src-desc
                (xref-make-file-location (codeql--tramp-wrap filename) src-line (1- src-column)))))))

;; XXX: TODO
(cl-defmethod xref-backend-apropos ((_backend (eql codeql)) _symbol)
  nil)

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql codeql)))
  "Return a completion list for identifiers.

Our implementation simply returns the thing at point as a candidate."
  ;; just do a dumb thing at point for now so there's not really any completions
  ;; below is what most folks do for this kind of thing, but I don't think we need it.
  ;; (let (words)
  ;;   (save-excursion
  ;;     (save-restriction
  ;;       (widen)
  ;;       (goto-char (point-min))
  ;;       (while (re-search-forward "\\w+" nil t)
  ;;         (add-to-list 'words (match-string-no-properties 0)))
  ;;       (seq-uniq words))))
  (list (codeql--xref-thing-at-point)))

(add-to-list 'xref-backend-functions #'codeql-xref-backend)

;; custom org link so we can do voodoo when C-c C-o on a codeql: link

(defun codeql--org-open-file-link (filename)
  (cl-multiple-value-bind (filename line-column) (split-string filename "::")
    (cl-multiple-value-bind (line column) (split-string line-column ":")
      (when-let ((src-buffer (find-file-other-window filename)))
        (with-current-buffer src-buffer
          (widen)
          ;; codeql is 1 based, eglot calcs are 0 based, adjust accordingly
          (when (and column line)
            (goto-char (codeql--eglot--lsp-position-to-point
                        `(:line ,(1- (string-to-number line))
                                :character ,(1- (string-to-number column))))))
          ;; enable our xref backend, this kicks in templated queries for refs and defs
          (codeql-xref-backend))))))

(org-link-set-parameters "codeql" :follow #'codeql--org-open-file-link)

(defun codeql--process-defs (json src-filename src-root _src-buffer)
  (puthash src-filename (list json src-root) codeql--definitions-cache)
  (message "Processed definitions for: %s" (file-name-nondirectory src-filename)))

(defun codeql--process-refs (json src-filename src-root _src-buffer)
  (puthash src-filename (list json src-root) codeql--references-cache)
  (message "Processed references for: %s" (file-name-nondirectory src-filename)))

;; AST viewer

(cl-defstruct (codeql--ast-item
               (:constructor codeql--ast-item-create)
               (:copier nil))
  (id                nil :read-only t)
  (parent            nil :read-only nil)
  (label             nil :read-only t)
  (location          nil :read-only t)
  (children          []  :read-only nil)
  (order             nil :read-only nil)
  (result-node       nil :read-only nil))

(defun codeql--valid-graph-p (graph-props)
  (cl-loop for prop across (json-pointer-get graph-props "/tuples")
           when (and (string= (seq-elt prop 0) "semmle.graphKind")
                     (string= (seq-elt prop 1) "tree"))
           do
           (cl-return t)))

(defun codeql--render-ast (json src-root src-filename src-buffer)
  (message "Rendering AST")
  ;; oh boy.
  (let ((id-to-item (make-hash-table :test #'equal))
        (parent-to-children (make-hash-table :test #'equal))
        (child-to-parent (make-hash-table :test #'equal))
        (ast-order (make-hash-table :test #'equal))
        (roots [])
        (edge-labels (make-hash-table :test #'equal))

        (node-tuples (json-pointer-get json "/nodes"))
        (edge-tuples (json-pointer-get json "/edges"))
        (graph-props (json-pointer-get json "graphProperties")))
    (when (codeql--valid-graph-p graph-props)

      ;; round 1
      (cl-loop for tuple across (json-pointer-get edge-tuples "/tuples")
               for source = (seq-elt tuple 0)
               for target = (seq-elt tuple 1)
               for tuple-type = (seq-elt tuple 2)
               for value = (seq-elt tuple 3)
               for source-id = (json-pointer-get source "/id")
               for target-id = (json-pointer-get target "/id")
               do
               (cond ((string= tuple-type "semmle.order")
                      (puthash target-id (string-to-number value) ast-order))
                     ((string= tuple-type "semmle.label")
                      (puthash target-id source-id child-to-parent)
                      (let ((children (gethash source-id parent-to-children)))
                        (puthash source-id (vconcat children `[,target-id]) parent-to-children))
                      (condition-case nil
                          (cl-parse-integer value)
                        (error
                         (puthash target-id value edge-labels))))))

      ;; round 2
      (cl-loop for tuple across (json-pointer-get node-tuples "/tuples")
               for entity = (seq-elt tuple 0)
               for tuple-type = (seq-elt tuple 1)
               for value = (seq-elt tuple 2)
               for entity-id = (json-pointer-get entity "/id")
               for entity-label = (json-pointer-get entity "/label")
               for entity-url = (json-pointer-get entity "/url")
               ;; XXX: double check value isn't "" here
               for node-label = (or value entity-label)
               for edge-label = (gethash entity-id edge-labels)
               for label = (if edge-label (format "%s: %s" edge-label node-label) node-label)
               do
               (cond ((string= tuple-type "semmle.order")
                      (puthash entity-id (string-to-number value) ast-order))
                     ((string= tuple-type "semmle.label")

                      ;; create an AST item node
                      (let ((item (codeql--ast-item-create
                                   :id entity-id
                                   :label label
                                   :location entity-url
                                   :children [])))

                        ;; tag on a location result node for our org renderer to use
                        (when entity-url
                          (setf (codeql--ast-item-result-node item)
                                (codeql--result-node-create
                                 :label label
                                 :mark ""
                                 :filename (codeql--uri-to-filename (json-pointer-get entity-url "/uri"))
                                 :line (json-pointer-get entity-url "/startLine")
                                 :column (json-pointer-get entity-url "/startColumn")
                                 :visitable t
                                 :url entity-url)))

                        (puthash entity-id item id-to-item)

                        ;; check if this item has a known parent
                        (when-let ((parent (gethash (gethash entity-id child-to-parent) id-to-item)))
                          (setf (codeql--ast-item-parent item) parent)
                          (setf (codeql--ast-item-children parent)
                                (vconcat (codeql--ast-item-children parent) `[,item])))

                        ;; check if this item has known children, children is a vector
                        (when-let ((children (gethash entity-id parent-to-children)))
                          (cl-loop for child-id across children
                                   for child = (gethash child-id id-to-item)
                                   when child
                                   do
                                   (setf (codeql--ast-item-parent child) item)
                                   (setf (codeql--ast-item-children item)
                                         (vconcat (codeql--ast-item-children item) `[,child]))))))))

      ;; round 3: sort the tree
      (maphash
       (lambda (_ item)
         (setf (codeql--ast-item-order item)
               (or (gethash (codeql--ast-item-id item) ast-order)
                   most-positive-fixnum))
         (unless (or (codeql--ast-item-parent item))
           (setq roots (vconcat roots `[,item]))))
       id-to-item)

      ;; round 4: ding ding ding,  order and render
      (let ((sorted-tree (codeql--sort-tree roots)))
        (message "Tree is sorted (%d roots) ... rendering." (length sorted-tree))
        ;; render with buffer context ...
        (cl-loop for active-source-root in (hash-table-keys codeql--active-source-roots-with-buffers)
                 with filename = (codeql--tramp-wrap (format "%s%s" src-root src-filename))
                 when (string-match active-source-root filename)
                 do
                 (message "Active buffer context available to render AST for %s" filename)
                 ;; hail to the guardians of the watch towers of the north.
                 (let ((buffer-context (gethash active-source-root codeql--active-source-roots-with-buffers)))
                   (codeql--ast-to-org sorted-tree src-filename src-buffer buffer-context))
                 ;; donezo.
                 (cl-return)
                 ;; fall back to plaintext rendering
                 finally
                 (message "No active buffer context available to render AST with, going to plaintext.")
                 (codeql--ast-to-org sorted-tree src-filename src-buffer))))))

(defun codeql--ast-to-org (sorted-tree src-filename src-buffer &optional buffer-context)
  ;; src-filename is (buffer-filename) for the src buffer ... XXX: check with TRAMP
  (let ((ast-buffer (get-buffer-create (format "* AST viewer: %s *" src-filename))))
    ;;(message "XXX: %s -> %s" src-filename src-buffer)
    (with-current-buffer ast-buffer
      ;; link the src buffer and the ast buffer in mutual harmony if it's still around
      (when (buffer-live-p src-buffer)
        (puthash ast-buffer src-buffer codeql--ast-to-src-buffer)
        (puthash src-buffer ast-buffer codeql--src-to-ast-buffer))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "#+AST_VIEWER: %s\n\n" (file-name-nondirectory src-filename)))
      (codeql--render-tree sorted-tree buffer-context)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (save-excursion
        (org-mode)
        ;; add our highlighting sync
        (when codeql-ast-sync-highlighting
          (setq codeql--ast-last-point (point))
          (add-hook 'post-command-hook #'codeql--sync-ast-to-src-overlay 99 :local))
        (switch-to-buffer-other-window (current-buffer))))))

(defun codeql--render-node (node level &optional buffer-context)
  (insert
   (concat
    (format "%s %s" level
            (if-let* ((result-node
                       ;; if these things hold, we want to resolve the org link
                       (and buffer-context
                            (codeql--ast-item-result-node node)))
                      ;; line == 0 means no location available
                      (line (and (> (codeql--result-node-line result-node) 0)
                                 (codeql--result-node-line result-node))))
                (progn
                  ;; init ast reference cache if need be
                  (let* ((src-filename (codeql--result-node-filename result-node))
                         ;; XXX: double check under TRAMP
                         (full-src-path
                          (with-current-buffer buffer-context
                            (format (codeql--tramp-wrap
                                     (format "%s%s"
                                             codeql--database-source-archive-root
                                             src-filename))))))
                    (unless (gethash full-src-path codeql--ast-backwards-definitions)
                      (puthash full-src-path [] codeql--ast-backwards-definitions)))
                  ;; there's backwards messages in this stuff
                  (let* ((src-filename (codeql--result-node-filename result-node))
                         (full-src-path
                          (with-current-buffer buffer-context
                            (format (codeql--tramp-wrap
                                     (format "%s%s"
                                             codeql--database-source-archive-root
                                             src-filename)))))
                         (ast-line (line-number-at-pos))
                         (ast-lookup-vector (gethash full-src-path codeql--ast-backwards-definitions))
                         (entity-url (codeql--result-node-url result-node))
                         (src-start-line (json-pointer-get entity-url "/startLine"))
                         (src-end-line (or (json-pointer-get entity-url "/endLine") src-start-line))
                         (src-start-column (or (json-pointer-get entity-url "/startColumn") 1))
                         (src-end-column (json-pointer-get entity-url "/endColumn")))
                    ;; make all the info we need to build an AST definition available from our xref backend
                    (puthash full-src-path
                             (vconcat `[,(vector
                                          src-start-line
                                          src-end-line
                                          src-start-column
                                          src-end-column
                                          ast-line)]
                                      ast-lookup-vector)
                             codeql--ast-backwards-definitions))

                  ;; go into the active query buffer context and resolve from database
                  (save-excursion
                    (with-current-buffer buffer-context
                      (format "%s %s%s"
                              (codeql--ast-item-label node)
                              (codeql--result-node-to-org
                               result-node
                               "⧉"
                               "〚" "〛")
                              ;; only add Line stamp to roots
                              (if (codeql--ast-item-parent node) "" (format " Line %s" line))))))
              ;; if not, just return a plain text heading
              (codeql--ast-item-label node))) "\n"))
  ;; hail to the guardians of the watch towers of the south.
  (cl-loop for node across (codeql--ast-item-children node)
           do
           (codeql--render-node node (concat level "*") buffer-context)))

(defun codeql--render-tree (tree &optional buffer-context)
  (cl-loop for root across tree
           do
           (codeql--render-node root "*" buffer-context)))

(defun codeql--sort-node (node)
  ;; sort this node's children
  (message "Sorting node children!")
  (setf (codeql--ast-item-children node)
        (sort (codeql--ast-item-children node)
              (lambda (left right)
                (< (codeql--ast-item-order left) (codeql--ast-item-order right)))))
  ;; sort any children of this node's children
  (when (> (length (codeql--ast-item-children node)) 0)
    (seq-map #'codeql--sort-node (codeql--ast-item-children node))))

(defun codeql--sort-tree (tree)
  ;; sort all the children of the tree
  (seq-map #'codeql--sort-node tree)
  ;; sort all the roots of the tree and return that vector
  (message "Sorting tree!")
  (sort tree (lambda (left right)
               (< (codeql--ast-item-order left) (codeql--ast-item-order right)))))

(defun codeql-view-ast ()
  "Display the AST for the current source archive file."
  (interactive)
  (cl-multiple-value-bind (json src-root) (gethash (buffer-file-name) codeql--ast-cache)
    (if json
        ;; render the AST from cache
        (codeql--render-ast json src-root (buffer-file-name) (current-buffer))
      ;; need to build an AST
      (cl-loop for source-root in (hash-table-keys codeql--active-source-roots-with-buffers)
               with src-filename = (buffer-file-name)
               with src-buffer = (current-buffer)
               when (string-match source-root src-filename)
               do
               (message "Cooking up AST for %s, please hold." (file-name-nondirectory src-filename))
               (let ((query-buffer
                      (gethash source-root codeql--active-source-roots-with-buffers)))
                 (with-current-buffer query-buffer
                   (let ((language (intern (format ":%s" codeql--active-database-language))))
                     (codeql--run-templated-query language "printAst" src-filename src-buffer))))
               ;; exit loop on success
               (cl-return)
               ;; if we reach here, we did not find an active database query server to use
               finally
               (message "Did not recognize this file as being part of an active codeql database.")))))

(defun codeql--process-ast (json src-filename src-root src-buffer)
  ;; only allow this to be called as part of a templated flow
  (puthash src-filename (list json src-root) codeql--ast-cache)
  (codeql--render-ast json src-root src-filename src-buffer))

;; abandon hope, all ye who enter here ...
(defun codeql-load-bqrs (bqrs-path query-path db-path query-name query-kind query-id &optional src-filename src-root src-buffer)
  "Parse the results at BQRS-PATH and render them accordingly to the user."

  (message "Loading bqrs from %s (name: %s kind: %s id: %s)"
           bqrs-path
           (or query-name "no-meta")
           (or query-kind "no-meta")
           (or query-id "no-meta"))

  (when-let ((bqrs-info (codeql--bqrs-info bqrs-path)))
    (let ((_result-sets (json-pointer-get bqrs-info "/result-sets"))
          (compatible-query-kinds (json-pointer-get bqrs-info "compatible-query-kinds"))
          (footer
           (concat
            ;; include query meta if we have some
            (if (and query-name query-kind query-id)
                (format
                 (mapconcat #'identity '("#+QUERY_NAME: %s"
                                         "#+QUERY_KIND: %s"
                                         "#+QUERY_ID: %s" "") "\n")
                 query-name
                 query-kind
                 query-id)
              "")
            ;; info we always want
            (format
             (mapconcat #'identity '("#+QUERY_TIME: %s"
                                     "#+BQRS_PATH: %s"
                                     "#+QUERY_PATH: %s"
                                     "#+DB_PATH: %s") "\n")
             (current-time-string)
             bqrs-path
             query-path
             db-path) "\n")))
      ;; parse results according to the query meta data
      (cond

       ;; AST parsing
       ;; process AST's, definitions, and references

       ((or (string-match "/localDefinitions.ql$" query-path)
            (string-match "/localReferences.ql$" query-path)
            (string-match "/printAst.ql$" query-path))

        ;; don't let these be entered from query history
        (if (and src-filename src-root)
            (let* ((json (json-parse-string
                          (codeql--bqrs-to-json bqrs-path "id,url,string")
                          ;; json-pointer-get wants list based json
                          :object-type 'alist)))
              (when json
                (cond ((string-match "/localDefinitions.ql$" query-path)
                       (codeql--process-defs json src-filename src-root src-buffer))
                      ((string-match "/localReferences.ql$" query-path)
                       (codeql--process-refs json src-filename src-root src-buffer))
                      ((string-match "/printAst.ql$" query-path)
                       (codeql--process-ast json src-filename src-root src-buffer)))))
          (message "Can't process templated query without src-filename and src-root.")))

       ;; SARIF parsing
       ;;
       ;; https://codeql.github.com/docs/codeql-cli/sarif-output/
       ;; https://docs.github.com/en/code-security/code-scanning/integrating-with-code-scanning/sarif-support-for-code-scanning
       ;;
       ;; Parse out a path-problem or problem query result set
       ;;
       ;; for a path-problem the org-tree will look like:
       ;;
       ;; * Issue: Location node
       ;; ** Path0
       ;; *** Source node
       ;; *** Path node(s)
       ;; *** Sink node
       ;; ** ..
       ;; ** Related Locations
       ;; *** Location node
       ;;
       ;; for a regular problem the org-tree will look like:
       ;;
       ;; * Issue: Location node
       ;; ** Related Locations
       ;; *** Location node

       ((or
         ;; path-problem
         (and (seq-contains-p compatible-query-kinds "PathProblem")
              (string= query-kind "path-problem")
              query-id)
         ;; problem
         (and (seq-contains-p compatible-query-kinds "Problem")
              (string= query-kind "problem")
              query-id))

        (when-let ((json (json-parse-string (codeql--bqrs-to-sarif bqrs-path query-id query-kind)
                                            :object-type 'alist)))
          ;; render any available runs
          (cl-loop
           with runs = (json-pointer-get json "/runs")
           ;; runs is a vector
           for run across runs
           for run-id below (length runs)
           for results = (json-pointer-get run "/results")
           for artifacts = (json-pointer-get run "/artifacts")
           do (message "Rendering results for run %s" run-id)
           ;; check if we have any artifact contents, TODO: do something with 'm?
           (cl-loop for artifact across artifacts
                    for contents = (json-pointer-get artifact "/contents")
                    when contents
                    do
                    (message "Found artifact contents.")
                    (cl-return t))
           ;; alrighty, let's start processing some results into org data
           (when-let ((org-results
                       (cl-loop for result across results
                                with n = (length results)
                                for i below n
                                for message = (json-pointer-get result "/message/text")
                                for rule-id = (json-pointer-get result "/ruleId")
                                for code-flows = (json-pointer-get result "/codeFlows")
                                for related-locations = (json-pointer-get result "/relatedLocations")
                                for locations = (json-pointer-get result "/locations")
                                do (message "Rendering SARIF results ... %s/%s" (1+ i) n)
                                collect
                                ;; each result gets collected as its resulting org-data
                                (let* ((codeql--query-results (list (codeql--issue-with-nodes
                                                                     code-flows
                                                                     locations
                                                                     related-locations
                                                                     message rule-id)))
                                       (kind (if code-flows 'path-problem 'problem)))
                                  (codeql--query-results-to-org codeql--query-results kind nil)))))
             ;; save off our results so we don't have to re-render for history
             (with-temp-buffer
               (cl-loop for org-data in org-results do (insert org-data))
               (let ((rendered
                      (codeql--org-render-sarif-results (buffer-string) footer (file-name-nondirectory query-path))))
                 ;; save off the fully rendered version for speedy re-loads
                 (with-temp-file (format "%s.org" bqrs-path)
                   (insert rendered))))))))

       ;; fall through to raw results parsing
       (t
        ;; raw results ... use an alist so we can use json-pointer-get
        ;; moving to list based json here because it's less predictable
        ;; and we can't just destructure from a set of known keys
        (let* ((codeql--query-results nil)
               (json (json-parse-string
                      (codeql--bqrs-to-json bqrs-path "string,url")
                      ;; json-pointer-get wants list based json
                      :object-type 'alist))
               (result-key (if (json-pointer-get json "/#select") "#select"
                             ;; deep sigh ... deal with #Quick_evaluation_of_something tuples
                             (cl-loop for ((k . v)) on json by #'cddr while v do
                                      (message "Checking %s for tuples" k)
                                      until (json-pointer-get json (format "/%s/tuples" k))
                                      finally return k)))
               (tuples (json-pointer-get json (format "/%s/tuples" result-key)))
               (columns (json-pointer-get json (format "/%s/columns" result-key))))
          (if (and tuples (> (length tuples) 0))
              ;; alright, we got tooooooples! time to parse them into display items
              (progn
                ;; tuples are array of tuple, tuple are array of element
                (cl-loop for tuple across tuples do
                         (let ((row-data nil))
                           (cl-loop
                            for element across tuple
                            do (cond
                                ;; node with location info
                                ((and (listp element)
                                      (json-pointer-get element "/url"))
                                 (let ((node (codeql--result-node-create
                                              :label (json-pointer-get element "/label")
                                              :mark "→"
                                              :filename (codeql--uri-to-filename
                                                         (json-pointer-get element "/url/uri"))
                                              :line (json-pointer-get element "/url/startLine")
                                              :column (json-pointer-get element "/url/startColumn")
                                              :visitable t
                                              :url (json-pointer-get element "/url"))))
                                   (cl-pushnew node row-data)))
                                ;; node with no location info, but with a label
                                ((and (listp element)
                                      (json-pointer-get element "/label"))
                                 (let ((node (codeql--result-node-create
                                              :label (json-pointer-get element "/label")
                                              :mark "≔")))
                                   (cl-pushnew node row-data)))

                                ;; everything else is a literal node
                                (t
                                 (let ((node (codeql--result-node-create
                                              :label element
                                              :mark "≔")))
                                   (cl-pushnew node row-data)))))
                           ;; represent each tuple as a row of node columns
                           (cl-pushnew row-data codeql--query-results))))
            (message "No results in bqrs."))
          ;; display results if we have any
          (when codeql--query-results
            (let ((org-data
                   (codeql--query-results-to-org
                    codeql--query-results 'raw columns)))
              (when org-data
                (let ((rendered (codeql--org-render-raw-query-results org-data footer (file-name-nondirectory query-path))))
                  (with-temp-file (format "%s.org" bqrs-path) (insert rendered))))))))))))


;; request cancellation control

(defvar-local codeql--query-server-jsonrpc-current-id nil)
(defvar-local codeql--query-server-jsonrpc-current-deferred nil)
(defvar-local codeql--query-server-jsonrpc-current-connection nil)

(defun codeql--query-server-jsonrpc-register-request (id connection deferred)
  (message "Registering jsonrpc request: %s" id)
  (setq codeql--query-server-jsonrpc-current-id id)
  (setq codeql--query-server-jsonrpc-current-connection connection)
  (setq codeql--query-server-jsonrpc-current-deferred deferred))

(defun codeql--query-server-jsonrpc-unregister-request ()
  (message "Unregistering jsonrpc request: %s" codeql--query-server-jsonrpc-current-id)
  (setq codeql--query-server-jsonrpc-current-id nil)
  (setq codeql--query-server-jsonrpc-current-connection nil)
  (setq codeql--query-server-jsonrpc-current-deferred nil))

(defun codeql-query-server-cancel-query ()
  "Cancel a currently active query."
  (interactive)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (if (and codeql--query-server-jsonrpc-current-id
           codeql--query-server-jsonrpc-current-deferred
           codeql--query-server-jsonrpc-current-connection)
      (progn
        (message "Canceling query id: %s" codeql--query-server-jsonrpc-current-id)
        (jsonrpc-notify (codeql--query-server-current-or-error) :$/cancelRequest
                        `(:id ,codeql--query-server-jsonrpc-current-id))
        (remhash codeql--query-server-jsonrpc-current-id
                 (jsonrpc--request-continuations codeql--query-server-jsonrpc-current-connection))
        (remhash (list codeql--query-server-jsonrpc-current-deferred (current-buffer))
                 (jsonrpc--deferred-actions codeql--query-server-jsonrpc-current-connection))
        (message "Query canceled."))
    (message "No active query running.")))

(defun codeql--jsonrpc-async-request (connection
                                      method
                                      params
                                      &rest args
                                      &key _success-fn _error-fn
                                      _timeout-fn
                                      _timeout _deferred)
  ;; we need to get at the request id, so we use our own wrapper
  (apply #'jsonrpc--async-request-1 connection method params args))

(defun codeql--query-server-request-run (buffer-context
                                         qlo-path
                                         bqrs-path
                                         query-path
                                         query-info
                                         db-path
                                         quick-eval
                                         &optional template-values src-filename src-buffer)
  "Request a query evaluation from the query server."
  (with-current-buffer buffer-context
    (let ((run-query-params
           `(:body
             (:db
              (:dbDir ,codeql--database-dataset-folder
                      :workingSet "default")
              :evaluateId ,(codeql--query-server-next-evaluate-id)
              :queries
              (:resultsPath ,(codeql--tramp-unwrap bqrs-path)
                            :qlo ,(format "file:%s" (codeql--tramp-unwrap qlo-path))
                            :allowUnknownTemplates t
                            :templateValues ,template-values
                            :id 0
                            :timeoutSecs 0)
              :stopOnError :json-false
              :useSequenceHint :json-false)
             :progressId ,(codeql--query-server-next-progress-id))))

      (message "Running query ...")
      (cl-multiple-value-bind (id timer)
          (codeql--jsonrpc-async-request
           (codeql--query-server-current-or-error)
           :evaluation/runQueries run-query-params
           :timeout codeql-query-server-timeout
           :success-fn
           (lexical-let ((buffer-context buffer-context)
                         (query-info query-info)
                         (bqrs-path bqrs-path)
                         (query-path query-path)
                         (db-path db-path)
                         (quick-eval quick-eval)
                         (src-filename src-filename)
                         (src-root codeql--database-source-archive-root))
             (jsonrpc-lambda (&rest _)
               (with-current-buffer buffer-context
                 (codeql--query-server-jsonrpc-unregister-request))
               (message "Query run completed, checking results.")
               ;; if size is > 0 then we have results to deal with
               (let ((bqrs-size (file-attribute-size (file-attributes bqrs-path))))
                 (if (> bqrs-size 0)
                     (progn
                       (with-current-buffer buffer-context
                         ;; save in query history,  name/kind/id can be nil!
                         (let ((name (json-pointer-get query-info "/name"))
                               (kind (json-pointer-get query-info "/kind"))
                               (id (json-pointer-get query-info "/id")))
                           (let ((timestamp (current-time-string)))
                             ;; skip templated queries in query history
                             (unless (or (string-match "/localDefinitions.ql$" query-path)
                                         (string-match "/localReferences.ql$" query-path)
                                         (string-match "/printAst.ql$" query-path))
                               (puthash
                                (format "[%s] %s (%s) [%s]"
                                        timestamp
                                        (file-name-nondirectory query-path)
                                        (if quick-eval "quick-eval" "full-query")
                                        (codeql-query-server-active-database))
                                `(:quick-eval ,quick-eval
                                              :query-path ,query-path
                                              :bqrs-path ,bqrs-path
                                              :db-path ,db-path
                                              :timestamp ,timestamp
                                              :name ,name
                                              :kind ,kind
                                              :id ,id)
                                codeql--completed-query-history)))
                           ;; display results
                           (codeql-load-bqrs
                            bqrs-path
                            query-path
                            db-path
                            name
                            kind
                            id
                            src-filename
                            codeql--database-source-archive-root
                            src-buffer))))
                   (message "No query results in %s!" bqrs-path)))))
           :error-fn
           (jsonrpc-lambda (&key code message data &allow-other-keys)
             (codeql--query-server-jsonrpc-unregister-request)
             (message "Error %s: %s %s" code message data))
           :deferred :evaluation/runQueries)
        (codeql--query-server-jsonrpc-register-request
         id
         (codeql--query-server-current-or-error)
         :evaluation/runQueries)))))

(defun codeql--query-server-request-compile-and-run (buffer-context
                                                     library-path
                                                     qlo-path
                                                     bqrs-path
                                                     query-path
                                                     query-info
                                                     db-path
                                                     db-scheme
                                                     quick-eval
                                                     &optional template-values src-filename src-buffer)
  "Request query compilation from the query server."

  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)

  (with-current-buffer buffer-context
    (let*
        ((query-target
          (if quick-eval
              ;; set a quick eval position if need be, otherwise run the entire query
              (cl-destructuring-bind (min . max) (car (region-bounds))
                ;; positions in CodeQL are 1-based
                (let ((start-line (line-number-at-pos min))
                      (end-line (line-number-at-pos max))
                      (start-col (save-excursion (goto-char min) (codeql--lsp-abiding-column)))
                      (end-col (save-excursion (goto-char max) (codeql--lsp-abiding-column))))
                  `(:quickEval
                    (:quickEvalPos
                     (:fileName ,(codeql--tramp-unwrap query-path)
                                :line ,start-line
                                :column ,start-col
                                :endLine ,end-line
                                :endColumn ,end-col)))))
            ;; any non-null value will trigger a full query run
            `(:query (:xx ""))))
         (compile-query-params
          `(:body
            (:compilationOptions
             (:computeNoLocationUrls t
                                     :failOnWarnings :json-false
                                     :fastCompilation :json-false
                                     :includeDilInQlo t
                                     :localChecking :json-false
                                     :noComputeGetUrl :json-false
                                     :noComputeToString :json-false
                                     :computeDefaultStrings t)
             :extraOptions
             (:timeoutSecs 0)
             :queryToCheck
             (:libraryPath ,library-path
                           :dbschemePath ,db-scheme
                           :queryPath ,(codeql--tramp-unwrap query-path))
             :resultPath ,(codeql--tramp-unwrap qlo-path)
             :target ,query-target)
            :progressId ,(codeql--query-server-next-progress-id))))

      (message "Compiling query (%s) ..." (if quick-eval "quick-eval" "full-query"))

      (cl-multiple-value-bind (id timer)
          (codeql--jsonrpc-async-request
           (codeql--query-server-current-or-error)
           :compilation/compileQuery compile-query-params
           :timeout codeql-query-server-timeout
           :success-fn
           (lexical-let ((buffer-context buffer-context)
                         (bqrs-path bqrs-path)
                         (qlo-path qlo-path)
                         (query-path query-path)
                         (query-info query-info)
                         (db-path db-path)
                         (quick-eval quick-eval)
                         (template-values template-values)
                         (src-filename src-filename))
             (jsonrpc-lambda (&key messages &allow-other-keys)
               (with-current-buffer buffer-context
                 (codeql--query-server-jsonrpc-unregister-request))
               (message "Compilation completed, checking results.")
               (let ((abort-run-query nil))
                 (seq-map (lambda (m)
                            (cl-destructuring-bind (&key severity message &allow-other-keys)
                                m
                              (message "[compilation/compileQuery] severity %s: %s" severity message)
                              (when (eql severity 0)
                                (message "[compilation/compileQuery] Aborting query! severity 0: %s" message)
                                (setq abort-run-query t)))) messages)
                 (unless abort-run-query
                   ;; note: this is a nested jsonrpc request, maintain buffer-local context
                   (codeql--query-server-request-run buffer-context qlo-path
                                                     bqrs-path query-path
                                                     query-info
                                                     db-path
                                                     quick-eval
                                                     template-values
                                                     src-filename
                                                     src-buffer)))))
           :error-fn
           (jsonrpc-lambda (&key code message data &allow-other-keys)
             (codeql--query-server-jsonrpc-unregister-request)
             (message "Error %s: %s %s" code message data))
           :deferred :compilation/compileQuery)
        ;; register this request so we can cancel it if need be
        (codeql--query-server-jsonrpc-register-request
         id
         (codeql--query-server-current-or-error)
         :compilation/compileQuery)))))

(defun codeql--query-server-run-query-from-path (query-path quick-eval &optional template-values src-filename src-buffer)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (cl-assert codeql--active-database t)
  (cl-assert (codeql--resolve-query-paths query-path) t)

  (let* ((qlo-path
          (let ((temporary-file-directory (codeql--tramp-wrap codeql-tmp-dir)))
            (make-temp-file "qlo" nil ".qlo")))
         (bqrs-path
          (let ((temporary-file-directory (codeql--tramp-wrap codeql-results-dir)))
            (make-temp-file "bqrs" nil ".bqrs")))
         (library-path codeql--library-path)
         (db-scheme codeql--dbscheme)
         (db-path (directory-file-name codeql--active-database))
         (query-info (codeql--query-info query-path)))

    ;; request a query compilation, its success callback will then request a query run
    (codeql--query-server-request-compile-and-run (current-buffer)
                                                  library-path
                                                  qlo-path
                                                  bqrs-path
                                                  query-path
                                                  query-info
                                                  db-path
                                                  db-scheme
                                                  quick-eval
                                                  template-values
                                                  src-filename
                                                  src-buffer)))

(defun codeql-query-server-run-query ()
  "Run a query or quick eval a query region.

Note: to quick eval a predicate, you have to select just the predicate name:

https://codeql.github.com/docs/codeql-for-visual-studio-code/analyzing-your-projects/#running-a-specific-part-of-a-query-or-library
"
  (interactive)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)

  ;; init the query history if need be
  (unless codeql--completed-query-history
    (setq codeql--completed-query-history (make-hash-table :test #'equal)))

  (if codeql--query-server
      (let* ((query-path (buffer-file-name))
             (quick-eval (if (use-region-p) t nil)))

        ;; make sure we save the query before running it
        (when (or codeql--run-query-always-save
                  (and (buffer-modified-p)
                       (y-or-n-p "Query was not saved prior to evaluation, save now?")))
          (save-buffer))

        (codeql--query-server-run-query-from-path query-path quick-eval))
    (message "No query server started.")))

(defun codeql-query-history ()
  "Offer query history selection to the user."
  (interactive)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (if (and codeql--completed-query-history (not (eql 0 (hash-table-count codeql--completed-query-history))))
      (let* ((query-keys (vconcat (hash-table-keys codeql--completed-query-history)))
             (query (completing-read "Query History: " (reverse (append query-keys nil)) nil t)))
        (when query
          (let ((query-data (gethash query codeql--completed-query-history)))
            (cl-destructuring-bind (&key
                                    query-path
                                    bqrs-path
                                    timestamp
                                    db-path
                                    name kind
                                    id &allow-other-keys)
                query-data
              (let ((org-results (format "%s.org" bqrs-path)))
                ;; since we render results in a flat text format
                ;; we store it to disk on first render
                (if (file-exists-p org-results)
                    (find-file org-results)
                  (codeql-load-bqrs bqrs-path query-path db-path name kind id)))))))
    ;; XXX: do we want to serialize query history state to disk?
    (message "No query history available yet in this session.")))

(defun codeql-database-history ()
  "Offer global database history selection to the user."
  (interactive)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (if (and codeql--registered-database-history
           (not (eql 0 (hash-table-count codeql--registered-database-history))))
      (let* ((db-keys (vconcat (hash-table-keys codeql--registered-database-history)))
             (db (completing-read
                  "Recent Databases: "
                  ;; show remote files only for active remote context
                  ;; show local files in global local context
                  (reverse
                   (append
                    (cl-loop
                     ;; file-remote-p returns the tramp prefix of a remote file or nil
                     with remote-prefix = (file-remote-p default-directory)
                     for k across db-keys
                     ;; deal with both strings and symbols
                     when (equal (file-remote-p k) remote-prefix)
                     collect k)
                    nil))
                  nil t)))
        (when db
          (let ((database-path (gethash db codeql--registered-database-history)))
            (codeql-query-server-register-database database-path))))
    (message "No database history available across any sessions.")))

;;; transient ui for buffer-local server state management

(transient-define-argument codeql-transient-query-server:--threads ()
  "Use this many threads to evaluate queries."
  :description "threads"
  :class 'transient-option
  :shortarg "t"
  :argument "--threads=")

(transient-define-argument codeql-transient-query-server:--max-disk-cache ()
  :description "disk cache"
  :class 'transient-option
  :shortarg "c"
  :argument "--max-disk-cache=")

(transient-define-argument codeql-transient-query-server:--logdir ()
  :description "log dir"
  :class 'transient-option
  :shortarg "d"
  :argument "--logdir=")

(transient-define-argument codeql-transient-query-server:--verbose ()
  :description "verbosity"
  :class 'transient-switches
  :key "v"
  :argument-format "--%s"
  :argument-regexp "\\(--\\(quiet\\|verbose\\)\\)"
  :choices '("quiet" "verbose"))

;; helpers for the query server transient

(defun codeql--query-server-resolve-ram (&optional max-ram)
  "Resolve ram options for jvm, optionally bounding to MAX-RAM in MB."
  (let* ((max-ram (if max-ram (format "-M=%s" max-ram) ""))
         (cmd (format "%s resolve ram %s --" codeql--cli-buffer-local max-ram))
         (options (codeql--shell-command-to-string
                   (if codeql--use-gh-cli
                       (format "gh %s" cmd) cmd))))
    (when options
      ;; errr ... it's late
      (split-string (string-trim-right options) "\n"))))

;; workaround from eglot.el, disable line buffering if in remote context
(defun codeql--query-server-cmd (contact)
  "Helper for codeql query server connection."
  (if (file-remote-p default-directory)
      (list "sh" "-c"
            (string-join (cons "stty raw > /dev/null;"
                               (mapcar #'shell-quote-argument contact))
                         " "))
    contact))

(transient-define-suffix codeql-transient-query-server-start ()
  "Start a CodeQL query server."
  :description "start"
  :transient nil
  (interactive)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (if codeql--query-server
      (message "There is already a query server running")
    (let* ((ram-options (codeql--query-server-resolve-ram codeql--query-server-max-ram))
           (args (append
                  (transient-args transient-current-command)
                  ram-options
                  ;; non-user arguments go here
                  '("--require-db-registration"))))
      (cl-assert ram-options t)
      ;; spawn the server process
      (let* ((name (format "CODEQL Query Server (%s%s -> %s)"
                           ;; add remote prefix if its there to dedicate event buffer
                           (or (file-remote-p default-directory) "")
                           ;; show query file name for this server if available
                           (file-name-base (directory-file-name default-directory))
                           (or (file-name-nondirectory (buffer-file-name)) "(no file name)")))
             (cmd (append
                   (when codeql--use-gh-cli '("gh"))
                   (list codeql--cli-buffer-local "execute" "query-server")
                   args)))
        (message "Starting query server.")
        ;; manage these buffer local
        (setq codeql--query-server
              (make-instance
               'jsonrpc-process-connection
               :process
               ;; spawn in the remote context if need be
               (let* ((default-directory default-directory)
                      (cmd (codeql--query-server-cmd cmd)))
                 (message "Starting query server with: %s" cmd)
                 (make-process
                  :name name
                  :command cmd
                  :connection-type 'pipe
                  :coding 'utf-8-emacs-unix
                  :noquery t
                  :stderr (get-buffer-create (format "*%s stderr*" name))
                  ;; enable tramp contexts
                  :file-handler t))
               :name name
               :events-buffer-scrollback-size codeql-query-server-events-buffer-size
               :notification-dispatcher #'codeql--query-server-handle-notification
               :request-dispatcher #'codeql--query-server-handle-request
               :on-shutdown
               (lexical-let ((buffer-context (current-buffer)))
                 (lambda (obj)
                   (codeql--query-server-on-shutdown obj buffer-context)))))
        (message "Started query server.")))))

(transient-define-suffix codeql-transient-query-server-stop ()
  "Stop a CodeQL query server."
  :description "stop"
  :transient nil
  (interactive)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (if (not codeql--query-server)
      (message "No active query server found.")
    (jsonrpc-shutdown codeql--query-server)))

(transient-define-prefix codeql-transient-query-server-init-start ()
  "Start a CodeQL query server."
  :value (list "--threads=0" "--quiet" "--log-to-stderr")
  :incompatible '(("--logdir=" "--log-to-stderr"))
  [:class transient-columns
          ["Options"
           (codeql-transient-query-server:--threads)
           (codeql-transient-query-server:--verbose)
           (codeql-transient-query-server:--logdir)
           ("e" "log stderr" "--log-to-stderr")
           (codeql-transient-query-server:--max-disk-cache)]
          ["Server"
           ("s" codeql-transient-query-server-start)]]
  ;; only make this transient available inside ql mode
  (interactive)
  (when (eq major-mode 'ql-tree-sitter-mode)
    (transient-setup 'codeql-transient-query-server-init)))

(transient-define-prefix codeql-transient-query-server-init ()
  "Start|Stop a CodeQL query server, pending on buffer-local state."
  (interactive)
  (when (eq major-mode 'ql-tree-sitter-mode)
    (if codeql--query-server
        ;; just go straight to stopping the server in this case
        (call-interactively #'codeql-transient-query-server-stop)
      (transient-setup 'codeql-transient-query-server-init-start))))

(defun codeql-set-max-paths (max-paths)
  "Configure the buffer-local value for path problem max paths."
  (interactive "n--max-paths=")
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (setq codeql--path-problem-max-paths max-paths))

(defun codeql--database-open-file (filename)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (when-let ((src-buffer (find-file filename)))
    (with-current-buffer src-buffer
      ;; enable our xref backend, this kicks in templated queries for refs and defs
      (codeql-xref-backend))))

(defun codeql-database-open-source-archive-file ()
  "Open a file from the active database source archive."
  (interactive)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (if (and codeql--database-source-archive-root
           codeql--database-source-archive-zip)
      (let* ((relative-file-path
              (completing-read "Source archive file: "
                               (codeql--database-archive-zipinfo)))
             (full-file-path
              (codeql--tramp-wrap
               (format "%s/%s"
                       codeql--database-source-archive-root
                       relative-file-path))))
        (codeql--database-open-file full-file-path))
    (message "No active database.")))

(transient-define-prefix codeql-transient-query-server-interact ()
  "Interact with a CodeQL query server via transient menu."
  [:class transient-columns
          ["Server"
           ("s" codeql-transient-query-server-init
            :description
            (lambda ()
              (if codeql--query-server "stop" "start")))]
          ["Database"
           ("d" codeql-query-server-register-database
            :description
            (lambda ()
              (codeql-query-server-active-database)))
           ("f" "source" codeql-database-open-source-archive-file)
           ("k" "known" codeql-database-history)]
          ["Query"
           ("r" "run" codeql-query-server-run-query)
           ("h" "history" codeql-query-history)
           ("c" "cancel" codeql-query-server-cancel-query)]
          ["Config"
           ("p" codeql-set-max-paths
            :description
            (lambda ()
              (format "--max-paths=%s"
                      codeql--path-problem-max-paths)))]]
  (interactive)
  (when (eq major-mode 'ql-tree-sitter-mode)
    (with-current-buffer (current-buffer)
      (transient-setup 'codeql-transient-query-server-interact))))

;; make this prefix  available in our ql mode
(add-hook 'ql-tree-sitter-mode-hook
          (lambda ()
            (local-set-key
             (kbd codeql-transient-binding)
             #'codeql-transient-query-server-interact)))

;; cleanup on kill
(add-hook 'kill-buffer-hook
          (lambda ()
            (when (eq major-mode 'ql-tree-sitter-mode)
              (when codeql--query-server
                (jsonrpc-shutdown codeql--query-server)))))

(provide 'emacs-codeql)

;;; emacs-codeql.el ends here
