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

;;; TODO

;; * add printAST support
;; * add source archive xref and region annotation support

;;; Code:

;; part of emacs
(require 'jsonrpc)
(require 'cl-lib)
(require 'url-util)
(require 'url-parse)
(require 'org)

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

;;; global user configuration options (XXX: move to defcustom)

(defvar codeql-transient-binding "C-c C-d"
  "The keybinding to start the emacs-codeql transient ui inside ql-tree-sitter-mode.")

(defvar codeql-configure-eglot-lsp t
  "Use eglot for LSP support by default.

emacs-codeql requires eglot 20220326.2143 or newer from MELPA.")

(defvar codeql-verbose-commands nil
  "Be verbose about which commands we're running at any given time.")

(defvar codeql-configure-projectile t
  "Configure project by default.")

(defvar codeql--query-server-max-ram nil
  "The max amount of RAM to use for query server evaluations, leave nil for default.")

(defvar codeql-query-server-events-buffer-size 2000000
  "The scrollback size for query server event buffers.")

(defvar codeql-state-dir "~/.config/codeql/emacs"
  "Base directory for codeql emacs state maintenance.

It is important to keep this as a ~/relative path so that it will resolve both
in local and remote contexts to something that readily exists.")

(defvar codeql-cli (executable-find "codeql")
  "Path to codeql-cli")

(defvar codeql-use-gh-codeql-extension-when-available t
  "If emacs-codeql detects the presence of a codeql enabled gh cli, use it.")

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

;; we use buffer-local copies of these so we can play context dependent tricks
(defvar-local codeql--cli-buffer-local nil)
(defvar-local codeql--search-paths-buffer-local nil)

(defun codeql--gh-codeql-cli-available-p ()
  (condition-case nil
      (eql 0 (with-temp-buffer
               (process-file "gh" nil `(,(current-buffer) nil) nil "codeql")))
    (error nil)))

(defvar-local codeql--use-gh-cli nil)

(defvar codeql-max-raw-results 2000
  "The max amount of raw result tuples to render in an org-table.")

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

;; cache version info for CodeQL CLI
(defvar-local codeql--cli-info nil
  "A cached copy of the current codeql cli version.")

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

(defun codeql--lang-server-contact (i)
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

;; LSP configuration
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

  ;; do any final init we need here
  (setq codeql--cli-info (codeql--get-cli-version))

  (cl-assert codeql--cli-info t)

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

;; helper functions to deal with tramp contexts

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

;; init our storage locations
(defun codeql--init-state-dirs ()
  "Create our various storage directories if they do not exist yet."
  (mapcar (lambda (path)
            (unless (codeql--file-exists-p path)
              (mkdir (codeql--tramp-wrap path) t)))
          (list codeql-state-dir
                codeql-results-dir
                codeql-tmp-dir)))

;;; request and notification handling

(cl-defgeneric codeql--query-server-handle-request (server method &rest params)
  "Handle SERVER's METHOD request with PARAMS."
  (message "handle request: %s %s" method params))

(cl-defgeneric codeql--query-server-handle-notification (server method &rest params)
  "Handle SERVER's METHOD notification with PARAMS."
  (message "handle notification"))

;; specialize the handlers here

(cl-defmethod codeql--query-server-handle-notification
  (server (method (eql ql/progressUpdated)) &rest params)
  "Handle SERVER's ql/progressUpdated notification with PARAMS."
  ;;(message "%s %s" method params)
  (cl-destructuring-bind ((&key step maxStep message &allow-other-keys)) params
    (unless (string= message "")
      (message "[%s] step %s -> %s" method step message))))

(cl-defmethod codeql--query-server-handle-request
  (server (method (eql evaluation/queryCompleted)) &rest params)
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

(defvar codeql--active-datasets nil
  "A hash map of all registered databases across all sessions.")

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

(defun codeql--reset-database-state ()
  "Clear out all the buffer-local database state."
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

;; local org state
(defvar-local codeql--org-parent-buffer nil)

;; local query history
(defvar-local codeql--completed-query-history nil)

;; local caches for database source archive files defs/refs/ast
(defvar-local codeql--definitions-cache nil)
(defvar-local codeql--references-cache nil)
(defvar-local codeql--ast-cache nil)

(defun codeql--reset-def-ref-ast-cache ()
  "Clear out all the buffer-local ref/def/ast caches."
  (setq codeql--definitions-cache nil)
  (setq codeql--references-cache nil)
  (setq codeql--ast-cache nil))


(defun codeql--query-server-on-shutdown (obj)
  ;; remove any active database from global database state
  (when codeql--database-dataset-folder
    (codeql--active-datasets-del codeql--database-dataset-folder))
  ;; clear out the buffer-local server and database state
  (setq codeql--query-server nil)
  (codeql--reset-database-state)
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

;; XXX: executes command synchronously, so if something gets stuck, it's blocking for emacs, move to async logic
(defun codeql--shell-command-to-string (cmd &optional verbose keep-stdout)
  "A shell command to string that gives us explicit control over stdout and stderr."
  (when (or verbose codeql-verbose-commands)
    (message "Running %s cmd: %s" (if (file-remote-p default-directory) "remote" "local") cmd))
  (condition-case nil
      (with-temp-buffer
        (let* ((cmd (if codeql--use-gh-cli (format "gh %s" cmd) cmd))
               (stderr-buffer (get-buffer-create "* codeql--shell-command-to-string stderr *"))
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
  "Resolve info for DATABASE-PATH."
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

(defvar-local codeql--path-problem-max-paths 10)

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
  (let ((zipinfo (codeql--shell-command-to-string
                  (format "%s -1 %s"
                          (executable-find "zipinfo" (file-remote-p default-directory))
                          codeql--database-source-archive-zip))))
    (when zipinfo
      (let ((zipinfo-relative-paths
             (cl-map 'list (lambda (file) file) (split-string zipinfo "\n"))))))))

(defun codeql--database-extract-source-archive-zip (trusted-root)
  "Return extraction-relative file listing for source archive zip."
  (message "archive-root: %s" (codeql--file-truename codeql--database-source-archive-root))
  (message "trusted-root: %s" (codeql--file-truename trusted-root))
  (if (not (codeql--file-exists-p (codeql--file-truename codeql--database-source-archive-root)))
      (when (eql (string-match (codeql--file-truename trusted-root)
                               (codeql--file-truename codeql--database-source-archive-root)) 0)
        ;; XXX: double check a malicious zip can't traverse out.
        ;; XXX: double check codeql--database-source-archive-root for traversals.
        ;; XXX: are malicious databases part of our threat model? decide.
        (message "Source root verified to exist in trusted path ... extracting.")
        (codeql--shell-command-to-string
         (format "%s %s -d %s"
                 ;; XXX: double check that executable find works under TRAMP context
                 (executable-find "unzip" (file-remote-p default-directory))
                 (codeql--file-truename codeql--database-source-archive-zip)
                 (codeql--file-truename codeql--database-source-archive-root))))
    (message "Source archive already extracted.")
    t))

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
               :on-shutdown #'codeql--query-server-on-shutdown))
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
        (codeql-transient-query-server-stop)
      (transient-setup 'codeql-transient-query-server-init-start))))

;;; jsonrpc interactions with the buffer-local query server

(cl-defmethod jsonrpc-connection-ready-p
  (server (method (eql :evaluation/registerDatabases)))
  "Provide synchronization for register/unregister."
  ;; wait until any buffer local deregistration has completed
  (if codeql--active-database
      nil
    t))

(defun codeql-query-server-register-database (&optional database-path)
  "Register a database with the query server."
  (interactive)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)

  ;; if no server is running yet, offer to start one
  (unless codeql--query-server
    (transient-setup 'codeql-transient-query-server-init-start)
    (error "No query server running, please start one and re-select database."))

  ;; init the database history if need be
  (unless codeql--registered-database-history
    (setq codeql--registered-database-history (make-hash-table :test #'equal)))

  (let ((database-path
         (or database-path
             ;; note that this doesn't return plain strings in remote context
             ;; but rather a tramp file object ...
             (read-file-name "Database: " nil default-directory t))))

    ;; resolve and set the dataset folder, we need this when running queries
    (let* ((database-info (codeql--database-info database-path))
           (database-language (json-pointer-get database-info "/languages/0"))
           (source-location-prefix (json-pointer-get database-info "/sourceLocationPrefix"))
           (source-archive-zip (json-pointer-get database-info "/sourceArchiveZip"))
           (source-archive-root (json-pointer-get database-info "/sourceArchiveRoot"))
           (database-dataset-folder (json-pointer-get database-info "/datasetFolder")))

      (message "XXX: database language: %s" database-language)

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
       (jsonrpc-lambda (&key code message _data &allow-other-keys)
         (message "Error %s: %s %s" code message _data))
       ;; synchronize to only register when deregister has completed in global state
       :deferred :evaluation/registerDatabases
       ))))

(defun codeql-query-server-deregister-database (database-dataset-folder)
  "Deregister DATABASE with the query server."
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
         (codeql--reset-def-ref-ast-cache)
         (codeql--reset-database-state)
         (message "Deregistered: %s" database-dataset-folder))))
   :error-fn
   (jsonrpc-lambda (&key code message _data &allow-other-keys)
     (message "Error %s: %s %s" code message _data))
   :deferred :evaluation/deregisterDatabases
   ))

(defun codeql-query-server-active-database ()
  "Get a short identifier for the currently active database."
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (if codeql--active-database
      (file-name-base
       (directory-file-name
        codeql--active-database))
    "(no active db)"))

(defvar codeql-run-query-always-save t
  "Whether you always want to save prior to query evaluation.

Evaluation requires a synced position in the query file on disk, such that the
query server can grab the right contents.

This applies to both normal evaluation and quick evaluation.")

;;; deal with utf-16 code points for column calculations, modified from eglot
(defun codeql-lsp-abiding-column ()
  "Calculate current COLUMN as defined by the LSP spec."
  ;; codeql query server uses 1-based column values and utf-16 code points
  (/ (length
      (encode-coding-region
       (line-beginning-position)
       (point) 'utf-16 t))
     2))

;; XXX: this feels a little crummy still, revisit
(defun codeql--uri-to-filename (uri)
  (cl-assert (stringp uri) t)
  (let ((template "%%SRCROOT%%")
        (resolved-path uri))
    ;; do any source prefixing required based on global db info
    (when (string-match template uri)
      ;;(message "XXX: uri transform on: %s" uri)
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

(defun codeql--escape-org-description (description)
  "Replace org-link special with something harmless."
  ;; XXX: have to find some unicode variant that fits nicely here without breaking alignments
  (replace-regexp-in-string "\\[\\|\\]" (lambda (x) (if (string= x "[") "|" "|")) description))

;; XXX: this needs to deal with various corner cases, build as we use and run into bugs
(defun codeql--node-to-org (node &optional custom-description)
  "Transform a result node into an org compatible link|string representation."
  ;; https://orgmode.org/guide/Hyperlinks.html
  (if (codeql--result-node-visitable node)
      (progn
        (cl-assert codeql--database-source-archive-zip t)
        (cl-assert (codeql--file-exists-p codeql--database-source-archive-root) t)
        (let ((filename (codeql--result-node-filename node)))
          ;; XXX: to alert on any wacky file schemes we might need to support
          ;; we implement a custom org link type so we can add extra sauce on link follow
          (when filename (cl-assert (not (string-match ":" filename)))))
        (let ((link (format "codeql:%s/%s::%s"
                            ;; we've extracted to the expected location
                            (codeql--tramp-wrap codeql--database-source-archive-root)
                            (codeql--result-node-filename node)
                            (codeql--result-node-line node)))
              (desc (codeql--result-node-label node)))
          (format "[[%s][%s]]" (org-link-escape link) (codeql--escape-org-description
                                                       (or custom-description desc)))))
    ;; not visitable, just return the label
    (format "%s" (codeql--result-node-label node))))

(defun codeql--org-list-from-nodes (parent-buffer nodes &optional header)
  "Turn a given list of PARENT-BUFFER context location NODES into an org-list, setting an optional HEADER."
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
                        (codeql--node-to-org node))))
                    (suffix
                     (if-let ((url (codeql--result-node-url node)))
                         (format "%s:%s:%s"
                                 ;; set a custom description for this link
                                 (with-current-buffer parent-buffer
                                   (codeql--node-to-org node
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
                                                                     (codeql--node-to-org node))))
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
                                           (codeql--node-to-org
                                            location
                                            (codeql--result-node-label issue)))
                                         (with-current-buffer parent-buffer
                                           (codeql--node-to-org
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

(defun codeql--org-render-sarif-results (org-data &optional footer parent-buffer)
  "Render ORG-DATA including an optional FOOTER."
  ;; sarif results are org trees
  (let ((buffer (generate-new-buffer "* codeql-org-results *")))
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
        ;; so we can make the query server context available to org links
        (when parent-buffer
          (setq-local codeql--org-parent-buffer parent-buffer))
        (switch-to-buffer-other-window buffer))
      (buffer-string))))

(defun codeql--org-render-raw-query-results (org-data &optional footer parent-buffer)
  "Render ORG-DATA including an optional FOOTER."
  ;; raw results are org tables
  (let ((buffer (generate-new-buffer "* codeql-org-results *")))
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
        ;; so we can make the query server context available to org links
        (when parent-buffer
          (setq-local codeql--org-parent-buffer parent-buffer))
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
   :filename nil
   :line nil
   :visitable nil
   :url nil
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
   (codeql--nodes-from-result-locations
    locations message)
   :related-locations
   ;; this returns a list of related-location nodes
   (codeql--nodes-from-result-locations
    related-locations message)))

(defun codeql--paths-from-thread-flows (thread-flows)
  "Returns all paths out of THREAD-FLOWS."
  ;; collect all paths out of thread-flows
  (cl-loop for thread-flow across thread-flows
           for locations = (json-pointer-get thread-flow "/locations")
           ;; collect a path
           collect (codeql--path-nodes-from-thread-flow-locations locations)))

(defun codeql--nodes-from-result-locations (locations message)
  "Returns a list of location nodes from result LOCATIONS."
  ;; note, this operates on an array of location objects, this is not the same
  ;; as the top level locations which are passed for a result ...

  ;; collect all path nodes out of locations
  (cl-loop for location across locations
           for i below (length locations)
           for uri = (json-pointer-get location "/physicalLocation/artifactLocation/uri")
           for uri-base-id = (json-pointer-get location "/physicalLocation/artifactLocation/uriBaseId")
           for region = (json-pointer-get location "/physicalLocation/region")
           ;; collect a node in a path
           collect
           (let* ((uri (if uri-base-id
                           (format "file:%s/%s" uri-base-id uri)
                         uri))
                  (node
                   (codeql--result-node-create
                    :label (or (json-pointer-get location "/message/text") message)
                    :mark "-->"
                    :filename (or (codeql--uri-to-filename uri) uri)
                    :line  (json-pointer-get region "/startLine")
                    :visitable region
                    ;; build a json alist to parse out for url
                    :url `(
                           ,(cons 'uri uri)
                           ,(cons 'startLine (json-pointer-get region "/startLine"))
                           ,(cons 'startColumn (json-pointer-get region "/startColumn"))
                           ,(cons 'endColumn (json-pointer-get region "/endColumn"))))))
             ;;(codeql--print-node node)
             node)))

(defun codeql--path-nodes-from-thread-flow-locations (locations)
  "Returns a list of location nodes from thread-flow LOCATIONS."
  ;; note, this operates on an array of location objects, this is not the same
  ;; as the top level locations which are passed for a result ...

  ;; collect all path nodes out of locations
  (cl-loop for location across locations
           for i below (length locations)
           for uri = (json-pointer-get location "/location/physicalLocation/artifactLocation/uri")
           for uri-base-id = (json-pointer-get location "/location/physicalLocation/artifactLocation/uriBaseId")
           for region = (json-pointer-get location "/location/physicalLocation/region")
           ;; collect a node in a path
           collect
           (let* ((uri (if uri-base-id
                           (format "file:%s/%s" uri-base-id uri)
                         uri))
                  (path-node
                   (codeql--result-node-create
                    :label (json-pointer-get location "/location/message/text")
                    :mark (cond ((eql i 0)
                                 ;; source for a flow
                                 "src")
                                ((eql i (1- (length locations)))
                                 ;; sink for a flow
                                 "snk")
                                (t
                                 ;; step for a flow
                                 "..."))
                    :filename (or (codeql--uri-to-filename uri) uri)
                    :line  (json-pointer-get region "/startLine")
                    :visitable region
                    ;; build a json alist to parse out for url
                    :url `(
                           ,(cons 'uri uri)
                           ,(cons 'startLine (json-pointer-get region "/startLine"))
                           ,(cons 'startColumn (json-pointer-get region "/startColumn"))
                           ,(cons 'endColumn (json-pointer-get region "/endColumn"))))))
             ;; ok we have a path-node, add it to the path list
             path-node)))

;; template queries for definitions, references, and AST

(defun codeql--templated-query-path (language query-name)
  "Return the relative path for a QUERY-NAME of type LANGUAGE."
  (when-let (fmt(plist-get codeql--templated-query-formats language))
    (format fmt query-name)))

(defun codeql--archive-path-from-org-filename (filename)
  ;; do a dance to normalize back to the archive root relative path for this filename
  (format "/%s" (cadr (split-string filename (format "%s/*" codeql--database-source-archive-root)))))

(defun codeql--run-templated-query (language query-name filename)
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (cl-assert codeql--active-database t)

  (message "XXX: running templated query! %s %s %s" language query-name filename)
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
                          ,(codeql--archive-path-from-org-filename (codeql--tramp-unwrap filename)))])))))
               (codeql--query-server-run-query-from-path full-path nil template-values filename))
             ;; respect search precedence and only return from first find
             (cl-return))))

(defun codeql--database-src-path-p (path)
  (let ((prefix (format "^%s/*%s"
                        codeql--database-source-archive-root
                        codeql--database-source-location-prefix)))
    (when
        (string-match
         prefix
         (codeql--tramp-unwrap path))
      t)))

;; custom org link so we can do voodoo when C-c C-o on a codeql: link

;; XXX: add an org link type of "codeql:" with associated handlers
(require 'ol)

;; XXX: build a hashmap of source-file to defs/refs or something?
;; XXX: and then make that available in the parent context as cache?
(defun codeql--org-open-file-link (filename)
  (message  "XXX: opening filename: %s" filename)
  (cl-multiple-value-bind (filename line) (split-string filename "::")
    (when (bound-and-true-p codeql--org-parent-buffer)
      (with-current-buffer codeql--org-parent-buffer
        (when codeql--active-database-language
          ;; we have an active database in our parent buffer context
          ;; so we'll use that to resolve definitions and references
          (let ((language (intern (format ":%s" codeql--active-database-language))))
            (message "XXX: checking filename: %s" filename)
            (when (codeql--database-src-path-p filename)
              (message "XXX: handling a file from database source archive")
              ;; don't process more than once, caches are buffer local
              (unless (and codeql--definitions-cache (gethash filename codeql--definitions-cache))
                (codeql--run-templated-query language "localDefinitions" filename))
              (unless (and codeql--references-cache (gethash filename codeql--references-cache))
                (codeql--run-templated-query language "localReferences"  filename)))))))
    (org-open-file filename t (if line (string-to-number line) line))))

(org-link-set-parameters "codeql" :follow #'codeql--org-open-file-link)

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

;; note: filenames in these hash tables will include their tramp prefixes

(defun codeql--process-defs (json filename)
  (message "XXX: processing definitions for: %s" filename)
  (unless codeql--definitions-cache
    (setq codeql--definitions-cache (make-hash-table :test #'equal)))
  (puthash filename json codeql--definitions-cache))

(defun codeql--process-refs (json filename)
  (message "XXX: processing references for: %s" filename)
  (unless codeql--references-cache
    (setq codeql--references-cache (make-hash-table :test #'equal)))
  (puthash filename json codeql--references-cache))

(defun codeql--print-ast (json filename)
  (message "XXX: processing AST for: %s" filename)
  (unless codeql--ast-cache
    (setq codeql--ast-cache (make-hash-table :test #'equal)))
  (puthash filename json codeql--ast-cache))

;; abandon hope, all ye who enter here ...
(defun codeql-load-bqrs (bqrs-path query-path db-path query-name query-kind query-id buffer-context &optional src-filename)
  "Parse the results at BQRS-PATH and render them accordingly to the user."

  (message "Loading bqrs from %s (name: %s kind: %s id: %s)"
           bqrs-path
           (or query-name "no-meta")
           (or query-kind "no-meta")
           (or query-id "no-meta"))

  (when-let ((bqrs-info (codeql--bqrs-info bqrs-path)))
    (let ((result-sets (json-pointer-get bqrs-info "/result-sets"))
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
                                     "#+DB_PATH: %s"
                                     "#+BEGIN_CODEQL_VERSION"
                                     "%s"
                                     "#+END_CODEQL_VERSION") "\n")
             (current-time-string)
             bqrs-path
             query-path
             db-path
             codeql--cli-info) "\n")))
      ;; parse results according to the query meta data
      (cond

       ;; AST parsing
       ;; process AST's, definitions, and references

       ((or (string-match "/localDefinitions.ql$" query-path)
            (string-match "/localReferences.ql$" query-path)
            (string-match "/printAst.ql$" query-path))
        (let* ((json (json-parse-string
                      (codeql--bqrs-to-json bqrs-path "id,url,string")
                      ;; json-pointer-get wants list based json
                      :object-type 'alist)))
          (when json
            (message "XXX: parsed a templated query! DO SOMETHING BOUT IT!")
            (cond ((string-match "/localDefinitions.ql$" query-path)
                   (codeql--process-defs json src-filename))
                  ((string-match "/localReferences.ql$" query-path)
                   (codeql--process-refs json src-filename))
                  ((string-match "/printAst.ql$" query-path)
                   (codeql--print-ast json src-filename))))))

       ;; SARIF parsing
       ;;
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

       ((or (and (seq-contains-p compatible-query-kinds "PathProblem")
                 (string= query-kind "path-problem")
                 query-id)

            ;; this is a problem
            (and (seq-contains-p compatible-query-kinds "Problem")
                 (string= query-kind "problem")
                 query-id))

        (let ((json (json-parse-string
                     (codeql--bqrs-to-sarif bqrs-path query-id query-kind)
                     :object-type 'alist)))

          (cl-assert json t)

          ;; render any available runs
          (cl-loop
           with runs = (json-pointer-get json "/runs")
           ;; runs is a vector
           for run across runs
           for run-id below (length runs)
           do
           (message "Rendering results for run %s" run-id)
           (let* ((results (json-pointer-get run "/results"))
                  (artifacts (json-pointer-get run "/artifacts")))

             ;; check if we have any artifact contents, TODO: do something with 'm?
             (let ((has-artifact-contents nil))
               ;; artifacts is a vector
               (cl-loop for artifact across artifacts
                        for contents = (json-pointer-get artifact "/contents")
                        do (when contents
                             (message "Sarif artifact contents detected!")
                             (setq has-artifacts-contents t)))

               ;; alrighty, let's start processing some results into org data
               (let ((org-results
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
                 (when org-results
                   ;; save off our results so we don't have to re-render for history
                   (with-temp-buffer
                     (cl-loop for org-data in org-results do (insert org-data))
                     (let ((rendered
                            (codeql--org-render-sarif-results (buffer-string) footer buffer-context)))
                       ;; save off the fully rendered version for speedy re-loads
                       (with-temp-file (format "%s.org" bqrs-path)
                         (insert rendered)))))))))))

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
                           (cl-loop for element across tuple do
                                    (cond

                                     ;; object with url info
                                     ((and (eq (type-of element) 'cons)
                                           (json-pointer-get element "/url"))
                                      (let ((node (codeql--result-node-create
                                                   :label (json-pointer-get element "/label")
                                                   :mark "→"
                                                   :filename (codeql--uri-to-filename
                                                              (json-pointer-get element "/url/uri"))
                                                   :line (json-pointer-get element "/url/startLine")
                                                   :visitable t
                                                   :url (json-pointer-get element "/url"))))
                                        ;;(message "XXX url node: %s" node)
                                        (cl-pushnew node row-data)))

                                     ;; object with no url info
                                     ((and (eq (type-of element) 'cons)
                                           (not (json-pointer-get element "/url")))
                                      (let ((node (codeql--result-node-create
                                                   :label (json-pointer-get element "/label")
                                                   :mark "≔"
                                                   :filename nil
                                                   :line nil
                                                   :visitable nil
                                                   :url nil)))
                                        ;;(message "XXX no-url node: %s" node)
                                        (cl-pushnew node row-data)))

                                     ;; string|number literal
                                     ((or (eq (type-of element) 'string)
                                          (eq (type-of element) 'integer))
                                      (let ((node (codeql--result-node-create
                                                   :label element
                                                   :mark "≔"
                                                   :filename nil
                                                   :line nil
                                                   :visitable nil
                                                   :url nil)))
                                        ;;(message "XXX literal node: %s" node)
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
                (let ((rendered (codeql--org-render-raw-query-results org-data footer buffer-context)))
                  (with-temp-file (format "%s.org" bqrs-path) (insert rendered))))))))))))

(defun codeql--query-server-request-run (buffer-context qlo-path bqrs-path query-path query-info db-path quick-eval &optional template-values src-filename)
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
      (jsonrpc-async-request
       (codeql--query-server-current-or-error)
       :evaluation/runQueries run-query-params
       :success-fn
       (lexical-let ((buffer-context buffer-context)
                     (query-info query-info)
                     (bqrs-path bqrs-path)
                     (query-path query-path)
                     (db-path db-path)
                     (quick-eval quick-eval)
                     (src-filename src-filename))
         (jsonrpc-lambda (&rest _)
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
                          codeql--completed-query-history))
                       ;; display results
                       (codeql-load-bqrs bqrs-path query-path db-path name kind id buffer-context src-filename))))
               (message "No query results in %s!" bqrs-path)))))
       :error-fn
       (jsonrpc-lambda (&key code message _data &allow-other-keys)
         (message "Error %s: %s %s" code message _data))
       :timeout-fn
       (jsonrpc-lambda (&rest _)
         (message ":evaluation/runQueries timed out."))
       :deferred :evaluation/runQueries))))

;; XXX: too many args, move all of those to passing a struct around instead
(defun codeql--query-server-request-compile-and-run (buffer-context library-path qlo-path bqrs-path query-path query-info db-path db-scheme quick-eval &optional template-values src-filename)
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
                      (start-col (save-excursion (goto-char min) (codeql-lsp-abiding-column)))
                      (end-col (save-excursion (goto-char max) (codeql-lsp-abiding-column))))
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
      (jsonrpc-async-request
       (codeql--query-server-current-or-error)
       :compilation/compileQuery compile-query-params
       :success-fn
       (lexical-let ((buffer-context (current-buffer))
                     (bqrs-path bqrs-path)
                     (qlo-path qlo-path)
                     (query-path query-path)
                     (query-info query-info)
                     (db-path db-path)
                     (quick-eval quick-eval)
                     (template-values template-values)
                     (src-filename src-filename))
         (jsonrpc-lambda (&key messages &allow-other-keys)
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
                                                 src-filename)))))
       :timeout-fn
       (jsonrpc-lambda (&rest _)
         (message ":compilation/compileQuery timed out."))
       :error-fn
       (jsonrpc-lambda (&key code message _data &allow-other-keys)
         (message "Error %s: %s %s" code message _data))
       :deferred :compilation/compileQuery))))

(defun codeql--query-server-run-query-from-path (query-path quick-eval &optional template-values src-filename)
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
                                                  src-filename)))

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
        (when (or codeql-run-query-always-save
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
    ;; XXX: do we want to serialize database history state to disk?
    (message "No database history available across any sessions.")))

;;; transient ui for interacting with the buffer-local query server

(defun codeql-set-max-paths (max-paths)
  "Configure the buffer-local value for path problem max paths."
  (interactive "n--max-paths=")
  (cl-assert (eq major-mode 'ql-tree-sitter-mode) t)
  (setq codeql--path-problem-max-paths max-paths))

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
           ("k" "known" codeql-database-history)]
          ["Query"
           ("r" "run" codeql-query-server-run-query)
           ("h" "history" codeql-query-history)]
          ["Config"
           ("p" codeql-set-max-paths
            :description
            (lambda ()
              (format "--max-paths=%s"
                      codeql--path-problem-max-paths)))]]
  (interactive)
  (when (eq major-mode 'ql-tree-sitter-mode)
    (transient-setup 'codeql-transient-query-server-interact)))

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
