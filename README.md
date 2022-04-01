# emacs-codeql

An emacs package for writing and testing [CodeQL](https://codeql.github.com/) queries.

![screenshot](img/codeql-dot-el.png?raw=true "emacs-codeql")

## Features 

- [Tree-sitter](https://tree-sitter.github.io/tree-sitter/) based syntax highlighting and indentation for the CodeQL query language
- Query execution
- Quick query evaluation
- Query history for the local session
- Database history for the global session
- Result browsing
- Source archive location opening
- Buffer local query server state with concurrent remote|local project, database and query support
- Automatic database selection deconfliction
- Org based rendering of path-problem, problem, and raw results
- Remote query development + evaluation via TRAMP
- Compatible with [github/gh-codeql](https://github.com/github/gh-codeql) for cli version management

## Requirements

- Emacs 27.1+
  - transient
  - tree-sitter
  - tree-sitter-langs
  - tree-sitter-indent
  - aggressive-indent
  - projectile
  - eglot
- CodeQL CLI 2.8.3+ 

## Installation

### Recommended install method

`use-package` + [quelpa](https://github.com/quelpa/quelpa) for non-MELPA package installs:

```elisp
(use-package emacs-codeql
  :quelpa
  (emacs-codeql :repo "anticomputer/emacs-codeql"
		:fetcher github-ssh
		:branch "main"
		:files (:defaults "bin"))
  :after tree-sitter-langs
  :demand
  :init
  (setq codeql-transient-binding "C-c q")
  (setq codeql-configure-eglot-lsp t)
  (setq codeql-configure-projectile t)
  :config
  ;; you should configure your standard search paths through a ~/.config/codeql/config entry
  ;; e.g. "--search-path /full/path/codeql:/full/path/codeql-go"
  ;; see: https://codeql.github.com/docs/codeql-cli/specifying-command-options-in-a-codeql-configuration-file/
  ;; this option is here to provide you with load/search precedence control
  ;; these paths will have precedence over the config file search paths
  (setq codeql-search-paths '("./"))
```

### Alternative install method

Alternatively, you can clone this repository and place it into your emacs `load-path`, you'll want to ensure that you have all required dependencies available, including the most recent MELPA version of eglot.

```elisp
;; initialization options
(setq codeql-transient-binding "C-c q")
(setq codeql-configure-eglot-lsp t)
(setq codeql-configure-projectile t)

(require 'emacs-codeql)

;; configuration

;; you should configure your standard search paths through a ~/.config/codeql/config entry
;; e.g. "--search-path /full/path/codeql:/full/path/codeql-go"
;; see: https://codeql.github.com/docs/codeql-cli/specifying-command-options-in-a-codeql-configuration-file/
;; this option is here to provide you with load/search precedence control
;; these paths will have precedence over the config file search paths
(setq codeql-search-paths '("./"))
```

### Configuring the CodeQL CLI

There's multiple ways you can bootstrap the codeql cli tool chain for query development. We'll outline two of the more popular strategies here.

#### 1: Use a bootstrap script to setup a codeql cli and library repo siblings

For convenience, `emacs-codeql` provides a [cli bootstrap script](https://raw.githubusercontent.com/anticomputer/emacs-codeql/main/tools/bootstrap-codeql-cli.sh) which will bootstrap the codeql cli in a given current working directory. You can run this script, paste its resulting search path configuration into your `emacs-codeql` config, and you're off to the races.

If you bootstrap into `~/codeql-home`, then `~/codeql-home/codeql-repo` is a clone of https://github.com/github/codeql and `~/codeql-home/codeql-go` is a clone of https://github.com/github/codeql-go

`emacs-codeql` expects the codeql cli to exist in your `PATH`. Follow the standard [codeql cli setup instructions](https://codeql.github.com/docs/codeql-cli/getting-started-with-the-codeql-cli/) to get the cli bootstrapped and add its location to your executable search `PATH`.

#### 2: Using the GitHub CLI codeql extension

If you use the gh cli, then you can also use the `github/gh-codeql` cli extension to initialize and configure your codeql cli:

TL;DR:

```shell
# clone codeql and codeql repos and configure codeql search paths
echo "--search-path /home/codespace/codeql-home/codeql-repo:/home/codespace/codeql-home/codeql-go-repo" >> ~/.config/codeql/config

# install codeql extension
gh extension install github/gh-codeql

# verify search paths are configured correctly
gh codeql resolve qlpacks
```

You'll still need to clone the `github/codeql` and `github/codeql-go` repositories as with the bootstrap script and make them available in the search path of the codeql cli.

Once the gh cli codeql extension is installed and configured, `emacs-codeql` will automagically use it both locally and remotely pending the value of `codeql-use-gh-codeql-extension-when-available` whenever its available.

If unavailable, `emacs-codeql` will fall back to its normal executable path expectations, so you can safely keep this value set to `t`, which is the default.

#### Custom search paths

We recommend you use [`.config/codeql/config`](https://codeql.github.com/docs/codeql-cli/specifying-command-options-in-a-codeql-configuration-file/) for your codeql cli search path configurations. This allows for more straightforward standard search path deconfliction between local and remote query contexts.

Ensure your codeql cli configuration contains your search paths in `.config/codeql/config` on any local or remote system that you intend to use the codeql cli on via `emacs-codeql`.

```
λ ~ › cat ~/.config/codeql/config 
--search-path /home/codespace/codeql-home/codeql-repo:/home/codespace/codeql-home/codeql-go-repo
λ ~ › 
```
In this example `~/codeql-home/codeql-repo` is a clone of https://github.com/github/codeql and `~/codeql-home/codeql-go` is a clone of https://github.com/github/codeql-go

Use `codeql resolve qlpacks` to double check that you can resolve all the expected qlpacks available in the default repositories.

The `codeql-search-paths` variable in `emacs-codeql` exists purely to provide you with search path precedence control. Unless you're wanting to override any of the default qlpacks from locations outside of the current project root, the existing and default value of '("./") suffices.

`codeql-search-paths` entries are expanded in the context of their location, so if you do want to configure search paths that work across both local and remote systems, you're able to e.g. use a standard "~/path" notation, which will expand to the correct absolute path in the context of the remote or local system via TRAMP.


### Optional: build custom tree-sitter QL artifacts

If this package does not contain the required `tree-sitter-langs` artifacts for your system, you may also need to install the following to be able to build the required QL `tree-sitter-langs` support for your system.

```elisp
(use-package tree-sitter
  :quelpa
  (tree-sitter
   :repo "anticomputer/elisp-tree-sitter-2"
   :fetcher github
   :branch "anticomputer-ql"
   :files ("lisp/*.el"
           (:exclude "lisp/tree-sitter-tests.el")))
  :demand
  :ensure t)

(use-package tree-sitter-langs
  :quelpa
  (tree-sitter-langs
   :repo "anticomputer/tree-sitter-langs-1"
   :fetcher github
   :branch "anticomputer-ql"
   :files (:defaults
           "queries"))
  :after tree-sitter
  :demand
  :ensure t)
```

Follow my [tree-sitter-langs fork documentation](https://github.com/anticomputer/tree-sitter-langs-1) for instructions on how to build such an artifact.

TL;DR: 

1. Install cask. 
2. run `cask install` for my fork (`git clone --branch anticomputer-ql git@github.com:/anticomputer/tree-sitter-langs-1`) of `tree-sitter-langs` from its project root. 
3. Download a copy of the [binary release](https://github.com/tree-sitter/tree-sitter/releases/tag/v0.19.5) of `tree-sitter` and put it in your `PATH` somewhere. 
4. Compile the QL support using `script/compile ql` from `tree-sitter-langs` project root.

NOTE:

`tree-sitter` version <0.20 is required due to a breaking change in >= 0.20, I use 0.19.5.

If you're on a Mac, you'll also need to set `EMACSDATA` and `EMACSLOADPATH` correctly, e.g:

```
EMACSDATA=/Applications/MacPorts/EmacsMac.app/Contents/Resources/etc EMACSLOADPATH=/Applications/MacPorts/EmacsMac.app/Contents/Resources/lisp cask install
EMACSDATA=/Applications/MacPorts/EmacsMac.app/Contents/Resources/etc EMACSLOADPATH=/Applications/MacPorts/EmacsMac.app/Contents/Resources/lisp script/compile ql
```

After building the QL artifact(s) for the architectures and platforms you need to support, you'll want to grab `bin/ql.*` and move those into `~/.emacs.d/elpa/tree-sitter-langs-*/bin/` and restart emacs. 

QL syntax highlighting and indentation should now work fine.

## Usage

### Query projects

Create a project root containing a `qlpack.yml` QL pack definition and then create query files inside this project.

NOTE: make sure that you enable the `projectile` configuration option if you intend to use LSP support via `eglot`, as `eglot` needs to send the project root as part of codeql langserver `workspaceFolders` initialization.

For example, to start a Javascript query project, `projectroot/qlpack.yml` could contain:

```yaml
---
library: false
name: testpack
version: 0.0.1
libraryPathDependencies: codeql/javascript-all
```

See [CodeQL packs documentation](https://codeql.github.com/docs/codeql-cli/about-ql-packs/) for additional information.

### Query files

Edit a `.ql` or `.qll` file inside a `qlpack.yml` project root and run `M-x codeql-transient-query-server-interact` to open the `emacs-codeql` transient. This transient is bound to the value of `codeql-transient-binding` on initialization, which may be customized by the user by setting `codeql-transient-binding` to a keybinding of their liking.

#### Syntax highlighting and indentation

`emacs-codeql` includes a `tree-sitter` based major-mode that provides indentation and syntax highlighting for `.ql` and `.qll` files. It relies on `aggressive-indent` for continuous indentation, as is common practice in structural editing modes.

`emacs-codeql` relies on a custom version of `tree-sitter-langs` that includes `tree-sitter` support for QL. This support has not been upstreamed as of 03-27-2022, but package recipes for custom forks are provided above. 

`emacs-codeql` also packages QL `tree-sitter` artifacts for Linux x64 and MacOS x64 systems, which may preclude the need to build your own artifacts. If your system is compatible with the existing artifacts, you are not required to install the custom forks of `tree-sitter`, `tree-sitter-langs` and `tree-sitter-indent` and can just use the existing MELPA versions.

### Database selection

You can register a database for your current project via `M-x codeql-transient-query-server-interact` and selecting `d` to register an active database. Ensure you have started a query server first by using `s` from the same menu. If a database is active in another query buffer, it will automatically be deregistered there and registered for your current buffer. If a database has been registered in the global emacs session before, you can fetch it more rapidly via selecting `k` for known database history.

Database selection is buffer-local per query file and you can run as many concurrent query projects as you have resources available. Database history allows you to rapidly register/unregister databases that were seen across all sessions.

`emacs-codeql` buffer-local approach means you can work on e.g. a `cpp` and `javascript` project concurrently, and you can an arbitrary amount of concurrent results buffers as you toggle between your query buffers.

You can find existing databases for a ton of open source projects on [LGTM.com](https://codeql.github.com/docs/codeql-cli/creating-codeql-databases/#obtaining-databases-from-lgtm-com), or you can [create your own](https://codeql.github.com/docs/codeql-cli/creating-codeql-databases/#creating-codeql-databases) using the codeql cli. 

### Run a query or eval predicates

If there is an active region, `emacs-codeql` will attempt to quick-eval that region. Note that to quick-eval an entire predicate, you should mark the predicate name itself as opposed to the entire predicate definition. This follows the same semantics as quick-evaluation in the vscode extension.

If there is no active region, `emacs-codeql` will attempt to run the full query by default.

## Result rendering

Results are rendered through `org-mode` buffers, which allows you to save results as normal text files and even read them in other editors. This approach also allows you to turn a result buffer into a living audit document, take notes, and do all the things that `org-mode` allows you to do.

The following query "kinds" are supported: `path-problem`, `problem`, and raw tuple results.

kind: `problem` and `path-problem` are rendered as org trees, raw tuple results are rendered as org tables.

Results contain source code locations in the form of org links, which can be visited with the normal org mode operations for link handling (e.g. `org-open-at-point`)

`emacs-codeql` will resolve file paths into the project snapshot source code archive included with the database archive. 

## Commands

All query server interaction for your query buffer routes via an intuitive `transient` interface. You can configure which keybinding is associated with starting this interface via the `codeql-transient-binding` variable. 

This interface is self-documenting and allows you to start/stop query servers, select databases, run queries, and fetch query history. 

Otherwise hidden features such as max path depths for path queries are unobtrusively surfaced to the user.

## TRAMP support

`emacs-codeql` is fully TRAMP compatible, which means you can edit a query file over TRAMP and as long as `emacs-codeql` can find, or is told where to find, the codeql cli installation on the remote end, everything will work just as if you were working locally. Yes, this means you can develop codeql queries without any locally installed dependencies from your emacs. Science.

![screenshot](img/codeql-over-tramp.png?raw=true "emacs-codeql")

On emacs 28.0.92, TRAMP version (2.5.2) provides a fairly trouble free experience on Linux even with the LSP enabled, at least in my experience. On emacs 27.2, with the older version of TRAMP, I've had less luck with the LSP experience, but the query server performs fine over TRAMP. `emacs-codeql` will ask for confirmation to enable the LSP when it detects it is running in a remote context. When in doubt, keep it disabled.

For optimal performance over TRAMP, especially if you're expecting very large (multiple thousands) of result sets, I recommend the following settings:

```elisp
;; be ok with large process outputs
(setq read-process-output-max (* 1024 1024))

;; disable file cache, vc registration in tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
              
;; shush tramp, shush
(setq tramp-verbose 1)

;; ensure projectile doesn't trigger tramp ops
(setq projectile-mode-line-function (lambda () "Projectile"))
```

### GitHub Codespaces access over TRAMP

I use the following config to enable convenient GitHub Codespaces access from emacs over TRAMP:


```elisp
;; add gh codespaces ssh method support for tramp editing
;; e.g. C-x C-f /ghcs:codespace-name:/path/to/file :)
(let ((ghcs (assoc "ghcs" tramp-methods))
      (ghcs-methods '((tramp-login-program "gh")
                      (tramp-login-args (("codespace") ("ssh") ("-c") ("%h")))
                      (tramp-remote-shell "/bin/sh")
                      (tramp-remote-shell-login ("-l"))
                      (tramp-remote-shell-args ("-c")))))
  ;; just for debugging the methods
  (if ghcs (setcdr ghcs ghcs-methods)
    (push (cons "ghcs" ghcs-methods) tramp-methods)))

;; provide codespace name completion for ghcs tramp method
;; use C-j if you use ivy to kick in host completion
(defun my/tramp-parse-codespaces (&optional nop)
  (let ((results '())
        (codespaces
         (split-string
          (shell-command-to-string
           "gh codespace list --json name -q '.[].name'"))))
    (dolist (name codespaces)
      ;; tramp completion expects a list of (user host)
      (add-to-list 'results (list nil name)))
    results))

(tramp-set-completion-function "ghcs" '((my/tramp-parse-codespaces "")))
```

With the completion function in place, you can just TAB to get a list of your available Codespaces when opening a `/ghcs:` prefixed file.

## Language Server Protocol

`emacs-codeql` performs very well with `eglot`. Due to the codeql language server relying on `workspaceFolders` support, `eglot 20220326.2143` or newer is required from MELPA, which includes the basic project-root based `workspaceFolders` introduced in: https://github.com/joaotavora/eglot/commit/9eb9353fdc15c91a66ef8f4e53e18b22aa0870cd

Projectile and eglot configurations are included in `emacs-codeql` and controlled by the `codeql-configure-eglot-lsp` and `codeql-configure-projectile` variables, respectively.

It is HIGHLY recommended to enable both eglot and projectile configurations, as they depend on each other and provide the most pleasant query editing experience.

`emacs-codeql` has not been tested in conjunction with `lsp-mode`.

### LSP + TRAMP

While the recommended LSP client, `eglot`, does function over TRAMP, running LSP over TRAMP can be very quirky and laggy unless you're on the latest emacs and TRAMP versions. Slow server initialization can be blocking to the point of full freezes. When operating in a TRAMP context, `emacs-codeql` will ask you if you want to enable the LSP client, unless you reaaaally want it, I recommend disabling it for remote editing and query debugging.

The codeql query server, while also running jsonrpc over stdio, is not tied to a direct editor feedback loop, so it is a much more pleasant experience over TRAMP, so all local functionality is enabled and available.

Having said that, on emacs 28.0.92, TRAMP version (2.5.2) provides a fairly trouble free experience on Linux at least even with LSP enabled remotely.

## Known Quirks

### The very first run of a newly created query file compiles but does not actually run

You can re-run the query and it will execute fine on the second run and any subsequent runs against any other database. Seemingly on the very first run of a new query file the query server may not report success on compilation completion. This happens only for the first run of a newly created query file. I'm debugging this still, but it's a rare enough event to not cause too much friction. It also works fine across restarts of emacs and for any iterations on the actual contents of the query after the very first compilation.

The symptom of this is the last message displayed remaining on `[ql/progressUpdated] step 1000 -> Compiling` and things seem to not progress further. Re-running the query at this point should then complete it fine.

### Sometimes running a remote query over TRAMP gets "stuck"

Currently `emacs-codeql` uses synchronous shell commands to invoke the codeql cli with various tasks, ranging from meta data gathering to result parsing. While these commands are generally short lived, and longer tasks such as query compilation and evaluation are handled by the query server asynchronously, on rare occasion one of the synchronous commands may block emacs, due to TRAMP quirks. This is known behavior, and I'm thinking about a more asynchronous design for the shell command handling. In practice this does not happen so frequently that it causes too much friction, but buyer beware when venturing into remote contexts.

When in doubt `C-g` is your friend. If you'd like to see what's happening under the covers `(setq tramp-verbose 6)` and watch the TRAMP debug buffer with some popcorn at hand.

### TRAMP sometimes returns unexpected data in buffers or complains about reentrancy

The code has some reentrancy issues currently which makes the TRAMP support a little flaky if you're spamming a ton of operations rapidly. I'm working on resolving these for a more stable experience, as well as looking into how to improve the LSP experience over TRAMP as well on older versions of emacs and TRAMP.

If you see `error in process filter: peculiar error: "Forbidden reentrant call of Tramp"` on TRAMP 2.5.2, I'm aware, and looking into resolving the reentrancy where possible. TRAMP is very chatty, so we need some better synchronization primitives to prevent launching into TRAMP operations while it's already in the middle of e.g. performing a file stat. 

In the meantime, you should be able to just repeat whatever action was interrupted by the error without causing any issues.

## TODO

- `emacs-codeql` does NOT yet provide xref and region annotation support for the database source code archive, however, this is underway and should be part of the first version.
- `emacs-codeql` ~~does NOT yet provide database upgrade support, however, this is underway and should be part of the first version.~~ UPDATE: @adityasharad informs me that the current CodeQL engine will deal with database upgrades/downgrades automagically as long as you're not operating on very old databases
- `emacs-codeql` does NOT yet provide an AST viewer, however, this is underway and should be a part of the first version.
- `emacs-codeql` does NOT yet provide database creation support, use the codeql cli directly for this. I'll likely provide projectile commands for codeql database creation out of a given project root for various languages in an upcoming version.

## Acknowledgements

This package was heavily inspired by, and in some cases directly ported from, [Alvaro Muñoz](https://github.com/pwntester)'s [codeql.nvim](https://github.com/pwntester/codeql.nvim) I'd like to thank him for dragging me into a continuously good natured editor arms race. He solved most of the hard problems in lua, and a lot of this package stands on the shoulders of his prior art. I also stole his `README.md` in an attempt at tongue in cheek comedy. I know it took me two years to actually start this project, but here we are sir :P

A lot of the fundamental ideas behind this package were first implemented by [Esben Sparre Andreasen](https://github.com/esbena). Esben is a seasoned CodeQL engineer and his deep QL knowledge and early work on emacs codeql support were a big inspiration to finally revive this effort. While this package no longer resembles Esben's original (private) major mode, the very first iteration of `emacs-codeql` was heavily drafted on top of his work, and his original design ideas such as the org-mode based result rendering are very much carried forward in this implementation. 

I'd also like to acknowledge the [eglot](https://github.com/joaotavora/eglot) project and its maintainers. The eglot project has solved many of the problems that pop up when dealing with asynchronous jsonrpc over stdio in a synchronous emacs world. Their various tips and tricks, sprinkled throughout the `eglot` codebase as well as the `jsonrpc` library itself, were consistently the answers to most of the blocking issues that popped up while writing `emacs-codeql`, especially when dealing with TRAMP quirks. They were also very responsive in getting the `workspaceFolders` support required by the codeql langserver in place.

Last, but not least, I want to acknowledge the omnipresent [Shohei YOSHIDA](https://github.com/syohex). While I was writing this code I found myself doing an inordinate amount of destructuring binds to chew through the various json objects involved in juggling the query server protocol as well as the local result parsing. In a pursuit of legibility, I came across https://github.com/syohex/emacs-json-pointer which is a small but fantastically useful piece of code for clear json object access in elisp. Since it is not available on MELPA, it is bundled as part of this package. All original copyright and licensing as designated by Shohei YOSHIDA applies, and I'd like to thank them for their continued contributions across the elisp ecosystem.

## Demo

You can find an early feature demo below, note that it is probably out of sync with the current state of the project.

[![emacs-codeql demo](https://img.youtube.com/vi/wP9fU9gVGS4/0.jpg)](https://www.youtube.com/watch?v=wP9fU9gVGS4)
