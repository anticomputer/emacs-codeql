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
- Database source archive xref backend support
- AST Viewer with xref backend support

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
  (setq codeql-search-paths '("./")))
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

### Getting Started: the recommended way

The quickest way into painfree CodeQL development is to use the CodeQL starter workspace and the gh cli extension to manage your toolchain dependencies. To get started:

1. Run [the starter workspace init script](https://github.com/anticomputer/emacs-codeql/blob/main/tools/init-starter-workspace.sh). This will setup a full CodeQL development environment in `~/codeql-home/codeql-starter-workspace` and also configure your search paths in `~/.config/codeql/config`.

2. Install the [GitHub CLI](https://cli.github.com/) via your package manager of choice.

3. Install the [GitHub CLI CodeQL extension](https://github.com/github/gh-codeql) via `gh extensions install github/gh-codeql`.

4. Confirm you can resolve qlpacks using `gh codeql resolve qlpacks`, on first run this will also download the CodeQL CLI on your behalf.

5. [Download or create a database](https://codeql.github.com/docs/codeql-cli/creating-codeql-databases/#obtaining-databases-from-lgtm-com) to query.

6. Open a `~/codeql-home/codeql-starter-workspace/codeql-custom-queries-*/example.ql` file, and start iterating on your query using `emacs-codeql`.


### Getting Started: the custom way

If you rather configure your codeql cli manually, for convenience, `emacs-codeql` provides a [cli bootstrap script](https://raw.githubusercontent.com/anticomputer/emacs-codeql/main/tools/bootstrap-codeql-cli.sh) which will bootstrap the codeql cli in a given current working directory. You can run this script, paste its resulting search path configuration into your `emacs-codeql` config, and you're off to the races.

If you bootstrap into `~/codeql-home`, then `~/codeql-home/codeql-repo` is a clone of https://github.com/github/codeql and `~/codeql-home/codeql-go` is a clone of https://github.com/github/codeql-go

`emacs-codeql` expects the codeql cli to exist in your `PATH`. Follow the standard [codeql cli setup instructions](https://codeql.github.com/docs/codeql-cli/getting-started-with-the-codeql-cli/) to get the cli bootstrapped and add its location to your executable search `PATH`.

If you use the gh cli, then you can also use the `github/gh-codeql` cli extension to initialize and configure your codeql cli.

You'll still need to clone the `github/codeql` and `github/codeql-go` repositories as with the bootstrap script and make them available in the search path of the codeql cli.

Once the gh cli codeql extension is installed and configured, `emacs-codeql` will automagically use it both locally and remotely pending the value of `codeql-use-gh-codeql-extension-when-available` whenever its available.

If unavailable, `emacs-codeql` will fall back to its normal executable path expectations, so you can safely keep this value set to `t`, which is the default.

#### Custom search paths

We recommend you use [`.config/codeql/config`](https://codeql.github.com/docs/codeql-cli/specifying-command-options-in-a-codeql-configuration-file/) for your codeql cli search path configurations. This allows for more straightforward standard search path deconfliction between local and remote query contexts. 

If you used the provided workspace init script it has already configured the paths for you.

It is important to ensure your codeql cli configuration contains the correct/desired search paths in `.config/codeql/config` on any local or remote system that you intend to use the codeql cli on via `emacs-codeql`.

Use `codeql resolve qlpacks` to double check that you can resolve all the expected qlpacks available in the default repositories.

The `codeql-search-paths` variable in `emacs-codeql` exists purely to provide you with search path precedence control. Unless you're wanting to override any of the default qlpacks from locations outside of the current project root, the existing and default value of '("./") suffices.

`codeql-search-paths` entries are expanded in the context of their location, so if you do want to configure search paths that work across both local and remote systems, you're able to e.g. use a standard "~/path" notation, which will expand to the correct absolute path in the context of the remote or local system via TRAMP.


### Optional: build custom tree-sitter QL artifacts

If this package does not contain the required `tree-sitter-langs` artifacts for your system, or you simply do not trust the binary artifacts I provided, you will have to build your own.

Follow the [tree-sitter-langs documentation](https://github.com/anticomputer/tree-sitter-langs-1) for detailed instructions on how to build such an artifact. 

In a nutshell:

1. Install cask. 
2. run `cask install` for my fork (`git clone --branch anticomputer-ql git@github.com:/anticomputer/tree-sitter-langs-1`) of `tree-sitter-langs` from its project root. 
3. Download a copy of the [binary release](https://github.com/tree-sitter/tree-sitter/releases/tag/v0.19.5) of `tree-sitter` and put it in your `PATH` somewhere. `tree-sitter` version <0.20 is required due to a breaking change in >= 0.20, I use 0.19.5.
4. Compile the QL support using `script/compile ql` from `tree-sitter-langs` project root, note that this also has `nodejs` as a dependency.

If you're on a Mac, you'll also need to set `EMACSDATA` and `EMACSLOADPATH` correctly and symlink `emacs` into your `PATH` somewhere, e.g when using MacPorts Emacs.app:

```
ln -s /Applications/MacPorts/EmacsMac.app/Contents/MacOS/Emacs ~/bin/emacs
EMACSDATA=/Applications/MacPorts/EmacsMac.app/Contents/Resources/etc EMACSLOADPATH=/Applications/MacPorts/EmacsMac.app/Contents/Resources/lisp cask install
EMACSDATA=/Applications/MacPorts/EmacsMac.app/Contents/Resources/etc EMACSLOADPATH=/Applications/MacPorts/EmacsMac.app/Contents/Resources/lisp script/compile ql
```

On M1 Macs, if you don't want to use the pre-packaged artifact, you're able to use the x86_64 `tree-sitter` 0.19.5 binary thanks to Rosetta, but you'll have to compile the `ql.dylib` yourself out of the `tree-sitter-langs` fork with an arch-appropriate compiler. 

To prepare the pre-packaged artifact on an M1 Mac I use the following:

```
$ cd ~/tree-sitter-langs-fork/repos/ql
$ tree-sitter generate
$ gcc -o ql.dylib --shared src/parser.c -I./src
$ cp ql.dylib ~/.emacs.d/elpa/tree-sitter-langs*/bin/
```

Alternatively you can compile `tree-sitter` 0.19.5 from scratch on an M1 mac by checking out the v0.19.5 tag of https://github.com/tree-sitter/tree-sitter and using `cargo` to build the `tree-sitter` cli, e.g. `cd cli && cargo install --path .`, which will get you an M1 build of `tree-sitter` for the proper architecture, but personally I just compile `ql.dylib` myself after using the x86_64 `tree-sitter` binary for `tree-sitter generate`. You can find more suggestions and alternatives for dealing with M1 Macs in https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/88

After building the QL artifact(s) for the architectures and platforms you need to support, you'll want to grab `bin/ql.*` and move those into `~/.emacs.d/elpa/tree-sitter-langs*/bin/` and restart emacs. 

QL syntax highlighting and indentation should now work fine.

## Using `emacs-codeql`

NOTE: make sure that you enable the `projectile` configuration option if you intend to use LSP support via `eglot`, as `eglot` needs to send the project root as part of codeql langserver `workspaceFolders` initialization.

### Query Development using the workspace

The quickest way to get up and running with a working CodeQL development environment is through the aforementioned [starter workspace](https://github.com/github/vscode-codeql-starter). For your convenience a `tools/init-starter-workspace.sh` bootstrap script is included with `emacs-codeql` that will clone and configure all the appropriate repositories. You can then start writing queries for the language of choice in any of the `codeql-custom-queries-*` directories.

### Query Development without using the workspace

If you already have a bootstrapped codeql cli configuration and prefer not to use the starter workspace, you can create a project root containing a `qlpack.yml` QL pack definition and then create query files inside this project.

For example, to start a Javascript query project, `projectroot/qlpack.yml` could contain:

```yaml
---
library: false
name: testpack
version: 0.0.1
libraryPathDependencies: codeql/javascript-all
```

See [CodeQL packs documentation](https://codeql.github.com/docs/codeql-cli/about-ql-packs/) for additional information.

### Writing and Running Queries

Edit a `.ql` or `.qll` file inside a `qlpack.yml` project root (e.g. a `codeql-custom-queries-*` directory) and run `M-x codeql-transient-query-server-interact` to open the `emacs-codeql` transient. This transient is bound to the value of `codeql-transient-binding` on initialization, which may be customized by the user by setting `codeql-transient-binding` to a keybinding of their liking.

#### Syntax highlighting and indentation

`emacs-codeql` includes a `tree-sitter` based major-mode that provides indentation and syntax highlighting for `.ql` and `.qll` files. It relies on `aggressive-indent` for continuous indentation, as is common practice in structural editing modes.

`emacs-codeql` relies on a custom version of `tree-sitter-langs` that includes `tree-sitter` support for QL. This support has not been upstreamed as of 03-27-2022, but package recipes for custom forks are provided above. 

`emacs-codeql` also packages QL `tree-sitter` artifacts for Linux x64 and MacOS x64 systems, which may preclude the need to build your own artifacts. If your system is compatible with the existing artifacts, you are not required to install the custom forks of `tree-sitter`, `tree-sitter-langs` and `tree-sitter-indent` and can just use the existing MELPA versions.

#### Database selection

You can register a database for your current project via `M-x codeql-transient-query-server-interact` and selecting `d` to register an active database. Ensure you have started a query server first by using `s` from the same menu. If a database is active in another query buffer, it will automatically be deregistered there and registered for your current buffer. If a database has been registered in the global emacs session before, you can fetch it more rapidly via selecting `k` for known database history.

Database selection is buffer-local per query file and you can run as many concurrent query projects as you have resources available. Database history allows you to rapidly register/unregister databases that were seen across all sessions.

`emacs-codeql` buffer-local approach means you can work on e.g. a `cpp` and `javascript` project concurrently, and you can an arbitrary amount of concurrent results buffers as you toggle between your query buffers.

You can find existing databases for a ton of open source projects on [LGTM.com](https://codeql.github.com/docs/codeql-cli/creating-codeql-databases/#obtaining-databases-from-lgtm-com), or you can [create your own](https://codeql.github.com/docs/codeql-cli/creating-codeql-databases/#creating-codeql-databases) using the codeql cli. 

#### Run a query or quick evaluate query regions

If there is an active region, `emacs-codeql` will attempt to quick-eval that region. Note that to quick-eval an entire predicate, you should mark the predicate name itself as opposed to the entire predicate definition. This follows the same semantics as quick-evaluation in the vscode extension.

If there is no active region, `emacs-codeql` will attempt to run the full query by default.

#### Result rendering

Results are rendered through `org-mode` buffers, which allows you to save results as normal text files and even read them in other editors. This approach also allows you to turn a result buffer into a living audit document, take notes, and do all the things that `org-mode` allows you to do.

The following query "kinds" are supported: `path-problem`, `problem`, and raw tuple results.

kind: `problem` and `path-problem` are rendered as org trees, raw tuple results are rendered as org tables.

Results contain source code locations in the form of org links, which can be visited with the normal org mode operations for link handling (e.g. `org-open-at-point`)

`emacs-codeql` will resolve file paths into the project snapshot source code archive included with the database archive. 

### Database source archive Xref support

`emacs-codeql` automatically retrieves references and definitions for database source archive files that are opened from a result buffer org link. It achieves this by registering a custom org-link handler for a `codeql:` link type which then performs a ridiculous elisp ritual to trick emacs into thinking it actually has xref support for these files specifically. 

`emacs-codeql` rudely overrides the default xref bindings for any existing major-mode that might normally take precedence for the type of source file that is opened, but only for files opened from a result buffer. But not to worry, these overrides are buffer-local only, and won't tamper with any other source files sharing the same major-mode that are NOT inside a codeql database source archive.

The `codeql:` link handler sets up a buffer-local context for the opened source file that initializes a custom xref backend, such that the normal emacs `xref-find-definitions` and `xref-find-references` functions (commonly bound to `M-.` and `M-,`) operate as you would expect them to.

Note: references and definitions are fetched asynchronously by running templated queries in the background the first time a given archive file is visited in the current session. When the xref results are available, the xref backend will automagically start working without any further requirements from the user, but it may take a few seconds before they actually become active. On subsequent visits, the xref data is pulled from cache and should be available instantly.

There occurs a somewhat awkward situation when you follow a transitive xref, i.e. one that is not directly in a child buffer of a org link follow. At that point `emacs-codeql` doesn't know yet that this file was opened in a codeql source browsing context and that it still needs its references and definitions resolved. To deal with this, you can can `M-x RET codeql-xref-backend RET` as this will initialize the buffer with the required xref bindings.

Alternatively, any invocation of the default `xref-find-references` or `xref-find-definitions` functions will trigger a call to the `codeql-xref-backend` function as well, since it serves as the `xref-backend-functions` entry for the codeql xref backend. After this initial invocation to the standard bindings, the codeql xref bindings will then take over if `codeql-xref-backend` determined that the file its operating on is inside a codeql database source archive.

It is left as a matter of user preference whether to invoke `codeql-xref-backend` themselves, or to deal with a single layer of indirection and wait for one of the xref API to invoke it for them.

### AST Viewer

Just like the vscode extension, `emacs-codeql` lets you browse the AST of a database source archive file. Similar to the other results buffers, the AST is rendered as an org-mode tree. You can invoke the AST viewer with `M-x RET codeql-view-ast RET` for a database source archive file. 

![screenshot](img/codeql-ast-viewer.png?raw=true "emacs-codeql")

There is AST xref support bound to `M->` in any database source file that also has an active AST viewer buffer open.

### Commands

All query server interaction for your query buffer routes via an intuitive `transient` interface. You can configure which keybinding is associated with starting this interface via the `codeql-transient-binding` variable. 

This interface is self-documenting and allows you to start/stop query servers, select databases, run queries, and fetch query history. 

Otherwise hidden features such as max path depths for path queries are unobtrusively surfaced to the user.

### TRAMP support

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

### Language Server Protocol

`emacs-codeql` performs very well with `eglot`. Due to the codeql language server relying on `workspaceFolders` support, `eglot 20220326.2143` or newer is required from MELPA, which includes the basic project-root based `workspaceFolders` introduced in: https://github.com/joaotavora/eglot/commit/9eb9353fdc15c91a66ef8f4e53e18b22aa0870cd

Projectile and eglot configurations are included in `emacs-codeql` and controlled by the `codeql-configure-eglot-lsp` and `codeql-configure-projectile` variables, respectively.

It is HIGHLY recommended to enable both eglot and projectile configurations, as they depend on each other and provide the most pleasant query editing experience.

`emacs-codeql` has not been tested in conjunction with `lsp-mode`.

#### LSP over TRAMP

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

- `emacs-codeql` ~~does NOT yet provide xref and region annotation support for the database source code archive, however, this is underway and should be part of the first version.~~
- `emacs-codeql` ~~does NOT yet provide database upgrade support, however, this is underway and should be part of the first version.~~ UPDATE: @adityasharad informs me that the current CodeQL engine will deal with database upgrades/downgrades automagically as long as you're not operating on very old databases
- `emacs-codeql` ~~does NOT yet provide an AST viewer, however, this is underway and should be a part of the first version.~~
- `emacs-codeql` does NOT yet provide database creation support, use the codeql cli directly for this. I'll likely provide projectile commands for codeql database creation out of a given project root for various languages in an upcoming version.
- `emacs-codeql` does NOT yet provide compressed database support, however this is underway and should be part of the first version.

## Acknowledgements

This package was heavily inspired by, and in some cases directly ported from, [Alvaro Muñoz](https://github.com/pwntester)'s [codeql.nvim](https://github.com/pwntester/codeql.nvim) I'd like to thank him for dragging me into a continuously good natured editor arms race. He solved most of the hard problems in lua, and a lot of this package stands on the shoulders of his prior art. I also stole his `README.md` in an attempt at tongue in cheek comedy. I know it took me two years to actually start this project, but here we are sir :P

A lot of the fundamental ideas behind this package were first implemented by [Esben Sparre Andreasen](https://github.com/esbena). Esben is a seasoned CodeQL engineer and his deep QL knowledge and early work on emacs codeql support were a big inspiration to finally revive this effort. While this package no longer resembles Esben's original (private) major mode, the very first iteration of `emacs-codeql` was heavily drafted on top of his work, and his original design ideas such as the org-mode based result rendering are very much carried forward in this implementation. 

I'd also like to acknowledge the [eglot](https://github.com/joaotavora/eglot) project and its maintainers. The eglot project has solved many of the problems that pop up when dealing with asynchronous jsonrpc over stdio in a synchronous emacs world. Their various tips and tricks, sprinkled throughout the `eglot` codebase as well as the `jsonrpc` library itself, were consistently the answers to most of the blocking issues that popped up while writing `emacs-codeql`, especially when dealing with TRAMP quirks. They were also very responsive in getting the `workspaceFolders` support required by the codeql langserver in place.

Last, but not least, I want to acknowledge the omnipresent [Shohei YOSHIDA](https://github.com/syohex). While I was writing this code I found myself doing an inordinate amount of destructuring binds to chew through the various json objects involved in juggling the query server protocol as well as the local result parsing. In a pursuit of legibility, I came across https://github.com/syohex/emacs-json-pointer which is a small but fantastically useful piece of code for clear json object access in elisp. Since it is not available on MELPA, it is bundled as part of this package. All original copyright and licensing as designated by Shohei YOSHIDA applies, and I'd like to thank them for their continued contributions across the elisp ecosystem.

## Demo

You can find an early feature demo below, note that it is probably out of sync with the current state of the project.

[![emacs-codeql demo](https://img.youtube.com/vi/wP9fU9gVGS4/0.jpg)](https://www.youtube.com/watch?v=wP9fU9gVGS4)
