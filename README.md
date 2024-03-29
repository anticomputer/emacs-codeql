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

- Emacs 29.1+ (built-in tree-sitter)
  - transient

- Emacs 27.1+ (external tree-sitter)
  - transient
  - tree-sitter
  - tree-sitter-langs
  - tree-sitter-indent
  - aggressive-indent
  - eglot

- CodeQL CLI 2.12.0+

## Installation

### Recommended install method (Emacs 29 and above)

Emacs 29.1+ comes with `use-package`, `tree-sitter` and `eglot` built-in, which simplifies `emacs-codeql` setup considerably. A [quelpa](https://github.com/quelpa/quelpa) for non-MELPA package installs configuration may look like:

```elisp
(use-package emacs-codeql
  :quelpa
  (emacs-codeql :repo "anticomputer/emacs-codeql"
		:fetcher github-ssh
		:branch "main"
		:files (:defaults "bin"))
  :demand t
  :init
  (setq codeql-transient-binding "C-c q")
  (setq codeql-configure-eglot-lsp t))
```

### Recommended install method (Emacs 28 and below)

`use-package` + [quelpa](https://github.com/quelpa/quelpa) for non-MELPA package installs:

```elisp
(use-package emacs-codeql
  :quelpa
  (emacs-codeql :repo "anticomputer/emacs-codeql"
		:fetcher github-ssh
		:branch "main"
		:files (:defaults "bin"))
  :after tree-sitter-langs
  :demand t
  :init
  (setq codeql-transient-binding "C-c q")
  (setq codeql-configure-eglot-lsp t))
```

### Alternative install methods

#### Local

You can clone this repository and place it into your emacs `load-path`, you'll want to ensure that you have all required dependencies available, including the most recent MELPA versions of any required packages.

```elisp
;; initialization options
(setq codeql-transient-binding "C-c q")
(setq codeql-configure-eglot-lsp t)
(require 'emacs-codeql)
```

#### Straight.el

Users of `straight.el` can use the below `use-package` declaration:

```elisp
(use-package emacs-codeql
  :straight
  (emacs-codeql :type git
		:host github
		:repo "anticomputer/emacs-codeql"
		:branch "main"
        :files (:defaults "bin"))
  :after tree-sitter-langs
  :demand t
  :init
  (setq codeql-transient-binding "C-c q")
  (setq codeql-configure-eglot-lsp t))
```

### Getting Started with CodeQL

#### Production CodeQL Development

The quickest way into painfree production-ready CodeQL development is to use the [GitHub CLI CodeQL extension](https://github.com/github/gh-codeql) to manage your CodeQL toolchain dependencies and then use [CodeQL Packs](https://codeql.github.com/docs/codeql-cli/about-codeql-packs/) and (optionally) [CodeQL Workspaces](https://codeql.github.com/docs/codeql-cli/about-codeql-workspaces/) for your query development. CodeQL packs are the canonical way to share your CodeQL queries with the CodeQL community.

To get started:

1. Install the [GitHub CLI](https://cli.github.com/) via your package manager of choice.

2. Install the [GitHub CLI CodeQL extension](https://github.com/github/gh-codeql) via `gh extensions install github/gh-codeql`.

3. Confirm you can resolve qlpacks using `gh codeql resolve qlpacks`, on first run this will also download the CodeQL CLI on your behalf.

4. [Download or create a database](https://codeql.github.com/docs/codeql-cli/creating-codeql-databases/#obtaining-databases-from-lgtm-com) to query.

5. Create a new CodeQL pack using `gh codeql pack init yourname/my-queries`. This will create a directory named `my-queries`. If you intend to eventually publish your queries, then `yourname` should map to a GitHub organization or user account (e.g. `anticomputer/my-queries`).

6. From inside your newly created pack directory, add any CodeQL dependencies that your query pack needs using `gh codeql pack add`. For example, if we're writing a Ruby query pack and want to add the latest Ruby support, we would run `gh codeql pack add codeql/ruby-all` inside our newly created `my-queries` pack. This operation will (re)install any declared dependencies. If we want to specify a specific version of the `codeql/ruby-all` dependency, say `0.4.2`, we can further qualify the dependency with `gh codeql pack add codeql/ruby-all@0.4.2`. Note that if you are working with a pre-existing `qlpack.yml` that has existing dependencies you can install those dependencies with `gh codeql pack install`.

7. Open `my-queries/my-first-query.ql` from your `emacs-codeql` enabled Emacs, and off you go!

Note: If you prefer not to use the gh cli, you can also [install the CodeQL CLI yourself](https://codeql.github.com/docs/codeql-cli/getting-started-with-the-codeql-cli/#getting-started-with-the-codeql-cli). The pack commands would have the `gh` prefix removed in this case, but otherwise remain the same.

##### More about CodeQL packs and workspaces

You can find the latest versions of the standard CodeQL packs at https://github.com/orgs/codeql/packages

For more details about how to properly create and work with CodeQL packs please see https://codeql.github.com/docs/codeql-cli/creating-and-working-with-codeql-packs/ and the [CodeQL packs documentation](https://codeql.github.com/docs/codeql-cli/about-ql-packs/).

For writing queries that only depend on standard CodeQL libraries, you do not have to create a workspace. However, for more advanced CodeQL library and query development, we recommend you use [CodeQL workspaces](https://codeql.github.com/docs/codeql-cli/about-codeql-workspaces/) to define and manage your CodeQL project dependencies.

#### Personal CodeQL Development

If you don't intend to publish or otherwise distribute your CodeQL queries, a quicker way to bootstrap a working CodeQL query template environment is via the [VSCode CodeQL Starter Workspace](https://github.com/github/vscode-codeql-starter).

You can again either bootstrap a CodeQL CLI via the [GitHub CLI CodeQL extension](https://github.com/github/gh-codeql) or by [installing it yourself](https://codeql.github.com/docs/codeql-cli/getting-started-with-the-codeql-cli/#getting-started-with-the-codeql-cli).

To use the VSCode CodeQL Starter workspace with `emacs-codeql`, just `git clone --recursive https://github.com/github/vscode-codeql-starter.git` and you'll be able to open any of the example queries contained within this workspace to start iterating on a query.

For example, you can open `vscode-codeql-starter/codeql-custom-queries-ruby/example.ql` with your `emacs-codeql` enabled Emacs, and you're ready to run and iterate on `example.ql` against a Ruby CodeQL database.

## Using `emacs-codeql`

### Writing and Running Queries

Edit a `.ql` or `.qll` file inside a `qlpack.yml` project root and run `M-x codeql-transient-query-server-interact` to open the `emacs-codeql` transient. This transient is bound to the value of `codeql-transient-binding` on initialization, which may be customized by the user by setting `codeql-transient-binding` to a keybinding of their liking.

#### Syntax highlighting and indentation

`emacs-codeql` includes a `tree-sitter` based major-mode that provides indentation and syntax highlighting for `.ql` and `.qll` files. On Emacs 28 and below, it relies on `aggressive-indent` for continuous indentation, as is common practice in structural editing modes. On Emacs 29 and above, it employs a more conservative electric indent based indentation approach.

On Emacs 28 and below, `emacs-codeql` relies on a custom version of `tree-sitter-langs` that includes `tree-sitter` support for QL. This support has not been upstreamed as of 03-27-2022, but package recipes for custom forks are provided above.

On Emacs 28 and below, `emacs-codeql` also packages QL `tree-sitter` artifacts for Linux x64 and MacOS x64 systems, which may preclude the need to build your own artifacts. If your system is compatible with the existing artifacts, you are not required to install the custom forks of `tree-sitter`, `tree-sitter-langs` and `tree-sitter-indent` and can just use the existing MELPA versions.

On Emacs 29 and above you do not have to worry about any of this, as all the required `tree-sitter` support is included in the default build of Emacs and building the required artifacts is handled by standard Emacs 29+ functionalities.

On Emacs 29 and above `emacs-codeql` also comes with an Emacs 29+ specific version of its CodeQL major mode `ql-tree-sitter-builtin.el` that relies only on the included tree-sitter libraries and API.

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

On emacs 28.1+, TRAMP version (2.5.2+) provides a fairly trouble free experience on Linux even with the LSP enabled, at least in my experience. On emacs 27.2, with the older version of TRAMP, I've had less luck with the LSP experience, but the query server performs fine over TRAMP. `emacs-codeql` will ask for confirmation to enable the LSP when it detects it is running in a remote context. When in doubt, keep it disabled.

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

I recommend using the `codespaces.el` [package](https://github.com/patrickt/codespaces.el), which provides convenient TRAMP support for GitHub codespaces.

With this package in place, you can just TAB to get a list of your available Codespaces when opening a `/ghcs:` prefixed file.

### Language Server Protocol

`emacs-codeql` performs very well with `eglot`. Due to the codeql language server relying on `workspaceFolders` support, `eglot 20220326.2143` or newer is required from MELPA, which includes the basic project-root based `workspaceFolders` introduced in: https://github.com/joaotavora/eglot/commit/9eb9353fdc15c91a66ef8f4e53e18b22aa0870cd and a compatible version of `eglot` ships with Emacs 29+ by default. An eglot configuration for the CodeQL LSP is included in `emacs-codeql` and controlled by the `codeql-configure-eglot-lsp` variable.

`emacs-codeql` has not been tested in conjunction with `lsp-mode`.

#### LSP over TRAMP

While the recommended LSP client, `eglot`, does function over TRAMP, running LSP over TRAMP can be very quirky and laggy unless you're on the latest emacs and TRAMP versions. Slow server initialization can be blocking to the point of full freezes. When operating in a TRAMP context, `emacs-codeql` will ask you if you want to enable the LSP client, unless you reaaaally want it, I recommend disabling it for remote editing and query debugging.

The codeql query server, while also running jsonrpc over stdio, is not tied to a direct editor feedback loop, so it is a much more pleasant experience over TRAMP, so all local functionality is enabled and available.

Having said that, on emacs 28.1+, TRAMP version (2.5.2+) provides a fairly trouble free experience on Linux at least even with LSP enabled remotely.

## Advanced Configuration

### Custom search paths

You can use [`.config/codeql/config`](https://codeql.github.com/docs/codeql-cli/specifying-command-options-in-a-codeql-configuration-file/) for your custom codeql cli search path configurations. This allows for more straightforward standard search path deconfliction between local and remote query contexts. Under normal circumstances, i.e. when developing using the recommended CodeQL pack and/or workspace methodology, you should not have to configure any custom search paths and `emacs-codeql` will automatically resolve any required paths.

The `codeql-search-paths` variable in `emacs-codeql` exists purely to provide you with search path precedence control. Unless you're wanting to override any of the default qlpacks from locations outside of the current project root, the existing and default value of '("./") suffices.

`codeql-search-paths` entries are expanded in the context of their location, so if you do want to configure search paths that work across both local and remote systems, you're able to e.g. use a standard "~/path" notation, which will expand to the correct absolute path in the context of the remote or local system via TRAMP.


### Custom tree-sitter artifacts

#### Emacs 29 and newer

On Emacs 29 and newer, with `tree-sitter` support enabled, you can use the `treesit-install-language-grammar` function to install the required ql artifact. You can either invoke `M-x treesit-install-language-grammar RET` and provide https://github.com/tree-sitter/tree-sitter-ql interactively, or programmatically you can run:

```elisp
(let ((treesit-language-source-alist
             '((ql . ("https://github.com/tree-sitter/tree-sitter-ql"
                      nil
                      nil
                      nil)))))
        (treesit-install-language-grammar 'ql))
```

On first use `emacs-codeql` will offer to run the above snippet if the ql tree-sitter grammar artifact is not yet installed on your system. Note that this requires a working C compiler (e.g. gcc) is present in your `PATH`.

#### Emacs 28 and older

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

- `emacs-codeql` does NOT yet provide compressed database support, however this is underway and should be part of the first version, for now you should manually `unzip` a CodeQL database to enable it for use with `emacs-codeql`.

## Acknowledgements

This package was heavily inspired by, and in some cases directly ported from, [Alvaro Muñoz](https://github.com/pwntester)'s [codeql.nvim](https://github.com/pwntester/codeql.nvim) I'd like to thank him for dragging me into a continuously good natured editor arms race. He solved most of the hard problems in lua, and a lot of this package stands on the shoulders of his prior art. I also stole his `README.md` in an attempt at tongue in cheek comedy. I know it took me two years to actually start this project, but here we are sir :P

A lot of the fundamental ideas behind this package were first implemented by [Esben Sparre Andreasen](https://github.com/esbena). Esben is a seasoned CodeQL engineer and his deep QL knowledge and early work on emacs codeql support were a big inspiration to finally revive this effort. While this package no longer resembles Esben's original (private) major mode, the very first iteration of `emacs-codeql` was heavily drafted on top of his work, and his original design ideas such as the org-mode based result rendering are very much carried forward in this implementation.

I'd also like to acknowledge the [eglot](https://github.com/joaotavora/eglot) project and its maintainers. The eglot project has solved many of the problems that pop up when dealing with asynchronous jsonrpc over stdio in a synchronous emacs world. Their various tips and tricks, sprinkled throughout the `eglot` codebase as well as the `jsonrpc` library itself, were consistently the answers to most of the blocking issues that popped up while writing `emacs-codeql`, especially when dealing with TRAMP quirks. They were also very responsive in getting the `workspaceFolders` support required by the codeql langserver in place.

Last, but not least, I want to acknowledge the omnipresent [Shohei YOSHIDA](https://github.com/syohex). While I was writing this code I found myself doing an inordinate amount of destructuring binds to chew through the various json objects involved in juggling the query server protocol as well as the local result parsing. In a pursuit of legibility, I came across https://github.com/syohex/emacs-json-pointer which is a small but fantastically useful piece of code for clear json object access in elisp. Since it is not available on MELPA, it is bundled as part of this package. All original copyright and licensing as designated by Shohei YOSHIDA applies, and I'd like to thank them for their continued contributions across the elisp ecosystem.

## Demo

You can find an early feature demo below, note that it is probably out of sync with the current state of the project.

[![emacs-codeql demo](https://img.youtube.com/vi/wP9fU9gVGS4/0.jpg)](https://www.youtube.com/watch?v=wP9fU9gVGS4)
