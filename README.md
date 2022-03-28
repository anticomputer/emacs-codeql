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
- Buffer local query server state (concurrent project, database and query support)
- Automatic database selection deconfliction
- Org based rendering of path-problem, problem, and raw results

## Requirements

- Emacs 27.1+
  - transient
  - tree-sitter
  - tree-sitter-langs
  - tree-sitter-indent
  - aggressive-indent
  - projectile
  - eglot

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
  (setq codeql-configure-projectile t))
```

### Alternative install method

Alternatively, you can clone this repository and place it into your emacs `load-path`, you'll want to ensure that you have all required dependencies available, including the most recent MELPA version of eglot.

```elisp
;; initialization options
(setq codeql-transient-binding "C-c q")
(setq codeql-configure-eglot-lsp t)
(setq codeql-configure-projectile t)

(require 'emacs-codeql)
```

### If needed: build a custom tree-sitter QL artifact

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

For example, to start a Javascript query project, `projectroot/qlpack.yaml` could contain:

```yaml
---
library: false
name: testpack
version: 0.0.1
libraryPathDependencies: codeql/javascript-all
```

See [QL packs](https://codeql.github.com/docs/codeql-cli/about-ql-packs/)) for additional information.

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

## Language Server Protocol

`emacs-codeql` performs very well with `eglot`. Due to the codeql language server relying on `workspaceFolders` support, `eglot 20220326.2143` or newer is required from MELPA, which includes the basic project-root based `workspaceFolders` introduced in: https://github.com/joaotavora/eglot/commit/9eb9353fdc15c91a66ef8f4e53e18b22aa0870cd

Projectile and eglot configurations are included in `emacs-codeql` and controlled by the `codeql-configure-eglot-lsp` and `codeql-configure-projectile` variables, respectively.

It is HIGHLY recommended to enable both eglot and projectile configurations, as they depend on each other and provide the most pleasant query editing experience.

`emacs-codeql` has not been tested in conjunction with `lsp-mode`.

## TODO

- `emacs-codeql` does NOT yet provide xref and region annotation support for the database source code archive, however, this is underway and should be part of the first version.
- `emacs-codeql` does NOT yet provide database upgrade support, however, this is underway and should be part of the first version.
- `emacs-codeql` does NOT yet provide an AST viewer, however, this is underway and should be a part of the first version.
- `emacs-codeql` does NOT yet provide database creation support, use the codeql cli directly for this.

## Acknowledgements

This package was heavily inspired by, and in some cases directly ported from, [Alvaro Muñoz](https://github.com/pwntester)'s [codeql.nvim](https://github.com/pwntester/codeql.nvim) I'd like to thank him for dragging me into a continuously good natured editor arms race. He solved most of the hard problems in lua, and a lot of this package stands on the shoulders of his prior art. I also stole his `README.md` in an attempt at tongue in cheek comedy. I know it took me two years to actually start this project, but here we are sir :P

A lot of the fundamental ideas behind this package were first implemented by [Esben Sparre Andreasen](https://github.com/esbena). Esben is a seasoned CodeQL engineer and his deep QL knowledge and early work on emacs codeql support were a big inspiration to finally revive this effort. While this package no longer resembles Esben's original (private) major mode, the very first iteration of `emacs-codeql` was heavily drafted on top of his work, and his original design ideas such as the org-mode based result rendering are very much carried forward in this implementation. 

Last, but not least, I want to acknowledge the omnipresent [Shohei YOSHIDA](https://github.com/syohex). While I was writing this code I found myself doing an inordinate amount of destructuring binds to chew through the various json objects involved in juggling the query server protocol as well as the local result parsing. In a pursuit of legibility, I came across https://github.com/syohex/emacs-json-pointer which is a small but fantastically useful piece of code for clear json object access in elisp. Since it is not available on MELPA, it is bundled as part of this package. All original copyright and licensing as designated by Shohei YOSHIDA applies, and I'd like to thank them for their continued contributions across the elisp ecosystem.

## Demo

You can find an early feature demo below, note that it is probably out of sync with the current state of the project.

[![emacs-codeql demo](https://img.youtube.com/vi/wP9fU9gVGS4/0.jpg)](https://www.youtube.com/watch?v=wP9fU9gVGS4)
