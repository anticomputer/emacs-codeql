#!/bin/sh
DST=~/codeql-home
# initialize|update a codeql workspace for query development in CWD
if [[ ! -d $DST/codeql-starter-workspace ]]; then
    echo "Initializing codeql-starter-workspace"
    mkdir -p $DST && git clone --recursive https://github.com/github/vscode-codeql-starter.git $DST/codeql-starter-workspace || exit 1
    codeql_repo=$(realpath $DST/codeql-starter-workspace/ql)
    codeql_go_repo=$(realpath $DST/codeql-starter-workspace/codeql-go)
    echo "*** initializing ~/.config/codeql/config ***"
    mkdir -p ~/.config/codeql && echo "--search-path $codeql_repo:$codeql_go_repo" > ~/.config/codeql/config
else
    echo "Updating codeql-starter-workspace"
    (cd $DST/codeql-starter-workspace; git submodule update --remote)
fi
