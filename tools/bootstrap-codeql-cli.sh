#!/usr/bin/env bash

# run this script in a directory where you want to bootstrap a sibling codeql-cli directory structure

CODEQL_REPO="" # set this to an existing clone of github.com/github/codeql, or leave empty to bootstrap
CODEQL_GO_REPO="" # set this to an existing clone of github.com/github/codeql-go, or leave empty to bootstrap
CODEQL_CLI_VERSION="v2.8.4"

if [ "$(uname)" == "Darwin" ]; then
    CODEQL_CLI_RELEASE="https://github.com/github/codeql-cli-binaries/releases/download/$CODEQL_CLI_VERSION/codeql-osx64.zip"
else
    CODEQL_CLI_RELEASE="https://github.com/github/codeql-cli-binaries/releases/download/$CODEQL_CLI_VERSION/codeql-linux64.zip"
fi

CWD="$(pwd)"
CURL="$(which curl)"
CODEQL="$(which codeql)"
GIT="$(which git)"

bootstrap_codeql_cli () {

    extraction_root="$CWD"

    # see if we need the codeql repo
    if [[ ! $CODEQL_REPO ]]; then
        CODEQL_REPO="$extraction_root/codeql-repo"
        # only clone if we don't exist yet
        echo "Checking to see if we need a codeql repo clone ..."
        if [[ ! -d "$CODEQL_REPO" ]]; then
            echo "Bootstrapping codeql repo clone ..."
            "$GIT" clone https://github.com/github/codeql "$CODEQL_REPO" || return 1
        else
            echo "Codeql repo already exists ..."
        fi
    fi

    # see if we need the codeql-go repo
    if [[ ! $CODEQL_GO_REPO ]]; then
        CODEQL_GO_REPO="$extraction_root/codeql-go-repo"
        # only clone if we don't exist yet
        echo "Checking to see if we need a codeql-go repo clone ..."
        if [[ ! -d "$CODEQL_GO_REPO" ]]; then
            echo "Bootstrapping codeql repo clone ..."
            "$GIT" clone https://github.com/github/codeql-go "$CODEQL_GO_REPO" || return 1
        else
            echo "Codeql Golang repo already exists ..."
        fi
    fi

    echo ""
    echo "*********************************************************************"
    echo "   ADD THIS SEARCH PATH CONFIGURATION TO ~/.config/codeql/config     "
    echo "*********************************************************************"
    echo ""
    echo "--search-path $extraction_root/codeql-repo:$extraction_root/codeql-go-repo"
    echo ""
    echo "********************* END OF CONFIGURATION **************************"
    echo ""
    
    if [[ $CODEQL ]]; then
        echo "[?] Which codeql cli do you want to use?"
        echo " +  Existing: use existing codeql cli from PATH"
        echo " +  Bootstrap: install or update a working copy of the codeql cli as a sibling"
        echo " +  Quit: abort"
        select ql in "Existing" "Bootstrap" "Quit"; do
            case $ql in
                Existing ) return 0;;
                Bootstrap ) break;;
                Quit ) exit;;
            esac
        done
    fi
    
    ql_bin_path="$extraction_root/codeql/codeql"

    if [[ -f "$ql_bin_path" ]]; then
        echo "[?] Update or keep the current codeql cli in $CWD?"
        echo " +  Update: move codeql to codeql.bak and install a new codeql cli"
        echo " +  Keep: keep the current codeql cli in $CWD"
        echo ""
        echo "********************************************************"
        echo "*** Warning: Update will remove any prior codeql.bak ***"
        echo "********************************************************"
        echo ""
        select up in "Update" "Keep" "Quit"; do
            case $up in
                Update )
                    if [[ -d "$CWD/codeql.bak" ]]; then
                        echo Removing "$CWD/codeql.bak"
                        rm -rf "$CWD/codeql.bak" || return 1
                    fi
                    echo "Backing up codeql to codeql.bak"
                    mv "$CWD/codeql" "$CWD/codeql.bak" || return 1
                    break;;
                Keep ) CODEQL="$ql_bin_path"; return 0;;
                Quit ) exit;;
            esac
        done
    fi

    echo "Fetching new CodeQL cli release ..."
    "$CURL" -L -o "$CWD/codeql-cli.zip" "$CODEQL_CLI_RELEASE" || return 1
    if [ "$(uname)" == "Darwin" ]; then
        /usr/bin/xattr -c codeql-cli.zip
    fi

    (cd "$extraction_root" && unzip codeql-cli.zip ) || return 1

    CODEQL="$ql_bin_path"

    echo "********************** PATH CONFIGURATION ****************************"
    echo ""
    echo "export PATH=$extraction_root/codeql:\$PATH"
    echo ""
    echo "********************* END OF CONFIGURATION ***************************"

    return 0
}

# set or get a codeql cli env
bootstrap_codeql_cli
if [[ $? -eq 1 ]]; then
    echo "Could not bootstrap codeql cli ... exiting"
    exit 1
fi

echo "All set!"
exit 0
