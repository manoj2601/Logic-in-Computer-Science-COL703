#! /bin/bash

if [ "$#" == 2 ]; then
    sml flasl2ast.sml "$1" "$2"
    rm lcs.grm.* lcs.lex.*
else
    echo "Illegal number of parameters ($#)"
fi
