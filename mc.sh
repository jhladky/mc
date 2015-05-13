#!/bin/bash

NAME=""

get_file_name () {
    NAME=`basename $1`
    local IFS='.'
    arr=($NAME)
    NAME=${arr[0]}
}

if [ $# -eq 0 ] || [ $# -gt 2 ]; then
    printf "Usage: mc -printAst <filename>\n"
    printf "       mc -noRegAlloc <filename>\n"
    printf "       mc -dumpIL <filename>\n"
    exit 1
fi

if [[ ${1:0:1} == "-" ]]; then
    COMPILER_ARG=$1
    FPATH=$2
else
    COMPILER_ARG=""
    FPATH=$1
fi

get_file_name $FPATH

cd mini-parser
PARSER_OUTPUT="$(java Mini ../$FPATH > ../$NAME.json)"
cd ..

if [ -n "$PARSER_OUPUT" ]; then
    echo $PARSER_OUTPUT
    exit 1
fi

./compiler $NAME $COMPILER_ARG

if [ $? -ne 0 ]; then
    exit 1
fi

gcc "$NAME.s"

# Remove temporary fies
if [ $? -eq 0 ]; then
    rm "$NAME.json"
    rm "$NAME.s"
fi
