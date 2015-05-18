#!/bin/bash

NAME=""
S_MODE=false
COMPILE_MODE="_"

get_file_name () {
    NAME=`basename $1`
    local IFS='.'
    arr=($NAME)
    NAME=${arr[0]}
}

print_usage () {
    printf -- "Usage: mc [options] <filename>\n"
    printf -- "\n"
    printf -- "Options:\n"
    printf -- "-printAst    Print the program's source as interpreted by the parser.\n"
    printf -- "-dumpIL      Print the ILOC representation of the program.\n"
    printf -- "-noRegAlloc  Don't run the register allocation algorithm.\n"
    printf -- "-S           Generate the target assembly and stop.\n"
    exit 1
}

if [ $# -eq 0 ] || [ $# -gt 5 ]; then
    print_usage
fi

if [[ ${1:0:1} == "-" ]]; then
    if [[ $1 == "-S" ]]; then
        S_MODE=true
    else
        COMPILE_MODE=$1
    fi

    FPATH=$2
else
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

./compiler $NAME $COMPILE_MODE `uname`

if [ $? -ne 0 ]; then
    exit 1
fi

if [ "$S_MODE" = true ]; then
    rm "$NAME.json"
else
    gcc "$NAME.s"

    # Remove temporary fies
    if [ $? -eq 0 ]; then
        rm "$NAME.json"
        rm "$NAME.s"
    fi
fi
