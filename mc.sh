#!/bin/bash

NAME=""

get_file_name () {
    NAME=`basename $1`
    local IFS='.'
    arr=($NAME)
    NAME=${arr[0]}
}

print_usage () {

    printf \
"Usage: mc [options] <file>
Options:
-dump-il       Generate the ILOC and stop.
-no-opt        Disable all optimizations.
-no-opt-copy-propagation
               Disable copy propagation optimization.
-no-reg-alloc  Don't run the register allocation algorithm.
-o <file>      Specify name of output binary. Default is \"a.out\".
-S             Generate the target assembly and stop.
-static-check  Run the static checker and stop.\n"
    exit 1
}

DUMP_IL=false
NO_OPT=false
NO_OPT_COPY_PROPAGATION=false
NO_REG_ALLOC=false
O="a.out"
S=false
STATIC_CHECK=false

while [[ $# > 1 ]]
do
key="$1"

case $key in
    -dump-il)                 DUMP_IL=true;;
    -h|-help)                 print_usage;;
    -no-opt)                  NO_OPT=true;;
    -no-opt-copy-propagation) NO_OPT_COPY_PROPAGATION=true;;
    -no-reg-alloc)            NO_REG_ALLOC=true;;
    -o)                       O="$2"; shift;;
    -S)                       S=true;;
    -static-check)            STATIC_CHECK=true;;
    *)
        printf "Unknown option $1\n"
    print_usage
    ;;
esac
shift
done

if [[ -n $1 ]]; then
    FPATH=$1
else
    print_usage
fi

if [[ "$1" == "-help" ]] || [[ "$1" == "-h" ]] || [[ "${1:0:1}" == "-" ]]; then
    print_usage
fi

get_file_name $FPATH

cd mini-parser
PARSER_OUTPUT="$(java Mini ../$FPATH > ../$NAME.json)"
cd ..

if [ -n "$PARSER_OUPUT" ]; then
    echo $PARSER_OUTPUT
    exit 1
fi

./compiler "$NAME" "$DUMP_IL" "$NO_OPT" "$NO_OPT_COPY_PROPAGATION" "$NO_REG_ALLOC" "$STATIC_CHECK" `uname`

if [ $? -ne 0 ]; then
    exit 1
fi

if [ "$S" = true ] || [ "$NO_REG_ALLOC" = true ]; then
    rm "$NAME.json"
else
    as -o "$NAME.o" "$NAME.s"
    if [[ `uname` == "Darwin" ]]; then
        ld -lc -e _main -macosx_version_min 10.10 -o "$O" "$NAME.o"
    else
        # I'm using gcc to link the object file b.c I can't
        # figure out how to get ld to do it on Linux!
        gcc -o "$O" "$NAME.o"
    fi

    # Remove temporary fies
    if [ $? -eq 0 ]; then
        rm "$NAME.json"
        rm "$NAME.o"
        rm "$NAME.s"
    fi
fi
