#!/bin/bash

NAME=""

get_file_name () {
    local IFS='/'
    arr=($1)
    NAME=${arr[2]}
    local IFS='.'
    arr=($NAME)
    NAME=${arr[0]}
}

cd mini-parser
make
for f in ../tests/*.mini; do
    get_file_name $f
    java Mini $f > "../tests/$NAME.json"
done
cd ..

mlton mc.mlb
if [ $? -ne 0 ]; then
    exit 1
else
    for f in tests/*.json; do
        ./mc $f
    done
fi
