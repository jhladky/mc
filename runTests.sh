#!/bin/bash

NAME=""

rm -f tests/*.json
rm -f tests/*.il

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

for dir in ../benchmarks/*; do
    for f in $dir/*.mini; do
        get_file_name $f
        java Mini $f > "../benchmarks/$dir/$NAME.json"
    done
done
cd ..

make
if [ $? -ne 0 ]; then
    exit 1
else
    for f in tests/*.json; do
        ./mc -dumpIL $f
    done

    for dir in benchmarks/*; do
        for f in $dir/*.json; do
            ./mc -dumpIL $f
        done
    done
fi
