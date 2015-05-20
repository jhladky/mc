#!/bin/bash

####################################################
# NOTE: This script is broken right now.           #
#       Fix later if we fix the problem w. Mochi.  #
####################################################

NAME=""

get_file_name () {
    local IFS='/'
    arr=($1)
    NAME=${arr[2]}
    local IFS='.'
    arr=($NAME)
    NAME=${arr[0]}
}

get_dir_name () {
    local IFS='/'
    arr=($1)
    NAME=${arr[1]}
}

rm -f tests/*.json
rm -f tests/*.il
for dir in benchmarks/*; do
    rm -f $dir/output.myout
    rm -f $dir/*.json
done

cd mini-parser
make &> .tmp
if [ $? -ne 0 ]; then
    cat .tmp
    exit 1
fi
rm .tmp

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

make &> .tmp
if [ $? -ne 0 ]; then
    cat .tmp
    exit 1
fi
rm .tmp

for f in tests/*.json; do
    ./mc -dumpIL $f
done

N=0

for dir in benchmarks/*; do
    for f in $dir/*.json; do
        get_dir_name $dir
        printf "test %-24s (%02d/19): " "$NAME" $N

        ./mc -dumpIL $f
        java -Xss256M \
             -jar ~/Documents/jars/mochi.jar \
             -r $dir/*.il < $dir/input 2>&1 | \
            tail -n +6 | \
            sed -n -e :a -e '1,2!{P;N;D;};N;ba' > \
                $dir/output.myout
        diff $dir/output $dir/output.myout > .tmp 2>&1
        if [ $? -eq 0 ]; then
            echo "pass"
        else
            echo "fail"
            head .tmp
        fi
        N=$((N + 1))
    done
done
