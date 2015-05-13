#!/bin/bash

NAME=""

get_dir_name () {
    local IFS='/'
    arr=($1)
    NAME=${arr[1]}
}

N=0

for dir in benchmarks/*; do
    for f in $dir/*.mini; do
        get_dir_name $dir
        printf "test %-24s (%02d/19): " "$NAME" $N
        ./mc.sh $f > .tmp 2>&1

        if [ $? -ne 0 ]; then
            echo "fail"
            echo "Failed to compile."
            head .tmp
        else
            ./a.out < $dir/input > $dir/output.myout
            diff $dir/output $dir/output.myout > .tmp 2>&1

            if [ $? -eq 0 ]; then
                echo "pass"
            else
                echo "fail"
                head .tmp
            fi
        fi
        N=$((N + 1))
    done
done
