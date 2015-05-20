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
            TIME_OUTPUT="$((time -p ./a.out < $dir/input > $dir/output.myout) 2>&1)"
            diff $dir/output $dir/output.myout > .tmp 2>&1

            if [ $? -eq 0 ]; then
                printf "pass\n$TIME_OUTPUT\n\n"
            else
                echo "fail"
                head .tmp
            fi

        fi
        N=$((N + 1))
    done
done
