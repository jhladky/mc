#!/bin/bash

NAME=""

get_dir_name () {
    local IFS='/'
    arr=($1)
    NAME=${arr[1]}
}

if [ $# -eq 0 ]; then
    REPEAT=10
else
    REPEAT="$1"
fi

N=0

echo "Running tests, average of $REPEAT trials..."

for dir in benchmarks/*; do
    for f in $dir/*.mini; do
        SUM=0
        get_dir_name $dir
        printf "test %-24s (%02d/19): " "$NAME" $N
        ./mc.sh $f > .tmp 2>&1

        if [ $? -ne 0 ]; then
            printf "\n!!!failed to compile!!!\n"
            head .tmp
            continue
        fi

        for ((i=0; i < $REPEAT; i++)) do
            TIME_OUTPUT="$((time -p ./a.out < $dir/input > $dir/output.myout) 2>&1 | sed -n 2p)"
            TIME_OUTPUT=($TIME_OUTPUT)
            TIME_OUTPUT=${TIME_OUTPUT[1]}

            diff $dir/output $dir/output.myout > .tmp 2>&1

            if [ $? -ne 0 ]; then
                echo "fail"
                head .tmp
                break
            fi

            SUM=`echo $SUM + $TIME_OUTPUT | bc`
        done

        echo "scale=4; $SUM / $REPEAT" | bc
        N=$((N + 1))
    done
done
