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
FAIL=false

echo "Running tests, average of $REPEAT trials..."

for dir in mini-benchmarks/*/; do
    for f in $dir/*.mini; do
        SUM=0
        get_dir_name $dir
        printf "test %-24s (%02d/19): " "$NAME" $N
        if [ -n "$2" ]; then
            ./mc $2 $f > .tmp 2>&1
        else
            ./mc $f > .tmp 2>&1
        fi

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
                FAIL=true
                break
            fi
            SUM=`echo $SUM + $TIME_OUTPUT | bc`
        done

        if [ "$FAIL" = false ]; then
            SIZE="$(wc -c a.out | sed 's/ *\([^ ]*\)\ *.*/\1/')"
            printf "size: ${SIZE}B time: "
            echo "scale=4; $SUM / $REPEAT" | bc
        fi
        FAIL=false

        N=$((N + 1))
    done
done
