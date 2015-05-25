#!/bin/bash

####################################################
# NOTE: This script is broken right now.           #
#       Fix later if we fix the problem w. Mochi.  #
####################################################

NAME=""

get_dir_name () {
    local IFS='/'
    arr=($1)
    NAME=${arr[1]}
}

echo "Testing static checker...."

for f in tests/*.mini; do
    ./mc.sh $f
done

N=0

echo "Testing benchmarks..."

for dir in benchmarks/*; do
    for f in $dir/*.mini; do
        get_dir_name $dir
        printf "test %-24s (%02d/19): " "$NAME" $N
        ./mc.sh -dump-il -mochi-compat -no-opt $f > .tmp 2>&1

        if [ $? -ne 0 ]; then
            printf "\n!!!failed to compile!!!\n"
            head .tmp
            continue
        fi

        java -Xss256M \
             -jar ~/Documents/jars/mochi.jar \
             -r "$NAME.il" < $dir/input 2>&1 | \
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

        rm "$NAME.il"

        N=$((N + 1))
    done
done
