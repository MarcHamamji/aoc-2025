#!/usr/bin/env bash

current_number=50
code=0

while read -r line; do
    echo "got line $line"
    num=${line:1}
    if [[ ${line::1} = L ]]; then
        current_number=$(( ((current_number-num) % 100 + 100) % 100 ))
    else
        current_number=$(( ((current_number+num) % 100 + 100) % 100 ))
    fi
    if [[ $current_number == 0 ]]; then
        code=$((code+1))
    fi
done

echo "$code"

