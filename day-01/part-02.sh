#!/usr/bin/env bash

current_number=50
code=0
echo "code: $code"
echo "num: $current_number"

while read -r line; do
    echo "-------------- $line"
    amount=${line:1}
    direction=1
    if [[ ${line::1} == L ]]; then
        direction=-1
    else
        direction=1
    fi

    while (( amount >= 100 )) do
        amount=$((amount-100))
        code=$((code+1))
    done

    new_number_unwrapped=$(( current_number + amount * direction ))
    new_number_wrapped=$(( (new_number_unwrapped % 100 + 100) % 100 ))

    if (( new_number_wrapped == 0 && current_number != 0 )); then
        code=$((code+1))
    else
        if (( (new_number_wrapped < current_number && direction == 1) || (new_number_wrapped > current_number && direction == -1) )); then
            if ((current_number != 0)); then
                code=$((code+1))
            fi
        fi
    fi

    current_number=$new_number_wrapped

    echo "code: $code"
    echo "num: $current_number"
done

echo "----------------------------"
echo "final code: $code"

