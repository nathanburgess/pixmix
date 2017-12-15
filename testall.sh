#!/bin/bash

clrClear='\033[0m'
clrBlue='\033[1;34m'
clrPurple='\033[1;35m'
clrGreen='\033[1;32m'
clrRed='\033[31;01m'
clrYellow='\033[33;01m'

result=true

PASS_FILES="tests_pass/*.pm"
FAIL_FILES="tests_fail/*.pm"

printf "\n${clrGreen}--==[ ${clrBlue}Running test suite... ${clrGreen}]==--${clrClear}\n\n"

runTest() {
    output_file=temp.out
    exe_file=temp.exe

    ./pixmix.native $1 &> temp.ll
   
    clang -Wno-override-module utils.bc temp.ll -o ${exe_file} &>/dev/null
    
    if [ -e "$exe_file" ]; then
        ./${exe_file} > $output_file

        if [ "$(tail -1 $output_file)" == "finished" ] && [ $2 == "pass" ]; then
            printf "  %-40s ${clrGreen}PASS\n${clrClear}" "$1" 1>&2
        else
            printf "  %-40s ${clrRed}FAIL\n${clrClear}" "$1" 1>&2
        fi

        rm ${exe_file} &>/dev/null
        rm ${output_file} &>/dev/null
    else
        if [ $2 == "fail" ]; then
            printf "  %-40s ${clrGreen}PASS\n${clrClear}" "$1" 1>&2
        else
            printf "  %-40s ${clrRed}FAIL\n${clrClear}" "$1" 1>&2
        fi
    fi

    rm temp.ll >/dev/null
}

for input_file in $PASS_FILES; do
    runTest $input_file "pass"
done

printf "\n"
for input_file in $FAIL_FILES; do
    runTest $input_file "fail"
done

exit 0