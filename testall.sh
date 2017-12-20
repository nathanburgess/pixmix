#!/bin/bash

clrClear='\033[0m'
clrBlue='\033[1;34m'
clrPurple='\033[1;35m'
clrGreen='\033[1;32m'
clrRed='\033[31;01m'
clrYellow='\033[33;01m'

PASS_FILES="tests_pass/*.pm"
FAIL_FILES="tests_fail/*.pm"
output_file=temp.out
exe_file=temp.exe
count=0

printf "\n${clrGreen}--==[ ${clrBlue}Running test suite... ${clrGreen}]==--${clrClear}\n\n"

runTest() {
    count=$((count + 1))
    name=$1
    if [ $2 == "pass" ]; then 
        testName=${name/tests_pass\//}
    else
        testName=${name/tests_fail\//}
    fi

    ./pixmix.native $1 &> temp.ll
   
    clang -Wno-override-module lib/utils.bc temp.ll -o ${exe_file} -lm &>/dev/null
    
    if [ -e "$exe_file" ]; then
        ./${exe_file} > $output_file

        if [ "$(tail -1 $output_file)" == "finished" ] && [ $2 == "pass" ]; then
            printf "  %-30s ${clrGreen}PASS${clrClear}" ${testName} 1>&2
            if [ $((count % 2)) = 0 ]; then 
                printf "\n" 
            fi
        else
            printf "  %-30s ${clrRed}FAIL${clrClear}" ${testName} 1>&2
            if [ $((count % 2)) = 0 ]; then 
                printf "\n" 
            fi
        fi

        rm ${exe_file} &>/dev/null
        rm ${output_file} &>/dev/null
    else
        if [ $2 == "fail" ]; then
            printf "  %-30s ${clrGreen}PASS${clrClear}" ${testName} 1>&2
            if [ $((count % 2)) = 0 ]; then 
                printf "\n" 
            fi
        else
            printf "  %-30s ${clrRed}FAIL${clrClear}" ${testName} 1>&2
            if [ $((count % 2)) = 0 ]; then 
                printf "\n" 
            fi
        fi
    fi

    rm temp.ll >/dev/null
}

for input_file in $PASS_FILES; do
    runTest $input_file "pass"
done

if [ $((count % 2)) != 0 ]; then 
    printf "\n" 
fi

count=0 
printf "\n"
for input_file in $FAIL_FILES; do
    runTest $input_file "fail"
done

exit 0