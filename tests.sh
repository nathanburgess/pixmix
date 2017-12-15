#!/bin/bash

clrClear='\033[0m'
clrBlue='\033[1;34m'
clrPurple='\033[1;35m'
clrGreen='\033[1;32m'
clrRed='\033[31;01m'
clrYellow='\033[33;01m'

result=true

INPUT_FILES="tests_pass/*.pm"

printf "\n${clrGreen}--==[ ${clrBlue}Running all tests... ${clrGreen}]==--${clrClear}\n"

for input_file in $INPUT_FILES; do
    ./pixmix.native $input_file | lli $1
    if [ "$?" -eq 0 ]; then
       printf "%-65s ${clrYellow}YES\n${clrClear}" "  - checking $input_file..."
    else
       printf "%-65s ${clrPurple}NO\n${clrClear}" "  - checking $input_file..." 1>&2
    	result=false
    fi
done
INPUT_FILES="tests_fail/*.pm"

for input_file in $INPUT_FILES; do
        ./pixmix.native $input_file | lli $1
            if [ "$?" -eq 0]; then
               printf "%-65s ${clrYellow}NO\n${clrClear}" "  - checking $input_file..."
            else
               printf "%-65s ${clrPurple}YES\n${clrClear}" "  - checking $input_file..." 1>&2
                 result=false
            fi
       done
               exit 0
