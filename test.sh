#!/bin/bash

./pixmix.native $1 > a.ll
clang -Wno-override-module utils.bc a.ll -o $1.exe

./$1.exe
rm a.ll
rm ./$1.exe