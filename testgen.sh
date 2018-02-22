#!/bin/bash

# automatically generate output for use with test.sh
# this assumes that everything is working correctly in the executable

TESTS_DIR="./tests"

rm "${TESTS_DIR}/*.out"

for file in $TESTS_DIR/*.ic
do
        filename=$(eval "basename ${file%.*}")
        output=$(eval "./automatic.native < ${file} > ${TESTS_DIR}/${filename}.out")
done
