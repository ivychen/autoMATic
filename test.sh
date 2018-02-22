#!/bin/bash

TESTS_DIR="./tests"

for file in $TESTS_DIR/*.ic
do
	eval "./automatic.native < ${file} > tmp 2> /dev/null"
	filename=$(eval "basename ${file%.*}")
	
	diff=$(eval "diff tmp ${TESTS_DIR}/${filename}.out")

	printf '%s\t\t' "${filename}"

	if [ "${diff}" = "" ]
	then
		printf '%s\n' "OK"
	else
		printf '%s\n' "NOPE"
	fi

	rm tmp
done	
