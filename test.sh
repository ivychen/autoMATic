#!/bin/bash

# TODO: Merge these two later when we know we haven't broken anything.
TESTS_DIR="./tests"
LLVMTESTS_DIR="./tests_llvm"
RETVAL=0

for file in $TESTS_DIR/*.ic
do
	touch tmp

	eval "./automatic.native -a < ${file} > tmp 2> /dev/null"
	filename=$(eval "basename ${file%.*}")
	
	diff=$(eval "diff tmp ${TESTS_DIR}/${filename}.out")

	printf '%s\t\t' "${filename}"

	if [ "${diff}" = "" ]
	then
		printf '%s\n' "OK"
	else
		printf '%s\n' "NOPE"
		RETVAL=1
	fi

	rm -f tmp
done

for file in $LLVMTESTS_DIR/*.ic
do
	touch tmp

	./automatic.native -c "${file}" > "${file}.ll" &&
	llc "${file}.ll" -o "${file}.o" &&
	cc "${file}.o" -o "${file}.exe" &&
	"./${file}.exe" > tmp

	filename=$(eval "basename ${file%.*}")
	
	diff=$(eval "diff tmp ${LLVMTESTS_DIR}/${filename}.out")

	printf '%s\t\t' "${filename}"

	if [ "${diff}" = "" ]
	then
		printf '%s\n' "OK"
	else
		printf '%s\n' "NOPE"
		RETVAL=1
	fi

	rm -f tmp
done

exit $RETVAL
