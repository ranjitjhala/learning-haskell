#!/bin/bash

cd tests

for f in *.test ; do
	fname="${f%.*}"
	cat "$f" | ../imp > "$fname.result"
	if diff -b -w "$fname.result" "$fname.answer"
	then 
		echo "*** $fname passed"
	else 
		echo "*** $fname FAILED" 
	fi
done

cd ..

