#!/bin/bash
cd tests
for i in test-*.input ; do
        echo -n $i ; 
        rm -f $i.mine ; 
        ../solver < $i > $i.mine ; 
        diff -w -b -B $i.mine `basename $i .input`.output > /dev/null && echo " passed" || echo " FAILED" 
done 
    


