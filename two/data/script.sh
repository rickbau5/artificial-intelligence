#!/bin/bash

for f in *.txt ; do
    echo $f
    echo Time Elapsed
    cat $f | sed -n -e 's/Time elapsed: \(.*\)/\1/p'
    echo Nodes Examined
    cat $f | sed -n -e 's/Examined \([0-9]*\) nodes\./\1/p'
    echo Pruned Branches
    cat $f | sed -n -e 's/Pruned \([0-9]*\) branches\./\1/p'
    echo Average Branching factor
    cat $f | sed -n -e 's/Average branching .*: \(.*\)/\1/p'
done