#!/bin/bash
# Run eventime experiment from delta=1 to delta=20
# Yang Xu
# 1/11/2017

for i in {1..20}
do
    python3 maptask_timeunit_eventime.py $i
    /usr/bin/python maptask_timeunit_entropy.py $i
    echo "delta=$i done"
done
