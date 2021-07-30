#!/bin/bash

month='1 2 3 4 5 6 7 8 9 10 11 12'
wkd='MON TUE WED THU FRI SAT SUN'

for m in $month
do
	for d in $wkd
	do
		python EstimateTrafficVolume.py $m $d
		echo $m $d is done
	done
done