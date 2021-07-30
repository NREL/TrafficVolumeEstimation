#!/bin/bash

month='1 2 3 4 5 6 7 8 9 10 11 12'
wkd='Monday Tuesday Wednesday Thursday Friday Saturday Sunday'

for m in $month
do
	for d in $wkd
	do
		Rscript CreateInputData.R $m $d
		echo $m $d is done
	done
done
