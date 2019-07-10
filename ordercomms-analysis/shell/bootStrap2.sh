#!/bin/bash

# if [ -z ${2+x} ]; then 
#	end=$2
#else 
#	end=10
#fi

rm medians.txt
touch medians.txt

declare -a arr=()

typeset -i i end
let end=10000 i=1
while ((i<=end)); do 
	echo $i;
#	echo $lines;
	for j in 50000 10000 5000 2000 1000 500 200 100; do
		k=$((j/2))
		m=$(shuf -n $j $1 | sort -n | sed -n "${k},${k}p")
		echo "$j	$m" >> medians.txt;
	done
	let i++;
done

#echo $(sort -n medians.txt | sed -n "950,950p")
#echo $(sort -n medians.txt | sed -n "500,500p")
#echo $(sort -n medians.txt | sed -n "50,50p")
