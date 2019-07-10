#!/bin/bash

mkdir -p "$2"
cd "$2"

if [ ! -e "libsvm" ]; then
	bcp RobsDatabase.dbo.aggClottingTrainingLibSVM out "libsvm" -c -t'\t' -r'\n'  -S 10.174.129.118 -U RobertSQL -P $1
fi

if [ ! -e "matrix" ]; then
	bcp RobsDatabase.dbo.aggClottingTrainingLibSVM out "matrix" -c -t'\t' -r'\n'  -S 10.174.129.118 -U RobertSQL -P $1
fi

subsets=( "libsvm" )
types=( "INR" )
outputs=( "class" "reg" )

for subset in "${subsets[@]}"; do

echo "Generating classifier files"
for type in "${types[@]}"; do


	if [ ! -e "$subset.$type.class" ]; then
		echo "Generating classification file: $subset.$type.class"	
		cat "$subset" | grep $type | awk -F'\t' '{if ($5 >= 0 && $5 <= 2) print $3 "\t" substr($0, index($0, $6))}' > "$subset.$type.class"
	fi

	if [ ! -e "$subset.$type.reg" ]; then
		echo "Generating regression file: $subset.$type.reg"	
		cat "$subset" | grep $type | awk -F'\t' '{if ($5 >= 0 && $5 <= 2) print $4 "\t" substr($0, index($0, $6))}'> "$subset.$type.reg"
	fi
	 
	echo "Generating random split in $subset.$type"
	for outType in "${outputs[@]}"; do
		train="$subset.$type.$outType.train"
		test="$subset.$type.$outType.test"
		length=$(wc -l < "$subset.$type.$outType")
		train_line=$((length*9/10))
		test_line=$((length*1/10))
		echo "lines: $length; split: $train_line / $test_line"
		shuf "$subset.$type.$outType" | tee >(head --lines=$train_line > "$train") >(tail --lines=$test_line > "$test" ) > /dev/null
		# awk -v train="$train" -v test="$test" '{if(rand()<0.9) {print > train} else {print > test}}' "$subset.$type.$outType"
	done

	# Classifier training
## Determine optimal C - not sure what this based on
	# liblinear-train -s 0 -e 0.001 -C $subset.$type.class.train
## 5 fold cross validation
# liblinear-train -s 0 -e 0.001 -v 5 clotting.tsv.INR.class.train
## write the model
# liblinear-train -s 0 -e 0.001 -c 2 clotting.tsv.INR.class.train clotting.tsv.INR.class.model
## do the prediction
# liblinear-predict -b 1 clotting.tsv.INR.class.test clotting.tsv.INR.class.model clotting.tsv.INR.class.test.pred
## get the labels from the training file
# tail -n +2 ~/data/clotting.tsv.INR.class.test.pred > /tmp/skip
## combine this with the predictions.
# awk '{print $1}' ~/data/clotting.tsv.INR.class.test | paste - /tmp/skip | sed 's/\s/\t/g' > ~/data/clotting.tsv.INR.class.test.pred.result

done

done

# cat "$2" | grep 'APTT' | awk -F'\t' '{if ($9 >= 0 && $9 <= 2) print $7 "\t" substr($0, index($0, $10))}' >> "$2.aptt.class"
# cat "$2" | grep 'ARAT' | awk -F'\t' '{if ($9 >= 0 && $9 <= 2) print $7 "\t" substr($0, index($0, $10))}' >> "$2.arat.class"
# cat "$2" | grep 'PT' | awk -F'\t' '{if ($9 >= 0 && $9 <= 2) print $7 "\t" substr($0, index($0, $10))}' >> "$2.pt.class"

# echo "Generating regression files"

# cat "$2" | grep 'APTT' | awk -F'\t' '{if ($9 >= 0 && $9 <= 2) print $8 "\t" substr($0, index($0, $10))}' >> "$2.aptt.reg"
# cat "$2" | grep 'ARAT' | awk -F'\t' '{if ($9 >= 0 && $9 <= 2) print $8 "\t" substr($0, index($0, $10))}' >> "$2.arat.reg"
# cat "$2" | grep 'PT' | awk -F'\t' '{if ($9 >= 0 && $9 <= 2) print $8 "\t" substr($0, index($0, $10))}' >> "$2.pt.reg"

# echo "Splitting class files"
# for i in "$2.*.class"; do
#	train="$i.train"
# 	test="$i.test"
# 	awk -v train="$train" -v test="$test" '{if(rand()>0.9) {print > train} else {print > test}}' "$i"
# done
