#!/usr/bin/env bash

[[ $# -eq 0 ]] && echo "Usage: $0 <number of transactions to generate>" && exit || echo "Generating $1 transactions into transactions.csv";

MIN_AMOUNT=1; MAX_AMOUNT=500; MIN_ID=1; MAX_ID=10; OUTPUT_FILE=./etc/transactions.csv
touch $OUTPUT_FILE && rm -f $OUTPUT_FILE;

for ((i=1;i<=$1;i++));
do
	send=$(shuf -i $MIN_ID-$MAX_ID -n 1);
	recv=$(shuf -i $MIN_ID-$MAX_ID -n 1);
	amount=$(shuf -i $MIN_AMOUNT-$MAX_AMOUNT -n 1);
	echo -e "$send,$recv,$amount" >> $OUTPUT_FILE;
done
