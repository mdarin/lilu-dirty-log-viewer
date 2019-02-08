#!/bin/bash

#example:
#$ fname="a.tar.gz"
#$ echo ${fname##*.}
#gz
#$ echo ${fname%.*}
#a.tar
#
for i in $(ls | grep _.{3,3}erl$)
do
	echo "file: $i" 
#	echo "${i_%.*}"
	echo "${i%*.}"
	echo "${i%*_}"
	echo "----"
done

for i in $(ls | grep src$)
do
	echo "file: $i" 
	echo "postfix: ${i#*.}"
	echo "----"
done
