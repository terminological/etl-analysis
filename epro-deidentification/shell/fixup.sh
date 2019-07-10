#!/bin/bash

cd ~/Dropbox/deidentification/components

sort -n -r -k 3 firstname | awk '{print $3 "\t" $1 "\t" $2}' > firstnameFreq
sort -n -r -k 2 lastname | awk '{print $2 "\t" $1}' > lastnameFreq
sort -n -r -k 2 address | awk '{print $2 "\t" $1}' > addressFreq
sort -n -r -k 2 location | awk '{print $2 "\t" $1}' > locationFreq
sort -n -r -k 2 clinician | awk '{print $2 "\t" $1}' > clinicianFreq

