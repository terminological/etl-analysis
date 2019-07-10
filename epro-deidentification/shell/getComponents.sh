#!/bin/bash

# mkdir -p "$2"
# cd "$2"


if [ ! -e "address" ]; then
	bcp RobsDatabase.dbo.deidAddressComponents out "address" -c -t'\t' -r'\n'  -S 10.174.129.118 -U RobertSQL -P $1
fi

if [ ! -e "clinician" ]; then
	bcp RobsDatabase.dbo.deidClinicianNameComponents out "clinician" -c -t'\t' -r'\n'  -S 10.174.129.118 -U RobertSQL -P $1
fi

if [ ! -e "location" ]; then
	bcp RobsDatabase.dbo.deidLocationNameComponents out "location" -c -t'\t' -r'\n'  -S 10.174.129.118 -U RobertSQL -P $1
fi

if [ ! -e "lastname" ]; then
	bcp RobsDatabase.dbo.deidLastNameComponents out "lastname" -c -t'\t' -r'\n'  -S 10.174.129.118 -U RobertSQL -P $1
fi

if [ ! -e "firstname" ]; then
	bcp RobsDatabase.dbo.deidFirstNameComponents out "firstname" -c -t'\t' -r'\n'  -S 10.174.129.118 -U RobertSQL -P $1
fi

if [ ! -e "addressList" ]; then
	bcp RobsDatabase.dbo.deidAddresses out "addressList" -c -t'\t' -r'\n'  -S 10.174.129.118 -U RobertSQL -P $1
fi
