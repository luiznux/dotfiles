#!/bin/sh

#notebook format
temp=$(sensors | grep "CPU" | cut -b 18,19,22,23,24)

#desktop format
#temp=$(sensors | grep "Package id 0" | cut -b 17,18,21,22,23)

#Amd format
#temp=$(sensors | grep "CPUTIN" | cut -b 29,30,33,34,35,36,37)

echo $temp
