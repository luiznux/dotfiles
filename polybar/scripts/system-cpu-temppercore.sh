#!/bin/sh

#notebook format
temp=$(sensors | grep "CPU" | cut -b 18,19,20,21,22,23,24)

#desktop format
#temp=$(sensors | grep "Package id 0" | cut -b 17,18,19,20,21,22,23)

echo $temp
