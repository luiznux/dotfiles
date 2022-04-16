#!/bin/sh

# Chose the right output dependending on which system are you using
# Just uncomment one output  and see if it work

#notebook Intel format
#sensors | grep "CPU" | cut -b 18,19,22,23,24 | awk '{print $1}'

#desktop Intel format
#sensors | grep "Package id 0" | cut -b 17,18,21,22,23 | awk '{print $1}'

#Amd CPU format
#sensors | grep "Tctl" | cut -b 16,17,20,21,22 | awk '{print $1}'
