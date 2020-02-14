#!/bin/sh

temp=$(sensors | grep "CPU" | cut -d: -f2)

echo $temp
