#!/bin/sh

speed=$(sensors | grep "Processor Fan" | cut -d " " -f 3)

if [ -z $speed ]; then
    echo off

 else
     echo $speed rpm
 fi
