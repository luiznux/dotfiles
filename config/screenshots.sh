#!/bin/bash

scrot '%d-%m-%Y__%H-%M-%S.png'  -e 'mv $f ~/Pictures/Screenshots/';
notify-send -u normal "
 Screenshot take!"

