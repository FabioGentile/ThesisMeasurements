#!/bin/bash

if [ "$#" -eq 1 ]; then
	./script_daq.R $1
	./script_pt.R $1
  exit 0
fi

for dir in `find . ! -path . -type d`
do
	echo $dir
	./script_daq.R $dir
	./script_pt.R $dir
done