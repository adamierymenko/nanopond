#!/bin/bash

while /bin/true; do
	BMPS=`ls *.bmp | sort -g`
	sleep 1
	if [ "$BMPS" != "" ]; then
		for i in $BMPS; do
			SGI=`echo $i | sed 's/.bmp$/.sgi/'`
			SGI2=`echo 000000000000000${SGI} | sed 's/.*\(.\{25\}\)/\1/'`
			bmptopnm $i | pnmtosgi >>$SGI2
			if [ -f $SGI2 ]; then
				rm -f $i
			fi
		done
	fi
done
