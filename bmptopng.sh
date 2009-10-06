#!/bin/bash

while /bin/true; do
	BMPS=`ls *.bmp | sort -g`
	sleep 1
	if [ "$BMPS" != "" ]; then
		for i in $BMPS; do
			PNG=`echo $i | sed 's/.bmp$/.png/'`
			PNG2=`echo 000000000000000${PNG} | sed 's/.*\(.\{25\}\)/\1/'`
			bmptopnm $i | pnmtopng -compression 9 >>$PNG2
			if [ -f $PNG2 ]; then
				rm -f $i
			fi
		done
	fi
done
