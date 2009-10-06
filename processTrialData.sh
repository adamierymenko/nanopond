#!/bin/bash

#
# This script should be executed from *within* the directory containing
# a series of compressed .dump.csv.bz2 files. (If all are not compressed,
# they should be.)
#
# This script will run each file through the information content test and
# then generate a merged trial-data.csv file ready for statistical
# and graphical analysis.
#
# The bzip2/bunzip2 programs must be installed.
#

# Delete old results if any
rm -f *.packed trial-data.csv

# Figure out the nonredundant information content of all the genomes in
# each genome dump.
for i in *.dump.csv.bz2; do
	bunzip2 -dc $i | ruby ../genomeDumpPredigestInformation.rb | bzip2 -9 >$i.packed
	echo $i digested to $i.packed
done
