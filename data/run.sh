#!/bin/bash
# This script quickly runs the BEVENT program to generate the game event files for all retrosheet
# play-by-play data. The output is stored in a CSV that can be imported with R or other tools. The
# BEVENT command can be modified to include additional columns for output, but currently only does the 
# default 36 columns that match the fields.csv file.
#
# Assumes script is being run in directory with .EVA/EVN files
#
# Written by Mike Nowicki 

# American League Files
for filename in *.EVA; do	
    ./BEVENT.EXE "$filename" >> al_data.txt
done

cat al_data.txt >> all_data.txt

# National League files
for filename in *.EVN; do
    ./BEVENT.EXE "$filename" >> nl_data.txt
done

cat nl_data.txt >> all_data.txt

mv all_data.txt all_data.csv
mv nl_data.txt nl_data.csv
mv al_data.txt al_data.csv
