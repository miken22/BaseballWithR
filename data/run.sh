#!/bin/bash
#
# This script quickly runs the BEVENT program to generate the game event files for all retrosheet
# play-by-play data. The output is stored in a CSV that can be imported with R or other tools. The
# list of fields has been modified to include the pitch sequence for further analysis.
#
# Assumes script is being run in directory with .EVA/EVN files
#
# Written by Mike Nowicki

YEAR="$1"
if [ -z "$1" ]
then
	echo "No YEAR provided, using 2017"
	YEAR=2017
fi

for filename in *.EVA; do
    ./BEVENT.EXE -y $YEAR -f 0-9,12-13,16-17,26-40,43-45,51,58-61 "$filename" >> al_data.txt
done

cat al_data.txt >> all_data.txt

# National League files
for filename in *.EVN; do
    ./BEVENT.EXE -y $YEAR -f 0-9,12-13,16-17,26-40,43-45,51,58-61 "$filename" >> nl_data.txt
done

cat nl_data.txt >> all_data.txt

mv all_data.txt all_data.csv
mv nl_data.txt nl_data.csv
mv al_data.txt al_data.csv

# Generate a master list of players for the season
echo "playerID, Last_Name,  First_Name, BH, TH, teamID, Position" >> roster.csv
for filename in *.ROS; do
	cat "$filename" | roster.csv
done

