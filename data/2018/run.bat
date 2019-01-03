REM American League files
for /f %%f in (`dir /b L:\BaseballWithR\data\2018`) do
    ./BEVENT.EXE -y 2018 -f 0-9,12-13,16-17,26-40,43-45,51,58-61 "$filename" >> al_data.txt
done

cat al_data.txt >> all_data.txt

REM National League files
for filename in *.EVN; do
    ./BEVENT.EXE -y 2018 -f 0-9,12-13,16-17,26-40,43-45,51,58-61 "$filename" >> nl_data.txt
done

cat nl_data.txt >> all_data.txt

mv all_data.txt all_data.csv
mv nl_data.txt nl_data.csv
mv al_data.txt al_data.csv

REM Generate a master list of players for the season
echo "playerID, Last_Name,  First_Name, BH, TH, teamID, Position" >> roster.csv
for filename in *.ROS; do
	cat "$filename" >> roster.csv
done

