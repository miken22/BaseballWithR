Write-Host "Running the BEVENT TOOL"

Get-ChildItem "L:\BaseballWithR\data\2018" -Filter *.EVA | ForEach-Object {
    ./BEVENT.EXE -y 2018 -f 0-9,12-13,16-17,26-40,43-45,51,58-61 $_ | Out-File -FilePath .\al_data.txt -Append
}

Get-ChildItem "L:\BaseballWithR\data\2018" -Filter *.EVN | ForEach-Object {
    ./BEVENT.EXE -y 2018 -f 0-9,12-13,16-17,26-40,43-45,51,58-61 $_ | Out-File -FilePath .\nl_data.txt -Append
}

Get-ChildItem "L:\BaseballWithR\data\2018" -Filter *.ROS | ForEach-Object {
	cat $_ | Out-File -FilePath .\roster_2.csv -Append
}
