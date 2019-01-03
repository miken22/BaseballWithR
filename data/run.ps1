Write-Host "Running the BEVENT TOOL"

Get-ChildItem "L:\BaseballWithR\data\2018" -Filter *.EVA | ForEach-Object {
    $content = Get-Content $_.FullName
    Write-Host $content
}