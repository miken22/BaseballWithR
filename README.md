# Baseball with R

Working through the book by Max Marchi and Jim Albert to become more familiar with R and gain a deeper understanding of my favourite sport.

---

This repository is where scripts will be kept that have been created around the discussions and ideas in *Analyzing Baseball Data with R*, with
the goal of continuing into further analysis of baseball.

## Data files

The data directory excludes all the raw data from the retrosheet website (www.retrosheet.org). Download the files that you wish to use
with the scripts (such as the 2015 play-by-play files) and run the `run.sh` script to generate R compatible CSV files based on the
play-by-play data. The script generates files for all AL games, all NL games, and the entire league combined. The `fields.csv` file
contains the 36 default headers that are needed to identify the columns in the CSV files. A full list of the 97 possible headers can
be found on the Retrosheet website, and the script can be modified to include/exclude additional headers, and then update the fields file.