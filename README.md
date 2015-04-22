# GettingCleaningData_project
Repository contains R code (run_analysis.R), code book (code_book.txt), and
readme
(README.md) for the Getting and Cleaning Data course project.

#First you must download the Samsung data, decompress (unzip) the data, and
change your working directory to the parent directory containing the unzipped
data files. Please see the README.txt file within the UCI HAR Dataset directory for details on the contents of the Samsung data and file structure.

The following commands can be used to download, unzip, and set the working directory:
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "./dataset.zip")
unzip("./dataset.zip")
setwd("./UCI HAR Dataset")

#Next the data is read into R using read.table() and combined into one large dataframe.
