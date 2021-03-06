# Getting and Cleaning Data Course Project

## Project Description

This repo contains my submission for the final course project for the Coursera course "Getting and Cleaning Data." This project depends on understanding how to collect and manipulate data to create a tidy data set that can be used for futher analysis

## Data

Data for this project can be found here:

<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

Further information about the data set can be found here:

<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>

## Files

`CodeBook.md` describes the data, variables, and transformations required to complete this project.


`run_analysis.R` performs preparatory steps, followed by the five steps given in the instructions for the course project:

* Merges the training and the test sets to create one data set.

* Extracts only the measurements on the mean and standard deviation for each measurement.

* Uses descriptive activity names to name the activities in the data set

* Appropriately labels the data set with descriptive variable names.

* From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


`tidy_data.txt` is the final exported tidy data set created with `run_analysis.R`