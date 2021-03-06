# Code Book-- Getting and Cleaning Data Course Project

This codebook describes the variables and data used in this project, as well as how the `run_analysis.R` script transforms the data to produce the `tidy_data.txt` output file.

## Data

Data for this project was downloaded at: <https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>.

The data set includes these files:

* 'README.txt'

* 'features_info.txt': Shows information about the variables used on the feature vector.

* 'features.txt': List of all features.

* 'activity_labels.txt': Links the class labels with their activity name.

* 'train/X_train.txt': Training set.

* 'train/y_train.txt': Training labels.

* 'test/X_test.txt': Test set.

* 'test/y_test.txt': Test labels.

### Dataset Description

A full description of this data set is available at: <http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>.

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

This data set includes the following attributes:

* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.

* Triaxial Angular velocity from the gyroscope.

* A 561-feature vector with time and frequency domain variables.

* Its activity label.

* An identifier of the subject who carried out the experiment.

### License information

The README.txt file included with the data set includes the following license information:

Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

## Transformation Methods

### Overview

The transformation involves the following steps:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Preparatory Steps

This analysis requires the packages `dplyr` and `data.table`.

The data set should be downloaded and extracted, which creates the folder `UCI HAR Dataset`.

Files in the data set are read and assigned to variables as follows:

`features <- features.txt` : 561 rows, 2 columns

`activities <- activity_labels.txt` : 6 rows, 2 columns

`subject_test <- test/subject_test.txt` : 2947 rows, 1 column

`x_test <- test/X_test.txt` : 2947 rows, 561 columns

`y_test <- test/y_test.txt` : 2947 rows, 1 columns

`subject_train <- test/subject_train.txt` : 7352 rows, 1 column

`x_train <- test/X_train.txt` : 7352 rows, 561 columns

`y_train <- test/y_train.txt` : 7352 rows, 1 columns

### Merging the data

`x_merge` (10299 rows, 561 columns) combines `x_train` and `x_test` with `rbind()`.

`y_merge` (10299 rows, 1 column) combines `y_train` and `y_test` with `rbind()`.

`subj_merge` (10299 rows, 1 column) combines `subject_train` and `subject_test` with `rbind()`. 

`Data_merged` (10299 rows, 563 column) combines `subj_merge`, `y_merge` and `x_merge` using `cbind()`.

### Selecting data for mean and standard deviation

`Data_selected` (10299 rows, 88 columns) selecting only these columns from `Data_merged`: `subject`, `code` and the `mean` and standard deviation (`std`) measurements for each measurement.

### Adding descriptive activity names to the data set

Entries in the `code` column of `Data_selected` are replaced with corresponding activity labels sourced from second column of `activities`.

These labels are:

* `WALKING`

* `WALKING_UPSTAIRS`

* `WALKING_DOWNSTAIRS`

* `SITTING`

* `STANDING`

* `LAYING`

### Changing the activty labels to appropriately descriptive variable names

`code` column in `Data_selected` renamed `activities`

`Acc` in column names replaced with `Accelerometer`

`Gyro` in column names replaced with `Gyroscope`

`BodyBody` in column names replaced with `Body`

`Mag` in column name replaced with `Magnitude`

Start of column names beginning with character `f` replaced with `Frequency`

Start  of column names beginning with character `t` replaced with `Time`

`tBody` in column names is replaced with `TimeBody`.

`-mean()` in column names is replaced with `Mean`.

`-std()` in column names is replaced with `STD`.

`-freq()` in column names is replaced with `Frequency`.

`angle` in column names is replaced with `Angle`.

`gravity` in column names is replaced with `Gravity`.

### Creating and saving the new tidy data set

`Finalized` (180 rows, 88 columns) is created from `Data_selected` by grouping the data by subject and activity, and then calculating the means of each variable for each activity and each subject.

`Finalized` is then used to create the file `tidy_data.txt` using `write.table`.

## Variables

The final `tidy_data.txt` file includes the following variables:

`subject`

`activity`                                          
`TimeBodyAccelerometer.mean...X`                    
`TimeBodyAccelerometer.mean...Y`                    
`TimeBodyAccelerometer.mean...Z`                    
`TimeGravityAccelerometer.mean...X`                 
`TimeGravityAccelerometer.mean...Y`                 
`TimeGravityAccelerometer.mean...Z`                 
`TimeBodyAccelerometerJerk.mean...X`                
`TimeBodyAccelerometerJerk.mean...Y`                
`TimeBodyAccelerometerJerk.mean...Z`                
`TimeBodyGyroscope.mean...X`                        
`TimeBodyGyroscope.mean...Y`                        
`TimeBodyGyroscope.mean...Z`                        
`TimeBodyGyroscopeJerk.mean...X`                    
`TimeBodyGyroscopeJerk.mean...Y`                    
`TimeBodyGyroscopeJerk.mean...Z`                   
`TimeBodyAccelerometerMagnitude.mean..`             
`TimeGravityAccelerometerMagnitude.mean..`        
`TimeBodyAccelerometerJerkMagnitude.mean..`         
`TimeBodyGyroscopeMagnitude.mean..`                 
`TimeBodyGyroscopeJerkMagnitude.mean..`             
`FrequencyBodyAccelerometer.mean...X`               
`FrequencyBodyAccelerometer.mean...Y`               
`FrequencyBodyAccelerometer.mean...Z`             
`FrequencyBodyAccelerometer.meanFreq...X`           
`FrequencyBodyAccelerometer.meanFreq...Y`           
`FrequencyBodyAccelerometer.meanFreq...Z`           
`FrequencyBodyAccelerometerJerk.mean...X`           
`FrequencyBodyAccelerometerJerk.mean...Y`           
`FrequencyBodyAccelerometerJerk.mean...Z`          
`FrequencyBodyAccelerometerJerk.meanFreq...X`       
`FrequencyBodyAccelerometerJerk.meanFreq...Y`      
`FrequencyBodyAccelerometerJerk.meanFreq...Z`       
`FrequencyBodyGyroscope.mean...X`                
`FrequencyBodyGyroscope.mean...Y`                   
`FrequencyBodyGyroscope.mean...Z`                   
`FrequencyBodyGyroscope.meanFreq...X`               
`FrequencyBodyGyroscope.meanFreq...Y`               
`FrequencyBodyGyroscope.meanFreq...Z`               
`FrequencyBodyAccelerometerMagnitude.mean..`        
`FrequencyBodyAccelerometerMagnitude.meanFreq..`    
`FrequencyBodyAccelerometerJerkMagnitude.mean..`   
`FrequencyBodyAccelerometerJerkMagnitude.meanFreq..`
`FrequencyBodyGyroscopeMagnitude.mean..`           
`FrequencyBodyGyroscopeMagnitude.meanFreq..`        
`FrequencyBodyGyroscopeJerkMagnitude.mean..`        
`FrequencyBodyGyroscopeJerkMagnitude.meanFreq..`   
`Angle.TimeBodyAccelerometerMean.Gravity.`          
`Angle.TimeBodyAccelerometerJerkMean..GravityMean.` 
`Angle.TimeBodyGyroscopeMean.GravityMean.`        
`Angle.TimeBodyGyroscopeJerkMean.GravityMean.`     
`Angle.X.GravityMean.`                              
`Angle.Y.GravityMean.`                             
`Angle.Z.GravityMean.`                              
`TimeBodyAccelerometer.std...X`                     
`TimeBodyAccelerometer.std...Y`                     
`TimeBodyAccelerometer.std...Z`                     
`TimeGravityAccelerometer.std...X`                  
`TimeGravityAccelerometer.std...Y`                 
`TimeGravityAccelerometer.std...Z`                  
`TimeBodyAccelerometerJerk.std...X`                 
`TimeBodyAccelerometerJerk.std...Y`                
`TimeBodyAccelerometerJerk.std...Z`                 
`TimeBodyGyroscope.std...X`                         
`TimeBodyGyroscope.std...Y`                         
`TimeBodyGyroscope.std...Z`                         
`TimeBodyGyroscopeJerk.std...X`                     
`TimeBodyGyroscopeJerk.std...Y`                     
`TimeBodyGyroscopeJerk.std...Z`                     
`TimeBodyAccelerometerMagnitude.std..`              
`TimeGravityAccelerometerMagnitude.std..`           
`TimeBodyAccelerometerJerkMagnitude.std..`          
`TimeBodyGyroscopeMagnitude.std..`                  
`TimeBodyGyroscopeJerkMagnitude.std..`              
`FrequencyBodyAccelerometer.std...X`                
`FrequencyBodyAccelerometer.std...Y`                
`FrequencyBodyAccelerometer.std...Z`                
`FrequencyBodyAccelerometerJerk.std...X`            
`FrequencyBodyAccelerometerJerk.std...Y`            
`FrequencyBodyAccelerometerJerk.std...Z`            
`FrequencyBodyGyroscope.std...X`                    
`FrequencyBodyGyroscope.std...Y`                    
`FrequencyBodyGyroscope.std...Z`                    
`FrequencyBodyAccelerometerMagnitude.std..`         
`FrequencyBodyAccelerometerJerkMagnitude.std..`     
`FrequencyBodyGyroscopeMagnitude.std..`             
`FrequencyBodyGyroscopeJerkMagnitude.std..`         