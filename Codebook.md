The dataset used in the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
Input files folder:
 ./data/raw/
Output files folder:
 ./data/tidy/
Output files names:
UCI_HAR_Average_Cleaned.txt
UCI_HAR_Cleaned.txt
Variables:
 subject 
 activity 
 tbodyaccmeanx 
 tbodyaccmeany 
 tbodyaccmeanz 
 tbodyaccstdx 
 tbodyaccstdy 
 tbodyaccstdz 
 tgravityaccmeanx 
 tgravityaccmeany 
 tgravityaccmeanz 
 tgravityaccstdx 
 tgravityaccstdy 
 tgravityaccstdz 
 tbodyaccjerkmeanx 
 tbodyaccjerkmeany 
 tbodyaccjerkmeanz 
 tbodyaccjerkstdx 
 tbodyaccjerkstdy 
 tbodyaccjerkstdz 
 tbodygyromeanx 
 tbodygyromeany 
 tbodygyromeanz 
 tbodygyrostdx 
 tbodygyrostdy 
 tbodygyrostdz 
 tbodygyrojerkmeanx 
 tbodygyrojerkmeany 
 tbodygyrojerkmeanz 
 tbodygyrojerkstdx 
 tbodygyrojerkstdy 
 tbodygyrojerkstdz 
 tbodyaccmagmean 
 tbodyaccmagstd 
 tgravityaccmagmean 
 tgravityaccmagstd 
 tbodyaccjerkmagmean 
 tbodyaccjerkmagstd 
 tbodygyromagmean 
 tbodygyromagstd 
 tbodygyrojerkmagmean 
 tbodygyrojerkmagstd 
 fbodyaccmeanx 
 fbodyaccmeany 
 fbodyaccmeanz 
 fbodyaccstdx 
 fbodyaccstdy 
 fbodyaccstdz 
 fbodyaccjerkmeanx 
 fbodyaccjerkmeany 
 fbodyaccjerkmeanz 
 fbodyaccjerkstdx 
 fbodyaccjerkstdy 
 fbodyaccjerkstdz 
 fbodygyromeanx 
 fbodygyromeany 
 fbodygyromeanz 
 fbodygyrostdx 
 fbodygyrostdy 
 fbodygyrostdz 
 fbodyaccmagmean 
 fbodyaccmagstd 
 fbodybodyaccjerkmagmean 
 fbodybodyaccjerkmagstd 
 fbodybodygyromagmean 
 fbodybodygyromagstd 
 fbodybodygyrojerkmagmean 
 fbodybodygyrojerkmagstd
Steps to clean a source dataset:
extracts only the measurements on the mean and standard deviation for each measurement
use lower case for column/variable names
replace non word (like space, underscore etc) symbols in column names
merge train and test datasets
use activity label instead of id