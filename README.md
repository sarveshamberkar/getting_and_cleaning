# Getting-and-Cleaning-Data-Week-4-Assignment
* First, download and unzip the data file into your R working directory.
* Second, download the R source code into your R working directory.
* Finally, execute R source code to generate tidy data file.

## Data description

The variables in the data X are sensor signals measured with waist-mounted smartphone from 30 subjects. The variable in the data Y indicates activity type the subjects performed during recording.

## Code explaination

The code was written based on the instruction of this assignment
1. Merges the training and the test sets to create one data set.
Use command rbind to combine training and test set
2. Extracts only the measurements on the mean and standard deviation for each measurement.
Use grep command to get column indexes for variable name contains "mean()" or "std()"
3. Uses descriptive activity names to name the activities in the data set
Convert activity labels to characters and add a new column as factor
4. Appropriately labels the data set with descriptive variable names.
Give the selected descriptive names to variable columns
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

