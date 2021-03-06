# CodeBook for run_analysis_output.txt

## Descriptions of variables in run_analysis_output.txt file:

- subject  : the numeric value used to identify a person participating in the data collection

- activity : the character string representing the activity that was being measured in the observation

- all other columns in the file : represent a *mean* value of the measurement named in the column.
  The mean was calculated by grouping the original data by subject and activity and taking the mean 
  over each group. The column names were **not** modified to reflect they are a mean of the measurement
  named by the column heading. So, please keep in mind that if a column name contains the substring
  "Mean" that you are looking at an average of those means. Also, if you're looking at a column name 
  that constains the substring "StdDev" you are looking at an *average* of the original standard 
  deviations measurement of that same name. Measurements are in units of Hz.

## How the run_analysis_output.txt file was generated:

The dataset in run_analysis_output.txt file represents a final product of a synthesis and summarization of raw
files from the UCI HAR Dataset. The original data and its metadata was spread across multiple files and segregated into separate versions for the training data and the test data. 

*The run_analysis_output.txt dataset was created by performing the following transformations on the original data:*

- The training and test data were merged into a single dataset. 
- The subject variable was left in its original state. 
- The original original numeric code values for the activity column were replaced with their corresponding activity name.
- The dataset was filtered such that the only measurement columns that were kept were those containing the substring "-mean()" or "-std()" in their name
- The measurement columns were slightly renamed such that the "-mean()" and "-std()" substrings were replaced with substrings "-Mean" and "-StdDev", respectively, to enhance readability only.
- The dataset was grouped by subject and activity  and a mean was taken over each of the groups in the  measurement columns.
- The final file has one header row and " " (a space) as a separator.


