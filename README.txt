BACKGROUND:
This file was created based on analysis of the Samsung smartphone website http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Where a series of experiments were made to monitor sensor signals by a group of 30 volunteers.
A Tidy data set was created as basis for further analysis of the data.

PROCEDURE:
The procedure for creating the tidy data set is as follows:
1.	Read observation data 
2.	Read activities associated with the data
3.	Read features associated with the data
4.	For each feature, identify only the mean and standard deviation observations, and retrieve the corresponding observation info and also corresponding activity
5.	Create tidy data set based on Activity (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING), Variable(Mean or Standard Deviation), and the Subject (X, Y, Z, or ALL) that carried out the experiment.  For each of these combinations, calculate an average measurement and include that in the tidy data set.  
6.	Remove duplicate rows in the tidy data set.

