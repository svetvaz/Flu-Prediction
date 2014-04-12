Flu-Prediction
==============

Using machine learning techniques such as Linear regression, K nearest neighbors, rpart, random forests and Support vector machines to predict the onset of flu in various regions of the US

Setup
1. Using the R console, set working directory to the folder that contains the data files (data) :
   e.g. setwd('/users/svetlana/flu_prediction/data')
2. CalculateTargetColumn.R is used to compute the target 'ILI Severity' using a formula similar to what CDC uses -> generates fluData.csv
3. Copy the ILI Severity column from fluData.csv to your cleaned up data set 'FinalDataSet_3_3.csv'


Instructions to run the code
	From R-studio
	1. Open the file, click on 'Run code as-> Run all'
1. First run CalculateTargetColumn.R to generate the target column
2. Once you have the data set with the target column run predictILISeverity.R to predict ILI Severity
3. Run predictFluPositive.R to predict the flu positive percent
4. Run predictFluSeverity.R to predict the flu severity
