# Retail-Project
Solved a buisness problem using Predictive Modelling in R.

PROBLEM STATEMENT & OTHER INSTRUCTIONS:

This data set is related with retail domain and challenge is to predict whether a store should get opened or not based on certain factors such as sales, population,area etc.

We have given you two datasets , store_train.csv and store_test.csv . You need to use data store_train to build predictive model for response variable ‘store’. store_test data contains all other factors except ‘store’, you need to predict that using the model that you developed and submit your predicted values in a csv files.

You have to submit the probability scores, not the hard classes.

If you are using decision trees or random forest here, probability scores can be calculated as

score=predict(rf_model,newdata= testdata, type="prob")[,2]

score=predict(tree_model,newdata= testdata, type=‘vector’)[,2]


Note : you need to submit probability of outcome being 1 

Evaluation Criterion : auc score on test data. larger auc score, better Model



Please read through the points given below before you begin :

1. Your auc score for test data should come out to be more than 0.810

2. You are NOT required to submit R script. However in some cases , we might ask you to send your script separately in order to verify that your submissions is a result of models that you built .

3. you can submit as many times as you want, we'll put best score obtained on the respective leader board. 

4. Your predictions should not contain any NA values.

5. You are are free to use any predictive modelling technique

Feel free to take to QA forum for this specific project for any clarifications. you will find this under the same tab



