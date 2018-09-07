# Kaggle in-class competition: Predicting-BookStore-Spending
 

Introduction:

On Nov 25, 2014 a leading German book retailer sent out an offer to its customers. They observed the purchases that were made in response to this offer. Your task is to use information that was available to the company as of Nov 25, 2014 to construct a machine learning model that predicts the spending in response to the offer.

The orders.csv file has the transaction history that was known as of Nov 24, 2014, which will be called the base period. You should create feature variables from this data set. The customer.csv contains the responses to the offer in the future period and has two variables, a customer id and logtarg which is the natural logarithm of the amount spent plus 1. Thus, those who did not respond will have a value of log(0+1) = 0. You have response data for about 5K records. You have data from the base period for these 5K training customers and about 11K in a test sample. Estimate your models using the 5K training sample, then apply the model to the test set with the predict function in R. Submit csv files with only the test-set customers to Kaggle with two variables, the customer id and your predicted log amount.

You will be evaluated on the RMSE of your predictions on a test set
