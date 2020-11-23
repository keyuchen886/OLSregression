# OLSregression
This package intend to compute linear regression from scratch. It will closely mimic the output of the famous lm() function, which includes t-test for each coefficients, overal F-test for the model, and R^2 and adjusted R^2 for summary statistics. The package also include some other functions like transform_data and simple_OLS to be called if user want to check the internal operation in the linear_regression() function.

# Usage
linear_regression() takes input of a dataframe and a respond variable. The output will be a list which contains all elements in lm() function.
regression_predict() takes a object produced by linear_regression() and new data to predict.
transform_data() takes a data.frame and transform the categorical data. This function is called internally in linear_regression. User can check the result by calling this function.
simple_ols() takes a matrix and a respond variable y. To compute the linear regression.
