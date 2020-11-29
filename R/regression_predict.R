#'regression_predict
#'
#'This function mimic the predict function. It takes input of a linear_regression object, designed in the same package.
#'And it wll use the coefficients in the object to predict by calculating yhat given the other input of a new data.
#'This function will check the input of your new dataframe object.
#'If one of the feature used in model training is not appear in the new data, then this function will not compute, and raise an error.
#'
#'@param regression_object this should be a result computed by linear_regression()
#'@param X new_data you want to predict
#'@return it will return a list with model coefficients, t statistics and pvalue of every coefficient, and F-stat for the overall model. It will also return R^2 and adjusted R^2, just like everything lm function will return.
#'@examples
#'library(MASS)
#'X=Boston[,-ncol(Boston)]
#'y=Boston[,ncol(Boston)]
#'reg_model = linear_regression(X,y)
#'yhat = regression_predict(reg_model, X)
#'@export

regression_predict = function(regression_object, X){
  if( class(X) != "data.frame"   ) return('your input data should be a data.frame, not a matrix or an array')
  predictor_names = regression_object$feature_names
  input_names = colnames(X)
  if(!all(predictor_names %in% input_names)) return('Your input does not match the regression model')
  X = X[predictor_names]
  X = transform_data(X)
  X = as.matrix(X)
  intercept = rep(1, dim(X)[1] )
  X_full = cbind( intercept , X )
  yhat = X_full%*% regression_object$coefficients[,1]
  return(yhat)
}
