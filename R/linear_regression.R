#'linear_regression
#'
#'This function will do almost exactly the same thing as the famous lm function.
#'To be specific, It will compute linear regression with the given data.
#'The data can have factor type and our function will automatically transform it by one-hot encoding.
#'It will also compute feature siginificance through t-test and an overall F-test just like the lm function.
#'
#'@param X input X, it should be a data.frame object with/without colnames.
#'@param y input y, this is the respond variable.
#'@return it will return a list with model coefficients, t statistics and pvalue of every coefficient, and F-stat for the overall model. It will also return R^2 and adjusted R^2, just like everything lm function will return.
#'@examples
#'library(MASS)
#'X=Boston[,-ncol(Boston)]
#'y=Boston[,ncol(Boston)]
#'linear_regression(X,y)
#'@export


linear_regression = function(X, y){
  #this function will try its best to mimic the behavior of our most famous 'lm' function
  if( class(X) != "data.frame"   ) return('your input data should be a data.frame, not a matrix or an array')
  if( dim(X)[1] != length(y) ) return('your input X and y does not has the same sample size')
  feature_names = colnames(X)
  X = transform_data(X)
  X = as.matrix(X)
  betas = simple_ols(X, y)
  se_object = compute_standard_error(X, y, betas)
  df_object = compute_degree_freedom(X)
  t_stat_object = compute_t_statistics(betas, se_object[['se']], df_object[['df_residual']])
  model_coefficient = data.frame(Estimate=betas , Std.Error = se_object[['se']],
                                 t_stat = t_stat_object[['t_stat']] ,
                                 p_value = t_stat_object[['p_value']])
  model_signi = model_significance(se_object, y, df_object)
  return( list(residual = quantile( se_object[['residual']] ),
               coefficients = model_coefficient,
               Residual_standard_error=sqrt( se_object[['mse']] ),
               Multiple_R_squared = model_signi[['Multiple_R_squared']],
               Adjusted_R_squared = model_signi[['Adjusted_R_squared']],
               F_stat = model_signi[['F_stat']],
               F_stat_p_value = model_signi[['F_stat_p_value']],
               feature_names = feature_names)
  )
}
