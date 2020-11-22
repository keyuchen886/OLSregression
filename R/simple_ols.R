# 'simple_ols'
#'
#'Compute the OLS linear regression for a given X and y. This is the most simple case of ordinary least square. In fact, this function will not perform any error checking because it's used as a internal function of least_square.
#'
#'@param X input X, it should be a matrix.
#'@param y input y, this is the respond variable.
#'@return it will return coefficient of beta, but it won't compute anything other than beta, such as R^2.
#'@examples
#'simple_ols(matrix(c(1,3,7,4,6,3),3,2), c(3,1,2))
#'
#'@export
#'


simple_ols = function(X ,y){
  intercept = rep(1, length(y) )
  X = cbind(intercept, X)  #this is the full X matrix
  betas = solve(t(X) %*% X) %*% t(X) %*% y
  return(betas)
}


