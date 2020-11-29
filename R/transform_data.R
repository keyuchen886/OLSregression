#'transform_data
#'
#'In linear regression, or in most of machine learning models, it's import to encode your categorical data.
#'In this transform_data function, we encode categorical variable by creating n-1 new variables as indicator, and the last one is the base level.
#'For example, if a feature sex has only two type: male or female, then we will generate Sexmale feature, which is 1 if the sex is male, 0 otherwise.
#'In fact, this function is used internally in linear_regression() function to change dataframe.
#'
#'@param X input X, it should be a data.frame object with colnames.
#'@return it will return a new dataset which is desirable for linear_regression function.
#'@examples
#'library(carData)
#'transform_data(Salaries)
#'@export

transform_data = function(X){
  #first check if there's factor type, and then transform data.
  factor_features = c()
  for(i in colnames(X)){
    if(class(X[[i]]) == "factor"){
      factor_features = c(factor_features, i)
    }
  }
  if (length(factor_features)==0){
    return(X)
  }else{
    formula = paste( c( '~0',paste(factor_features,collapse ="+" ) ),collapse="+" )
    formula = as.formula(formula)
    one_hot = model.matrix(formula,data=X)
    return(cbind( X[, !colnames(X) %in% factor_features] , one_hot[,-1] )
    )
  }
}
