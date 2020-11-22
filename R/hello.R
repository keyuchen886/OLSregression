# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

simple_ols = function(X ,y){
  #this function will take input of matrix x and y
  #to inplement a simpliest case of ordinary linear regression
  #this function should not be directly called
  intercept = rep(1, length(y) )
  X = cbind(intercept, X)  #this is the full X matrix
  betas = solve(t(X) %*% X) %*% t(X) %*% y
  return(betas)
}

compute_standard_error = function(X, y, betas){
  #this function will take input of matrix x and respond y
  #it will compute the standard error along with the residual.
  intercept = rep(1, length(y) )
  X_full = cbind( intercept , X )
  residual = (y- X_full%*%betas)
  esti_mse = sum(residual^2)/ (nrow(X_full) - ncol(X_full) )  #this is the mean squared error, which is used to compute variance covariance matrix
  var_cov = esti_mse * solve(t(X_full) %*% X_full) #this is the variance-covariance matrix of our X.
  coeff_se = sqrt(diag(var_cov))   #this is the standard error of our coefficient
  return(list(se=coeff_se , residual = residual, mse=esti_mse))
}

compute_degree_freedom = function(X){
  #this function works to compute the degree freedom of residual, model
  return(list(df_residual=nrow(X)-ncol(X)-1 , df_model = ncol(X)))
}

compute_t_statistics = function(betas , standard_error, df_residual){
  t_stat = betas/standard_error
  p_value = 2*pt(abs(t_stat), df_residual, lower.tail=F)
  return(list(t_stat=t_stat, p_value=p_value))
}

model_significance = function(se_object, y, df_object){
  #this function will be used to calculate R^2, adjusted R^2, and conduct a F-test.
  rsqr = 1 - sum(se_object[['residual']]^2) / sum( (y-mean(y))^2 )
  adj_rsqr = 1 - ( (1-rsqr)*( length(y)-1 ) / (df_object[['df_residual']])  )
  F_stat = sum( (-se_object[['residual']]+y-mean(y)  )^2 )/sum(se_object[['residual']]^2)/df_object[['df_model']]*df_object[['df_residual']]
  F_stat_p_value = pf(F_stat, df_object[['df_model']] ,df_object[['df_residual']], lower.tail=F)
  return(list(F_stat = F_stat, Multiple_R_squared = rsqr, Adjusted_R_squared=adj_rsqr, F_stat_p_value = F_stat_p_value))
}

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
