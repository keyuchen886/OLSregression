
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


