
generate_data = function(n, p){
  covar_data = rnorm(n * p)
  covariates = matrix(covar_data, nrow = n, ncol = p)
  responses = rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff){
  reg = lm(responses ~ covariates)
  pval = summary(reg)$coefficients[,4]
  retain = covariates[which(pval <= cutoff),]
  res_ret = response[which(pval <= cutoff)]
  regnew = lm(res_ret ~ retain)
  newpval = summary(regnew)$coefficients[,4]
  return(newpval)
}