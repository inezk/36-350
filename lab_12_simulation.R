
generate_data = function(n, p){
  covar_data = rnorm(n * p)
  covariates = matrix(covar_data, nrow = n, ncol = p)
  responses = rnorm(n)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff){
  reg = lm(responses ~ covariates)
  pval = summary(reg)$coefficients[,4]
  retain = covariates[which(pval <= cutoff)]
  res_ret = responses[which(pval <= cutoff)]
  if(length(retain) >  0){
    regnew = lm(res_ret ~ retain)
    newpval = summary(regnew)$coefficients[,4]
  }
  else{
    return(vector())
  }
  if(all(is.na(x))) {
    return(vector())}
  return(newpval)
}

run_simulation = function(n_trials, n, p, cutoff){
  pval_list = c()
  for(i in 1:n_trials){
    results = generate_data(n, p)
    pval = model_select(results$covariates, results$responses, cutoff)
    pval_list = c(pval_list, pval)
  }
  hist(pval_list)
}

run_simulation(10, n = 100, p = 10, 0.05)