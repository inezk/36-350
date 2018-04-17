
generate_data = function(n, p){
  covar_data = rnorm(n * p)
  covariates = matrix(covar_data, nrow = n, ncol = p)
  responses = rnorm(n)
}
