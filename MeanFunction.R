####################
## MeanFunction ####
####################

# Mean function for generalized linear models
MeanFunction <- function(X, theta, case){
  # Description :
  #               Compute the mean for generalized linear models.
  # Usage : 
  #         MeanFunction(X, theta, case)
  # Arguments : 
  #   X : A design matrix of dimension n * p.
  #   theta : A vector of dimension p.
  #   case : The type of generalized linear models. 
  #          The options are "gaussian", "poisson", and "binomial".
  # Returns : 
  #   A mean vector of dimension n 
  #   for the type of generalized linear model assigned by case argument. 
  
  if (case == "gaussain"){
    result <- as.vector(X %*% theta)
  } else if (case == "poisson"){
    result <- as.vector(exp(X %*% theta))
  } else {
    result <- as.vector(exp(X %*% theta)) / ( 1 + as.vector(exp(X %*% theta)))
  }
  return(result)
}



