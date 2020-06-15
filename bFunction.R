####################
## bFunction #######
####################

# b function for generalized linear models.
bFunction <- function(X, theta, case){
  # Description :
  #               Compute the parametric vector for generalized linear models.
  # Usage : 
  #         bFunction(X, theta, case)
  # Arguments : 
  #   X : A design matrix of dimension n * p.
  #   theta : A vector of dimension p.
  #   case : The type of generalized linear models. 
  #          The options are "gaussian", "poisson", and "binomial".
  # Returns : 
  #   A vector of dimension n 
  #   for the type of generalized linear model assigned by case argument. 
  
  
  if (case == "gaussian"){
    result <- 0.5 * as.vector(X %*% theta) ^ 2  
  } else if (case == "poisson"){
    result <- as.vector(exp(X %*% theta))
  } else {
    result <- log(1 + as.vector(exp(X %*% theta)))
  }
  return(result)
}



