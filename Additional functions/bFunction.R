####################
## bFunction #######
####################

bFunction <- function(X, theta, case){
  # Description :
  #               Compute the parametric vector for generalized linear models.
  # Usage : 
  #         bFunction(X, theta)
  # Arguments : 
  #   X : A design matrix of dimension n * p.
  #   theta : A vector of dimension p.
  # Returns : 
  #   A vector of dimension n for generalized linear models. 
  
  if (Test.case == "gaussian"){
    result <- 0.5 * as.vector(X %*% theta) ^ 2  
  } else if (Test.case == "poisson"){
    result <- as.vector(exp(X %*% theta))
  } else {
    result <- log(1 + as.vector(exp(X %*% theta)))
  }
  return(result)
}



