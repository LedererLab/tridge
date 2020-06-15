####################
## MeanPrime #######
####################

MeanPrime <- function (X, theta){
  # Description :
  #               Compute the derivative of mean function for generalized linear models.
  # Usage : 
  #         MeanPrime(X, theta, case)
  # Arguments : 
  #   X : A design matrix of dimension n * p.
  #   theta : A vector of dimension p.
  # Returns : 
  #   A vector of dimension n for generalized linear models. 
  
  if (Test.case == "gaussian"){
    result <- rep(1, dim(X)[2])
  } else if (Test.case == "poisson"){
    result <- as.vector(exp(X %*% theta))
  } else {
    result <- as.vector(exp(X %*% theta)) / (1 + 
                                               as.vector(exp(X %*% theta))) ^ 2
  }
  return(result)
}

