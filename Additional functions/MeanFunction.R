####################
## MeanFunction ####
####################

MeanFunction <- function(X, theta){
  # Description :
  #               Compute the mean vector for generalized linear models.
  # Usage : 
  #         MeanFunction(X, theta, case)
  # Arguments : 
  #   X : A design matrix of dimension n * p.
  #   theta : A vector of dimension p.
  # Returns : 
  #   A mean vector of dimension n for generalized linear models. 
  
  if (Test.case == "gaussain"){
    result <- as.vector(X %*% theta)
  } else if (Test.case == "poisson"){
    result <- as.vector(exp(X %*% theta))
  } else {
    result <- as.vector(exp(X %*% theta)) / ( 1 + as.vector(exp(X %*% theta)))
  }
  return(result)
}



