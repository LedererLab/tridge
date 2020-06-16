####################
## GradientEdr #####
####################

GradientEdr <- function(theta) {
  # Description :
  #               Compute the value of Edr derivative function.
  # Usage : 
  #         GradientEdr(theta)
  # Arguments : 
  #   theta : A vector of dimension p.
  # Returns : 
  #   A real value of the Edr derivative function.
  
  if(Test.case=="gaussian") {
    derivative <- t(X) %*% (y - X %*% theta)
    result <- -2 * derivative + u * theta / qnorm(2, theta)
    return(result)
  } else {
    loss <- sum(y * as.vector(X %*% theta) - bFunction(X, theta)) 
    tune.vector <- as.vector(t(y - MeanFunction(X, theta)) %*% X)
    result <- -tune.vector  + u * theta / qnorm(2, theta)
    return(result)
  }
}

