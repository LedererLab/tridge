######################
## GradientRidge #####
######################

GradientRidge <- function(theta) {
  # Description :
  #               Compute the value of Ridge derivative function.
  # Usage : 
  #         GradientRidge(theta)
  # Arguments : 
  #   theta : A vector of dimension p.
  # Returns : 
  #   A real value of the Ridge derivative function.
  
  if(Test.case=="gaussian") {
    derivative <- t(X) %*% (y - X %*% theta)
    result <- -2 * derivative + r * 2 * qnorm(2, theta)
    return(result)
  } else {
    loss <- sum(y * as.vector(X %*% theta) - bFunction(X, theta)) 
    tune.vector <- as.vector(t(y - MeanFunction(X, theta)) %*% X)
    result <- -tune.vector  + r * 2 * qnorm(2, theta)
    return(result)
  }
}
