######################
## GradientLs ########
######################

GradientLs <- function(theta) {
  # Description :
  #               Compute the value of least-squares derivative function.
  # Usage : 
  #         GradientLs(theta)
  # Arguments : 
  #   theta : A vector of dimension p.
  # Returns : 
  #   A real value of the least-squares derivative function.
  
  if(Test.case=="gaussian") {
    derivative <- t(X) %*% (y - X %*% theta)
    result <- -2 * derivative 
    return(result)
  } else {
    loss <- sum(y * as.vector(X %*% theta) - bFunction(X, theta)) 
    tune.vector <- as.vector(t(y - MeanFunction(X, theta)) %*% X)
    result <- -tune.vector
    return(result)
  }
}
