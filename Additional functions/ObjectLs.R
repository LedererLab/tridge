####################
## ObjectLs #####
####################

ObjectLs <- function(theta) {
  # Description :
  #               Compute the value of least-squares object function.
  # Usage : 
  #         ObjectLs(theta)
  # Arguments : 
  #   theta : A vector of dimension p.
  # Returns : 
  #   A real value of the least-squares objective function. 
  
  if(Test.case=="gaussian") {
    loss <- as.vector(y - X %*% theta)
    derivative <- as.vector(t(X) %*% (y - X %*% theta))
    result <- (qnorm(2, loss) ^ 2- qnorm(2, y) ^ 2) 
    return(result)
  } else {
    loss <- sum(y * as.vector(X %*% theta) - bFunction(X, theta)) 
    tune.vector <- as.vector(t(y - MeanFunction(X, theta)) %*% X)
    regularization <- qnorm(2, theta) ^ 2
    result <- -loss 
    return(result)
  }
}
