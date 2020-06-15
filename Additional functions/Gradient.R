#################
## Gradient #####
#################

Gradient <- function(theta) {
  # Description :
  #               Compute the value of T-ridge derivative function.
  # Usage : 
  #         Gradient(X, theta)
  # Arguments : 
  #   theta : A vector of dimension p.
  # Returns : 
  #   A real value of the T-ridge derivative function.
  
  
  if(Test.case == "gaussian") {
    derivative <- t(X) %*% (y - X %*% theta)
    loss <- (y - X %*% theta) 
    result <- (qnorm(2, loss) ^ 2- qnorm(2, y) ^ 2) * t(X) %*% X %*% derivative / 
      (TREX.c * qnorm(2, derivative) ^ 3) - 
      2 * derivative / (TREX.c * qnorm(2, derivative)) +
      theta / qnorm(2, theta)
    return(result)
  } else {
    loss <- sum(y * as.vector(X %*% theta) - bFunction(X, theta)) 
    tune.vector <- as.vector(t(y - MeanFunction(X, theta)) %*% X)
    result <- -tune.vector / (TREX.c * qnorm(2, tune.vector)) - 
      as.vector(0.5 * TREX.c * 2 * loss * t(tune.vector) %*% t(X) %*% (X * MeanPrime(X, theta)) / (TREX.c ^ 2 * qnorm(2, tune.vector) ^ 3)) + 
      theta / qnorm(2, theta)
    return(result)
  }
}

