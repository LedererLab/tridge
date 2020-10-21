# If you run the riboflavin data, then set class to FALSE.
class <- FALSE

if (isTRUE(class)){
  library(readr)
  dataset <- read.csv("./RealData/mass-spec500peaks.csv", sep = ",")
  dim(dataset)
  data <- as.matrix(dataset)
  p <- 101 / 205
  X <- data[, -1]
  y <- c(rep(1, 101), rep(0, 104))
  y.class.all <- c(rep(1, 101), rep(0, 104))
  dim(X)
  length(y)
} else {
  library(readr)
  riboflavin <- read_csv("./RealData/riboflavin.csv")
  data <- t(riboflavin)
  X <- as.matrix(data)
  X <- X[-1, ]
  X <- X[, -1]
  X <- as.numeric(X)
  X <- matrix(X, nrow = 71)
  dim(X)
  y <- as.vector(data[, 1])
  y <- y[-1]
  y <- as.numeric(y)
  length(y)
}

num.obs <- dim(X)[1]
num.par <- dim(X)[2]

norm <- rep(0, num.par)
for(i in 1:num.par) {
  nv <- as.vector(X[, i])
  norm[i] <- as.numeric(sqrt(t(nv) %*% nv))
  X[, i] <- nv / norm[i]
}
#################################################
X.all <- X
y.all <- y
######################################################################################
if (isTRUE(class)){
  Test.case <- "binomial" 
} else {
  Test.case == "gaussian"
}

# TREX parameters
if(Test.case == "gaussian") {
  TREX.c.vector <- 2 
} else {
  TREX.c.vector <- 1 
}

##### Required package
#library(CVXR)
library(glmnet) # Generalized linear model
library(MASS) # Multivariate normal

##### Functions

# Compute the L_q norm
qnorm <- function(q, v){
  (sum(abs(v) ^ q)) ^ (1 / q)
}

# Mean function in generalized linear model
MeanFunction <- function (X, theta){
  if (Test.case == "gaussian"){
    result <- as.vector(X %*% theta)
  } else if (Test.case == "poisson"){
    result <- as.vector(exp(X %*% theta))
  } else {
    result <- as.vector(exp(X %*% theta)) / ( 1 + as.vector(exp(X %*% theta)))
  }
  return(result)
}

# b function in generalized linear model
bFunction <- function(X, theta){
  if (Test.case == "gaussian"){
    result <- 0.5 * as.vector(X %*% theta) ^ 2  
  } else if (Test.case == "poisson"){
    result <- as.vector(exp(X %*% theta))
  } else {
    result <- log(1 + as.vector(exp(X %*% theta)))
  }
  return(result)
}

# mean.prime function in generalized linear model
MeanPrime <- function (X, theta){
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

# Objective function
ObjectiveFunction <- function(theta) {
  if(Test.case == "gaussian") {
    loss <- as.vector(y - X %*% theta) 
    derivative <- as.vector(t(X) %*% (y - X %*% theta))
    result <- (qnorm(2, loss) ^ 2- qnorm(2, y) ^ 2) / (TREX.c * qnorm(2, derivative)) + qnorm(2, theta)
    return(result)
  } else {
    loss <- sum(y * as.vector(X %*% theta) - bFunction(X, theta)) 
    tune.vector <- as.vector(t(y - MeanFunction(X, theta)) %*% X)
    regularization <- qnorm(2, theta)
    result <- -loss /  (TREX.c * qnorm(2, tune.vector)) + regularization
    return(result)
  }
}

# Gradient
Gradient <- function(theta) {
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

ObjectEdr <- function(theta) {
  if(Test.case=="gaussian") {
    loss <- as.vector(y - X %*% theta)
    derivative <- as.vector(t(X) %*% (y - X %*% theta))
    result <- (qnorm(2, loss) ^ 2- qnorm(2, y) ^ 2) + u * qnorm(2, theta)
    return(result)
  } else {
    loss <- sum(y * as.vector(X %*% theta) - bFunction(X, theta)) 
    tune.vector <- as.vector(t(y - MeanFunction(X, theta)) %*% X)
    regularization <- qnorm(2, theta)
    result <- -loss + u * regularization
    return(result)
  }
}

GradientEdr <- function(theta) {
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

ObjectRidge <- function(theta) {
  if(Test.case=="gaussian") {
    loss <- as.vector(y - X %*% theta)
    derivative <- as.vector(t(X) %*% (y - X %*% theta))
    result <- (qnorm(2, loss) ^ 2- qnorm(2, y) ^ 2) + r * qnorm(2, theta) ^ 2
    return(result)
  } else {
    loss <- sum(y * as.vector(X %*% theta) - bFunction(X, theta)) 
    tune.vector <- as.vector(t(y - MeanFunction(X, theta)) %*% X)
    regularization <- qnorm(2, theta) ^ 2
    result <- -loss + r * regularization
    return(result)
  }
}

GradientRidge <- function(theta) {
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

ObjectLs <- function(theta) {
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

GradientLs <- function(theta) {
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
######################################################################################
errors.CV10 <- rep(0, length(y.all))
errors.CV5 <- rep(0, length(y.all))
errors.lasso.CV10 <- rep(0, length(y.all))
errors.lasso.CV5 <- rep(0, length(y.all))
errors.Edr.min <- rep(0, length(y.all))
######################################################################################
for (run in 1:length(y.all)){
  cat(sprintf("run %d out of %d runs\n", run, length(y.all)))
  X <- X.all[-run, ]
  y <- y.all[-run]
  num.obs <- dim(X)[1]
  num.par <- dim(X)[2]
  
  cat("  -> Perform the K-fold cross validation pipeline... ")
  cv10 <- cv.glmnet(X, y, nfolds=10, alpha=0, family=Test.case, intercept = FALSE)
  cv10.estimation <- glmnet(X, y, alpha=0, family=Test.case, lambda=cv10$lambda.min, intercept = FALSE)
  CV10.estimator <- as.vector(coef(cv10.estimation))[-1]
  
  cv5 <- cv.glmnet(X, y, nfolds=5, alpha=0, family=Test.case, intercept = FALSE)
  cv5.estimation <- glmnet(X, y, alpha=0, family=Test.case, lambda=cv5$lambda.min, intercept = FALSE)
  CV5.estimator <- as.vector(coef(cv5.estimation))[-1]
  
  cv10 <- cv.glmnet(X, y, nfolds=10, alpha=1, family=Test.case, intercept = FALSE)
  cv10.estimation <- glmnet(X, y, alpha=1, family=Test.case, lambda=cv10$lambda.min, intercept = FALSE)
  CV10.lasso.estimator <- as.vector(coef(cv10.estimation))[-1]
  
  cv5 <- cv.glmnet(X, y, nfolds=5, alpha=1, family=Test.case, intercept = FALSE)
  cv5.estimation <- glmnet(X, y, alpha=1, family=Test.case, lambda=cv5$lambda.min, intercept = FALSE)
  CV5.lasso.estimator <- as.vector(coef(cv5.estimation))[-1]
  ######################################################################################
  cat("done\n")
  cat("  -> compute t-ridge estimators... ")
  init <- optim(par=rep(0, num.par), fn=ObjectLs, gr=GradientLs, 
                method="CG")
  init.vector <- init$par
  
  init2 <- cv.glmnet(X, y, nfolds=10, alpha=0, family=Test.case, intercept = FALSE)
  init2.estimation <- glmnet(X, y, alpha=0, family=Test.case, lambda=init2$lambda.min, intercept = FALSE)
  init2.vector <- as.vector(coef(init2.estimation))[-1]
  
  GlmTrex.estimators <- matrix(nrow=num.par, ncol=length(TREX.c.vector))
  for(trex.e in 1:length(TREX.c.vector)) {
    TREX.c <- TREX.c.vector[trex.e]
    GlmTrex.estimation <- optim(par=init.vector, fn=ObjectiveFunction, gr=Gradient, method="CG")
    GlmTrex.estimators[, trex.e] <- GlmTrex.estimation$par 
  }
  
  length.tuning <- 2
  u <- TREX.c * qnorm(2, GradientLs(GlmTrex.estimators))
  Edr.estimator <- matrix(nrow=num.par, ncol=length.tuning)
  tuning.parameters <- seq(max(0.05, (u - 10)), (u + 10), length.out=length.tuning)
  cost <- rep(0, length.tuning)
  for(edr.tune in 1:length.tuning) {
    TREX.c <- TREX.c.vector
    u <- tuning.parameters[edr.tune]
    Edr.estimation <- optim(par=CV10.estimator, fn=ObjectEdr, gr=GradientEdr, method="CG")
    Edr.estimator[, edr.tune] <- Edr.estimation$par 
    cost[edr.tune] <- ObjectiveFunction(Edr.estimation$par) 
  }
  Edr.estimators <- Edr.estimator[, which.min(cost)]
  
  length.tuning <- 1000
  r.max <- tuning.parameters[2] / (2 * qnorm(2, Edr.estimator[, 2]))
  r.min <- tuning.parameters[1] / (2 * qnorm(2, Edr.estimator[, 1]))
  edr.lambda <- qnorm(2, GradientLs(GlmTrex.estimators)) / (2 * qnorm(2, GlmTrex.estimators))
  r.max <- edr.lambda + 0.1
  r.min <- max(0.05, (edr.lambda - 0.1))
  if(r.min == Inf || r.max == Inf ) {
    tuning.parameters <- seq(1e+10, 1e+11, length.out=length.tuning)
  } else {
    tuning.parameters <- seq(r.min, r.max, length.out=length.tuning)
  }
  Ridge.estimators <- matrix(nrow=num.par, ncol=length.tuning)
  cost <- rep(0, length.tuning)
  for(tune in 1:length.tuning) {
    r <- tuning.parameters[tune]
    estimation <- optim(par=init2.vector, fn=ObjectRidge, gr=GradientRidge, 
                        method="CG")
    Ridge.estimators[, tune] <- estimation$par 
    estimator.r <- as.vector(Ridge.estimators[, tune])
    cost[tune] <- ObjectiveFunction(estimator.r) 
  }
  estimator <- Ridge.estimators[, which.min(cost)]
  ######################################################################################
  cat("done\n")
  if (isTRUE(class)){
    class.cv10 <- 1 / (1 + exp(-as.numeric(X.all[run, ] %*% (CV10.estimator))))
    class.cv5 <- 1 / (1 + exp(-as.numeric(X.all[run, ] %*% (CV5.estimator))))
    classlasso.cv10 <- 1 / (1 + exp(-as.numeric(X.all[run, ] %*% (CV10.lasso.estimator))))
    classlasso.cv5 <- 1 / (1 + exp(-as.numeric(X.all[run, ] %*% (CV5.lasso.estimator))))
    class.tridge <- 1 / (1 + exp(-as.numeric(X.all[run, ] %*% (estimator))))
    if (isTRUE(class.cv10 >= 0.5)){
      class.cv10 <- 1
    } else {
      class.cv10 <- 0
    }
    if (isTRUE(class.cv5 >= 0.5)){
      class.cv5 <- 1
    } else {
      class.cv5 <- 0
    }
    if (isTRUE(classlasso.cv10 >= 0.5)){
      classlasso.cv10 <- 1
    } else {
      classlasso.cv10 <- 0
    }
    if (isTRUE(classlasso.cv5 >= 0.5)){
      classlasso.cv5 <- 1
    } else {
      classlasso.cv5 <- 0
    }
    if (isTRUE(class.tridge >= 0.5)){
      class.tridge <- 1
    } else {
      class.tridge <- 0
    }
    errors.CV10[run] <- class.cv10 - y.class.all[run]
    errors.CV5[run] <- class.cv5 - y.class.all[run]
    errors.lasso.CV10[run] <- classlasso.cv10 - y.class.all[run]
    errors.lasso.CV5[run] <- classlasso.cv5 - y.class.all[run]
    errors.Edr.min[run] <- class.tridge - y.class.all[run]
  } else {
    errors.CV10[run] <- qnorm(2, abs(X.all[run, ] %*% (CV10.estimator) - y.all[run])) / sqrt(num.obs) 
    errors.CV5[run] <- qnorm(2, abs(X.all[run, ] %*% (CV5.estimator) - y.all[run])) / sqrt(num.obs) 
    errors.lasso.CV10[run] <- qnorm(2, abs(X.all[run, ] %*% (CV10.lasso.estimator) - y.all[run])) / sqrt(num.obs) 
    errors.lasso.CV5[run] <- qnorm(2, abs(X.all[run, ] %*% (CV5.lasso.estimator) - y.all[run])) / sqrt(num.obs) 
    errors.Edr.min[run] <- qnorm(2, abs(X.all[run, ] %*% (estimator) - y.all[run])) / sqrt(num.obs) 
  }
}

if (isTRUE(class)){
  print(1-length(errors.CV10[errors.CV10 == 0]) / 205)
  print(1-length(errors.CV5[errors.CV5 == 0]) / 205)
  print(1-length(errors.lasso.CV10[errors.lasso.CV10 == 0]) / 205)
  print(1-length(errors.lasso.CV5[errors.lasso.CV5 == 0]) / 205)
  print(1-length(errors.Edr.min[errors.Edr.min == 0]) / 205)
} else {
  print(sum(errors.CV10^2))
  print(sum(errors.CV5^2))
  print(sum(errors.lasso.CV10^2))
  print(sum(errors.lasso.CV5^2))
  print(sum(errors.Edr.min^2))
}







