#########################
## DataGeneration #######
#########################

##### Generate the test setting

# design matrix
sigma <- matrix(nrow=num.par, ncol=num.par)
for(rr in 1:num.par) {
  for(cc in 1:num.par) {
    sigma[rr, cc] <- k ^ abs(rr - cc)
  }
}
mu <- rep(0, num.par)
X <- mvrnorm(n = num.obs, mu=mu, Sigma=sigma)

# Normalize each column of the design matrix
norm <- rep(0, num.par)
for(i in 1:num.par) {
  nv <- as.vector(X[, i])
  norm[i] <- as.numeric(sqrt(t(nv) %*% nv))
  X[, i] <- nv / norm[i]
}

# Singular value decomposition of X
if(num.obs < num.par){
  SVD <- svd(X)
  U <- SVD$u
  V <- SVD$v
} else {
  SVD <- svd(X, nu = num.obs, nv = num.par)
  U <- SVD$u
  V <- SVD$v
}

# regression vector
Projection <- V %*% t(V)
if(Test.case=="gaussian") {
  beta <- rnorm(num.par, mean = 0, sd = 1) 
  beta <- Projection %*% beta
} else if(Test.case=="poisson") {
  beta <- rnorm(num.par, mean = 0, sd = 1) 
  beta <- Projection %*% beta
} else {
  beta <- rnorm(num.par, mean = 0, sd = 1) 
  beta <- Projection %*% beta
}

#Signal-to.noise ratio
if(Test.case == "gaussian") {
  stn <- SNR 
} else {
  stn <- NA
}

# outcome
y <- rep(NA, num.obs)
for(i in 1:num.obs) {
  if(Test.case == "gaussian") {
    y[i] <- rnorm(1, mean=as.vector(X %*% beta)[i], sd=sqrt(stn ^ -1 * var(X %*% beta)))
  } else if(Test.case == "poisson") {
    y[i] <- rpois(1, lambda=as.vector(exp(X %*% beta))[i])
  } else {
    y[i] <- rbinom(1, size=class.num, prob=as.vector(exp(X %*% beta) / (1 + exp(X %*% beta)))[i]) 
  }
}

