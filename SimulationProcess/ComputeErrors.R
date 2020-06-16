########################
## ComputeErrors #######
########################

##### Compute the prediction errors 

# Prediction error
for(trex.err in 1:length(TREX.c.vector)) {
  errors.TREX[run, trex.err] <- qnorm(2, abs(X %*% (estimator - beta))) / sqrt(num.obs)
}
errors.CV10[run] <- qnorm(2, abs(X %*% (CV.estimator - beta))) / sqrt(num.obs) 

#Relative Prediction error
relative.errors.TREX[run, ] <- errors.TREX[run, ] / qnorm(2, (X %*% beta)) * sqrt(num.obs)
relative.errors.CV10[run] <- errors.CV10[run] / qnorm(2, (X %*% beta)) * sqrt(num.obs)

# beta error
for(trex.b.err in 1:length(TREX.c.vector)) {
  beta.error.TREX[run, trex.b.err] <- qnorm(2, (estimator - beta))
}
beta.error.CV10[run] <- qnorm(2, (CV.estimator - beta))

#relative beta error
relative.beta.error.TREX[run, ] <- beta.error.TREX[run, ] / qnorm(2, beta)
relative.beta.error.CV10[run] <- beta.error.CV10[run] / qnorm(2, beta)

