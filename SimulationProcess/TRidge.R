#################
## TRidge #######
#################

# Perform the T-ridge pipeline
init <- optim(par=rep(0, num.par), fn=ObjectLs, gr=GradientLs, 
              method="CG")
init.vector <- init$par
GlmTrex.estimators <- matrix(nrow=num.par, ncol=length(TREX.c.vector))
for(trex.e in 1:length(TREX.c.vector)) {
  TREX.c <- TREX.c.vector[trex.e]
  GlmTrex.estimation <- optim(par=init.vector, fn=ObjectiveFunction, gr=Gradient
                              , method="CG")
  GlmTrex.estimators[, trex.e] <- GlmTrex.estimation$par 
}

length.tuning <- 1000
edr.lambda <- qnorm(2, GradientLs(GlmTrex.estimators)) / 
  (2 * qnorm(2, GlmTrex.estimators))
r.max <- edr.lambda + 0.1
r.min <- max(0.05, (edr.lambda - 0.1))
if(r.min == Inf || r.max == Inf ) {
  tuning.parameters <- seq(1e+10, 1e+11, length.out=length.tuning)
} else {
  tuning.parameters <- seq(r.min, r.max, length.out=length.tuning)
}
init <- cv.glmnet(X, y, nfolds=10, alpha=0, family=Test.case)
init.estimation <- glmnet(X, y, alpha=0, family=Test.case, 
                          lambda=init$lambda.min)
init.vector <- as.vector(coef(init.estimation))[-1]
Ridge.estimators <- matrix(nrow=num.par, ncol=length.tuning)
cost <- rep(0, length.tuning)
for(tune in 1:length.tuning) {
  r <- tuning.parameters[tune]
  if(Test.case=="gaussian") {
    estimation <- glmnet(X, y, alpha=0, family=Test.case, lambda=r)
    Ridge.estimators[, tune] <- as.vector(coef(estimation)[-1])
  } else {
    estimation <- optim(par=init.vector, fn=ObjectRidge, gr=GradientRidge, 
                        method="CG")
    Ridge.estimators[, tune] <- estimation$par 
  }
  estimator.r <- as.vector(Ridge.estimators[, tune])
  cost[tune] <- ObjectiveFunction(estimator.r) 
}
estimator <- Ridge.estimators[, which.min(cost)]

