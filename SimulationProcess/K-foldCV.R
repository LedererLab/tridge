###################
## K-foldCV #######
###################

# Perform the K-fold cross validation pipeline
if(num.obs < num.par) {
  cv <- cv.glmnet(X, y, nfolds=10, alpha=0, family=Test.case)
  cv.estimation <- glmnet(X, y, alpha=0, family=Test.case, lambda=cv$lambda.min)
  CV.estimator <- as.vector(coef(cv.estimation))[-1]
} else {
  cv.estimation <- glmnet(X, y, alpha=0, family=Test.case, lambda=0)
  CV.estimator <- as.vector(coef(cv.estimation))[-1]
}
