########################
## ResultsOutput #######
########################

##### Output results
output.Data <- matrix(nrow = (length(TREX.c.vector) + 1), ncol = 4)
for(case in 1:length(TREX.c.vector)){
  output.Data[case, ] <- c(mean(errors.TREX[, case]), mean(beta.error.TREX[, case]), mean(relative.errors.TREX[, case]), mean(relative.beta.error.TREX[, case]))
}
output.Data[(case + 1), ] <- c(mean(errors.CV10), mean(beta.error.CV10), mean(relative.errors.CV10), mean(relative.beta.error.CV10))
names <- rep("", length(TREX.c.vector))
for(name in 1:length(TREX.c.vector)) {
  names[name] <- paste0("T-ridge", TREX.c.vector[name])
}
if(num.obs < num.par) {
  rownames(output.Data) <- c(names, "10-fold CV")
} else {
  rownames(output.Data) <- c(names, "MLE")
}

colnames(output.Data)<- c("Xbeta", "Beta", "r.Xbeta", "r.Beta")

output <- 
  matrix(c(round(output.Data[1, 3], 2),
           round(output.Data[2, 3], 2),
           round(output.Data[1, 4], 2),
           round(output.Data[2, 4], 2)), 
         ncol=4, byrow = TRUE)

if(num.obs < num.par) {
  H <- c("T-ridge", "10-fold CV",
         "T-ridge", "10-fold CV")
} else {
  H <- c("T-ridge(sd)", "MLE(sd)",
         "T-ridge(sd)", "MLE(sd)")
}


