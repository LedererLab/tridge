#####################
## Initialize #######
#####################

# TREX parameters
if(Test.case=="gaussian") {
  TREX.c.vector <- 2 
} else {
  TREX.c.vector <- 1
}

class.num <- 1

# Initialize absolute error
errors.TREX <- matrix(nrow=num.runs, ncol=length(TREX.c.vector))
relative.errors.TREX <- matrix(nrow=num.runs, ncol=length(TREX.c.vector))
errors.CV10 <- rep(NA, num.runs)
relative.errors.CV10 <- rep(NA, num.runs)


beta.error.TREX <- matrix(nrow=num.runs, ncol=length(TREX.c.vector))
relative.beta.error.TREX <- matrix(nrow=num.runs, ncol=length(TREX.c.vector))
beta.error.CV10 <- rep(NA, num.runs)
relative.beta.error.CV10 <- rep(NA, num.runs)

ratio <- rep(NA, num.runs)



