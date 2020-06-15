#############
## qnorm ####
#############

# Compute the L_q norm for vector
qnorm <- function(q, v){
  # Description :
  #               L_q norm for vector.
  # Usage : 
  #         qnorm(q, v)
  # Arguments : 
  #   q : A number greater than or equal to one.
  #   v : A vector or a number.
  # Returns : 
  #   A numerical value for the L_q norm of v.
  
  (sum(abs(v) ^ q)) ^ (1 / q)
}




