#######################################################################################
#                                                                                     
#   Filename    :	invH.R    												  
#                                                                                     
#   Description :   inverting the piece-wise components of the
#                   cumulative hazard function (based on the work of Austin (2012))
#					These functions are used in the initiation step of mySimrecTDC function 
#    
#   Value : a numeric 
#
#   Ref: 
#   Austin, Peter C. "Generating survival times to simulate Cox proportional hazards
#        models with time‐varying covariates." 
#        Statistics in medicine 31.29 (2012): 3946-3958.
########################################################################################

invH1 <- function(t,x,beta.x,lambda,nu){
    (t / (lambda * exp(beta.x %*% x)))^(1/nu)
}

invH2 <- function(t,t1,x,beta.x,beta.x.TD,lambda,nu){
    num <- t - lambda * exp(beta.x %*% x) * t1^nu + lambda * exp(beta.x %*% x + beta.x.TD) * t1^nu
    denom <- lambda * exp(beta.x %*% x + beta.x.TD)
    (num / denom)^(1/nu)
}

invH3 <- function(t,t1,t2,x,beta.x,beta.x.TD,lambda,nu){
    num <- t - lambda * exp(beta.x %*% x) * t1^nu - lambda * exp(beta.x %*% x + beta.x.TD) * (t2^nu - t1^nu) + lambda * exp(beta.x %*% x) * t2^nu
    denom <- lambda * exp(beta.x %*% x)
    (num / denom)^(1/nu)
}

invH4 <- function(t,t1,t2,t3,x,beta.x,beta.x.TD,lambda,nu){
    num <- t - lambda * exp(beta.x %*% x) * t1^nu - lambda * exp(beta.x %*% x + beta.x.TD) * (t2^nu - t1^nu) - lambda * exp(beta.x %*% x) * (t3^nu - t2^nu) + lambda * exp(beta.x %*% x + beta.x.TD) * t3^nu
    denom <- lambda * exp(beta.x %*% x + beta.x.TD)
    (num / denom)^(1/nu)
}