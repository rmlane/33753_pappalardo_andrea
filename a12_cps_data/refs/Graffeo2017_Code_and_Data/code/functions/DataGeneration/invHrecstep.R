#######################################################################################
#                                                                                     
#   Filename    :	invHrecstep.R    												  
#                                                                                     
#   Description :   inverting the piece-wise components of the
#                   cumulative hazard function (based on the work of Austin (2012))
#					These functions are used in the recursive step of mySimrecTDC function 
#    
#   Value : a numeric 
#
#   Ref: 
#   Austin, Peter C. "Generating survival times to simulate Cox proportional hazards
#        models with time‐varying covariates." 
#        Statistics in medicine 31.29 (2012): 3946-3958.
########################################################################################

invHt0.t <- function(v,t,xx,beta.x,beta.e,lambda,nu){
    (v / (lambda * exp(beta.x %*% xx + beta.e)) + t^nu)^(1/nu)
}

invHt1.t <- function(v,t,t.switch1,xx,beta.x,beta.e,beta.x.TD,lambda,nu){
    ((v/(lambda * exp(beta.x %*% xx + beta.e)) -(t.switch1^nu-t^nu))*exp(-beta.x.TD)+t.switch1^nu)^(1/nu) 
}

invHt2.t <- function(v,t,xx,beta.x,beta.e,beta.x.TD,beta.x.TD2,lambda,nu){
    (v/(lambda * exp(beta.x %*% xx + beta.e + beta.x.TD + beta.x.TD2)) + t^nu)^(1/nu) 
}

invHt3.t <- function(v,t,t.switch1,t.switch2,xx,beta.x,beta.e,beta.x.TD,lambda,nu){
    (v/(lambda * exp(beta.x %*% xx + beta.e)) -(t.switch1^nu-t^nu) - exp(beta.x.TD) %*% (t.switch2^nu-t.switch1^nu) +t.switch2^nu)^(1/nu) 
}

invHt4.t <- function(v,t,t.switch2,xx,beta.x,beta.e,beta.x.TD,beta.x.TD2,lambda,nu){
    (v/(lambda * exp(beta.x %*% xx + beta.x.TD2 + beta.e)) -(t.switch2^nu-t^nu)*exp(beta.x.TD)+t.switch2^nu)^(1/nu) 
}

invHt5.t <- function(v,t,t.switch1,t.switch2,t.switch3,xx,beta.x,beta.e,beta.x.TD,lambda,nu){
    ((v/(lambda * exp(beta.x %*% xx + beta.e)) -(t.switch1^nu-t^nu) - exp(beta.x.TD) %*% (t.switch2^nu-t.switch1^nu) -(t.switch3^nu-t.switch2^nu))*exp(-beta.x.TD)+t.switch3^nu)^(1/nu) 
}

invHt6.t <- function(v,t,t.switch2,t.switch3,xx,beta.x,beta.e,beta.x.TD,beta.x.TD2,lambda,nu){
    ((v/(lambda * exp(beta.x %*% xx+beta.e+beta.x.TD2)) - exp(beta.x.TD) %*% (t.switch2^nu-t^nu) - (t.switch3^nu-t.switch2^nu))*exp(-beta.x.TD)+ t.switch3^nu)^(1/nu) 
}