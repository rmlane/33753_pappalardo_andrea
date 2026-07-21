#######################################################################################
#                                                                                     
#   Filename    :	CumHazRange.R    												  
#                                                                                     
#   Description :   computing the range of cumulative hazard functions
#                   over each interval (based on the work of Austin (2012))
#					These functions are used in the initiation step of mySimrecTDC function 
#    
#   Value : a list containing a vector of times 
#
#   Ref: 
#   Austin, Peter C. "Generating survival times to simulate Cox proportional hazards
#        models with time‐varying covariates." 
#        Statistics in medicine 31.29 (2012): 3946-3958.
######################################################################################## 

R1fct <- function(list, indic, x, beta.x, lambda, nu){
    # renvoie une liste de R1(t1)
    yy <- lambda * exp(as.matrix(x[indic,]) %*% beta.x) * unlist(list)^nu
}

R2fct <- function(list, indic, x, beta.x, beta.x.TD, lambda, nu){
    # renvoie une liste de vecteurss R1(t1)) R2(t2)
    list1 <- lapply(list, "[[", 1)
    list2 <- lapply(list, "[[", 2)
    yy <- lambda * exp(as.matrix(x[indic,]) %*% beta.x) * unlist(list1)^nu
    zz <- lambda * exp(as.matrix(x[indic,]) %*% beta.x) * (unlist(list1)^nu + exp(beta.x.TD)*(unlist(list2)^nu - unlist(list1)^nu))
    mat <- rbind(t(yy),t(zz))
    split(mat, col(mat))
}

R3fct <- function(list, indic, x, beta.x, beta.x.TD, lambda, nu){
    # renvoie une liste de vecteurss R1(t1)) R2(t2) R3(t3)
    list1 <- lapply(list, "[[", 1)
    list2 <- lapply(list, "[[", 2)
    list3 <- lapply(list, "[[", 3)
    yy <- lambda * exp(as.matrix(x[indic,]) %*% beta.x) * unlist(list1)^nu
    zz <- lambda * exp(as.matrix(x[indic,]) %*% beta.x) * (unlist(list1)^nu + exp(beta.x.TD)*(unlist(list2)^nu - unlist(list1)^nu))
    tt <- lambda * exp(as.matrix(x[indic,]) %*% beta.x) * (unlist(list1)^nu + exp(beta.x.TD)*(unlist(list2)^nu - unlist(list1)^nu) + unlist(list3)^nu - unlist(list2)^nu)
    mat <- rbind(t(yy),t(zz),t(tt))
    split(mat, col(mat))
}