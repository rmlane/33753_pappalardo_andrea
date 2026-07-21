mySimrecTDC <- function ( N,  
                          beta.x = 0, # log HR of x (fixed covariate)
                          beta.e = 0, # log HR of previous outcome (exposure)
                          beta.x.TD = 0,  # log HR of xt (time-dpt covariate)
                          beta.x.TD2 = 0, # log HR of previous xt
                          par.rec, # parameters of the Weibull distribution chosen as baseline hazard
                          dfree = 0 # length of the interval free of event (always exposed)
                         ){
#######################################################################################
#
#                                                                                     
#   Filename    :	mySimrecTDC.R    												  
#                                                                                     
#   Description :   modified simrec function (from the simrec package, Jahn-Eimermacher et al. (2015))     
#					using the work of Austin (2012)
#                   generates recurrent survival times of exposure initiation
#                   according to an Andersen-Gill model with a time-free period
#                   (the patient is exposed during these intervals)
#                   depending on a fixed covariate x, 
#                             on a time-dependent covariate xt at times t-1 and t
#                             on the previous value of the exposure
#                   the baseline hazard distribution is Weibull
#
#   Usage       :   mySimrecTDC(N, beta.x, beta.e, beta.x.TD, beta.x.TD2,
#                               par.rec, dfree)
#             
#    
#   Value : a list containing a data.frame (ipwtab) and a list (switch.times) 
#
#   Ref: 
#   Austin, Peter C. "Generating survival times to simulate Cox proportional hazards
#        models with time‐varying covariates." 
#        Statistics in medicine 31.29 (2012): 3946-3958.
#   Jahn-Eimermacher, Antje, et al. "Simulating recurrent event data with hazard
#        functions defined on a total time scale." 
#        BMC medical research methodology 15.1 (2015): 16.
########################################################################################    
    # id of the N patients
    ID <- c(1:N)
    
    # generating the follow-up
    fu <- rep(1,N)
    
    # generating the covariate-matrix x
    nr.cov <- 1    # number of fixed covariates = 1
    x <- matrix(0, N, 1)   # matrix with N lines and one column for each covariate
    # binomial distribution with parameter 0.5
    x[, 1] <- c(rbinom(N, 1, 0.5))

    # generating a list containing the switch times for the time-varying covariate xt (treatment)
    # at t = 0 we assume that all subjects are untreated 
    # at t = t_1 change from unexposed to exposed
    # maximal number of switches = 3                    +++
    switch.times <- lapply(1:N, function(i){
        nb.switch <- rbinom(1,3,0.5)
        if(nb.switch !=0){
            return(sort(runif(n = nb.switch, 0, fu[i])))
        }else{
            return(NA)
        }
    }) 
    # Number of switches per patient
    nb.switch <- ifelse(is.na(switch.times),0, sapply(switch.times,length))
    
    
    # derivation of the distributional parameters for the recurrent event data
    lambda <- par.rec[1]
    nu <- par.rec[2]
    
    # Initial step ####
    # Range of cumulative hazard fct over each interval +++
    RangeCumHaz <- vector(mode = "list", length = N)
    
    RangeCumHaz[is.na(switch.times)] <- NA             
    
    ind1 <- which(!is.na(switch.times) & sapply(switch.times, length)==1)
    RangeCumHaz[!is.na(switch.times) & sapply(switch.times, length)==1] <-
        R1fct(switch.times[ind1], ind1, x, beta.x, lambda, nu)
    
    ind2 <- which(sapply(switch.times, length)==2)
    RangeCumHaz[sapply(switch.times, length)==2] <- 
        R2fct(switch.times[ind2], ind2, x, beta.x, beta.x.TD, lambda, nu)
    
    ind3 <- which(sapply(switch.times, length)==3)
    RangeCumHaz[sapply(switch.times, length)==3] <- 
        R3fct(switch.times[ind3], ind3, x, beta.x, beta.x.TD, lambda, nu)
    
    # Simulation of N first event times
    U <- runif(N)
    pos.tswitch <- rep(0, N) # where is -log(U) compared with the boundaries of RangeCumHaz
    indna <- which(!is.na(RangeCumHaz))
    pos.tswitch[-indna] <- NA
    pos.tswitch[indna] <-sapply(indna, function(i){
        tempodiff <- -log(U)[i] - RangeCumHaz[[i]]
        return(sum(tempodiff > 0) + 1)
    })
    tt <- rep(0, N)
    tt[is.na(pos.tswitch) | pos.tswitch==1] <- invH1(-log(U)[is.na(pos.tswitch) | pos.tswitch==1],x[is.na(pos.tswitch) | pos.tswitch==1],beta.x,lambda,nu)
    tt[!is.na(pos.tswitch) & pos.tswitch==2] <- invH2(-log(U)[!is.na(pos.tswitch) & pos.tswitch==2], unlist(lapply(switch.times[!is.na(pos.tswitch) & pos.tswitch==2],"[[",1)),x[!is.na(pos.tswitch) & pos.tswitch==2],beta.x,beta.x.TD,lambda,nu) 
    tt[!is.na(pos.tswitch) & pos.tswitch==3] <- invH3(-log(U)[!is.na(pos.tswitch) & pos.tswitch==3], unlist(lapply(switch.times[!is.na(pos.tswitch) & pos.tswitch==3],"[[",1)), unlist(lapply(switch.times[!is.na(pos.tswitch) & pos.tswitch==3],"[[",2)),x[!is.na(pos.tswitch) & pos.tswitch==3],beta.x,beta.x.TD,lambda,nu) 
    tt[!is.na(pos.tswitch) & pos.tswitch==4] <- invH4(-log(U)[!is.na(pos.tswitch) & pos.tswitch==4], unlist(lapply(switch.times[!is.na(pos.tswitch) & pos.tswitch==4],"[[",1)), unlist(lapply(switch.times[!is.na(pos.tswitch) & pos.tswitch==4],"[[",2)), unlist(lapply(switch.times[!is.na(pos.tswitch) & pos.tswitch==4],"[[",3)),x[!is.na(pos.tswitch) & pos.tswitch==4],beta.x,beta.x.TD,lambda,nu)
    
    
    T <- matrix(tt, N, 1)
    dirty <- rep(TRUE, N)
    T1 <- NULL
    
    # Recursive step: simulation of N subsequent event times ####
    # loop only if event at previous time (otherwise it would be cancelled via dirty=F)
    while (any(dirty)) {
        U <- runif(N)
        t1 <- tt + dfree # no event ( = not exposed) during a period of length dfree
        # from t1 to t1+u
        # t <- t1 + \Lambda^{-1}(u) with \Lambda(u) = \Lambda(u+t1) + \Lambda(t1)
        # nb.switch = 0 ####
        tt[nb.switch==0] <- invHt0.t(-log(U)[nb.switch==0],t1[nb.switch==0],x[nb.switch==0],beta.x,beta.e,lambda,nu)
        # nb.switch = 1 ####
        cond1 <- t1[nb.switch==1] < switch.times[nb.switch==1]
        v.1   <- -log(U)[nb.switch==1]
        x.1   <- x[nb.switch==1]
        t1.1  <- t1[nb.switch==1]
        swt.1 <- switch.times[nb.switch==1]
        tt[nb.switch==1] <- ifelse(cond1, 
                                   ifelse(v.1<(lambda*exp(beta.x %*% x.1+beta.e)*(unlist(swt.1)^nu-t1.1^nu)),
                                          invHt0.t(v.1,t1.1,x.1,beta.x,beta.e,lambda,nu),
                                          invHt1.t(v.1,t1.1,(unlist(swt.1)),x.1,beta.x,beta.e,beta.x.TD,lambda,nu)),
                                   invHt2.t(v.1,t1.1,x.1,beta.x,beta.e,beta.x.TD,beta.x.TD2,lambda,nu))
        # nb.switch = 2 ####
        cond2.1 <- t1[nb.switch==2] < lapply(switch.times[nb.switch==2],"[[",1)
        cond2.2 <- (!cond2.1) & (t1[nb.switch==2] < lapply(switch.times[nb.switch==2],"[[",2))
        v.2   <- -log(U)[nb.switch==2]
        x.2   <- x[nb.switch==2]
        t1.2  <- t1[nb.switch==2]
        swt.2 <- switch.times[nb.switch==2]
        tt[nb.switch==2] <- ifelse(cond2.1,
                                   ifelse(v.2<(lambda*exp(beta.x %*% x.2+beta.e)*(unlist(lapply(swt.2,"[[",1))^nu-t1.2^nu)),
                                          invHt0.t(v.2,t1.2,x.2,beta.x,beta.e,lambda,nu),
                                          ifelse(v.2<lambda*exp(beta.x %*% x.2+beta.e)*(unlist(lapply(swt.2,"[[",1))^nu-t1.2^nu + exp(beta.x.TD)*(unlist(lapply(swt.2,"[[",2))^nu-unlist(lapply(swt.2,"[[",1))^nu)),
                                                 invHt1.t(v.2,t1.2,unlist(lapply(swt.2,"[[",1)),x.2,beta.x,beta.e,beta.x.TD,lambda,nu),
                                                 invHt3.t(v.2,t1.2,unlist(lapply(swt.2,"[[",1)),unlist(lapply(swt.2,"[[",2)),x.2,beta.x,beta.e,beta.x.TD,lambda,nu))),
                                   ifelse(cond2.2,
                                          ifelse(v.2<lambda*exp(beta.x %*% x.2 +beta.e+beta.x.TD+beta.x.TD2)*(unlist(lapply(swt.2,"[[",2))^nu-t1.2^nu),
                                                 invHt2.t(v.2,t1.2,x.2,beta.x,beta.e,beta.x.TD,beta.x.TD2,lambda,nu),
                                                 invHt4.t(v.2,t1.2,unlist(lapply(swt.2,"[[",2)),x.2,beta.x,beta.e,beta.x.TD,beta.x.TD2,lambda,nu)), 
                                          invHt0.t(v.2,t1.2,x.2,beta.x,beta.e,lambda,nu)))
        # nb.switch = 3 ####
        cond3.1 <- t1[nb.switch==3] < lapply(switch.times[nb.switch==3],"[[",1)
        cond3.2 <- (!cond3.1) & (t1[nb.switch==3] < lapply(switch.times[nb.switch==3],"[[",2))
        cond3.3 <- (!cond3.1) & (!cond3.2) & (t1[nb.switch==3] < lapply(switch.times[nb.switch==3],"[[",3))
        v.3   <- -log(U)[nb.switch==3]
        x.3   <- x[nb.switch==3]
        t1.3  <- t1[nb.switch==3]
        swt.3 <- switch.times[nb.switch==3]
        tt[nb.switch==3] <- ifelse(cond3.1,
                                   ifelse(v.3<(lambda*exp(beta.x %*% x.3+beta.e)*(unlist(lapply(swt.3,"[[",1))^nu-t1.3^nu)),
                                          invHt0.t(v.3,t1.3,x.3,beta.x,beta.e,lambda,nu),
                                          ifelse(v.3<lambda*exp(beta.x %*% x.3+beta.e)*(unlist(lapply(swt.3,"[[",1))^nu-t1.3^nu + exp(beta.x.TD)*(unlist(lapply(swt.3,"[[",2))^nu-unlist(lapply(swt.3,"[[",1))^nu)),
                                                 invHt1.t(v.3,t1.3,unlist(lapply(swt.3,"[[",1)),x.3,beta.x,beta.e,beta.x.TD,lambda,nu),
                                                 ifelse(v.3<lambda*exp(beta.x %*% x.3+beta.e)*(unlist(lapply(swt.3,"[[",1))^nu-t1.3^nu + exp(beta.x.TD)*(unlist(lapply(swt.3,"[[",2))^nu-unlist(lapply(swt.3,"[[",1))^nu)+unlist(lapply(swt.3,"[[",3))^nu-unlist(lapply(swt.3,"[[",2))^nu),
                                                        invHt3.t(v.3,t1.3,unlist(lapply(swt.3,"[[",1)),unlist(lapply(swt.3,"[[",2)),x.3,beta.x,beta.e,beta.x.TD,lambda,nu),
                                                        invHt5.t(v.3,t1.3,unlist(lapply(swt.3,"[[",1)),unlist(lapply(swt.3,"[[",2)),unlist(lapply(swt.3,"[[",3)),x.3,beta.x,beta.e,beta.x.TD,lambda,nu)))),
                                   ifelse(cond3.2,
                                          ifelse(v.3<lambda*exp(beta.x %*% x.3 +beta.e+beta.x.TD+beta.x.TD2)*(unlist(lapply(swt.3,"[[",2))^nu-t1.3^nu),
                                                 invHt2.t(v.3,t1.3,x.3,beta.x,beta.e,beta.x.TD,beta.x.TD2,lambda,nu),
                                                 ifelse(v.3<lambda*exp(beta.x %*% x.3+beta.e+beta.x.TD2)*(exp(beta.x.TD)*(unlist(lapply(swt.3,"[[",2))^nu-t1.3^nu)+(unlist(lapply(swt.3,"[[",3))^nu-unlist(lapply(swt.3,"[[",2))^nu)),
                                                        invHt4.t(v.3,t1.3,unlist(lapply(swt.3,"[[",2)),x.3,beta.x,beta.e,beta.x.TD,beta.x.TD2,lambda,nu),
                                                        invHt6.t(v.3,t1.3,unlist(lapply(swt.3,"[[",2)),unlist(lapply(swt.3,"[[",3)),x.3,beta.x,beta.e,beta.x.TD,beta.x.TD2,lambda,nu))),
                                          ifelse(cond3.3,
                                                 ifelse(v.3<lambda*exp(beta.x %*% x.3+beta.e)*(unlist(lapply(swt.3,"[[",3))^nu-t1.3^nu),
                                                        invHt0.t(v.3,t1.3,x.3,beta.x,beta.e,lambda,nu),
                                                        invHt1.t(v.3,t1.3,unlist(lapply(swt.3,"[[",3)),x.3,beta.x,beta.e,beta.x.TD,lambda,nu)),
                                                 invHt2.t(v.3,t1.3,x.3,beta.x,beta.e,beta.x.TD,beta.x.TD2,lambda,nu))))
        # T1 contains the t1 except for lines where dirty[i] = F ( then T1[i] <- NA )
        T1 <- cbind(T1, ifelse(dirty, t1, NA))   # note : some times can be >= fup
        # dirty = TRUE except if t or t1 is >= fup
        dirty <- ifelse(dirty, (t(tt) < fu) & (t(t1) < fu), dirty)
        # break when all elements of dirty = FALSE (no more time < fup)
        if (!any(dirty)) 
            break
        # T contains the t except for lines where dirty[i] = F ( then T[i] <- NA )
        T <- cbind(T, ifelse(dirty, tt, NA))
    }
    
    
    # start times
    start.t <- cbind(0, T1)     # debut a 0 pour tous
    start.t <- as.vector(t(start.t))    # on met en vecteur (ligne)
    tab.start.t <- start.t[!is.na(start.t)] # suppression des NA
    
    # stop times
    stop.t <- cbind(T, NA)
    d <- apply(!is.na(T), 1, sum)   # number of events per individual
    f <- d + 1
    for (i in 1:N) {
        stop.t[i, f[i]] <- fu[i]    # on concatene avec dernier tps = fu
    }
    stop.t <- as.vector(t(stop.t))
    tab.stop.t <- stop.t[!is.na(stop.t)]
    e <- NULL
    for (i in 1:N) {
        e <- cbind(e, t(rep(1, d[i])), 0)
    }
    tab.ID <- rep(ID, f)
    tab.X <- x[rep(1:nrow(x), f), ]
    tab.Fu <- rep(fu, f)
    w <- tab.start.t > tab.stop.t
    v <- rep(0, length(w))
    for (i in 1:length(w)) {
        if (w[i]) {
            v[i - 1] <- 1
        }
    }
    l <- tab.stop.t > tab.Fu
    for (i in 1:length(l)) {
        if (l[i]) {
            tab.stop.t[i] <- tab.Fu[i]
            e[i] <- 0
        }
    }
    tab <- cbind(tab.ID, tab.X, tab.start.t, tab.stop.t,
                 t(e), tab.Fu)
    for (i in 1:length(w)) {
        if (w[i]) {
            tab[i, ] <- rep(NA, 6)
        }
    }
    tab <- data.frame(id = tab[, 1], x = tab[, 2],
                      start = tab[, 3], stop = tab[, 4],
                      status = tab[, 5], fu = tab[, 6])
    tab <- na.omit(tab)
    
    # work on df by id
    tempotab <- split(tab, factor(tab$id))
    ipwtablist <- lapply(1:N, function(i){
        tabi <- tempotab[[i]]
        swi  <- switch.times[[i]]
        fuptime <- sort(unique(c(tabi$start, tabi$stop, tabi$fu, swi)))
        L <- length(fuptime)
        ipwtabi <- data.frame(id = rep(tabi$id[1], L), 
                              tstart = c(-1, fuptime[-length(fuptime)]),
                              fuptime = fuptime,
                              Init = rep(0, L), Term = rep(NA,L),
                              x = rep(unique(tabi$x), L), xt = rep(0,L))
        rm(fuptime)
        ind <- ipwtabi$fuptime %in% tabi$stop[tabi$status==1]
        
        # Init
        # = 1 when fuptime is a stop time with status=1
        ipwtabi$Init[ind] <- 1
        
        # = NA when !=1 AND
        # Init[k-1]=NA & tstart is a switch.time for the TDC
        # OR
        # Init[k-1]=1 & tstart is NOT a start of an at-risk interval
        for(k in which(!ind)[-1]){
            if((is.na(ipwtabi$Init[k-1]) & (ipwtabi$tstart[k] %in% swi)) || (ipwtabi$Init[k-1]==1 & !(ipwtabi$tstart[k] %in% tabi$start))){
                ipwtabi$Init[k] <- NA
            } else {
                ipwtabi$Init[k] <- 0
            }
        }
        
        # Term
        ipwtabi$Term[is.na(ipwtabi$Init)] <- ifelse(ipwtabi$fuptime[is.na(ipwtabi$Init)] %in% tabi$start,1,0)
        
        # L
        nbswi <- length(swi)
        if(nbswi !=0){
            for(l in 2:L){
                if(ipwtabi$tstart[l] %in% swi)
                    ipwtabi$xt[l:L] <- rep(abs(ipwtabi$xt[l-1] - 1), L-l+1)
            }
        }
        return(ipwtabi)
    })
    
    
    ipwtab <- do.call(rbind, ipwtablist)
    # deleting extra lines where tstart=-1 
    # (only used when modelling exposure allocation at the start of the fup (for fuptime=0))
    # here, nobody has treatment of interest already at time 0
    ipwtab <- ipwtab[ipwtab$tstart != -1, ] 
    
    # adding a variable expo corresponding to the treatment of interest exposure as covariate
    # expo = 1 when exposed between tstart and fuptime, i.e. when Init=NA
    ipwtab$expo <- rep(0, nrow(ipwtab))
    ipwtab$expo[is.na(ipwtab$Init)] <- 1
    
    return( list( ipwtab = ipwtab, switch.times = switch.times ))
}

