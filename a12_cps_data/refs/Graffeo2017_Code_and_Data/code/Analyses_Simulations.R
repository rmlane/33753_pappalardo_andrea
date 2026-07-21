#######################################################################################
#
#                                                                                     
#   Filename    :	Analyses_Simulations.R    												  
#                                                                                     
#   Project     :       BiomJ article "Modeling time-varying exposure using inverse probability of treatment weights"                                                             
#   Authors     :       N Grafféo, A Latouche, RB Geskus, and S Chevret                                                                
#   Date        :       04.10.2017
#																				  
#   R Version   :       R-3.3.3                                                                
#
#   Input data files  :    ---                                                          
#   Output data files :    csv file
#
#   Required R packages :  ipw, survival, parallel (optional)
#
#   Note that to speed up the code, it is possible to use parallel processing
#   (e.g., using the "parallel" package) 
########################################################################################

#### loading required packages and functions ####
# packages
require(ipw)
require(survival)

# functions
source("functions/DataGeneration/mySimrecTDC.R")
source("functions/DataGeneration/CumHazRange.R") 
source("functions/DataGeneration/invH.R")
source("functions/DataGeneration/invHrecstep.R")

source("functions/DataGeneration/generEvent.R")

source("functions/processing.data.duration.2.R")
source("functions/SVall.R")

# Set the seed ####
Nsimu   <- 100   # to adapt if parallel process is used
nb_core <- 100/4 # to adapt if parallel process is used
MySeeds <- c(103,2015)
graineMat <- matrix(MySeeds[1]+MySeeds[2]*(1:(Nsimu*nb_core)), nrow=nb_core, ncol=Nsimu)
ng <- graineMat[1,] # reimplace 1 by i in 1:nb_core if parallel process is used

# Required parameters ####
npatient <- 200 # the number of patients equal 500 in the simulation study presented in the paper
                # here, we put 100 to speed up the code

# for the generation of recurrent event times for the trt of interest exposure initiation
# (model for the weights)
beta.x     <- 0.5  # log HR of x (fixed covariate)
beta.e     <- 1.1  # log HR of previous outcome (exposure)
beta.x.TD  <- -1.2 # log HR of xt (time-dpt covariate)
beta.x.TD2 <- -0.8 # log HR of previous xt 
par.rec  <- c(1,2) 
dfree <- 1/12  # length of the interval free of event (i.e., where patient is always exposed)

# for the generation of recurrent event times depending on expo and xt
# (Andersen-Gill structural model)
HR_e  <- 1.3
HR_xt <- 1.2

####
mytest <- list(); fit <- list(); fit.old <- list()


res <- data.frame(case1=rep("OldWeights", Nsimu),
                  HRe1=rep(0,Nsimu), CIinfe1=rep(0,Nsimu), CIsupe1=rep(0,Nsimu), 
                  HRx1=rep(0,Nsimu), CIinfx1=rep(0,Nsimu), CIsupx1=rep(0,Nsimu), 
                  var.expo1=rep(0,Nsimu), var.x1=rep(0,Nsimu), cov.expox1=rep(0,Nsimu),
                  var.naive.expo1=rep(0,Nsimu), var.naive.x1=rep(0,Nsimu),
                  cov.naive.expox1=rep(0,Nsimu), 
                  n1=rep(0,Nsimu), nevent1=rep(0,Nsimu),
                  case2=rep("NewWeights", Nsimu),
                  HRe2=rep(0,Nsimu), CIinfe2=rep(0,Nsimu), CIsupe2=rep(0,Nsimu), 
                  HRx2=rep(0,Nsimu), CIinfx2=rep(0,Nsimu), CIsupx2=rep(0,Nsimu),
                  var.expo2=rep(0,Nsimu), var.x2=rep(0,Nsimu), cov.expox2=rep(0,Nsimu),
                  var.naive.expo2=rep(0,Nsimu), var.naive.x2=rep(0,Nsimu),
                  cov.naive.expox2=rep(0,Nsimu), 
                  n2=rep(0,Nsimu), nevent2=rep(0,Nsimu))


for (i in 1:Nsimu)
{
    ### generating the data ####
    
    set.seed(ng[i])
    mytest[[i]] <- (mySimrecTDC( N = npatient, 
                                 beta.x = beta.x, beta.e=beta.e, 
                                 beta.x.TD = beta.x.TD, beta.x.TD2=beta.x.TD2,
                                 par.rec = par.rec,
                                 dfree = dfree ) )$ipwtab
    
    
    ### adding a recurrent event depending on xt and expo
    # HR of expo = 1.3 and HR of xt = 1.2
    # the two other parameters are the one of exponential distribution 
    mytest[[i]] <- generEvent(mytest[[i]], HR_e = HR_e, HR_xt = HR_xt, 5, 5)
    
    ### data processing ####
    startstop <- process.data.dur.2(data = mytest[[i]], exposureI = Init, exposureT = Term,
                                    id = id, tstart = tstart, fuptime = fuptime,
                                    timefixedcov1 = x, 
                                    timevarconf1 = xt, 
                                    expTrtInterest = expo, event = event)
    
    # Analysis with new weights ####
    #### no truncation or truncation percentiles = (1,99) ####
    #### Weights when exposure = initiation of treatment of interest ####
    startstop_Init <- startstop[ ! is.na(startstop$Init), ]
    
    out.Init <- SVall(exposure = Init,
                      numerator = ~ x, denominator =  ~ x + xt,
                      id = id, tstart = tstart, timevar = fuptime,
                      data = startstop_Init , trunc = 0.01)
    
    #### Weights when exposure = termination of treatment of interest ####
    startstop_Term <- startstop[ ! is.na(startstop$Term), ]
    
    out.Term <- SVall(exposure = Term,
                      numerator = ~ x, denominator =  ~ x + xt,
                      id = id, tstart = tstart, timevar = fuptime,
                      data = startstop_Term , trunc = 0.01)
    
    #### Merging the colums of weights ####
    data.w.Init <- data.frame( id = out.Init$id,
                               weights = out.Init$ipw.weights,
                               weights.trunc = out.Init$weights.trunc )
    data.w.Term <- data.frame( id = out.Term$id,
                               weights = out.Term$ipw.weights,
                               weights.trunc = out.Term$weights.trunc )
    
    # initialisation of 2 columns for the weights and the truncated weights
    L.startstop             <- nrow(startstop)
    
    startstop$weights       <- vector("numeric", L.startstop)
    startstop$weights.trunc <- vector("numeric", L.startstop)
    # rows with Init != NA
    rows.init <- which( !is.na(startstop$Init))
    startstop$weights[rows.init]       <- data.w.Init$weights
    startstop$weights.trunc[rows.init] <- data.w.Init$weights.trunc
    # rows with Term != NA
    rows.term <- which( !is.na( startstop$Term))
    startstop$weights[rows.term]       <- data.w.Term$weights
    startstop$weights.trunc[rows.term] <- data.w.Term$weights.trunc
    
    # Analysis with weights with the current ipw package ####
    #### no truncation or truncation percentiles = (1,99) ####
    #### Weights when exposure = initiation of treatment of interest ####
    out.Init.1 <- ipwtm(exposure = Init, family="survival",
                        numerator = ~ x, denominator =  ~ x + xt,
                        id = id, tstart = tstart, timevar = fuptime,
                        type="first",
                        data = startstop_Init , trunc = 0.01)
    
    #### Weights when exposure = termination of treatment of interest ####
    out.Term.1 <- ipwtm(exposure = Term, family="survival",
                        numerator = ~ x, denominator =  ~ x + xt,
                        id = id, tstart = tstart, timevar = fuptime,
                        type="first",
                        data = startstop_Term , trunc = 0.01)
    
    #### Merging the colums of weights ####
    data.w.Init.1 <- data.frame( id = out.Init$id,
                                 weights = out.Init.1$ipw.weights,
                                 weights.trunc = out.Init.1$weights.trunc )
    data.w.Term.1 <- data.frame( id = out.Term$id,
                                 weights = out.Term.1$ipw.weights,
                                 weights.trunc = out.Term.1$weights.trunc )
    
    # initialisation of 2 columns for the weights and the truncated weights
    L.startstop             <- nrow(startstop)
    startstop$weights.old       <- vector("numeric", L.startstop)
    startstop$weights.trunc.old <- vector("numeric", L.startstop)
    # rows with Init != NA
    startstop$weights.old[rows.init]       <- data.w.Init.1$weights
    startstop$weights.trunc.old[rows.init] <- data.w.Init.1$weights.trunc
    # rows with Term != NA
    startstop$weights.old[rows.term]       <- data.w.Term.1$weights
    startstop$weights.trunc.old[rows.term] <- data.w.Term.1$weights.trunc
    
    
    #### Cox Marginal structural models ####
    # with truncated "old" weights (from ipw package)   
    fit.old[[i]] <- coxph(Surv(tstart, fuptime, event == 1) ~ expo + x
                            + cluster(id),
                            data = startstop, weights = startstop$weights.trunc.old)
    fit1 <- summary(fit.old[[i]])
    
    # with truncated "new" weights   
    fit[[i]] <- coxph(Surv(tstart, fuptime, event == 1) ~ expo + x 
                      + cluster(id),
                      data = startstop, weights = startstop$weights.trunc)
    fit2 <- summary(fit[[i]])
    
    
    
    res[i,] <- c("OldWeights", fit1$conf.int[1,1], fit1$conf.int[1,3], fit1$conf.int[1,4],
                 fit1$conf.int[2,1], fit1$conf.int[2,3], fit1$conf.int[2,4],
                 fit.old[[i]]$var[1,1],fit.old[[i]]$var[2,2],
                 fit.old[[i]]$var[1,2], 
                 fit.old[[i]]$naive.var[1,1], fit.old[[i]]$naive.var[2,2],
                 fit.old[[i]]$naive.var[1,2],
                 fit.old[[i]]$n, fit.old[[i]]$nevent,
                 "NewWeights", fit2$conf.int[1,1], fit2$conf.int[1,3], fit2$conf.int[1,4],
                 fit2$conf.int[2,1], fit2$conf.int[2,3], fit2$conf.int[2,4],
                 fit[[i]]$var[1,1],fit[[i]]$var[2,2],
                 fit[[i]]$var[1,2], 
                 fit[[i]]$naive.var[1,1], fit[[i]]$naive.var[2,2],
                 fit[[i]]$naive.var[1,2],
                 fit[[i]]$n, fit[[i]]$nevent)
}


res

write.table(res,"../results/SimulationsResults.csv")

## Biases and TRE ####
result <- read.csv2("../results/SimulationsResults.csv", sep=" ", header = TRUE, dec=".")

bias.oldmethod <- mean(result$HRe1) - HR_e # [1] 0.1535018
TREoldmethod  <- sum((1.3>result$CIinfe1) & (1.3<result$CIsupe1)) / nrow(result) # [1] 0.90


bias.newmethod <- mean(result$HRe2) - HR_e # [1] 0.0604612
TREnewmethod  <- sum((1.3>result$CIinfe2) & (1.3<result$CIsupe2)) / nrow(result) # [1] 0.96
