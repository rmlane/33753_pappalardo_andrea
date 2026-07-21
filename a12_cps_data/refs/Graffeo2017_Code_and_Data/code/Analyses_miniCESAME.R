#######################################################################################
#
#                                                                                     
#   Filename    :	Analyses_miniCESAME.R    												  
#                                                                                     
#   Project     :       BiomJ article "Modeling time-varying exposure using inverse probability of treatment weights"                                                             
#   Authors     :       N Grafféo, A Latouche, RB Geskus, and S Chevret                                                                
#   Date        :       19.05.2017
#																				  
#
#   Input data files  :    miniCESAME.RData                                                          
#   Output data files :    ---
#
#   Required R packages :  ipw, survival
#
#
########################################################################################

# Start the clock!
ptm <- proc.time()

#### loading required packages and functions ####
library(survival)
library(ipw)
source("functions/processing.data.duration.R")
source("functions/SVall.R")

#### loadind the dataset ####
load("../data/miniCESAME.RData")

#### data processing ####
# # # to check on a sample
# set.seed(12345)
# idtoy <- sample(miniCESAME$id, 1000, replace = FALSE)
# mytoy <- miniCESAME[miniCESAME$id %in% idtoy,  ]
# startstop <- process.data.dur(data = mytoy, exposureI = Init, exposureT = Term,
#                               id = id, tstart = tstart, fuptime = fuptime,
#                               timefixedcov1 = age, timefixedcov2 = sex,
#                               timevarconf1 = expMetho, timevarconf2 = expAntiTNF,
#                               expTrtInterest = expThiop, event = event)
startstop <- process.data.dur(data = miniCESAME, exposureI = Init, exposureT = Term,
                              id = id, tstart = tstart, fuptime = fuptime,
                              timefixedcov1 = age, timefixedcov2 = sex,
                              timevarconf1 = expMetho, timevarconf2 = expAntiTNF,
                              expTrtInterest = expThiop, event = event)

########################################################################################
#### section 4
########################################################################################
#### no truncation or truncation percentiles = (1,99) ####
#### Weights when exposure = initiation of treatment of interest ####
startstop_Init <- startstop[ ! is.na(startstop$Init), ]

out.Init <- SVall(exposure = Init,
                  numerator = ~ age, denominator =  ~ age + expMetho + expAntiTNF,
                  id = id, tstart = tstart, timevar = fuptime,
                  data = startstop_Init , trunc = 0.01)

#### Weights when exposure = termination of treatment of interest ####
startstop_Term <- startstop[ ! is.na(startstop$Term), ]

out.Term <- SVall(exposure = Term,
                  numerator = ~ age, denominator =  ~ age + expMetho + expAntiTNF,
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

#### Cox Marginal structural models ####
    # cancer (with normal weights)
summary.K <- summary(coxph(Surv(tstart, fuptime, event == 1) ~ expThiop + sex + age 
                           + cluster(id),
                           data = startstop, weights = startstop$weights))

    # cancer (with truncated weights)    
summary.K.trunc <- summary(coxph(Surv(tstart, fuptime, event == 1) ~ expThiop + sex + age
                                 + cluster(id),
                                 data = startstop, weights = startstop$weights.trunc))

    # death free of cancer (with normal weights) - RESULTS NOT SHOW-
# summary.DC <- summary(coxph(Surv(tstart, fuptime, event == 2) ~ expThiop + sex + age 
#                             + cluster(id),
#                             data = startstop, weights = startstop$weights)) 

    # death free of cancer (with truncated weights) - RESULTS NOT SHOW-
# summary.DC.trunc <- summary(coxph(Surv(tstart, fuptime, event == 2) ~ expThiop + sex + age  
#                             + cluster(id),
#                             data = startstop, weights = startstop$weights)) 


#### truncation percentiles = (5,95) ####
#### Weights when exposure = initiation of treatment of interest ####
out.Init.2 <- SVall(exposure = Init,
                    numerator = ~ age, denominator =  ~ age + expMetho + expAntiTNF,
                    id = id, tstart = tstart, timevar = fuptime,
                    data = startstop_Init , trunc = 0.05)

#### Weights when exposure = termination of treatment of interest ####
out.Term.2 <- SVall(exposure = Term,
                    numerator = ~ age, denominator =  ~ age + expMetho + expAntiTNF,
                    id = id, tstart = tstart, timevar = fuptime,
                    data = startstop_Term , trunc = 0.05)

#### Merging the colums of weights ####
data.w.Init.2 <- data.frame( id = out.Init.2$id,
                             weights = out.Init.2$ipw.weights,
                             weights.trunc = out.Init.2$weights.trunc )
data.w.Term.2 <- data.frame( id = out.Term.2$id,
                             weights = out.Term.2$ipw.weights,
                             weights.trunc = out.Term.2$weights.trunc )


# initialisation of 2 columns for the weights and the truncated weights
startstop$weights.trunc.2 <- vector("numeric", L.startstop)
# rows with Init != NA
startstop$weights.trunc.2[rows.init] <- data.w.Init.2$weights.trunc
# rows with Term != NA
startstop$weights.trunc.2[rows.term] <- data.w.Term.2$weights.trunc

#### Cox Marginal structural models ####
    # cancer (with truncated weights)    
summary.K.trunc.2 <- summary(coxph(Surv(tstart, fuptime, event == 1) ~ expThiop + sex + age
                                   + cluster(id),
                                   data = startstop, weights = startstop$weights.trunc.2))

# Table 2 ####

# # toy example
# sink("../results/Table_toy.txt")
sink("../results/Table_2.txt")
cat("-------------------------------------------------------------------------------------------------------\n")
cat("Truncation percentiles                   Mean (Min/Max)                               CSHR of cancer  \n")
cat("                                  Estimated stabilized weights                                        \n")
cat("                       -------------------------   -------------------------                     \n")
cat("                            relative to                    relative to             Estimate (95% CI)\n")
cat("                         treatment initiation        treatment discontinuation                    \n")
cat("-------------------------------------------------------------------------------------------------------\n")
cat("      (0,100)                ", round(mean(out.Init$ipw.weights),3), 
    "                         ", round(mean(out.Term$ipw.weights),3),
    "         ", round(summary.K$coefficients,3)[1,2], 
    " (", round(summary.K$conf.int[,"lower .95"],3)[1], "-",  round(summary.K$conf.int[,"upper .95"],3)[1],
    ")\n")
cat("                       (", round(min(out.Init$ipw.weights),3), "-", round(max(out.Init$ipw.weights),3),
    ")               (", round(min(out.Term$ipw.weights),3), "-", round(max(out.Term$ipw.weights),3),
    ")\n")
cat("      (1,99)                 ", round(mean(out.Init$weights.trunc),3), 
    "                         ", round(mean(out.Term$weights.trunc),3),
    "         ", round(summary.K.trunc$coefficients,3)[1,2], 
    " (", round(summary.K.trunc$conf.int[,"lower .95"],3)[1], "-",  round(summary.K.trunc$conf.int[,"upper .95"],3)[1],
    ")\n")
cat("                       (", round(min(out.Init$weights.trunc),3), "-", round(max(out.Init$weights.trunc),3),
    ")               (", round(min(out.Term$weights.trunc),3), "-", round(max(out.Term$weights.trunc),3),
    ")\n")
cat("      (5,95)                 ", round(mean(out.Init.2$weights.trunc),3), 
    "                         ", round(mean(out.Term.2$weights.trunc),3),
    "         ", round(summary.K.trunc.2$coefficients,3)[1,2], 
    " (", round(summary.K.trunc.2$conf.int[,"lower .95"],3)[1], "-",  round(summary.K.trunc.2$conf.int[,"upper .95"],3)[1],
    ")\n")
cat("                       (", round(min(out.Init.2$weights.trunc),3), "-", round(max(out.Init.2$weights.trunc),3),
    ")               (", round(min(out.Term.2$weights.trunc),3), "-", round(max(out.Term.2$weights.trunc),3),
    ")\n")
cat("-------------------------------------------------------------------------------------------------------\n")
sink()



# Stop the clock
proc.time() - ptm
