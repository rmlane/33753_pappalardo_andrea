#######################################################################################
#
#                                                                                     
#   Filename    :	Analyses_dataExample.R    												  
#                                                                                     
#   Project     :       BiomJ article "Modeling time-varying exposure using inverse probability of treatment weights"                                                             
#   Authors     :       N Grafféo, A Latouche, RB Geskus, and S Chevret                                                                
#   Date        :       19.05.2017
#																				  
#   R Version   :       R-3.3.3                                                                
#
#   Input data files  :    dataExample.RData                                                          
#   Output data files :    ---
#
#   Required R packages :  ipw, survival
#
#
########################################################################################

#### loading required packages and functions ####
library(survival)
library(ipw)
source("functions/processing.data.R")
source("functions/SVall.R")

#### loadind the dataset ####
load("../data/dataExample.RData")

#### data processing ####
startstop <- process.data(data = dataExample, exposure = expo,
                          id = id, tstart = tstart, fuptime = fuptime,
                          timefixedcov = x, timevarconf = xt)

########################################################################################
#### section 3.2 ####
########################################################################################
#### analysis with the R ipw function (up to the 1st treatment of interest switch) ####
# ipw: up to the 1st time switch ####
example.ipw <- ipwtm(exposure = expo, family = "survival",
                     numerator = ~ x, denominator = ~ x + xt,
                     id = id, tstart = tstart, timevar = fuptime,
                     type = "first", data = startstop, trunc = 0.01)

# summary ####
a <- example.ipw$ipw.weights
summary.example.ipw <- matrix(NA, nrow=1, ncol=5)
colnames(summary.example.ipw) <- c("min", "1st percentile", "Mean", "99th percentile", "Max")
summary.example.ipw[1,] <- c(quantile(a, probs=c(0, 0.01)), mean(a), quantile(a, probs=c(0.99, 1)))
capture.output(summary.example.ipw, file = "../results/summary_ipw.txt")


# Figure 2 ####
#jpeg(file="../results/Figure_2.jpg")
setEPS()
postscript("../results/Figure_2.eps")
ipwplot(weights = example.ipw$weights.trunc,
        timevar = startstop$fuptime,
        binwidth = 0.05, 
        xlab = "Time since enrollment (years)",
        ylab = "Logarithm of the truncated stabilized weights",
        ylim = c(-1,1))
dev.off()

########################################################################################
#### section 3.3 ####
########################################################################################
#### analysis with the modified ipw function ####
example.ipw.mod <- SVall(exposure = expo, 
                         numerator = ~ x, denominator = ~ x + xt,
                         id = id, tstart = tstart, timevar = fuptime,
                         data = startstop, trunc = 0.01)

# summary ####
b <- example.ipw.mod$ipw.weights
summary.example.ipw.mod <- matrix(NA, nrow=1, ncol=5)
colnames(summary.example.ipw.mod) <- c("min", "1st percentile", "Mean", "99th percentile", "Max")
summary.example.ipw.mod[1,] <- c(quantile(b, probs=c(0, 0.01)), mean(b), quantile(b, probs=c(0.99, 1)))
capture.output(summary.example.ipw.mod, file = "../results/summary_ipw_mod.txt")


# Figure 3 ####
#jpeg(file="../results/Figure_3.jpg")
setEPS()
postscript("../results/Figure_3.eps")
ipwplot(weights = example.ipw.mod$weights.trunc,
        timevar = startstop$fuptime,
        binwidth = 0.05, 
        xlab = "Time since enrollment (years)",
        ylab = "Logarithm of the truncated stabilized weights",
        ylim = c(-1,1))
dev.off()

# Figure 4 ####
# Exact weights: 
lambda    <- 1 ; nu <- 2 # coeff. of the Weibull distribution (baseline hazard)
                         # used to generate the dataExample
beta.x    <-  0.5        # coeff. of the fixed covariate x
beta.x.TD <-  -1.2       # coeff. of the time-dependent covariate xt
# probabilities for lines where treatment of interest is not initiated
ind.trtNotInit <- startstop$expo == 0
startstop$exact.prob[ind.trtNotInit] <- exp( - lambda * (startstop$fuptime ^ nu - startstop$tstart ^ nu )[ind.trtNotInit] * 
                                                exp(beta.x * startstop$x + beta.x.TD * startstop$xt)[ind.trtNotInit])
# probabilities for lines where treatment of interest is initiated
ind.trtInit <- startstop$expo == 1
startstop$exact.prob[ind.trtInit] <- 1 - exp( - lambda * (startstop$fuptime ^ nu - startstop$tstart ^ nu )[ind.trtInit] * 
                                                  exp(beta.x * startstop$x + beta.x.TD * startstop$xt)[ind.trtInit])
# exact.weights = cumulative product of the "1 / exact.weights"
startstop$exact.weights <- 1 / unlist(lapply(split(startstop$exact.prob, startstop$id), function(x)  cumprod(x)))


# Unstabilized weights obtained with the modified ipw function:
example.unst.ipw.mod <- SVall(exposure = expo, 
                              numerator = NULL, denominator = ~ x + xt,
                              id = id, tstart = tstart, timevar = fuptime,
                              data = startstop, trunc = 0.01)

# Relative Biases:
relBias <- (log10(startstop$exact.weights) - log10(example.unst.ipw.mod$ipw.weights)) / 
                      log10(startstop$exact.weights)


# #jpeg(file="../results/Figure_4.jpg")
# setEPS()
# postscript("../results/Figure_4.eps")
# boxplot(relBias, outline=FALSE)
# dev.off()
quantile(relBias)
