generEvent <- function(mydata, HR_e, HR_xt, h000, h001){
    
#######################################################################################
#                                                                                     
#   Filename    :	generEvent.R    												  
#                                                                                     
#   Description :   adding recurrent events to mydata depending on 2 time-dpt cov (xt and expo)
#                              and on the length of interval using the exponential distribution
#                   setting HR of expo = HR_e and HR of xt = HR_xt
#                   Given that expo = 0 or 1, xt = 0 or 1 and x = 0 or 1 => 8 combinations (= strata)
#                   Example: parameter of exponential distribution = h010 for expo = 0, xt = 1 and x = 0
#                   To have constant rates in each stratum, we set:
#                   HR_e = h100/h000 = h101/h001 = h111/h011 = h110/h010
#                   HR_xt = h010/h000 = h011/h001 = h111/h101 = h110/h100
#                   
#
#   Usage       :   generEvent(mydata, HR_e, HR_xt, h000, h001)
#             
#    
#   Value : mydata with an additional columns containing the indicator 
#                       of the generated recurrent events 
#######################################################################################
    
    mydata$event <- rep(0, nrow(mydata))
    
    h100 <- HR_e * h000
    h010 <- HR_xt * h000
    h101 <- HR_e * h001
    h110 <- HR_e * h010
    h011 <- HR_xt * h001
    h111 <- HR_e * h011
    
    set.seed(1357)
    
    strat1 <- mydata$expo==0 & mydata$xt==0 & mydata$x ==0
    strat2 <- mydata$expo==0 & mydata$xt==0 & mydata$x ==1
    strat3 <- mydata$expo==0 & mydata$xt==1 & mydata$x ==0
    strat4 <- mydata$expo==0 & mydata$xt==1 & mydata$x ==1
    strat5 <- mydata$expo==1 & mydata$xt==0 & mydata$x ==0
    strat6 <- mydata$expo==1 & mydata$xt==0 & mydata$x ==1
    strat7 <- mydata$expo==1 & mydata$xt==1 & mydata$x ==0
    strat8 <- mydata$expo==1 & mydata$xt==1 & mydata$x ==1
    
    mydata$event[strat1] <- ifelse(rexp(sum(strat1), h000) < (mydata$fuptime[strat1]-mydata$tstart[strat1]),1,0)
    mydata$event[strat2] <- ifelse(rexp(sum(strat2), h001) < (mydata$fuptime[strat2]-mydata$tstart[strat2]),1,0)
    mydata$event[strat3] <- ifelse(rexp(sum(strat3), h010) < (mydata$fuptime[strat3]-mydata$tstart[strat3]),1,0)
    mydata$event[strat4] <- ifelse(rexp(sum(strat4), h011) < (mydata$fuptime[strat4]-mydata$tstart[strat4]),1,0)
    mydata$event[strat5] <- ifelse(rexp(sum(strat5), h100) < (mydata$fuptime[strat5]-mydata$tstart[strat5]),1,0)
    mydata$event[strat6] <- ifelse(rexp(sum(strat6), h101) < (mydata$fuptime[strat6]-mydata$tstart[strat6]),1,0)
    mydata$event[strat7] <- ifelse(rexp(sum(strat7), h110) < (mydata$fuptime[strat7]-mydata$tstart[strat7]),1,0)
    mydata$event[strat8] <- ifelse(rexp(sum(strat8), h111) < (mydata$fuptime[strat8]-mydata$tstart[strat8]),1,0)
    
    

    return(mydata)
}
