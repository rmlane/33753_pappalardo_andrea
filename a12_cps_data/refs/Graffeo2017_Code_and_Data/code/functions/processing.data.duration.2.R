process.data.dur.2 <- function(data , exposureI, exposureT, id, tstart, fuptime, 
                               timefixedcov1, timevarconf1, 
                               expTrtInterest, event) {
    
    #######################################################################################
    #   Filename    :	processing.data.dur.2.R
    #
    #   Description :   some data processing to apply the SVall function
    #                   computing the weights for all time points in the dataset, 
    #                   that is event times and time changes in the treatment of interest status
    #                   (see example 4.3 in van der Wal and Geskus (2011))
    #    
    #   Required package  :    ipw
    #
    #   Usage       :   process.data.dur(data , exposureI, exposureT, id, tstart, fuptime, 
    #                                    timefixedcov1, timevarconf1, 
    #                                    expTrtInterest, event)
    #                   data : the data to process
    #                   exposureI : indicator of initiation of the binary treatment of interest
    #                   exposureT : indicator of termination of the binary treatment of interest
    #                   id : the patients' id
    #                   tstart : the starting time for each interval of follow-up
    #                   fuptime : the end time for each interval of follow-up
    #                   timefixedcov_{1} : time-fixed binary covariates (1=numeric, 2=character)
    #                   timevarconf_{1}  : indicator of exposition of time-varying binary confounders
    #                   expTrtInterest : indicator of exposition of the treatment of interest
    #                   event: the recurrent event to study (categorical)
    #
    #    
    #   Value : a data.frame with all time points 
    #           (event times + time changes in the treatment of interest status)
    ########################################################################################
    tempcall <- match.call()
    data     <- data.frame(id = data[ , as.character(tempcall$id)],
                           tstart = data[ , as.character(tempcall$tstart)],     
                           fuptime = data[ , as.character(tempcall$fuptime)], 
                           Init = data[ , as.character(tempcall$exposureI)],
                           Term = data[ , as.character(tempcall$exposureT)],
                           x1 = data[ , as.character(tempcall$timefixedcov1)],
                           C1 = data[ , as.character(tempcall$timevarconf1)],
                           expo = data[ , as.character(tempcall$expTrtInterest)],
                           event = data[ , as.character(tempcall$event)]) 
    
    nPat   <- unique(data$id) 
    L.nPat <- length(nPat)
    # Tend by patient ####
    Tend <- vector("numeric", length = L.nPat)
    Tend <- sapply(nPat, function(i){
        Tend[i] <- max(data$fuptime[data$id == i])
    })
    
    # We consider the event times + the change times for the time-varying confounders C1 and C2
    times <- sort(unique(c(data$fuptime)))
    
    # building startstop ####
    startstop <- data.frame(id = rep(nPat, each = length(times)),
                            fuptime = rep(times, L.nPat))
    # dataFrame (id, Tend)
    timeEnd <- data.frame(id = unique(data$id),
                          Tend = Tend)
    # data.frame (id, fuptime, Tend)
    startstop <- merge(startstop, timeEnd, by = "id", all.x = TRUE)
    # we only keep fuptime <= Tend
    startstop <- startstop[with(startstop, fuptime <= Tend), ]
    # building tstart
    startstop$tstart <- tstartfun(id, fuptime, startstop)  
    # instead of tstart = -1 we want 0
    startstop$tstart <- ifelse(startstop$tstart == -1, 0, startstop$tstart)
    
    # split by patient  
    tabi     <- split(startstop, startstop$id)
    datatabi <- split(data, data$id)
    L.tabi   <- length(tabi)
    tablist <- lapply(1:L.tabi, function(i){
        lignes.tabi <- nrow(tabi[[i]])
        lignes.datatabi <- nrow(datatabi[[i]])
        # exposure initiation
        tabi[[i]]$Init <- rep(0, lignes.tabi)
        InitTimes <- 0
        # fup times at which changes in exposure status are observed
        InitTimes <- ifelse(datatabi[[i]]$Init ==1, datatabi[[i]]$fuptime, 0)
        # Initiation of the treatment of interest if fuptime belongs to the times previously listed
        tabi[[i]]$Init <- ifelse(tabi[[i]]$fuptime %in% InitTimes, 1, 0)
        # tstart and fuptime when Init = NA
        tempotab <- datatabi[[i]][is.na(datatabi[[i]]$Init), c("tstart","fuptime")]
        LignesTempo <- nrow(tempotab)
        if(LignesTempo >= 1){
            for(k in 1:LignesTempo){
                tabi[[i]]$Init <- ifelse(tabi[[i]]$tstart >= tempotab[k,1] & tabi[[i]]$tstart < tempotab[k,2], 
                                         NA, tabi[[i]]$Init)    
            }
        }
        # exposure of interest termination
        tabi[[i]]$Term <- rep(0, lignes.tabi)
        TermTimes <- 0
        # fup times at which changes in exposure status are observed
        TermTimes <- ifelse(datatabi[[i]]$Term ==1, datatabi[[i]]$fuptime, 0)
        # Termination of the treatment of interest if fuptime belongs to the times previously listed
        tabi[[i]]$Term <- ifelse(tabi[[i]]$fuptime %in% TermTimes, 1, 0)
        # tstart and fuptime when Term = NA
        tempotab <- datatabi[[i]][is.na(datatabi[[i]]$Term), c("tstart","fuptime")]
        LignesTempo <- nrow(tempotab)
        if(LignesTempo >= 1){
            for(k in 1:LignesTempo){
                tabi[[i]]$Term <- ifelse(tabi[[i]]$tstart >= tempotab[k,1] & tabi[[i]]$tstart < tempotab[k,2], 
                                         NA, tabi[[i]]$Term)    
            }
        }
        # time-varying confounder C1
        tabi[[i]]$C1 <- rep(0, lignes.tabi)
        # tstart and fuptime when C1 = 1
        tempotab <- datatabi[[i]][datatabi[[i]]$C1 == 1, c("tstart","fuptime")]
        LignesTempo <- nrow(tempotab)
        if(LignesTempo >= 1){
            for(k in 1:LignesTempo){
                tabi[[i]]$C1 <- ifelse(tabi[[i]]$tstart >= tempotab[k,1] & tabi[[i]]$tstart < tempotab[k,2], 
                                       1, tabi[[i]]$C1)    
            }
        }
        # time-fixed covariate x1
        tabi[[i]]$x1 <- vector("numeric", lignes.tabi)
        tabi[[i]]$x1 <- rep(datatabi[[i]][1, "x1"], lignes.tabi)
        tabi[[i]]$event <- vector("numeric", lignes.tabi)
        # event (recurrent)
        tabi[[i]]$event <- rep(0, lignes.tabi) #vector("numeric", lignes.tabi)
        if(length(datatabi[[i]]$fuptime[datatabi[[i]]$event==1]) != 0){
            fu.i <- datatabi[[i]]$fuptime[datatabi[[i]]$event==1]
            tabi[[i]]$event[tabi[[i]]$fuptime %in% fu.i] <- 1
        }
        
        # exposition of the treatment of interest
        tabi[[i]]$expo <- rep(0, lignes.tabi)
        # tstart and fuptime when expo = 1
        tempotab <- datatabi[[i]][datatabi[[i]]$expo == 1, c("tstart","fuptime")]
        LignesTempo <- nrow(tempotab)
        if(LignesTempo >= 1){
            for(k in 1:LignesTempo){
                tabi[[i]]$expo <- ifelse(tabi[[i]]$tstart >= tempotab[k,1] & tabi[[i]]$tstart < tempotab[k,2], 
                                         1, tabi[[i]]$expo)    
            }
        }
        # output
        return(tabi[[i]])
    })
    startstop <- do.call( rbind, tablist )
    
    # order the columns
    startstop <- startstop[, c("id", "tstart", "fuptime", "Init", "Term", 
                               "C1", "x1", "expo", "event")]
    
    names(startstop) <- c(tempcall$id, tempcall$tstart, tempcall$fuptime, 
                          tempcall$exposureI, tempcall$exposureT,
                          tempcall$timevarconf1, 
                          tempcall$timefixedcov1, 
                          tempcall$expTrtInterest, tempcall$event)
    
    return(startstop)
}