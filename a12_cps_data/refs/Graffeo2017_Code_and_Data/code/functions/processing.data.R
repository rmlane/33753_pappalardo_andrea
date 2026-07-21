process.data <- function(data , exposure, id, tstart, fuptime, 
                         timefixedcov, timevarconf) {

#######################################################################################
#   Filename    :	processing.data.R
#
#   Description :   some data processing to apply the ipwtm/SVall functions
#                   computing the weights for all time points in the dataset, 
#                   that is event times and time changes in the treatment of interest status
#                   (see example 4.3 in van der Wal and Geskus (2011))
#    
#   Required package  :    ipw
#
#   Usage       :   process.data(data, exposure, id, tstart, fuptime, 
#                                timefixedcov, timevarconf)
#                   data : the data to process
#                   exposure : indicator of administration of the binary treatment of interest
#                   id : the patients' id
#                   tstart : the starting time for each interval of follow-up
#                   fuptime : the end time for each interval of follow-up
#                   timefixedcov : a time-fixed binary covariate
#                   timevarconf : the time-varying binary confounder  
#    
#   Value : a data.frame with all time points 
#           (event times + time changes in the treatment of interest status)
########################################################################################
    tempcall <- match.call()
    data     <- data.frame(id = data[ , as.character(tempcall$id)],
                           tstart = data[ , as.character(tempcall$tstart)],     
                           fuptime = data[ , as.character(tempcall$fuptime)], 
                           exposure = data[ , as.character(tempcall$exposure)],
                           timefixedcov = data[ , as.character(tempcall$timefixedcov)],
                           timevarconf = data[ , as.character(tempcall$timevarconf)]) 

    nPat <- length(unique(data$id))
    # Tend by patient
    Tend <- vector("numeric", length = nPat)
    Tend <- sapply(1:nPat, function(i){
        Tend[i] <- max(data$fuptime[data$id == i])
    } )
    
    # We consider the event times + the change times for the time-varying confounder timevarconf 
    times <- sort(unique(c(data$fuptime)))
    
    # replication of rows
    startstop <- data.frame(id = rep(1:nPat, each = length( times)),
                            fuptime = rep(times, nPat))
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
    tabi   <- split(startstop, startstop$id)
    datatabi <- split(data, data$id)
    L.tabi <- length(tabi)
    tablist <- lapply(1:L.tabi, function( i ){
        lignes.tabi <- nrow(tabi[[i]])
        lignes.datatabi <- row(datatabi[[i]])
        # exposure
        tabi[[i]]$exposure <- rep(0, lignes.tabi)
        InitTimes <- 0
        InitTimes <- ifelse(datatabi[[i]]$exposure == 1, datatabi[[i]]$fuptime, 0)
        tabi[[i]]$exposure <- ifelse(tabi[[i]]$fuptime %in% InitTimes, 1, 0)
        # time-varying confounder timevarconf
        tabi[[i]]$timevarconf <- rep(0, lignes.tabi)
        tempotab <- datatabi[[i]][datatabi[[i]]$timevarconf==1, c(2,3)]
        LignesTempo <- nrow(tempotab)
        if(LignesTempo >= 1){
            for(k in 1:LignesTempo){
                tabi[[i]]$timevarconf <- ifelse(tabi[[i]]$tstart >= tempotab[k,1] & tabi[[i]]$tstart < tempotab[k,2], 
                                       1, tabi[[i]]$timevarconf)    
            }
        }
        # time-fixed covariate timefixedcov
        tabi[[i]]$timefixedcov <- vector("numeric", lignes.tabi)
        tabi[[i]]$timefixedcov <- ifelse(datatabi[[i]][1, "timefixedcov"] == 1, 1, 0)
        return(tabi[[i]])
    })
    startstop <- do.call(rbind, tablist)
    
    startstop <- startstop[, c("id", "tstart", "fuptime", "exposure", 
                               "timefixedcov", "timevarconf")]
    
    names(startstop) <- c(tempcall$id, tempcall$tstart, tempcall$fuptime, 
                          tempcall$exposure, tempcall$timefixedcov, tempcall$timevarconf)
    
    return(startstop)
}