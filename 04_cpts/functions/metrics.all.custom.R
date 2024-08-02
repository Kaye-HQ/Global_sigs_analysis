metrics.all.custom <- function(TS_sig, Sigs_selected,NAthresh = 0.95,
                               TrendMethod = "yuepilon", CPTMethod = "BinSeg") {
  TS_sig %<>% setDT()
  get_each <- function(sigsn,
           TS_sig, NAthresh = 0.95,
           TrendMethod , CPTMethod ){
    
    TS <- TS_sig[,c('Year',sigsn,'ncount'),with = F]
    setnames(TS,sigsn,'Value')
    
    TS <- subset(TS, !is.na(TS$Value)  )
    TS <- subset(TS,  !is.infinite(Value))
    
    OmitYears <- TS[ncount < NAthresh*365,c('Year','ncount')]
    TS.sub <- TS[ncount >= NAthresh*365,]
    
    ## only run data analysis if there are more than 5 years in record
    if (length(unique(TS.sub$Year)) > 5) {
      Year1 <- min(TS.sub$Year)
      YearEnd <- max(TS.sub$Year)
      
      ## Calculate slopes, p-values, changepoints, etc.
      params <- list()
        
        MyY <- TS.sub$Value
        
        if (!is.na(MyY[1])) {
          MyX <- TS.sub$Year
          
          ### format x values to work with plotting of sen slopes and change points 
            MyX.mod <- c(1:length(MyX))
            for (j in 1:length(MyX)) {MyX.mod[j] <- (as.numeric(MyX[j]) - Year1) + 1}

          mrange <- max(MyY, na.rm=T) - min(MyY, na.rm=T)
          
          if (mrange > 0 & length(MyX.mod) > 5) {
            slope <- zyp::zyp.sen(MyY~MyX.mod)
            ci <- zyp::confint.zyp(slope)
            ci1 <- ci[,2]
            ci2 <- ci[,1]
            
            res <- zyp::zyp.trend.vector(MyY, x=MyX.mod, method=TrendMethod)
            slope <- c(res[[11]], res[[2]])
            pval <- res[[6]]
          } else {
            slope <- NA
            ci1 <- NA
            ci2 <- NA
            pval <- NA
          }
          
          if (length(MyY) > 3) {
            
            out <- tryCatch(suppressWarnings(changepoint::cpt.meanvar(as.numeric(MyY), 
                                                                      penalty="Asymptotic",
                                                                      pen.value=0.05,
                                                                      method=CPTMethod)), 
                            error = function(e) NA)
            
            if (class(out) == "cpt.range") {
              MyCpts <- out@cpts
              attr(MyCpts, "times") <- MyX[MyCpts]
              NumPoints <- length(MyX.mod)
              
              MyCpts_filtered <- NA
              if(sum(MyCpts > 3 & MyCpts < (NumPoints-3)) > 0){
                MyCpts_filtered <- MyCpts[MyCpts > 3 & MyCpts < (NumPoints-3)] ## remove cpts at end and beginning
                attr(MyCpts_filtered, "times") <- MyX[MyCpts_filtered]
              }
              
              MyMeans <- out@param.est$mean
            } else {
              MyCpts <- NA
              MyMeans <- NA
              MyCpts_filtered <- NA
            }
            
          } else {
            MyCpts <- NA
            MyMeans <- NA
            MyCpts_filtered <- NA
          }
          
          NumObs <- length(MyX.mod)
        } else {
          slope <- NA
          ci1 <- NA
          ci2 <- NA
          pval <- NA
          MyCpts <- NA
          MyMeans <- NA
          MyCpts_filtered <- NA
          NumObs <- NA
        }
        
        
        params <- list( MetricName=sigsn,
                            TrendMethod =TrendMethod , CPTMethod =CPTMethod, 
                            Slope=slope, ci1=ci1, ci2=ci2,
                           pval=pval, cpts=MyCpts, means=MyMeans, NumObs=NumObs,
                        cpts_filtered = MyCpts_filtered)
          
        # final output
      output <- list(metricTS=TS.sub, tcpRes=params,  OmitYrs=OmitYears)
      
      return(output)
      
    } else {return("Record is not of sufficient length (<5 hyears)")}
    print(sigsn)
  }
  
  out_ls <- lapply(as.list(Sigs_selected) , 
                   get_each,
                   TS_sig, NAthresh = 0.95,
                   TrendMethod , CPTMethod
                  )
  names(out_ls) <- Sigs_selected
  
  return(out_ls)
}
  
    
    