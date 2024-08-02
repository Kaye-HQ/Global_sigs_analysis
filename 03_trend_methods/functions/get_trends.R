get_trends <- function(TS_sig, Sigs_selected,
                       NAthresh = 0.95,
                       TrendMethod = "yuepilon",
                       if_fill = T) {
  TS_sig %<>% setDT()
  get_each <- function(sigsn,
                       TS_sig, NAthresh = 0.95,
                       TrendMethod ,if_fill){
    print(paste(sigsn,  TrendMethod,'start'))
    
    TS <- TS_sig[,c('Year',sigsn,'ncount'),with = F] %>% setDT()
    setnames(TS,sigsn,'Value')
    
    TS[ is.na(TS$Value) | is.infinite(Value) | ncount < NAthresh*365 ,]$Value <- NA
    
    if(sum(!is.na(TS$Value)) > 5 ){
      # 找到第一个非NA值的索引
    first_non_na <- which(!is.na(TS$Value))[1]
    # 找到最后一个非NA值的索引
    last_non_na <- tail(which(!is.na(TS$Value)), 1)
    # 保留非NA值之间的部分   # 移除首尾的NA值
    df_keep <- TS[first_non_na:last_non_na, ]
    
    if(sum(is.na(df_keep$Value)) == 0){
      # 如果中间无空值  直接使用df_keep
      df_keep$Value_complete <- df_keep$Value
      fill_rate <- 0
      data <- ts(data = df_keep$Value,
                 frequency = 1,
                 start = min(df_keep$Year),
                 end = max(df_keep$Year))
    }else{
       # 有空值则判断是否填充
    ## if not consecutive, first generate only lm sens spearman, 
    ### then fill na to make ts, calculate all
      if(if_fill == T){
        df_keep$Value_complete <- zoo::na.approx(df_keep$Value, na.rm=FALSE) 
        fill_rate <- sum(is.na(df_keep$Value)) / nrow(df_keep)
        data <- ts(data = df_keep$Value_complete,
                   frequency = 1,
                   start = min(df_keep$Year),
                   end = max(df_keep$Year))
      }else{
        fill_rate <- 0
        data <- ts(data = df_keep$Value,
                   frequency = 1,
                   start = min(df_keep$Year),
                   end = max(df_keep$Year))
      }
    }
    
    ## if consecutive, generate a ts and calculate all metrics
    ## only run data analysis if there are more than 5 years in record
    if (length(data) > 5) {
       ########### 
      #############不同方法计算趋势
      if (TrendMethod %in% 'lm'){
        # 1. Linear regression
        linear_model <- lm(data ~  time(data))
        trend_out <- data.table(TrendMethod = TrendMethod,
                                Trend = coef(linear_model)[2],
                                p = summary(linear_model)$coefficients[2, 4]
                                ) %>% 
          mutate(     TrendDir = ifelse(is.na(Trend),NA,
                                        ifelse(Trend > 0, "upward",
                                              ifelse( Trend < 0,"downward","Zero")) )
                      ) %>% setDT()
        
      }else if(TrendMethod %in% 'spearman'){
        #  2. Spearman's Rank correlation trend test
        spearman_result <- cor.test(time(data), data, method = "spearman")
        trend_out <- data.table(TrendMethod = TrendMethod,
                                Trend = NA,
                                TrendDir = ifelse(is.na(spearman_result$estimate),NA,
                                                  ifelse(spearman_result$estimate > 0, "upward",
                                                         ifelse(spearman_result$estimate < 0,"downward","Zero"))
                                ) ,
                                p = spearman_result$p.value
        )
      }else if(TrendMethod %in% 'zyp'){
        # 3. Sen's Slope (zyp package)
        zyp_result <- zyp::zyp.trend.vector(data) %>% t()  %>% as.data.frame()
        trend_out <- data.table(TrendMethod = TrendMethod,
                                Trend = zyp_result$trend,
                                p = zyp_result$sig,
                                TrendDir = ifelse(is.na(zyp_result$trend),NA,
                                                  ifelse(zyp_result$trend> 0, "upward",
                                                         ifelse(zyp_result$trend< 0,"downward","Zero"))
                                ) 
        )
      }else if(TrendMethod %in% 'mov_avg'){
        # 4. TTR
        moving_avg <- TTR::SMA(data ,n= 3)
        # 对平滑后的数据进行线性回归
        model_ma <- lm(moving_avg ~ time(moving_avg))
        summary_model_ma <- summary(model_ma)
        # 提取斜率（趋势）和显著性p值
        trend_out <- data.table(TrendMethod = TrendMethod, 
                                Trend =  coef(model_ma)[2],
                                TrendDir = ifelse(is.na(coef(model_ma)[2]),NA,
                                                  ifelse(coef(model_ma)[2] > 0, "upward",
                                                         ifelse(coef(model_ma)[2]< 0,"downward","Zero"))
                                ) ,
                                p = summary_model_ma$coefficients[2, 4]
                                )
      }else if(TrendMethod %in% 'loess'){
        # 5. loess 计算指数平滑值（alpha = 0.3）
        loess_smooth <-  loess(data~time(data))
        
        smoothed_series_loess <- predict(loess_smooth)
        model_loess <- lm(smoothed_series_loess ~ time(smoothed_series_loess))
        summary_model_loess <- summary(model_loess)
        # 提取斜率（趋势）和显著性p值
        trend_out <- data.table(TrendMethod = TrendMethod, 
                                Trend =  coef(model_loess)[2],
                                TrendDir = ifelse(is.na(coef(model_loess)[2]),NA,
                                                  ifelse(coef(model_loess)[2] > 0, "upward",
                                                         ifelse(coef(model_loess)[2] < 0,"downward","Zero"))
                                ) ,
                                p =  summary_model_loess$coefficients[2, 4]
        )
        }else if(TrendMethod %in% 'sens'){
        # 6. Sen's Slope (trend package)
        sens_slope <- trend::sens.slope(data)
        trend_out <- data.table(TrendMethod = TrendMethod,
                                Trend = sens_slope$estimates,
                                TrendDir = ifelse(is.na(sens_slope$estimates),NA,
                                                  ifelse(sens_slope$estimates > 0, "upward",
                                                         ifelse(sens_slope$estimates < 0,"downward","Zero"))
                                ) ,
                                p = sens_slope$p.value
        )
        }else if(TrendMethod %in% 'mk'){
        # 7. Mann-Kendall trend test
        mk_result <- Kendall::MannKendall(data)
        trend_out <- data.table(TrendMethod = TrendMethod,
                                Trend = NA,
                                TrendDir = ifelse(is.na(mk_result$tau),NA,
                                                   ifelse(mk_result$tau > 0, "upward",
                                                  ifelse(mk_result$tau < 0,"downward","Zero"))
                                                  ) ,
                                p = mk_result$sl 
        )
      }else if(TrendMethod %in% 'ARIMA'){
        # 8. ARIMA model
        arima_model <- forecast::auto.arima(data)
        
        trend_out <- data.table(TrendMethod = TrendMethod,
                                Trend = if ("drift" %in% names(coef(arima_model))) coef(arima_model)["drift"] else NA,
                                p = NA
        ) %>% mutate(TrendDir = ifelse(is.na(Trend),NA,
                                       ifelse(Trend > 0 ,
                                              "upward", 
                                              ifelse(Trend == 0, 
                                                     "Zero",
                                                     "downward"))))
        
    
      } 
      
    
    trend_out %<>% mutate(Trend_pct = Trend/data[1],
                          Trend_pct_mean = Trend/mean(data,na.rm =T),
                          if_fill = if_fill,
                          fill_rate = fill_rate ,
                          data_length = length(data),
                          Year_Start = time(data)[1],
                          Year_End = time(data)[length(data)],
                          Sigs = sigsn
    )
    trend_out %<>% setDT()
    trend_out[is.infinite(Trend_pct),]$Trend_pct <- NA
    trend_out[is.infinite(Trend_pct_mean),]$Trend_pct_mean <- NA
  
    print(paste(sigsn,  TrendMethod,'done'))
      return(trend_out)

    } else {
      return(NULL)
      print("Record is not of sufficient length (<5 hyears)")
    }
    }else{
      return(NULL)
      print("Record is not of sufficient length (<5 hyears)")
    }
    
  }
  
  out_ls <- lapply(as.list(Sigs_selected) , 
                   get_each,
                   TS_sig, NAthresh = 0.95,
                   TrendMethod ,if_fill
  ) %>% rbindlist(.,use.names = T,fill = T) %>% setDT()
  
  return(out_ls)
}