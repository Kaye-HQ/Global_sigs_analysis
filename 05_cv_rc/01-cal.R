

  Path0 <- 'H:/0001Work/Global_analyze_detect/Result/05_cv_rc/'
  
  out_path <- paste0(Path0,'01_cal/')
  dir.create(out_path,showWarnings = F,recursive = T)
  
  
  water_yr_out_path <- paste0(Path0,'02_cal_hydro_yr/')
  dir.create(water_yr_out_path,showWarnings = F,recursive = T)
    
Sigs_selected <- c("BFI_Eckhardt", "BFI_Chapman_maxwell",  "BFI_Chapman"  ,  
                   "BFI_Lynehollick",  "BFI_UKIH"  ,
                   "Quantile_90"   , "Quantile_10" ,   
                   "Quantile_95"   , "Quantile_05" ,
                   "Quantile_99"   , "Quantile_01" ,
                   'Mean','QT1', 'QT3', 'IQR'   ,   'FDCs', 'CI',
                   "LFD"     , 'HFD' ,  "ZFD"    ,'MLFD','MHFD',
                   "DOYMAX", "P_MSWEP", "Temp"  ,"Tmax" ,  "Tmin" ,'LAI_yr_GLASS' ,
                   # others 
                   "Mean_peaks_50","Num_peaks_50" ,   "Mean_peaks"  ,    "Num_peaks", 
                   "Mean_peak_Q99" ,  "Mean_peak_Q90" ,"Mean_low_Q10", "Mean_low_Q01" ,
                   "HFD_Q99"   ,"HFD_Q90" ,   "LFD_Q10" ,   "LFD_Q01" , 
                   "HFI_Q99" ,"HFI_Q90" ,  "LFI_Q10" ,  "LFI_Q01"  ,
                   "MLFD_Q01" , "MLFD_Q10"    , "MHFD_Q90" ,   "MHFD_Q99",
                   "HFT","cv","AI"   ,"DOY_MIN7"  ,  "DOY_MAX7" 
                   
)

Data_path <- 'H:/0001Work/Global_analyze_detect/Data/'

load(paste0(Data_path,'RF_Training_all_trend.RData'))
Trend_train <- Training_all
load(paste0(Data_path,'Training_all_annual_7683.RData'))

dt_annual <- Training_all[STAID %in% Trend_train$STAID,] 

daily_path <- 'H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/Merge_Data/Data_Daily/'
water_yr_path <- 'H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/Merge_Data/00_Signatures/WatYear/'
sig_yr_path <- 'H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/Merge_Data/00_Signatures/Year/'
infos <- fread('H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/infos_1_4.csv')


done <- list.files(paste0(out_path,'02_CV_each/'),pattern = '.csv',full.names = F) %>% 
  str_remove(.,'.csv') %>%
  data.table(Name = .)

dt_ls <- dt_annual[!Name %in% done$Name,] %>% split(.,.$STAID)

get_data <- function(x,out_path,sig_yr_path,daily_path,Sigs_selected,yearType){
  print(unique(x$Name))

  sigs_all <- fread(paste0(sig_yr_path,unique(x$Name),'.csv')) %>% setDT()
  setnames(sigs_all,'Index','Year')
  
  sigs_name <- names(sigs_all)
  # filter years qualified
  TS_sig <- x[,c(1:3,18:361)] %>% left_join(.,
                                            sigs_all[,c('Year','ncount', intersect( sigs_name, Sigs_selected)) ,with = F]) %>% 
    .[Year >= 1979, c('STAID','Year','ncount',intersect(names(.),Sigs_selected) ),with = F]
 
  Sigs_selected0 <- intersect(names(TS_sig),Sigs_selected)
  
  data_yr <- TS_sig[ncount >= 365*0.95, c('STAID','Year',Sigs_selected0),with = F]
  # 年内cv
  daily_all <- fread(paste0(daily_path,unique(x$Name),'.csv')) %>% setDT()
  
  cv_intra <- daily_all %>% 
    group_by(STAID,Year) %>% 
    summarise(nCVQ = length(Flow_mmd[!is.na(Flow_mmd)]),
              CVQ = ifelse(nCVQ >= 365*0.95,
                           cv(Flow_mmd[!is.na(Flow_mmd)]),
                           NA
                           )  ,
              nCVP = length(P_MSWEP[!is.na(P_MSWEP)]),
              CVP = ifelse(nCVP >= 365*0.95,
                           cv(P_MSWEP[!is.na(P_MSWEP)]),
                           NA
                           ),
              nCVT = length(Temp[!is.na(Temp)]),
              CVT = ifelse(nCVT >= 365*0.95,
                           cv(Temp[!is.na(Temp)]),
                           NA
                           ),
              nCVTmax = length(Tmax[!is.na(Tmax)]),
              CVTmax = ifelse(nCVTmax >= 365*0.95,
                              cv(Tmax[!is.na(Tmax)]),
                              NA
                              ),
              RP = ifelse(nCVP >= 365*0.95 & nCVQ >= 365*0.95,
                           mean(Flow_mmd[!is.na(Flow_mmd)])/mean(P_MSWEP[!is.na(P_MSWEP)]),
                           NA
              ) # 年径流系数
              ) %>% setDT()
  cv_intra$STAID %<>% as.character()
  
  data_yr <- left_join(data_yr,
                       cv_intra[,c('STAID','Year','CVQ','CVP','CVT','CVTmax','RP')])
  
  ## 年尺度各信号的cv值
  CV_dt <- sapply(data_yr[,c(-1,-2)],
                  function (col) {
                    CV =  cv(col[!is.infinite(col)],na.rm = T)
                    Mean =  mean(col[!is.infinite(col)],na.rm = T)
                    Sd = sd(col[!is.infinite(col)],na.rm = T)
                    Count = length(col[!is.infinite(col) & !is.na(col)] )
                    return(data.frame(CV_inter = CV,
                                      Mean_inter = Mean,
                                      Sd_inter = Sd,
                                      Count_inter = Count))}
                  )  %>% t() %>%  as.data.frame() %>%
    mutate(.,
           STAID = unique(data_yr$STAID)
                  ) %>% 
    cbind(.,
          Var = rownames(.),
          yearType = yearType) %>% setDT()

 
  get_Er <- function(P,Q){
    # 计算年际变化率
    delta_P <- diff(P) / head(P, -1)
    delta_Q <- diff(Q) / head(Q, -1)
    
    # 计算降雨径流弹性系数
    lm_result <- lm(delta_Q[!is.infinite(delta_P) & !is.infinite(delta_Q) & !is.na(delta_P) & !is.na(delta_Q)] ~
                      delta_P[!is.infinite(delta_P) & !is.infinite(delta_Q) & !is.na(delta_P) & !is.na(delta_Q)])
    # 提取回归系数（斜率）
    E_R <- coef(lm_result)[2]
  }
  
   ## 多年降雨径流系数 & 多年平均年内cv
  Mean_annual_cv <- data_yr %>% 
    mutate(Q = ifelse(Year %% 4 == 0,
                      366 * Mean,
                      365*Mean
                      )) %>%
    group_by(STAID) %>% setDT() %>% 
    .[!is.na(P_MSWEP) & !is.infinite(P_MSWEP) &!is.na(Q) & !is.infinite(Q), ] %>%
    summarise(RP_inter = mean(Q,na.rm = T)/mean(P_MSWEP,na.rm = T), # 多年平均径流系数
              Er_inter =  get_Er(P = P_MSWEP,
                                 Q = Q), # 降雨径流弹性系数
              ncount = length(Q),
              # CVP_intra = mean(CVP,na.rm = T),
              # CVQ_intra = mean(CVQ,na.rm = T),
              # CVT_intra = mean(CVT,na.rm = T),
              # CVTmax_intra = mean(CVTmax,na.rm = T)
              yearType = yearType
              ) 

  dir.create(paste0(out_path,'01_Er/'),showWarnings = F,recursive = T)
  dir.create(paste0(out_path,'02_CV_each/'),showWarnings = F,recursive = T)
  fwrite(Mean_annual_cv,
         paste0(out_path,'01_Er/',unique(x$Name),'.csv'))
  fwrite(CV_dt,
         paste0(out_path,'02_CV_each/',unique(x$Name),'.csv'))
  
}

# 
# library(parallel)
# ncores <- detectCores(logical=F) # physical cores
# ncores <- min(4,ncores-1)
# cl <- parallel::makeCluster(ncores)

# parLapply(cl,
          lapply(
          dt_ls,
          function(x,out_path,water_yr_out_path,sig_yr_path,water_yr_path,daily_path,Sigs_selected, get_data ){
            if(str_detect(unique(x$Name),'ADHI',negate = T)){
              get_data(x,
                            out_path = water_yr_out_path,
                            sig_yr_path = water_yr_path,
                       daily_path,Sigs_selected,yearType = 'hydro'
              )
            }
            
            
            get_data(x,
                     out_path,
                          sig_yr_path,
                     daily_path,Sigs_selected,yearType = 'calendar'
            )
          },
          out_path,water_yr_out_path,sig_yr_path,water_yr_path,daily_path,Sigs_selected,
          get_data
          
)
# gc()
# #
# stopCluster(cl)
