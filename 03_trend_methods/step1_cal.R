rm(list = ls())
path_code_util <- 'H:/0001Work/Global_Streamflow/20240215_tidy/0001_Amount/0000_Code/00000_util/'

Path0 <- 'H:/0001Work/Global_analyze_detect/Result/03_trend_methods/'
  
out_path <- paste0(Path0,'01_each_sig/')
dir.create(paste0(out_path,'trends/'),showWarnings = F,recursive = T)

water_yr_out_path <- paste0(Path0,'01_each_sig_hydro_yr/')
dir.create(paste0(water_yr_out_path,'trends/'),showWarnings = F,recursive = T)

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


get_flow_trend <- function(x,out_path,sig_yr_path,Sigs_selected,yearType){
  source('H:/0001Work/Global_analyze_detect/Code/Global_sigs_analysis/03_trend_methods/functions/get_trends.R')
  print(unique(x$Name))
  ### x <- dt_annual[Name %in% "CAMELS-BR_10500000", ]
  sigs_all <- fread(paste0(sig_yr_path,unique(x$Name),'.csv')) %>% setDT()
  setnames(sigs_all,'Index','Year')
  
  sigs_name <- names(sigs_all)
  # filter years qualified
  TS_sig <- x[,c(1:3,18:361)] %>% left_join(.,
                                            sigs_all[,c('Year','ncount', intersect( sigs_name, Sigs_selected)) ,with = F]) %>% 
    .[Year >= 1979, c('STAID','Year','ncount',intersect(names(.),Sigs_selected) ),with = F]
  
  methods_trend <- c("lm","spearman","zyp","mov_avg","loess","sens","mk","ARIMA")
  Sigs_selected0 <- intersect(names(TS_sig),Sigs_selected)
  
  out_all <- lapply(as.list(methods_trend ), 
         function(TrendMethod,TS_sig, Sigs_selected){
          out <-  get_trends(TS_sig, Sigs_selected, NAthresh = 0.95,
                    TrendMethod , if_fill = T)
         },TS_sig, Sigs_selected = Sigs_selected0
         ) %>% rbindlist(.,
                         use.names = T,
                         fill = T) %>% setDT()
  
  out_all %<>% mutate(STAID = unique(x$STAID),
                      yearType = yearType)
  
  fwrite(out_all,paste0(out_path,'trends/',unique(x$Name),'.csv'))
  
}



done2 <- list.files(paste0(out_path,'trends/'),pattern = '.csv',full.names = F) %>% 
  str_remove(.,'.csv') %>%
  data.table(Name = .)

library(parallel)
ncores <- detectCores(logical=F) # physical cores
ncores <- min(4,ncores-1)
cl <- parallel::makeCluster(ncores)

parLapply(cl,
          split(dt_annual[!Name %in% done2$Name,],dt_annual[!Name %in% done2$Name,]$STAID),
          function(x,out_path,water_yr_out_path,sig_yr_path,water_yr_path,Sigs_selected, get_flow_trend ){
            if(str_detect(unique(x$Name),'ADHI',negate = T)){
              get_flow_trend(x,
                             out_path = water_yr_out_path,
                             sig_yr_path = water_yr_path,Sigs_selected,yearType = 'hydro'
              )
            }
            
            
            get_flow_trend(x,
                           out_path,
                   sig_yr_path,Sigs_selected,yearType = 'calendar'
            )
          },
          out_path,water_yr_out_path,sig_yr_path,water_yr_path,Sigs_selected  , 
          get_flow_trend
          
)
gc()

stopCluster(cl)
          

