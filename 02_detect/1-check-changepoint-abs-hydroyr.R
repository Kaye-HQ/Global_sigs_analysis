rm(list = ls())
path_code_util <- 'H:/0001Work/Global_Streamflow/20240215_tidy/0001_Amount/0000_Code/00000_util/'

out_path <- 'H:/0001Work/Global_analyze_detect/Result/02_detect/01_each_sig_hydroyr/'
dir.create(out_path,showWarnings = F,recursive = T)
dir.create(paste0(out_path,'cal_statistics/'),showWarnings = F,recursive = T)
dir.create(paste0(out_path,'cpts/'),showWarnings = F,recursive = T)


Sigs_selected <- c("BFI_Eckhardt", "BFI_Chapman_maxwell",  "BFI_Chapman"  ,  
                   "BFI_Lynehollick",  "BFI_UKIH"  ,
                   "Quantile_90"   , "Quantile_10" ,   
                   "Quantile_95"   , "Quantile_05" ,
                   "Quantile_99"   , "Quantile_01" ,
                   'Mean','QT1', 'QT3', 'IQR'   ,   'FDCs', 'CI',
                   "LFD"     , 'HFD' ,  "ZFD"    ,'MLFD','MHFD',
                   "DOYMAX", "P_MSWEP", "Temp"  ,"Tmax" ,  "Tmin" ,'LAI_yr_GLASS'       )

Data_path <- 'H:/0001Work/Global_analyze_detect/Data/'

load(paste0(Data_path,'RF_Training_all_trend.RData'))
Trend_train <- Training_all
load(paste0(Data_path,'Training_all_annual_7683.RData'))

dt_annual <- Training_all[STAID %in% Trend_train$STAID,] 

daily_path <- 'H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/Merge_Data/Data_Daily/'
water_yr_path <- 'H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/Merge_Data/00_Signatures/WatYear/'
infos <- fread('H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/infos_1_4.csv')

done <- list.files(paste0(out_path,'cpts/'),pattern = '.csv',full.names = F) %>% 
  str_remove(.,'.csv') %>%
  data.table(Name = .)

get_flowscreen_cpt <- function(x,out_path,sig_yr_path,Sigs_selected){
  source('F:/00000Functions/FunctionsR/TScpt/metrics.all.custom.R')
  print(unique(x$Name))
  ### x <- dt_annual[Name %in% "CAMELS-BR_10500000", ]
  sigs_all <- fread(paste0(sig_yr_path,unique(x$Name),'.csv')) %>% setDT()
  setnames(sigs_all,'Index','Year')
  
  sigs_name <- names(sigs_all)
  # filter years qualified
  TS_sig <- x[,c(1:3,18:361)] %>% left_join(.,
                                            sigs_all[,c('Year','ncount', intersect( sigs_name, Sigs_selected)) ,with = F]) %>% 
    .[, c('STAID','Year','ncount',intersect(names(.),Sigs_selected) ),with = F]
  
  methods_cpt <- c("AMOC", "PELT", "SegNeigh", "BinSeg")
  methods_trend <- c("yuepilon","zhang")
  Sigs_selected0 <- intersect(names(TS_sig),Sigs_selected)
  
  out_sig_detect1 <- metrics.all.custom(TS_sig, Sigs_selected0,NAthresh = 0.95,
                                        TrendMethod = methods_trend[1], CPTMethod = methods_cpt[1]) 
  out_sig_detect2 <- metrics.all.custom(TS_sig, Sigs_selected0,NAthresh = 0.95,
                                        TrendMethod = methods_trend[1], CPTMethod = methods_cpt[2]) 
  out_sig_detect3 <- metrics.all.custom(TS_sig, Sigs_selected0,NAthresh = 0.95,
                                        TrendMethod = methods_trend[1], CPTMethod = methods_cpt[3]) 
  out_sig_detect4 <- metrics.all.custom(TS_sig, Sigs_selected0,NAthresh = 0.95,
                                        TrendMethod = methods_trend[1], CPTMethod = methods_cpt[4]) 
  out_sig_detect5 <- metrics.all.custom(TS_sig, Sigs_selected0,NAthresh = 0.95,
                                        TrendMethod = methods_trend[2], CPTMethod = methods_cpt[1]) 
  out_sig_detect6 <- metrics.all.custom(TS_sig, Sigs_selected0,NAthresh = 0.95,
                                        TrendMethod = methods_trend[2], CPTMethod = methods_cpt[2]) 
  out_sig_detect7 <- metrics.all.custom(TS_sig, Sigs_selected0,NAthresh = 0.95,
                                        TrendMethod = methods_trend[2], CPTMethod = methods_cpt[3]) 
  out_sig_detect8 <- metrics.all.custom(TS_sig, Sigs_selected0,NAthresh = 0.95,
                                        TrendMethod = methods_trend[2], CPTMethod = methods_cpt[4]) 
  
  out_sig_detect <- c(out_sig_detect1,out_sig_detect2,out_sig_detect3,out_sig_detect4,
                      out_sig_detect5,out_sig_detect6,out_sig_detect7,out_sig_detect8)
  
  cpt_dt <- lapply(out_sig_detect, 
                   function(x){
                     if(length(x)  == 3){
                       out<-  data.table(Sig = x[["tcpRes"]][["MetricName"]],
                                         cptMethod = x$tcpRes$CPTMethod,
                                         TrendMethod = x$tcpRes$TrendMethod,
                                         cpts = paste(attr(x$tcpRes$cpts, 'times'),collapse = ';') ,
                                         cpts_filtered = paste(attr(x$tcpRes$cpts_filtered, 'times'),collapse = ';') 
                                         
                       )
                       
                       return(out)
                       print(x[["tcpRes"]][["MetricName"]])
                     }else{
                       return(NULL)
                     }
                     
                   }
  ) %>% rbindlist(.,use.names = T,fill = T) %>% cbind(.,
                                                      STAID= unique(TS_sig$STAID),
                                                      type = 'hydro')
  
  save(file = paste0(out_path,'cal_statistics/',unique(x$Name),'.RData'),'out_sig_detect')
  fwrite(cpt_dt,paste0(out_path,'cpts/',unique(x$Name),'.csv'))
  
}

test_all <- lapply(split(dt_annual[!Name %in% done$Name & str_detect(Name,'ADHI',negate = T),],dt_annual[!Name %in% done$Name & str_detect(Name,'ADHI',negate = T),]$STAID),
                   get_flowscreen_cpt,out_path,
                   sig_yr_path = water_yr_path,Sigs_selected
)

