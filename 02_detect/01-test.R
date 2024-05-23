rm(list = ls())
path_code_util <- 'H:/0001Work/Global_Streamflow/20240215_tidy/0001_Amount/0000_Code/00000_util/'

out_path <- 'H:/0001Work/Global_analyze_detect/Result/02_detect/test/'
dir.create(out_path,showWarnings = F,recursive = T)
Data_path <- 'H:/0001Work/Global_analyze_detect/Data/'

load(paste0(Data_path,'RF_Training_all_trend.RData'))
Trend_train <- Training_all
load(paste0(Data_path,'Training_all_annual_7683.RData'))

dt_annual <- Training_all[STAID %in% Trend_train$STAID,] 

daily_path <- 'H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/Merge_Data/Data_Daily/'
water_yr_path <- 'H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/Merge_Data/00_WaterYear/'
infos <- fread('H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/infos_1_4.csv')

done <- list.files(out_path,pattern = '.csv',full.names = F) %>% 
  str_remove(.,'.csv') %>%
  data.table(Name = .)

get_flowscreen_cpt <- function(x,daily_path,infos,water_yr_path,out_path){
  print(unique(x$Name))
  
  list.files(path = 'F:/00000Functions/FlowScreen/',
             pattern = '.R$',
             full.names = T) %>% lapply(.,source) 
  list.files(path = 'F:/00000Functions/FlowScreen/',
             pattern = '.r$',
             full.names = T) %>% lapply(.,source) 
  
  # x <- dt_annual[STAID %in% '10002', ] 
  daily_dt <- fread(paste0(daily_path,unique(x$Name),'.csv'))
  daily_dt$ID <- daily_dt$STAID
  daily_dt$SYM <- daily_dt$qc
  daily_dt$Agency <- infos[STAID %in% x$STAID,]$Source 
  
  waterYr <- fread(paste0(water_yr_path,unique(x$Name),'.csv'))
  
  input_dt <- create.ts(daily_dt[!is.na(Flow),c('ID','Date','Flow','SYM','Agency')],
                 hyrstart = waterYr$StartMon)
  
  
  # # plot detail of each basin
  # # screen.summary(metrics.all(cania.sub.ts),type = 'l')
  # 
  res_dt <- metrics.all(input_dt)
  all_dt <- screen.cpts(res_dt,type = 'a')
  out <- cbind(all_dt,STAID = unique(x$STAID))
  fwrite(out,file = paste0(out_path,unique(x$Name),'.csv'))
  
  return(out)
  # # custom variable
  # res0 <- dt_annual[STAID %in% x$STAID,]$Mean
  # attributes(res0) <-  list(times  = dt_annual[STAID %in% x$STAID,]$Year )
  # screen.metric(res0)
  
  
  #' Calculate mean annual minimum n-day flows
  # res <- MAMn(input_dt,n = 1, by = 'hyear')
  # res2 <- screen.metric(res, "Q (m3/s)")

  ##' This function extracts the partial duration series for all streamflow droughts based
  ##' on a moving window quantile threshold. 
  # res1 <- dr.events(input_dt, Qdr=0.2, WinSize=30, IntEventDur=10, EventDur=15)
  # events <- res1$DroughtEvents
  # plot(events$Start, events$Duration, pch=19, ylab="Drought Duration (days)", xlab="")
  gc()
  
  
  
}

test_all <- lapply(split(dt_annual[!Name %in% done$Name,],dt_annual[!Name %in% done$Name,]$STAID),
       get_flowscreen_cpt,
       daily_path,infos,water_yr_path,out_path
       )
# 
# 
# # flow screen
# library(FlowScreen)
# data(caniapiscau)
# res1 <- dr.events(cania.sub.ts)
# events <- res1$DroughtEvents
# plot(events$Start, events$Duration, pch=19, ylab="Drought Duration (days)", xlab="")
