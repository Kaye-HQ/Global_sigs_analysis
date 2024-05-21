rm(list = ls())
path_code_util <- 'H:/0001Work/Global_Streamflow/20240215_tidy/0001_Amount/0000_Code/00000_util/'

out_path <- 'H:/0001Work/Global_analyze_detect/Result/02_detect/'
dir.create(out_path,showWarnings = F,recursive = T)
Data_path <- 'H:/0001Work/Global_analyze_detect/Data/'

load(paste0(Data_path,'RF_Training_all_trend.RData'))
Trend_train <- Training_all
load(paste0(Data_path,'Training_all_annual_7683.RData'))

dt_annual <- Training_all[STAID %in% Trend_train$STAID,] 

