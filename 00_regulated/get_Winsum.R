get_Winsum <- function(info_n,signature_path,data){
  data$STAID %<>% as.character()
  info_n$STAID %<>% as.character()
  info_n %<>% setDT()
  if (file.exists(paste0(str_replace(signature_path,'/Year/','/SeasonsYear/'),
                         info_n$Name,
                         ".csv"))){
    data_season <- fread(paste0(str_replace(signature_path,'/Year/','/SeasonsYear/'),
                                info_n$Name,
                                ".csv"))
    data_season$STAID %<>% as.character()
  }else{
    data_season <- data.table(STAID = NULL)
  }
  
  if (nrow(data) > 0){
    if (nrow(data_season) > 0){
      dtfiltered <- data_season[ncount > 30*3 *0.95,c('STAID','Index','Quantile_05')] %>% 
        mutate(Year = str_sub(Index,1,4),
               Season = str_sub(Index,6,10)) %>% setDT() %>% .[,-c('Index')] %>% 
        rbind(.,
              data.table(
                STAID = info_n$STAID,
                Year = '0',
                Season = c('03_05','06_08','09_11','12_02'),
                Quantile_05 = c(NA,NA,NA,NA)
              ),
              use.names = T,
              fill = T )
      
      if (nrow(dtfiltered) > 0){
        WinSum_info <- dtfiltered %>%
          dcast(.,
                STAID  + Year ~ Season ,
                value.var = 'Quantile_05'
          ) 
        setnames(WinSum_info,
                 c('03_05','06_08','09_11','12_02'),
                 c('S03_05','S06_08','S09_11','S12_02'),
                 skip_absent = T)
        
        WinSum_data <- left_join(WinSum_info,
                                 info_n[,c('STAID','Lat_centroid')]) %>%
          mutate(
            WinSum = ifelse(Lat_centroid > 0 ,
                            S12_02 / S06_08,
                            S06_08 / S12_02 
            )
          ) %>% setDT() %>% .[!Year %in% '0',]
        WinSum_data$Index <- as.numeric(WinSum_data$Year)
        
        data_sig <- data %>% left_join(.,WinSum_data[,c('Index','WinSum')],
                                       by = 'Index') 
      }
      
    }else{
      data_sig <- data
    }
    

  }
  return(data_sig)
}
