infos <- fread('F:/0000Data/Global_StreamFlow_V1-4/infos_1_4.csv')
info_new <- fread('F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Global/Code/info_all.csv')
################################################# load Dam files and regulated basins################################################
Dams_all <- fread('F:/0000Data/Global_StreamFlow_V1-4/Dams_Year_Max_Min.csv')
Data <- fread('F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Global/Trend/Sig_raw/Data_Trend.csv')

out <- left_join(unique(unique(Data[,c('STAID')])),
                 infos[,-c( "GDAT" , "GRanD" )]
                 ) %>% left_join(.,
                                 Dams_all) %>% 
  left_join(.,
            info_new[,c('STAID','IfRegulated')]) %>% setDT()
fwrite(out,'Regulated_info.csv')
