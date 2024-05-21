rm(list = ls())
out_path <- 'F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Regulated/Paried/'
infos <- fread('F:/0000Data/Global_StreamFlow_V1-4/infos_1_4.csv')
Dams_max <- fread('F:/0000Data/Global_StreamFlow_V1-4/Dams_Year_Max_Min.csv')
info_new <- infos[,-c("GDAT"  ,"GRanD")] %>% left_join(.,
                                Dams_max) %>% setDT()

paired <- fread(paste0(out_path,'paired_one_filetered.csv')) %>% left_join(.,
                                                                           infos[,c('STAID','Name','Lat_centroid')],
                                                                           by = c('Regulated' = 'STAID')) %>% setDT()

signature_path <- 'F:/0000Data/Global_StreamFlow_V1-4/Merge_Data/00_Signatures/Year/'
flow_path <- 'F:/0000Data/Global_StreamFlow_V1-4/Merge_Data/Data_Daily/'

library(showtext)
showtext_opts(dpi = 150)
showtext_auto(enable = TRUE)
Sigs <- c('Max',"Quantile_90" ,"Quantile_10",'Min','LFD','ZFD','DOYMAX','WinSum')

cmp_paried <- function(iseq,paired,info_new,signature_path,flow_path,out_path,Sigs){
  source('get_Winsum.R')  
    info_n <- paired[iseq,] #info_n <- paired[Regulated %in% "USGS_06869500",]
    info_n$STAID <- info_n$Regulated
    Name_regulated <- info_new[STAID %in% info_n$Regulated,]$Name
    Name_Donor <- info_new[STAID %in% info_n$Donor,]$Name
    
    MinY <- info_new[STAID %in% info_n$Regulated,]$Dams_year_Min
    MaxY <- info_new[STAID %in% info_n$Regulated,]$Dams_year_Max
    data <- fread(paste0(signature_path,
                         info_new[STAID %in% info_n$Regulated,]$Name,
                         ".csv")) %>% .[Index <= MinY | Index > MaxY,]
    data <-  get_Winsum(info_n,signature_path,data)
    
    data_flow <- fread(paste0(flow_path,
                              info_new[STAID %in% info_n$Regulated,]$Name,
                              ".csv")) %>% .[Year <= MinY | Year > MaxY,] %>%
      mutate(.,
             Norm_Index = ifelse(Year > MaxY,
                                 Year - MaxY,
                                 Year - MinY
             ),
             Norm_Flow = (Flow_mmd - min(na.omit(Flow_mmd)))/(max(na.omit(Flow_mmd)) - min(na.omit(Flow_mmd)))  )
    
    
    data_Donor <- fread(paste0(signature_path,
                               info_new[STAID %in% info_n$Donor,]$Name,
                         ".csv")) %>% 
                  .[Index <= MinY | Index > MaxY,] 
    data_Donor <-  get_Winsum(info_new[STAID %in% data_Donor$STAID],
                              signature_path,data_Donor)
    
    data_flow_Donor <- fread(paste0(flow_path,
                                    info_new[STAID %in% info_n$Donor,]$Name,
                              ".csv")) %>%  .[Year <= MinY | Year > MaxY,] %>%
      mutate(.,
             Norm_Index = ifelse(Year > MaxY,
                                 Year - MaxY,
                                 Year - MinY
             ),
             Norm_Flow = (Flow_mmd - min(na.omit(Flow_mmd)))/(max(na.omit(Flow_mmd)) - min(na.omit(Flow_mmd)))  )
    setnames(data_flow_Donor,
             c(  "STAID",  "Flow_mmd" , "Norm_Flow"),
             c(  "Donor_STAID", "Donor_Flow_mmd" , "Donor_Norm_Flow"))
   data_flow_out <- left_join(data_flow[,c("Date",   "STAID","Flow_mmd" ,"Norm_Index", "Norm_Flow")],
                              data_flow_Donor[, c("Date",   "Donor_STAID","Donor_Flow_mmd" ,"Norm_Index", "Donor_Norm_Flow")]
                              ) %>% setDT()
    # normalized data sig 
        data_sig <- data %>% melt(.,
                                      id.vars = c("STAID","type" ,"Index", "ncount"),
                                      variable.name = "Signatures",
                                      value.name = "Value"
        ) %>% setDT() %>% group_by(Signatures) %>%
          summarise(.,
                    STAID = STAID[1],
                    type = type[1],
                    ncount = ncount,
                    Index = Index,
                    Norm_Index = ifelse(Index > MaxY,
                                        Index - MaxY,
                                        Index - MinY
                    ),
                    Value = Value,
                    Norm_Sig = (Value - min(na.omit(Value)))/(max(na.omit(Value)) - min(na.omit(Value)))
                    
          ) %>% setDT()
        
        data_sig_Donor <- data_Donor %>% melt(.,
                                              id.vars = c("STAID","type" ,"Index", "ncount"),
                                              variable.name = "Signatures",
                                              value.name = "Value"
        ) %>% setDT() %>% group_by(Signatures) %>%
          summarise(.,
                    Donor_STAID = STAID[1],
                    type = type[1],
                    Donor_ncount = ncount,
                    Index = Index,
                    Norm_Index = ifelse(Index > MaxY,
                                        Index - MaxY,
                                        Index - MinY
                    ),
                    Donor_Value = Value,
                    Donor_Norm_Sig = (Value - min(na.omit(Value)))/(max(na.omit(Value)) - min(na.omit(Value)))
                    
          ) %>% setDT()
        
       data_sig_out <- left_join(data_sig[,c(  "STAID","Signatures" , "ncount" ,"Index","Norm_Index","Value", "Norm_Sig")],
                                   data_sig_Donor[, c(  "Donor_STAID","Signatures" , "Donor_ncount" ,"Index","Norm_Index","Donor_Value", "Donor_Norm_Sig")]
        ) %>% setDT()
     
        
        dir.create( paste0(out_path,'Flow/'),showWarnings = F,recursive = T)
        dir.create( paste0(out_path,'Sig/'),showWarnings = F,recursive = T)
        fwrite(data_flow_out,
               paste0(out_path,'Flow/',Name_regulated,'.csv')
        )
        fwrite(data_sig_out,
               paste0(out_path,'Sig/',Name_regulated,'.csv')
        )
 
        ## Plot   
        data_sig_plot <- data_sig_out[ncount >= 365 *0.95 & Donor_ncount >= 365 *0.95 &
                                    Signatures %in% Sigs,]
        
        if(nrow(data_sig_plot) > 0 & min(data_sig_plot$Norm_Index) < 0 &  max(data_sig_plot$Norm_Index) > 0){
          p_sig <- ggplot(   data_sig_plot,
                             aes(x = Index)) +
            geom_point(size = 0.5,
                       shape = 20,
                       aes(y = Value),
                       color = 'black') +
            geom_point(size = 0.5,
                       shape = 0,
                       aes(y = Donor_Value),
                       color ='blue') +
            geom_vline(xintercept = as.numeric(MinY),
                       color = 'red',
                       size = 0.5,
                       linetype = 'dashed') +
            geom_vline(xintercept = as.numeric(MaxY),
                       color = 'red',
                       size = 0.5,
                       linetype = 'dashed') +
            facet_wrap(Signatures~.,
                       scales = 'free')+
            ggtitle(paste0('Regulated: ',Name_regulated,'    Donor: ',Name_Donor)) +
            theme_bw()+
            theme(strip.background = element_blank())
          
          dir.create( paste0(out_path,'Sig/Plot/'),showWarnings = F,recursive = T)
          ggsave(paste0(out_path,'Sig/Plot/',Name_regulated,'.png'),
                 p_sig,width = 8,height = 4 )

          
          out_simp <- data_sig_out[ncount >= 365 & Donor_ncount >= 365 ,
                               c("Signatures", "STAID" , "Index"  , "Value" ,"Norm_Index", "Norm_Sig",
                                 "Donor_STAID" ,  "Donor_Value" , "Donor_Norm_Sig"
                                 )] %>% setDT()
          dir.create( paste0(out_path,'Normalized_Sigs/'),showWarnings = F,recursive = T)
          fwrite(out_simp,
                 paste0(out_path,'Normalized_Sigs/',Name_regulated,'.csv')
          )
          
        }
        
        
        p <- ggplot(data_flow_out,
                    aes(x = Date)) +
          geom_line(size = 0.5,
                    aes(y = Flow_mmd),
                    color = 'black') +
          geom_line(size = 0.5,
                    aes(y = Donor_Flow_mmd),
                    color = 'blue') +
          geom_vline(xintercept = as.Date(paste0(MinY,'-01-01')),
                     color = 'red',
                     size = 0.5,
                     linetype = 'dashed')+
          geom_vline(xintercept = as.Date(paste0(MaxY,'-01-01')),
                     color = 'red',
                     size = 0.5,
                     linetype = 'dashed')+
          ggtitle(paste0('Regulated: ',Name_regulated,'    Donor: ',Name_Donor)) +
          theme_bw() +
          theme(strip.background = element_blank())
        
        dir.create( paste0(out_path,'Flow/Plot/'),showWarnings = F,recursive = T)
        ggsave(paste0(out_path,'Flow/Plot/',Name_regulated,'.png'),
               p,width = 8,height = 4 )
        
        print(paste(Name_regulated,'done'))
}

library(stringr)
dir.create( paste0(out_path,'Flow/Plot/'),showWarnings = F,recursive = T)
done <- list.files(paste0(out_path,'Flow/Plot/'),
                   full.names = F)  %>% str_remove_all(.,'.png') %>% data.table(Name = .)

seqs <- which(!paired$Regulated %in% done$Name)

lapply(as.list(seqs),
       cmp_paried,
       paired,info_new,signature_path,flow_path,out_path,Sigs
)
