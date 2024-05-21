rm(list = ls())
OutPath <- 'F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Regulated/Paried/Quatified/'
SigPath <- 'F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Regulated/Paried/Normalized_Sigs/' 

Sigs <- c('Max',"Quantile_90" ,"Quantile_10",'Min','LFD','ZFD','DOYMAX','WinSum')

file_ls <- list.files(path =  SigPath,
                       pattern = '.csv',
                       full.names = T,
                       recursive = T)  %>% as.list()

cmp_sig <- function(ifile,file_ls,OutPath){
  Sigs <- fread(file_ls[[ifile]])
  Namen <- basename(file_ls[[ifile]]) %>% str_remove(.,'.csv')
  
  Sigs_cmp <- Sigs %>% mutate(
    Diff_Value = Value - Donor_Value,
    Diff_Value_pct = (Value - Donor_Value)/Donor_Value,
    Diff_Norm_Value = Norm_Sig - Donor_Norm_Sig
  ) %>% setDT()
  
  Sigs_cmp_info <- Sigs_cmp %>% mutate(Stage = ifelse(
                                                    Norm_Index <= 0,
                                                    'Pre',
                                                    'Post'
                                                  )) %>%
                     group_by(STAID,Signatures,Stage) %>% summarise(
                       n = length(na.omit(Value)),
                       Donor_n = length(na.omit(Value)),
                       Value =  ifelse(is.infinite(mean(na.omit(Value))),NA,mean(na.omit(Value))),
                       Donor_Value =  ifelse(is.infinite(mean(na.omit(Donor_Value))),NA,mean(na.omit(Donor_Value))),
                       Change_Sig = Value - Donor_Value,
                       Change_Sig_pct = ifelse(is.infinite(((Value - Donor_Value)/ Donor_Value * 100)),NA,((Value - Donor_Value)/ Donor_Value * 100)),# for post period
                       Bias_Value = (Donor_Value - Value)/Value, # for pre period
                       Norm_Sig = ifelse(is.infinite(mean(na.omit(Norm_Sig))),NA,mean(na.omit(Norm_Sig))),
                       Donor_Norm_Sig = ifelse(is.infinite(mean(na.omit(Donor_Norm_Sig))),NA,mean(na.omit(Donor_Norm_Sig))),
                       Change_Norm_Sig = Norm_Sig - Donor_Norm_Sig,
                       Diff_Value_sd = sd(na.omit(Diff_Value)), 
                       Diff_Value = ifelse(is.infinite(mean(na.omit(Diff_Value))),NA,mean(na.omit(Diff_Value))),
                       Diff_Value_median = ifelse(is.infinite(median(na.omit(Diff_Value))),NA,median(na.omit(Diff_Value))),
                       Diff_Norm_Value_sd = sd(na.omit(Diff_Norm_Value)),
                       Diff_Norm_Value = ifelse(is.infinite(mean(na.omit(Diff_Norm_Value))),NA,mean(na.omit(Diff_Norm_Value)))
                     )
  dir.create( paste0(OutPath,'rawData/'),showWarnings = F,recursive = T)
  fwrite(Sigs_cmp,
         paste0(OutPath,'rawData/',Namen,'.csv')
  )
  dir.create( paste0(OutPath,'meanData/'),showWarnings = F,recursive = T)
  fwrite(Sigs_cmp_info,
         paste0(OutPath,'meanData/',Namen,'.csv')
  )
  
  # how to remove bias of pre influenced period?

  
  
}

lapply(as.list(seq(1,length(file_ls))),
       cmp_sig,
       file_ls,OutPath
       )

data_sig <- list.files(paste0(OutPath,'meanData/'),
                       recursive = T,
                       full.names = T
                       )  %>% as.list() %>% 
            lapply(.,fread) %>% rbindlist(.) %>% setDT(.)


data_draw <- data_sig[Signatures %in% Sigs & Stage %in% 'Post', ]
data_draw$draw_value <- data_draw$Change_Sig_pct
# data_draw$draw_value <- data_draw$Change_Sig

Sig_change_summary <- data_draw %>% group_by(Signatures) %>% 
  summarise(Mean = ifelse(is.infinite(mean(na.omit(draw_value))),NA,mean(na.omit(draw_value))),
            Median = ifelse(is.infinite(median(na.omit(draw_value))),NA,median(na.omit(draw_value))),
            Sd = ifelse(is.infinite(sd(na.omit(draw_value))),NA,sd(na.omit(draw_value))),
            Min = ifelse(is.infinite(min(na.omit(draw_value))),NA,min(na.omit(draw_value))),
            Max = ifelse(is.infinite(max(na.omit(draw_value))),NA,max(na.omit(draw_value))),
            n_stations = length(na.omit(draw_value))
  )   %>% setDT()

    data_draw$Signatures  <- factor(data_draw$Signatures , levels = Sigs)
    levels(data_draw$Signatures ) <- paste0('△',Sigs,'(%)')
   
    seqs <- c(min(data_draw$draw_value,na.rm = T),-50,-10,-1,1,10,50,max(data_draw$State_change_median_rel,na.rm = T))
    data_draw$Group_draw_value <- cut(data_draw$draw_value,
                                            seqs,
                                            right = T,
                                            ordered_results = T)
    
    data_draw %<>% setDT()
    
    Stations <-  paste0('F:/0000Data/Global_StreamFlow_V1-4/Shp/Stations.shp') %>% st_read()
    Stations_draw <- inner_join(Stations,data_draw )  %>% st_as_sf()
    
 
    Text <- Sig_change_summary %>% group_by(Signatures) %>% 
      summarise(Value = paste0(round(Median,0) ,'±',round(Sd,0)))
    Text$Signatures  <- factor(  Text$Signatures , levels = Sigs)
    levels(  Text$Signatures ) <- paste0('△',Sigs,'(%)')
    
world <- map_data("world")
p <- ggplot() +
  geom_map(data = world,
           map = world,
           aes(long, 
               lat, 
               map_id = region),
           fill = 'grey90',
           color = 'grey90',
           lwd = 0.01
  )+
  geom_sf(data = Stations_draw,
          aes(color = Group_draw_value ),
          show.legend = 'point',
          size = 1,
          shape = 20)+
  geom_text(
    data = Text,
    aes(x = -Inf,
        y = +Inf,
        label = Value
    ),
    vjust = 10.5,
    hjust = -0.15,
    color = 'black',
    show.legend = F
  )+
  coord_sf(crs = st_crs(4326),
           xlim = c(-180,180),
           ylim = c(-60,90),
           expand = FALSE,
           clip ='on')+
  scale_color_brewer(palette = "RdBu")+
  guides(colour = guide_legend(nrow = 3))+
  facet_wrap(.~Signatures) +  
  xlab(NULL)+
  ylab(NULL)+
  ggtitle('Post Change_Sig_pct')+
  theme_bw()+
  theme(legend.position = c(0.78,0.1),
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank()
  )

ggsave(paste0(OutPath,'Sig_change.pdf'),p,width = 12,height = 5)
fwrite(Sig_change_summary,
       paste0(OutPath,'Sig_change.csv')
       )


# ribbon
Sig_group <- list.files(paste0(OutPath,'rawData/'),
                       recursive = T,
                       full.names = T
)  %>% as.list() %>% 
  lapply(.,fread) %>% rbindlist(.) %>% setDT(.)
############# plot time series
    rm_inf <- function(x){ifelse(is.infinite(x),NA,x)}
    sig_ts_stat <- Sig_group %>% group_by(Signatures,Norm_Index) %>% 
      summarise(Sig_10 = quantile(Norm_Sig,0.10,na.rm = T) %>% rm_inf,
                Sig_25 = quantile(Norm_Sig,0.25,na.rm = T) %>% rm_inf,
                Sig_50 = quantile(Norm_Sig,0.50,na.rm = T) %>% rm_inf,
                Sig_75 = quantile(Norm_Sig,0.75,na.rm = T) %>% rm_inf,
                Sig_90 = quantile(Norm_Sig,0.90,na.rm = T) %>% rm_inf,
                n = length(na.omit(Norm_Sig)),
                Sig_10_Donor = quantile(Donor_Norm_Sig,0.10,na.rm = T) %>% rm_inf,
                Sig_25_Donor = quantile(Donor_Norm_Sig,0.25,na.rm = T) %>% rm_inf,
                Sig_50_Donor = quantile(Donor_Norm_Sig,0.50,na.rm = T) %>% rm_inf,
                Sig_75_Donor = quantile(Donor_Norm_Sig,0.75,na.rm = T) %>% rm_inf,
                Sig_90_Donor = quantile(Donor_Norm_Sig,0.90,na.rm = T) %>% rm_inf,
                n_Donor = length(na.omit(Donor_Norm_Sig))
      ) %>% setDT()

    p_sig_ts <- ggplot( sig_ts_stat[ n > 10 &Signatures %in% Sigs ,],
                        aes(x = Norm_Index)) + 
      geom_vline(xintercept = 0,
                 color = 'yellow',
                 # linetype = 'dashed',
                 size = 2,
                 alpha = 0.5) + 
      # geom_ribbon(aes(ymin = Sig_10,
      #                 ymax = Sig_90),
      #             fill = 'red',
      #             color = NA,
      #             alpha = 0.2
      # )+
      # geom_ribbon(aes(ymin = Sig_25,
      #                 ymax = Sig_75),
      #             fill = 'red',
      #             color = NA,
      #             alpha = 0.2
      # ) +
      geom_line(aes(y = Sig_50),
                # alpha = 0.5,
                color = 'red')+
      # geom_ribbon(aes(ymin = Sig_10_Donor,
      #                 ymax = Sig_90_Donor),
      #             fill = 'blue',
      #             color = NA,
      #             alpha = 0.2
      # )+
      # geom_ribbon(aes(ymin = Sig_25_Donor,
      #                 ymax = Sig_75_Donor),
      #             fill = 'blue',
      #             color = NA,
      #             alpha = 0.2
      # ) +
      geom_line(aes(y = Sig_50_Donor),
                # alpha = 0.5,
                color = 'blue')+
      ylab('Normalized Signatures')+ 
      facet_wrap(Signatures~.,
                 nrow = 2,
                 scales = 'free_y')+
      theme_bw()+
      theme(strip.background = element_blank(),
            panel.grid = element_blank())
    
    ggsave(paste0(OutPath,'Sig_temporal_rm_ribbon','.pdf'), p_sig_ts,width = 10,height = 6)


############ plot change
rm_inf <- function(x){ifelse(is.infinite(x),NA,x)}
Sig_group$Signatures  <- factor(Sig_group$Signatures , levels = Sigs)
levels(Sig_group$Signatures ) <- paste0('△',Sigs)

Sig_group$draw_Value <- Sig_group$Diff_Value
sig_group_stat <- Sig_group %>% group_by(Signatures,Norm_Index) %>% 
  summarise(Sig_10 = quantile(draw_Value,0.10,na.rm = T) %>% rm_inf,
            Sig_25 = quantile(draw_Value,0.25,na.rm = T) %>% rm_inf,
            Sig_50 = quantile(draw_Value,0.50,na.rm = T) %>% rm_inf,
            Sig_75 = quantile(draw_Value,0.75,na.rm = T) %>% rm_inf,
            Sig_90 = quantile(draw_Value,0.90,na.rm = T) %>% rm_inf,
            n = length(na.omit(draw_Value))
  ) %>% setDT()

p_sig_all <- ggplot(sig_group_stat[ n > 10 & Signatures %in%  paste0('△',Sigs) ,],
                    aes(x = Norm_Index)) + 
  geom_hline(yintercept = 0,
             color = 'grey',
             # linetype = 'dashed',
             size = 0.5,
             alpha = 0.5) + 
  geom_vline(xintercept = 0,
             color = 'yellow',
             # linetype = 'dashed',
             size = 2,
             alpha = 0.5) + 
geom_ribbon(aes(ymin = Sig_10,
                ymax = Sig_90),
            fill = 'red',
            color = NA,
            alpha = 0.2
)+
geom_ribbon(aes(ymin = Sig_25,
                ymax = Sig_75),
            fill = 'red',
            color = NA,
            alpha = 0.2
) +
geom_line(aes(y = Sig_50),
          alpha = 0.5)+
  ylab('Normalized Signatures')+ 
  ggtitle('Sig_Regulated - Sig_Donor') + 
  facet_wrap(Signatures~.,
             nrow = 2,
             scales = 'free_y')+
  theme_bw()+
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggsave(paste0(OutPath,'delt_Sig_summary','.pdf'),p_sig_all,width = 10,height = 6)


