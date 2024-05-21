rm(list = ls())
out_path <- 'F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Regulated/Group/Map/'
dir.create(out_path,showWarnings = F,recursive = T)
dir.create(paste0(out_path,'Summary/'),showWarnings = F,recursive = T)

get_temporal_cmp <- function(temporal_range,out_path){
  ########## prepare signatures##########
  source('get_changeSig.R')
  Sig_ls_Single <- get_changeSig('../SingleDam/',temporal_range)
  Sig_ls_Multi <- get_changeSig('../MultiDam/',temporal_range)
  
  Sig_stage_all <- cbind(Sig_ls_Single[[2]],Source = 'Single') %>% 
    rbind(.,
          cbind(Sig_ls_Multi[[2]],Source = 'Multi')  )
  fwrite(Sig_stage_all,paste0(out_path,'Summary/Stage_',temporal_range,'.csv'))
  
  # Median change Result
  Sig_stat_all <- cbind(Sig_ls_Single[[3]],Source = 'Single') %>% 
    rbind(.,
          cbind(Sig_ls_Multi[[3]],Source = 'Multi')  ) %>% .[!is.na(State_change_median_rel),]
  
  Sigs <- c('Max',"Quantile_90" ,"Quantile_10",'Min','LFD','ZFD','DOYMAX','WinSum')
  Sig_stat_all <- Sig_stat_all[Sig_stat_all$Signatures %in% Sigs,]
  Sig_stat_all$Signatures  <- factor(Sig_stat_all$Signatures , levels = Sigs)
  levels(Sig_stat_all$Signatures ) <- paste0('△',Sigs,'(%)')
  
  seqs <- c(min(Sig_stat_all$State_change_median_rel,na.rm = T),-200,-100,-50,-10,-1,1,10,50,100,200,max(Sig_stat_all$State_change_median_rel,na.rm = T))
  Sig_stat_all$Group_median_change <- cut(Sig_stat_all$State_change_median_rel,
                                        seqs,
                                        right = T,
                                        ordered_results = T)
  
    Sig_stat_all %<>% setDT()
    
    Sig_stat_all_summary <- Sig_stat_all %>% group_by(Signatures) %>% 
      summarise(Mean = ifelse(is.infinite(mean(na.omit(State_change_median_rel))),NA,mean(na.omit(State_change_median_rel))),
                Median = ifelse(is.infinite(median(na.omit(State_change_median_rel))),NA,median(na.omit(State_change_median_rel))),
                Sd = ifelse(is.infinite(sd(na.omit(State_change_median_rel))),NA,sd(na.omit(State_change_median_rel))),
                Min = ifelse(is.infinite(min(na.omit(State_change_median_rel))),NA,min(na.omit(State_change_median_rel))),
                Max = ifelse(is.infinite(max(na.omit(State_change_median_rel))),NA,max(na.omit(State_change_median_rel))),
                n_stations = length(na.omit(State_change_median_rel))
                ) %>% mutate(temporal_range = temporal_range)  %>% setDT()
    fwrite(Sig_stat_all_summary,paste0(out_path,'Summary/Stage_change_',temporal_range,'.csv'))
    
  Stations <-  paste0('F:/0000Data/Global_StreamFlow_V1-4/Shp/Stations.shp') %>% st_read() %>% .[.$STAID %in% Sig_stat_all$STAID,]
  Stations_draw <- merge(Stations,Sig_stat_all)  
  # Stations_draw$Signatures  <- factor(Stations_draw$Signatures , levels = Sigs)
  world <- map_data("world")
  
  # Sig_stat_all_summary$Signatures  <- factor(Sig_stat_all_summary$Signatures , levels = Sigs)
  
  Text <- Sig_stat_all_summary %>% group_by(Signatures) %>% 
    summarise(Value = paste0(round(Median,0) ,'±',round(Sd,0))) #%>% 
    # mutate(Vertical_adj = rep(12,length(unique(Signatures))),
    #        Horizontal_adj = 10
    #        )
  
  get_spatial_change <- function(type_n,Satations_draw,world,out_path,Text,temporal_range){
    
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
              aes_string(color = type_n),
              show.legend = 'point',
              size = 0.5,
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
      guides(colour = guide_legend(nrow = 4,
                                   title = ''))+
      facet_wrap(.~Signatures) +  
      ggtitle(paste0(type_n,'_',temporal_range)) +
      xlab(NULL)+
      ylab(NULL)+
      theme_bw()+
      theme(legend.position = c(0.85,0.1),
            legend.direction = 'horizontal',
            legend.background = element_blank(),
            panel.grid = element_blank(),
            strip.background = element_blank()
      )
    
    ggsave(paste0(out_path,type_n,'_',temporal_range,'.pdf'),p,width = 11,height = 5)
  }
  
 # get_spatial_change(type_n = 'Group_mean_change',Satations_draw,world,out_path,temporal_range)
  get_spatial_change(type_n = 'Group_median_change',Satations_draw,world,out_path,Text,temporal_range)
}

get_temporal_cmp(temporal_range = 'All',out_path)
# get_temporal_cmp(temporal_range = 5,out_path)
# get_temporal_cmp(temporal_range = 10,out_path)
# get_temporal_cmp(temporal_range = 20,out_path)
# get_temporal_cmp(temporal_range = 50,out_path)
# get_temporal_cmp(temporal_range = 80,out_path)
# get_temporal_cmp(temporal_range = 100,out_path)