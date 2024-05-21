rm(list = ls())
rm(list = ls())
info_new <- fread('Regulated_info.csv')

Attr_filtered <- info_new
# Influenced by dams
Attr_filtered <- Attr_filtered[ (GRanD >= 10  | 
                                   GDAT >= 10  |
                                   Dor_pc_pva >= 100)& dLength >= 365*5,] %>% setDT()

Attr_filtered2 <- Attr_filtered[!is.na(Dams_year_Max) & Dams_year_Max > Dams_year_Min  & year(dEnd) > Dams_year_Max   & year(dStart) < Dams_year_Min,] 

out_path <- 'F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Regulated/MultiDam/'
signature_path <- 'F:/0000Data/Global_StreamFlow_V1-4/Merge_Data/00_Signatures/Year/'
flow_path <- 'F:/0000Data/Global_StreamFlow_V1-4/Merge_Data/Data_Daily/'
Sigs <- c('Max',"Quantile_90" ,"Quantile_10",'Min','LFD','ZFD','DOYMAX','WinSum')

################################################ get_normalized_data ################################################
library(showtext)
showtext_opts(dpi = 150)
showtext_auto(enable = TRUE)

get_normalized_data <- function(iseq,Attr_filtered2,signature_path,flow_path,out_path,Sigs){
  source('get_Winsum.R')
  # test max
  # info_n <- Attr_filtered[GDAT == max(GDAT,na.rm = T)  ,]
  info_n <- Attr_filtered2[iseq,]
  # Res_Data <-  data2[STAID %in% info_n$STAID,]
  data <- fread(paste0(signature_path,
                       info_n$Name,
                       ".csv")) %>% .[Index <= info_n$Dams_year_Min | Index > info_n$Dams_year_Max,]
  data_flow <- fread(paste0(flow_path,
                            info_n$Name,
                            ".csv")) %>% .[Year <= info_n$Dams_year_Min | Year > info_n$Dams_year_Max,]
  
  data <-  get_Winsum(info_n,signature_path,data)
  # normalized_Data
  data_flow %<>% mutate(.,
                        Norm_Index = ifelse(Year > info_n$Dams_year_Max,
                                            Year - info_n$Dams_year_Max,
                                            Year - info_n$Dams_year_Min
                        ),
                        Norm_Flow = (Flow_mmd - min(na.omit(Flow_mmd)))/(max(na.omit(Flow_mmd)) - min(na.omit(Flow_mmd)))  )
  
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
              Norm_Index = ifelse(Index > info_n$Dams_year_Max,
                                  Index - info_n$Dams_year_Max,
                                  Index - info_n$Dams_year_Min
                                  ),
              Value = Value,
              Norm_Sig = (Value - min(na.omit(Value)))/(max(na.omit(Value)) - min(na.omit(Value)))
              
    ) %>% setDT()
  
  dir.create( paste0(out_path,'Flow/'),showWarnings = F,recursive = T)
  dir.create( paste0(out_path,'Sig/'),showWarnings = F,recursive = T)
  fwrite(data_flow,
         paste0(out_path,'Flow/',info_n$Name,'.csv')
  )
  fwrite(data_sig,
         paste0(out_path,'Sig/',info_n$Name,'.csv')
  )
  
  data_sig_plot <- data_sig[ncount >= 365 * 0.95 & 
                              Signatures %in% c("Index",
                                                "HFI","HFI_Q90","HFI_Q99",
                                                "Mean_peaks","Mean_peak_Q99","Mean_peak_Q90",
                                                "Num_peaks","Max_peak",
                                                "LFI"  ,"LFI_Q10"  ,"LFI_Q01"  ,
                                                "Mean_low_Q10","Mean_low_Q01",
                                                "MHFD", "MLFD", "MZFD",
                                                "GINI" ,"FDCs","CI"),]
  
  if(nrow(data_sig_plot) > 0 & min(data_sig_plot$Norm_Index) < 0 &  max(data_sig_plot$Norm_Index) > 0){
    p_sig <- ggplot(   data_sig_plot,
                       
                       aes(x = Index,
                           y = Value)) +
      geom_point(size = 0.5) +
      geom_vline(xintercept = as.numeric(info_n$Dams_year_Min),
                 color = 'red',
                 size = 0.5,
                 linetype = 'dashed') +
      geom_vline(xintercept = as.numeric(info_n$Dams_year_Median),
                 color = 'red',
                 size = 0.5,
                 linetype = 'dashed') +
      geom_vline(xintercept = as.numeric(info_n$Dams_year_Max),
                 color = 'red',
                 size = 0.5,
                 linetype = 'dashed') +
      facet_wrap(Signatures~.,
                 scales = 'free')+
      ggtitle(info_n$Name) +
      theme_bw()+
      theme(strip.background = element_blank())
    
    dir.create( paste0(out_path,'Sig/Plot/'),showWarnings = F,recursive = T)
    ggsave(paste0(out_path,'Sig/Plot/',info_n$Name,'.png'),
           p_sig,width = 8,height = 4 )
    
    
    
    out_simp <- data_sig[ncount >= 365 *0.95 ,
                         c("Signatures", "STAID" , "Norm_Index", "Norm_Sig" )] %>% setDT()
    dir.create( paste0(out_path,'Normalized_Sigs/'),showWarnings = F,recursive = T)
    fwrite(out_simp,
           paste0(out_path,'Normalized_Sigs/',info_n$Name,'.csv')
    )
    
  }
  
  
  p <- ggplot(data_flow,
              aes(x = Date,
                  y = Flow_mmd)) +
    geom_line(size = 0.5) +
    geom_vline(xintercept = as.Date(paste0(info_n$Dams_year_Min,'-01-01')),
               color = 'red',
               size = 0.5,
               linetype = 'dashed')+
    geom_vline(xintercept = as.Date(paste0(info_n$Dams_year_Median,'-01-01')),
               color = 'red',
               size = 0.5,
               linetype = 'dashed')+
    geom_vline(xintercept = as.Date(paste0(info_n$Dams_year_Max,'-01-01')),
               color = 'red',
               size = 0.5,
               linetype = 'dashed')+
    ggtitle(info_n$Name)  +
    theme_bw() +
    theme(strip.background = element_blank())
  
  dir.create( paste0(out_path,'Flow/Plot/'),showWarnings = F,recursive = T)
  ggsave(paste0(out_path,'Flow/Plot/',info_n$Name,'.png'),
         p,width = 8,height = 4 )
  
  print(paste(info_n$Name,'done'))
}


library(stringr)
dir.create( paste0(out_path,'Flow/Plot/'),showWarnings = F,recursive = T)
done <- list.files(paste0(out_path,'Flow/Plot/'),
                   full.names = F)  %>% str_remove_all(.,'.png') %>% data.table(Name = .)

seqs <- which(!Attr_filtered2$Name %in% done$Name)

lapply(as.list(seqs),
       get_normalized_data,
       Attr_filtered2,signature_path,flow_path,out_path,Sigs
)

########################################### Plot all ###################################################


file_ls_sig_norm <- list.files(paste0(out_path ,'Normalized_Sigs/'),
                               pattern = '.csv',
                               full.names = T)
Sig_all_norm <- lapply(file_ls_sig_norm, fread) %>% rbindlist()

Sig_all_norm_stat <- Sig_all_norm %>% group_by(Signatures,Norm_Index) %>% 
  summarise(Sig_10 = quantile(Norm_Sig,0.10,na.rm = T),
            Sig_25 = quantile(Norm_Sig,0.25,na.rm = T),
            Sig_50 = quantile(Norm_Sig,0.50,na.rm = T),
            Sig_75 = quantile(Norm_Sig,0.75,na.rm = T),
            Sig_90 = quantile(Norm_Sig,0.90,na.rm = T),
            n = length(Norm_Sig)
  ) %>% setDT()

Sig_all_norm %<>% left_join(.,
                            data.table(Norm_Index = seq(-90,110,1),
                                       Decade_Index = c(rep(seq(-90,-10,10),each = 10),
                                                        rep(seq(10,110,10),each = 10),
                                                        111
                                       )
                            )
)
Sig_box <- Sig_all_norm %>% group_by(Signatures,Decade_Index) %>%
  summarise(Sig_10 = quantile(Norm_Sig,0.10,na.rm = T),
            Sig_25 = quantile(Norm_Sig,0.25,na.rm = T),
            Sig_50 = quantile(Norm_Sig,0.50,na.rm = T),
            Sig_75 = quantile(Norm_Sig,0.75,na.rm = T),
            Sig_90 = quantile(Norm_Sig,0.90,na.rm = T),
            n = length(Norm_Sig)
  ) %>% setDT()


Sig_box$Signatures  <- factor(Sig_box$Signatures , levels = Sigs)
Sig_all_norm_stat$Signatures  <- factor(Sig_all_norm_stat$Signatures , levels = Sigs)

p_sig_all <- ggplot(Sig_all_norm_stat[n >= 10 & Signatures %in% Sigs ,],
                    aes(x = Norm_Index)) + 
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
            color = 'black')+
  ylab('Normalized Signatures')+
  facet_wrap(Signatures~.,
             nrow = 2)+
  theme_bw()+
  theme(strip.background = element_blank(),
        panel.grid = element_blank())

# box plot
calc_stat <- function(x) {
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(stats)
}

p_box <- ggplot(data = Sig_box[n >= 10 & Signatures %in% Sigs,],
                aes(x = Decade_Index,
                    group = Decade_Index)) + 
  geom_vline(xintercept = 0,
             color = 'yellow',
             #linetype = 'dashed',
             size = 2,
             alpha = 0.3) + 
  geom_errorbar(aes(ymin = Sig_25,
                    ymax = Sig_75)) + 
  geom_point(aes(y = Sig_50),
             color = 'red',
             size = 1)+
  # stat_summary(fun.data = calc_stat,
  #              geom="boxplot",
  #              position = position_dodge(0.5),
  #              width = 0.35 ,
  #              size = 0.2,
  #              color = 'black'
  # ) +
  # stat_summary(fun = "mean",
  #              geom="point",
  #              position = position_dodge(0.5),
  #              size = 1,
#              color = 'red'
# ) +
facet_wrap(Signatures~.,
           nrow = 2)+
  ylab('Normalized Signatures')+
  theme_bw()+
  theme(strip.background = element_blank(),
        panel.grid = element_blank())


# p_sig_all
# p_box

ggsave(paste0(out_path,'Sig_quantile.pdf'),p_sig_all,width = 10,height = 6)
ggsave(paste0(out_path,'Sig_decade.pdf'),p_box,width = 10,height = 6)

Sig_stat <- Sig_all_norm %>% mutate(Stage = ifelse(Decade_Index>=0,
                                                   'post',
                                                   'pre')) %>%
  group_by(Signatures,Stage) %>%
  summarise(Sig_10 = quantile(Norm_Sig,0.10,na.rm = T),
            Sig_25 = quantile(Norm_Sig,0.25,na.rm = T),
            Sig_50 = quantile(Norm_Sig,0.50,na.rm = T),
            Sig_75 = quantile(Norm_Sig,0.75,na.rm = T),
            Sig_90 = quantile(Norm_Sig,0.90,na.rm = T),
            Sig_mean = mean(Norm_Sig,na.rm = T),
            n = length(Norm_Sig)
  ) %>% setDT()
fwrite(Sig_stat,paste0(out_path,'Sig_stat.csv'))
fwrite(Sig_stat[Signatures %in% Sigs,],paste0(out_path,'Sig_stat_Selected.csv'))
########################## plot location map ###################################
Stations <-  paste0('F:/0000Data/Global_StreamFlow_V1-4/Shp/Stations.shp') %>% st_read()
basins <-  paste0('F:/0000Data/Global_StreamFlow_V1-4/Shp/Basins_simp.shp') %>% st_read()
basins$Area <- st_area(basins) / 10^6 / 10^4 # 万平方千米
units(basins$Area) <- NULL
sf::sf_use_s2(FALSE)
Stations_draw <- merge(Stations,info_new) %>% filter(.,STAID %in% Sig_all_norm$STAID) 
basins_draw <- merge(basins,unique(Sig_all_norm[,c('STAID')]))
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
  geom_sf(data = basins_draw,
          show.legend = 'polygon',
          color = 'black',
          aes(fill = Area),
          alpha = 0.6,
          size = 0.1)+
  geom_sf(data = Stations_draw,
          show.legend = 'point',
          color = 'red',
          size = 1,
          shape = 20)+
  coord_sf(crs = st_crs(4326),
           xlim = c(-180,180),
           ylim = c(-60,90),
           expand = FALSE,
           clip ='on')+
  scale_fill_continuous(name = expression('Area (10'^'4'*'km'^'2'*')'))+
  xlab(NULL)+
  ylab(NULL)+
  theme_bw()+
  theme(legend.position = c(0.1,0.25),
        # legend.direction = 'vertical',
        legend.background = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(-0.5,0.2,-1.5,0.23),units = 'cm')
  )

ggsave(paste0(out_path,'./Sig_Map.pdf'),p,width = 6,height = 3)
# 
# library(parallel)
# ncores <- detectCores(logical=F) # physical cores
# ncores <- min(6,ncores-1)
# cl <- parallel::makeCluster(ncores)
# parLapply(cl,
#           as.list(seqs),
#           get_normalized_data,
#           Attr_filtered2
# )
# gc()
# stopCluster(cl)

################################################ read normalized data and plot ################################################
# plot_Normalized <- function()

#  p
#  
#  drawdata <- data[ncount >= 365,c("Index",
#                                   "HFI","HFI_Q90","HFI_Q99",
#                                   "Mean_peaks","Mean_peak_Q99","Mean_peak_Q90",
#                                   "Num_peaks","Max_peak",
#                                   "LFI"  ,"LFI_Q10"  ,"LFI_Q01"  ,
#                                   "Mean_low_Q10","Mean_low_Q01",
#                                    "MHFD", "MLFD", "MZFD",
#                                   "GINI" ,"FDCs","CI" 
#                                   
#                      )] %>% 
#    melt(.,
#         id.vars = "Index",
#         variable.name =   'Signatures',
#         value.name = 'Value')
#  

#  
#  p_sig

