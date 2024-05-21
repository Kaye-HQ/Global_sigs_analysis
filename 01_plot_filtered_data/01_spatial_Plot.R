
rm(list = ls())
path_code_util <- 'H:/0001Work/Global_Streamflow/20240215_tidy/0001_Amount/0000_Code/00000_util/'
source(paste0(path_code_util,'get_robin_plot.R'))
library(terra)
library(tidyterra) # if fail, use pak::pkg_install to install... 
library(ggplot2)
library(rgdal)
library(purrr)
library(RColorBrewer)
library(sf)
library(magrittr)
library(rcolors)
library(cowplot)
library(scales)

out_path <- 'H:/0001Work/Global_analyze_detect/Result/01_plot_filtered_data/'
Data_path <- 'H:/0001Work/Global_analyze_detect/Data/'

load(paste0(Data_path,'RF_Training_all_trend.RData'))
Stations <- st_read('F:/0000Data/Global_StreamFlow_V1-4/Shp/Stations.shp') %>% .[Stations$STAID %in% Training_all$STAID, ]
Basins <- st_read('F:/0000Data/Global_StreamFlow_V1-4/Shp/Basins.shp') %>% .[Basins$STAID %in% Training_all$STAID, ]

    sf_fn <-  'F:/0000Data/BaseFile/natural_earth_vector/110m_physical/ne_110m_land.shp'
    sf_fn_box <-  'F:/0000Data/BaseFile/natural_earth_vector/110m_physical/ne_110m_graticules_all/ne_110m_wgs84_bounding_box.shp'
    
    # crs for the global-scale map 
    crs_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" 
    crs_84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    # read sf
    
    global_land <- st_read(sf_fn) 
    st_crs(global_land) <- crs_84
    global_land %<>% st_transform(crs_robin) %>% st_union %>% vect 
    
    # read and reproject outside box
    com_ext <- extent(-180, 180,  -59, 88)
    # read and reproject outside box, robinson
    bbox <- readOGR(sf_fn_box)
    bbox <- crop(bbox, com_ext)  # Set smaller extent, excl. Antarctica
    bbox_robin <- spTransform(bbox, CRS("+proj=robin"))  # reproject bounding box
    bbox_robin_df <- fortify(bbox_robin)
    
    Stations_robin <- st_transform(Stations,crs_robin)
    Basins_robin <- st_transform(Basins,crs_robin)

    p <- ggplot()+
      geom_spatvector(data =  Basins_robin,fill= NA,color = 'grey80',size = 0.02,alpha = 0.8) + 
      geom_spatvector(data =  Stations_robin, aes(color = 'red'),shape = 20,alpha = 1,size = 0.8) + 
      geom_spatvector(data = global_land, fill = NA, size = 0.1) +
      geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
      # while for ROBIN
      scale_y_continuous(expand = c(0,0), limits = c(-65*10^5, 86*10^5))+
      scale_x_continuous(expand = c(0,0), limits = c(-180*10^5, 180*10^5))+
      theme_void()+
      theme(legend.position = 'none')
    
    ggsave(paste0(out_path,'study_area.pdf'),p,width = 8,height = 4)
    
    
    bin_width <- 1
    p2 <- ggplot(Training_all,
                 aes(x = Year_count,
                     y = ..count..)) + 
      stat_bin(binwidth=bin_width, 
               geom="bar", 
               colour="black", 
               fill = NA,
               size = 0.5,
               boundary = 0, 
               aes(y=(..count..))) + 
      stat_bin(binwidth = bin_width, 
               geom="line", 
               colour="black", 
               size = 1,
               boundary = 0, 
               aes(y=(..count..))) + 
      stat_bin(binwidth=bin_width, 
               geom="point", 
               colour="black", 
               size = 1.5,
               boundary = 0, 
               aes(y=(..count..))) + 
      stat_bin(binwidth=bin_width, 
               geom="text", 
               colour="grey80", 
               size=3.5, 
               boundary = 0, 
               aes(label=..count.., 
                   y=0.5*(..count..))) +
      coord_cartesian(expand = F)+
      scale_x_continuous(breaks=seq(0,max(Training_all$Year_count/365+bin_width), bin_width))+
      scale_y_continuous(limits = c(0,700)) + 
      theme_bw()+
      xlab('Data Length(year)')+
      ylab('Gauge Number')+
      theme(legend.position = c(0.8,0.85),
            legend.direction = 'horizontal',
            legend.background = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank()
      )
    ggsave(paste0(out_path,'Yr_hist.pdf'),p2,width = 6,height = 3)
    
    
    # 
    # # get area hist pdf
    get_hist_area <- function(input){
      area  <- data.table(area  = input)
      area$Group <- cut(input,
                        c(0,1,5,10,50,100,250,500),
                        include.lowest = T,
                        dig.lab = 10)
      mm <- area  %>%
        dplyr::group_by(Group) %>%
        dplyr::summarise(n = length(area))
      return(mm)
    }
    
    mm <- get_hist_area(Training_all$Area/10^4)
    
    p3 <- ggplot(data = mm,
                 aes(x = Group,
                     y = n) ) +
      geom_bar(stat = "identity",
               width = 0.5,
               position = position_dodge(),
               fill = 'steelblue',
               alpha = 0.6) +
      geom_text(aes(label = n),
                vjust = -0.5,
                color = "black",
                position = position_dodge(0.5),
                size = 4)+
      ylim(c(0,max(mm$n)*1.1))+
      coord_cartesian(expand = F)+
      theme_bw()+
      xlab(expression('Area (10'^'4'*'km'^'2'*')'))+
      ylab('Gauge Number')+
      theme(legend.position = c(0.8,0.85),
            legend.direction = 'horizontal',
            legend.background = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank()
      )
    ggsave(paste0(out_path,'Area_hist_V2.pdf'),p3,width = 6,height = 3)
    
    
    myPalette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")))
    Stations_robin %<>% left_join(.,Training_all[,c('STAID','dStart','dEnd')])
    p4 <- ggplot() +
      geom_spatvector(data =  Stations_robin, aes(color = year(dStart)),shape = 20,alpha = 1,size = 0.8) + 
      geom_spatvector(data = global_land, fill = NA, size = 0.1) +
      geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
      scale_colour_gradientn(name ="Start Year",
                             colours = myPalette(100), 
                             limits=range(year(Stations_robin$dStart)))+
      # while for ROBIN
      scale_y_continuous(expand = c(0,0), limits = c(-65*10^5, 86*10^5))+
      scale_x_continuous(expand = c(0,0), limits = c(-180*10^5, 180*10^5))+
      theme_void()+
      theme(legend.position = c(0.16,0.26))
    
 
    ggsave(paste0(out_path,'Map_basin_dStart.pdf'),p4,width = 8,height = 4)  
    
    p5 <-ggplot() +
      geom_spatvector(data =  Stations_robin, aes(color = year(dEnd)),shape = 20,alpha = 1,size = 0.8) + 
      geom_spatvector(data = global_land, fill = NA, size = 0.1) +
      geom_path(data=bbox_robin_df, aes(long, lat, group=group), color="black", size=0.2) +
      scale_colour_gradientn(name ="End Year",
                             colours = myPalette(100), 
                             limits=range(year(Stations_robin$dEnd)))+
      # while for ROBIN
      scale_y_continuous(expand = c(0,0), limits = c(-65*10^5, 86*10^5))+
      scale_x_continuous(expand = c(0,0), limits = c(-180*10^5, 180*10^5))+
      theme_void()+
      theme(legend.position =  c(0.16,0.26))
    
    
    ggsave(paste0(out_path,'Map_basin_dEnd.pdf'),p5,width = 8,height = 4)  
    