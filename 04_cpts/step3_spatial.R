
out_path <- paste0(Path0,'02_cpt_summary/')


cpt_counts <- fread(paste0(out_path,'Cpt_counts.csv'))
cpt_counts$cpts %<>% as.numeric() 
## 径流信号趋势全球分布图
source('../0000_util/get_robin_plot.R')
    # prepare sf, transform data coordinates
    library(terra)
    library(tidyterra)
    crs_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
    crs_84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


    Stations <-  paste0('F:/0000Data/Global_StreamFlow_V1-4/Shp/Stations.shp') %>% st_read()
    # world <- map_data("world")
    sf_fn <-  'F:/0000Data/BaseFile/Global_land/ne_110m_land.shp'

    Stations_draw <- merge(Stations,
                           cpt_counts) %>% st_as_sf()
    Stations_draw$Value <- Stations_draw$cpts
    Stations_draw$p_group <- Stations_draw$Probs

    Stations_draw %<>% st_transform(.,crs_robin)

    get_each_sig_plot <- function(Sign,Stations_draw,global_land,out_path){
      print(paste0(Sign, 'start'))

      Stations_draw_final <- Stations_draw[which(Stations_draw$Sigs %in% Sign ) ,]
      library(rcolors)
      library(scales)

      stats <- Stations_draw_final$Value %>% .[!is.na(.)]
        limitsn <- c(1980,2015)
        breaksn <-  seq(1980,2015,5)
    
      p_station <-  get_robin_plot(r = Stations_draw_final,
                                   sf_fn,
                                   sizen = 0.3,
                                   colorsn = rev(rcolors::get_color("MPL_coolwarm", n = length(breaksn) + 2)) , # ,
                                   limitsn =  limitsn,breaksn =  breaksn,alphan = 1,
                                   Title =  bquote(.(Sign) *' (' *' yr'^'-1'*')'),
                                # Title = expression('Runoff Signature Changes'*' (%'*' decade'^'-1'*')'),
                                   DataType = 'Continuous',
                                   VarType = 'VectorPoint') +
        facet_grid(.~yearType)  +
        scale_alpha_manual(values = c("Not Significant" = 0.5,
                                      "NA" = 0.5,
                                      "90%" = 0.8,
                                      "95%" = 0.9,
                                      "99%"  = 1   )) +
        guides(alpha = 'none',
               color = guide_colorbar()
               )+
        theme(strip.text.y = element_text(angle = -90))



      p_out <- p_station

      dir.create(paste0(out_path,'cpt_spatial/'),showWarnings = F,recursive = T)
      ggsave(paste0(out_path,'cpt_spatial/',Sign,'.pdf'),p_out,width = 8,height = 3)
    }


    lapply(as.list(unique(Stations_draw$Sigs)),
           get_each_sig_plot,
           Stations_draw,global_land,out_path
    )
    