get_robin_plot <- function(r,sf_fn,sizen,colorsn,limitsn,breaksn,alphan,Title,DataType,VarType){
  
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
  
  # crs for the global-scale map 
  crs_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" 
  crs_84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
    # read sf
  global_land <- st_read(sf_fn) 
  st_crs(global_land) <- crs_84
  global_land %<>% st_transform(crs_robin) %>% st_union %>% vect 
  
  # choose geom
    if(VarType %in% 'VectorPoint'){
      
      r <- r %>% sf::st_transform(crs_robin) 
      p0 <- ggplot()+
        geom_spatvector(data = r, aes(color = Value,alpha = p_group),shape = 20,size = sizen)
      
    }else if(VarType %in% 'VectorPolygon'){
      
      r <- r %>% sf::st_transform(crs_robin) 
      p0 <- ggplot()+
        geom_spatvector(data = r, aes(fill= Value,alpha = p_group),color = 'grey80',size = 0.1)
      
    }else if(VarType %in% 'Raster'){
      # read raster and reproject
      r <- r %>% terra::project(crs_robin) 
      p0 <- ggplot()+
        geom_spatraster(data = r, maxcell = 1*10^7,aes(alpha = p_group))
    }
  
  # set basemap and limits
  p1 <- p0 +
    geom_spatvector(data = global_land, fill = NA, size = 0.1) +
    # while for WGS 84
    # scale_y_continuous(expand = c(0,0), limits = c(-60, 90))+
    # scale_x_continuous(expand = c(0,0), limits = c(-180, 180))+
    
    # while for ROBIN
    scale_y_continuous(expand = c(0,0), limits = c(-65*10^5, 86*10^5))+
    scale_x_continuous(expand = c(0,0), limits = c(-180*10^5, 180*10^5))
  
  # choose colorbar
  if(DataType %in% 'Continuous'){
    p_out <- p1 + 
      scale_fill_gradientn(
      colors = colorsn,
      name = Title,
      #     # Further refinements
      limits = limitsn,
      breaks = breaksn,
      labels = breaksn,
      na.value = NA,
      oob = squish,
      guide = guide_colorbar(
        direction = "horizontal",
        nrow = 1,
        title.position = "top",
        barwidth = 20,
        barheight = 0.5,
        ticks.colour = "black",
        ticks.linewidth = 0.3,
        drop = F
      )
    ) + 
      scale_color_gradientn(
        colors = colorsn,
        name = Title,
        #     # Further refinements
        limits = limitsn,
        breaks = breaksn,
        labels = breaksn,
        na.value = NA,
        oob = squish,
        guide = guide_colorbar(
          direction = "horizontal",
          nrow = 1,
          title.position = "top",
          barwidth = 20,
          barheight = 0.5,
          ticks.colour = "black",
          ticks.linewidth = 0.3,
          drop = F
        )
      )
  }else if(DataType %in% 'Discrete'){
    p_out <- p1 +
      scale_color_manual(
        values  = colorsn, # get the same data from
        na.value = NA,
        name = Title,
        drop = F
      ) +
      scale_fill_manual(
        values  = colorsn, # get the same data from
        na.value = NA,
        name = Title,
        drop = F
      ) 
  }
  

  
  p_out <-  p_out+
    theme_void()+
    theme(legend.position = 'bottom')
  return(p_out)
  
}