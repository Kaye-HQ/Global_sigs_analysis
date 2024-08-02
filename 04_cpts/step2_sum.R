rm(list = ls())
path_code_util <- 'H:/0001Work/Global_Streamflow/20240215_tidy/0001_Amount/0000_Code/00000_util/'

Path0 <- 'H:/0001Work/Global_analyze_detect/Result/04_cpt_methods/'
out_path <- paste0(Path0,'02_cpt_summary/')
dir.create(out_path,showWarnings = F,recursive = T)
# 
# cpt_all <- list.files(Path0,
#                         pattern = '.csv',
#                         full.names = T,
#                         recursive = T) %>%
#   lapply(., fread) %>%
#   rbindlist(.,
#             use.names = T,
#             fill = T)
# 
# fwrite(cpt_all,paste0(out_path,'Cpt_all.csv'))

cpt_all <- paste0(out_path,'Cpt_all.csv') %>% fread() %>% 
  mutate(cpt_filtered_final =   ifelse(cpts + 3 >= Year_End | cpts - 3 <= Year_Start,
                                 NA,
                                 floor(cpts)
                                 )
  ) %>% setDT() %>% .[!is.na(cpt_filtered_final),]
# cpt_all$cpt_filtered <- NULL
# cpt_all$cpt_filtered2 <- NULL


cpt_prob <- cpt_all %>% group_by(STAID,Sigs,yearType,cpt_filtered_final)  %>%
  summarise(Fill_rate = unique(fill_rate),
            ncount_method = length(unique(cptMethod))/10,
            methods = paste(unique(cptMethod),collapse = ',')
  ) %<>% setDT()

cpt_prob_final <- cpt_prob[ncount_method > 0.5 & Fill_rate < 0.1,]

cpt_counts <- cpt_prob_final %>% group_by(STAID,Sigs,yearType,methods)  %>%
  summarise(ncount = length(unique(cpt_filtered_final)),
            cpts = paste(unique(cpt_filtered_final),collapse = ','),
            Prob =  paste(unique(ncount_method),collapse = ',')
  )%>% setDT()

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

    cpt_all %<>% mutate(.,
                          p_group = ifelse(is.na(p),
                                           'NA',
                                           ifelse(p > 0.1,'Not Significant',
                                           ifelse(p >0.05, '90%',
                                                  ifelse(p > 0.01,'95%',
                                                         '99%'))
                                           )
                                           )
    )
    
    Stations_draw <- merge(Stations,
                           Trend_all) %>% st_as_sf()
    
    Stations_draw %<>% st_transform(.,crs_robin)
    
    get_each_sig_plot <- function(Sign,Stations_draw,global_land,out_path,TrendType){
      print(paste0(Sign, 'start'))
      
      Stations_draw_final <- Stations_draw[which(Stations_draw$Sigs %in% Sign & !is.na(Stations_draw$Trend)) ,]
      library(rcolors)
      library(scales)
      
      if(TrendType %in% 'trend'){
        Stations_draw_final$Value <- Stations_draw_final$Trend 
      }else if(TrendType %in% 'percent'){
        Stations_draw_final$Value <- Stations_draw_final$Trend_pct_mean
      }
     

      stats <- Stations_draw_final$Value %>% .[!is.na(.)]
      
      if(Sign %in% 'ZFD'){
        limitsn <- c(-1,1)
        breaksn <- c(-0.5,-0.25,-0.1,0,0.1,0.25,0.5)
      }else{
        limitsn <- c(quantile(stats,0.1) - abs(quantile(stats,0.1)) *0.1,
                     quantile(stats,0.9) + abs(quantile(stats,0.9)) *0.1
        )
        #   c(floor(quantile(stats,0.1)*10)/10 -1,
        #              floor(quantile(stats,0.9)*10)/10 + 1 
        # )
        # c((floor(min(stats)/10) ) * 10 ,
        #              (ceiling(max(stats)/10) ) * 10)
        breaksn <- round(quantile(stats,c(0.1,0.25,0.5,0.75,0.9)),3)
        # c(floor(quantile(stats,0.1)*10)/10,
        #            floor(quantile(stats,0.25)*10)/10,
        #            floor(quantile(stats,0.5)*10)/10,
        #            floor(quantile(stats,0.75)*10)/10,
        #            floor(quantile(stats,0.9)*10)/10
        #            )
      }
      
      
      p_station <-  get_robin_plot(r = Stations_draw_final,
                                   sf_fn,
                                   sizen = 0.3,
                                   colorsn = rev(rcolors::get_color("MPL_coolwarm", n = length(breaksn) + 2)) , # ,
                                   limitsn =  limitsn,breaksn =  breaksn,alphan = 1,
                                   Title =  bquote(.(Sign) *' (' * .(TrendType)*' yr'^'-1'*')'),
                                # Title = expression('Runoff Signature Changes'*' (%'*' decade'^'-1'*')'),
                                   DataType = 'Continuous',
                                   VarType = 'VectorPoint') + 
        facet_grid(TrendMethod~yearType)  +
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
      
      dir.create(paste0(out_path,'Trend_spatial/',TrendType,'/'),showWarnings = F,recursive = T)
      ggsave(paste0(out_path,'Trend_spatial/',TrendType,'/',Sign,'.pdf'),p_out,width = 8,height = 10)
    }
    
    lapply(as.list(unique(Stations_draw$Sigs)), 
           get_each_sig_plot,
           Stations_draw[which(!Stations_draw$TrendMethod %in% 'ARIMA'),],global_land,out_path,
           TrendType = 'percent'
           )
    
    lapply(as.list(unique(Stations_draw$Sigs)), 
           get_each_sig_plot,
           Stations_draw[which(!Stations_draw$TrendMethod %in% 'ARIMA'),],global_land,out_path,
           TrendType = 'trend'
    )


