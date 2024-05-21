rm(list = ls())
out_path <- 'F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Regulated/Paried/'
dir.create(out_path,showWarnings = F,recursive = T)

path_mon <- 'F:/0000Data/Global_StreamFlow_V1-4/Merge_Data/Data_Monthly/'
infos <- fread('F:/0000Data/Global_StreamFlow_V1-4/infos_1_4.csv')
Attrs <- fread('F:/0000Data/Global_StreamFlow_V1-4/Attr.csv')
Stations <- st_read('F:/0000Data/Global_StreamFlow_V1-4/Shp/Stations.shp')

Dams_max <- fread('F:/0000Data/Global_StreamFlow_V1-4/Dams_Year_Max_Min.csv')
info_new <- fread('Regulated_info.csv')

selected_sq <-  names(Attrs) %>% .[c(1,14:ncol(Attrs)) ]
Attr_filtered <- info_new %>% left_join(.,
                                        select(Attrs,selected_sq)
                                        )
# Influenced by dams
Attr_filtered <- Attr_filtered[ (GRanD >= 10  | 
                                   GDAT >= 10  |
                                   Dor_pc_pva >= 100)& dLength >= 365*5,] %>% setDT()

Attr_filtered_single <- Attr_filtered[!is.na(Dams_year_Max) & 
                                        Dams_year_Max == Dams_year_Min  &
                                        year(dEnd) > Dams_year_Max   & 
                                        year(dStart) < Dams_year_Min,] 
Attr_filtered_multi <- Attr_filtered[!is.na(Dams_year_Max) & 
                                       Dams_year_Max > Dams_year_Min  & 
                                       year(dEnd) > Dams_year_Max   & 
                                       year(dStart) < Dams_year_Min,] 
Attr_filtered_regulated <- rbind(Attr_filtered_single,Attr_filtered_multi)

Attr_filtered_Natural <- infos %>% left_join(.,
                                                Dams_max ) %>%
                        left_join(.,
                                  select(Attrs,selected_sq)
                        ) %>%
                        setDT() %>% .[(GRanD < 10 | is.na(GRanD)) & 
                                    (GDAT < 10 | is.na(GDAT)) &
                                    (Dor_pc_pva < 100 | is.na(Dor_pc_pva))& dLength >= 365*5,]

# Attr_n <- Attr_filtered_regulated[STAID %in% 'USGS_14148000',]
# Attr_n <- Attr_filtered_regulated[STAID %in% '189-guide',]
find_paired <- function(i,Attr_filtered_regulated,Attr_filtered_Natural,Stations,flow_mon,path_mon){
############################# identification of posible similar donor catchment #####################
  Attr_n <- Attr_filtered_regulated[i,]
  ####### Step 1  Overlap temporal period of year ######
    MinY <- Attr_n$Dams_year_Min
    MaxY <- Attr_n$Dams_year_Max
    Potential1 <- Attr_filtered_Natural[year(dEnd) > MaxY   & 
                                          year(dStart) < MinY & dLength >= 365*5,] 
    ####### Step 2  Start with distance   ###
    Dist_data <- data.table(STAID = Stations[Stations$STAID %in% Potential1$STAID,]$STAID,
                            Dist = as.numeric(st_distance(Stations[Stations$STAID %in% Attr_n$STAID,],
                                                          Stations[Stations$STAID %in% Potential1$STAID,]
                            ) / 1000 )
    )
    Potential1 %<>% left_join(.,Dist_data) %>% setDT()
    units(Potential1$Dist) <- NULL
    Potential2 <- Potential1[Dist < 250 , ] %>% setDT() 
    
        ####### Step 2  check runoff variation in pre influenced period   ###   
     mon_attr <- fread(paste0(path_mon,Attr_n$Name,'.csv')) %>% .[Year <= MinY,]
     if (nrow(Potential2) > 0 & nrow(mon_attr) >= 5*12){
       mon_check  <- lapply(split(Potential2,Potential2$STAID),
                            FUN = function(x){
                              data_mon <- fread(paste0(path_mon,x$Name,'.csv')) %>% 
                                .[Year <= MinY #&
                                    # Year >= min(mon_attr[!is.na(Flow_mm),]$Year) &
                                    # Year <= max(mon_attr[!is.na(Flow_mm),]$Year)
                                  ,] %>% 
                                inner_join(mon_attr[,-c('STAID','Name')],
                                           by = c('Year','Month')) %>% setDT() %>%
                                .[n_flow.x >= 28 & n_flow.y >= 28,]
                              
                              if (nrow(data_mon) >= 5*12){
                                out_mon <- data.table(STAID = x$STAID,
                                                      NSE = xu::NSE(data_mon$Flow_mm.x,data_mon$Flow_mm.y) ,
                                                      KGE = xu::KGE(data_mon$Flow_mm.x,data_mon$Flow_mm.y) ,
                                                      Cor = cor(data_mon$Flow_mm.x,data_mon$Flow_mm.y) ,
                                                      Rsquare = cor(data_mon$Flow_mm.x,data_mon$Flow_mm.y) %>% .^2,
                                                      lm_a = lm(Flow_mm.y~Flow_mm.x,data_mon) %>% coefficients(.) %>% .[2],
                                                      lm_b = lm(Flow_mm.y~Flow_mm.x,data_mon) %>% coefficients(.) %>% .[1],
                                                      lm_p = lm(Flow_mm.y~Flow_mm.x,data_mon) %>% summary(.) %>%  .[['coefficients']] %>% .[2,4],
                                                      lm_r2 = lm(Flow_mm.y~Flow_mm.x,data_mon) %>% summary(.) %>%  .[['r.squared']] ,
                                                      Rsquare_P = cor(data_mon[!is.na(P_MSWEP.x),]$P_MSWEP.x,data_mon[!is.na(P_MSWEP.x),]$P_MSWEP.y) %>% .^2
                                )
                              }else{
                                out_mon <- data.table(STAID = x$STAID,
                                                      NSE = NA,
                                                      KGE = NA,
                                                      Cor = NA,
                                                      Rsquare = NA,
                                                      lm_a = NA,
                                                      lm_b = NA,
                                                      lm_p = NA,
                                                      lm_r2 = NA,
                                                      Rsquare_P = NA)
                                
                              }
                              return(out_mon)
                              
                            }
                            ) %>% rbindlist(.,use.names = T) %>% setDT()
       
       Potential3 <- Potential2[STAID %in% mon_check[Cor >= 0.4 | lm_r2 >= 0.7,]$STAID,] %>% 
         left_join(.,
                   mon_check
                   ) %>% setDT()
       
      
       # dt <- Potential2[,c('pre_mm_s01','pre_mm_s02','pre_mm_s03','pre_mm_s04','pre_mm_s05','pre_mm_s06',
       #                       'pre_mm_s07','pre_mm_s08','pre_mm_s09','pre_mm_s10','pre_mm_s11','pre_mm_s12')] %>%
       #   rbind(Attr_n[,c('pre_mm_s01','pre_mm_s02','pre_mm_s03','pre_mm_s04','pre_mm_s05','pre_mm_s06',
       #                     'pre_mm_s07','pre_mm_s08','pre_mm_s09','pre_mm_s10','pre_mm_s11','pre_mm_s12')],
       #         .) %>% t() %>% as.data.frame()  %>% apply(., 2, as.numeric) 
       # Potential2$run_cor <-  cor(dt[,1] ,
       #                            dt[,2:ncol(dt)],
       #                            method = 'pearson') 
       # Potential2 <- Potential2[run_cor >= 0.85,]
       
       ####### Step 3   mutate other differences for more rigorous rules ( Similar AI, geology and land cover type )....) ######
       
       com_attr <- Potential3 %>% mutate(
         Main_climates,
         diff_area = (abs(Area_shp - Attr_n$Area_shp)/Attr_n$Area_shp),
         diff_pre = (abs(pre_mm_syr - Attr_n$pre_mm_syr)/Attr_n$pre_mm_syr),
         diff_ari = (abs(ari_ix_sav - Attr_n$ari_ix_sav)/Attr_n$ari_ix_sav),
         diff_LAI_GIMMS = (abs(LAI_multiyr_mean - Attr_n$LAI_multiyr_mean)/Attr_n$LAI_multiyr_mean) ,
         diff_cly = (abs(cly_pc_sav - Attr_n$cly_pc_sav)/Attr_n$cly_pc_sav),
         diff_slt = (abs(slt_pc_sav - Attr_n$slt_pc_sav)/Attr_n$slt_pc_sav), 
         diff_snd = (abs(snd_pc_sav - Attr_n$snd_pc_sav)/Attr_n$snd_pc_sav), 
         diff_for = (abs(for_pc_sse - Attr_n$for_pc_sse)/Attr_n$for_pc_sse), 
         diff_pst = (abs(pst_pc_sse - Attr_n$pst_pc_sse)/Attr_n$pst_pc_sse) ,
         diff_crp = (abs(crp_pc_sse - Attr_n$crp_pc_sse)/Attr_n$crp_pc_sse) 
       ) %>% setDT()
       setnames(com_attr,'STAID','Donor')
       
       out_pair_all <- data.table(Regulated = Attr_n$STAID) %>% 
         cbind(.,
               com_attr[,c('Donor','Dist','NSE','KGE','Rsquare','Cor','lm_a','lm_b','lm_p','lm_r2','Rsquare_P')]) %>% 
         left_join(.,
                   com_attr[,c('Donor','diff_area','diff_pre','diff_ari',
                               'diff_LAI_GIMMS','diff_cly','diff_slt','diff_snd',
                               'diff_for','diff_pst','diff_crp')]
         )
       #### Manually check if more than one candidates
       
       
     }else{
       out_pair_all <- NULL
       
     }
    
print(paste(i,Attr_n$Name) )
   
    return(out_pair_all)
}
   
paired_1 <- lapply(seq(1,nrow(Attr_filtered_regulated)) %>% as.list(.),
                 find_paired,
                 Attr_filtered_regulated,
                 Attr_filtered_Natural,Stations,flow_mon,path_mon) 
paired_test <- paired_1[!is.na(paired_1)] %>% rbindlist() %>% .[!is.na(Donor),]
fwrite(paired_test,
       paste0(out_path,'Paired_V1.csv')
       )


 paired_one <- split(paired_test,
                     paired_test$Regulated,) %>%
                  lapply(., function(x){
                    out <- x[ Rsquare < 1,] %>% .[ which.max(Rsquare) ,] 
                  }) %>% rbindlist(.,use.names = T) 
 # paired_one_filetered <-  paired_one[Rsquare >= 0.7 & diff_pre < 0.1,]
 paired_one_filetered <-  paired_one[NSE >= 0.6 & diff_pre < 0.1,]
 fwrite(paired_one_filetered,
        paste0(out_path,'paired_one_filetered.csv')
 )

 Stations <-  paste0('F:/0000Data/Global_StreamFlow_V1-4/Shp/Stations.shp') %>% st_read()
 setnames(Stations,'STAID','Regulated')
 Stations_Regulated <- merge(Stations,paired_one_filetered) %>% filter(.,Regulated %in% paired_one_filetered$Regulated) 
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
   geom_sf(data = Stations_Regulated,
           show.legend = 'point',
           color = 'red',
           size = 1,
           shape = 20)+
   coord_sf(crs = st_crs(4326),
            xlim = c(-180,180),
            ylim = c(-60,90),
            expand = FALSE,
            clip ='on')+
   xlab(NULL)+
   ylab(NULL)+
   theme_bw()+
   theme(legend.position = c(0.1,0.25),
         # legend.direction = 'vertical',
         legend.background = element_blank(),
         panel.grid = element_blank(),
         plot.margin = unit(c(-0.5,0.2,-1.5,0.23),units = 'cm')
   )
 
 ggsave(paste0(out_path,'Sig_Map_Regulated.pdf'),p,width = 6,height = 3)
#  
# # after step 7- quantify
 paired_one_filetered <- fread(paste0(out_path,'paired_one_filetered.csv'))
 selected <- list.files(paste0(out_path,'Quatified/rawData/'),
                        recursive = T,
                        full.names = F
                        )   %>% str_remove(.,'.csv')
 draw_box <- paired_one_filetered[Regulated %in% infos[Name %in%  selected,]$STAID,] %>%
   melt(.,
        id.vars = c('Regulated','Donor'),
        variable.name = 'Vars',
        value.name = 'Value'
        )

 # box plot
 calc_stat <- function(x) {
   n <- sum(!is.na(x))
   # calculate quantiles
   stats <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
   names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
   return(stats)
 }

 p_box <- ggplot(data =  draw_box,
                 aes(y = Value)) +
 stat_summary(fun.data = calc_stat,
              aes(x = Vars),
              geom="boxplot",
              position = position_dodge(0.5),
              width = 0.35 ,
              size = 0.2,
              color = 'black'
 ) +
 stat_summary(fun = "mean",
              aes(x = Vars),
              geom="point",
              position = position_dodge(0.5),
              size = 1,
              color = 'red'
 ) +
 facet_wrap(Vars~.,
            # nrow = 2,
            scales = 'free_y')+
   ylab('Indexes')+
   theme_bw()+
   theme(strip.background = element_blank(),
         panel.grid = element_blank())

 ggsave(paste0(out_path,'paired_evaluate.pdf'), p_box,width = 6,height = 3)


 draw_box_Stat <-  draw_box %>% group_by(Vars) %>%
   summarise(Sig_10 = quantile(Value,0.10,na.rm = T),
             Sig_25 = quantile(Value,0.25,na.rm = T),
             Sig_50 = quantile(Value,0.50,na.rm = T),
             Sig_75 = quantile(Value,0.75,na.rm = T),
             Sig_90 = quantile(Value,0.90,na.rm = T),
             n = length(Value)
   ) %>% setDT()
 fwrite(draw_box_Stat,paste0(out_path,'paired_evaluate.csv'))
#  