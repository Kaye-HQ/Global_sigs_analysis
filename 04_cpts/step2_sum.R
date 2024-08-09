rm(list = ls())
path_code_util <- 'H:/0001Work/Global_Streamflow/20240215_tidy/0001_Amount/0000_Code/00000_util/'

Path0 <- 'H:/0001Work/Global_analyze_detect/Result/04_cpt_methods/'
out_path <- paste0(Path0,'02_cpt_summary/')
dir.create(out_path,showWarnings = F,recursive = T)
out_path_tsplot <- paste0(Path0,'03_cpt_ts/')
dir.create(out_path_tsplot,showWarnings = F,recursive = T)
dir.create(paste0(out_path_tsplot,'pdf/'),showWarnings = F,recursive = T)
dir.create(paste0(out_path_tsplot,'raw/'),showWarnings = F,recursive = T)
dir.create(paste0(out_path_tsplot,'raw_without_segmented/'),showWarnings = F,recursive = T)

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

# cpt_all <- paste0(out_path,'Cpt_all.csv') %>% fread() %>% 
#   mutate(cpt_filtered_final =   ifelse(cpts + 3 >= Year_End | cpts - 3 <= Year_Start,
#                                  NA,
#                                  floor(cpts)
#                                  )
#   ) %>% setDT() %>% .[!is.na(cpt_filtered_final),]
# cpt_all$cpt_filtered <- NULL
# cpt_all$cpt_filtered2 <- NULL

cpt_all <- paste0(out_path,'Cpt_all.csv') %>% fread()
# 
# # cpt_prob <- unique(cpt_all[,c('STAID','Sigs','yearType','cpt_filtered_final','fill_rate','cptMethod')]) %>%
# #   group_by(STAID,Sigs,yearType,cpt_filtered_final)  %>%
# #   summarise(Fill_rate = unique(fill_rate),
# #             ncount_method = (length(unique(cptMethod)))/10,
# #             methods = paste(unique(cptMethod),collapse = ',')
# #   ) %>% setDT()
# # 
# # # 使用未合并数据，查看如果合并了邻近年份时该点概率，结果表明概率本来就小于0.5的点合并邻近后也未大于0.5
# # cpt_prob_final <- cpt_prob[Fill_rate < 0.1,] %>%
# #   group_by(STAID,Sigs,yearType) %>%
# #   mutate(Cpt_all = paste0(unique(cpt_filtered_final),collapse = ',') ) %>%
# #   group_by(STAID,Sigs,yearType,cpt_filtered_final)  %>%
# #   summarise(cpt_nearby = sum(strsplit(Cpt_all,',')[[1]] %in% c( as.character(cpt_filtered_final+1) ,as.character(cpt_filtered_final -1))),
# #            prob_all = ncount_method + cpt_nearby/10
# #             )
# # 
# 
# 
# ## 因此先合并邻近点
# cpt_prob2 <- unique(cpt_all[fill_rate < 0.1,c('STAID','Sigs','yearType','cpt_filtered_final','fill_rate','cptMethod')]) %>%
#   arrange(STAID, Sigs, yearType,cptMethod, cpt_filtered_final) %>% # 先排序
#   group_by(STAID,Sigs,yearType,cptMethod)  %>%
#   mutate(.,group = cumsum(c(TRUE, abs(diff(cpt_filtered_final))  > 1))) %>%
#   group_by(STAID,Sigs,yearType,cptMethod,group)  %>%
#   summarise(cpt_final = min(cpt_filtered_final),
#             Fill_rate = unique(fill_rate),
#             cpt_final_all = paste(unique(cpt_filtered_final),collapse = ','),
#             ncount_method = (length(unique(cptMethod)))/10,
#             methods = paste(unique(cptMethod),collapse = ',')
#   ) %>% setDT()
# 
# fwrite(cpt_prob2,paste0(out_path,'Cpt_merged.csv'))
# 
# cpt_prob_final <- cpt_prob2 %>%
#   group_by(STAID,Sigs,yearType,cpt_final)  %>%
#   summarise(Prob = (length(unique(cptMethod)))/10,
#             methods = paste(unique(cptMethod),collapse = ',')
#             )%>% setDT()
# fwrite(cpt_prob_final,paste0(out_path,'Cpt_prob.csv'))
# 
# cpt_counts2 <- cpt_prob_final[Prob >= 0.5,]  %>% group_by(STAID,Sigs,yearType,methods)  %>%
#   summarise(ncount = length(cpt_final),
#             cpts = paste(cpt_final,collapse = ','),
#             Probs =  paste(Prob,collapse = ',')
#   )%>% setDT()
# fwrite(cpt_counts2,paste0(out_path,'Cpt_counts.csv'))

cpt_counts <- fread(paste0(out_path,'Cpt_counts.csv'))


# load data
Data_path <- 'H:/0001Work/Global_analyze_detect/Data/'

load(paste0(Data_path,'RF_Training_all_trend.RData'))
Trend_train <- Training_all
load(paste0(Data_path,'Training_all_annual_7683.RData'))

dt_annual <- Training_all[STAID %in% Trend_train$STAID,] 
daily_path <- 'H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/Merge_Data/Data_Daily/'
water_yr_path <- 'H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/Merge_Data/00_Signatures/WatYear/'
sig_yr_path <- 'H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/Merge_Data/00_Signatures/Year/'
infos <- fread('H:/00000000_Data/00000000_MergedData/Global_StreamFlow_V1-4/infos_1_4.csv')

plot_each_cpt <- function(x,cpt_all,cpt_counts,out_path_tsplot,sig_yr_path,water_yr_path ){
  
  Sigs_selected <- cpt_counts[STAID%in% x$STAID,]$Sigs %>% unique()
  
  read_data <- function(x,Sigs_selected ,Path){
    sigs_all <- fread(paste0(Path,unique(x$Name),'.csv')) %>% setDT()
    setnames(sigs_all,'Index','Year')
    
    sigs_name <- names(sigs_all)
    TS_sig <- x[,c(1:3,18:361)] %>% left_join(.,
                                              sigs_all[,c('Year','ncount', intersect( sigs_name, Sigs_selected)) ,with = F]) %>% 
      .[Year >= 1979, c('STAID','Year','ncount',intersect(names(.),Sigs_selected) ),with = F]
    
    draw_ts <- TS_sig[ ncount >= 365*0.95,] %>% melt(.,
                               id.vars = c( 'STAID' ,'Year', 'ncount' ),
                               variable.name = 'Sigs',
                               value.name = 'Value'
    ) %>% setDT()
    return(draw_ts)
  }
  
 Sig1 <-  read_data(x,Sigs_selected ,Path = sig_yr_path)
 Sig2 <-  read_data(x,Sigs_selected ,Path = water_yr_path)
 Sig_all <- rbind(cbind(Sig1,
                        yearType = "calendar" ),
                  cbind(Sig2,
                        yearType = "hydro")
                  ) %>% setDT()
  
 cpt_possib <- cpt_all[STAID %in% x$STAID & Sigs %in% Sig_all$Sigs,]
 
 cpt_detected <- cpt_counts[STAID %in% x$STAID & Sigs %in% Sig_all$Sigs,]
 cpt_detected$cpts %<>% as.numeric()
 cpt_detected$Probs %<>% as.numeric()
 
 
 # 函数：插入换行符
 insert_newlines <- function(text, width) {
   str_replace_all(text, ",", "\n")
 }
 
 # 调整 label 数据列，插入换行符
 cpt_detected$label_wrapped <- sapply(cpt_detected$methods, insert_newlines, width = 10)

 
  p <- ggplot( Sig_all,
              aes(x = Year,
                  y = Value,
                  color = yearType)) +
    # geom_vline(data = cpt_possib,
    #            aes(xintercept = cpt_filtered_final,
    #                color = yearType,
    #                linetype = cptMethod),
    #            alpha = 0.2,
    #            size = 1) + 
    geom_vline(data = cpt_detected,
               aes(xintercept = cpts,
                   color = yearType),
               size = 1,
               alpha = 0.5) + 
    # geom_text(data = cpt_detected[yearType %in% 'hydro',],
    #           x = -Inf,
    #           y = -Inf,
    #           aes(label = label_wrapped ,
    #               color = yearType),
    #           hjust = 0.1,
    #           vjust = -1) + 
    geom_line(size = 0.3) + 
    geom_point(size = 0.8,color = 'white') + 
    geom_point(size = 0.5,alpha = 0.8) + 
    ggtitle(unique(x$Name)) +
    facet_wrap(.~Sigs,
               scales = 'free_y') + 
    theme_bw() + 
    theme(strip.background = element_blank(),
          panel.grid = element_blank(),
          text = element_text(size = 15),
          plot.title = element_text(size = 20, face = "bold")
          )
  
  
  p2 <- p + 
    geom_vline(data = cpt_possib,
               aes(xintercept = cpt_filtered_final,
                   color = yearType,
                   linetype = cptMethod),
               alpha = 0.2,
               size = 0.5) +
    facet_wrap(.~Sigs,
               scales = 'free_y') 
  
  p3 <- p + 
    geom_vline(data = cpt_possib[!cptMethod %in% "segmented",],
               aes(xintercept = cpt_filtered_final,
                   color = yearType,
                   linetype = cptMethod),
               alpha = 0.2,
               size = 0.5) +
    facet_wrap(.~Sigs,
               scales = 'free_y')
  
    
  ggsave(paste0(out_path_tsplot,unique(x$Name),'.tiff'),p,
         device = "tiff", width = 6, height = 4, dpi = 300)
  
  ggsave(paste0(out_path_tsplot,'raw/',unique(x$Name),'.tiff'),p2,
         device = "tiff", width = 6, height = 4, dpi = 300)
  
  ggsave(paste0(out_path_tsplot,'raw_without_segmented/',unique(x$Name),'.tiff'),p3,
         device = "tiff", width = 6, height = 4, dpi = 300)

  ggsave(paste0(out_path_tsplot,'pdf/',unique(x$Name),'.pdf'),p,
         width = 8, height = 6)
  
  
  }


done2 <- list.files(paste0(out_path_tsplot,'pdf/'),pattern = '.pdf',full.names = F) %>% 
  str_remove(.,'.pdf') %>%
  data.table(Name = .)
undone_ls <- split(dt_annual[!Name %in% done2$Name & STAID %in% cpt_counts$STAID,],dt_annual[!Name %in% done2$Name & STAID %in% cpt_counts$STAID,]$STAID)

lapply(undone_ls,
       plot_each_cpt,
       cpt_all,cpt_counts,out_path_tsplot,sig_yr_path,water_yr_path 
       )

