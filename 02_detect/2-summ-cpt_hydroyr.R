rm(list = ls())

# out_path1 <- 'H:/0001Work/Global_analyze_detect/Result/02_detect/01_each_sig/'
# cpt_all1 <- list.files(paste0(out_path1,'cpts/'),
#                       pattern = '.csv',
#                       full.names = T
#                       ) %>% as.list() %>% lapply(.,fread) %>% rbindlist(.,use.names = T)
# out_path2 <- 'H:/0001Work/Global_analyze_detect/Result/02_detect/01_each_sig_hydroyr/'
# cpt_all2 <- list.files(paste0(out_path2,'cpts/'),
#                        pattern = '.csv',
#                        full.names = T
# ) %>% as.list() %>% lapply(.,fread) %>% rbindlist(.,use.names = T)
# 
# cpt_filtered <- rbind(cpt_all1,cpt_all2) %>% setDT() %>%
#   .[!is.na(cpts_filtered) & ! cpts_filtered %in% "" & TrendMethod %in% 'yuepilon', ] %>%
#   dcast(STAID + cptMethod  + Sig ~ type ,
#         value.var = 'cpts_filtered') %>% setDT()

base_path <- 'H:/0001Work/Global_analyze_detect/Result/02_detect/'

in_path <- paste0(base_path,'01_each_sig_hydroyr/cal_statistics/')
info_path <- paste0(base_path,'01_each_sig_hydroyr/cpts/')
out_path  <- paste0(base_path,'01_each_sig_hydroyr/')
dir.create(paste0(out_path,'plot_each/'),showWarnings = F,recursive = T)
dir.create(paste0(out_path,'cpts_year_each/'),showWarnings = F,recursive = T)

file_ls <- list.files(in_path ,
                      pattern = '.RData',
                      full.names = F) 

plot_each <- function(file,out_path,info_path){
  SName <- str_replace( basename(file),'.RData','')
  load(file)
  infos <- fread(paste0(info_path,  SName,'.csv')) %>% 
    cbind(.,
          IDls = seq(1,nrow(.))) %>% setDT() %>% .[!is.na(cpts_filtered) & ! cpts_filtered %in% "" ,]
  
  if(nrow(infos) > 0){
    sigs_all <- infos$IDls %>% out_sig_detect[.] %>% lapply(.,
                                                            function(x){
                                                              if(length(x)  == 3){
                                                                out <- x[["metricTS"]] %>% 
                                                                  cbind(.,Sig = x[["tcpRes"]][["MetricName"]]) %>% 
                                                                  setDT()
                                                              }else{out <- NULL}
                                                              return(out)
                                                            }) %>% rbindlist() %>% unique()
    
    dt_cpt <-  infos %>% split(.$IDls) %>% 
      lapply(.,function(x){
        out_cpt <- str_split(x$cpts_filtered,';') %>% as.data.table() %>% 
          cbind(Sig = x$Sig) %>% right_join(x,.) %>% setDT() %>% .[,-c('cpts_filtered','cpts')]
        setnames(out_cpt,'V1','Year_cpt')
        out_cpt$Year_cpt %<>% as.numeric()
        return(out_cpt)
      }) %>% rbindlist()
    
    dt_cpt_summ <- dt_cpt %>% group_by(STAID,type,Sig,Year_cpt) %>%
      summarise(.,
                cpt_count = length(Sig)) %>% setDT()
    dt_cpt_summ$Year <- dt_cpt_summ$Year_cpt
    
    fwrite(dt_cpt_summ,paste0(out_path,'cpts_year_each/',SName,'.csv'))
    
    draw_Data <- sigs_all %>% left_join(., dt_cpt_summ)
    
    # 定义函数计算每段的均值
    calculate_segment_means <- function(data, dt_cpt_summ) {
      cpts <- dt_cpt_summ[Sig %in% unique(data$Sig),]$Year
      change_points <- which(data$Year %in% cpts)
      # 在突变点位置上添加起始点和终止点
      segments <- c(0, change_points, nrow(data)+1)
      
      # 计算每段的均值
      segment_means <- lapply(1:(length(segments) - 1), function(i) {
        out <- data.table(
          # Year = data$Year[(segments[i]+1 ):segments[i+1 ]- 1],
          Start = data$Year[(segments[i]+1 )]-0.5,
          End = data$Year[segments[i+1 ] -1]+0.5,
          Mean = mean(data$Value[(segments[i] + 1 ):segments[i+1 ]- 1])
        )
        return(out)
      }) %>% rbindlist(.) %>% cbind(Sig = unique(data$Sig))
      
      
      return(segment_means)
    }
    
    # 计算每段的均值
    hline_mean <- sigs_all  %>% split(.,.$Sig) %>% 
      lapply(.,
             calculate_segment_means, 
             dt_cpt_summ
      ) %>% rbindlist(.)
    
    
    
    
    p <- ggplot(draw_Data) + 
      geom_vline(aes(xintercept  = Year_cpt),
                 color = 'red',
                 linetype = 'dashed') +
      geom_text(aes(x  = Year_cpt,
                    y = 0,
                    label = cpt_count),
                vjust = -5,
                hjust = 1.5,
                color = 'red') +
      geom_segment(data = hline_mean,
                   aes(y = Mean,
                       yend = Mean,
                       x = Start,
                       xend = End)) +
      geom_point(aes(x = Year,
                     y = Value),
                 color = 'darkblue')+
      facet_wrap(~Sig,
                 scales = 'free') +
      xlab('Year')+
      ylab(NULL) + 
      ggtitle(SName) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        text = element_text(size = 15),
        plot.title = element_text(size = 20, face = "bold")
      ) 
    
    ggsave(paste0(out_path,'plot_each/',SName,'.tiff'),p,
           device = "tiff", width = 6, height = 4, dpi = 300)
  }
  
  
  
  
  
}

Done <- list.files(paste0(out_path,'plot_each/'),
                   full.names = F)

Undone <- file_ls[!str_remove(file_ls,'.RData') %in% str_remove(Done ,'.tiff') ]

lapply(as.list(paste0(in_path,Undone )),
       plot_each,out_path,info_path
)
