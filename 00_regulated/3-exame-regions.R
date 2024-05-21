rm(list = ls())
out_path <- 'F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Regulated/Group/'
dir.create(out_path,showWarnings = F,recursive = T)
########## prepare signatures##########
source('get_normSig.R')
Sig_ls_Single <- get_normSig('F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Regulated/SingleDam/')
Sig_ls_Multi <- get_normSig('F:/0001Work/02_Globalstreamflow_update/05_Runoff_Signatures_update/Regulated/MultiDam/')

Sig_norm_stat_all <- cbind(Sig_ls_Single,Source = 'Single') %>% 
  rbind(.,
        cbind(Sig_ls_Multi,Source = 'Multi')  )

Sigs <- c('Max',"Quantile_90" ,"Quantile_10",'Min','LFD','ZFD','DOYMAX','WinSum')
Sig_norm_stat_all <- Sig_norm_stat_all[Signatures %in% Sigs,] 

Sig_norm_stat_all$Signatures  <- factor(Sig_norm_stat_all$Signatures , levels = Sigs)

infos <- fread('F:/0000Data/Global_StreamFlow_V1-4/infos_1_4.csv')
infos$Main_climates <- factor(infos$Main_climates,levels = c('A','B','C','D','E'))
levels(infos$Main_climates) <- c('Equatorial','Arid','WarmTemperate','Snow','Polar')

sig_group <- Sig_norm_stat_all[Signatures %in% Sigs & !is.na(Decade_Index),] %>% left_join(infos[,c('STAID','Main_climates','Continents','Acronym')])

get_grouped <- function (sig_group,Var,basins_group,Sigs,out_path){
  if (basins_group %in% c('Single' ,'Multi' )){
    Sig_group <- sig_group[Source %in% basins_group,]
  }else if (basins_group %in% 'All'){
    Sig_group <- sig_group
  }
  
  Sig_group$Group_var <- select(Sig_group,Var)
  sig_group_stat <- Sig_group %>% group_by(Signatures,Norm_Index,Group_var) %>% 
    summarise(Sig_10 = quantile(Norm_Sig,0.10,na.rm = T),
              Sig_25 = quantile(Norm_Sig,0.25,na.rm = T),
              Sig_50 = quantile(Norm_Sig,0.50,na.rm = T),
              Sig_75 = quantile(Norm_Sig,0.75,na.rm = T),
              Sig_90 = quantile(Norm_Sig,0.90,na.rm = T),
              n = length(Norm_Sig)
    ) %>% setDT()
  
  p_sig_all <- ggplot(sig_group_stat[ n > 10 & Signatures %in% Sigs ,],
                      aes(x = Norm_Index)) + 
    geom_vline(xintercept = 0,
               color = 'yellow',
               # linetype = 'dashed',
               size = 2,
               alpha = 0.5) + 
    # geom_ribbon(aes(ymin = Sig_10,
    #                 ymax = Sig_90,
    #                 fill = Group_var),
    #             color = NA,
    #             alpha = 0.2
    # )+ 
    # geom_ribbon(aes(ymin = Sig_25,
    #                 ymax = Sig_75),
    #             fill = 'red',
    #             color = NA,
    #             alpha = 0.2
  # ) + 
  geom_line(aes(y = Sig_50,
                color = Group_var),
            alpha = 0.5)+
    ylab('Normalized Signatures')+
    ggtitle(paste0(Var,'_',basins_group)) + 
    facet_wrap(Signatures~.,
               nrow = 2)+
    theme_bw()+
    theme(strip.background = element_blank(),
          panel.grid = element_blank())
  
  ggsave(paste0(out_path,'Sig_',Var,'_',basins_group,'.pdf'),p_sig_all,width = 10,height = 6)
  
  
  Sig_stat <- Sig_group %>% mutate(Stage = ifelse(Decade_Index>=0,
                                                     'post',
                                                     'pre')) %>%
    group_by(Signatures,Stage,Group_var) %>%
    summarise(Sig_10 = quantile(Norm_Sig,0.10,na.rm = T) %>% as.numeric(),
              Sig_25 = quantile(Norm_Sig,0.25,na.rm = T) %>% as.numeric() ,
              Sig_50 = quantile(Norm_Sig,0.50,na.rm = T) %>% as.numeric(),
              Sig_75 = quantile(Norm_Sig,0.75,na.rm = T) %>% as.numeric(),
              Sig_90 = quantile(Norm_Sig,0.90,na.rm = T) %>% as.numeric(),
              Sig_mean = mean(Norm_Sig,na.rm = T) %>% as.numeric(),
              n = length(Norm_Sig)
    ) %>% setDT()
  fwrite(Sig_stat,paste0(out_path,'Sig_stat_',basins_group,'_',Var,'.csv'))
  fwrite(Sig_stat[Signatures %in% Sigs,],paste0(out_path,'Sig_stat_Selected',basins_group,'_',Var,'.csv'))
  
  
  Sig_group_draw <-  Sig_group %>% mutate(Stage = ifelse(Decade_Index>=0,
                                      'post',
                                      'pre'))
  Sig_group_draw$Signatures  <- factor(  Sig_group_draw$Signatures , levels = Sigs)
  Sig_group_draw$Stage <- factor(  Sig_group_draw$Stage,levels = c('pre','post'))
  # box plot
  calc_stat <- function(x) {
    n <- sum(!is.na(x))
    # calculate quantiles
    stats <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    return(stats)
  }
  
  p_box <- ggplot(data = Sig_group_draw[Signatures %in% Sigs,],
                  aes(x = Stage,
                      y = Norm_Sig,
                      group = interaction(Stage,Var),
                      )
                  ) + 
    geom_hline(data = Sig_stat[Stage %in% 'pre',],
               aes(yintercept = Sig_50),
               color = 'grey') + 
  stat_summary(fun.data = calc_stat,
               geom="boxplot",
               position = position_dodge(0.5),
               width = 0.35 ,
               size = 0.2,
               color = 'black'
  ) +
  stat_summary(fun = "mean",
               geom="point",
               position = position_dodge(0.5),
               size = 1,
               color = 'red'
  ) +
  stat_summary(fun = "mean",
                 geom="text",
                 aes(label = round(..y.., 2)
                    ),
                 position = position_dodge(0.5),
               vjust = -2,
                 size = 3,
                 color = 'red'
    ) +

  facet_grid(Group_var~Signatures,scales = 'free_y')+
    ylab('Normalized Signatures')+
    ggtitle(paste0(Var,'_',basins_group)) + 
    theme_bw()+
    theme(strip.background = element_blank(),
          panel.grid = element_blank())
  
  ggsave(paste0(out_path,'Sig_box',Var,'_',basins_group,'.pdf'),p_box,width = 10,height = 6)
  
}

get_grouped(sig_group ,Var = 'Continents',basins_group = 'Single',Sigs,out_path)
get_grouped(sig_group ,Var = 'Continents',basins_group = 'Multi',Sigs,out_path)
get_grouped(sig_group ,Var = 'Continents',basins_group = 'All',Sigs,out_path)
get_grouped(sig_group ,Var = 'Main_climates',basins_group = 'Single',Sigs,out_path)
get_grouped(sig_group ,Var = 'Main_climates',basins_group = 'Multi',Sigs,out_path)
get_grouped(sig_group ,Var = 'Main_climates',basins_group = 'All',Sigs,out_path)
get_grouped(sig_group ,Var = 'Acronym',basins_group = 'Single',Sigs,out_path)
get_grouped(sig_group ,Var = 'Acronym',basins_group = 'Multi',Sigs,out_path)
get_grouped(sig_group ,Var = 'Acronym',basins_group = 'All',Sigs,out_path)

