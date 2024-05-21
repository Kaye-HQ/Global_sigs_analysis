get_changeSig <- function(out_path,temporal_range){
  rm_inf <- function(x){ifelse(is.infinite(x),NA,x)}
  file_ls_sig_norm <- list.files(paste0(out_path ,'Sig/'),
                                 pattern = '.csv',
                                 full.names = T)
  Sig_all <- lapply(file_ls_sig_norm, fread) %>% rbindlist()
  
  
  Sig_all %<>% left_join(.,
                              data.table(Norm_Index = seq(-90,110,1),
                                         Decade_Index = c(rep(seq(-90,-10,10),each = 10),
                                                          rep(seq(10,110,10),each = 10),
                                                          111
                                         )
                              )
  )%>% setDT()
  if (temporal_range %in% 'All'){
    Sig_all <- Sig_all
  }else{
    Sig_all %<>% .[which(abs(Norm_Index) <= temporal_range) ,] 
  }
  # change each year
  Sig_out <-  Sig_all %>% group_by(STAID,Signatures) %>%
            mutate(Index = Index,
                      Norm_Index = Norm_Index,
                      Value = Value,
                      Norm_Sig = Norm_Sig,
                      Decade_Index = Decade_Index,
                      Base_Index = which.min(abs(Norm_Index)),
                      change_abs = Value - Value[Base_Index],
                      change_rel = change_abs / Value[Base_Index] * 100,
                   Stage = ifelse(Norm_Index > 0,'post',
                                  ifelse(Norm_Index < 0,'pre','Base'))
              
            )%>% setDT()
    
  # change stage
  Sig_out_stage <- Sig_out[ncount >=365*0.95 & !Stage %in% 'Base',] %>%
    group_by(STAID,Signatures,Stage) %>% summarise(Mean = rm_inf(mean(na.omit(rm_inf(change_rel)))),
                                             Median = rm_inf(median(na.omit(rm_inf(change_rel)))),
                                             Sd = rm_inf(is.infinite(sd(na.omit(rm_inf(change_rel))))),
                                             n_year = length(na.omit(change_rel))
                                             ) %>% setDT() 
  Sig_out_stage_change_median <- Sig_out_stage %>% 
    dcast(., STAID + Signatures ~ Stage,
          value.var = 'Median' ) %>%
    group_by(STAID,Signatures) %>%
    summarise(State_change_median_rel = rm_inf(post - pre)
    ) %>% setDT() 
  
  Sig_out_stage_change_mean <- Sig_out_stage %>% 
    dcast(., STAID + Signatures ~ Stage,
          value.var = 'Mean' ) %>%
    group_by(STAID,Signatures) %>%
    summarise(State_change_mean_rel = rm_inf(post - pre)
    ) %>% setDT() 
    
    
  return(out <- list(Sig_out = Sig_out,
                     Sig_out_stage = Sig_out_stage,
                     Sig_out_stage_change_median = Sig_out_stage_change_median,
                     Sig_out_stage_change_mean  = Sig_out_stage_change_mean 
                     ))
}
