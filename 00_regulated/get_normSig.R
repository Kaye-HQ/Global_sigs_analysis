get_normSig <- function(out_path){
  file_ls_sig_norm <- list.files(paste0(out_path ,'Normalized_Sigs/'),
                                 pattern = '.csv',
                                 full.names = T)
  Sig_all_norm <- lapply(file_ls_sig_norm, fread) %>% rbindlist()
  
 
  Sig_all_norm %<>% left_join(.,
                              data.table(Norm_Index = seq(-90,110,1),
                                         Decade_Index = c(rep(seq(-90,-10,10),each = 10),
                                                          rep(seq(10,110,10),each = 10),
                                                          111
                                         )
                              )
  )

  return(Sig_all_norm)
}
