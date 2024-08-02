get_cpts <- function(TS_sig, Sigs_selected,
                       NAthresh = 0.95,
                       cptMethod = "changepoint",
                       if_fill = T) {
  TS_sig %<>% setDT()
  get_each <- function(sigsn,
                       TS_sig, NAthresh = 0.95,
                       cptMethod ,if_fill){
    print(paste(sigsn,  cptMethod,'start'))
    
    TS <- TS_sig[,c('Year',sigsn,'ncount'),with = F] %>% setDT()
    setnames(TS,sigsn,'Value')
    
    TS[ is.na(TS$Value) | is.infinite(Value) | ncount < NAthresh*365 ,]$Value <- NA
    
    if(sum(!is.na(TS$Value)) > 5 ){
      # 找到第一个非NA值的索引
    first_non_na <- which(!is.na(TS$Value))[1]
    # 找到最后一个非NA值的索引
    last_non_na <- tail(which(!is.na(TS$Value)), 1)
    # 保留非NA值之间的部分   # 移除首尾的NA值
    df_keep <- TS[first_non_na:last_non_na, ]
    
        if(sum(is.na(df_keep$Value)) == 0){
          # 如果中间无空值  直接使用df_keep
          df_keep$Value_complete <- df_keep$Value
          fill_rate <- 0
          data <- ts(data = df_keep$Value,
                     frequency = 1,
                     start = min(df_keep$Year),
                     end = max(df_keep$Year))
        }else{
           # 有空值则判断是否填充
        ## if not consecutive, first generate only lm sens spearman, 
        ### then fill na to make ts, calculate all
          if(if_fill == T){
            df_keep$Value_complete <- zoo::na.approx(df_keep$Value, na.rm=FALSE) 
            fill_rate <- sum(is.na(df_keep$Value)) / nrow(df_keep)
            data <- ts(data = df_keep$Value_complete,
                       frequency = 1,
                       start = min(df_keep$Year),
                       end = max(df_keep$Year))
          }else{
            fill_rate <- 0
            data <- ts(data = df_keep$Value,
                       frequency = 1,
                       start = min(df_keep$Year),
                       end = max(df_keep$Year))
          }
        }
    
    ## if consecutive, generate a ts and calculate all metrics
    ## only run data analysis if there are more than 5 years in record
    if (length(data) > 5 &!(sum(data) == 0  & sd(data) == 0 )& sum(!data == 0) > 5) {
       ########### 
      #############不同方法计算趋势
       if(cptMethod %in% 'Bayesian'){
        #  1. 进行贝叶斯突变点分析
        bcp_result <- bcp::bcp(data)
        # 汇总突变点概率大的点
        if(length(which(bcp_result$posterior.prob > 0.5)) == 0){
          cpts <- data.table(cpts = time(data)[which(bcp_result$posterior.prob > 0.5)])
        }else{
          cpts <- NULL
        }
        
       }else if(cptMethod %in% c('AMOC','PELT',"SegNeigh" , "BinSeg")){
         # 2. changepoint 包
           out <- tryCatch(suppressWarnings(changepoint::cpt.meanvar(as.numeric(data), 
                                                                     penalty="Asymptotic",
                                                                     pen.value=0.05,
                                                                     method=cptMethod)), 
                           error = function(e) NA)
           
           if (class(out) == "cpt.range") {
             MyCpts <- out@cpts
             cpts <- data.table(cpts = time(data)[MyCpts])
           } else {
             cpts <- NULL
           }
         
      }else if (cptMethod %in% 'RBeast'){
        # 3. Rbeast probable trend changepoints
        Rbeast_result <- Rbeast::beast(data,season = 'none')
        cpts <- Rbeast_result[["trend"]][["cp"]] %>% .[!is.na(.)]
        cpts_pr <- Rbeast_result[["trend"]][["cpPr"]] %>% .[!is.na(.)]

      }else if(cptMethod %in% 'cpm'){
        # 4. cpm changepoint detection
        cpm_result <- cpm::processStream(data, cpmType = "Kolmogorov-Smirnov")
        cpts <- cpm_result$changePoints
        
      }  else if (cptMethod %in% 'Cusum'){
        # 5. cusum changepoint detection
        cusum_result <- qcc::cusum(data)
        # cusum找到的点
       cpts <- time(data)[
         which(cusum_result$pos > cusum_result$decision.interval | cusum_result$neg < -cusum_result$decision.interval)
       ] 
       
      }else if(cptMethod %in% 'mk'){
        ## 5. mk 突变点检验
        mk_res <- trend::mk.test(data)
        cpts <- ifelse(mk_res$p.value < 0.05, which(abs(mk_res[["estimates"]][["S"]]) == max(abs(mk_res[["estimates"]][["S"]]))), NA)  # 如果 p 值显著，返回最大 S 值位置
        
      }else if(cptMethod %in% 'EnvCpt'){
        ## 6. envcpt突变点检验
        cpts <- NULL
        envcpt_res <- tryCatch(suppressWarnings(EnvCpt::envcpt(data)), 
                        error = function(e) NA)
        
        if(is.list(envcpt_res)){
          ## best model
          Best <- which(AIC(envcpt_res) == min(AIC(envcpt_res))) %>% names() 
          Final_cpt <- Best[str_detect(Best,'cpt')]
              if(is_empty(Final_cpt)){
                cpts <- NULL
              }else{
                cpts <-  envcpt_res[[ Final_cpt]] @cpts %>% time(data)[.]
                cptMethod <- paste0('EnvCpt-',Final_cpt)
              }
        }else{
          Final_cpt <- NA
          cpts <- NULL
        }
     
          # which.min(AIC(envcpt_res)) %>% names()
        # for (istr in Best){
        #  a <- envcpt_res[[istr]] 
        #   if (str_detect(istr,'cpt')){
        #     cpts <-  a@cpts
        #   }else if (is.list(a)){
        #     cpts <- NULL
        #   }
        # }
       
        
        
       
       
      }else if(cptMethod %in% 'segmented'){
        ## 7. segmented突变点检验
        data_frame <- data.frame(time = as.numeric(time(data)), y = as.numeric(data))
        linmod <- lm(y ~ time, data = data_frame)
          # if(linmod[["coefficients"]]['time'] == 0){
          #   cpts <- NULL # 拟合时为无趋势的直线
          # }else{
          #   segmod <- segmented::segmented(linmod, seg.Z = ~time)
          #   cpts <- segmod$psi[, "Est."]  # 突变点位置
          # }
        # 检查线性模型的系数
        if (linmod[["coefficients"]]['time'] == 0) {
          cpts <- NULL # 拟合时为无趋势的直线
        } else {
          cpts <- NULL
          # 尝试不指定初始分段点，让模型自行确定
          tryCatch({
            segmod <- segmented::segmented(linmod, seg.Z = ~time)
            cpts <- segmod$psi[, "Est."]  # 突变点位置
            # print(cpts)
          }, error = function(e) {
            message("Found Error in segmented fitting: ", e)
          })
        
        }
      
      } else if(cptMethod %in% 'mcp1'){
        ## 8. mcp 贝叶斯突变点检验
            data_df <- data.frame(time = as.numeric(time(data)), value = as.numeric(data))
                    # 定义模型
            # 模型表示两个阶段，第一阶段 y 是一个常数，第二阶段 y 也是一个常数，但水平不同
            model <- list(
              value ~ 1,          # 第一阶段的模型
              1~1                 # 第二阶段的模型
            )
            
            # 拟合模型
            fit <- mcp::mcp(model, data = data_df,par_x = "time")
            a <- summary(fit) %>% setDT()
            cpts <- a[name %in% 'cp_1',]$mean %>% floor()
      
      }else if(cptMethod %in% 'Strucchange'){
        #  9. 用strucchange分析
          time_series_data <- as.numeric(data)
          bp <- tryCatch(suppressWarnings(strucchange::breakpoints(time_series_data ~ 1)), 
                                 error = function(e) NA)
         
          if(is.list(bp)){
            # 提取突变点位置
            cpts <- bp$breakpoints %>% time(data)[.]
          }else{
            cpts <- NA
          }
          
      }else if(cptMethod %in% 'Kmeans'){
        #  10. Kmeans突变点分析
        kmeans_result <- stats::kmeans(data, centers = 2)
        # 将聚类结果加入数据中
        cluster <- kmeans_result$cluster
        cpts <- (which(diff(cluster) != 0) + 1) %>% time(data)[.]
        
      }else if(cptMethod %in% 'mcp2'){
        ## 11. mcp 贝叶斯突变点检验2个点
        data_df <- data.frame(time = as.numeric(time(data)), value = as.numeric(data))
        # 定义模型
        # 模型表示两个阶段，第一阶段 y 是一个常数，第二阶段 y 也是一个常数，但水平不同
        model <- list(
          value ~ 1,          # 第一阶段的模型
          1~1,                # 第二阶段的模型
          1~1
        )
        
        # 拟合模型
        fit <- mcp::mcp(model, data = data_df,par_x = "time")
        a <- summary(fit) %>% setDT()
        cpts <- a[name %in% c('cp_1','cp_2','cp_3','cp_4','cp_5','cp_6'),]$mean %>% floor()
      }
    
    cpt_out <- data.table(cptMethod = cptMethod) %>% 
        cbind(cpts)%>%  setDT()
    cpt_out %<>% mutate( if_fill = if_fill,
                          fill_rate = fill_rate ,
                          data_length = length(data),
                          Year_Start = time(data)[1],
                          Year_End = time(data)[length(data)],
                          Sigs = sigsn
    )
    cpt_out %<>% setDT()
    
    # 总体删除端点三年内的几个点
    cpt_out <- cpt_out %>% 
      mutate(cpt_filtered =
               ifelse(!is.null(cpts),
                      ifelse(cpts + 3 >= Year_End | cpts - 3 <= Year_Start,
                             NA,
                             unique(cpts)
                      ),
                      NA
                      )
               
             ) %>% setDT()
    # cpt_out$cpt_num <- sum(!is.na(cpt_out$cpt_filtered) & !is.null(cpt_out$cpt_filtered))
    
    print(paste(sigsn,  cptMethod,'done'))
      return(cpt_out)

    } else {
      return(NULL)
      print("Record is not of sufficient length (<5 hyears)")
    }
    }else{
      return(NULL)
      print("Record is not of sufficient length (<5 hyears)")
    }
    
  }
  
  out_ls <- lapply(as.list(Sigs_selected) , 
                   get_each,
                   TS_sig, NAthresh = 0.95,
                   cptMethod ,if_fill
  ) %>% rbindlist(.,use.names = T,fill = T) %>% setDT()
  
  return(out_ls)
}