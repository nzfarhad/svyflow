library(dplyr)
#single select analyzer
single_select <- function(df, ques, disag, level){
  
  if (all(is.na(df[[ques]]))) {
    prop <- data.frame(row.names = 1)
    prop$Var1 <- NA
    prop$Freq <- NA
    prop$aggregation_method <- "perc"
    prop$variable <- ques
    prop$count <- NA
    prop$valid <- sum(!is.na(df[[ques]]))
    prop$disaggregation <- disag
    prop$disagg_level <- level
    return(prop) 
  }
  
  else{
    
    cnt <- table(df[[ques]])
    prop <- round(prop.table(table(df[[ques]])) * 100 , 1)
    cnt <- as.data.frame(cnt)
    prop <- as.data.frame(prop)
    
    prop$aggregation_method <- "perc"
    prop$variable <- ques
    prop$count <- cnt$Freq
    prop$valid <- sum(!is.na(df[[ques]]))
    prop$disaggregation <- disag
    prop$disagg_level <- level
    return(prop) 
  }
}


#multiselect analyzer
multi_select <- function(df, ques, disag, level){
  
  if (all(is.na(df[[ques]]))) {
    prop <- data.frame(row.names = 1)
    prop$Var1 <- NA
    prop$Freq <- NA
    prop$aggregation_method <- "perc"
    prop$variable <- ques
    prop$count <- NA
    prop$valid <- sum(!is.na(df[[ques]]))
    prop$disaggregation <- disag
    prop$disagg_level <- level
    return(prop) 
    
  }
  
  else{
    
    # vec <- stringr::str_split(df[[ques]],pattern = " ") %>% reshape2::melt()
    # vec <- stringr::str_split(df[[ques]],pattern = " & ") %>% reshape2::melt()
    vec <- stringr::str_split(df[[ques]],pattern = multi_response_sep) %>% reshape2::melt()
    # prop <- round((table(vec$value) / nrow(df)) * 100, 1)
    prop <- round((table(vec$value) / nrow(df[!is.na(df[[ques]]), ])) * 100, 1)
    cnt <- table(vec$value)
    cnt <- as.data.frame(cnt)
    prop <- as.data.frame(prop)
    prop$aggregation_method <- "perc"
    prop$variable <- ques
    prop$count <- cnt$Freq
    prop$valid <- sum(!is.na(df[[ques]]))
    prop$disaggregation <- disag
    prop$disagg_level <- level
    return(prop)
    
  }
  
}

# mean
stat_mean <- function(df,ques, disag, level){
  st_mean <- round(mean(as.numeric(df[[ques]]), na.rm = T),1)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- st_mean
  res$aggregation_method <- "mean"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  return(res)  
}

# median
stat_median <- function(df, ques, disag, level){
  st_median <- round(median(as.numeric(df[[ques]]), na.rm = T), 1)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- st_median
  res$aggregation_method <- "median"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  return(res)  
}

# sum
stat_sum <- function(df, ques, disag, level){
  st_sum <- sum(as.numeric(df[[ques]]), na.rm = T)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- st_sum
  res$aggregation_method <- "sum"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  return(res)  
}

# first qu
stat_1stq <- function(df, ques, disag, level){
  first_quart <- round(quantile(as.numeric(df[[ques]]), na.rm = T)[2],1)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- first_quart
  res$aggregation_method <- "1st_Qu"
  res$variable <- ques
  res$count <-sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  return(res)  
}

# 3rd qu
stat_3rdq <- function(df, ques, disag, level){
  third_quart <- round(quantile(as.numeric(df[[ques]]), na.rm = T)[4],1)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- third_quart
  res$aggregation_method <- "3rd_Qu"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  return(res)  
}

# min
stat_min <- function(df, ques, disag, level){
  st_min <- min(as.numeric(df[[ques]]), na.rm = T)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- st_min
  res$aggregation_method <- "min"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  return(res)  
}

# max
stat_max <- function(df, ques, disag, level){
  st_max <- max(as.numeric(df[[ques]]), na.rm = T)
  res <- data.frame(row.names = 1)
  res$Var1 <- NA
  res$Freq <- st_max
  res$aggregation_method <- "max"
  res$variable <- ques
  res$count <- sum(!is.na(df[[ques]]))
  res$valid <- sum(!is.na(df[[ques]]))
  res$disaggregation <- disag
  res$disagg_level <- level
  return(res)  
}


analyze <- function(df, analysis_plan){
  
  # res_list <- list()
  ap_len <- nrow(analysis_plan)
  prog_counter <- 1/ap_len
  res_list <- vector(mode = "list", ap_len)
  
  for (i in 1:ap_len) {
    ap_var <- analysis_plan$variable[i]
    ap_kobo_type <- analysis_plan$kobo_type[i]
    ap_agr_type <- analysis_plan$aggregation_method[i]
    ap_disagg <- analysis_plan$disaggregation[i]
    
    cat(paste(ap_var,  "-", "Done\n"))
    
    if(ap_disagg == "all" | is.na(ap_disagg)){
      
      if(ap_kobo_type == "select_one"){
        temp <-  single_select(df, ap_var, ap_disagg, ap_disagg )
        res_list[[i]] <- temp
        
      }
      
      if (ap_kobo_type == "select_multiple") {
        multi_temp <- multi_select(df, ap_var, ap_disagg, ap_disagg)
        res_list[[i]] <- multi_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "mean"){
        mean_temp <- stat_mean(df, ap_var, ap_disagg, ap_disagg)
        res_list[[i]] <- mean_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "median"){
        med_temp <- stat_median(df, ap_var, ap_disagg, ap_disagg)
        res_list[[i]] <- med_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "sum"){
        sum_temp <- stat_sum(df, ap_var, ap_disagg, ap_disagg)
        res_list[[i]] <- sum_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "firstq"){
        first_temp <- stat_1stq(df, ap_var, ap_disagg, ap_disagg)
        res_list[[i]] <- first_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "thirdq"){
        third_temp <- stat_3rdq(df, ap_var, ap_disagg, ap_disagg)
        res_list[[i]] <- third_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "min"){
        min_temp <- stat_min(df, ap_var, ap_disagg, ap_disagg)
        res_list[[i]] <- min_temp
      }
      
      if(ap_kobo_type == "integer" & ap_agr_type == "max"){
        max_temp <- stat_max(df, ap_var, ap_disagg, ap_disagg)
        res_list[[i]] <- max_temp
      }
      
    }
    
    
    if(ap_disagg != "all"){
      
      temp_result <- vector("list", length(unique(df[[ap_disagg]])))
      j = 0
      
      for (var in unique(df[[ap_disagg]])) {
        j = j + 1
        # print(var)
        
        df_sub <- df[df[[ap_disagg]] == var, ]
        cat(paste(ap_var, "-", var, "-", "Done\n"))
        
        if(ap_kobo_type == "select_one"){
          temp <-  single_select(df_sub, ap_var, ap_disagg, var)
        }
        
        if (ap_kobo_type == "select_multiple") {
          temp <- multi_select(df_sub, ap_var, ap_disagg, var)
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "mean"){
          temp <- stat_mean(df_sub, ap_var, ap_disagg, var)
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "median"){
          temp <- stat_median(df_sub, ap_var, ap_disagg, var)
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "sum"){
          temp <- stat_sum(df_sub, ap_var, ap_disagg, var)
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "firstq"){
          temp <- stat_1stq(df_sub, ap_var, ap_disagg, var)
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "thirdq"){
          temp <- stat_3rdq(df_sub, ap_var, ap_disagg, var)
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "min"){
          temp <- stat_min(df_sub, ap_var, ap_disagg, var)
        }
        
        if(ap_kobo_type == "integer" & ap_agr_type == "max"){
          temp <- stat_max(df_sub, ap_var, ap_disagg, var)
        }
        
        temp_result[[j]] <- temp
        
      }
      
      temp_result <- do.call(rbind, temp_result)
      res_list[[i]] <- temp_result
      
    }
    
    
  }
  
  
  result <- do.call(rbind,res_list) %>%
    select( Disaggregation = disaggregation,
            Disaggregation_level = disagg_level,
            Question = variable,
            Response = Var1,
            Aggregation_method = aggregation_method,
            Result = Freq,
            Count = count,
            Denominator = valid)
  
  
  return(result)
  
}

################################################################################

analysis_func_optimized <- function(df, ap, multi_response_sep = "; "){
  
  if("repeat_for" %in% names(ap) & any(!is.na(ap$repeat_for))) {
    ap_dle_disg <- filter(ap, !is.na(repeat_for))
    ap_no_dbl_disg <- filter(ap, is.na(repeat_for))
  } else{
    ap_no_dbl_disg <- ap
  }
  
  if(exists("ap_dle_disg")){
    
    list_analysis <- list()
    for (i in unique(ap_dle_disg$repeat_for)) {
      for (j in unique(df[[i]])) {
        df_i <- df %>% filter(df[[i]] == j)
        
        res <- analyze(df_i, ap_dle_disg)
        res$repeat_for <- j
        
        list_analysis[[length(list_analysis)+1]] <- res
      }
    }
    
    restuls_dbl_disg <- do.call(rbind,list_analysis)
    
  }
  
  if(exists("ap_no_dbl_disg")) {
    result_no_dbl_disag <- analyze(df, ap_no_dbl_disg)
    result_no_dbl_disag$repeat_for <- NA
  }
  
  if(exists("restuls_dbl_disg") & exists("result_no_dbl_disag")){
    results_merged <- rbind(result_no_dbl_disag, restuls_dbl_disg)
  } 
  if(exists("restuls_dbl_disg") & !exists("result_no_dbl_disag")){
    results_merged <- restuls_dbl_disg
  }
  if(!exists("restuls_dbl_disg") & exists("result_no_dbl_disag")){
    results_merged <- result_no_dbl_disag
  }
  
  return(results_merged)
}

