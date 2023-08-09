plot_WDI_SD<-function(data=corrWDItest,Wdi_list=comparethiswdi,criterium_name,criterium_direction,criterium_value){
  
  
  for(i in 1:length(Wdi_list)){
    if(i==1){WDI_plots<-list()}
    WDI_sd<-data[(!is.na(data[,Wdi_list[i]])&!is.na(data[,criterium_name]))
                 ,c("Country Code",criterium_name,Wdi_list[i])] 
    N_all<-nrow(WDI_sd)
    if(criterium_direction==">"){
      split_vector<-unlist((WDI_sd[,2]>criterium_value)) 
      
    }
    if(criterium_direction=="<"){
      split_vector<-unlist((WDI_sd[,2]<criterium_value)) 
      
    }
    
    colnames(split_vector)<- "group"
    
    N_NonShapley<-sum(!split_vector,na.rm = T)
    N_Shapley<-sum(split_vector,na.rm = T)
    WDI_sd_2<-cbind(WDI_sd,split_vector)
    WDI_sd_2$all<-"All"
    scatterPlotMatrix(WDI_sd_2[,c(2:4)],zAxisDim = "group")
    seriescode<-colnames(WDI_sd_2)[3]
    colnames(WDI_sd_2)[3]<-"value"
    text_subtitle<-paste0("Subset for: ",toupper(criterium_name)," ",criterium_direction," ",criterium_value)
    series_label<-wdi_explained[wdi_explained$`Series Code` %in% seriescode,"Indicator Name"]
    ggbetweenstats(
      data = WDI_sd_2,
      x = group,
      y = value ) + 
      # Add labels and title
      labs(x="",
           y = seriescode,
           title = series_label,subtitle = text_subtitle
      )+theme(
        # This is the new default font in the plot
        text = element_text(family = "Roboto", size = 10, color = "black"),
        plot.title = element_text(
          family = "Lobster Two", 
          size = 14,
          face = "bold",
          color = "#2a475e"
        ),
        # # Statistical annotations below the main title
        plot.subtitle = element_text(
          family = "Roboto",
          size = 13,
          face = "bold",
          color="#1b2838"
        ),
        plot.title.position = "plot", # slightly different from default
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 12)
      )-> WDI_plots[[i]] 
  }
  return(WDI_plots)
}


train_xgboost_model <- function(testdata, Label, features, custom_control, param_grid) {
  # Filter the data based on IsoYear and select relevant columns
  thisData_2_input <- testdata %>% select(Label, features) %>% drop_na()
  
  
  # Test different parameters
  xgb_caret <- caret::train(
    x = thisData_2_input %>% dplyr::select(features),
    y = thisData_2_input %>% pull(Label),
    method = "xgbTree",
    verbosity = 0,
    trControl = custom_control,tuneGrid=param_grid,
    tuneLength = 1
  )
  
  finalmodel <- caret::train(
    x = thisData_2_input %>% dplyr::select(features),
    y = thisData_2_input %>% pull(Label),
    method = "xgbTree",
    verbosity = 0,
    trControl = trainControl(method = "cv", number = 30),
    tuneGrid = xgb_caret$bestTune
  )
  
  return(finalmodel)
}
calculate_waves <- function(data) {
  wave_list <- list()
  wave_start <- NA
  wave_end <- NA
  found_max <- FALSE
  global_minima <- c()
  
  for (i in 3:(nrow(data) - 2)) { # Skip first and last entries
    # Calculate the slope to the next month and to the previous month
    slope_next <- data$p_value_mort_smoothed[i+1] - data$p_value_mort_smoothed[i]
    slope_prev <- data$p_value_mort_smoothed[i] - data$p_value_mort_smoothed[i-1]
    
    # Check for start of wave (local minima)
    if (is.na(wave_start) && slope_prev < 0 && slope_next > 0) {
      wave_start <- i
    }
    
    # Check for maxima within wave
    if (!is.na(wave_start) && !found_max && slope_prev > 0 && slope_next < 0) {
      found_max <- TRUE
    }
    
    # Check for end of wave (local minima) and avoid capturing minima within 3 months of the start
    if (!is.na(wave_start) && found_max && slope_prev < 0 && slope_next > 0 && (i - wave_start) > 3) {
      wave_end <- i
      global_minima <- c(global_minima, i)  # Save global minima
    }
    
    # If wave ended, add it to the list
    if (!is.na(wave_start) && !is.na(wave_end)) {
      wave_list <- c(wave_list, list(data[wave_start:wave_end, ]))
      wave_start <- NA
      wave_end <- NA
      found_max <- FALSE
    }
  }
  
  # Check for open wave at end of data
  if (!is.na(wave_start) && found_max) {
    wave_list <- c(wave_list, list(data[wave_start:(nrow(data)-1), ])) # Adjust for NA at the end
  }
  
  # If only one wave is found and it starts after 9 months from the beginning of the data
  if(length(wave_list) == 1 && wave_list[[1]][1, ]$date >= as.Date("2020-09-01")) {
    first_global_minima <- wave_list[[1]][1, ]$date
  } else if (length(global_minima) >= 1) {
    # If at least one global minima found, return the first one
    first_global_minima <- data[global_minima[1],"date"]
  } else {
    first_global_minima <- NA
  }
  
  return(list('waves' = wave_list, 'first_global_minima' = first_global_minima))
}

get_wdi_trajectory<-function(data,Indicator,year_start="2015",year_end="2021"){
  dimred2021_knn250_3 %>% filter(year%in% year_start:year_end) %>% group_by(year,p_score_level_label)  %>% summarise(valueA=mean(get(Indicator),na.rm=T),Sample=(length(get(Indicator))-sum(is.na(get(Indicator))))) %>% ggplot(aes(y=valueA,x=year,color=p_score_level_label))+ geom_point()+ geom_line()+ ggtitle(wdi_explained[wdi_explained$`Series Code`%in% Indicator,"Indicator Name"])+ scale_color_viridis(discrete = T)+ theme_bw() +
    geom_label(aes(label = paste0("n = ", Sample)))+ xlab("Year")+ylab("")+theme(legend.position = "bottom")}
