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

get_wdi_trajectory<-function(data,Indicator,groupedby,year_start="2015",year_end="2021",CI=F){
  if(CI==F){
    data %>% filter(year%in% year_start:year_end) %>% group_by(year,get(groupedby))  %>% 
      summarise(valueA=mean(get(Indicator),na.rm=T),Sample=(length(get(Indicator))-sum(is.na(get(Indicator))))) %>% ggplot(aes(y=valueA,x=year,color=get(groupedby)))+ geom_point()+ geom_line()+ ggtitle(wdi_explained[wdi_explained$`Series Code`%in% Indicator,"Indicator Name"])+ scale_color_viridis(discrete = T)+ theme_bw() +
      geom_label(aes(label = paste0("n = ", Sample)))+ xlab("Year")+ylab("")+theme(legend.position = "bottom")}
  
  if(CI==T){
    data %>% filter(year%in% year_start:year_end) %>% group_by(year,group=get(groupedby))  %>% 
      summarise(valueA=mean(get(Indicator),na.rm=T),sd_valueA = sd(get(Indicator), na.rm = TRUE),Sample=(length(get(Indicator))-sum(is.na(get(Indicator))))) %>% 
      ggplot(aes(y=valueA,x=year,color=group)) +
      geom_ribbon(aes(ymin = valueA - sd_valueA, ymax = valueA + sd_valueA, fill = group),
                  alpha = 0.1) +  geom_line() + geom_line()+ ggtitle(wdi_explained[wdi_explained$`Series Code`%in% Indicator,"Indicator Name"])+ scale_color_viridis(discrete = T)+ theme_bw() +
      geom_label(aes(label = paste0("n = ", Sample)))+ xlab("Year")+ylab("")+theme(legend.position = "bottom")} 
}
process_indicator<-function(indicator){
  require(tidyverse)
  require(data.table)
  require(xgboost)
  require(caret)
  require(viridis)
  require(cowplot)
  Label<-indicator
  features<-c("iso1","iso2","iso3","iso4","iso5","iso6","iso7")
  features2<-c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14")
  
  # Set the proportion for the test set (e.g., 0.2 for 20% test data)
  
  test_proportion <- 0.8
  Xgboostinput_filtered<-Xgboostinput2 %>%  select(features,Label)  %>% drop_na()
  Xgboostinput_filtered[, (Label) := scale(.SD[[Label]])]
  index <- caret::createDataPartition(Xgboostinput_filtered$iso1, p = test_proportion, list = FALSE)
  # Split the data into training and test sets
  train_data <- Xgboostinput_filtered[index, ]
  test_data <- Xgboostinput_filtered[-index, ]
  
  # Train the XGBoost model using the function
  trainedModel<- train_xgboost_model(train_data, Label, features, custom_control, param_grid)
  # Assuming you have the 'finalmodel' and 'test_data' from the previous steps
  predictedValue<-postResample(
    predict(trainedModel, 
            test_data %>% dplyr::select(features)),  
    test_data %>% pull(Label)
  )
  
  plotA<-get_wdi_trajectory(data=dimred2021_knn250_5,Label,groupedby ="p_score_level_label2" ,CI = T)
  plotB<-xgboost::xgb.ggplot.shap.summary(data = as.matrix(train_data %>% 
                                                             dplyr::select(features)),
                                          model = trainedModel$finalModel) +scale_color_viridis("Feature Value")+ theme_bw() + xlab("Features")+ylab("Shapley value")+theme(legend.position = "bottom")
  importance<-varImp(trainedModel)$importance
  predictedValue
  
  importance$RMSE<-predictedValue[1]
  importance$Rsquared<-predictedValue[2]
  importance$MAE<-predictedValue[3]
  pdp_all <- lapply(features, function(x){#
    pdp <- pdp::partial(trainedModel, pred.var = c(x), chull = TRUE)
    df <- pdp %>%
      gather(-yhat, key = key, value = value)
  })
  pdp_all2<-do.call(rbind,pdp_all)
  
  pdp_all2$indiacator<-Label
  pdp_all2[pdp_all2$key %in% "iso1","Importance"]<-importance[rownames(importance)%in%"iso1","Overall"]
  pdp_all2[pdp_all2$key %in% "iso1","RMSE"]<-importance[rownames(importance)%in%"iso1","RMSE"]
  pdp_all2[pdp_all2$key %in% "iso1","Rsquared"]<-importance[rownames(importance)%in%"iso1","Rsquared"]
  pdp_all2[pdp_all2$key %in% "iso1","MAE"]<-importance[rownames(importance)%in%"iso1","MAE"]
  
  pdp_all2[pdp_all2$key %in% "iso2","Importance"]<-importance[rownames(importance)%in%"iso2","Overall"]
  pdp_all2[pdp_all2$key %in% "iso2","RMSE"]<-importance[rownames(importance)%in%"iso2","RMSE"]
  pdp_all2[pdp_all2$key %in% "iso2","Rsquared"]<-importance[rownames(importance)%in%"iso2","Rsquared"]
  pdp_all2[pdp_all2$key %in% "iso2","MAE"]<-importance[rownames(importance)%in%"iso2","MAE"]
  
  pdp_all2[pdp_all2$key %in% "iso3","Importance"]<-importance[rownames(importance)%in%"iso3","Overall"]
  pdp_all2[pdp_all2$key %in% "iso3","RMSE"]<-importance[rownames(importance)%in%"iso3","RMSE"]
  pdp_all2[pdp_all2$key %in% "iso3","Rsquared"]<-importance[rownames(importance)%in%"iso3","Rsquared"]
  pdp_all2[pdp_all2$key %in% "iso3","MAE"]<-importance[rownames(importance)%in%"iso3","MAE"]
  
  pdp_all2[pdp_all2$key %in% "iso4","Importance"]<-importance[rownames(importance)%in%"iso4","Overall"]
  pdp_all2[pdp_all2$key %in% "iso4","RMSE"]<-importance[rownames(importance)%in%"iso4","RMSE"]
  pdp_all2[pdp_all2$key %in% "iso4","Rsquared"]<-importance[rownames(importance)%in%"iso4","Rsquared"]
  pdp_all2[pdp_all2$key %in% "iso4","MAE"]<-importance[rownames(importance)%in%"iso4","MAE"]
  
  pdp_all2[pdp_all2$key %in% "iso5","Importance"]<-importance[rownames(importance)%in%"iso5","Overall"]
  pdp_all2[pdp_all2$key %in% "iso5","RMSE"]<-importance[rownames(importance)%in%"iso5","RMSE"]
  pdp_all2[pdp_all2$key %in% "iso5","Rsquared"]<-importance[rownames(importance)%in%"iso5","Rsquared"]
  pdp_all2[pdp_all2$key %in% "iso5","MAE"]<-importance[rownames(importance)%in%"iso5","MAE"]
  
  pdp_all2[pdp_all2$key %in% "iso6","Importance"]<-importance[rownames(importance)%in%"iso6","Overall"]
  pdp_all2[pdp_all2$key %in% "iso6","RMSE"]<-importance[rownames(importance)%in%"iso6","RMSE"]
  pdp_all2[pdp_all2$key %in% "iso6","Rsquared"]<-importance[rownames(importance)%in%"iso6","Rsquared"]
  pdp_all2[pdp_all2$key %in% "iso6","MAE"]<-importance[rownames(importance)%in%"iso6","MAE"]
  
  pdp_all2[pdp_all2$key %in% "iso7","Importance"]<-importance[rownames(importance)%in%"iso7","Overall"]
  pdp_all2[pdp_all2$key %in% "iso7","RMSE"]<-importance[rownames(importance)%in%"iso7","RMSE"]
  pdp_all2[pdp_all2$key %in% "iso7","Rsquared"]<-importance[rownames(importance)%in%"iso7","Rsquared"]
  pdp_all2[pdp_all2$key %in% "iso7","MAE"]<-importance[rownames(importance)%in%"iso7","MAE"]
  
  pc_Corr_filtered<-Xgboostinput2 %>%  select(features2,Label)  %>% drop_na()
  PC_Corrtable<-matrix(nrow = length(fortheseIndicators),ncol=length(features2)+1) %>%  data.frame()
  colnames(PC_Corrtable)<-c("Indicator",features2)
  PC_Corrtable$Indicator<-fortheseIndicators
  
  for(j in 1:length(features2)){
    
    cor_value<-energy::dcor(pc_Corr_filtered%>% select(features2[j]),pc_Corr_filtered %>% select(Label))
    PC_Corrtable[PC_Corrtable$Indicator%in% Label,colnames(PC_Corrtable)%in% features2[j]]  <-cor_value
    
  }
  
  
  plotC<-pdp_all2 %>%   mutate(key = factor(key, levels=features)) %>%  ggplot(pdp_all2,
                                                                               mapping = aes_string(x = "value",
                                                                                                    y = "yhat"))+
    geom_point()+
    geom_smooth(method = "loess", se=T)+
    theme_minimal()+
    facet_wrap(~(key), scales = "free", ncol = 3) 
  
  shapleyplot<-plot_grid(plotC,plotB,ncol = 2,rel_widths  =     c(1, 5/10))
  indicatorplot<-plotA
  PC_Corrtable%>%  drop_na() %>%  drop_na()%>% fwrite(paste0("../output/wdi_dim_corr/summarystat_pc/PC_corr",indicator,"_summary.csv"),)
  pdp_all2 %>% fwrite(paste0("../output/wdi_dim_corr/summarystat/",indicator,"_summary.csv"),)
  shapleyplot %>% ggsave(file=paste0("../output/wdi_dim_corr/plots/",indicator,"_shapley_plot.png"),device="png")
  indicatorplot %>% ggsave(file=paste0("../output/wdi_dim_corr/plots/",indicator,"_indicator_plot.png"),device="png")
  return(list(indicator = indicator))
}

