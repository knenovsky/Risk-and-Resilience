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
