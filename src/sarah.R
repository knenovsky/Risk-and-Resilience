```{r}
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

Xgboostinput<- fread("file")
Xgboostinput2<-Xgboostinput %>% mutate(yearmon=as.numeric(as.factor(str_remove(pattern = "[a-zA-Z_]+",identifier)))) %>% filter(yearmon>14)

Label<-"p_value_mort"
features<-c("iso1","iso2","iso3","iso4","iso5","iso6","iso7","new_cases_smoothed_per_million","meanTemp","yearmon","diabetes_prevalence","median_age")


custom_control <- trainControl(
  method = "cv",
  number = 30,  # Increase the number of folds
  search = "random",
  allowParallel = TRUE
  
)

param_grid <- expand.grid(
  nrounds = c(50,100),
  eta = c(0.05),
  max_depth = c(2, 4),
  colsample_bytree = c( 1),
  subsample = c(1),
  gamma = c(0),
  min_child_weight = c(0)
)



# Set the proportion for the test set (e.g., 0.2 for 20% test data)

test_proportion <- 0.8
Xgboostinput_filtered<-Xgboostinput2 %>%  select(features,Label) %>% drop_na()
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
predictedValue


### Agnostic

shap_values <- shap.values(xgb_model = trainedModel$finalModel, X_train = as.matrix(train_data %>% dplyr::select(all_of(features))))
xgboost::xgb.ggplot.shap.summary(data = as.matrix(train_data %>% 
                                                    dplyr::select(features)),
                                 model = trainedModel$finalModel) +scale_color_viridis("Feature Value")+ theme_bw() + xlab("Features")+ylab("Shapley value")+theme(legend.position = "left")


pdp_all <- lapply(features, function(x){#
  pdp <- pdp::partial(trainedModel, pred.var = c(x), chull = TRUE)
  df <- pdp %>%
    gather(-yhat, key = key, value = value)
})
pdp_all2<-do.call(rbind,pdp_all)
pdp_all2 %>%   mutate(key = factor(key, levels=features)) %>%  ggplot(pdp_all2,
                                                                      mapping = aes_string(x = "value",
                                                                                           y = "yhat"))+
  geom_point()+
  geom_smooth(method = "loess", se=T)+
  theme_minimal()+
  facet_wrap(~(key), scales = "free", ncol = 3)  + ggtitle("")

```
