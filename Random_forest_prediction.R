#Loading required R packages
library(tidyverse)
library(caret)
library(randomForest)
library(party)

path1 = '/../../all_variables_and_GPI_monthly_all_countries'
path2 = '/../../rf_results'

country_files = list.files(path1, pattern="*.csv")

for (i in country_files){
  coun<-strsplit(i,"_")[[1]][[3]]
  country<-strsplit(coun, ".", fixed = TRUE)[[1]][[1]]
  print(country)
  
  results_analytics <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c('country', 'mtry', 'RMSE', 'Rsquare', 'MSE', 'Pearson'))
  
  #Load the data
  file_df <- file.path(path1, paste('all_variables_', country, '.csv', sep = '')) 
  if (file.exists(file_df)){
    df_country_initial<- read.csv(file_df, stringsAsFactors = FALSE)
    drops <- c("MonthYear")
    df_country_rf<-df_country_initial[ , !(names(df_country_initial) %in% drops)]
    
    #Split the data into training and test set
    
    train_set<-0.5
    
    train.data <- head(df_country_rf, round(length(df_country_rf$GPI) * train_set))
    h <- length(df_country_rf$GPI) - length(train.data$GPI)
    test.data <- tail(df_country_rf, h)
      
    #Prediction model
    
    #The dataframe with the most important variables per rolling
    df_important_var<-setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("var_name"))
    
    mtry_var <- list()
    predictions <- double()
    for (i in (1:nrow(test.data))){
      model <- train(
      GPI~., data = train.data,
      method = "rf", 
      importance = TRUE,
      trControl = trainControl(method="cv", number=10),
      tuneLength = 10
      )
    
    #CVariables' importance
    imp_var <- varImp(model)$importance
    imp_var <- rownames_to_column(imp_var, var = "var_name")
    df_important_var <- rbind(df_important_var, imp_var)
    
    # Best tuning parameter
    mtry <- model$bestTune
    mtry_var <- rbind(mtry_var, mtry) 
    predictions <- append(predictions, model %>% predict(test.data[i:i,]))
    train.data <- rbind(train.data, test.data[i:i,])
    }
    
    #Save the most important variables per rolling
    write.csv(df_important_var,file.path (path2, paste(country, '_rf_', train_set, '_important_variables.csv', sep = '')))
    
    #Save in a dataframe mtry and predictions 
    predictions_mtry <- data.frame(mtry_var,predictions)
    rownames(predictions_mtry) <- NULL
    write.csv(predictions_mtry, file.path (path2, paste(country, '_rf_', train_set, '_predictions_mtry.csv', sep = '')), row.names=T)
    
    # Model performance metrics
    results_analytics <- data.frame(
    RMSE = RMSE(predictions, test.data$GPI),
    Rsquare = R2(predictions, test.data$GPI),
    MSE = mean((test.data$GPI-predictions)^2),
    Pearson = cor(test.data$GPI, predictions,  method = "pearson")
    )
    write.csv(results_analytics, file.path (path2, paste(country, '_rf_', train_set, '_results.csv', sep = '')), row.names=T)
    
    #Scatterplot predicted VS actual data
    pdf(file = file.path (path2, paste(country, '_rf_', train_set, '_scatterplot', '.pdf', sep = ''))) 
    plot(test.data$GPI,predictions,
         xlab="Predicted GPI",ylab="Actual GPI")
    abline(lm(predictions ~ test.data$GPI))
    dev.off()
    
    #Plot predicted VS actual data
    pdf(file = file.path (path2, paste(country, '_rf_', train_set, '_trends_pred_actual', '.pdf', sep = ''))) 
    plot(test.data$GPI,type="l",col="red",main="Predicted VS Actual GPI", sub="",
         ylab="gpi values", xlab="months")
    lines(predictions,col="blue")
    dev.off()
    }
    #Confirm remove all objects before going to the next interation
    #rm(list = ls(all.names = TRUE))
}
