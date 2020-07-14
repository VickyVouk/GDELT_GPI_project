#Loading required R packages
library(tidyverse)
library(caret)
library(randomForest)
library(party)
library(MLmetrics)

path1 = '/../../all_variables_and_GPI_monthly_all_countries'
path2 = '/../../elnet_results'

country_files = list.files(path1, pattern="*.csv")

for (i in country_files){
  coun<-strsplit(i,"_")[[1]][[3]]
  country<-strsplit(coun, ".", fixed = TRUE)[[1]][[1]]

  #Load the data
  file_df <- file.path(path1, paste('all_variables_', country, '.csv', sep = '')) #sep = '' does not leave gaps during name creation
  if (file.exists(file_df)){
    print(country)
    df_country_initial<- read.csv(file_df, stringsAsFactors = FALSE)
    drops <- c("MonthYear")
    df_country<-df_country_initial[ , !(names(df_country_initial) %in% drops)] 

    #Split the data into training and test set
    
    #Set the train percentage 
    train_set<-0.5

    train.data <- head(df_country, round(length(df_country$GPI) * train_set))
    h <- length(df_country$GPI) - length(train.data$GPI)
    test.data <- tail(df_country, h)
      
    #Prediction model

    #Create the dataframe to save the impVar per rolling
    df_impvar<-setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("Overall"))
    
    predictions<-double()
    for (i in (1:(nrow(test.data)))) { 
      model <- train(
        GPI~., data = train.data,
        method = "glmnet",
        trControl = trainControl("cv", number = 10),
        tuneLength = 10
      )
      
      #Variables' importance
      imp_var <- varImp(model)$importance
      imp_var <- rownames_to_column(imp_var, var = "var_name")
      df_impvar <- rbind(df_impvar, imp_var)
      
      predictions <- append(predictions, model %>% predict(test.data[i:i,]))
      train.data <- rbind(train.data, test.data[i:i,])
    }
  
  #Save the most important variables per rolling
  write.csv(df_impvar, file.path (path2, paste(country, '_elnet_', train_set, '_impvar.csv', sep = '')))
  
  #Save the predictions
  write.csv(predictions, file.path (path2, paste(country, '_elnet_', train_set, '_predictions.csv', sep = '')), row.names=T)
  
  # Model performance metrics
  results_analytics <- data.frame(
    RMSE = RMSE(predictions, test.data$GPI), 
    Rsquare = R2(predictions, test.data$GPI),
    Mape = MAPE(predictions, test.data$GPI),
    Pearson = cor(test.data$GPI, predictions,  method = "pearson")
  )
  write.csv(results_analytics, file.path (path2, paste(country, '_elnet_', train_set, '_results.csv', sep = '')), row.names=T)

  #Scatterplot predicted VS actual data
  pdf(file = file.path (path2, paste(country, '_elnet_', train_set, '_scatterplot', '.pdf', sep = ''))) 
  plot(test.data$GPI,predictions,
       xlab="Predicted GPI",ylab="Actual GPI")
  abline(lm(predictions ~ test.data$GPI))
  dev.off()
  
  #Plot predicted VS actual data
  pdf(file = file.path (path2, paste(country, '_elnet_', train_set, '_trends_pred_actual', '.pdf', sep = ''))) 
  plot(test.data$GPI,type="l",col="red",main="Predicted VS Actual GPI", sub="",
       ylab="gpi values", xlab="months")
  lines(predictions,col="blue")
  dev.off()
  }
 
#Confirm remove all objects before going to the next interation
rm(list = ls(all.names = TRUE))
}

