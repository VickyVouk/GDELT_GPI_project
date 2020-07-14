#Loading required R packages
library(tidyverse) 
library(caret) 
library(rpart) 
library(party)
library(MLmetrics)

path1 = '/../../all_variables_and_GPI_monthly_all_countries'
path2 = '/../../../dt_results'

country_files = list.files(path1, pattern="*.csv")

for (i in country_files){
  coun<-strsplit(i,"_")[[1]][[3]]
  country<-strsplit(coun, ".", fixed = TRUE)[[1]][[1]]

  print(country)
  #Load the data
  file_df <- file.path(path1, paste('all_variables_', country, '.csv', sep = ''))
  if (file.exists(file_df)){
    df_country_initial<- read.csv(file_df, stringsAsFactors = FALSE)
    drops <- c("MonthYear")
    df_country<-df_country_initial[ , !(names(df_country_initial) %in% drops)] 

    #Split the data into training and test set

    train_set<-0.5
    
    train.data <- head(df_country, round(length(df_country$GPI) * train_set))
    h <- length(df_country$GPI) - length(train.data$GPI)
    test.data <- tail(df_country, h)
  
  #Create the dataframe to save the most important variables per rolling
  df_important_var<-setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("var_name"))
  
  cparameters<-list()
  predictions<-double()
  for (i in (1:(nrow(test.data)))) { 
    model <- train(
      GPI ~., data = train.data, method = "rpart", 
      trControl = trainControl("cv", number = 10),
      tuneLength = 10
  )
    
    #Get the complexity parameter and save it 
    cp <- model$bestTune
    cparameters <- rbind(cparameters, cp) 
    
    predictions <- append(predictions, model %>% predict(test.data[i:i,]))
    
    #Variables' importance
    imp_var <- varImp(model)$importance
    imp_var <- rownames_to_column(imp_var, var = "var_name")
    df_important_var <- rbind(df_important_var, imp_var)
    
    train.data <- rbind(train.data, test.data[i:i,])
  }
  
  #Save in a dataframe cp and predictions 
  predictions_cp <- data.frame(cparameters,predictions)
  rownames(predictions_cp) <- NULL
  write.csv(predictions_cp, file.path (path2, paste(country, '_dt_', train_set, '_predictions_cp.csv', sep = '')))
  
  #Save the important variables per rolling
  write.csv(df_important_var, file.path (path2, paste(country, '_dt_', train_set, '_important_variables.csv', sep = '')))
  
  # Model performance metrics
  results_analytics <- data.frame(
    RMSE = RMSE(predictions, test.data$GPI),
    Rsquare = R2(predictions, test.data$GPI),
    Mape = MAPE(actualpreds, actualtestGPI),
    Pearson = cor(test.data$GPI, predictions,  method = "pearson")
  )
  print(results_analytics)
  #Save result analytics
  write.csv(results_analytics, file.path (path2, paste(country, '_dt_', train_set, '_results.csv', sep = '')), row.names=T)

  #Scatterplot predicted VS actual data
  pdf(file = file.path (path2, paste(country, '_dt_', train_set, '_scatterplot_gpi', '.pdf', sep = ''))) 
  plot(test.data$GPI,predictions,
       xlab="Predicted GPI",ylab="Actual GPI")
  abline(lm(predictions ~ test.data$GPI))
  dev.off()
  
  #Plot predicted VS actual data
  pdf(file = file.path (path2, paste(country, '_dt_', train_set, '_trends_pred_actual_gpi', '.pdf', sep = ''))) 
  plot(test.data$GPI,type="l",col="red",main="Predicted VS Actual GPI", sub="",
       ylab="gpi values", xlab="months")
  lines(predictions,col="blue")
  dev.off()
  }
  #Confirm remove all objects before going to the next interation
  #rm(list = ls(all.names = TRUE))
}  