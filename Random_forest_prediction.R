#http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/140-bagging-and-random-forest-essentials#computing-random-forest-regression-trees
#Loading required R packages
library(tidyverse)
library(caret)
library(randomForest)
library(party)

country_files = list.files("/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/Lead_lag/gdelt_variables/all_variables",pattern="*.csv")
for (i in country_files){
  #we use paste to concatenate the string file name in R
  coun<-strsplit(i,"_")[[1]][[3]]
  country<-strsplit(coun, ".", fixed = TRUE)[[1]][[1]]

  results_analytics <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c('country', 'mtry', 'RMSE', 'Rsquare', 'MSE', 'Pearson'))
  
  #Load the data
  file_df <-sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/all_variables_non_log/all_variables_%s.csv', country)
  if (file.exists(file_df)){
    print(country)
    df_country_initial<- read.csv(file_df, stringsAsFactors = FALSE)
    drops <- c("MonthYear")
    df_country_rf<-df_country_initial[ , !(names(df_country_initial) %in% drops)]
    
    # Split the data into training and test set
    #You still do not seperate between dependent and indepqendent variables
    #Create the training and test set
    
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
        method = "rf",  #train() with method= "glmnet" fits elastic-net
        importance = TRUE, #MeanDecreaseAccuracy, which is the average decrease of model 
        #accuracy in predicting the outcome of the out-of-bag samples when a specific variable
        #is excluded from the model.
        trControl = trainControl(method="cv", number=10),
        tuneLength = 10
      )

      
      #Create a dataframe with the variables' importance
      imp_var <- varImp(model)$importance
      #Keep variable names from the index to seperate column
      imp_var <- rownames_to_column(imp_var, var = "var_name")
      #Add the dataframe to the bigger dataframe
      df_important_var <- rbind(df_important_var, imp_var)
      
      # Best tuning parameter
      mtry <- model$bestTune
      mtry_var <- rbind(mtry_var, mtry) 
      predictions <- append(predictions, model %>% predict(test.data[i:i,]))
      train.data <- rbind(train.data, test.data[i:i,])
    }
    
    #Save the most important variables per rolling
    write.csv(df_important_var, sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/rf_nowcasting_non_log/%s_rf_nowcasting_%s_important_variables.csv', country, train_set))
    
    #Save in a dataframe mtry and predictions 
    predictions_mtry <- data.frame(mtry_var,predictions)
    #print(predictions_mtry)
    rownames(predictions_mtry) <- NULL
    write.csv(predictions_mtry, sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/rf_nowcasting_non_log/%s_rf_nowcasting_%s_predictions_mtry.csv', country, train_set))
    
    # Model performance metrics
    results_analytics <- data.frame(
      RMSE = RMSE(predictions, test.data$GPI),
      Rsquare = R2(predictions, test.data$GPI),
      MSE = mean((test.data$GPI-predictions)^2),
      Pearson = cor(test.data$GPI, predictions,  method = "pearson")
    )
    write.csv(results_analytics, sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/rf_nowcasting_non_log/%s_rf_ordered_%s_results.csv',country,train_set))
    
    
    #Scatterplot predicted VS actual data
    #First set where to save the plot
    setwd(sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/rf_nowcasting_non_log/'))
    png(filename=sprintf('%s_rf_ordered_%s_scatterplot.png', country, train_set))
    #Scatterplot predicted VS actual data
    plot(test.data$GPI,predictions,
         xlab="Predicted GPI",ylab="Actual GPI")
    abline(lm(predictions ~ test.data$GPI))
    dev.off()
    
    #Plot predicted VS actual data
    #First set where to save the plot
    setwd(sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/rf_nowcasting_non_log/'))
    png(filename=sprintf('%s_rf_ordered_%s_trends_pred_actual.png', country, train_set))
    #Plot predicted VS actual data
    plot(test.data$GPI,type="l",col="red",main="Predicted VS Actual GPI", sub="",
         ylab="gpi values", xlab="months")
    lines(predictions,col="blue")
    dev.off()
  }
  #Confirm remove all objects before going to the next interation
  rm(list = ls(all.names = TRUE))
}
