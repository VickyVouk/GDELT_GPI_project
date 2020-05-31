#Loading required R packages
library(tidyverse)
library(caret)
library(randomForest)
library(party)

country_files = list.files("/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/all_variables/",pattern="*.csv")

for (i in country_files){
  coun<-strsplit(i,"_")[[1]][[3]]
  country<-strsplit(coun, ".", fixed = TRUE)[[1]][[1]]

  #Load the data
  file_df <-sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/all_variables/all_variables_%s.csv', country)
  if (file.exists(file_df)){
    print(country)#print in order to control in which iteration we are
    df_country_initial<- read.csv(file_df, stringsAsFactors = FALSE)
    drops <- c("MonthYear")
    df_country<-df_country_initial[ , !(names(df_country_initial) %in% drops)]
    
    
    # Split the data into training and test set
    #You still do not seperate between dependent and indepqendent variables
    #Create the training and test set
    
    #Set the train percentage 
    train_set_list<-list(0.6)
    
    for (train_set in train_set_list){
      print(train_set)
      
      train.data <- head(df_country, round(length(df_country$GPI) * train_set))
      h <- length(df_country$GPI) - length(train.data$GPI)
      test.data <- tail(df_country, h)
      
      
      #Prediction model
      
      #set.seed(123)
      
      #The dataframe with the most important variables per rolling
      df_important_var<-setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("Overall"))
      
      predictions<-double()
      for (i in (1:(nrow(test.data)))) { 
        model <- train(
          GPI~., data = train.data,
          method = "glmnet",
          trControl = trainControl("cv", number = 10),
          tuneLength = 10
        )
        
        #Create a dataframe with the variables' importance
        imp_var <- varImp(model)$importance
        #Keep variable names from the index to seperate column
        imp_var <- rownames_to_column(imp_var, var = "var_name")
        #Add the dataframe to the bigger dataframe
        df_important_var <- rbind(df_important_var, imp_var)
        
        predictions <- append(predictions, model %>% predict(test.data[i:i,]))
        train.data <- rbind(train.data, test.data[i:i,])
      }
      
      #Save the most important variables per rolling
      write.csv(df_impvar, sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/elnet/%s_elnet_%s_important_variables.csv', country, train_set))

      #Save the predictions
      write.csv(predictions, sprintf("/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/elnet/%s_elnet_%s_predictions.csv", country, train_set), row.names=T)

      # Model performance metrics
      results_analytics <- data.frame(
        RMSE = RMSE(predictions, test.data$GPI), 
        Rsquare = R2(predictions, test.data$GPI),
        MSE = mean((test.data$GPI-predictions)^2),
        Pearson = cor(test.data$GPI, predictions,  method = "pearson")
      )
      write.csv(results_analytics, sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/elnet/%s_elnet_%s_results.csv', country, train_set))
      
      #Scatterplot predicted VS actual data
      #First set where to save the plot
      setwd(sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/elnet/',train_set))
      png(filename=sprintf('%s_elnet_%s_scatterplot.png', country, train_set))
      #Scatterplot predicted VS actual data
      plot(test.data$GPI,predictions,
           xlab="Predicted GPI",ylab="Actual GPI")
      abline(lm(predictions ~ test.data$GPI))
      dev.off()
      
      #Plot predicted VS actual data
      #First set where to save the plot
      setwd(sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/elnet/',train_set))
      png(filename=sprintf('%s_elnet_%s_trends_pred_actual_new.png', country, train_set))
      #Plot predicted VS actual data
      plot(test.data$GPI,type="l",col="red",main="Predicted VS Actual GPI", sub="",
           ylab="gpi values", xlab="months")
      lines(predictions,col="blue")
      dev.off()
    }
  }
  #Confirm remove all objects before going to the next interation
  rm(list = ls(all.names = TRUE))
}

