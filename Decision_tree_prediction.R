#http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials#loading-required-r-packages
#Loading required R packages
library(tidyverse) #for easy data manipulation and visualization
library(caret) #for easy machine learning workflow
library(rpart) #for computing decision tree models
library(party)

country_files = list.files("/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/Lead_lag/gdelt_variables/all_variables",pattern="*.csv")
for (i in country_files){
  #we use paste to concatenate the string file name in R
  coun<-strsplit(i,"_")[[1]][[3]]
  country<-strsplit(coun, ".", fixed = TRUE)[[1]][[1]]
  
  results_analytics <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c('country', 'mtry', 'RMSE', 'Rsquare', 'MSE', 'Pearson'))
  
  #Load the data
  file_df <-sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/all_variables/all_variables_%s.csv', country)
  if (file.exists(file_df)){
    df_country_initial<- read.csv(file_df, stringsAsFactors = FALSE)
    drops <- c("MonthYear")
    df_country<-df_country_initial[ , !(names(df_country_initial) %in% drops)]
    #df_country<- na.omit(df_country) #remove na
    
    #Set the train percentage 
    train_set_list<-list(0.5)
    
    for (train_set in train_set_list){
      print(train_set)
      
      train.data <- head(df_country, round(length(df_country$GPI) * train_set))
      h <- length(df_country$GPI) - length(train.data$GPI)
      test.data <- tail(df_country, h)
      
      #The dataframe with the the 5 most important variables per rolling
      df_important_var<-setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("var_name"))
      
      cparameters<-list()
      predictions<-double()
      for (i in (1:(nrow(test.data)))) { 
        model <- train(
          GPI ~., data = train.data, method = "rpart", 
          trControl = trainControl("cv", number = 10),
          tuneLength = 10
        )
        
        cp <- model$bestTune
        cparameters <- rbind(cparameters, cp) 
        predictions <- append(predictions, model %>% predict(test.data[i:i,]))
        #Create a dataframe with the variables' importance
        imp_var <- varImp(model)$importance
        #Keep variable names from the index to seperate column
        imp_var <- rownames_to_column(imp_var, var = "var_name")
        #Add the dataframe to the bigger dataframe
        df_important_var <- rbind(df_important_var, imp_var)
        train.data <- rbind(train.data, test.data[i:i,])
      }
      
      # Plot model error vs different values of
      # cp (complexity parameter)
      # plot(model)
      # Print the best tuning parameter cp that
      # minimize the model RMSE
      
      #Save in a dataframe cp and predictions 
      predictions_cp <- data.frame(cparameters,predictions)
      rownames(predictions_cp) <- NULL
      write.csv(predictions_cp, sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/dt/%s_dt_%s_predictions_cp.csv', country, train_set))
      
      #Save the important variables per rolling
      write.csv(df_important_var, sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/dt/%s_dt_%s_important_variables.csv', country, train_set))
      
      
      # Model performance metrics
      results_analytics <- data.frame(
        RMSE = RMSE(predictions, test.data$GPI),
        Rsquare = R2(predictions, test.data$GPI),
        MSE = mean((test.data$GPI-predictions)^2),
        Pearson = cor(test.data$GPI, predictions,  method = "pearson")
      )
      #Save result analytics
      write.csv(results_analytics, sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/dt/%s_dt_%s_results.csv', country, train_set))
      
      #Scatterplot predicted VS actual data
      #First set where to save the plot
      setwd(sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/dt/'))
      png(filename=sprintf('%s_dt_%s_scatterplot_gpi.png', country, train_set))
      #Scatterplot predicted VS actual data
      plot(test.data$GPI,predictions_cp$predictions,
           xlab="Predicted GPI",ylab="Actual GPI")
      abline(lm(predictions_cp$predictions ~ test.data$GPI))
      dev.off()
      
      #Plot predicted VS actual data
      #First set where to save the plot
      setwd(sprintf('/Users/vickyvoukelatou/Documents/Gdelt/GPI_project/experiment/dt/'))
      png(filename=sprintf('%s_dt_%s_trends_pred_actual_gpi.png', country, train_set))
      #Plot predicted VS actual data
      plot(test.data$GPI,type="l",col="red",main="Predicted VS Actual GPI", sub="",
           ylab="gpi values", xlab="months")
      lines(predictions_cp$predictions,col="blue")
      dev.off()
      
    }
  }  
  #Confirm remove all objects before going to the next interation
  rm(list = ls(all.names = TRUE))
}  