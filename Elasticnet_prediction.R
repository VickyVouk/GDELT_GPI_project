#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/153-penalized-regression-essentials-ridge-lasso-elastic-net
#Loading required R packages
library(tidyverse)
library(caret)
library(randomForest)
library(party)

#country_list<-list('UK')#, 'RS', 'CH', 'GM', 'FR', 'JA', 'SA', 'IC', 'NZ', 'PO', 'AU', 'DA', 'CA', 'SN', 'SI', 'EZ','AF', 'SY', 'OD', 'YM', 'IZ', 'SO', 'CT', 'LY', 'CG', 'PK', 'MX','NI', 'SU')

path1 = '/../../../all_variables_and_GPI_monthly_all_countries'
path2 = '/../../../elnet_results'

country_files = list.files(path1, pattern="*.csv")

for (i in country_files){
  #we use paste to concatenate the string file name in R
  coun<-strsplit(i,"_")[[1]][[3]]
  country<-strsplit(coun, ".", fixed = TRUE)[[1]][[1]]

#for (country in country_list){
  #Load the data
  file_df <- file.path(path1, paste('all_variables_', country, '.csv', sep = '')) #sep = '' does not leave gaps during name creation
  if (file.exists(file_df)){
    print(country)#print in order to control in which iteration we are
    df_country_initial<- read.csv(file_df, stringsAsFactors = FALSE)
    drops <- c("MonthYear")
    df_country<-df_country_initial[ , !(names(df_country_initial) %in% drops)] ##drop the MonthYear column
    #df_country<- na.omit(df_country) #remove na

    # Split the data into training and test set
    #Create the training and test set
    
    #Set the train percentage 
    train_set_list<-list(0.5)#,0.6, 0.7, 0.8)
    
    for (train_set in train_set_list){
      print(train_set)
        
      train.data <- head(df_country, round(length(df_country$GPI) * train_set))
      h <- length(df_country$GPI) - length(train.data$GPI)
      test.data <- tail(df_country, h)
        
      
      #Prediction model
      
      #Cross_validation

      #The dataframe with the impVar per rolling
      df_impvar<-setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("Overall"))
      #The dataframe with the most important variables/coefficient per rolling
      df_important_var<-setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("var_name"))
      
      predictions<-double()
      for (i in (1:(nrow(test.data)))) { #(i in (1:(nrow(test.data)-25))){
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
        df_impvar <- rbind(df_impvar, imp_var)
        
        #Model coefficients
        coefficients<-coef(model$finalModel, model$bestTune$lambda)
        
        #Convert coefficient object to dataframe
        df_coefficient<-data.frame(var_name = coefficients@Dimnames[[1]][coefficients@i + 1], coefficient = coefficients@x)
        #The reason for +1 is that the @i method indexes from 0 for the intercept but @Dimnames[[1]] starts at 1.
       
        #Delete the first row that is the intercept
        df_coefficient <- df_coefficient[-1,]
        
        #Get the absolute values of the coefficients
        df_coefficient['coefficient']<-abs(df_coefficient['coefficient'])
        #Order coefficients by importance 
        df_coefficient<-df_coefficient[order(-df_coefficient$coefficient),]
        #Reset index
        row.names(df_coefficient) <- NULL
        #Keep the 5 most important
        df_coefficient<-head(df_coefficient,5)
        #Keep only the first column with the variable names if not wanting coefficients
        #df_coefficient <- df_coefficient['var_name']
        #Save the dataframe
        df_important_var <- rbind(df_important_var, df_coefficient)
         
      
        predictions <- append(predictions, model %>% predict(test.data[i:i,]))
        train.data <- rbind(train.data, test.data[i:i,])
      }
    
    #Save the most important variables per rolling
    write.csv(df_impvar, file.path (path2, paste(country, '_elnet_ordered_', train_set, '_impvar.csv', sep = '')))
    
    #Save the predictions
    write.csv(predictions, file.path (path2, paste(country, '_elnet_ordered_', train_set, '_predictions.csv', sep = '')), row.names=T)
    
    # Model performance metrics
    results_analytics <- data.frame(
      RMSE = RMSE(predictions, test.data$GPI), #test.data$GPI[1:39]
      Rsquare = R2(predictions, test.data$GPI),
      MSE = mean((test.data$GPI-predictions)^2),
      Pearson = cor(test.data$GPI, predictions,  method = "pearson")
    )
    write.csv(results_analytics, file.path (path2, paste(country, '_elnet_ordered_', train_set, '_results.csv', sep = '')), row.names=T)

    #Scatterplot predicted VS actual data
    #Set the file directory and name
    pdf(file = file.path (path2, paste(country, '_elnet_ordered_', train_set, '_scatterplot', '.pdf', sep = ''))) 
    plot(test.data$GPI,predictions,
         xlab="Predicted GPI",ylab="Actual GPI")
    abline(lm(predictions ~ test.data$GPI))
    dev.off()
    
    #Plot predicted VS actual data
    #First set where to save the plot
    pdf(file = file.path (path2, paste(country, '_elnet_ordered_', train_set, '_trends_pred_actual', '.pdf', sep = ''))) 
    #Plot predicted VS actual data
    plot(test.data$GPI,type="l",col="red",main="Predicted VS Actual GPI", sub="",
         ylab="gpi values", xlab="months")
    lines(predictions,col="blue")
    dev.off()
    }
   }
  #Confirm remove all objects before going to the next interation
  #rm(list = ls(all.names = TRUE))
}

