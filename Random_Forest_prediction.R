#http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/140-bagging-and-random-forest-essentials#computing-random-forest-regression-trees
#Loading required R packages
library(tidyverse)
library(caret)
library(randomForest)
library(party)

#country_list<-list('MP', 'MR', 'MU', 'MX', 'MY', 'MZ' )#,'','LY','SO','NZ','PO') ##'SI','SN','RS','CH','ES','VE')#'RS','SY', NZ','AE','BA','TH','AL','ER','JM','ML','SI','CG','CF','UZ','SA','LE','PE','SN','BO','YM','BC','KG','CM','SY','TX','AG','BL','ZI','GG','CB','NP','JA','SW','EC','IC','KU','DJ','BE','JO','WZ','PO','ID','LT')

path1 = '/../../../all_variables_and_GPI_monthly_all_countries'
path2 = '/../../../rf_results'

country_files = list.files(path1, pattern="*.csv")

for (i in country_files){
  #we use paste to concatenate the string file name in R
  coun<-strsplit(i,"_")[[1]][[3]]
  country<-strsplit(coun, ".", fixed = TRUE)[[1]][[1]]
  

#for (country in country_list){
  print(country)
  results_analytics <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c('country', 'mtry', 'RMSE', 'Rsquare', 'MSE', 'Pearson'))
  
  #Load the data
  file_df <- file.path(path1, paste('all_variables_', country, '.csv', sep = '')) #sep = '' does not leave gaps during name creation
  if (file.exists(file_df)){
    df_country_initial<- read.csv(file_df, stringsAsFactors = FALSE)
    drops <- c("MonthYear")
    df_country_rf<-df_country_initial[ , !(names(df_country_initial) %in% drops)]
    
    #Split the data into training and test set
    #You still do not seperate between dependent and indepqendent variables
    #Create the training and test set
    
    train_set<-0.5
    
    train.data <- head(df_country_rf, round(length(df_country_rf$GPI) * train_set))
    h <- length(df_country_rf$GPI) - length(train.data$GPI)
    test.data <- tail(df_country_rf, h)
      
    #Prediction model
    
    # Plot MeanDecreaseAccuracy
    #The results show that across all of the trees considered in the random forest, 
    #the most important variables are those with higher values.
    #varImpPlot(model$finalModel, type = 1)
    
    #The dataframe with the most important variables per rolling
    df_important_var<-setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("var_name"))
    
    mtry_var <- list()
    predictions <- double()
    for (i in (1:nrow(test.data))){
      model <- train(
      GPI~., data = train.data,
      method = "rf", 
      importance = TRUE,#, #MeanDecreaseAccuracy, which is the average decrease of model 
      #accuracy in predicting the outcome of the out-of-bag samples when a specific variable
      #is excluded from the model.
      trControl = trainControl(method="cv", number=10),
      tuneLength = 10
      )
      
    #The function varImp() [in caret] displays the importance of variables in percentage:
    #varImp(model)
    
    #Create a dataframe with the variables' importance
    imp_var <- varImp(model)$importance
    #Order the dataframe
    #imp_var <- imp_var[order(imp_var$Overall, decreasing=TRUE),, drop=FALSE]
    #Keep the 5 most important variables
    #imp_var <- head(imp_var,5)
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
    write.csv(results_analytics, file.path (path2, paste(country, '_rf_ordered_', train_set, '_results.csv', sep = '')), row.names=T)
    

    #Scatterplot predicted VS actual data
    #Set the file directory and name
    pdf(file = file.path (path2, paste(country, '_rf_ordered_', train_set, '_scatterplot', '.pdf', sep = ''))) 
    plot(test.data$GPI,predictions,
         xlab="Predicted GPI",ylab="Actual GPI")
    abline(lm(predictions ~ test.data$GPI))
    dev.off()
    
    #Plot predicted VS actual data
    #First set where to save the plot
    pdf(file = file.path (path2, paste(country, '_rf_ordered_', train_set, '_trends_pred_actual', '.pdf', sep = ''))) 
    #Plot predicted VS actual data
    plot(test.data$GPI,type="l",col="red",main="Predicted VS Actual GPI", sub="",
         ylab="gpi values", xlab="months")
    lines(predictions,col="blue")
    dev.off()
    }
    #Confirm remove all objects before going to the next interation
    #rm(list = ls(all.names = TRUE))
}
