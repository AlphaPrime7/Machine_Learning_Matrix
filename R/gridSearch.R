train_test_split <- function(df,train=NULL){
  numrow = nrow(df)
  sample_formula = sample(1:numrow,size = as.integer(0.7*numrow))
  train_data = df[sample_formula,]
  test_data = df[-sample_formula,]
  if(!is.null(train) && train == 1){
    return(train_data)
  } else if(!is.null(train) && train == 0){
    return(test_data)
  }
}

#takes a csv, preferably in the wd
gridSearch <- function(gs_data, y){
  library(tidyverse)
  library(rpart)
  gs_data = read.csv(gs_data,header=FALSE)
  y <- as.factor(y)
  
  
  train_data <- train_test_split(gs_data, train=1)
  test_data <- train_test_split(gs_data, train=0)
  
  minspset = c(2,3);minobset = c(1,2,3)
  initacc = 0
  
  for (minsp in minspset){
    for (minob in minobset){
      tr_fit = rpart(V1559 ~.,data = train_data, method = "class",minsplit =
                       minsp, minbucket = minob)
      tr_predt = predict(tr_fit,newdata = train_data,type = "class")
      tble = table(tr_predt,train_data$V1559)
      print(tble)
      acc = (tble[1,1]+tble[2,2])/sum(tble)
      acc
      
      #use model on test data if acc from the train set > init_acc = 0
      if (acc > initacc){
        tr_predtst = predict(tr_fit,newdata = test_data,type = "class")
        tblet = table(test_data$V1559,tr_predtst)
        acct = (tblet[1,1]+tblet[2,2])/sum(tblet)
        acct
        print(paste("Best Score"))
        print( paste("Train Accuracy ",round(acc,3),"Test Accuracy",round(acct,3)))
        print( paste(" Min split ",minsp," Min obs per node ",minob))
        print(paste("Confusion matrix on test data"))
        print(tblet)
        precsn_0 = (tblet[1,1])/(tblet[1,1]+tblet[2,1])
        precsn_1 = (tblet[2,2])/(tblet[1,2]+tblet[2,2])
        print(paste("Precision_0: ",round(precsn_0,3),"Precision_1:
",round(precsn_1,3)))
        rcall_0 = (tblet[1,1])/(tblet[1,1]+tblet[1,2])
        rcall_1 = (tblet[2,2])/(tblet[2,1]+tblet[2,2])
        print(paste("Recall_0: ",round(rcall_0,3),"Recall_1:
",round(rcall_1,3)))
        initacc = acc
      }
    }
  }
}
ad_data = read.csv("Data/ad.csv",header=FALSE)
gridSearch(gs_data = "Data/ad.csv", y = ad_data$V1559)
