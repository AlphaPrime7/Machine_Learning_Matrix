library(rpart)
ad_data = read.csv("Data/ad.csv",header=FALSE)
ad_data$V1559 = as.factor(ad_data$V1559)

train_data <- train_test_split(ad_data, train=1)
test_data <- train_test_split(ad_data, train=0)

#define minimum observations for in terminal node and required for node split
minspset = c(2,3);minobset = c(1,2,3)
initacc = 0

for (minsp in minspset){
  for (minob in minobset){
    tr_fit = rpart(V1559 ~.,data = train_data,method = "class",minsplit =
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
      print( paste("Train Accuracy ",round(acc,3),"Test
     
#format results  
Accuracy",round(acct,3)))
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

#Import data
ad_data = read.csv("Data/ad.csv",header=FALSE)

#Data modeling
library(tidymodels)
set.seed(123)
split <- initial_split(ad_data)
train_data <- training(split) #or use my function
test_data <- testing(split)
cv <- vfold_cv(train_data) #cross-validation of the data
set.seed(123)
train_data_boot <- bootstraps(train_data, times = 10, apparent = TRUE) #ignore if using CV

#cv to recipe
rec <- recipe(V1559 ~ ., train_data) %>% 
  step_nzv(all_predictors(), freq_cut = 0, unique_cut = 0) %>% 
  step_novel(all_nominal()) %>%
  step_impute_median(all_numeric(), -all_outcomes(), -has_role("id vars"))


#model chosen (piece of cake in spss)
library(parsnip)
library(rpart)
set.seed(100)
model_bag_ads <- bag_tree() %>% #bag is the aggregation method (gradient boosts or random forests)
  set_mode("regression") %>%
  set_engine("rpart", times = 10)

#apply model to recipe
library(baguette)
bag_ads_app <- workflow() %>% 
  add_recipe(rec) %>%
  add_model(model_bag_ads)

#fit the model
set.seed(100)
library(future)
plan(multisession) #running multiple R sessions 
fit_bag_ads_app <- fit_resamples(bag_ads_app, cv,metrics = metric_set(rmse, rsq),
      control = control_resamples(verbose = TRUE, save_pred = TRUE,
      extract = function(x) extract_model(x)))

dectree_fit_extracts <- function(x){
  for(i in x){
    print(i)
  }
}
dectree_fit_extracts(fit_bag_ads_app)

#Visualization
#extract roots
bag_roots_extract <-  function(x){
  x %>% 
    select(.extracts) %>% 
    unnest(cols = c(.extracts)) %>% 
    mutate(models = map(.extracts,
                        ~.x$model_df)) %>% 
    select(-.extracts) %>% 
    unnest(cols = c(models)) %>% 
    mutate(root = map_chr(model,
                          ~as.character(.x$fit$frame[1, 1]))) %>%
    select(root)  
}
bag_roots_extract(fit_bag_ads)

# plot
library(forcats)
bag_roots_extract(fit_bag_ads_app) %>% 
  ggplot(mapping = aes(x = fct_rev(fct_infreq(root)))) + 
  geom_bar() + 
  coord_flip() + 
  labs(x = "root", y = "count") +
  theme(axis.text.y = element_text(size = 9, angle = 30))

#Bootstrapping vs cross-validation
rsq_function <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

set.seed(100)
model_rf_ads <-rand_forest() %>%
  set_engine("ranger",
             num.threads = parallel::detectCores(), 
             importance = "permutation", 
             verbose = TRUE) %>% 
  set_mode("regression") %>% 
  set_args(trees = 1000)

reps <- boot(data=train_data, statistic=rsq_function, R=2000, formula= V1559~.)
reps