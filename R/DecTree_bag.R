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
      extract = function(x) extract_model(x))) #advised to use extract_fit_engine()

dectree_fit_extracts <- function(x){
  for(i in x){
    print(i)
  }
}
dectree_fit_extracts(fit_bag_ads_app)

#extract roots
bag_roots_extract <-  function(x){
  x %>% 
    select(.extracts) %>% 
    unnest(cols = c(.extracts)) %>% 
    mutate(models = map(.extracts,
                        ~.x$model_df)) %>% 
    select(-.extracts) %>% 
    unnest(cols = c(models)) %>% 
    mutate(root = map_chr(model, ~as.character(.x$fit$frame[1, 1]))) %>%
    select(root)  
}
bag_roots_extract(fit_bag_ads_app)

# plot
library(forcats)
bag_roots_extract(fit_bag_ads_app) %>% 
  ggplot(mapping = aes(x = fct_rev(fct_infreq(root)))) + 
  geom_bar() + 
  coord_flip() + 
  labs(x = "root", y = "count") +
  theme(axis.text.y = element_text(size = 9, angle = 30))
