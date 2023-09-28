#spec model
mod_boost <- boost_tree() %>% 
  set_engine("xgboost", nthreads = parallel::detectCores()) %>% 
  set_mode("regression")

#app model to rec
wflow_boost <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(mod_boost)

#fit to data cross sections
library(xgboost)
set.seed(100)
train_data_boot <- bootstraps(train_data, times = 10, apparent = TRUE) #bootstrap samples
plan(multisession)
fit_boost <- fit_resamples(
  wflow_boost, 
  train_data_boot,
  metrics = metric_set(rmse, rsq),
  control = control_resamples(verbose = TRUE,
                              save_pred = TRUE)
)
