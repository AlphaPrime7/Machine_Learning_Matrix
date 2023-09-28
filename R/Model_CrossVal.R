library(tune)
collect_metrics(fit_bag_ads_app) %>% 
  bind_rows(collect_metrics(fit_rf_ads)) %>%
  bind_rows(collect_metrics(fit_boost)) %>% 
  filter(.metric == "rmse") %>% 
  mutate(model = c("bag", "rf", "boost")) %>% 
  select(model, everything()) %>% 
  knitr::kable()

# bagged trees
final_fit_bag <- last_fit(
  bag_ads_app,
  split = split
)
# random forest
final_fit_rf <- last_fit(
  rf_ads_app,
  split = split
)
# boosted trees
final_fit_boost <- last_fit(
  wflow_boost,
  split = split
)

collect_metrics(final_fit_bag) %>% 
  bind_rows(collect_metrics(final_fit_rf)) %>%
  bind_rows(collect_metrics(final_fit_boost)) %>% 
  filter(.metric == "rmse") %>% 
  mutate(model = c("bag", "rf", "boost")) %>% 
  select(model, everything()) %>% 
  knitr::kable()
