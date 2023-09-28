#spec model
set.seed(100)
model_rf_ads <-rand_forest() %>%
  set_engine("ranger",
             num.threads = parallel::detectCores(), 
             importance = "permutation", 
             verbose = TRUE) %>% 
  set_mode("regression") %>% 
  set_args(trees = 1000)

#workflow
rf_ads_app <- workflow() %>% 
  add_model(model_rf_ads) %>% 
  add_recipe(rec)

#fit model
set.seed(100)
plan(multisession)
fit_rf_ads <- fit_resamples(
  rf_ads_app,
  cv,
  metrics = metric_set(rmse, rsq),
  control = control_resamples(verbose = TRUE,
                              save_pred = TRUE,
                              extract = function(x) x)
)

#visuals
rf_tree_roots <- function(x){
  library(ranger)
  map_chr(1:1000, 
          ~ranger::treeInfo(x, tree = .)[1, "splitvarName"])
}

rf_roots <- function(x){
  x %>% 
    select(.extracts) %>% 
    unnest(cols = c(.extracts)) %>% 
    mutate(fit = map(.extracts,
                     ~.x$fit$fit$fit),
           oob_rmse = map_dbl(fit,
                              ~sqrt(.x$prediction.error)),
           roots = map(fit, 
                       ~rf_tree_roots(.))
    ) %>% 
    select(roots) %>% 
    unnest(cols = c(roots))
}

# plot
library(ggthemes)
rf_roots(fit_rf_ads) %>% 
  group_by(roots) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n > 75) %>% 
  ggplot(aes(fct_reorder(roots, n), n)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "root", y = "count") +
  theme_economist(horizontal = F) +
  theme(axis.text.y = element_text(size = 8))
