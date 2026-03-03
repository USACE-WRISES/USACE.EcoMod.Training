## Load packages ------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(doParallel)
library(vip)
library(pdp)

## Import and inspect dataset ------------------------------------------------------------
blgl <- read_csv("data/bluegill.csv") 
str(blgl)

## Convert variables to factors ------------------------------------------------------------
blgl <- blgl %>%
  mutate(across(.cols = c(occ, pool, period), as.factor))

## Plot spatial patterns ------------------------------------------------------------
filter(blgl, depth < 4) %>%
ggplot()+
geom_point(aes(utm_e, utm_n, color = depth, shape = occ))+
  facet_wrap(~pool, scales = "free")+
  scale_color_viridis_c()+
  theme_void() +
  labs(shape = "Presence",
       color = "Depth (m)")

## Split data--------------------------------------------------
blgl_split <- initial_split(blgl, strata = occ, prop = .75)
blgl_train <- training(blgl_split)
blgl_test <- testing(blgl_split)

## Pre-process data with a recipe --------------------------------------------------
blgl_rec <- recipe(occ ~ ., data = blgl_train) %>%
  step_impute_bag(all_predictors())

## Prep dataset to examine pre-processing ------------------------------------------------------------
blgl_prep <- prep(blgl_rec)
juiced <- juice(blgl_prep)  

## Specify random forest model --------------------------------------------------
tune_spec <-
    rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
    set_engine("ranger") %>%
    set_mode("classification")

## Create workflow --------------------------------------------------
tune_wf <- workflow() %>%
  add_recipe(blgl_rec) %>%
  add_model(tune_spec)

## Set up cross-validation for tuning --------------------------------------------------
blgl_folds <- vfold_cv(blgl_train, v = 10)

## Tune models --------------------------------------------------

# Enable parallel processing to increase speed
doParallel::registerDoParallel()

# Tune and fit model
tune_mod <- tune_grid(
  tune_wf,
  resamples = blgl_folds,
  grid = 20
)

tune_mod

## Plot tuning results --------------------------------------------------
tuning_results <- collect_metrics(tune_mod) 

# Subset to just ROC AUC results
results_auc <- tuning_results %>%
  filter(.metric == "roc_auc")

ggplot(results_auc) +
  geom_point(aes(min_n, mean)) +
  labs(y = "AUC")

## Select best model and finalize ------------------------------------------------------------
best_auc <- select_best(tune_mod, metric = "roc_auc")

final_rf <- finalize_model(tune_spec, best_auc)

final_rf

## Evaluate against test data --------------------------------------------------
final_wf <- workflow() %>%
  add_recipe(blgl_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(blgl_split)

final_res %>%
  collect_metrics()

# Collect predictions
preds <- final_res %>% collect_predictions()

# Print confusion matrix
conf_mat(data = preds, truth = occ, estimate = .pred_class)

## Plot where correct/incorrect --------------------------------------------------
preds %>%
  mutate(Accuracy = case_when(
    occ == .pred_class ~ "Correct",
    TRUE ~ "Incorrect"
  )) %>%
  bind_cols(blgl_test) %>%
  ggplot(aes(utm_e, utm_n, color = Accuracy)) +
  geom_point(size = 1, alpha = 0.5) +
  scale_color_manual(values = c("gray80", "darkred"))+
  facet_wrap(~paste("Pool",pool), scales = "free")+
  theme_void()

## Calculate variable importance with the vip package --------------------------------------------------

final_rf %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(occ ~ ., data = juice(blgl_prep)) %>%
  vip(geom = "point")

## Partial dependence plots ------------------------------------------------------------

final_fitted <- final_res$.workflow[[1]]

rf_engine <- extract_fit_engine(final_fitted)

new_depths <- data.frame(depth = seq(0, 2.5, length.out = 50))

pdp_depth <- partial(
  rf_engine,
  pred.var = "depth",
  train = blgl_train,
  prob = TRUE,
  which.class = "1",
  pred.grid = new_depths)
  
# Plot
ggplot(pdp_depth) +
  geom_line(aes(depth, yhat)) +
  labs(
    x = "Depth",
    y = "Predicted occurrence probability",
    title = "Partial dependence plot for bluegill occupancy"
  )
