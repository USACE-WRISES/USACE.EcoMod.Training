## Load packages ------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(doParallel)
library(vip)
library(DALEXtra)

## Import and inspect dataset and  ------------------------------------------------------------
blgl <- read_csv("data/bluegill.csv") 
str(blgl)

## Convert variables to factors ------------------------------------------------------------
blgl <- blgl %>%
  mutate(across(.cols = c(occ, pool, period), as.factor))

## Plot spatial patterns ------------------------------------------------------------
filter(blgl, depth < 4) %>%
ggplot()+
geom_point(aes(utm_e/1000, utm_n/1000, color = depth, shape = occ))+
  facet_wrap(~pool, scales = "free")+
  scale_color_viridis_c()+
  theme_minimal() +
  labs(x = "UTM E (thousands of m)",
       y = "UTM N (thousands of m)",
       shape = "Presence",
       color = "Depth (m)")

## Split data--------------------------------------------------
set.seed(123)
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
set.seed(234)
blgl_folds <- vfold_cv(blgl_train, v = 10)

## Tune models --------------------------------------------------
doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = blgl_folds,
  grid = 20
)

tune_res

## Plot tuning results --------------------------------------------------
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

## Select best model and finalize ------------------------------------------------------------
best_auc <- select_best(tune_res, metric = "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

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
final_res %>%
  collect_predictions() %>%
  mutate(correct = case_when(
    occ == .pred_class ~ "Correct",
    TRUE ~ "Incorrect"
  )) %>%
  bind_cols(blgl_test) %>%
  ggplot(aes(utm_e/1000, utm_n/1000, color = correct)) +
  geom_point(size = 0.5, alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c("gray80", "darkred"))+
  facet_wrap(~paste("Pool",pool), scales = "free")+
  theme_minimal()

## Calculate variable importance with the vip package --------------------------------------------------

final_rf %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(occ ~ ., data = juice(blgl_prep)) %>%
  vip(geom = "point")

## PDP data ------------------------------------------------------------

final_fitted <- final_res$.workflow[[1]]

### Explainer
blgl_explainer <- explain_tidymodels(
  final_fitted,
  data = dplyr::select(blgl_train, -occ),
  y = as.integer(blgl_train$occ),
  verbose = FALSE
)

## Model profile
pdp_depth <- model_profile(
  blgl_explainer,
  variables = "depth",
  N = NULL,
  groups = "pool"
)

plot(pdp_depth)

## PDP Plot ------------------------------------------------------------
pdp_df <- as_tibble(pdp_depth$agr_profiles) %>%
  filter(`_x_` < 5) %>%
  mutate(`_label_` = str_remove(`_label_`, "workflow_"))

ggplot(pdp_df, aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
    x = "Depth",
    y = "Predicted occurrence probability",
    color = "Pool",
    title = "Partial dependence plot for Largemouth bass occupancy"
  )
