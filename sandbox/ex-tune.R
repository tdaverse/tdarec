## errors tuning parameters requiring finalization via bayesian optimization


library(tidymodels)
set.seed(0)
dat <- subset(mtcars, select = c(mpg, cyl, hp, wt))
folds <- vfold_cv(dat, v = 3L)

# recipe with no hyperparameters
rec <- recipe(mpg ~ ., data = dat)

# model with a hyperparameter not requiring finalization
spec <- rand_forest(trees = 5, min_n = tune()) |> 
  set_mode("regression") |> 
  set_engine("randomForest")

# tune over a grid
res_g <- tune_grid(
  spec,
  preprocessor = rec,
  resamples = folds
)
collect_metrics(res_g) |> count(min_n)
# tune using bayesian optimization
res <- tune_bayes(
  spec,
  preprocessor = rec,
  resamples = folds,
  iter = 2, initial = 6
)
collect_metrics(res) |> count(min_n)

# random forest model, still with a hyperparameter requiring finalization
spec_t <- rand_forest(trees = 5, min_n = tune(), mtry = tune()) |> 
  set_mode("regression") |> 
  set_engine("randomForest")
(mtry_param <- finalize(mtry(), subset(dat, select = c(-mpg))))

# doesn't work with bayesian optimization
res_g <- tune_grid(
  spec_t,
  preprocessor = rec,
  resamples = folds
)
collect_metrics(res_g) |> count(mtry)
res <- tune_bayes(
  spec_t,
  preprocessor = rec,
  resamples = folds,
  iter = 2, initial = 6
)
# bad syntax, but different error with bayesian optimization
res_g <- tune_grid(
  spec_t,
  preprocessor = rec,
  resamples = folds,
  param_info = parameters(list(mtry = mtry_param))
)
res <- tune_bayes(
  spec_t,
  preprocessor = rec,
  resamples = folds,
  param_info = parameters(list(mtry = mtry_param)),
  iter = 2, initial = 6
)
# works but is redundant
res_g <- tune_grid(
  spec_t,
  preprocessor = rec,
  resamples = folds,
  param_info = parameters(list(mtry = mtry_param, min_n = min_n()))
)
collect_metrics(res_g) |> count(mtry)
res <- tune_bayes(
  spec_t,
  preprocessor = rec,
  resamples = folds,
  param_info = parameters(list(mtry = mtry_param, min_n = min_n())),
  iter = 2, initial = 6
)
collect_metrics(res) |> count(mtry)

# Question 1: How should this task be done (whether grid or bayesian)?

# also tune a preprocessing hyperparameter
rec_pca <- recipe(mpg ~ ., data = dat) |> 
  step_pca(num_comp = tune())
# no longer works, but again different errors
res_g <- tune_grid(
  spec_t,
  preprocessor = rec_pca,
  resamples = folds,
  param_info = parameters(list(mtry = mtry_param, min_n = min_n()))
)
res <- tune_bayes(
  spec_t,
  preprocessor = rec_pca,
  resamples = folds,
  param_info = parameters(list(mtry = mtry_param, min_n = min_n())),
  iter = 2, initial = 6
)
# this also doesn't work, again with different errors
res_g <- tune_grid(
  spec_t,
  preprocessor = rec_pca,
  resamples = folds,
  param_info = parameters(list(num_comp = num_comp(),
                               mtry = mtry_param, min_n = min_n()))
)
res <- tune_bayes(
  spec_t,
  preprocessor = rec_pca,
  resamples = folds,
  param_info = parameters(list(num_comp = num_comp(),
                               mtry = mtry_param, min_n = min_n())),
  iter = 2, initial = 6
)

# Question 2: How should this task be done?
