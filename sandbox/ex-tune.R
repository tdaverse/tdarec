stop("Use `update()` per topepo's suggestion.")
# https://github.com/tidymodels/tune/issues/995#issuecomment-2757967821

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
collect_metrics(res_g) |> head()
# tune using bayesian optimization
res <- tune_bayes(
  spec,
  preprocessor = rec,
  resamples = folds,
  iter = 2, initial = 6
)
collect_metrics(res) |> head()

# random forest model, still with a hyperparameter requiring finalization
spec_t <- rand_forest(trees = 5, min_n = tune(), mtry = tune()) |> 
  set_mode("regression") |> 
  set_engine("randomForest")
(mtry_param <- finalize(mtry(), subset(dat, select = c(-mpg))))

# Answer 1: Prepare a workflow and tune over a grid or list of finalized params.

wflow <- workflow() |> 
  add_recipe(rec) |> 
  add_model(spec_t)

res_g <- tune_grid(
  wflow,
  resamples = folds,
  grid = grid_regular(mtry_param, min_n(), levels = 3)
)
collect_metrics(res_g) |> head()

res <- tune_bayes(
  wflow,
  resamples = folds,
  param_info = parameters(list(mtry = mtry_param, min_n = min_n())),
  iter = 2, initial = 6
)
collect_metrics(res) |> head()

# also tune a preprocessing hyperparameter
rec_pca <- recipe(mpg ~ ., data = dat) |> 
  step_pca(num_comp = tune())

# Answer 2: Additionally finalize recipe params as necessary.

(num_comp_param <- finalize(num_comp(), subset(dat, select = c(-mpg))))

wflow <- workflow() |> 
  add_recipe(rec_pca) |> 
  add_model(spec_t)

grid_wf <- merge(
  grid_regular(num_comp_param, levels = 3),
  grid_regular(mtry_param, min_n(), levels = 3)
)

res_g <- tune_grid(
  wflow,
  resamples = folds,
  grid = grid_wf
)
collect_metrics(res_g) |> head()

res <- tune_bayes(
  wflow,
  resamples = folds,
  param_info = parameters(list(
    num_comp = num_comp_param,
    mtry = mtry_param, min_n = min_n()
  )),
  iter = 2, initial = 6
)
collect_metrics(res) |> head()
