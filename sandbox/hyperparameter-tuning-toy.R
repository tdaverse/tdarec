library(tidymodels)

cars_train <- mtcars[seq(1, 24), ]
cars_test <- mtcars[seq(25, 32), ]
cars_folds <- vfold_cv(cars_train, v = 8)

# with fixed hyperparameters

# write pre-processing steps with careful role assignment
recipe(cars_train) |> 
  update_role(mpg, new_role = "outcome") |> 
  step_center(all_numeric() & ! all_outcomes()) |> 
  step_pca(all_numeric() & ! all_outcomes(), num_comp = 6L) |> 
  update_role(starts_with("PC"), new_role = "predictor") |> 
  print() -> cars_rec

# check that pre-processed data are appropriate for model
cars_rec |> 
  prep() |> 
  bake(new_data = cars_train)

# specify model with hyperparameter values
rand_forest(
  trees = 500,
  min_n = 3,
  mtry = 3
) |> 
  set_mode("regression") |> 
  set_engine("randomForest") |> 
  print() -> cars_spec

# fit model to specification and pre-processing
fit(
  cars_spec,
  cars_rec |> prep() |> formula(),
  data = cars_rec |> prep() |> bake(new_data = cars_train)
)

# with hyperparameter tuning

# rewrite pre-processing steps with tuning
recipe(cars_train) |> 
  update_role(mpg, new_role = "outcome") |> 
  step_center(all_numeric() & ! all_outcomes()) |> 
  step_pca(all_numeric() & ! all_outcomes(), num_comp = tune()) |> 
  update_role(starts_with("PC"), new_role = "predictor") |> 
  print() -> cars_rec
extract_parameter_set_dials(cars_rec)

# finalize recipe with excessive hyperparameter value & pre-process again
cars_rec |> 
  finalize_recipe(parameters = list(num_comp = 100L)) |> 
  prep() |> 
  bake(new_data = cars_train) ->
  cars_bake
print(cars_bake)
# finalize hyperparameter range based on finalized recipe
num_comp_fin <- finalize(num_comp(), cars_bake |> select(starts_with("PC")))

# specify model with hyperparameter values
rand_forest(
  trees = 500,
  min_n = tune("rf_node"),
  mtry = tune("rf_pred")
) |> 
  set_mode("regression") |> 
  set_engine("randomForest") |> 
  print() -> cars_spec
extract_parameter_set_dials(cars_spec)
# finalize hyperparameter ranges based on finalized recipe
min_n_fin <- finalize(min_n(), cars_bake |> select(starts_with("PC")))
mtry_fin <- finalize(mtry(), cars_bake |> select(starts_with("PC")))

# build and evaluate workflow
cars_wflow <- workflow() |> 
  add_recipe(cars_rec) |> 
  add_model(cars_spec) |> 
  tune_bayes(
    resamples = cars_folds,
    param_info = parameters(list(
      num_comp = num_comp_fin,
      rf_node = min_n_fin, rf_pred = mtry_fin
    )),
    metrics = metric_set(rmse)
  )

# optimize hyperparameters
collect_metrics(cars_wflow)

# fit model using best hyperparameters
(cars_best <- select_best(cars_wflow, metric = "rmse"))
cars_fin <- prep(finalize_recipe(cars_rec, cars_best))
fit(
  finalize_model(cars_spec, cars_best),
  formula(cars_fin),
  data = bake(cars_fin, new_data = cars_train)
) |> 
  print() -> cars_fit

# evaluate best-fit model on heldout data
cars_fit |> 
  predict(new_data = bake(cars_fin, new_data = cars_test)) |> 
  bind_cols(select(cars_test, mpg)) |> 
  rmse(truth = mpg, estimate = .pred)



# {randomForest}
iris.rf <- randomForest::randomForest(
  Species ~ .,
  data=iris,
  importance=TRUE, proximity=TRUE
)
# NB: `randomForest()` can freeze when all predictors have zero variance.
stop("Code below freezes.")
iris.rf <- randomForest::randomForest(
  Species ~ .,
  data=data.frame(Species = iris$Species, a = 0, b = 1, c = 2),
  importance=TRUE, proximity=TRUE
)
# {ranger}
ranger::ranger(Species ~ .,
               data = iris)
ranger::ranger(Species ~ .,
               data = data.frame(Species = iris$Species, a = 0, b = 1, c = 2))
