dat <- data.frame(dataset = I(list(eurodist, mtcars, Nile)))

test_that("`step_phom_point_cloud()` accepts multiple data classes", {
  rec <- recipe(~ dataset, data = dat) |> 
    step_phom_point_cloud(dataset)
  expect_no_error(bake(prep(rec, training = dat), new_data = dat))
})

test_that("`prep()` requires at least one variable", {
  rec <- recipe(~ dataset, data = dat) |> 
    step_phom_point_cloud()
  expect_error(prep(rec, training = dat), "name")
})

test_that("`prep()` checks names", {
  dat2 <- transform(dat, dataset_phom = 0)
  rec2 <- recipe(~ ., data = dat2) |> 
    step_phom_point_cloud(dataset)
  expect_message(prep(rec2, training = dat2), "[Nn]ew names")
})

test_that("`tunable()` return standard names", {
  rec <- recipe(~ dataset, data = dat) |> 
    step_phom_point_cloud(dataset)
  tun <- tunable(rec$steps[[1]])
  expect_equal(
    names(tun),
    c("name", "call_info", "source", "component", "component_id")
  )
  expect_equal(tun$name, "max_hom_degree")
  expect_equal(tun$source, "recipe")
  expect_true(is.list(tun$call_info))
  expect_equal(nrow(tun), 1L)
})

skip_if_not_installed("tdaunif")

set.seed(278918L)
dims <- rbinom(6, 1, .5) + 1L
sphere_data <- data.frame(
  dimension = dims,
  sample = I(lapply(dims, \(d) tdaunif::sample_sphere(60, dim = d))),
  part = rep(c("train", "test"), c(3, 3))
)
sphere_data |> 
  filter(part == "train") |> 
  select(-part) ->
  sphere_train
sphere_data |> 
  filter(part == "test") |> 
  select(-part) ->
  sphere_test

diam_max <- sphere_train$sample |> 
  lapply(dist) |> 
  sapply(max) |> 
  max()

phom_rec <- recipe(dimension ~ ., data = sphere_train)

test_that("within-step and without-step calculations agree", {
  
  # no parameter specifications
  phom_extract <- phom_rec |> 
    step_phom_point_cloud(sample, id = "")
  phom_train <- prep(phom_extract, training = sphere_train)
  phom_test <- bake(phom_train, new_data = sphere_test)
  manual_calc <- lapply(sphere_test$sample, ripserr::vietoris_rips)
  expect_equal(phom_test$sample_phom, manual_calc)
  
  # data-determined maximum diameter
  phom_extract <- phom_rec |> 
    step_phom_point_cloud(sample, diameter_max = diam_max, id = "")
  phom_train <- prep(phom_extract, training = sphere_train)
  phom_test <- bake(phom_train, new_data = sphere_test)
  manual_calc <- lapply(
    sphere_test$sample,
    ripserr::vietoris_rips, threshold = diam_max
  )
  expect_equal(phom_test$sample_phom, manual_calc)
  
  # non-trivial maximum diameter
  phom_extract <- phom_rec |> 
    step_phom_point_cloud(sample, diameter_max = diam_max/6, id = "")
  phom_train <- prep(phom_extract, training = sphere_train)
  phom_test <- bake(phom_train, new_data = sphere_test)
  manual_calc <- lapply(
    sphere_test$sample,
    ripserr::vietoris_rips, threshold = diam_max/6
  )
  expect_equal(phom_test$sample_phom, manual_calc)
  
  # higher homological degree
  phom_extract <- phom_rec |> 
    step_phom_point_cloud(sample, max_hom_degree = 2, id = "")
  phom_train <- prep(phom_extract, training = sphere_train)
  phom_test <- bake(phom_train, new_data = sphere_test)
  manual_calc <- lapply(sphere_test$sample, ripserr::vietoris_rips, max_dim = 2)
  expect_equal(phom_test$sample_phom, manual_calc)
  
})
