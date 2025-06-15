sq1 <- matrix(byrow = TRUE, nrow = 6L, c(
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0
))
sq2 <- matrix(byrow = TRUE, nrow = 6L, c(
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 1, 1, 0, 0, 0,
  0, 1, 1, 1, 1, 1, 1, 0,
  0, 1, 1, 1, 1, 1, 1, 0,
  0, 1, 1, 1, 1, 1, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0
))
sq3 <- matrix(byrow = TRUE, nrow = 6L, c(
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 1, 1, 0, 0, 0,
  0, 1, 0, 0, 1, 1, 1, 0,
  0, 1, 0, 0, 1, 0, 1, 0,
  0, 1, 1, 1, 1, 1, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0
))
sq4 <- matrix(byrow = TRUE, nrow = 6L, c(
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 1, 1, 0, 0, 0,
  0, 1, 0, 0, 1, 1, 1, 0,
  0, 1, 0, 0, 1, 1, 1, 0,
  0, 1, 1, 1, 1, 1, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0
))
sq5 <- matrix(byrow = TRUE, nrow = 6L, c(
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 1, 1, 0, 0, 0,
  0, 1, 1, 1, 1, 0, 0, 0,
  0, 1, 1, 1, 1, 0, 0, 0,
  0, 1, 1, 1, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0
))
sq6 <- matrix(byrow = TRUE, nrow = 6L, c(
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0
))
cube <- array(c(sq1, sq2, sq3, sq4, sq5, sq6), dim = c(6, 8, 6))

dat <- data.frame(img = I(list(matrix(runif(120)), volcano, cube)))

test_that("`step_pd_raster()` accepts different values and dimensions", {
  rec <- recipe(~ ., data = dat) |> 
    step_pd_raster(img)
  expect_no_error(bake(prep(rec, training = dat), new_data = dat))
})

test_that("`prep()` passes when no variables are selected", {
  rec <- recipe(~ ., data = dat) |> 
    step_pd_raster()
  expect_no_error(prep(rec, training = dat))
})

test_that("`prep()` checks for list columns", {
  dat2 <- transform(dat, img = 0)
  rec2 <- recipe(~ ., data = dat2) |> 
    step_pd_raster(img)
  expect_error(prep(rec2, training = dat2), "list-columns")
})

test_that("`tunable()` return standard names", {
  rec <- recipe(~ ., data = dat) |> 
    step_pd_raster(img)
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

pd_rec <- recipe(~ img, data = dat)

test_that("within-step and without-step calculations agree", {
  
  # no parameter specifications
  pd_extract <- pd_rec |> 
    step_pd_raster(img, id = "")
  pd_train <- prep(pd_extract, training = dat)
  pd_test <- bake(pd_train, new_data = dat)
  manual_calc <- lapply(dat$img, ripserr::cubical)
  expect_equal(pd_test$img, manual_calc)
  
  # data-determined maximum value
  pd_extract <- pd_rec |> 
    step_pd_raster(img, value_max = 1000, id = "")
  pd_train <- prep(pd_extract, training = dat)
  pd_test <- bake(pd_train, new_data = dat)
  manual_calc <- lapply(
    dat$img,
    ripserr::cubical, threshold = 1000
  )
  expect_equal(pd_test$img, manual_calc)
  
  # higher homological degree
  pd_extract <- pd_rec |> 
    step_pd_raster(img, method = "compute_pairs", id = "")
  pd_train <- prep(pd_extract, training = dat)
  pd_test <- bake(pd_train, new_data = dat)
  # not equal to result using link-join method
  expect_error(expect_equal(pd_test$img, manual_calc))
  manual_calc <- if (.ripserr_version < "0.2.0") {
    lapply(dat$img, ripserr::cubical, method = 1)
  } else {
    lapply(dat$img, ripserr::cubical, method = "cp")
  }
  expect_equal(pd_test$img, manual_calc)
  
})
