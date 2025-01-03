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

test_that("`step_phom_image()` accepts different values and array dimensions", {
  rec <- recipe(~ ., data = dat) |> 
    step_phom_image(img)
  expect_no_error(bake(prep(rec, training = dat), new_data = dat))
})

test_that("`prep()` requires at least one variable", {
  rec <- recipe(~ ., data = dat) |> 
    step_phom_image()
  expect_error(prep(rec, training = dat), "name")
})

test_that("`prep()` checks names", {
  dat2 <- transform(dat, img_phom = 0)
  rec2 <- recipe(~ ., data = dat2) |> 
    step_phom_image(img)
  expect_message(prep(rec2, training = dat2), "[Nn]ew names")
})

test_that("`tunable()` return standard names", {
  rec <- recipe(~ ., data = dat) |> 
    step_phom_image(img)
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

phom_rec <- recipe(~ img, data = dat)

test_that("within-step and without-step calculations agree", {
  
  # no parameter specifications
  phom_extract <- phom_rec |> 
    step_phom_image(img, id = "")
  phom_train <- prep(phom_extract, training = dat)
  phom_test <- bake(phom_train, new_data = dat)
  manual_calc <- lapply(dat$img, ripserr::cubical)
  expect_equal(phom_test$img_phom, manual_calc)
  
  # data-determined maximum value
  phom_extract <- phom_rec |> 
    step_phom_image(img, value_max = 1000, id = "")
  phom_train <- prep(phom_extract, training = dat)
  phom_test <- bake(phom_train, new_data = dat)
  manual_calc <- lapply(
    dat$img,
    ripserr::cubical, threshold = 1000
  )
  expect_equal(phom_test$img_phom, manual_calc)
  
  # higher homological degree
  phom_extract <- phom_rec |> 
    step_phom_image(img, method = "compute_pairs", id = "")
  phom_train <- prep(phom_extract, training = dat)
  phom_test <- bake(phom_train, new_data = dat)
  # not equal to result using link-join method
  expect_error(expect_equal(phom_test$img_phom, manual_calc))
  manual_calc <- lapply(dat$img, ripserr::cubical, method = "cp")
  expect_equal(phom_test$img_phom, manual_calc)
  
})
