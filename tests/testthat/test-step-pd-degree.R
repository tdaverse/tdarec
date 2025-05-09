set.seed(620726L)
ph <- data.frame(
  dimension = rep(c(0L, 1L, 2L), c(8, 5, 3)),
  birth = rnbinom(16, 5, .85),
  death = rnbinom(16, 5, .85)
)
dat <- data.frame(df = I(list(ph)), mat = I(list(as.matrix(ph))))

test_that("`step_pd_degree()` accepts base R classes", {
  rec <- recipe(~ ., data = dat) |> 
    step_pd_degree(df, mat)
  expect_no_error(bake(prep(rec, training = dat), new_data = dat))
})

test_that("`prep()` requires at least one variable", {
  rec <- recipe(~ ., data = dat) |> 
    step_pd_degree()
  expect_error(prep(rec, training = dat), "name")
})

test_that("`prep()` checks names", {
  dat2 <- transform(dat, df_0 = 0, mat_0 = 0)
  rec2 <- recipe(~ ., data = dat2) |> 
    step_pd_degree(df, mat)
  expect_message(prep(rec2, training = dat2), "[Nn]ew names")
})

test_that("no parameters are `tunable()`", {
  rec <- recipe(~ ., data = dat) |> 
    step_pd_degree(df, mat)
  tun <- tunable(rec$steps[[1]])
  expect_equal(nrow(tun), 0L)
})

test_that("within-step and without-step calculations agree", {
  df <- dat$df[[1L]]
  mat <- dat$mat[[1L]]
  recipe(~ ., data = dat) |> 
    step_pd_degree(df, mat) |> 
    prep(training = dat) |> 
    bake(new_data = dat) ->
    step_calc
  expect_equal(step_calc$df_0[[1L]], df[df$dimension == 0, , drop = FALSE])
  expect_equal(step_calc$df_1[[1L]], df[df$dimension == 1, , drop = FALSE])
  expect_equal(step_calc$df_2[[1L]], df[df$dimension == 2, , drop = FALSE])
  expect_equal(step_calc$mat_0[[1L]], mat[mat[, 1] == 0, , drop = FALSE])
  expect_equal(step_calc$mat_1[[1L]], mat[mat[, 1] == 1, , drop = FALSE])
  expect_equal(step_calc$mat_2[[1L]], mat[mat[, 1] == 2, , drop = FALSE])
})

test_that("`step_pd_degree()` accepts {ripserr} 'PHom' class", {
  skip_if_not_installed("ripserr")
  skip("Unnecessary; persistence data should be coerced to a standard format.")
  
  dat <- data.frame(
    roads = I(lapply(list(eurodist, UScitiesD), ripserr::vietoris_rips)),
    topos = I(lapply(list(volcano, 255 - volcano), ripserr::cubical))
  )
  rec <- recipe(~ ., data = dat) |> 
    step_pd_degree(roads, topos)
  expect_no_error(bake(prep(rec, training = dat), new_data = dat))
})

test_that("`step_pd_degree()` accepts {TDA} 'diagram' class", {
  skip_if_not_installed("TDA")
  skip("Unnecessary; persistence data should be coerced to a standard format.")
  
  dat <- data.frame(
    roads = I(lapply(
      list(eurodist, UScitiesD),
      \(d) TDA::ripsDiag(d, maxdimension = 2, maxscale = 10000)$diagram
    ))
  )
  rec <- recipe(~ ., data = dat) |> 
    step_pd_degree(roads)
  expect_no_error(bake(prep(rec, training = dat), new_data = dat))
})
