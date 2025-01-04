data(mnist)

cph_transform <- recipe(~ ., data = mnist_train) |> 
  step_phom_lattice(digit, value_max = 255L) |> 
  step_vpd_ecc(digit_phom)
cph_estimates <- prep(cph_transform, training = mnist_train)
cph_data <- bake(cph_estimates, mnist_test)

head(cph_data$digit_phom[[1]])
cph_data |> 
  select(contains("ecc")) |> 
  prcomp() ->
  cph_pca
plot(cph_pca$x[, 1:2], col = cph_data$label, pch = 16, asp = 1)

tidy(cph_transform)
tidy(cph_estimates)
