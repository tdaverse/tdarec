roads <- data.frame(dist = I(list(eurodist, UScitiesD)))
rec <- recipe(~ ., data = roads)
ph_trans <- rec |> 
  step_phom_point_cloud(dist, max_hom_degree = 1, filtration = "Rips")
ph_estimates <- prep(ph_trans, training = roads)
ph_data <- bake(ph_estimates, roads)

par(mfrow = c(1, 2), mar = c(2, 2, 0, 0) + 0.1)
for (i in seq(nrow(ph_data))) {
  with(ph_data$dist_phom[[i]], plot(
    x = birth, y = death, pch = dimension + 1, col = dimension + 1,
    xlab = NA, ylab = "", asp = 1
  ))
}

with_thres <- rec |> 
  step_phom_point_cloud(dist, max_hom_degree = 1, diameter_max = 200)
with_thres <- prep(with_thres, training = roads)
bake(with_thres, roads)

tidy(ph_trans, number = 1)
tidy(ph_estimates, number = 1)
