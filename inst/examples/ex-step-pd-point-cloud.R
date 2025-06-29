roads <- data.frame(dist = I(list(eurodist, UScitiesD * 1.6)))

ph_rec <- recipe(~ ., data = roads) %>% 
  step_pd_point_cloud(dist, max_hom_degree = 1, filtration = "Rips")
ph_prep <- prep(ph_rec, training = roads)
ph_res <- bake(ph_prep, roads)

tidy(ph_rec, number = 1)
tidy(ph_prep, number = 1)

ops <- par(mfrow = c(1, 2), mar = c(2, 2, 0, 0) + 0.1)
for (i in seq(nrow(ph_res))) {
  with(ph_res$dist[[i]], plot(
    x = birth, y = death, pch = dimension + 1, col = dimension + 1,
    xlab = NA, ylab = "", asp = 1
  ))
}
par(ops)

with_max <- recipe(~ ., data = roads) %>% 
  step_pd_point_cloud(dist, max_hom_degree = 1, diameter_max = 200)
with_max <- prep(with_max, training = roads)
bake(with_max, roads)
