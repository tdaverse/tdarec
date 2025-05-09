topos <- data.frame(pix = I(list(volcano)))

ph_rec <- recipe(~ ., data = topos) %>% 
  step_pd_raster(pix)
ph_prep <- prep(ph_rec, training = topos)
ph_res <- bake(ph_prep, topos)

tidy(ph_rec, number = 1)
tidy(ph_prep, number = 1)

with(ph_res$pix_pd[[1]], plot(
  x = birth, y = death, pch = dimension + 1, col = dimension + 1,
  xlab = NA, ylab = "", asp = 1
))

with_max <- recipe(~ ., data = topos) %>% 
  step_pd_raster(pix, value_max = 150)
with_max <- prep(with_max, training = topos)
bake(with_max, topos)
