topos <- data.frame(pix = I(list(volcano)))
rec <- recipe(~ ., data = topos)
ph_trans <- rec |> 
  step_phom_image(pix)
ph_estimates <- prep(ph_trans, training = topos)
ph_data <- bake(ph_estimates, topos)

with(ph_data$pix_phom[[1]], plot(
  x = birth, y = death, pch = dimension + 1, col = dimension + 1,
  xlab = NA, ylab = "", asp = 1
))

with_thres <- rec |> 
  step_phom_image(pix, value_max = 150)
with_thres <- prep(with_thres, training = topos)
bake(with_thres, topos)

tidy(ph_trans, number = 1)
tidy(ph_estimates, number = 1)
