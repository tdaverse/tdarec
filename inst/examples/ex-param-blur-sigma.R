img_dat <- data.frame(img = I(list(volcano)))

# options to calibrate gaussian noise
blur_sigma(range = c(0, 10))
blur_sigma() |> get_blur_range(x = img_dat)
