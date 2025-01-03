img_dat <- data.frame(img = I(list(volcano)))

(blur_man <- blur_sigma(range = c(0, 3)))
grid_regular(blur_man)

(blur_fin <- blur_sigma() |> get_blur_range(x = img_dat))
grid_regular(blur_fin)
