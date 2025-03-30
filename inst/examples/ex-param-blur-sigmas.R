img_dat <- data.frame(img = I(list(volcano)))

(blur_man <- blur_sigmas(range = c(0, 3)))
grid_regular(blur_man)

(blur_fin <- blur_sigmas() |> get_blur_range(x = img_dat))
grid_regular(blur_fin)
