topos <- data.frame(pix = I(list(volcano)))

blur_rec <- recipe(~ ., data = topos) |> step_blur(pix)
blur_prep <- prep(blur_rec, training = topos)
blur_res <- bake(blur_prep, topos)

tidy(blur_rec, number = 1)
tidy(blur_prep, number = 1)

with_sigma <- recipe(~ ., data = topos) |> step_blur(pix, sigma = 10)
with_sigma <- bake(prep(with_sigma, training = topos), topos)

ops <- par(mfrow = c(1, 3))
image(topos$pix[[1]])
image(blur_res$pix_blur[[1]])
image(with_sigma$pix_blur[[1]])
par(mfrow = ops$mfrow)
