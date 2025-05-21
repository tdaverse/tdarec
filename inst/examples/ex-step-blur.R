topos <- data.frame(pix = I(list(volcano)))

blur_rec <- recipe(~ ., data = topos) %>% step_blur(pix)
blur_prep <- prep(blur_rec, training = topos)
blur_res <- bake(blur_prep, topos)

tidy(blur_rec, number = 1)
tidy(blur_prep, number = 1)

with_sigmas <- recipe(~ ., data = topos) %>% step_blur(pix, blur_sigmas = 10)
with_sigmas <- bake(prep(with_sigmas, training = topos), topos)

ops <- par(mfrow = c(1, 3))
image(topos$pix[[1]])
image(blur_res$pix[[1]])
image(with_sigmas$pix[[1]])
par(ops)
