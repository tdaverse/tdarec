dat <- data.frame(
  roads = I(list(eurodist, UScitiesD * 1.6)),
  topos = I(list(volcano, 255 - volcano))
)

ph_rec <- recipe(~ ., data = dat) %>% 
  step_pd_point_cloud(roads, keep_original_cols = FALSE) %>% 
  step_pd_raster(topos, keep_original_cols = FALSE) %>% 
  step_pd_degree(roads_pd, topos_pd)
ph_prep <- prep(ph_rec, training = dat)
(ph_res <- bake(ph_prep, dat))

tidy(ph_rec, number = 3)
tidy(ph_prep, number = 3)

with_degs <- recipe(~ ., data = dat) %>% 
  step_pd_point_cloud(roads, keep_original_cols = FALSE) %>% 
  step_pd_raster(topos, keep_original_cols = FALSE) %>% 
  step_pd_degree(roads_pd, topos_pd, hom_degrees = c(1, 2))
with_degs <- prep(with_degs, training = dat)
bake(with_degs, dat)
