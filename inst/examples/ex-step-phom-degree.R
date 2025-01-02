roads <- data.frame(
  dist = I(list(eurodist, UScitiesD)),
  pix = I(list(volcano, 255 - volcano))
)

ph_rec <- recipe(~ ., data = roads) |> 
  step_phom_point_cloud(dist, max_hom_degree = 2, keep_original_cols = FALSE) |> 
  step_phom_image(pix, keep_original_cols = FALSE) |> 
  step_phom_degree(dist_phom, pix_phom)
ph_prep <- prep(ph_rec, training = roads)
ph_res <- bake(ph_prep, roads)

tidy(ph_rec, number = 3)
tidy(ph_prep, number = 3)

with_degs <- recipe(~ ., data = roads) |> 
  step_phom_point_cloud(dist, max_hom_degree = 2, keep_original_cols = FALSE) |> 
  step_phom_image(pix, keep_original_cols = FALSE) |> 
  step_phom_degree(dist_phom, pix_phom, hom_degrees = c(1, 2))
with_degs <- prep(with_degs, training = roads)
bake(with_degs, roads)
