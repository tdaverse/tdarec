dat <- data.frame(
  roads = I(list(eurodist, UScitiesD)),
  topos = I(list(volcano, 255 - volcano))
)

ph_rec <- recipe(~ ., data = dat) |> 
  step_phom_point_cloud(roads, keep_original_cols = FALSE) |> 
  step_phom_lattice(topos, keep_original_cols = FALSE) |> 
  step_phom_degree(roads_phom, topos_phom)
ph_prep <- prep(ph_rec, training = dat)
(ph_res <- bake(ph_prep, dat))

tidy(ph_rec, number = 3)
tidy(ph_prep, number = 3)

with_degs <- recipe(~ ., data = dat) |> 
  step_phom_point_cloud(roads, keep_original_cols = FALSE) |> 
  step_phom_lattice(topos, keep_original_cols = FALSE) |> 
  step_phom_degree(roads_phom, topos_phom, hom_degrees = c(1, 2))
with_degs <- prep(with_degs, training = dat)
bake(with_degs, dat)
