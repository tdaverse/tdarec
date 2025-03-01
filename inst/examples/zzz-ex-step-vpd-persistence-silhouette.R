library(recipes)

data(permeability_qsar, package = "modeldata")
permeability_qsar |> 
  transform(perm_cut = cut(permeability, breaks = seq(0, 60, 10))) |> 
  subset(select = -permeability) |> 
  tidyr::nest(chem_fp = -perm_cut) |> 
  print() -> perm_dat
recipe(perm_cut ~ chem_fp, data = perm_dat) |> 
  step_phom_point_cloud(chem_fp, max_hom_degree = 2) |> 
  step_vpd_persistence_silhouette(chem_fp_phom, hom_degree = 1) |> 
  step_pca(starts_with("chem_fp_phom_"), num_comp = 2) |>
  print() -> perm_rec
perm_est <- prep(perm_rec, training = perm_dat)
perm_res <- bake(perm_est, new_data = perm_dat)

tidy(perm_rec)
tidy(perm_rec, number = 2)
tidy(perm_est, number = 2)

with(perm_res, {
  plot(PC1, PC2, type = "n", asp = 1)
  text(PC1, PC2, labels = perm_cut)
})
