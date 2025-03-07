data.frame(dist = I(list(eurodist, UScitiesD))) |> 
  transform(pd = I(lapply(dist, ripserr::vietoris_rips))) |> 
  subset(select = c(pd)) |> 
  print() -> pd_data

(sig_man <- img_sigma(range = c(-1, 1)))
grid_regular(sig_man)

# FIXME: Preclude infinite values.
(sig_hom <- img_sigma() |> get_persistence_range(x = pd_data, hom_degree = 0))
grid_regular(sig_hom)

(sig_hom <- img_sigma() |> get_persistence_range(x = pd_data, hom_degree = 1))
grid_regular(sig_hom)
