data.frame(dist = I(list(eurodist, UScitiesD))) |> 
  transform(pd = I(lapply(dist, ripserr::vietoris_rips))) |> 
  subset(select = c(pd)) |> 
  print() -> pd_data

(sig_man <- img_sigma(range = c(10, 50), trans = NULL))
grid_regular(sig_man)

(sig_hom <- img_sigma() |> get_pers_frac_range(x = pd_data, hom_degree = 0))
grid_regular(sig_hom)

(sig_hom <- img_sigma() |> get_pers_frac_range(x = pd_data, hom_degree = 1))
grid_regular(sig_hom)
