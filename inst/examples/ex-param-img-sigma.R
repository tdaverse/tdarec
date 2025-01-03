data.frame(dist = I(list(eurodist, UScitiesD))) |> 
  transform(pd = I(lapply(dist, ripserr::vietoris_rips))) |> 
  subset(select = c(pd)) |> 
  print() -> pd_data

(sig_man <- img_sigma(range = c(-1, 1)))
grid_regular(sig_man)
