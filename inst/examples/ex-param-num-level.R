data.frame(dist = I(list(eurodist, UScitiesD))) |> 
  transform(pd = I(lapply(dist, ripserr::vietoris_rips))) |> 
  subset(select = c(pd)) |> 
  print() -> pd_data

num_level(range = c(1, 24))
num_level() |> get_level_range(x = pd_data)
num_level() |> get_level_range(x = pd_data, hom_degrees = seq(2L))
