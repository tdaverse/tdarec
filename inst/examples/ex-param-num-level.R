data.frame(dist = I(list(eurodist, UScitiesD * 1.6))) %>% 
  transform(pd = I(lapply(dist, ripserr::vietoris_rips))) %>% 
  subset(select = c(pd)) %>% 
  print() -> pd_data

(lev_man <- num_level(range = c(1, 24)))
grid_regular(lev_man)

(lev_dat <- num_level() %>% get_pairs_max(x = pd_data))
grid_regular(lev_dat)

(lev_hom <- num_level() %>% get_pairs_max(x = pd_data, hom_degrees = seq(2L)))
grid_regular(lev_hom)
