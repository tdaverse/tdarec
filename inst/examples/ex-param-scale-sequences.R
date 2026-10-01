data.frame(dist = I(list(eurodist, UScitiesD * 1.6))) %>%
  transform(pd = I(lapply(dist, ripserr::vietoris_rips))) %>%
  subset(select = c(pd)) ->
  pd_data
# grid_min & grid_max
(gmin_man <- grid_min(range = c(0, 1000)))
grid_regular(gmin_man)
(gmin_dat <- grid_min() %>% get_scale_range(x = pd_data, hom_degree = 1L, bound = "lower"))
grid_regular(gmin_dat)
(gmax_man <- grid_max(range = c(1000, 5000)))
grid_regular(gmax_man)
(gmax_dat <- grid_max() %>% get_scale_range(x = pd_data, hom_degree = 1L, bound = "upper"))
grid_regular(gmax_dat)

# grid_by
(gby_man <- grid_by(range = c(5, 10)))
grid_regular(gby_man)
(gby_dat <- grid_by() %>% get_scale_increment(x = pd_data, hom_degree = 1L))
grid_regular(gby_dat)
