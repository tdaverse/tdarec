data.frame(dist = I(list(eurodist, UScitiesD))) |>
  transform(pd = I(lapply(dist, ripserr::vietoris_rips))) |>
  subset(select = c(pd)) |>
  print() -> pd_data

# `num_coef` for `step_vpn_complex_polynomial()`

(nc_man <- num_coef(range = c(1L, 3L)))
grid_regular(nc_man)

# `poly_type` for `step_vpn_complex_polynomial()`

(pt_man <- poly_type(values = c("R", "S")))
grid_regular(pt_man)

# `img_sigma` for `step_vpn_persistence_image()`

(is_man <- img_sigma(range = c(100, 400), trans = NULL))
grid_regular(is_man)

(is_dat <- img_sigma() |> get_pers_max_frac(x = pd_data))
grid_regular(is_dat)

(is_hom <- img_sigma() |> get_pers_max_frac(x = pd_data, hom_degrees = seq(2L)))
grid_regular(is_hom)

# `num_levels` for `step_vpn_persistence_landscape()`

(nl_man <- num_levels(range = c(1L, 6L)))
grid_regular(nl_man)

# `weight_func_pl` for `step_vpn_persistence_landscape()`

(wfp_man <- weight_func_pl(values = c("triangle", "tricubic")))
grid_regular(wfp_man)

# `bandwidth` for `step_vpn_persistence_landscape()`

(b_man <- bandwidth(range = c(500, 1500), trans = NULL))
grid_regular(b_man)

(b_dat <- bandwidth() |> get_pers_max_frac(x = pd_data))
grid_regular(b_dat)

(b_hom <- bandwidth() |> get_pers_max_frac(x = pd_data, hom_degrees = seq(2L)))
grid_regular(b_hom)

# `weight_power` for `step_vpn_persistence_silhouette()`

(wp_man <- weight_power(range = c(1, 3)))
grid_regular(wp_man)

# `num_bars` for `step_vpn_tropical_coordinates()`

(nb_man <- num_bars(range = c(1L, 3L)))
grid_regular(nb_man)

# `num_bins` for `step_vpn_tent_template_functions()`

(nb_man <- num_bins(range = c(5L, 10L)))
grid_regular(nb_man)

# `tent_shift` for `step_vpn_tent_template_functions()`

(ts_man <- tent_shift(range = c(100, 200), trans = NULL))
grid_regular(ts_man)

(ts_dat <- tent_shift() |> get_pers_min_mult(x = pd_data))
grid_regular(ts_dat)

(ts_hom <- tent_shift() |> get_pers_min_mult(x = pd_data, hom_degrees = seq(2L)))
grid_regular(ts_hom)

# `tent_size` for `step_vpn_tent_template_functions()`

(ts_man <- tent_size(range = c(1000, 1300), trans = NULL))
grid_regular(ts_man)

(ts_dat <- tent_size() |> NULL(x = pd_data))
grid_regular(ts_dat)

(ts_hom <- tent_size() |> NULL(x = pd_data, hom_degrees = seq(2L)))
grid_regular(ts_hom)

# `block_size` for `step_vpn_persistence_block()`

(bs_man <- block_size(range = c(0, 0.5), trans = NULL))
grid_regular(bs_man)

(bs_dat <- block_size() |> NULL(x = pd_data))
grid_regular(bs_dat)

(bs_hom <- block_size() |> NULL(x = pd_data, hom_degrees = seq(2L)))
grid_regular(bs_hom)

