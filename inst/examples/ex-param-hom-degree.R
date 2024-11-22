# toy data set
klein_sampler <- function(n, prob = .5) {
  if (rbinom(1, 1, prob) == 0) {
    tdaunif::sample_klein_flat(n)
  } else {
    tdaunif::sample_klein_tube(n)
  }
}
sample_data <- data.frame(
  id = LETTERS[seq(4L)],
  sample = I(c(replicate(4L, klein_sampler(60), simplify = FALSE)))
)

# options to calibrate homological degree
hom_degree(range = c(2, 5))
hom_degree() |> get_hom_range(x = sample_data[, 2, drop = FALSE])
hom_degree() |> get_hom_range(x = sample_data[, 2, drop = FALSE], max_dim = 5)

# heterogeneous data types
hetero_data <- tibble(dataset = list(mtcars, nhtemp, eurodist, HairEyeColor))
hetero_data |> 
  mutate(class = vapply(dataset, \(x) class(x)[[1L]], ""))
get_hom_range(
  hom_degree(),
  hetero_data,
  max_dim = 60
)
