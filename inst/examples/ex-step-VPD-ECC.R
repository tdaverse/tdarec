# toy data set
klein_sampler <- function(n, prob = .5) {
  if (rbinom(1, 1, prob) == 0) {
    tdaunif::sample_klein_flat(n)
  } else {
    tdaunif::sample_klein_tube(n)
  }
}
phom_data <- data.frame(
  id = LETTERS[seq(6L)],
  phom = I(lapply(
    replicate(6L, klein_sampler(60), simplify = FALSE),
    \(d) as.data.frame(ripserr::vietoris_rips(d))
  )),
  part = rep(c("train", "test"), each = 3)
)
print(phom_data)
head(phom_data$phom[[1]])

phom_train <- filter(phom_data, part == "train")
phom_test <- filter(phom_data, part == "test")

# guess maximum filtration parameter
phom_train$phom |> 
  lapply(\(d) d$death) |> 
  unlist() |> max() |> 
  print() -> max_threshold
# create grid
phom_seq <- seq(0, round(max_threshold * 2.01, digits = 2), .01)

# build preprocessing recipe with custom settings
phom_train %>%
  recipe() %>%
  update_role(id, new_role = "id") %>%
  step_vpd_ecc(phom, dim_max = 2, scale_seq = phom_seq) %>%
  prep(training = phom_train, strings_as_factors = FALSE) ->
  phom_rec
print(phom_rec)

# build preprocessing recipe with default settings
phom_train %>%
  recipe() %>%
  update_role(id, new_role = "id") %>%
  step_vpd_ecc(phom) %>%
  prep(training = phom_train, strings_as_factors = FALSE) ->
  phom_rec
print(phom_rec)

# preprocess training data
juice(phom_rec)

# preprocess testing data
bake(phom_test, object = phom_rec)

# two-step recipe from sample to vectorization
sample_data <- data.frame(
  id = LETTERS[seq(6L)],
  sample = I(c(replicate(6L, klein_sampler(60), simplify = FALSE))),
  part = rep(c("train", "test"), each = 3L)
)
sample_train <- filter(sample_data, part == "train")
sample_test <- filter(sample_data, part == "test")

# build preprocessing recipe with default settings
sample_train %>%
  recipe() %>%
  update_role(id, new_role = "id") %>%
  step_phom(sample, engine = "ripserr") %>%
  step_vpd_ecc(sample_phom) %>%
  prep(training = sample_train, strings_as_factors = FALSE) ->
  sample_rec
print(sample_rec)

# preprocess training data
juice(sample_rec)

# preprocess testing data
bake(sample_test, object = sample_rec)
