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
    function(d) as.data.frame(ripserr::vietoris_rips(d))
  )),
  part = rep(c("train", "test"), each = 3)
)
print(phom_data)
head(phom_data$phom[[1]])

phom_train <- filter(phom_data, part == "train")
phom_test <- filter(phom_data, part == "test")

# choose maximum filtration parameters
max_death <- phom_train$phom %>% 
  lapply(function(d) d$death) %>% 
  unlist() %>% max()
max_persistence <- phom_train$phom %>% 
  lapply(function(d) d$death - d$birth) %>% 
  unlist() %>% max()

# build preprocessing recipe with custom settings
phom_train %>%
  recipe() %>%
  update_role(id, new_role = "id") %>%
  step_vpd_(phom, {param_vals}) %>%
  print() -> phom_rec
# tidy the prepped recipe step
tidy(phom_rec, number = 1)

# prepare recipe with training data
phom_rec %>%
  prep(training = phom_train, strings_as_factors = FALSE) %>%
  print() -> phom_prep
print(phom_prep)
# tidy the prepped recipe step
tidy(phom_prep, number = 1)

# preprocess training data
juice(phom_prep)

# preprocess testing data
bake(phom_test, object = phom_prep)

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
  step_phom_point_cloud(sample, engine = "ripserr") %>%
  step_vpd_(sample_phom, keep_original_cols = FALSE) %>%
  prep(training = sample_train, strings_as_factors = FALSE) ->
  sample_rec
print(sample_rec)

# preprocess training data
juice(sample_rec)

# preprocess testing data
bake(sample_test, object = sample_rec)
