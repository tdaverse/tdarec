# toy data set
klein_sampler <- function(n, prob = .5) {
  if (rbinom(1, 1, prob) == 0) {
    tdaunif::sample_klein_flat(n)
  } else {
    tdaunif::sample_klein_tube(n)
  }
}
pd_data <- data.frame(
  id = LETTERS[seq(6L)],
  pd = I(lapply(
    replicate(6L, klein_sampler(60), simplify = FALSE),
    function(d) as.data.frame(ripserr::vietoris_rips(d))
  )),
  part = rep(c("train", "test"), each = 3)
)
print(pd_data)
head(pd_data$pd[[1]])

pd_train <- filter(pd_data, part == "train")
pd_test <- filter(pd_data, part == "test")

# choose maximum filtration parameters
max_death <- pd_train$pd %>% 
  lapply(function(d) d$death) %>% 
  unlist() %>% max()
max_persistence <- pd_train$pd %>% 
  lapply(function(d) d$death - d$birth) %>% 
  unlist() %>% max()

# build preprocessing recipe with custom settings
pd_train %>%
  recipe() %>%
  update_role(id, new_role = "id") %>%
  step_vpd_(pd, {param_vals}) %>%
  print() -> pd_rec
# tidy the prepped recipe step
tidy(pd_rec, number = 1)

# prepare recipe with training data
pd_rec %>%
  prep(training = pd_train, strings_as_factors = FALSE) %>%
  print() -> pd_prep
print(pd_prep)
# tidy the prepped recipe step
tidy(pd_prep, number = 1)

# preprocess training data
juice(pd_prep)

# preprocess testing data
bake(pd_test, object = pd_prep)

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
  step_pd_point_cloud(sample, engine = "ripserr") %>%
  step_vpd_(sample, keep_original_cols = FALSE) %>%
  prep(training = sample_train, strings_as_factors = FALSE) ->
  sample_rec
print(sample_rec)

# preprocess training data
juice(sample_rec)

# preprocess testing data
bake(sample_test, object = sample_rec)
