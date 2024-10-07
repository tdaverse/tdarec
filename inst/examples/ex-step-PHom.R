# toy data set
klein_sampler <- function(n, prob = .5) {
  if (rbinom(1, 1, prob) == 0) {
    tdaunif::sample_klein_flat(n)
  } else {
    tdaunif::sample_klein_tube(n)
  }
}
sample_data <- data.frame(
  id = LETTERS[seq(6L)],
  sample = I(c(replicate(6L, klein_sampler(60), simplify = FALSE))),
  part = rep(c("train", "test"), each = 3L)
)
print(sample_data)
head(sample_data$sample[[1]])

sample_train <- filter(sample_data, part == "train")
sample_test <- filter(sample_data, part == "test")

# build preprocessing recipe with default settings
sample_train %>%
  recipe() %>%
  update_role(id, new_role = "id") %>%
  step_phom(sample) %>%
  prep(training = sample_train, strings_as_factors = FALSE) ->
  sample_rec
print(sample_rec)

# preprocess training data
juice(sample_rec)

# preprocess testing data
bake(sample_test, object = sample_rec)

