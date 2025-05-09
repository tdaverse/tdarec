#' This script provides helper files for the code generating scripts
#' 'build-pre/build-*.R'.
#'
#' Refer to the following resources for guidance:
#' https://blog.r-hub.io/2020/02/10/code-generation/
#' https://www.tidymodels.org/learn/develop/recipes/
#'
#' Customization steps are tagged "CHOICE:".

#' SETUP

#' Attach packages.

library(TDAvec)
# library(devtools)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)

#' RETRIEVAL

#' Retrieve functions and their attributes from {TDAvec}.

lsf.str("package:TDAvec") |> 
  enframe(name = NULL, value = "structure") |> 
  mutate(name = map_chr(structure, as.character)) |> 
  filter(grepl("^compute[A-Za-z]+$", name)) |> 
  # exclude helper function
  filter(name != "computeLimits") |> 
  mutate(fun = map(name, \(s) getFromNamespace(s, ns = "TDAvec"))) |> 
  mutate(args = map(fun, formals)) |> 
  print() -> tdavec_functions

# # CHOICE: convenience wrappers for {TDAvec} functions
# computeClassicalPersistenceLandscape <- function(
#     D, homDim, scaleSeq, k = 1L
# ) {
#   computePersistenceLandscape(D, homDim, scaleSeq, k, FALSE)
# }
# computeGeneralizedPersistenceLandscape <- function(
#     D, homDim, scaleSeq, k = 1L, kernel = "triangle", h = NULL
# ) {
#   computePersistenceLandscape(D, homDim, scaleSeq, k, TRUE, kernel, h)
# }

# tabulate original argument defaults
tdavec_functions |> 
  select(fun = name, args) |> 
  mutate(args = lapply(args, as.list)) |> 
  mutate(args = lapply(args, enframe, name = "arg", value = "default")) |> 
  unnest(args) |> 
  # dataset is not a tunable parameter
  filter(arg != "D") |> 
  # scale sequences are handled separately and should default to `NULL`
  filter(arg != "scaleSeq" & arg != "xSeq" & arg != "ySeq") |> 
  # encase default as a code string (retaining quotes)
  mutate(default = sapply(default, deparse)) |> 
  print(n = Inf) -> tdavec_defaults

#' Retrieve and adapt documentation from {TDAvec}.

# retrieve function titles from {TDAvec}
# https://stackoverflow.com/a/46712167/4556798
# char_by_tag <- function(x, tag) {
#   x_tags <- vapply(x, function(e) attr(e, "Rd_tag"), "")
#   vapply(x[x_tags == tag], as.character, "")
# }
tdavec_functions |> 
  select(name) |> 
  mutate(help = map(name, help, package = "TDAvec")) |> 
  mutate(doc = map(help, utils:::.getHelpFile)) |> 
  # mutate(title = map_chr(doc, char_by_tag, tag = "\\title")) |> 
  mutate(title = map_chr(
    doc,
    \(x) unlist(x[vapply(x, function(e) attr(e, "Rd_tag"), "") == "\\title"])
  )) |> 
  print() -> tdavec_content

# words to keep capitalized
proper_names <- c("Betti", "Euler")

#' ADAPTATION

#' Rename functions and parameters.

# CHOICE: rename functions for step names & documentation
vec_renames <- c(
  EulerCharacteristic = "EulerCharacteristicCurve",
  NormalizedLife = "NormalizedLifeCurve",
  PersistentEntropy = "PersistentEntropySummary",
  Stats = "DescriptiveStatistics",
  TemplateFunction = "TentTemplateFunctions"
)
tdavec_functions |> 
  select(name) |> 
  mutate(rename = gsub("^compute", "", name)) |> 
  mutate(rename = ifelse(
    rename %in% names(vec_renames),
    unname(vec_renames[rename]),
    rename
  )) |> 
  print() -> tdavec_renames

# all parameters used by {TDAvec} vectorization functions
tdavec_functions |> 
  transmute(name, arg = map(args, names)) |> 
  unnest(arg) |> 
  nest(funs = c(name)) |> 
  # filter(arg != "D") |> 
  arrange(desc(map_int(funs, nrow))) |> 
  print() -> tdavec_args
# which vectorizations use which parameters
tdavec_args |> 
  mutate(funs = map(funs, deframe)) |> 
  mutate(funs = map(funs, gsub, pattern = "compute", replacement = "")) |> 
  mutate(funs = map_chr(funs, paste, collapse = ", ")) |> 
  print(n = Inf)

# CHOICE: assign parameter names (existing or new) for recipe steps
arg_params <- c(
  homDim = "hom_degree",
  maxhomDim = "max_hom_degree",
  scaleSeq = "xseq",
  xSeq = "xseq",
  ySeq = "yseq",
  evaluate = "evaluate",
  # ComplexPolynomial
  m = "num_coef",
  polyType = "poly_type",
  # PersistenceBlock
  tau = "block_size",
  # PersistenceImage
  sigma = "img_sigma",
  # PersistenceLandscape
  k = "num_levels",
  generalized = "generalized",
  kernel = "weight_func_pl",
  h = "bandwidth",
  # TODO: Check that the silhouette function uses this as a distance power.
  # PersistenceSilhouette
  p = "weight_power",
  # TemplateFunction
  delta = "tent_size",
  d = "num_bins",
  epsilon = "tent_shift",
  # TropicalCoordinates
  r = "num_bars"
)

# document tunable parameters
param_docs <- list(
  hom_degree = c(
    "The homological degree of the features to be transformed."
  ),
  max_hom_degree = c(
    "The highest degree, starting from 0, of the features to be transformed."
  ),
  xseq = c(
    "A discretization grid, as an increasing numeric vector.",
    "`xseq` overrides the other `x*` parameters with a warning."
  ),
  xother = c(
    "Limits and resolution of a discretization grid;",
    "specify only one of `xlen` and `xby`."
  ),
  yseq = c(
    "Combined with `xseq` to form a 2-dimensional discretization grid."
  ),
  yother = c(
    "Limits and resolution of a discretization grid;",
    "specify only one of `ylen` and `yby`."
  ),
  evaluate = c(
    "The method by which to vectorize continuous functions over a grid,",
    "either 'intervals' or 'points'.",
    "Some functions only admit one method."
  ),
  # ComplexPolynomial
  num_coef = c(
    "The number of coefficients of a convex polynomial",
    "fitted to finite persistence pairs."
  ),
  poly_type = c(
    "The type of complex polynomial to fit ('R', 'S', or 'T')."
  ),
  # PersistenceImage
  img_sigma = c(
    "The standard deviation of the gaussian distribution",
    "convolved with persistence diagrams to obtain persistence images."
  ),
  # PersistenceLandscape
  num_levels = c(
    "The number of levels of a persistence landscape to vectorize.",
    "If `num_levels` is greater than the length of a landscape,",
    "then additional levels of zeros will be included."
  ),
  generalized = c(
    "Logical indicator to compute generalized functions."
  ),
  # TODO: Replace with `dials::weight_func()` iff choices agree.
  # weight_func_pl = c("parsnip::nearest_neighbor"),
  weight_func_pl = c(
    "A _single_ character for the type of kernel function",
    "used to compute generalized landscapes."
  ),
  bandwidth = c(
    "The bandwidth of a kernel function."
  ),
  # PersistenceSilhouette
  weight_power = c(
    "The power of weights in a persistence silhouette function."
  ),
  # TropicalCoordinates
  num_bars = c(
    "Number of bars (persistent pairs) over which to maximize...."
  ),
  # TemplateFunction
  num_bins = c(
    "The number of bins along each axis in the discretization grid."
  ),
  tent_size = c(
    "The length of the increment used to discretize tent template functions."
  ),
  tent_shift = c(
    "The vertical shift applied to the discretization grid."
  ),
  # PersistenceBlock
  block_size = c(
    "The scaling factor of the squares used to obtain persistence blocks.",
    "The side length of the square centered at a feature \\eqn{{(b,p)}}",
    "is obtained by multiplying \\eqn{{2p}} by this factor."
  )
)

# CHOICE: assign param default values (omit to inherit from original args)
# NOTE: If default should be learned (e.g. `mtry`) then set it equal to `NA`.
list(
  # consistent behavior across all steps
  hom_degree = 0L,
  max_hom_degree = Inf,
  # missing or disfavored original defaults
  img_sigma = 1,
  num_levels = 6L, weight_func_pl = "triangle", bandwidth = NULL,
  num_bins = 10L, tent_size = NULL, tent_shift = NULL
) |> 
  sapply(deparse) |> 
  enframe(name = "param", value = "default") |> 
  print() -> param_new_defaults
# use to impute missing or overwrite original defaults
tdavec_defaults |> 
  left_join(enframe(arg_params, name = "arg", value = "param"), by = "arg") |> 
  left_join(param_new_defaults, by = "param", suffix = c("_arg", "_param")) |> 
  mutate(default = ifelse(is.na(default_param), default_arg, default_param)) |> 
  select(-contains("default_")) |> 
  print(n = Inf) -> param_defaults

# CHOICE: express preparations of parameters
# FIXME: Ensure that proper parameters are tuned,
# e.g. only `num_levels` when `generalized = FALSE`.
tdavec_preps <- list(
  # generalized persistence landscapes; level error
  PersistenceLandscape = expression({
    # `generalized` should be determined from the presence of `bandwidth`
    if (is.null(x$bandwidth)) {
      if (! isFALSE(x$generalized))
        warning("`bandwidth` is `NULL` so `generalized` is set to `FALSE`.")
      x$generalized = FALSE
    } else {
      if (! isTRUE(x$generalized))
        warning("`bandwidth` is provided so `generalized` is set to `TRUE`.")
      x$generalized = TRUE
    }
    
    if (is.na(.TDAvec_version) || .TDAvec_version == "0.1.4") {
      # restrict number of levels to the minimum number of bars
      x_pairs_min <- vapply(
        training[, col_names, drop = FALSE],
        function(l) {
          val <- vapply(l, pairs_min, 0., hom_degree = x$hom_degree)
          min(val[is.finite(val)])
        },
        0.
      )
      if (x$num_levels > x_pairs_min) {
        warning(
          "`num_levels = ", x$num_levels,
          "` is less than minimum diagram size ",
          "so will be reset to ", x_pairs_min
        )
        x$num_levels <- x_pairs_min
      }
    }
  }),
  # tent function radius and shift
  TemplateFunction = expression({
    # `num_bins` is required; `tent_*` params may be derived therefrom
    if (is.null(x$tent_shift) | is.null(x$tent_size)) {
      x_pers_ranges <- sapply(
        training[, col_names, drop = FALSE],
        function(l) {
          val <- 
            sapply(l, pers_range, hom_degree = x$hom_degree, simplify = TRUE)
          range(val[is.finite(val)])
        },
        simplify = TRUE
      )
    }
    if (is.null(x$tent_size)) {
      x_birth_ranges <- sapply(
        training[, col_names, drop = FALSE],
        function(l) {
          val <- 
            sapply(l, birth_range, hom_degree = x$hom_degree, simplify = TRUE)
          range(val[is.finite(val)])
        },
        simplify = TRUE
      )
    }
    if (is.null(x$tent_shift)) x$tent_shift <- x_pers_ranges[1L, ] / 2
    if (is.null(x$tent_size)) x$tent_size <-
        pmax(x_birth_ranges[2L, ], x_pers_ranges[2L, ] - x$tent_shift) /
        x$num_bins
  })
)

# data set of dials (tunable parameters)
# NOTE: Currently assumes that each dial is a param.
c(
  hom_degree = "hom_degree",
  max_hom_degree = "max_hom_degree",
  # ComplexPolynomial
  num_coef = "num_coef",
  poly_type = "poly_type",
  # PersistenceImage
  img_sigma = "img_sigma",
  # PersistenceLandscape (`generalized = FALSE`)
  num_levels = "num_levels",
  # PersistenceLandscape (`generalized = TRUE`)
  # TODO: Replace with `dials::weight_func()` iff choices agree.
  weight_func_pl = "weight_func_pl",
  bandwidth = "bandwidth",
  # PersistenceSilhouette
  weight_power = "weight_power",
  # TropicalCoordinates
  num_bars = "num_bars",
  # TemplateFunction
  # TODO: Replace with `dials::num_breaks()`.
  num_bins = "num_bins",
  # TODO: Replace with `dials::kernel_offset()`?
  tent_shift = "tent_shift",
  # TODO: Rename `kernel_radius()`?
  tent_size = "tent_size",
  # PersistenceBlock
  block_size = "block_size"
) |> 
  # enframe(name = NULL, value = "param") |> 
  # mutate(dial = param) |> 
  print() -> param_dials
# dials for which to generate source code;
# mostly those that are specific to a single vectorization method
param_dials |> 
  (\(s) s[! s %in% c("generalized")])() |> 
  print() -> param_tuners
no_autotuners <- c("hom_degree", "max_hom_degree", "tent_size", "block_size")
param_tuners |> 
  (\(s) s[! s %in% no_autotuners])() |> 
  print() -> param_autotuners

# title (tunable) parameters
dial_titles <- c(
  hom_degree = "Homological degree",
  max_hom_degree = "Highest homological degree",
  # xseq = "Discretization intervals",
  # yseq = "2D discretization intervals",
  evaluate = "Evaluation method",
  num_coef = "Number of Polynomial coefficients",
  poly_type = "Type of polynomial",
  img_sigma = "Convolved Gaussian standard deviation",
  num_levels = "Number of Levels or envelopes",
  # generalized = "Use generalized functions?",
  weight_func_pl = "Kernel distance weight function",
  bandwidth = "Kernel bandwidth",
  weight_power = "Exponent weight",
  num_bars = "Number of Bars (persistence pairs)",
  num_bins = "Discretization grid bins",
  tent_size = "Discretization grid increment",
  tent_shift = "Discretization grid shift",
  block_size = "Square side length scaling factor"
)
# describe tunable parameters
dial_descriptions <- c(
  hom_degree = "The homological degree of persistent features.",
  max_hom_degree = "The highest homological degree of persistent features.",
  # xseq = "Discretization intervals along the abscissa (birth).",
  # yseq = "Discretization intervals along the ordinate (death or persistence).",
  evaluate = "How to evaluate functions to obtain vectorizations.",
  num_coef = "The number of complex polynomial coefficients.",
  poly_type = "The type of complex polynomial.",
  img_sigma = "The standard deviation of the Gaussian convolved with points.",
  num_levels = "The number of levels or envelopes.",
  # generalized = "Whether to use alternative kernels and bandwidths.",
  weight_func_pl = "The distance weight function of a generalized kernel.",
  bandwidth = "The bandwidth of a generalized kernel.",
  weight_power = "The exponent of the weights.",
  num_bars = "The number of persistence pairs used in computations.",
  num_bins = "The number of bins into which to partition grid axes.",
  tent_size = "The increment size for tent template functions.",
  tent_shift = "The positive shift for tent template functions.",
  block_size = "The side length scaling parameter for persistence blocks."
)

# categorize new dials by input type & as quantitative or qualitative
type_class <- c(
  integer = "quant",
  double = "quant",
  logical = "qual",
  character = "qual"
)
dial_types <- c(
  hom_degree     = "integer",
  max_hom_degree = "integer",
  # xseq = "",
  # yseq = "",
  evaluate       = "character",
  num_coef       = "integer",
  poly_type      = "character",
  img_sigma      = "double",
  num_levels     = "integer",
  # generalized    = "logical",
  weight_func_pl = "character",
  bandwidth      = "double",
  weight_power   = "double",
  num_bars       = "integer",
  num_bins       = "integer",
  tent_size     = "double",
  tent_shift    = "double",
  block_size     = "double"
)
# CHOICE: assign defaults to new dials & o/w note how to finalize from data
# NOTE: Finalizers are written in `R/vpd-finalizers.R`.
dial_ranges_values <- list(
  # finalize to highest degree
  hom_degree = c(0L, NA_integer_),
  # finalize to highest degree
  max_hom_degree = c(0L, NA_integer_),
  evaluate = c("intervals", "points"),
  # finalize to number of features
  num_coef = c(1L, NA_integer_),
  poly_type = c("R", "S", "T"),
  # finalize to maximum persistence (fractions)
  img_sigma = c(NA_real_, NA_real_),
  # finalize to number of features
  num_levels = c(1L, NA_integer_),
  # generalized = c(FALSE, TRUE),
  weight_func_pl = c("triangle", "epanechnikov", "tricubic"),
  # finalize to maximum persistence (fractions)
  bandwidth = c(NA_real_, NA_real_),
  weight_power = c(1, 2),
  # finalize to number of features
  num_bars = c(1L, NA_integer_),
  num_bins = c(2L, 20L),
  # fix to maximum ( birth | persistence + tent_shift ) / num_bins
  tent_size = c(NA_real_, NA_real_),
  # finalize to minimum persistence (fractions)
  tent_shift = c(NA_real_, NA_real_),
  block_size = c(0, 1)
)
# dial range endpoints
dial_inclusive <- list(
  hom_degree = c(TRUE, TRUE),
  max_hom_degree = c(TRUE, TRUE),
  num_coef = c(TRUE, TRUE),
  img_sigma = c(TRUE, TRUE),
  num_levels = c(TRUE, TRUE),
  bandwidth = c(TRUE, TRUE),
  weight_power = c(TRUE, TRUE),
  num_bars = c(TRUE, TRUE),
  num_bins = c(TRUE, TRUE),
  tent_size = c(TRUE, TRUE),
  tent_shift = c(TRUE, TRUE),
  block_size = c(TRUE, TRUE)
)
# dial transformations
dial_transforms <- list(
  img_sigma = expr(transform_log10()),
  bandwidth = expr(transform_log10()),
  # TODO: Revisit this choice. Compare to publication. Harmonize with endpoints.
  tent_size = expr(transform_log10()),
  tent_shift = expr(transform_log10()),
  block_size = expr(transform_log10())
)

# CHOICE: finalizer for each dial
dial_finalizers <- list(
  hom_degree = "get_hom_range",
  max_hom_degree = "get_hom_range",
  num_coef = "get_pairs_max",
  img_sigma = "get_pers_max_frac",
  num_levels = "get_pairs_max",
  bandwidth = "get_pers_max_frac",
  num_bars = "get_pairs_max",
  tent_shift = "get_pers_min_mult"
)

# CHOICE: example ranges of dials (tailor to 'zzz-ex-vpd-param.R')
dial_range_value_examples <- list(
  hom_degree = c(0L, 2L),
  max_hom_degree = c(1L, 2L),
  num_coef = c(1L, 3L),
  poly_type = c("R", "S"),
  img_sigma = c(100, 400),
  # finalize to number of features
  num_levels = c(1L, 6L),
  weight_func_pl = c("triangle", "tricubic"),
  bandwidth = c(500, 1500),
  weight_power = c(1, 3),
  num_bars = c(1L, 3L),
  num_bins = c(5L, 10L),
  # fix to maximum ( birth | persistence + tent_shift ) / num_bins
  tent_size = c(1000, 1300),
  # finalize to minimum persistence (fractions)
  tent_shift = c(100, 200),
  block_size = c(0, .5)
)

#' HELPERS

param_args <- names(arg_params)
names(param_args) <- arg_params

dial_params <- names(param_dials)
names(dial_params) <- param_dials

#' Format text.

# abbr_vec <- function(name) tolower(gsub("^compute", "", name))
# get snakecase name of vectorization method
vec_sname <- function(name) {
  name <- tdavec_renames$rename[tdavec_renames$name == name]
  name <- snakecase::to_snake_case(name)
  name
}
# vec_sname("computePersistenceLandscape")

# capitalize proper names
capitalize_proper_names <- function(full_name) {
  for (s in proper_names) {
    full_name <- gsub(tolower(s), s, full_name)
  }
  full_name
}
# capitalize_proper_names("euler characteristic curve")

# wrap external objects in hyperlink syntax
# [ggplot2::draw_key_point()]
link_obj <- function(name) {
  env <- pryr::where(name)
  pkg <- gsub("^package\\:", "", attr(env, "name"))
  # search <- paste0("package:", pkg)
  # stopifnot(name %in% as.character(lsf.str(search)))
  res <- paste0(
    "[",
    if (pkg == "tdarec") "" else paste0(pkg, "::"),
    name,
    if (class(get(name)) == "function") "()" else "",
    "]"
  )
  res
}
# load_all()
# link_obj("step_pd_point_cloud")
# link_obj("computePersistenceLandscape")
# # FIXME: This should link to `dials::check_param`, but it is not exported.
# link_obj("check_param")

# surround lines of documentation with "#' " and "\n"
doc_wrap <- function(s) paste0("#' ", s, "\n")
