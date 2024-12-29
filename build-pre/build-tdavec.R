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
  filter(grepl("^compute[A-Z]+$", name)) |> 
  mutate(fun = map(name, \(s) getFromNamespace(s, ns = "TDAvec"))) |> 
  mutate(args = map(fun, formals)) |> 
  print() -> tdavec_functions

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
  # pull(doc) |> first() -> x
  # mutate(title = map_chr(doc, char_by_tag, tag = "\\title")) |> 
  mutate(title = map_chr(
    doc,
    \(x) unlist(x[vapply(x, function(e) attr(e, "Rd_tag"), "") == "\\title"])
  )) |> 
  mutate(full_name = gsub("A Vector Summary of the ", "", title)) |> 
  mutate(full_name = gsub(" Function", "", full_name)) |> 
  # TODO: Do this later, after the retrieval section.
  mutate(step_title = map_chr(
    full_name,
    \(s) paste(s, "Vectorization of Persistent Homology", sep = " ")
  )) |> 
  print() -> tdavec_content

# words to keep capitalized
proper_names <- c("Betti", "Euler")

#' ADAPTATION

#' Rename parameters.

# all parameters used by {TDAvec} vectorization functions
tdavec_functions |> 
  transmute(name, arg = map(args, names)) |> 
  unnest(arg) |> 
  nest(funs = c(name)) |> 
  filter(arg != "D") |> 
  arrange(desc(map_int(funs, nrow))) |> 
  print() -> tdavec_args
# which vectorizations use which parameters
tdavec_args |> 
  mutate(funs = map(funs, deframe)) |> 
  mutate(funs = map(funs, gsub, pattern = "compute", replacement = "")) |> 
  mutate(funs = map_chr(funs, paste, collapse = ", ")) |> 
  print()

# CHOICE: assign parameter names (existing or new) for recipe steps
arg_params <- c(
  homDim = "hom_degree",
  maxhomDim = "max_hom_degree",
  # TODO: Revisit this choice closer to release.
  scaleSeq = "xseq",
  xSeq = "xseq",
  ySeq = "yseq",
  k = "num_levels",
  # TODO: Check that the silhouette function uses this as a distance power.
  p = "weight_power",
  sigma = "std_dev",
  tau = "block_size"
)

# document parameters
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
  num_levels = c(
    "The number of levels of a persistence landscape to vectorize.",
    "If `num_levels` is greater than the length of a landscape,",
    "then additional levels of zeros will be included."
  ),
  weight_power = c(
    "The power of weights in a persistence silhouette function."
  ),
  std_dev = c(
    "The standard deviation of the gaussian distribution",
    "convolved with persistence diagrams to obtain persistence images."
  ),
  block_size = c(
    "The scaling factor of the squares used to obtain persistence blocks.",
    "The side length of the square centered at a feature \\eqn{{(b,p)}}",
    "is obtained by multiplying \\eqn{{2p}} by this factor."
  )
)

# CHOICE: assign default values to parameters
param_defaults <- c(
  hom_degree = "0L",
  max_hom_degree = "Inf",
  num_levels = "6L",
  weight_power = "1",
  std_dev = "1",
  block_size = "1"
)

# CHOICE: assign dials to parameters
param_dials <- c(
  hom_degree = "hom_degree",
  max_hom_degree = "hom_degree",
  # TODO: Revisit this name.
  num_levels = "num_levels",
  weight_power = "weight_power",
  std_dev = "std_dev",
  block_size = "block_size"
)

# list (tunable) parameters
param_bullets <- c(
  hom_degree = "Homological degree",
  max_hom_degree = "Highest homological degree",
  xseq = "Discretization intervals",
  yseq = "2D discretization intervals",
  num_levels = "# Levels or envelopes",
  weight_power = "Exponent weight",
  std_dev = "Convolved Gaussian standard deviation",
  block_size = "Square side length scaling factor"
)

# CHOICE: assign defaults to new dials (sub `NA` for `unknown()`)
dial_ranges <- list(
  hom_degree = c("0L", "unknown()"),
  num_levels = c("1L", "unknown()"),
  weight_power = c("1", "2"),
  # TODO: Learn range from diagram radius to some orders of magnitude smaller.
  std_dev = c("unknown()", "unknown()"),
  block_size = c("unknown()", "unknown()")
)
dial_transforms <- list(
  hom_degree = NULL,
  num_levels = NULL,
  weight_power = NULL,
  std_dev = expr(transform_log10()),
  block_size = expr(transform_log10())
)
dial_types <- c(
  hom_degree = "integer",
  num_levels = "integer",
  weight_power = "double",
  std_dev = "double",
  block_size = "double"
)
dial_inclusive <- list(
  hom_degree = c(TRUE, TRUE),
  num_levels = c(TRUE, TRUE),
  weight_power = c(TRUE, TRUE),
  std_dev = c(TRUE, TRUE),
  block_size = c(TRUE, TRUE)
)



#' HELPERS

#' Format text.

# get lowercase abbreviation of vectorization method
abbr_vec <- function(name) tolower(gsub("^compute", "", name))
# abbr_vec("computePL")

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
# link_obj("step_phom_point_cloud")
# link_obj("computePL")
# # FIXME: This should link to `dials::check_param`.
# link_obj("check_param")

# surround lines of documentation with "#' " and "\n"
doc_wrap <- function(s) paste0("#' ", s, "\n")
