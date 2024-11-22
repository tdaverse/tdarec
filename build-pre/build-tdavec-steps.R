#' This script assembles source files containing objects, methods, and
#' {roxygen2} documentation for {recipe} steps to perform vectorizations of
#' persistent homology provided by {TDAvec}.
#'
#' To generate or update these files, execute the following:
#' source(here::here("build-pre/build-tdavec-steps.R"))
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

#' Set parameters.

# standard prefix for built files
build_prefix <- "zzz-"

#' Remove previous builds.
Sys.sleep(.5)

# remove previously built step files
built_files <- 
  list.files(here::here("R"), paste0(build_prefix, "\\-step\\-[a-z]+\\.R"))
file.remove(here::here("R", built_files))

# # re-document package without built files and load remaining objects
# document()
# load_all()

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

#' CHOICE: 
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
  p = "dist_power",
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
    "The side length of the square centered at a feature \\eqn{(b,p)}",
    "is obtained by multiplying \\eqn{2p} by this factor."
  )
)

# CHOICE: assign default values to parameters
param_defaults <- c(
  hom_degree = "0L",
  max_hom_degree = "Inf",
  num_levels = "6L",
  dist_power = "1",
  std_dev = "1",
  block_size = "1"
)

# CHOICE: assign dials to parameters
param_dials <- c(
  hom_degree = "hom_degree",
  max_hom_degree = "hom_degree",
  # TODO: Revisit this name.
  num_levels = "num_levels",
  dist_power = "dist_power",
  std_dev = "std_dev",
  block_size = "block_size"
)

# CHOICE: assign defaults to new dials (sub `NA` for `unknown()`)
dial_ranges <- list(
  hom_degree = c(0L, NA),
  # TODO: Learn range from diagram radius to some orders of magnitude smaller.
  std_dev = c(NA, NA),
  block_size = c(NA, NA)
)
dial_transforms <- list(
  hom_degree = NULL,
  std_dev = expr(transform_log10()),
  block_size = expr(transform_log10())
)
dial_types <- c(
  hom_degree = "integer",
  std_dev = "double",
  block_size = "double"
)
dial_inclusive <- list(
  hom_degree = c(TRUE, TRUE),
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



#' GENERATORS

# Follow these resources:
# https://blog.r-hub.io/2020/02/10/code-generation/
# https://www.tidymodels.org/learn/develop/recipes/

#' Generate step documentation.

# format custom parameters for inclusion into step arguments and augment scale
# sequence parameters with scale sequence-generating parameters on one line
custom_params_leave <- function(args) {
  params <- unname(arg_params[args])
  lines <- params
  lines <- gsub(
    "([xy]{1})seq",
    paste0(
      vapply(c("seq", "min", "max", "len", "by"), \(s) paste0("\\1", s), ""),
      collapse = ", "
    ),
    lines
  )
  lines
}
custom_params_set <- function(args) {
  params <- unname(arg_params[args])
  lines <- paste0(params, " = ", param_defaults[params])
  # if no default provided, use `NULL`
  lines <- gsub(" NA$", " NULL", lines)
  lines <- gsub(
    "([xy]{1})seq = NULL",
    paste0(
      c("\\1seq", "\\1min", "\\1max", "\\1len", "\\1by"),
      " = NULL", collapse = ", "
    ),
    lines
  )
  lines
}
custom_params_pass <- function(args) {
  params <- unname(arg_params[args])
  lines <- paste0(params, " = ", params)
  lines <- gsub(
    "([xy]{1})seq = \\1seq",
    paste0(
      vapply(
        c("seq", "min", "max", "len", "by"),
        \(s) paste0("\\1", s, " = \\1", s),
        ""
      ),
      collapse = ", "
    ),
    lines
  )
  lines
}
# custom_params_leave(c("homDim", "scaleSeq", "tau"))
# custom_params_set(c("homDim", "scaleSeq", "tau"))
# custom_params_pass(c("homDim", "scaleSeq", "tau"))

# generate title and description
build_title_descr <- function(fn) {
  fn_abbr <- abbr_vec(fn)
  fn_title <- tdavec_content |> 
    filter(name == fn) |> 
    pull(step_title) |> first()
  fn_title_doc <- paste0("@title ", fn_title)
  fn_descr <- c(
    paste0("@description The function `step_vpd_", fn_abbr, "()` creates"),
    "  a _specification_ of a recipe step that will convert",
    "  a list-column of 3-column matrices of persistence data",
    "  to a list-column of 1-row matrices of vectorizations."
  )
  
  c(fn_title_doc, "", fn_descr, "") |> 
    doc_wrap() |> 
    as.list() |> 
    do.call(what = glue::glue)
}
# build_title_descr("computePL")

# generate parameter documentation
build_param_docs <- function(fn) {
  fn_abbr <- abbr_vec(fn)
  fn_args <- tdavec_functions |> 
    filter(name == fn) |> 
    pull(args) |> first() |> names() |> setdiff("D")
  fn_params <- arg_params[fn_args] |> 
    gsub(pattern = "([xy]{1})seq", replacement = "\\1seq|\\1other") |> 
    strsplit("\\|") |> unlist() |> unname()
  fn_param_docs <- mapply(
    \(p, d) c(paste0("@param ", p, "\n", d[1L]), d[-1L]),
    p = gsub("([xy]{1})other", "\\1min,\\1max,\\1len,\\1by", fn_params),
    d = param_docs[fn_params]
  ) |> 
    unname() |> unlist() |> 
    strsplit("\n *") |> unlist() |> 
    gsub(pattern = "^([^@]+)", replacement = "  \\1") |> 
    doc_wrap() |> as.list()
  
  do.call(what = glue::glue, args = fn_param_docs)
}
# build_param_docs("computePL")

# generate details
build_details <- function(fn) {
  fn_abbr <- abbr_vec(fn)
  fn_name <- tdavec_content |> 
    filter(name == fn) |> 
    pull(full_name) |> first() |> 
    tolower() |> capitalize_proper_names()
  fn_line <- c(
    paste0("The ", fn_name, " vectorization deploys"),
    paste0(link_obj(fn), "."),
    "See there for definitions and references."
  )
  fn_knit <- c(
    "```{{r, echo=FALSE, results=\"asis\"}}",
    paste0("step <- \"step_vpd_", fn_abbr, "\""),
    "result <- knitr::knit_child(\"man/rmd/tunable-args.Rmd\")",
    "cat(result)",
    "```"
  )
  
  c(
    "@template step-vpd-details",
    "",
    fn_line,
    "",
    fn_knit,
    ""
  ) |> 
    doc_wrap() |> 
    as.list() |> 
    do.call(what = glue::glue)
}
# build_details("computePI")

# generate importation and inheritance instructions
build_import_inherit <- function() {
  glue::glue(
    doc_wrap("@import recipes"),
    doc_wrap("@inheritParams recipes::step_pca"),
    doc_wrap("@inherit recipes::step_pca return")
  )
}
# build_import_inherit()

# generate link to example file
build_ex <- function(fn) {
  fn_abbr <- abbr_vec(fn)
  fn_line <- glue::glue("@example inst/examples/ex-step-vpd-{fn_abbr}.R")
  
  glue::glue(doc_wrap(fn_line))
}
# build_ex("computePS")

#' Generate step functions.

# generate `step_*()`
build_step <- function(fn) {
  fn_abbr <- abbr_vec(fn)
  fn_args <- tdavec_functions |> 
    filter(name == fn) |> 
    pull(args) |> first() |> names() |> setdiff("D")
  fn_set <- paste0(
    "    ", custom_params_set(fn_args), ",\n",
    collapse = ""
  )
  fn_pass <- paste0(
    "      ", custom_params_pass(fn_args), ",\n",
    collapse = ""
  )
  
  # TODO: Can't this be done using `paste0()`?
  glue::glue(
    doc_wrap("@export"),
    "step_vpd_{fn_abbr} <- function(\n",
    "    recipe,\n",
    "    ...,\n",
    # standard inputs
    "    role = \"predictor\",\n",
    "    trained = FALSE,\n",
    # custom parameters
    "{fn_set}",
    # standard parameters
    "    columns = NULL,\n",
    "    keep_original_cols = TRUE,\n",
    "    skip = FALSE,\n",
    "    id = rand_id(\"vpd_{fn_abbr}\"),\n",
    ") {{\n",
    # ensure that required packages are installed
    "  recipes_pkg_check(required_pkgs.step_vpd_{fn_abbr}())\n",
    "  \n",
    # output the step
    "  add_step(\n",
    "    recipe,\n",
    "    step_vpd_{fn_abbr}_new(\n",
    "      terms = rlang::enquos(...),\n",
    "      trained = trained,\n",
    "      role = role,\n",
    "{fn_pass}",
    "      columns = columns,\n",
    "      keep_original_cols = keep_original_cols,\n",
    "      skip = skip,\n",
    "      id = id\n",
    "    )\n",
    "  )\n",
    "}}\n"
  )
}
# build_step("computeVPB")



#' Generate parameter documentation.



#' Generate parameter functions.



#' WRITING

#' Write source code.



#' Write examples.

# write example files from template (overwrites existing files)
for (fn in tdavec_functions$name) {
  fn_abbr <- abbr_vec(fn)
  
  readLines("man/ex/ex-step-vpd-template.R") |> 
    gsub(pattern = "step_vpd_", replacement = paste0("step_vpd_", fn_abbr)) |> 
    writeLines(glue::glue("inst/examples/ex-step-vpd-{fn_abbr}.R"))
}
