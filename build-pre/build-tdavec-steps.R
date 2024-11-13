#' This script assembles source files containing objects, methods, and
#' {roxygen2} documentation for {recipe} steps to perform vectorizations of
#' persistent homology provided by {TDAvec}. To generate or update these files,
#' execute the following: source(here::here("build-pre/build-tdavec-steps.R"))

library(devtools)

#' Set parameters.

# standard prefix for built files
build_prefix <- "zzz-"

#' Remove previous builds.
Sys.sleep(.5)

# remove previous built step files
built_files <- 
  list.files(here::here("R"), paste0(build_prefix, "\\-step\\-[a-z]+\\.R"))
file.remove(here::here("R", built_files))

# re-document package without built files and load remaining objects
document()
load_all()

#' Retrieve functions and their attributes from {TDAvec}.

# retrieve all function objects from {TDAvec}
fun_names <- as.character(lsf.str("package:TDAvec"))
# ensure common naming convention
stopifnot(all(vapply(fun_names, grepl, NA, pattern = "^compute[A-Z]+$")))
# extract step names
names(fun_names) <- 
  tolower(vapply(fun_names, gsub, "", pattern = "^compute", replacement = ""))
# ensure uniqueness of names
stopifnot(all(! duplicated(names(fun_names))))

# retrieve function parameters from {TDAvec}
fun_names |> 
  lapply(utils::getFromNamespace, ns = "TDAvec") |> 
  lapply(formals) ->
  fun_formals

#' Retrieve and adapt documentation from {TDAvec}.

# retrieve function titles from {TDAvec}
# https://stackoverflow.com/a/46712167/4556798
char_by_tag <- function(x, tag) {
  x_tags <- vapply(x, function(e) attr(e, "Rd_tag"), "")
  vapply(x[x_tags == tag], as.character, "")
}
fun_names |> 
  lapply(help, package = "TDAvec") |> 
  lapply(utils:::.getHelpFile) |> 
  # first() -> test
  vapply(char_by_tag, "", tag = "\\title") ->
  fun_titles
# ensure common title convention
stopifnot(all(vapply(
  fun_titles, grepl, NA, pattern = paste0("^A Vector Summary of the ")
)))
# create step titles
step_titles <- paste(
  vapply(
    fun_titles, gsub, "",
    pattern = "A Vector Summary of the ", replacement = ""
  ),
  "Vectorization of Persistent Homology",
  sep = " "
)

#' 

