#---------------------------------------------------------------------------------
# Setup & Directory Configuration
#---------------------------------------------------------------------------------

find_project_root <- function() {
  curr <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (curr != dirname(curr)) {
    if (file.exists(file.path(curr, "README.md")) || 
        file.exists(file.path(curr, ".git")) || 
        dir.exists(file.path(curr, "scripts"))) {
      return(curr)
    }
    curr <- dirname(curr)
  }
  return(normalizePath(".", winslash = "/"))
}

root <- find_project_root()

# Main directories list (all ending with / for paste0 compatibility)
d <- list(
  root     = paste0(root, "/"),
  fantasy  = paste0(root, "/"),
  data     = file.path(root, "data", ""),
  scripts  = file.path(root, "scripts", ""),
  sims     = file.path(root, "sims", ""),
  analysis = file.path(root, "analysis", ""),
  shiny    = file.path(root, "shiny", ""),
  tex      = file.path(root, "TeX", ""),
  figures  = file.path(root, "figures", "")
)

# Backward-compatible global aliases for legacy scripts
ddata    <- d$data
dsource  <- d$data
dfantasy <- root
dsims    <- d$sims
dtex     <- d$tex

# Options
options(
  stringsAsFactors = FALSE,
  digits = 4
)

# Packages
pkgs <- c("readr", "reshape2", "rvest", "foreign", "dplyr", "magrittr", "stringr", "ggplot2", "xtable")
missing_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Note: The following packages are required for full pipeline runs: ", paste(missing_pkgs, collapse = ", "))
}
for (p in pkgs) {
  if (requireNamespace(p, quietly = TRUE)) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}

# Functions
functions_path <- file.path(d$scripts, "fantasy_functions.R")
if (file.exists(functions_path)) {
  source(functions_path)
}
