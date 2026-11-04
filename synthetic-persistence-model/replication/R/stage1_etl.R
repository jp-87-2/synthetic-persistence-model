# Stage 1 (R): locate the repository, SIS term coding, and load the model inputs
# (src/data/siteData.json, data/reference/wiche_hsgrad.csv).

spm_find_root <- function(start = getwd()) {
  dir <- normalizePath(start, mustWork = FALSE)
  repeat {
    if (file.exists(file.path(dir, "src", "data", "siteData.json"))) return(dir)
    parent <- dirname(dir)
    if (parent == dir) stop("Repository root (src/data/siteData.json) not found from ", start)
    dir <- parent
  }
}

# SIS term coding: YYYYSU = YYYY1, YYYYFA = YYYY2, YYYYSP = (YYYY-1)3
spm_term_to_int <- function(term) {
  year <- as.integer(substr(term, 1, 4))
  season <- substr(term, 5, 6)
  ifelse(season == "SU", year * 10L + 1L,
         ifelse(season == "FA", year * 10L + 2L,
                ifelse(season == "SP", (year - 1L) * 10L + 3L, year * 10L)))
}

spm_int_to_term <- function(term_int) {
  year <- term_int %/% 10L
  suffix <- term_int %% 10L
  ifelse(suffix == 1L, paste0(year, "SU"),
         ifelse(suffix == 3L, paste0(year + 1L, "SP"), paste0(year, "FA")))
}

spm_load_site_data <- function(root) {
  jsonlite::fromJSON(file.path(root, "src", "data", "siteData.json"), simplifyVector = FALSE)
}

spm_load_wiche <- function(root) {
  read.csv(file.path(root, "data", "reference", "wiche_hsgrad.csv"),
           comment.char = "#", stringsAsFactors = FALSE)
}
