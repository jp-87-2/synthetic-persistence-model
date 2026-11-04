# Writes src/data/computed.json: every number the calculator and cohort inspector use,
# calculated from data/synthetic_enrollment.csv with replication/R/segment_model.R.
#
#   Rscript replication/R/export_site_data.R           write the file
#   Rscript replication/R/export_site_data.R --check   fail if the file differs from a fresh calculation
#
# Choices (stated, not hidden):
#   population  undergraduates, all entry types, per school
#   as of       2025SP, the last term with an entering class in the CSV
#   entering    mean of the last 3 entering classes of the same season (admissions-target placeholder)
#   rates       notebook defaults: simple mean of cohort ratios, COVID terms excluded, no cap
#   charge      mean (ntr + institutional_grant) per entering student-term; comp_fee 0 because the
#               CSV's ntr already includes the fee (generator: tuition - grant + fee noise)

source("replication/R/segment_model.R")
CSV <- "data/synthetic_enrollment.csv"
OUT <- "src/data/computed.json"
AS_OF <- "2025SP"

d <- read.csv(CSV, stringsAsFactors = FALSE)
DD <- term_to_int(AS_OF)
zero_na <- function(x) ifelse(is.na(x), 0, x)

school_block <- function(sc) {
  tpy <- if (sc == "MCM") 3L else 2L
  max_d <- if (tpy == 3L) 12L else 8L
  r <- d[d$school == sc & d$student_program == "Undergraduate", ]
  m <- cohort_matrix(r, tpy, max_d)
  no_covid <- segment_rates(m, tpy, DD)
  with_covid <- segment_rates(m, tpy, DD, exclude_covid = FALSE)
  curve <- persistence_curve(no_covid, tpy, "2025FA")$persistence

  er <- r[r$term_code == r$cohort_term & grepl("FA$", r$cohort_term), ]
  charge <- mean(er$ntr + er$institutional_grant)

  # cohort inspector: observed cohorts 2021FA..2025SP, projected 2025FA, 2026SP, 2026FA
  obs <- as.integer(colnames(m)); obs <- obs[obs >= term_to_int("2021FA") & obs <= DD]
  cohorts <- lapply(obs, function(c0) {
    o <- list()
    for (k in 0:max_d) {
      cal <- cal_term(c0, k, tpy)
      if (cal > DD) break
      o[[as.character(k)]] <- list(calendarTerm = int_to_term(cal), heads = m[k + 1L, as.character(c0)])
    }
    list(cohortTerm = int_to_term(c0), entrants = m[1, as.character(c0)],
         intakeType = c(SU = "summer", FA = "fall", SP = "spring")[[season(c0)]], observed = o)
  })
  future <- c("2025FA", "2026SP", "2026FA")
  ent <- default_entering(m, DD, future)
  for (t in future)
    cohorts[[length(cohorts) + 1L]] <- list(cohortTerm = t, entrants = unname(ent[t]),
      intakeType = if (grepl("FA$", t)) "fall" else "spring", observed = setNames(list(), character()))

  list(
    entrant = list(
      terms_per_year = tpy, include_summer = tpy == 3L, spring_to_fall_offset = if (tpy == 3L) 2L else 1L,
      rates = round(zero_na(no_covid[-1, "FA"]), 6), curve = round(curve, 6),
      entering = unname(ent["2025FA"]), charge_per_term = round(charge),
      comp_fee_per_term = 0, aid_rate = round(mean(er$institutional_grant) / charge, 6)),
    matrix = list(
      rates = list(noCovid = round(zero_na(no_covid[-1, "FA"]), 6), withCovid = round(zero_na(with_covid[-1, "FA"]), 6)),
      spRates = list(noCovid = round(zero_na(no_covid[-1, "SP"]), 6), withCovid = round(zero_na(with_covid[-1, "SP"]), 6)),
      summer_uptake = if (tpy == 3L) round(no_covid[3, "FA"], 4) else NA,
      cohorts = cohorts))
}

blocks <- lapply(c(MCM = "MCM", MCC = "MCC"), school_block)
out <- list(
  source = list(csv = CSV, md5 = unname(tools::md5sum(CSV)), records = nrow(d),
                students = length(unique(d$student_id)), as_of = AS_OF,
                population = "undergraduates, all entry types",
                engine = "replication/R/segment_model.R"),
  entrant = list(as_of = AS_OF, years = 4L, schools = lapply(blocks, `[[`, "entrant")),
  cohort_matrix = lapply(blocks, `[[`, "matrix"))
txt <- jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE, digits = NA)

if ("--check" %in% commandArgs(TRUE)) {
  same <- file.exists(OUT) && identical(paste(readLines(OUT), collapse = "\n"), as.character(txt))
  if (!same) { message("computed.json does NOT match a fresh calculation from ", CSV); quit(status = 1) }
  message("computed.json matches a fresh calculation from ", CSV)
} else {
  writeLines(txt, OUT); message("wrote ", OUT)
}
