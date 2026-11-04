# Segment-level cohort survival model (demo)
# Rebuilt in base R from batched_cohort_projections_prep.ipynb (cell 14).

COVID_TERMS <- c(20202L, 20203L, 20211L)  # 2020FA, 2021SP, 2021SU

# --- Term arithmetic ---------------------------------------------------------
# SIS coding: YYYYSU = YYYY1, YYYYFA = YYYY2, YYYYSP = (YYYY-1)3.
# Integers sort chronologically: 2020SP (20193) < 2020SU (20201) < 2020FA (20202).
term_to_int <- function(term) {
  y <- as.integer(substr(term, 1, 4)); s <- substr(term, 5, 6)
  ifelse(s == "SU", y * 10L + 1L, ifelse(s == "FA", y * 10L + 2L, (y - 1L) * 10L + 3L))
}
int_to_term <- function(x) {
  y <- x %/% 10L; s <- x %% 10L
  ifelse(s == 1L, paste0(y, "SU"), ifelse(s == 2L, paste0(y, "FA"), paste0(y + 1L, "SP")))
}
# Position on the school calendar: 3 terms/year (with summer) or 2 (fall/spring).
term_pos <- function(x, tpy) {
  ay <- x %/% 10L; s <- x %% 10L
  if (tpy == 3L) ay * 3L + (s - 1L) else ay * 2L + (s - 2L)
}
pos_to_int <- function(p, tpy) {
  if (tpy == 3L) (p %/% 3L) * 10L + (p %% 3L) + 1L else (p %/% 2L) * 10L + (p %% 2L) + 2L
}
cal_term <- function(cohort, d, tpy) pos_to_int(term_pos(cohort, tpy) + d, tpy)

# Fall is measured from the prior spring (skipping optional summer),
# except a summer starter's first fall, which is measured from that summer.
ref_offset <- function(cohort, d, tpy) {
  fall <- cal_term(cohort, d, tpy) %% 10L == 2L
  if (tpy == 3L && fall && !(cohort %% 10L == 1L && d == 1L)) 2L else 1L
}
season <- function(x) c("SU", "FA", "SP")[x %% 10L]

# --- 1. Cohort matrix: students enrolled, by terms since entry (rows) x cohort (cols)
cohort_matrix <- function(records, tpy, max_d) {
  r <- records
  r$term_int <- term_to_int(r$term_code)
  r$cohort_int <- term_to_int(r$cohort_term)
  if (tpy == 2L) r <- r[r$term_int %% 10L != 1L, ]
  r$d <- term_pos(r$term_int, tpy) - term_pos(r$cohort_int, tpy)
  r <- r[r$d >= 0 & r$d <= max_d & !duplicated(r[c("student_id", "term_int")]), ]
  tab <- table(factor(r$d, 0:max_d), r$cohort_int)
  matrix(as.numeric(tab), nrow(tab), dimnames = dimnames(tab))
}

# --- 2. Continuation rates, pooled across cohorts (size-weighted), by entry season
# Default matches the 2025 notebook: simple mean of per-cohort ratios, a cohort
# counted only where both cells are non-zero, no cap. weighted / cap are opt-in.
segment_rates <- function(m, tpy, data_date, exclude_covid = TRUE, years = NULL,
                          weighted = FALSE, cap = FALSE) {
  cohorts <- as.integer(colnames(m)); D <- nrow(m) - 1L
  ss <- unique(season(cohorts))
  out <- matrix(NA_real_, D + 1L, length(ss), dimnames = list(0:D, ss))
  out[1, ] <- 1
  for (s in ss) for (d in seq_len(D)) {
    num <- c(); den <- c()
    for (j in which(season(cohorts) == s)) {
      cal <- cal_term(cohorts[j], d, tpy)
      if (cal > data_date) next                                   # not yet observed
      if (exclude_covid && cal %in% COVID_TERMS) next
      if (!is.null(years) && cal <= data_date - years * 10L) next # lookback window
      a <- m[d + 1L, j]; b <- m[d + 1L - ref_offset(cohorts[j], d, tpy), j]
      if (a > 0 && b > 0) { num <- c(num, a); den <- c(den, b) }
    }
    if (length(den)) {
      r <- if (weighted) sum(num) / sum(den) else mean(num / den)
      out[d + 1L, s] <- if (cap) min(1, r) else r
    }
  }
  out
}

# --- 3. Projection: fill unobserved cells with (reference cell x average rate)
project_segment <- function(m, rates, tpy, data_date, entering = NULL) {
  m <- m[, as.integer(colnames(m)) <= data_date, drop = FALSE]  # only cohorts already entered
  cohorts <- as.integer(colnames(m)); D <- nrow(m) - 1L
  for (j in seq_along(cohorts)) for (d in 0:D)
    if (cal_term(cohorts[j], d, tpy) > data_date) m[d + 1L, j] <- NA
  if (length(entering)) {                     # future entering classes (admissions targets)
    new <- matrix(NA_real_, D + 1L, length(entering),
                  dimnames = list(rownames(m), term_to_int(names(entering))))
    new[1, ] <- entering
    m <- cbind(m, new); cohorts <- as.integer(colnames(m))
  }
  observed <- !is.na(m)
  for (j in seq_along(cohorts)) for (d in seq_len(D)) if (is.na(m[d + 1L, j])) {
    r <- rates[d + 1L, season(cohorts[j])]
    m[d + 1L, j] <- m[d + 1L - ref_offset(cohorts[j], d, tpy), j] * ifelse(is.na(r), 0, r)
  }
  list(heads = m, observed = observed, tpy = tpy)
}

# --- 4. Census: sum each calendar-term diagonal
headcount_by_term <- function(p) {
  cohorts <- as.integer(colnames(p$heads)); D <- nrow(p$heads) - 1L
  cal <- outer(0:D, cohorts, function(d, c) cal_term(c, d, p$tpy))
  heads <- tapply(p$heads, cal, sum)
  obs <- tapply(p$observed, cal, all)
  data.frame(term = int_to_term(as.integer(names(heads))),
             heads = round(as.numeric(heads)),
             status = ifelse(obs, "observed", "projected"))
}

# Admissions-target placeholder: mean of the last 3 entering classes of each season.
default_entering <- function(m, data_date, future_terms) {
  cohorts <- as.integer(colnames(m))
  sapply(future_terms, function(t) {
    same <- cohorts[season(cohorts) == season(term_to_int(t)) & cohorts <= data_date]
    round(mean(m[1, as.character(tail(sort(same), 3))]))
  })
}

# --- 5. Normalized views for spreadsheet use ---------------------------------
# Each cohort divided by its entering headcount (row 0 = 1.000); unobserved cells blank.
normalize_cohorts <- function(m, tpy, data_date) {
  cohorts <- as.integer(colnames(m))
  m <- m[, cohorts <= data_date, drop = FALSE]; cohorts <- as.integer(colnames(m))
  n <- sweep(m, 2, m[1, ], "/")
  for (j in seq_along(cohorts)) for (d in 0:(nrow(m) - 1L))
    if (cal_term(cohorts[j], d, tpy) > data_date) n[d + 1L, j] <- NA
  colnames(n) <- int_to_term(cohorts)
  n
}
# Cumulative persistence: share of an entering class still enrolled d terms after entry.
# heads(d) = entering x persistence(d), so a spreadsheet needs only this column per season.
persistence_curve <- function(rates, tpy, entry_term) {
  c0 <- term_to_int(entry_term); D <- nrow(rates) - 1L
  p <- numeric(D + 1L); p[1] <- 1
  for (d in seq_len(D)) {
    r <- rates[d + 1L, season(c0)]
    p[d + 1L] <- p[d + 1L - ref_offset(c0, d, tpy)] * ifelse(is.na(r), 0, r)
  }
  data.frame(d = 0:D, term = int_to_term(cal_term(c0, 0:D, tpy)), persistence = round(p, 4))
}
