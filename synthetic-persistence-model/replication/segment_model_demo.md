# Segment-Level Cohort Survival Model: Demo and Gap Review

John Schofield · Student Persistence Model · draft, November 2026

All data in this document is synthetic. The records in `data/synthetic_enrollment.csv` describe two fictional schools, Millbury College of Music (MCM) and Millbury College Conservatory (MCC). They contain no real student, program, or institutional information.

## Purpose

This document recombines two versions of the same work:

| Version | What it had | What it lacked |
|---|---|---|
| `batched_cohort_projections_prep.ipynb` (2025, Python) | Term-by-term retention calculated from student records, run separately for each program segment, with separate rates for fall, spring and summer entrants | Financial layer, presentation |
| Current site (`src/`, React) | Net Tuition Revenue (NTR), aid tiers, WICHE scenarios, cohort matrix visuals | Segments; its rates are typed into the site source instead of calculated from data |

The demo below rebuilds the notebook's engine in base R, runs it on four segments of the current synthetic file, and backtests it. The last section lists what is missing or inconsistent and needs a decision before the engine replaces the hardcoded rates on the site.

## Method

1. **Term coding.** `YYYYSU = YYYY1`, `YYYYFA = YYYY2`, `YYYYSP = (YYYY-1)3`. The integers sort chronologically, and each academic year runs summer, fall, spring.
2. **Cohort matrix.** For each segment, count the students enrolled in each term since entry (`d`), for each entering cohort. MCM runs three terms a year. MCC runs two, so its summer records are dropped.
3. **Continuation ratios.** `rate(d) = heads(d) / heads(d − offset)`.
   - `offset = 2` for a fall term at MCM. Fall is measured from the prior spring, because summer is optional and skipping it is not leaving.
   - The exception is a summer entrant's first fall, which uses `offset = 1`.
   - Everything else uses `offset = 1`.
4. **Average rates.** Rates are averaged separately for each entry season (fall, spring, summer starters). As in the notebook, this is a simple mean of each cohort's ratio, counting a cohort only where both cells are non-zero, with no cap. Size weighting (`weighted = TRUE`) and a cap at 1 (`cap = TRUE`) are available as options. Three variants:
   - all non-COVID years (the notebook's default)
   - a trailing three-year window
   - COVID terms included, as a stress scenario. The COVID terms are 2020FA, 2021SP and 2021SU.
5. **Projection.** Each unobserved cell = its reference cell × the average rate for that entry season and term since entry. Future entering classes come from admissions targets. The demo uses the mean of the last three entering classes of the same season as a placeholder.
6. **Census.** The headcount for a calendar term is the sum of that term's diagonal across all cohorts.
7. **Revenue.** Fall NTR = projected fall headcount × NTR per enrolled student-term. The site's formula is `Gross Tuition − Institutional Grant + Comprehensive Fee`.

### Changes from the 2025 notebook

| Notebook behavior | Demo behavior | Why |
|---|---|---|
| Simple mean of cohort ratios; zero-count cells skipped | Same by default; size weighting available with `weighted = TRUE` | Kept as written. Weighting stops a 15-student cohort counting as much as an 800-student one, but it is your call. |
| Ratios not capped | Same by default; cap at 1 available with `cap = TRUE` | Kept as written. A cap stops stop-out returns from compounding above 1, but it is your call. |
| Final diagonal sum calls `get_terms_ahead(j, i)` without `include_summer` | The census uses the school's own calendar | Fixes a misaligned total for the two-term school |
| Segment kept if it has more than 20 **rows** | Demo segments are chosen explicitly | The row threshold mixed up records and students |
| All years averaged | All years, or a trailing window via `years =` | Supports the three-year-mean convention |

## Code

The whole engine is base R, about 110 lines, with no packages.

```r
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

# --- 2. Continuation rates, by entry season
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
```

### Usage: one segment

```r
source("replication/R/segment_model.R")
d <- read.csv("data/synthetic_enrollment.csv", stringsAsFactors = FALSE)

seg <- d[d$school == "MCM" & d$student_program == "Undergraduate" &
         d$program == "Performance" & d$attendance_type == "Full-time", ]

DATA_DATE <- term_to_int("2025SP")
m      <- cohort_matrix(seg, tpy = 3L, max_d = 11L)
rates  <- segment_rates(m, tpy = 3L, data_date = DATA_DATE)
future <- c("2025FA", "2026SP", "2026FA", "2027SP", "2027FA", "2028SP",
            "2028FA", "2029SP", "2029FA", "2030SP", "2030FA")
ent    <- default_entering(m, DATA_DATE, future)
proj   <- project_segment(m, rates, tpy = 3L, data_date = DATA_DATE, entering = ent)
hc     <- headcount_by_term(proj)
hc[grepl("FA$", hc$term) & hc$term >= "2025FA" & hc$term <= "2030FA", ]
```

## Demo results

Data cutoff: **2025SP**, the last term with an entering class in the synthetic file (see Gap 2). Segments:

| Segment | School calendar | Students |
|---|---|---|
| MCM · Undergraduate · Performance · Full-time | 3 terms | 1,525 |
| MCM · Undergraduate · Music Business · Full-time | 3 terms | 1,533 |
| MCC · Undergraduate · Composition · Full-time | 2 terms | 679 |
| MCM · Graduate · Full-time (all programs) | 3 terms | 1,355 |

### Continuation rates: MCM Undergraduate Performance

`d` = terms since entry. For fall entrants: d1 = spring, d2 = summer, d3 = second fall (measured from spring).

| d | Fall entrants (all years) | Fall entrants (3-year) | Fall entrants (with COVID) | Spring entrants (all years) |
|---|---|---|---|---|
| 0 | 1.000 | 1.000 | 1.000 | 1.000 |
| 1 | 0.804 | 0.796 | 0.803 | 0.782 |
| 2 | 0.772 | 0.781 | 0.784 | 0.608 |
| 3 | 0.593 | 0.603 | 0.590 | 0.722 |
| 4 | 0.669 | 0.685 | 0.679 | 0.736 |
| 5 | 0.636 | 0.618 | 0.665 | 0.486 |
| 6 | 0.363 | 0.318 | 0.367 | 0.687 |
| 7 | 0.743 | 0.744 | 0.721 | 0.576 |
| 8 | 0.512 | 0.512 | 0.527 | 0.344 |

### Year-2 fall survival by segment (fall entrants)

| Segment | Spring (d1) | Second fall from spring | Year-2 fall survival |
|---|---|---|---|
| MCM UG Performance | 0.804 | 0.593 | 0.477 |
| MCM UG Music Business | 0.821 | 0.588 | 0.483 |
| MCC UG Composition | 0.847 | 0.765 | 0.648 |
| MCM Graduate | 0.821 | 0.561 | 0.461 |

### Fall headcount projection, 2025FA–2030FA

Entering classes are held at the three-cohort mean, so the projections settle to a steady state. Applying WICHE factors to the entering targets is where the site's scenario layer would attach.

| Segment | 2025FA | 2026FA | 2027FA | 2028FA | 2029FA | 2030FA |
|---|---|---|---|---|---|---|
| MCM UG Performance | 281 | 280 | 279 | 278 | 278 | 278 |
| MCM UG Music Business | 283 | 282 | 280 | 279 | 279 | 279 |
| MCC UG Composition | 193 | 194 | 191 | 190 | 189 | 189 |
| MCM Graduate | 235 | 235 | 235 | 235 | 235 | 235 |

Revenue is left out until the units of `ntr` are confirmed (Gap 9).

### Normalized cohorts and the spreadsheet view

Each cohort's counts divided by its entering headcount, for MCM UG Performance. Blank cells are not yet observed at the 2025SP cutoff. This is the actual history the average rates are drawn from.

| d | 2022FA | 2023SP | 2023FA | 2024SP | 2024FA | 2025SP |
|---|---|---|---|---|---|---|
| 0 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 | 1.000 |
| 1 | 0.800 | 0.738 | 0.835 | 0.785 | 0.753 | |
| 2 | 0.570 | 0.619 | 0.701 | 0.579 | | |
| 3 | 0.467 | 0.381 | 0.526 | 0.388 | | |
| 4 | 0.319 | 0.274 | 0.381 | | | |
| 5 | 0.207 | 0.190 | | | | |
| 6 | 0.126 | 0.155 | | | | |

The average rates turn into one cumulative persistence curve per entry season. This is the column a spreadsheet multiplies by the admissions target:

> **Projected heads(d) = entering target × persistence(d)**

| d | Term (fall entrant) | Persistence |
|---|---|---|
| 0 | Fall, year 1 | 1.0000 |
| 1 | Spring, year 1 | 0.8043 |
| 2 | Summer | 0.6213 |
| 3 | Fall, year 2 | 0.4772 |
| 4 | Spring, year 2 | 0.3193 |
| 6 | Fall, year 3 | 0.1161 |
| 9 | Fall, year 4 | 0.0292 |

The curve matches the observed history: the 2022FA cohort reached 0.467 at d3, against a curve value of 0.477. Exported files:
- `output/segment_persistence_curves.csv`: curves for all four segments, for fall and spring entrants
- `output/mcm_ug_performance_normalized_cohorts.csv`: the normalized matrix

Both files open directly in Google Sheets or Excel.

### Backtest

Rates were estimated from data through 2023FA. Entering classes after that were set to their actual sizes, and the projections were compared with the actual census.

| Segment | 2024SP | 2024FA | 2025SP |
|---|---|---|---|
| MCM UG Performance | 300 vs 298 (+0.7%) | 257 vs 259 (−0.8%) | 325 vs 321 (+1.2%) |
| MCM UG Music Business | 304 vs 311 (−2.3%) | 262 vs 261 (+0.4%) | 331 vs 334 (−0.9%) |
| MCC UG Composition | 176 vs 176 (0.0%) | 183 vs 182 (+0.5%) | 198 vs 190 (+4.2%) |
| MCM Graduate | 279 vs 272 (+2.6%) | 220 vs 224 (−1.8%) | 287 vs 284 (+1.1%) |

All errors fall within ±4.2%. This is the defensible replacement for the hardcoded "92% forecasting accuracy" in the 2025 notebook. It flatters the method, though, because the synthetic generator appears to use stable rates (Gaps 6 and 7). A backtest on real data would show larger errors.

## Gaps and open questions

### Data and provenance

1. **The generator is missing.** The CSV has an MD5 checksum in the README, but no script shows how it was made. A seeded, readable generator is the proof the data is synthetic. It needs to be rebuilt, which will change every downstream number.
   Calibration targets come from public sources only, as ranges for a peer group rather than one identifiable school:
   - IPEDS full-time first-year retention (fall to next fall). The engine's year-2 fall survival for fall entrants should land in that range.
   - IPEDS 4-year and 6-year graduation rates. These require graduation codes (Gap 8).
   - IPEDS enrollment mix: undergraduate vs graduate, full-time vs part-time.
   - NACUBO discount-rate benchmarks, for aid tiers.
   - Summer attendance and spring-entrant behavior are not in IPEDS, so they stay as stated assumptions.
2. **Entering classes stop at 2025SP, but records continue to 2030SP.** There is no 2025FA or later entering cohort. Records after 2025SP are simulated continuations of existing students, which is why the raw census falls from 259 (2024FA) to 178 (2025FA) to 9 (2027FA) for the Performance segment. Decision needed: what is the file's "as of" date?
3. **The site's numbers do not come from this CSV.**

   | Measure | Site | This CSV |
   |---|---|---|
   | MCM Year-2 retention | 86.1% | about 48% for full-time undergraduates |
   | Summer attendance | 13.3% | about 73–77% of spring headcount attends summer |
   | 2026FA baseline headcount | 5,464 | no entering class after 2025SP to support it |

   The normalized curve makes this sharper. In the CSV, only about 12% of a fall entering class is still enrolled by year 3, and about 3% by year 4. That attrition is far steeper than the site describes, or than a real conservatory would see.

   **Resolved (Nov 4, 2026):** the site's rates first appear in a single commit (`student-persistence-model` 4656939, Oct 21, 2026) and are not produced by any synthetic file or code found. They were most likely made up by an AI assistant. Every rate should come from the synthetic data and nothing else. To make the synthetic data realistic, calibrate the generator to public IPEDS figures (see Gap 1), then calculate every rate from the generated records.
4. **No summer entrants.** Every cohort starts in fall or spring, but both codebases handle summer starters. The `112/120` constant for a summer cohort's first fall has no data behind it here.
5. **MCC has spring entrants.** The notebook's two-term logic (`OTHER_ENTERING = False`) used fall cohorts only. Decision needed: does the two-term school admit in spring?
6. **Programs behave almost identically.** Performance and Music Business differ by about one point at every step, because the generator doesn't vary behavior by program. Right now the segment model demonstrates the method, not an insight. Decision needed: how should programs, regions and attendance types differ? This is your domain knowledge.
7. **There is no COVID shock in the data.** Rates with and without COVID differ by about a point, so the COVID adjustment can't be demonstrated. Decision needed: should the generator include a 2020–21 disruption, so the stress scenario has something to show?

### Field definitions

8. **`enrollment_status` is "E" on every row.** There is no graduation, withdrawal or stop-out code, so survival mixes completers with leavers. The low late-term rates (d8–d11) are probably graduation, not attrition. A degree-conferral term is needed to separate them.
9. **Units of `ntr` and `institutional_grant`.** Gross (`ntr + institutional_grant`) is about $47,100 per row. The site implies about $19,050 NTR per student-term. Decision needed: is a row one term, or one year?
10. **Aid tiers overlap.** In the CSV, "Minimal Aid" covers 9–24% discounts and "Partial Aid" covers 23–49%. The ranges overlap, and they don't match the site's thresholds (under 15%, 15–35%, 35–60%, 60–80%, 80% and up). The tiers weren't derived from those thresholds.
11. **What does "Continuing" mean for an entering cohort?** Every student's first record equals their `cohort_term`, and Continuing students are spread across all cohorts, so the label isn't marking students already enrolled when the data begins. The notebook included `enrollment_type` in its segment code. Decision needed: what did it mean in the original system?
12. **Graduate persistence.** Graduate students persist at meaningful rates up to d11, about four years. Most master's programs run two years. Decision needed: what program length should each level have?
13. **The `region` field mixes geography with delivery.** In-State, Out-of-State and International are origin; Online is delivery mode. A student can be both.

### Method decisions

14. **Lookback.** The notebook averaged all non-COVID years. You described three-year means. Both are implemented (`years = 3`). Decision needed: which is the default?
15. **Segment definition and minimum size.** The notebook crossed region × level × enrollment type × attendance × program (up to 240 segments on about 16,000 students). Proposal: four to six named example segments, with a minimum of about 200 students.
16. **Entering targets.** The demo uses the mean of the last three cohorts. In practice this is the admissions headcount goal, entered by hand. Decision needed: should it be a pipeline input file?
17. **Horizon.** The notebook tracked 20 terms; the demo tracks 4 years (12 terms at MCM, 8 at MCC). Students beyond the horizon are dropped from the census.

### Context only you have

18. What the statistician's original script did, compared with what you added. This matters for how the work is credited.
19. Whether the production model was ever backtested against actual census figures, and what the error was.
20. Which institutional benchmarks (NACUBO, IPEDS, WICHE) the original reports cited, and for which numbers.
