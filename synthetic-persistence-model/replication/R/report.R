# Console output for res = run_persistence_pipeline().

spm_rule <- function(title) cat("\n", strrep("=", 78), "\n ", title, "\n", strrep("=", 78), "\n", sep = "")

spm_print <- function(df, digits = 3) print(format(df, digits = digits, big.mark = ","), row.names = FALSE)

# Full results (used by the RStudio script).
spm_print_results <- function(res) {
  spm_rule("A. calculator.ts port on siteData.entrant (4-year entrant NTR)")
  for (s in c("MCM", "MCC")) {
    sm <- res$A[[s]]$summary
    cat(sprintf("\n%s  entering %d | net/term $%s | Y2 Fall retention %.1f%% | 4-yr NTR $%.2fM [%.2f, %.2f] | per entrant $%s | tier %s\n",
                s, sm$entering, format(round(sm$net_per_term), big.mark = ","),
                100 * sm$implied_fall_to_fall_retention, sm$total_ntr / 1e6,
                sm$total_low_ntr / 1e6, sm$total_high_ntr / 1e6,
                format(round(sm$ntr_per_entrant), big.mark = ","), sm$aid_tier_label))
    spm_print(res$A[[s]]$rows[, c("year", "entry_term_heads", "spring_term_heads", "summer_term_heads",
                                  "student_terms", "ntr", "low_ntr", "high_ntr")])
  }
  cv <- res$A$MCM$curve
  cat(sprintf("\nMCM Year-2 Fall survival: offset=2 %.3f vs naive offset=1 %.3f\n",
              cv$rebuilt[4], cv$naive_offset1[4]))

  spm_rule("B. Site cohort tables (parsed from .tsx): census diagonal & vintage")
  cs <- res$B$census[res$B$census$exclude_covid, ]
  spm_print(cs[, c("school", "offset", "census_term", "heads", "ntr_m")])
  cat("\n"); spm_print(res$B$vintage)
  cat("\nCombined MCM + MCC discount tiers (CohortsSection.tsx):\n")
  spm_print(res$B$combined_tiers)
}

# Short console summary.
spm_print_summary <- function(res) {
  cat("Student Persistence Model (R + Python)\n\n")
  cat("4-year entrant NTR (siteData inputs):\n")
  for (s in c("MCM", "MCC")) {
    sm <- res$A[[s]]$summary
    cat(sprintf("  %s  entering %4d  Y2 retention %.1f%%  NTR $%.1fM  [%.1f-%.1f]  per entrant $%s\n",
                s, sm$entering, 100 * sm$implied_fall_to_fall_retention, sm$total_ntr / 1e6,
                sm$total_low_ntr / 1e6, sm$total_high_ntr / 1e6,
                format(round(sm$ntr_per_entrant), big.mark = ",")))
  }
  cs <- res$B$census
  cs <- cs[cs$exclude_covid & cs$school == "MCM", ]
  w <- reshape(cs[, c("census_term", "offset", "heads")], idvar = "census_term",
               timevar = "offset", direction = "wide")
  names(w) <- c("census_term", "offset_2_heads", "offset_1_heads")
  cat("\nMCM census diagonal (site cohort tables):\n")
  print(w, row.names = FALSE)
  invisible(res)
}
