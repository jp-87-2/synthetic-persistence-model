# ggplot2 figures for the persistence model results (res = run_persistence_pipeline()).

spm_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(),
                   plot.title.position = "plot")
}

spm_plot_survival <- function(res) {
  d <- do.call(rbind, lapply(c("MCM", "MCC"), function(s) {
    cv <- res$A[[s]]$curve
    rbind(data.frame(school = s, t = cv$t, survival = cv$rebuilt, rule = "calendar-aware offset"),
          if (s == "MCM") data.frame(school = s, t = cv$t, survival = cv$naive_offset1,
                                     rule = "naive offset = 1"))
  }))
  ggplot2::ggplot(d, ggplot2::aes(t, survival, colour = rule)) +
    ggplot2::geom_line(linewidth = 0.9) + ggplot2::geom_point(size = 1.6) +
    ggplot2::facet_wrap(~school, scales = "free_x") +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_colour_manual(values = c("calendar-aware offset" = "#2e6f4e",
                                            "naive offset = 1" = "#9c998e")) +
    ggplot2::labs(title = "Cumulative survival S(t) for Fall entrants",
                  subtitle = "MCM Fall return conditions on Spring (offset = 2); MCC is a plain chain",
                  x = "Terms since entry (t)", y = "Share of entrants enrolled", colour = NULL) +
    spm_theme()
}

spm_plot_census <- function(res) {
  cs <- res$B$census
  cs <- cs[cs$school == "MCM" & cs$exclude_covid, ]
  cs$rule <- ifelse(cs$offset == 2, "offset = 2 (Fall from Spring)", "naive offset = 1")
  cs$census_term <- factor(cs$census_term, levels = unique(cs$census_term))
  ggplot2::ggplot(cs, ggplot2::aes(census_term, heads, fill = rule)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(heads)),
                       position = ggplot2::position_dodge(0.9), vjust = -0.3, size = 3) +
    ggplot2::scale_fill_manual(values = c("offset = 2 (Fall from Spring)" = "#2e6f4e",
                                          "naive offset = 1" = "#9c998e")) +
    ggplot2::labs(title = "MCM census diagonal from the site cohort tables",
                  subtitle = "Naive offset = 1 lets the optional Summer dip wipe out returning Fall heads",
                  x = "Census term", y = "Projected headcount", fill = NULL) +
    spm_theme()
}

spm_plot_scenarios <- function(res) {
  d <- do.call(rbind, lapply(names(res$site$scenarios), function(n) {
    x <- do.call(rbind, lapply(res$site$scenarios[[n]], as.data.frame)); x$scenario <- n; x
  }))
  d$year <- as.integer(substr(d$fall, 1, 4))
  pal <- c(flat = "#2e6f4e", cliff = "#b06a3a", northeast = "#2596be")
  ggplot2::ggplot(d, ggplot2::aes(year, ntr_m, colour = scenario, fill = scenario)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p10_m, ymax = p90_m), alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 0.9) + ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = pal) + ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(title = "Fall Net Tuition Revenue under WICHE scenarios, 2026FA-2030FA",
                  subtitle = "Bands: 10th-90th percentile",
                  x = "Fall", y = "Fall NTR ($M)", colour = "Scenario", fill = "Scenario") +
    spm_theme()
}

spm_plot_calculator <- function(res) {
  d <- do.call(rbind, lapply(c("MCM", "MCC"), function(s) {
    r <- res$A[[s]]$rows; r$school <- s; r
  }))
  ggplot2::ggplot(d, ggplot2::aes(factor(year), ntr / 1e6)) +
    ggplot2::geom_col(fill = "#2e6f4e") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = low_ntr / 1e6, ymax = high_ntr / 1e6), width = 0.25) +
    ggplot2::facet_wrap(~school) +
    ggplot2::labs(title = "Entering class -> 4-year NTR (calculator.ts port)",
                  subtitle = "Error bars: +/-2.5pp retention sensitivity",
                  x = "Academic year", y = "NTR ($M)") +
    spm_theme()
}

spm_plot_vintage <- function(res) {
  d <- do.call(rbind, lapply(res$site$vintage, as.data.frame))
  long <- rbind(data.frame(cohort = d$cohort, effect = "rate", heads = d$rate_alpha_heads),
                data.frame(cohort = d$cohort, effect = "tier mix", heads = d$mix_alpha_heads))
  long$cohort <- factor(long$cohort, levels = unique(d$cohort[order(spm_term_to_int(d$cohort))]))
  ggplot2::ggplot(long, ggplot2::aes(cohort, heads, fill = effect)) +
    ggplot2::geom_col() + ggplot2::geom_hline(yintercept = 0) +
    ggplot2::scale_fill_manual(values = c(rate = "#2e6f4e", "tier mix" = "#b06a3a")) +
    ggplot2::labs(title = "MCM entering-class performance: actual vs expected student-terms",
                  x = "Entry cohort", y = "Student-terms vs expected", fill = NULL) +
    spm_theme()
}

spm_plot_tiers <- function(res) {
  d <- res$B$combined_tiers
  long <- rbind(data.frame(tier = d$discount_description, part = "Retained first-year NTR", usd = d$retained_ntr),
                data.frame(tier = d$discount_description, part = "First-year NTR at risk", usd = d$ntr_at_risk))
  long$tier <- factor(long$tier, levels = d$discount_description)
  ggplot2::ggplot(long, ggplot2::aes(tier, usd / 1e6, fill = part)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c("Retained first-year NTR" = "#2e6f4e",
                                          "First-year NTR at risk" = "#b06a3a")) +
    ggplot2::labs(title = "Entering Fall class NTR by tuition discount tier (MCM + MCC)",
                  x = NULL, y = "NTR ($M)", fill = NULL) +
    spm_theme()
}

spm_plot_wiche <- function(res) {
  w <- res$wiche
  w$index <- ave(seq_len(nrow(w)), w$series, FUN = function(i)
    w$hs_graduates[i] / w$hs_graduates[i][w$class_year[i] == 2025])
  ggplot2::ggplot(w, ggplot2::aes(class_year, index, colour = series)) +
    ggplot2::geom_vline(xintercept = c(2025, 2030), linetype = "dotted") +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = c(national = "#b06a3a", northeast = "#2596be")) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(title = "WICHE high school graduates, indexed to the 2025 peak",
                  x = "Graduating class", y = "Index (2025 = 100%)", colour = NULL) +
    spm_theme()
}
