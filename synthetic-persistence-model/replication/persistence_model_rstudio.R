# Student Persistence Model

# ---- setup ----
library(ggplot2)
library(reticulate)
# Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3")  # pick an interpreter with numpy + pandas

root <- getwd()
while (!file.exists(file.path(root, "src", "data", "siteData.json"))) {
  if (dirname(root) == root) stop("Set the working directory inside the repository.")
  root <- dirname(root)
}
for (f in c("stage1_etl.R", "pipeline.R", "report.R", "plots.R")) {
  source(file.path(root, "replication", "R", f))
}

# ---- run model (R inputs -> Python model) ----
res <- run_persistence_pipeline(root)

# ---- console results ----
spm_print_results(res)

# ---- 1. survival curves: offset = 2 vs naive offset = 1 ----
print(spm_plot_survival(res))

# ---- 2. census diagonal offset comparison (site tables) ----
print(spm_plot_census(res))

# ---- 3. WICHE scenarios ----
print(spm_plot_wiche(res))
print(spm_plot_scenarios(res))

# ---- 4. entrant 4-year NTR calculator ----
print(spm_plot_calculator(res))

# ---- 5. vintage rate / mix decomposition ----
print(spm_plot_vintage(res))

# ---- 6. discount tiers: retained vs at-risk NTR ----
print(spm_plot_tiers(res))
