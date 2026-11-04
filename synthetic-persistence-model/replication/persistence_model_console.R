# Student Persistence Model: simple console run.
# Usage: Rscript replication/persistence_model_console.R
# Needs a Python with numpy + pandas (set RETICULATE_PYTHON to choose one).

for (f in c("stage1_etl.R", "pipeline.R", "report.R")) source(file.path("replication", "R", f))

res <- run_persistence_pipeline(spm_find_root())
spm_print_summary(res)
