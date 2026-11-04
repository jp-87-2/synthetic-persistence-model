# Pipeline glue: Stage 1 inputs in R -> Stage 2 model in Python (via reticulate)
# -> results back in R.

spm_use_python <- function(python = Sys.getenv("RETICULATE_PYTHON")) {
  if (!nzchar(python)) python <- Sys.which("python3")
  if (nzchar(python)) reticulate::use_python(python, required = TRUE)
  for (m in c("numpy", "pandas")) {
    if (!reticulate::py_module_available(m)) {
      stop("Python module '", m, "' is not available to reticulate (", python, ")")
    }
  }
  invisible(reticulate::py_config()$python)
}

run_persistence_pipeline <- function(root = spm_find_root()) {
  spm_use_python()
  site <- spm_load_site_data(root)
  wiche <- spm_load_wiche(root)
  py <- reticulate::import_from_path(
    "stage2_model", path = file.path(root, "replication", "python"), convert = TRUE)
  stage2 <- py$run_stage2(root)
  list(root = root, site = site, wiche = wiche, A = stage2$A, B = stage2$B,
       python = reticulate::py_config()$python)
}
