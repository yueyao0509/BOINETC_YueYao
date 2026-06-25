# BOINETC Shiny app installer/launcher
# Set the working directory to this folder before running, or source this file from RStudio.

this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE), error = function(e) NA_character_)
app_dir <- if (!is.na(this_file)) dirname(this_file) else getwd()

needed <- c("shiny", "DT", "openxlsx", "Iso")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing)
}

tarball <- file.path(app_dir, "BOINETC_0.1.0.tar.gz")
if (!requireNamespace("BOINETC", quietly = TRUE)) {
  if (!file.exists(tarball)) {
    stop("BOINETC_0.1.0.tar.gz was not found in this folder.", call. = FALSE)
  }
  install.packages(tarball, repos = NULL, type = "source")
}

shiny::runApp(app_dir, launch.browser = TRUE)
