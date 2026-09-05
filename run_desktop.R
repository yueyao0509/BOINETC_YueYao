this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE), error = function(e) NA_character_)
app_dir <- if (!is.na(this_file)) dirname(this_file) else normalizePath(getwd(), winslash = "/", mustWork = TRUE)
setwd(app_dir)

cat("BOIN-ETC launcher: app directory = ", app_dir, "\n", sep = "")
flush.console()

repos <- c(CRAN = "https://cloud.r-project.org")
needed <- c("shiny", "DT", "openxlsx", "Iso")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installing missing R package(s): ", paste(missing, collapse = ", "), "\n", sep = "")
  flush.console()
  utils::install.packages(missing, repos = repos, dependencies = TRUE)
}

still_missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing) > 0) {
  stop("Required R package(s) could not be installed: ", paste(still_missing, collapse = ", "), call. = FALSE)
}

# V5.10 needs BOINETC 0.1.1 because the result tables include parameter_set
# and the parameter values used by each run. Install only when necessary.
tarball <- file.path(app_dir, "BOINETC_0.1.1.tar.gz")
need_boinetc <- !requireNamespace("BOINETC", quietly = TRUE)
if (!need_boinetc) {
  installed_version <- tryCatch(as.character(utils::packageVersion("BOINETC")), error = function(e) "0.0.0")
  need_boinetc <- utils::compareVersion(installed_version, "0.1.1") < 0
}

if (need_boinetc) {
  if (!file.exists(tarball)) stop("BOINETC_0.1.1.tar.gz was not found in the app folder.", call. = FALSE)
  cat("Installing local BOINETC 0.1.1 package...\n")
  flush.console()
  # Pure-R package: no Rtools compilation is expected.
  utils::install.packages(tarball, repos = NULL, type = "source", dependencies = FALSE)
}

if (!requireNamespace("BOINETC", quietly = TRUE)) {
  stop("BOINETC could not be loaded after installation.", call. = FALSE)
}
installed_version <- tryCatch(as.character(utils::packageVersion("BOINETC")), error = function(e) "unknown")
cat("BOINETC version: ", installed_version, "\n", sep = "")
cat("Checking app.R syntax...\n")
flush.console()
parse(file.path(app_dir, "app.R"))
cat("app.R syntax OK.\n")
cat("Starting Shiny at http://127.0.0.1:8765\n")
flush.console()
shiny::runApp(app_dir, host = "127.0.0.1", port = 8765, launch.browser = FALSE)
