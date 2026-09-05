.boinetc_parent_value <- function(name, default = NULL, env = parent.frame(2L), inherits = TRUE) {
  if (exists(name, envir = env, inherits = inherits)) {
    get(name, envir = env, inherits = inherits)
  } else {
    default
  }
}

.boinetc_resolve_outdir <- function(outdir = NULL, env = parent.frame(2L)) {
  if (!is.null(outdir)) {
    return(outdir)
  }
  if (exists("OUTFILE", envir = env, inherits = TRUE)) {
    return(get("OUTFILE", envir = env, inherits = TRUE))
  }
  if (exists("OUT", envir = env, inherits = TRUE)) {
    return(get("OUT", envir = env, inherits = TRUE))
  }
  getOption("BOINETC.outdir", ".")
}

.boinetc_ensure_outdir <- function(outdir) {
  if (is.null(outdir) || !nzchar(outdir)) {
    outdir <- "."
  }
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }
  outdir
}

.boinetc_output_file <- function(outdir, filename, suffix) {
  file.path(.boinetc_ensure_outdir(outdir), paste0(filename, suffix))
}

.boinetc_ratio <- function(num, den) {
  z <- num / den
  z[!is.finite(z)] <- NA_real_
  z
}

.boinetc_method_name <- function(method) {
  method <- as.character(method)
  if (method %in% c("1", "m1", "BOINETC_m1", "BOIN-ETC1", "BOINETC1")) {
    "BOINETC_m1"
  } else if (method %in% c("2", "m2", "BOINETC_m2", "BOIN-ETC2", "BOINETC2")) {
    "BOINETC_m2"
  } else if (method %in% c("3", "m3", "BOINETC_m3", "BOIN-ETC3", "BOINETC3")) {
    "BOINETC_m3"
  } else {
    stop("Unknown BOIN-ETC method: ", method, call. = FALSE)
  }
}

.boinetc_method_function <- function(method) {
  method <- .boinetc_method_name(method)
  switch(method,
         BOINETC_m1 = get.oc.BOINETC,
         BOINETC_m2 = get.oc.BOINETC_m2,
         BOINETC_m3 = get.oc.BOINETC_m3)
}

.boinetc_biviso <- function(x) {
  for (pkg in c("UniIsoRegression", "Iso")) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      ns <- asNamespace(pkg)
      if (exists("biviso", envir = ns, inherits = FALSE)) {
        return(get("biviso", envir = ns, inherits = FALSE)(x))
      }
    }
  }
  stop("Function 'biviso' was not found. Install a package that provides it, such as 'UniIsoRegression' or 'Iso'.", call. = FALSE)
}
