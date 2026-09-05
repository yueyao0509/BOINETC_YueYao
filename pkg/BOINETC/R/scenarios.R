# Scenario definitions for BOIN-ETC.

pt.true.mat1 <- matrix(c(0.05, 0.10, 0.30,
                         0.10, 0.30, 0.50), nrow = 2, byrow = TRUE)
pe.true.mat1 <- matrix(c(0.10, 0.25, 0.60,
                         0.25, 0.60, 0.65), nrow = 2, byrow = TRUE)

pt.true.mat2 <- matrix(c(0.05, 0.15, 0.30,
                         0.15, 0.35, 0.55), nrow = 2, byrow = TRUE)
pe.true.mat2 <- matrix(c(0.25, 0.60, 0.65,
                         0.60, 0.65, 0.70), nrow = 2, byrow = TRUE)

pt.true.mat3 <- matrix(c(0.02, 0.06, 0.12, 0.18,
                         0.04, 0.11, 0.15, 0.30,
                         0.05, 0.13, 0.30, 0.50,
                         0.10, 0.30, 0.48, 0.54), nrow = 4, byrow = TRUE)
pe.true.mat3 <- matrix(c(0.05, 0.10, 0.20, 0.30,
                         0.10, 0.20, 0.30, 0.60,
                         0.20, 0.30, 0.60, 0.65,
                         0.30, 0.60, 0.65, 0.70), nrow = 4, byrow = TRUE)

pt.true.mat4 <- matrix(c(0.02, 0.06, 0.12, 0.18,
                         0.04, 0.11, 0.15, 0.35,
                         0.05, 0.13, 0.35, 0.50,
                         0.10, 0.35, 0.48, 0.54), nrow = 4, byrow = TRUE)
pe.true.mat4 <- matrix(c(0.10, 0.20, 0.20, 0.40,
                         0.20, 0.30, 0.61, 0.65,
                         0.30, 0.60, 0.65, 0.70,
                         0.40, 0.65, 0.70, 0.75), nrow = 4, byrow = TRUE)

pt.true.mat5 <- matrix(c(0.02, 0.06, 0.12, 0.20,
                         0.04, 0.11, 0.20, 0.35,
                         0.05, 0.13, 0.35, 0.50,
                         0.10, 0.35, 0.48, 0.54), nrow = 4, byrow = TRUE)
pe.true.mat5 <- matrix(c(0.10, 0.20, 0.25, 0.30,
                         0.20, 0.30, 0.40, 0.50,
                         0.30, 0.61, 0.65, 0.70,
                         0.60, 0.65, 0.70, 0.75), nrow = 4, byrow = TRUE)

pt.true.mat6 <- matrix(c(0.01, 0.03, 0.05, 0.10,
                         0.03, 0.05, 0.10, 0.20,
                         0.05, 0.10, 0.20, 0.25,
                         0.10, 0.20, 0.25, 0.30), nrow = 4, byrow = TRUE)
pe.true.mat6 <- matrix(c(0.10, 0.20, 0.30, 0.40,
                         0.20, 0.30, 0.60, 0.60,
                         0.30, 0.60, 0.60, 0.60,
                         0.40, 0.60, 0.60, 0.60), nrow = 4, byrow = TRUE)

pt.true.mat7 <- matrix(c(0.01, 0.04, 0.11, 0.15, 0.30,
                         0.03, 0.05, 0.13, 0.30, 0.50,
                         0.07, 0.10, 0.30, 0.48, 0.54), nrow = 3, byrow = TRUE)
pe.true.mat7 <- matrix(c(0.05, 0.10, 0.20, 0.30, 0.60,
                         0.10, 0.20, 0.30, 0.60, 0.65,
                         0.20, 0.30, 0.60, 0.65, 0.70), nrow = 3, byrow = TRUE)

pt.true.mat8 <- matrix(c(0.01, 0.04, 0.11, 0.15, 0.30,
                         0.03, 0.05, 0.13, 0.30, 0.50,
                         0.07, 0.10, 0.30, 0.48, 0.54), nrow = 3, byrow = TRUE)
pe.true.mat8 <- matrix(c(0.10, 0.20, 0.30, 0.62, 0.65,
                         0.20, 0.30, 0.61, 0.65, 0.70,
                         0.30, 0.60, 0.65, 0.70, 0.75), nrow = 3, byrow = TRUE)

pt.true.mat9 <- matrix(c(0.01, 0.04, 0.11, 0.15, 0.30,
                         0.03, 0.05, 0.13, 0.30, 0.50,
                         0.07, 0.10, 0.30, 0.48, 0.54), nrow = 3, byrow = TRUE)
pe.true.mat9 <- matrix(c(0.05, 0.10, 0.20, 0.30, 0.40,
                         0.10, 0.20, 0.30, 0.60, 0.65,
                         0.20, 0.30, 0.60, 0.65, 0.70), nrow = 3, byrow = TRUE)

pt.true.mat10 <- matrix(c(0.01, 0.04, 0.09, 0.10, 0.20,
                          0.03, 0.05, 0.10, 0.20, 0.25,
                          0.07, 0.10, 0.20, 0.25, 0.30), nrow = 3, byrow = TRUE)
pe.true.mat10 <- matrix(c(0.10, 0.20, 0.30, 0.60, 0.60,
                          0.20, 0.30, 0.60, 0.60, 0.60,
                          0.30, 0.40, 0.60, 0.60, 0.60), nrow = 3, byrow = TRUE)

BOINETC_ptox <- list(pt.true.mat1,  pt.true.mat2,  pt.true.mat3,
                     pt.true.mat4,  pt.true.mat5,  pt.true.mat6,
                     pt.true.mat7,  pt.true.mat8,  pt.true.mat9,
                     pt.true.mat10)
BOINETC_peff <- list(pe.true.mat1,  pe.true.mat2,  pe.true.mat3,
                     pe.true.mat4,  pe.true.mat5,  pe.true.mat6,
                     pe.true.mat7,  pe.true.mat8,  pe.true.mat9,
                     pe.true.mat10)
BOINETC_tdose <- list(c(4, 5), c(2, 3),
                      c(8, 11, 14), c(7, 10), c(4, 7), c(7, 10),
                      c(9, 11, 13), c(6, 8, 10), c(9, 11), c(8, 10))

# Backward-compatible object names from the original main script.
list.p.truetox <- BOINETC_ptox
list.p.trueeff <- BOINETC_peff
list.tdose <- BOINETC_tdose

BOINETC_scenarios <- lapply(seq_along(BOINETC_ptox), function(i) {
  list(
    id = i,
    pt.true.mat = BOINETC_ptox[[i]],
    pe.true.mat = BOINETC_peff[[i]],
    tdose = BOINETC_tdose[[i]],
    dim = dim(BOINETC_ptox[[i]])
  )
})

get_boinetc_scenario <- function(SC) {
  if (!is.numeric(SC) || length(SC) != 1L || is.na(SC) || SC < 1L || SC > length(BOINETC_scenarios)) {
    stop("SC must be a single scenario number between 1 and ", length(BOINETC_scenarios), ".", call. = FALSE)
  }
  BOINETC_scenarios[[as.integer(SC)]]
}
