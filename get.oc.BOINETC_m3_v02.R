#############################################################################
# Modified operating-characteristic function for BOIN-ETC method 2/3.
#############################################################################

.init_outstudy <- function(nd1, nd2){
  OUT.STUDY <- rbind(rep(rep(1:nd1, nd2), 14),
                     rep(rep(1:nd2, each=nd1), 14))
  colnames(OUT.STUDY) <- paste(rep(c("npts", "ntox", "neff",
                                     "ppts", "ptox", "peff",
                                     "nct1", "nct2", "nct3", "nct4",
                                     "pct1", "pct2", "pct3", "pct4"),
                                   each=nd1*nd2),
                               rep(rep(1:nd1, nd2), 14),
                               rep(rep(1:nd2, each=nd1), 14), sep="")
  OUT.STUDY
}

.append_trial <- function(OUT.STUDY, npts, ntox, neff, nct1, nct2, nct3, nct4){
  safe_div <- function(a, b) ifelse(b > 0, a/b, NA)
  ppts <- npts/sum(npts)
  ptox <- safe_div(ntox, npts)
  peff <- safe_div(neff, npts)
  pct1 <- safe_div(nct1, npts)
  pct2 <- safe_div(nct2, npts)
  pct3 <- safe_div(nct3, npts)
  pct4 <- safe_div(nct4, npts)
  rbind(OUT.STUDY, c(as.vector(npts), as.vector(ntox), as.vector(neff),
                     as.vector(ppts), as.vector(ptox), as.vector(peff),
                     as.vector(nct1), as.vector(nct2), as.vector(nct3), as.vector(nct4),
                     as.vector(pct1), as.vector(pct2), as.vector(pct3), as.vector(pct4)))
}

.closest_start <- function(abs.selectdose, next.space){
  if (is.na(abs.selectdose) || abs.selectdose == 99) return(1)
  which.min(abs(next.space - abs.selectdose))
}

get.oc.BOINETC_m3 <- function(ncohort, cohortsize, n.earlystop, ntrial,
                          phi, delta, lambda1, lambda2, eta1,
                          tdose, u.min, n.scr,
                          pt.true.mat, pe.true.mat, filename){

  nd1 <- nrow(pt.true.mat); nd2 <- ncol(pt.true.mat)
  OUT.STUDY <- .init_outstudy(nd1, nd2)

  # Same diagonal family as original BOIN-ETC3, but startdose is projected from
  # the prior selected absolute dose. This prevents repeated bottom starts and
  # allocates more patients near likely interior ODCs.
  dosespace <- list(c(1:nd1, (2:nd2)*nd1))
  for (j in 1:(nd1-1)){
    if (nd1 > j+1){
      dosespace <- append(dosespace, list(c(j*nd1 + 1:(nd1-j), ((j+2):nd2)*nd1 - j)))
    } else {
      dosespace <- append(dosespace, list(nd1*(j:(nd2-1)) + 1))
    }
  }

  for (jjj in 1:ntrial){
    ntox <- neff <- npts <- elimi <- matrix(0, nrow=nd1, ncol=nd2)
    nct1 <- nct2 <- nct3 <- nct4 <- matrix(0, nrow=nd1, ncol=nd2)
    sum.n <- 0
    dspace <- 1
    startdose <- 1

    for (jj in 1:nd1){
      pt.true <- as.vector(pt.true.mat)[dosespace[[dspace]]]
      pe.true <- as.vector(pe.true.mat)[dosespace[[dspace]]]

      sub <- BOINETC.subtrial.improved(pt.true, pe.true, dosespace[[dspace]],
                                  npts, ntox, neff, nct1, nct2, nct3, nct4,
                                  elimi, ncohort, cohortsize,
                                  phi, delta, lambda1, lambda2, eta1,
                                  n.earlystop, startdose=startdose,
                                  cutoff.eli = 0.95, extrasafe = FALSE,
                                  offset = 0.05,
                                  sum.nn = ncohort*cohortsize-sum.n,
                                  titration.first.trial = FALSE,
                                  util.weight = u,
                                  explore.weight = 0.070,
                                  eff.weight = 0.18)

      npts <- sub$npts; ntox <- sub$ntox; neff <- sub$neff
      nct1 <- sub$nct1; nct2 <- sub$nct2; nct3 <- sub$nct3; nct4 <- sub$nct4
      elimi <- sub$elimi; sum.n <- sub$sum.n

      dspace <- dspace + 1
      if (dspace > nd1 || sub$sum.n >= ncohort*cohortsize) break
      startdose <- .closest_start(sub$selectdose, dosespace[[dspace]])
    }
    OUT.STUDY <- .append_trial(OUT.STUDY, npts, ntox, neff, nct1, nct2, nct3, nct4)
  }
  write.csv(x=OUT.STUDY, file=paste(OUTFILE, filename, "_data.csv", sep=""))
}
