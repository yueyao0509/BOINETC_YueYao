#############################################################################
# Modified operating-characteristic function for BOIN-ETC method 2.
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

get.oc.BOINETC_m2 <- function(ncohort, cohortsize, n.earlystop, ntrial,
                          phi, delta, lambda1, lambda2, eta1,
                          tdose, u.min, n.scr,
                          pt.true.mat, pe.true.mat, filename){

  nd1 <- nrow(pt.true.mat); nd2 <- ncol(pt.true.mat)
  OUT.STUDY <- .init_outstudy(nd1, nd2)

  # Same overall method-2 family as original, but no longer restarts every strip
  # at local dose 1. It projects the previously selected absolute dose onto the
  # next strip, which is more efficient for interior ODC scenarios.
  dosespace <- list(c(1:nd1, (2:nd2)*nd1))
  for (j in (nd1-1):1){
    dosespace <- append(dosespace, list(1:(nd2-1)*nd1 + j))
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
                                  util.weight = u)

      npts <- sub$npts; ntox <- sub$ntox; neff <- sub$neff
      nct1 <- sub$nct1; nct2 <- sub$nct2; nct3 <- sub$nct3; nct4 <- sub$nct4
      elimi <- sub$elimi; sum.n <- sub$sum.n

      # method-2 path update, still strip-based but adaptive.
      if (jj == 1){
        if (sub$l.selectdose == 1){dspace <- nd1}
        else if (sub$l.selectdose <= nd1){dspace <- nd1 - sub$l.selectdose + 2}
        else {dspace <- dspace + 1}
      } else {
        dspace <- dspace + 1
      }
      if (dspace > nd1 || sub$sum.n >= ncohort*cohortsize) break
      startdose <- .closest_start(sub$selectdose, dosespace[[dspace]])
    }
    OUT.STUDY <- .append_trial(OUT.STUDY, npts, ntox, neff, nct1, nct2, nct3, nct4)
  }
  write.csv(x=OUT.STUDY, file=paste(OUTFILE, filename, "_data.csv", sep=""))
}
