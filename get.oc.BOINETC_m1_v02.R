#####################################################################
# get.oc.BOINETC_m1_v02.R: function to calculate operating characteristics of BOIN-ETC1
#####################################################################
get.oc.BOINETC <- function(ncohort, cohortsize, n.earlystop, ntrial,
                           phi, delta, lambda1, lambda2, eta1, 
                           tdose, u.min, n.scr,
                           pt.true.mat, pe.true.mat, filename){

  nd1 <- nrow(pt.true.mat); nd2 <- ncol(pt.true.mat); nd <- nd1*nd2;

  OUT.STUDY <- rbind(rep(rep(1:nd1, nd2), 14),
                     rep(rep(1:nd2, each=nd1), 14))
  colnames(OUT.STUDY) <- paste(rep(c("npts", "ntox", "neff", 
                                     "ppts", "ptox", "peff", 
                                     "nct1", "nct2", "nct3", "nct4", 
                                     "pct1", "pct2", "pct3", "pct4"), 
                                   each=nd1*nd2), 
                               rep(rep(1:nd1, nd2), 14), 
                               rep(rep(1:nd2, each=nd1), 14), sep="")

  dosespace <- list(c(1:nd1, (2:nd2)*nd1))
  for (j in (nd1-1):1){
    dosespace  <- append(dosespace, list(1:(nd2-1)*nd1+j))
  }

  j.rd <- j.rd2 <- n.tdose <- n.tdose2 <- 0;
  
  for (jjj in 1:ntrial){

    ntox <- neff <- npts <- elimi <- matrix(0, nrow=nd1, ncol=nd2)
    nct1 <- nct2 <- nct3 <- nct4 <- matrix(0, nrow=nd1, ncol=nd2)
    sum.tox <- sum.eff <- sum.n <- sum.cohort <- 0
    non.rd <- 0; non.rd2 <- 0;
    odc     <- numeric(length(dosespace))
    dspace <- 1; startdose <- 1

    for (jj in 1:nd1){
      pt.true <- as.vector(pt.true.mat)[dosespace[[dspace]]]
      pe.true <- as.vector(pe.true.mat)[dosespace[[dspace]]]

      sub <- BOINETC.subtrial(pt.true, pe.true, dosespace[[dspace]],
                      npts, ntox, neff, nct1, nct2, nct3, nct4, 
                      elimi, ncohort, cohortsize, 
                      phi, delta, lambda1, lambda2, eta1, 
                      n.earlystop, startdose, 
                      cutoff.eli = 0.95, extrasafe = FALSE, 
                      offset = 0.05,
                      sum.nn = ncohort*cohortsize-sum.n, 
                      titration.first.trial = FALSE)

      odc[jj] = sub$selectdose
      sum.cohort = sum.cohort + sub$ncohort
      npts = sub$npts; ntox = sub$ntox; neff = sub$neff; 
      nct1 = sub$nct1; nct2 = sub$nct2; 
      nct3 = sub$nct3; nct4 = sub$nct4; 
      sum.tox = sub$sum.tox; sum.eff = sub$sum.eff; 
      sum.n = sub$sum.n;
      estop = sub$estop; elimi = sub$elimi;
      l.selectdose = sub$l.selectdose;

      if (jj == 1){
        if (l.selectdose == 1){dspace <- nd1; startdose <- 1}
        else if (l.selectdose<=nd1){
          dspace <- nd1-l.selectdose+2; startdose <- 1;
        }
        else {dspace <- 2; startdose <- l.selectdose-nd1}
      } 
      else {
        dspace <- dspace+1; startdose <- l.selectdose
      }

      if (dspace > nd1){break}
      if (sub$sum.n >= ncohort*cohortsize){break}
      
    }

    ### percentage of number of patients assigned by dose level
    ppts = npts/sum(npts)
    ### percentage of number of tox by dose level
    ptox = ntox/npts
    ### percentage of number of tox by dose level
    peff = neff/npts
    ### percentage of number in cat1 (tox=0, eff=1) by dose level
    pct1 = nct1/npts
    ### percentage of number in cat2 (tox=0, eff=0) by dose level
    pct2 = nct2/npts
    ### percentage of number in cat3 (tox=1, eff=1) by dose level
    pct3 = nct3/npts
    ### percentage of number in cat4 (tox=1, eff=0) by dose level
    pct4 = nct4/npts
    
    ### output npts, ntox, neff
    OUT.STUDY <- rbind(OUT.STUDY, c(as.vector(npts),
                                    as.vector(ntox),
                                    as.vector(neff),
                                    as.vector(ppts),
                                    as.vector(ptox),
                                    as.vector(peff),
                                    as.vector(nct1),
                                    as.vector(nct2),
                                    as.vector(nct3),
                                    as.vector(nct4),
                                    as.vector(pct1),
                                    as.vector(pct2),
                                    as.vector(pct3),
                                    as.vector(pct4)))
    
  }

  #############################################################################
  # Output (csv file)
  #############################################################################
  write.csv(x=OUT.STUDY, 
            file=paste(OUTFILE, filename, "_data.csv", sep=""))
}

#############################################################################
# End of program
#############################################################################
