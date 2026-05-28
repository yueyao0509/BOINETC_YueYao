####################################################################
# FUNC_CALC.SUM: funtion to simulation
####################################################################
CALC.SUM <- function(FILENAME, SC){
  
  ##################################################################
  # import data
  ##################################################################
  data <- read_csv(paste(OUT, FILENAME, "_data.csv", sep=""));
  DATA <- as.data.frame(sapply(data[-2:-1, -1], as.numeric));
  DOSE <- as.data.frame(sapply(data[1:2, -1],   as.numeric));

  #####################################################################
  #####################################################################
  tdose <- list.tdose[[SC]];

  # number of doselevel (A, B, A*B)
  nd1 <- max(data[1,]); nd2 <- max(data[2,]); nd <- nd1*nd2;
  m.dose <- DOSE[,nd]
  # number of studies without ODC
  non.ODC <- 0; 
  
  for (i in 1:ntrial){
    npts = matrix(as.numeric(DATA[i, 1:nd]),              nrow=nd1);
    ntox = matrix(as.numeric(DATA[i, (nd+1):(nd*2)]),     nrow=nd1);
    neff = matrix(as.numeric(DATA[i, (nd*2+1):(nd*3)]),   nrow=nd1);
    ppts = matrix(as.numeric(DATA[i, (nd*3+1):(nd*4)]),   nrow=nd1);
    ptox = matrix(as.numeric(DATA[i, (nd*4+1):(nd*5)]),   nrow=nd1);
    peff = matrix(as.numeric(DATA[i, (nd*5+1):(nd*6)]),   nrow=nd1);
    nct1 = matrix(as.numeric(DATA[i, (nd*6+1):(nd*7)]),   nrow=nd1);
    nct2 = matrix(as.numeric(DATA[i, (nd*7+1):(nd*8)]),   nrow=nd1);
    nct3 = matrix(as.numeric(DATA[i, (nd*8+1):(nd*9)]),   nrow=nd1);
    nct4 = matrix(as.numeric(DATA[i, (nd*9+1):(nd*10)]),  nrow=nd1);
    pct1 = matrix(as.numeric(DATA[i, (nd*10+1):(nd*11)]), nrow=nd1);
    pct2 = matrix(as.numeric(DATA[i, (nd*11+1):(nd*12)]), nrow=nd1);
    pct3 = matrix(as.numeric(DATA[i, (nd*12+1):(nd*13)]), nrow=nd1);
    pct4 = matrix(as.numeric(DATA[i, (nd*13+1):(nd*14)]), nrow=nd1);
  
    if (i == 1){
      NPTS = list(npts); NTOX = list(ntox); NEFF = list(neff);
      PPTS = list(ppts); PTOX = list(ptox); PEFF = list(peff);
      NCT1 = list(nct1); NCT2 = list(nct2); NCT3 = list(nct3);
      NCT4 = list(nct4); PCT1 = list(pct1); PCT2 = list(pct2);
      PCT3 = list(pct3); PCT4 = list(pct4);
    }
    else{
      NPTS = append(NPTS, list(npts));
      NTOX = append(NTOX, list(ntox));
      NEFF = append(NEFF, list(neff));
      PPTS = append(PPTS, list(ppts));
      PTOX = append(PTOX, list(ptox));
      PEFF = append(PEFF, list(peff));
      NCT1 = append(NCT1, list(nct1));
      NCT2 = append(NCT2, list(nct2));
      NCT3 = append(NCT3, list(nct3));
      NCT4 = append(NCT4, list(nct4));
      PCT1 = append(PCT1, list(pct1));
      PCT2 = append(PCT2, list(pct2));
      PCT3 = append(PCT3, list(pct3));
      PCT4 = append(PCT4, list(pct4));
    }

    ##############################################################
    # find ODCs
    ##############################################################
    ### estimate p(tox) by PAVA
    ntox.c = as.numeric(DATA[i, (nd+1):(nd*2)]);
    npts.c = as.numeric(DATA[i, 1:nd]);
    trial.ptox = matrix((ntox.c+0.05)/(npts.c+0.1), nrow=nd1);
    phat.tox   = biviso(trial.ptox);

    ### find MTD contour (dose combination with the closest P(tox) 
    ### to target by row)
    dl.MTD <- which(abs(phat.tox-phi)==apply(abs(phat.tox-phi), 1, min))

    ### find tolerable doses 
    for (k in 1:length(dl.MTD)){
      dd1 <- as.numeric(DOSE[1, dl.MTD][k]);
      dd2 <- as.numeric(DOSE[2, dl.MTD][k]);
      if (k == 1){
        dl.tol <- c(0:(dd2-1))*nd1+dd1;
      } else{
        dl.tol <- c(dl.tol, c(0:(dd2-1))*nd1+dd1);
      }
    }
    ind.tol <- matrix(numeric(nd), nrow=nd1, ncol=nd2);
    ind.tol[dl.tol] <- 1;

    ### calculate utility
    nct1.c = as.numeric(DATA[i, (nd*6+1):(nd*7)]);
    nct2.c = as.numeric(DATA[i, (nd*7+1):(nd*8)]);
    nct3.c = as.numeric(DATA[i, (nd*8+1):(nd*9)]);
    nct4.c = as.numeric(DATA[i, (nd*9+1):(nd*10)]);
  
    x = (u[1]*nct1.c + u[2]*nct2.c + u[3]*nct3.c + u[4]*nct4.c)/100;
    util = matrix((x+alpha)/(npts+alpha+beta), nrow=nd1);
    util = util*ind.tol;
    
    ### find ODCs
    odc = which(c(util)==max(util, na.rm=TRUE) & util>0);
    if (i == 1){ODC = list(odc)} else{ODC = append(ODC, list(odc))}
    if (length(odc)==0){non.ODC = non.ODC+1}
    
    ##############################################################
    # calculate total sample size
    ##############################################################
    if (i == 1){SUM.N = sum(npts)} else{SUM.N = c(SUM.N, sum(npts))};

  }

  #################################################################
  # Summary
  #################################################################
  m.NPTS = round(Reduce('+', NPTS)/ntrial, digits=2) # mean number of patients by dose level
  m.NTOX = round(Reduce('+', NTOX)/ntrial, digits=2) # mean number of tox by dose level
  m.NEFF = round(Reduce('+', NEFF)/ntrial, digits=2) # mean number of eff by dose level
  m.PPTS = round(Reduce('+', PPTS)/ntrial*100, digits=1) # percentage of number of patients treated by dose level
  m.pTOX = round(Reduce('+', NTOX)/Reduce('+', NPTS)*100, digits=1) # P(tox) by dose level
  m.pEFF = round(Reduce('+', NEFF)/Reduce('+', NPTS)*100, digits=1) # P(eff) by dose level

  tODC <- round(table(factor(Reduce(function(a,b){c(a, b)},ODC), levels = 1:nd))/ntrial*100, digits=1)
  pODC <- matrix(tODC, nrow=nd1);
  correct.pODC <- sum(pODC[tdose]);
  
  target.PPTS <- sum(m.PPTS[tdose]); # percentage of number of patients administered target doses

  #################################################################
  # Output summary results (text file)
  #################################################################
  sink(paste(OUT, FILENAME, "_sum.txt", sep=""), append=F)
  cat("#################################################################\n")
  cat("# Simulation results of BOIN-ETC \n")
  cat("#################################################################\n")
  cat("\n\n")
  
  cat("#####################################\n")
  cat("# Settings \n")
  cat("#####################################\n")
  cat(paste("simulation times               = ", ntrial, "\n", sep=""))
  cat(paste("number of max cohort           = ", ncohort, "\n", sep=""))
  cat(paste("cohortsize                     = ", cohortsize, "\n", sep=""))
  cat(paste("(target tox, target eff)       = (", phi, ",", delta, ")\n", sep=""))
  cat(paste("#pats to move to next subtrial = ", n.es, "\n", sep=""))
  cat(paste("(lambda1, lambda2, eta1)       = (", lambda1, ", ", lambda2, ", ", eta1, ")\n", sep=""))
  cat("\n")
  cat(paste("scenario = ", SC, "\n", sep=""));

  cat("true probability (tox) \n")
  print(list.p.truetox[[SC]]);
  cat("true probability (eff) \n")
  print(list.p.trueeff[[SC]]); cat("\n");
  
  cat("parameter for utility calculation \n");
  print(u);

  cat("\n\n")
  cat("#####################################\n")
  cat("# Results \n")
  cat("#####################################\n")
  
  cat("# mean number of patients by dose level \n")
  print(m.NPTS); cat("\n")
  cat("# percentage of number of patients treated by dose level \n")
  print(m.PPTS); cat("\n")
  cat("# mean number of tox by dose level \n")
  print(m.NTOX); cat("\n")
  cat("# mean number of eff by dose level \n")
  print(m.NEFF); cat("\n")
  
  cat("# observed P(tox) by dose level \n")
  print(m.pTOX); cat("\n")
  cat("# observed P(eff) by dose level \n")
  print(m.pEFF); cat("\n")
  
  cat("# percentage to select as ODC by dose level \n")
  print(pODC); cat("\n")
  
  cat("# percentage to not select ODC \n")
  print(round(non.ODC/ntrial, digits=1)); cat("\n")
  
  cat("# percentage to select target dose correctly \n")
  print(correct.pODC); cat("\n")
  
  cat("# percentage of number of patients assigned to target doses \n")
  print(target.PPTS); cat("\n")
  
  cat("# mean number of patients \n")
  print(summary(SUM.N))

  sink()

  ### data frame of summary
  out.sum1 <- data.frame(
    method      = rep(method, nd1),
    totaln      = rep(ncohort*cohortsize, nd1), 
    cohortsize  = rep(cohortsize, nd1), 
    delta       = rep(delta, nd1),
    n.earlystop = rep(n.es, nd1), 
    scenario    = rep(SC, nd1),
    doseA       = nd1:1
  )
  for (i in 1:nd2){
    out1 <- data.frame(rev(pODC[,i]));   names(out1) <- paste("pODC", i, sep="");
    out2 <- data.frame(rev(m.PPTS[,i])); names(out2) <- paste("m.PPTS", i, sep="");
    out3 <- data.frame(rev(m.NPTS[,i])); names(out3) <- paste("m.NPTS", i, sep="");
    out.sum1 <- cbind(out.sum1, out1, out2, out3)
  }
  
  out.sum2 <- data.frame(
    method      = method,
    totaln      = ncohort*cohortsize, 
    cohortsize  = cohortsize, 
    delta       = delta,
    n.earlystop = n.es, 
    scenario    = SC,
    correct.pODC = correct.pODC,
    mean.npat   = round(mean(SUM.N), digits=1),
    sd.npat     = round(sd(SUM.N), digits=2),
    min.npat    = min(SUM.N),
    med.npat    = round(median(SUM.N), digits=1),
    max.npat    = max(SUM.N),
    target.PPTS = target.PPTS)
  
  list(out.sum1 = out.sum1,
       out.sum2 = out.sum2)

}


##################################################################
# End of program
##################################################################
