####################################################################
# SIM_Main.R: Code to conduct simulation and summarize simulation results
####################################################################
#rm(list=ls())
library("Iso")
library("UniIsoRegression")
library("mfp")
library("openxlsx")
library("readr")

####################################################################
# Sources
####################################################################
source("../FUNC_BOINETC.subtrial.R")
source("../get.oc.BOINETC_m1_v02.R")
source("../get.oc.BOINETC_m2_v02.R")
source("../get.oc.BOINETC_m3_v02.R")
source("../FUNC_CALC.SUM.R")
source("../BOINETC.scenario.R")

####################################################################
# true tox/eff probabilities
####################################################################
list.p.truetox <- list(pt.true.mat1,  pt.true.mat2,  pt.true.mat3, 
                       pt.true.mat4,  pt.true.mat5,  pt.true.mat6,
                       pt.true.mat7,  pt.true.mat8,  pt.true.mat9,
                       pt.true.mat10)
list.p.trueeff <- list(pe.true.mat1,  pe.true.mat2,  pe.true.mat3, 
                       pe.true.mat4,  pe.true.mat5,  pe.true.mat6,  
                       pe.true.mat7,  pe.true.mat8,  pe.true.mat9,
                       pe.true.mat10)
list.tdose     <- list(c(4, 5), c(2, 3), 
		       c(8, 11, 14), c(7, 10), c(4, 7), c(7, 10), 
		       c(9, 11, 13), c(6, 8, 10), c(9, 11), c(8, 10))

####################################################################
# Settings
####################################################################
##### Common
phi        <- 0.30; # target toxic probability
delta      <- 0.60; # target efficacy probability
cohortsize <- 3;    # fixed cohort size
ntrial     <- 1000; # simulation times
lambda1    <- 0.14; # parameter
lambda2    <- 0.35; # parameter 
eta1       <- 0.48; # parameter

u <- c(100, 25, 75, 0); # utility score
alpha <- 1; beta <- 1;  # prior beta parameters for utility

c.method <- c("BOINETC_m1", "BOINETC_m2", "BOINETC_m3");
out.util <- paste(u[1], u[2], u[3], u[4], sep="-"); 

##### Scenario specific
# scenario 1-2 (2x3)
# out.dl <- "2x3"; c.sc <- 1:2; c.ncohort <- c(8, 12); c.n.earlystop <- c(6, 9);
# scenario 3-6 (4x4)
 out.dl <- "4x4"; c.sc <- 3:6; c.ncohort <- c(16, 24); c.n.earlystop <- c(6, 9); 
# scenario 7-10 (3x5)
# out.dl <- "3x5"; c.sc <- 7:10; c.ncohort <- c(16, 24); c.n.earlystop <- c(6, 9); 

####################################################################
# Simulation
####################################################################
set.seed(1234)
source("../FUNC_BOINETC.study.R", sep=""))

####################################################################
# Summarization
####################################################################
for (kk in 1:length(c.sc)){
  for (jj in 1:length(c.method)){
    for (ii in 1:length(c.n.earlystop)){
      sc <- c.sc[kk]; method <- c.method[jj]; 
      n.es <- c.n.earlystop[ii]; ncohort <- c.ncohort[ii];
      filename <- paste(method, ".new.sc", sc, "_cs3_ns", n.es, sep="")
      cat(paste("kk = ", kk, ", ii = ", ii, ", jj = ", jj, "\n", sep=""));
      print(filename);
      aaa <- CALC.SUM(SC=sc, FILENAME=filename);
      if (ii==1){
        out.sum1 <- aaa$out.sum1
        out.sum2 <- aaa$out.sum2
      }
      else{
        out.sum1 <- rbind(out.sum1, aaa$out.sum1)
        out.sum2 <- rbind(out.sum2, aaa$out.sum2)
      }
    }
    if (jj==1){
      out.sum1.sc <- out.sum1
      out.sum2.sc <- out.sum2
    }
    else{
      out.sum1.sc <- rbind(out.sum1.sc, out.sum1)
      out.sum2.sc <- rbind(out.sum2.sc, out.sum2)
    }
  }
  if (kk==1){
    out.sum1.all <- out.sum1.sc
    out.sum2.all <- out.sum2.sc
  }
  else{
    out.sum1.all <- rbind(out.sum1.all, out.sum1.sc)
    out.sum2.all <- rbind(out.sum2.all, out.sum2.sc)
  }
  
  ### Excel file ###
  ## Create Workbook object and add worksheets
  wb <- createWorkbook()
  addWorksheet(wb, "Sum1")
  addWorksheet(wb, "Sum2")

  writeDataTable(wb, "Sum1", x = out.sum1.all)
  writeDataTable(wb, "Sum2", x = out.sum2.all)
  saveWorkbook(wb, paste("../../output/BOINTEC.new.summary_", out.dl, "_", out.util, ".xlsx",　sep=""), 
               overwrite = TRUE)
}


####################################################################
# End of program
####################################################################
