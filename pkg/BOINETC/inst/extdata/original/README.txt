################################
###   Software information   ###
################################

All the simulation studies were implemented using R software. The output of sessionInfo() is as follows:

R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default

locale:
[1] LC_COLLATE=Japanese_Japan.932  
[2] LC_CTYPE=Japanese_Japan.932    
[3] LC_MONETARY=Japanese_Japan.932
[4] LC_NUMERIC=C                   
[5] LC_TIME=Japanese_Japan.932    

time zone: Asia/Tokyo
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

loaded via a namespace (and not attached):
[1] compiler_4.3.1   tools_4.3.1      Rcpp_1.0.12      stringi_1.8.3    zip_2.3.1        openxlsx_4.2.5.2
 

####################################################
###   Information of each code                   ###
####################################################

/prog/SIM_Main.R: main code

/prog/BOINETC.scenario.R: define efficacy/toxicity probabilities at each dose level

/prog/FUNC_BOINETC.subtrial.R: function to find next data in subtrial
/prog/FUNC_BOINETC.study.R:    function to conduct simulation of BOIN-ETC1, 2, 3 
/prog/FUNC_CALC.SUM.R:         function to find ODCs and provide summary tables
/prog/get.oc.BOINETC_m1_v02.R: function to caltulate operation characteristics of BOIN-ETC1
/prog/get.oc.BOINETC_m2_v02.R: function to caltulate operation characteristics of BOIN-ETC2
/prog/get.oc.BOINETC_m3_v02.R: function to caltulate operation characteristics of BOIN-ETC3

