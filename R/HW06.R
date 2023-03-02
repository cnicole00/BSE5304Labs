citation("DEoptim")

install.packages("DEoptim")
library(DEoptim)
?DEoptim

pacman::p_load(DEoptim)
Rosenbrock <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}
# this function takes in 2 different parameters and generates a value
Rosenbrock(c(3.7,5))

## DEoptim searches for minima of the objective function between
## lower and upper bounds on each parameter to be optimized. Therefore
## in the call to DEoptim we specify vectors that comprise the
## lower and upper bounds; these vectors are the same length as the
## parameter vector.
lower <- c(-10,-10)
upper <- -lower

set.seed(1234)
outDEoptim=DEoptim(Rosenbrock, lower, upper)

## increase the population size
DEoptim(Rosenbrock, lower, upper, DEoptim.control(NP = 100))
?DEoptim.control
## change other settings and store the output
outDEoptim <- DEoptim(Rosenbrock, lower, upper, DEoptim.control(NP = 80,
                                                                itermax = 400, F = 1.2, CR = 0.7))
url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/Lab05SetupDRF.R"
# This will grab the solution for last weeks Lab03 Homework
download.file(url,"Lab05SetupDRF.R")
file.edit("Lab05SetupDRF.R")

# Grab out models for Snow and TMWB
# https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TMWBFuncs.R
# becomes: 
url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TMWBFuncs.R"
# This will grab the solution for last weeks Lab03 Homework
download.file(url,"TMWBFuncs.R")
file.edit("TMWBFuncs.R")
# I actually am starting to trust my snow model
url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/TISnow.R"
# This will grab the solution for last weeks Lab03 Homework
source(url)

TMWBoptFunc <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  x5 <- x[5]
  x6 <- x[6]
  outTMWB=TMWBmodel(TMWBdf = TMWB,fcres=x1,Z=x2,SFTmp=x3,bmlt6=x4,bmlt12=x5,Tlag=x6)
  return(1-NSE(Yobs=outTMWB$Qmm,Ysim=outTMWB$Qpred))
}
# Optimize TMWB function
TMWBoptFunc(x)
lower <- c(.01,300,1,.1,.1,.01)
upper <- c(.95,3000,6,5,5,1)
outDEoptim=DEoptim(TMWBoptFunc,lower,upper,
                   DEoptim.control(NP = 80,
                                   itermax = 10,F = 1.2, CR = 0.7))

# Optimize CN function
CNoptFunc <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  x4 <- x[4]
  x5 <- x[5]
  x6 <- x[6]
  x7 <- x[7]
  outCN=CNmodel(CNmodeldf=TMWB,CNavg=x1,IaFrac=x2,fnc_slope=x3,fnc_aspect=x4,func_DAWC=x5,func_z=x6,fnc_fcres=x7)
  return(1-NSE(Yobs=outCN$Qmm,Ysim=outCN$Qpred))
}
CNoptFunc(x)
lower <- c(50,.01,0,.01,.1,100,.01)
upper <- c(100,1,.4,5,5,3000,1)
outDEoptimCN=DEoptim(CNoptFunc,lower,upper,
                   DEoptim.control(NP = 80,
                                   itermax = 10,F = 1.2, CR = 0.7))
# Beginning of SWAT file review
setwd("~/src/")
install.packages(c("ecohydrology/pkg/SWATmodel/"),repos = NULL)
pacman::p_load(SWATmodel)
setwd(datadir)


AllDays=data.frame(date=seq(min(myflowgage$flowdata$mdate), by = "day", 
                            length.out = max(myflowgage$flowdata$mdate)-min(myflowgage$flowdata$mdate)))
WXData=merge(AllDays,WXData,all=T)
WXData$PRECIP=WXData$P
WXData$PRECIP[is.na(WXData$PRECIP)]=-99
WXData$TMX=WXData$MaxTemp
WXData$TMX[is.na(WXData$TMX)]=-99
WXData$TMN=WXData$MinTemp
WXData$TMN[is.na(WXData$TMN)]=-99
WXData$DATE=WXData$date
#making swat init in the directory with the same name as usgs gagename
build_swat_basic(dirname= myflowgage$gagename, iyr=min(year(WXData$DATE),na.rm=T),
                 nbyr=(max(year(WXData$DATE),na.rm=T)-min(year(WXData$DATE),na.rm=T) +1),
                 wsarea=myflowgage$area, elev=myflowgage$elev, declat=myflowgage$declat,
                 declon=myflowgage$declon, hist_wx=WXData)
# 
# Wait for Dan!
#

build_wgn_file() #wgn func
runSWAT2012() #run swat 
