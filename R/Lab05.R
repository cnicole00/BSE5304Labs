
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

## run DEoptim and set a seed first for replicability
set.seed(1234)
outDEoptim=DEoptim(Rosenbrock, lower, upper)

## increase the population size
DEoptim(Rosenbrock, lower, upper, DEoptim.control(NP = 100))
?DEoptim.control
## change other settings and store the output
outDEoptim <- DEoptim(Rosenbrock, lower, upper, DEoptim.control(NP = 80,
      itermax = 400, F = 1.2, CR = 0.7))

## plot the output
plot(outDEoptim)

## 'Wild' function, global minimum at about -15.81515
Wild <- function(x)
  10 * sin(0.3 * x) * sin(1.3 * x^2) +
  0.00001 * x^4 + 0.2 * x + 80

plot(Wild, -50, 50, n = 1000, main = "'Wild function'")

outDEoptim <- DEoptim(Wild, lower = -50, upper = 50,
                      control = DEoptim.control(trace = FALSE))

plot(outDEoptim)

DEoptim(Wild, lower = -50, upper = 50,
        control = DEoptim.control(NP = 50))

## The below examples shows how the call to DEoptim can be
## parallelized.
## Note that if your objective function requires packages to be
## loaded or has arguments supplied via \code{...}, these should be
## specified using the \code{packages} and \code{parVar} arguments
## in control.  
## Not run:  

Genrose <- function(x) {
  ## One generalization of the Rosenbrock banana valley function (n parameters)
  n <- length(x)
  ## make it take some time ... 
  Sys.sleep(.001) 
  1.0 + sum (100 * (x[-n]^2 - x[-1])^2 + (x[-1] - 1)^2)
}

# get some run-time on simple problems
maxIt <- 250                     
n <- 5

oneCore <- system.time( DEoptim(fn=Genrose, lower=rep(-25, n), upper=rep(25, n),
                                control=list(NP=10*n, itermax=maxIt)))

withParallel <-  system.time( DEoptim(fn=Genrose, lower=rep(-25, n), upper=rep(25, n),
                                      control=list(NP=10*n, itermax=maxIt, parallelType=1)))

## Compare timings 
(oneCore)
(withParallel)

## End(Not run)

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
  outTMWB=TMWBmodel(TMWBdf = TMWB,fcres=x1,Z=x2,SFTmp=x3,bmlt6=x4)
  return(1-NSE(Yobs=outTMWB$Qmm,Ysim=outTMWB$Qpred))
}
TMWBoptFunc(x)
lower <- c(.01,300,1,.1) #lower bounds for each of the x's in order
upper <- c(.95,3000,6,5)
outDEoptim=DEoptim(TMWBoptFunc,lower,upper,
                   DEoptim.control(NP = 80,
                    itermax = 10,F = 1.2, CR = 0.7))
# if this function gives you like a million objects are masked, then detach WBData and TMWBdf until you get the name error and it works

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
