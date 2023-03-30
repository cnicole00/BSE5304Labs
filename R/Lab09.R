Sys.getenv('cnicole00')
LabNo="/Lab09"
#
# What needs to be loaded
#
if (!require("pacman")) install.packages("pacman")
myhomedir=Sys.getenv("HOME")
datadir=paste0(myhomedir,"/data",LabNo)
dir.create(datadir,recursive = T)
srcdir=paste0(myhomedir,"/src")
dir.create(srcdir,recursive = T)
# Setting the directory for where the GitHub project exists. 
# This depends on where you set up your git, and what you called it locally, 
# but when you start a new git project, it will be the first directory you 
# are placed in... or if later in the project:
# 
mygitdir=rstudioapi::getActiveProject()
mypdfdir=paste0(mygitdir,"/pdfs",LabNo)
dir.create(mypdfdir)
# 
setwd(mygitdir)
system("git config --global user.email 'cnicole00@vt.edu' ") 
system("git config --global user.name 'cnicole00' ")
system("git config pull.rebase false")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr,EcoHydRology,curl,elevatr,raster,rgdal,
                 data.table,foreign,maptools,dataRetrieval,gdistance)
setwd(datadir)
#
# Note we have a new library to access USGS Waterdata
# https://owi.usgs.gov/R/dataRetrieval.html
# https://owi.usgs.gov/R/training-curriculum/usgs-packages/dataRetrieval-readNWIS/
#
?dataRetrieval  # Review the man page for this package
?readNWISuv
?readNWISdv
?readNWISdata
#
url="https://nwis.waterdata.usgs.gov/nwis/pmcodes?radio_pm_search=param_group&pm_group=All+--+include+all+parameter+groups&pm_search=&casrn_search=&srsname_search=&format=html_table&show=parameter_group_nm&show=parameter_nm&show=casrn&show=srsname&show=parameter_units"
browseURL(url)
#
# Yeah, these databases are complex to get to know, remember our 
# SSURGO?
#
# Before you begin your modeling project, confirm what your model outputs 
# has a value to calibrate against, i.e. match parameter and units. For 
# this lab we are looking for Gage Height, while historically, we have been 
# looking at Discharge. NOT ALL PARAMETERS ARE AVAILABLE!
#
url="https://help.waterdata.usgs.gov/parameter_cd?group_cd=%"
browseURL(url)
View(parameterCdFile)
##############################################
# 0205551460 LICK RUN ABOVE PATTON AVENUE AT ROANOKE, VA
##############################################


make_usgs_gage_list=function(
siteNo = "0205551460",
parameterCd = c("00060","00065"),
start.date = "2017-05-01",  # Not frozen to not frozen
end.date = "2017-11-01")    # to still not frozen
{
  
  USGS=list()   # Organize the data in a nice list as in previous labs
  USGS[["flowdata"]]<- readNWISuv(siteNumbers = siteNo,parameterCd = parameterCd,startDate = start.date,endDate = end.date)
  # View(USGS$flowdata)  # Note that we have 00060 and 00065...
  #  agency_cd	site_no        	dateTime X_00060_00000 X_00060_00000_cd
  #1  	USGS 0205551460 2017-05-01 04:00:00      	6.38            	A
  #2  	USGS 0205551460 2017-05-01 04:05:00      	6.38            	A
  #  X_00065_00000 X_00065_00000_cd tz_cd
  #1      	2.74            	A   UTC
  #2      	2.74            	A   UTC
  #
  # And of course we want to work in SI units so:
  USGS$flowdata$depth_m=USGS$flowdata$X_00065_00000*0.3048
  # m/ft depth
  USGS$flowdata$cms=USGS$flowdata$X_00060_00000*.02832
  # m3/ft3 flow
  #
  # Let's add in the USGS gage site information to the list and inspect
  USGS[["site"]]=readNWISsite(siteNo)
  # View(USGS$site)
  class(USGS$site$dec_lat_va)
  #
  # Set the Manning Coefficient in the USGS Gage's Site Table
  #
  # url="https://www.google.com/search?q=manning%27s+n+for+stream"
  # browseURL(url)
  # url="https://www.fsl.orst.edu/geowater/FX3/help/8_Hydraulic_Reference/Mannings_n_Tables.htm"
  # browseURL(url)
  USGS$site$man_n=.035/1.49
  #
  # Create a SpatialPointsDataFrame out of the site dataframe in the USGS list
  coordinates(USGS$site)=~dec_long_va+dec_lat_va
  return(USGS)
}

pacman::p_load(useful)
compare.list(testlist,USGS0205551460)

USGS02056000=make_usgs_gage_list(siteNo = "02056000")
USGS0205551460=make_usgs_gage_list(siteNo ="0205551460" )
USGS02055100=make_usgs_gage_list(siteNo ="02055100" )
USGS02055000=make_usgs_gage_list(siteNo ="02055000" )
USGS02054530=make_usgs_gage_list(siteNo ="02054530" )

ab_ll=rbind(USGS02056000$site,
              USGS0205551460$site,
              USGS02055100$site,
              USGS02055000$site,
              USGS02054530$site)
class(ab_ll)
ab_ll@proj4string
proj4_utm = paste0("+proj=utm +zone=",
                     trunc((180+coordinates(USGS02055000$site)[1])/6+1), 
                     " +datum=WGS84 +units=m +no_defs")
print(proj4_utm)
# Lat/Lon (_ll) is much easier!
proj4_ll = "+proj=longlat"
crs_ll=CRS(proj4_ll)
crs_utm=CRS(proj4_utm)
proj4string(ab_ll)=proj4_ll
ab_utm=spTransform(ab_ll,crs_utm)
ab_utm@coords
mydem=get_aws_terrain(locations=ab_utm@coords, 
                        z = 12, prj = proj4_utm,expand=1)
#
# Lets plot the DEM and the gage locations so we can guess 
# what gages connect with what gages
#
plot(mydem)
plot(ab_utm,add=T)
text(ab_utm, labels=ab_utm@data$site_no, cex=0.6, font=2,pos=1)
# Streams as USGS sees them, I know I can get an overview of streams with the 
# USGS H
url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/Shape/NHD_H_03010101_HU8_Shape.zip"
curl_download(url,"NHD_H_03010101_HU8_Shape.zip")
unzip("NHD_H_03010101_HU8_Shape.zip",exdir="03010101")
streams=readOGR("03010101/Shape/NHDFlowline.dbf")
streams_utm=spTransform(streams,crs_utm)
plot(streams_utm,col="blue",add=T)

#
# Breaking things for educational purposes
View(USGS02056000$flowdata)
USGS02056000$flowdata=USGS02056000$flowdata[,c(1,2,3,4,5,8,10)]
View(USGS02056000$flowdata)
# Oh Noooooo!!!! This gage for some reason doesn't have "Gage height"
# 00065! What can we do!?!? OK, no worries, we do have "Discharge" 00060	
###########################################
# 02056000 ROANOKE RIVER AT NIAGARA, VA
###########################################

# and use the readNWISrating() function to grab it for this gage
USGS02056000[["rating"]]=readNWISrating(USGS02056000$site$site_no)
plot(USGS02056000$rating$DEP,USGS02056000$rating$INDEP,xlab="DEP",ylab="INDEP")
#

USGS02056000$flowdata$X_00065_00000=approx(USGS02056000$rating$DEP,
                                             USGS02056000$rating$INDEP, xout = USGS02056000$flowdata$X_00060_00000, ties = min)$y
points(USGS02056000$flowdata$X_00060_00000,USGS02056000$flowdata$X_00065_00000,
         col="red")
#
USGS02056000$flowdata$depth_m=USGS02056000$flowdata$X_00065_00000*0.3048
# m/ft depth
#
vignette("Overview", package = "gdistance")
# Set the starting and ending locations
# determine the river reach length and slope using the gdistance package.
#
A=SpatialPoints(USGS0205551460$site)# Up gradient site Lick Run
B=SpatialPoints(USGS02056000$site) # Down gradient site ROA River atNiagara
proj4string(A)=proj4_ll
proj4string(B)=proj4_ll
A_utm=spTransform(A,crs_utm)
B_utm=spTransform(B,crs_utm)
# Cut the DEM down to a more manageable size
cropmydem=crop(mydem,extend(extent(ab_utm),600))
cropmydem=trim(cropmydem)
cropmydem=cropmydem*1000.0
plot(cropmydem)
plot(ab_utm,add=T)
# Set up the weighting functions
altDiff <- function(x){x[2] - x[1]}
hd <- transition(cropmydem, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(cropmydem, cells=1:ncell(cropmydem), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
# Find and plot the flow path
AtoB <- shortestPath(Conductance, A_utm, B_utm, output="SpatialLines")
plot(AtoB,add=T)
plot(streams_utm,col="blue",add=T)
plot(AtoB,add=T)
SpatialLinesLengths(AtoB)
USGS0205551460$site$L=SpatialLinesLengths(AtoB) # km to m
USGS0205551460$site$L # reach length in m

USGS0205551460$site$slope=(extract(mydem,A_utm)-
                               extract(mydem,B_utm))/USGS0205551460$site$L
USGS0205551460$site$slope

# ck
USGS0205551460$flowdata$ck = 5/3*((sqrt(USGS0205551460$site$slope))
                                 /USGS0205551460$site$man_n)*USGS0205551460$flowdata$depth_m^(2/3)
  # ANS
  mean(USGS0205551460$flowdata$ck,na.rm=T)
# [1] 2.547238 for this example, confirm this result
USGS0205551460$flowdata$dt = USGS0205551460$site$L/USGS0205551460$flowdata$ck
  mean(USGS0205551460$flowdata$dt,na.rm=T)
# [1] 6328.655  for this example, confirm this result

plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$dt)
USGS0205551460$flowdata$outTime=USGS0205551460$flowdata$dateTime+
USGS0205551460$flowdata$dt
  
  # Find the beginning of  Waves assuming a new wave starts at 110% of prior 
  # flow. This might need to change for your homework
WaveStartDecPercent=1.10
USGS0205551460$flowdata$newwave=
USGS0205551460$flowdata$cms *WaveStartDecPercent <
data.table::shift(USGS0205551460$flowdata$cms)
summary(USGS0205551460$flowdata$newwave)
  # Add plot of the point found
len=length(USGS0205551460$flowdata$newwave)
USGS0205551460$flowdata$newwave[is.na(USGS0205551460$flowdata$newwave)]=F
  # Removes repeated finds by going through loop backwords
for (i in seq(len,2)){
  print(i)
  if(USGS0205551460$flowdata$newwave[i]==T &
       USGS0205551460$flowdata$newwave[i-1]==T){
      USGS0205551460$flowdata$newwave[i]=F
    }
  }
plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$cms,type="l")
points(USGS0205551460$flowdata$dateTime[USGS0205551460$flowdata$newwave],
           USGS0205551460$flowdata$cms[USGS0205551460$flowdata$newwave],col=2)
  
  # Find the time locations where waves begin
which(USGS0205551460$flowdata$newwave == TRUE)
plot(USGS0205551460$flowdata$dateTime,USGS0205551460$flowdata$cms,
         type="l",xlim=c(USGS0205551460$flowdata$dateTime[1109],
                         USGS0205551460$flowdata$dateTime[1109+200]))
lines(USGS0205551460$flowdata$outTime,USGS0205551460$flowdata$cms,col=2)
  