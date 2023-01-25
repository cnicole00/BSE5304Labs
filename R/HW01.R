system("gitconfig --global user.email 'cnicole00@vt.edu' ")
system("gitconfig --global user.name 'Nicole Chapman' ")

install.packages("pacman") #install packages for HW01
pacman::p_load(rgdal,parallel, ggplot2, dplyr, patchwork, hrbrthemes, rnoaa)

print("Hello World")

# find USGS gage near my hometown
source("https://goo.gl/Cb8zGn")
# get data for USGS 01648000 ROCK CREEK AT SHERRIL DRIVE WASHINGTON, DC

myflowgage_id="01648000"
myflowgage=get_usgs_gage(myflowgage_id,
                         begin_date="2017-02-01", end_date="2023-02-01")

class(myflowgage)
str(myflowgage)
class(myflowgage$declat)
class(myflowgage$gagename)
class(myflowgage$flowdata)
View(myflowgage$flowdata)

plot(myflowgage$flowdata$mdate, myflowgage$flowdata$flow,
     main=myflowgage$gagename, xlab="Date",
     ylab="Flow m^3/day", type="l")

# get weather data
station_data <- ghcnd_stations()
meteo_distance(station_data, 38.96017, -77.04206, radius=10, limit=10)
meteo_distance(station_data, 38.96017, -77.04206, limit=3)

# find stations close to my hometown
stns=meteo_distance(
  station_data=ghcnd_stations(),
  lat=myflowgage$declat,
  long=myflowgage$declon,
  units="deg",
  radius=20,
  limit=NULL
)
# examine stations to find one that has PRCP, TMIN, TMAX, current years, and close proximity
WXData=meteo_pull_monitors(
  monitors="USW00013743",
  keep_flags=FALSE,
  date_min="2016-01-01",
  date_max=NULL,
  var=c("TMAX","TMIN","PRCP")
)

str(WXData)

#check that data looks right
mean(WXData$prcp, na.rm=T)*365

# plot weather data
coeff <- 10 # value used to transform data

tminColor <- "#69b3a2"
  tmaxColor <- rgb(0.2, 0.6, 0.9, 1)
  
  ggplot(WXData, aes(x=date)) +
    geom_line(aes(y=tmin/coeff), size=0.3, color=tminColor) +
    geom_line(aes(y=tmax/coeff), size=0.3, color=tmaxColor) +
    geom_point(aes(y=prcp/coeff), size=0.3, color="maroon") +
    
    scale_y_continuous(
      name="Temperature (C)",
      sec.axis=sec_axis(trans=~.*coeff, name="Precipitation (mm)")) +
    theme_ipsum() +
    theme(
      axis.title.y = element_text(color = tminColor, size=13),
      axis.title.y.right = element_text(color = tmaxColor, size=13)) +
    ggtitle("Weather Data at Rock Creek at Sherril Dr Washington, DC")
  
  