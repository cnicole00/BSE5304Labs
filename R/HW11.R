setwd("~")
objects()
rm(list=objects())
url="https://raw.githubusercontent.com/vtdrfuka/BSE5304Labs/main/R/MySetup.R"
download.file(url,"DanSetup.R")
file.edit("DanSetup.R")
#

#
url="https://cran.r-project.org/web/packages/multisensi/vignettes/multisensi-vignette.pdf"
browseURL(url)
pacman::p_load(data.table,multisensi)
# KEEP OPEN AS YOU WILL BE WALKING THROUGH IT FOR LAB	
vignette("multisensi-vignette")

verhulst <- function(K, Y0, a, t) {
  output <- K/(1 + (K/Y0 - 1) * exp(-a * t))
  return(output)
}

T <- seq(from = 5, to = 100, by = 5)
verhulst2 <- function(X, t = T) {
  out <- matrix(nrow = nrow(X), ncol = length(t), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- verhulst(X$K[i], X$Y0[i], X$a[i], t)
  }
  out <- as.data.frame(out)
  names(out) <- paste("t", t, sep = "")
  return(out)
}
n <- 10
set.seed(1234)
X <- data.frame(K = runif(n, min = 100, max = 1000), Y0 = runif(n, min = 1,
                                                                max = 40), a = runif(n, min = 0.05, max = 0.2))
Y <- verhulst2(X)
par(cex.axis = 0.7, cex.lab = 0.8)
plot(T, Y[1, ], type = "l", xlab = "Time", ylab = "Population size",
     ylim = c(0, 1000))
for (i in 2:n) {
  lines(T, Y[i, ], type = "l", col = i)
}

verhulst.seq <- multisensi(model=verhulst2, reduction=NULL, center=FALSE,
                           design.args = list( K=c(100,400,1000), Y0=c(1,20,40), a=c(0.05,0.1,0.2)))

print(verhulst.seq, digits = 2)

plot(verhulst.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Time in half-decades.")
plot(verhulst.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Time in half-decades.")

X <- expand.grid(K=c(100,400,1000), Y0=c(1,20,40), a=c(0.05,0.1,0.2))
Y <- verhulst2(X) ## this part can be performed outside R if necessary
verhulst.seq <- multisensi(design=X, model=Y, reduction=NULL, center=FALSE)


verhulst.pca <- multisensi(design=X, model=Y, reduction=basis.ACP, scale=FALSE)
summary(verhulst.pca, digits = 2)

verhulst.poly <- multisensi(design = X, model = Y, reduction = basis.poly,
                            dimension = 0.99, center = FALSE, scale = FALSE, cumul = FALSE,
                            basis.args = list(degree=6, x.coord=T), analysis = analysis.anoasg,
                            analysis.args = list(formula=2, keep.outputs=FALSE))
## [*] Dimension Reduction
## [*] Analysis + Sensitivity Indices
## [*] Goodness of fit computing
summary(verhulst.poly, digits=2)


plot(verhulst.poly, nb.comp = 3, graph = 1)

verhulst.bspl <- multisensi(design=X, model=Y, reduction=basis.bsplines,
                            dimension=NULL, center=FALSE, scale=FALSE,
                            basis.args=list(knots=10, mdegree=3), cumul=FALSE,
                            analysis=analysis.anoasg,
                            analysis.args=list(formula=2, keep.outputs=FALSE))
## [*] Dimension Reduction
## [*] Analysis + Sensitivity Indices
## [*] Goodness of fit computing
plot(verhulst.bspl, nb.comp = 5, graph = 1)

library(sensitivity)

m <- 10000
Xb <- data.frame(K = runif(m, min = 100, max = 1000), Y0 = runif(m, min = 1,
                                                                 max = 40), a = runif(m, min = 0.05, max = 0.2))
verhulst.seq.sobol <- multisensi(design = sobol2007, model = verhulst2,
                                 reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                 design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                 analysis.args = list(keep.outputs = FALSE))
## [*] Design
## [*] Response simulation
## [*] Analysis + Sensitivity Indices
print(verhulst.seq.sobol, digits = 2)

?EcoHydRology::Solar

#
J <- seq(from = 1, to = 365, by = 5)
# Solar(lat, Jday, Tx, Tn, albedo=0.2, forest=0, slope=0, aspect = 0,
#      units="kJm2d")
# Note that the EcoHydRology::Solar() function is for specific days, 
# as such, we will want to create a function to loop through our period
# of interest:
Solar_Looped <- function(X, Jday = J) {
  out <- matrix(nrow = nrow(X), ncol = length(Jday), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- Solar(lat=X$lat[i],
                      Jday=Jday, Tx=X$Tx[i], 
                      Tn=(X$Tx[i]-X$Trange[i]), 
                      X$slope[i],X$aspect[i],units="Wm2")
  }
  out <- as.data.frame(out)
  names(out) <- paste("Jday", Jday, sep = "")
  return(out)
}
n <- 10
set.seed(1234)
X <- data.frame(Tx = runif(n, min = 5, max = 30), Trange = runif(n, min = 2,
                                                                 max = 16), slope = runif(n, min = 0.0, max = 0.2),
                aspect = runif(n, min = 0.0, max = 0.2),
                lat=runif(n, min = 0.0, max = 1.1))  # 1.1 radians lat is where?
Y <- Solar_Looped(X,Jday = J)

par(cex.axis = 0.7, cex.lab = 0.8)
plot(J, Y[1, ], type = "l", xlab = "Day of Year", 
     ylab = "Surface Short Wave Rad(W/m^2)")
for (i in 2:n) {
  lines(J, Y[i, ], type = "l", col = i)
}

Solar_Looped.seq <- multisensi(model=Solar_Looped, reduction=NULL, center=FALSE,
                               design.args = list( Tx = c(5,15,25), 
                                                   Trange = c(2,9,16), 
                                                   slope = c(0.1,0.2,0.3),
                                                   aspect = c(0.1,.5,1.0),
                                                   lat=c(0.1,.77,1.1)))

print(Solar_Looped.seq, digits = 2)
#
# 3.2 Graphical representation of sensitivity indices
#
dev.off() # Clean up previous par()
plot(Solar_Looped.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)#normalized the upper subplot shows the extreme (tirets), #inter-quartile (grey) and median (bold line) output values
title(xlab = "Days of the Year.")
plot(Solar_Looped.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Days of the Year.")


#
# Take note of section 3.3 Calculating simulations apart, as there are 
# several ways to invoke the multisensi() function.
# First by building our X (design input) and Y (model run on X). Here 
# here is us doing the same sensitivities by passing previously run 
# models (Y)
X <- expand.grid(Tx = c(5,15,25), 
                 Trange = c(2,9,16), 
                 slope = c(0.1,0.2,0.3),
                 aspect = c(0.1,.5,1.0),
                 lat=c(0.1,.77,1.1))
# Look at our input
head(X,10)
Y <- Solar_Looped(X,Jday=J) # can be performed outside R if necessary
# Look at our model output
head(Y,10)
# Notice on the next line that “model=Solar_Looped” is replaced with our 
# model output “Y” and we add in the input into “design=X”. This 
# is exactly the same as above, though with the model run 
# external to the multisensi() function:
Solar_Looped.seq <- multisensi(design=X, model=Y, reduction=NULL, center=FALSE) 

# 4 Multivariate sensitivity analysis based on PCA
# read the vignette, though note we are using the multisensi() function
# to run our model (i.e. no “design” variable, and model=Solar_Looped)
Solar_Looped.pca <- multisensi(model=Solar_Looped, reduction=basis.ACP, scale=FALSE,
                               design.args = list( Tx = c(5,15,25), 
                                                   Trange = c(2,9,16), 
                                                   slope = c(0.1,0.2,0.3),
                                                   aspect = c(0.1,.5,1.0),
                                                   lat=c(0.1,.77,1.1)))

summary(Solar_Looped.pca, digits = 2)
# 4.2 Graphical representation for PCA based analysis with 
# explanation in vignette. These graphs require the plot window to be larger
# and might give "Error in plot.new() : figure margins too large". 
# If so expand the plot window.
dev.off()
plot(Solar_Looped.pca, graph = 1)
plot(Solar_Looped.pca, graph = 2)
plot(Solar_Looped.pca, graph = 3)
#
# 5.1 Polynomial reduction of the multivariate output
# Skip 5.1 Polynomial reduction for now and move on to
# 6 Alternative methods of sensitivity analysis
# 6.1 With Sobol2007 implemented in the package sensitivity
# 
library(sensitivity)
m <- 10000
Xb <- data.frame(Tx = runif(m, min = 5, max = 30), 
                 Trange = runif(m, min = 2,max = 16), 
                 slope = runif(m, min = 0.0, max = 0.2),
                 aspect = runif(m, min = 0.0, max = 0.2),
                 lat=runif(m, min = 0.0, max = 1.1))

Solar_Looped.seq.sobol <- multisensi(design = sobol2007, model = Solar_Looped,
                                     reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                     design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                     analysis.args = list(keep.outputs = FALSE))
#
# Note, this is a good time time get a drink of water and/or pee as 
# it is running the function m=10,000 times (a few minutes).
#
print(Solar_Looped.seq.sobol, digits = 2)
dev.off()
plot(Solar_Looped.seq.sobol, normalized = TRUE, color = terrain.colors)

# PET_fromTemp
?EcoHydRology::PET_fromTemp
#
J <- seq(from = 1, to = 365, by = 5)
# Solution for PET_fromTemp
# Trick is you have to notice that "lat_radians" has replaced "lat" and
# there is no "units" variable... and... notice that the function has to
# be fixed to allow Jday to be a vector of a different size than Tmax and Tmin
PET_fromTemp <- function (Jday, Tmax_C, Tmin_C, lat_radians, AvgT = (Tmax_C + Tmin_C)/2, albedo = 0.18, TerrestEmiss = 0.97, aspect = 0, slope = 0, forest = 0, PTconstant=1.26, AEparams=list(vp=NULL, opt="linear"))
{
  cloudiness <- EstCloudiness(Tmax_C, Tmin_C)
  DailyRad <- NetRad(lat_radians, Jday, Tmax_C, Tmin_C, albedo, forest, slope, aspect, AvgT, cloudiness, TerrestEmiss, AvgT, AEparams=AEparams)
  potentialET <- PTpet(DailyRad, AvgT, PTconstant)
  potentialET[which(potentialET < 0)] <- 0
  potentialET[which(Tmax_C == -999 | Tmin_C == -999)] <- (-999)
  return(potentialET)
}


PET_Looped <- function(X, Jday = J) {
    out <- matrix(nrow = nrow(X), ncol = length(Jday), NA)
    for (i in 1:nrow(X)) {
      out[i, ] <- PET_fromTemp(lat_radians=X$lat_radians[i],
                        Jday=Jday,
                        Tmax_C=X$Tx[i], 
                        Tmin_C=(X$Tx[i]-X$Trange[i])
      )
    }
    out <- as.data.frame(out)
    names(out) <- paste("Jday", Jday, sep = "")
    return(out)
  }
n <- 10
set.seed(1234)
X <- data.frame(Tx = runif(n, min = 5, max = 30), Tn = runif(n, min = 2, max = 16),slope = runif(n, min = 0.0, max = 0.2),
                aspect = runif(n, min = 0.0, max = 0.2),
                lat_radians=runif(n, min = 0.1, max = 1.1))
Y <- PET_Looped(X,Jday=J)

par(cex.axis = 0.7, cex.lab = 0.8)
plot(J, Y[1, ], type = "l", xlab = "Day of Year", 
     ylab = "Potential ET (m)")
for (i in 2:n) {
  lines(J, Y[i, ], type = "l", col = i)
}

PET_Looped.seq <- multisensi(model=PET_Looped, reduction=NULL, center=FALSE,
                               design.args = list( Tx = c(5,15,25), 
                                                   Trange = c(2,9,16), 
                                                   slope = c(0.1,0.2,0.3),
                                                   aspect = c(0.1,.5,1.0),
                                                   lat=c(0.1,.77,1.1)))

print(PET_Looped.seq, digits = 2)
#
# 3.2 Graphical representation of sensitivity indices
#
dev.off() # Clean up previous par()
plot(PET_Looped.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)#normalized the upper subplot shows the extreme (tirets), #inter-quartile (grey) and median (bold line) output values
title(xlab = "Days of the Year.")
plot(PET_Looped.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Days of the Year.")


#
# Take note of section 3.3 Calculating simulations apart, as there are 
# several ways to invoke the multisensi() function.
# First by building our X (design input) and Y (model run on X). Here 
# here is us doing the same sensitivities by passing previously run 
# models (Y)
X <- expand.grid(Tx = c(5,15,25), 
                 Trange = c(2,9,16), 
                 slope = c(0.1,0.2,0.3),
                 aspect = c(0.1,.5,1.0),
                 lat=c(0.1,.77,1.1))
# Look at our input
head(X,10)
Y <- PET_Looped(X,Jday=J) # can be performed outside R if necessary
# Look at our model output
head(Y,10)
# Notice on the next line that “model=Solar_Looped” is replaced with our 
# model output “Y” and we add in the input into “design=X”. This 
# is exactly the same as above, though with the model run 
# external to the multisensi() function:
PET_Looped.seq <- multisensi(design=X, model=Y, reduction=NULL, center=FALSE) 

# 4 Multivariate sensitivity analysis based on PCA
# read the vignette, though note we are using the multisensi() function
# to run our model (i.e. no “design” variable, and model=Solar_Looped)
PET_Looped.pca <- multisensi(model=PET_Looped, reduction=basis.ACP, scale=FALSE,
                               design.args = list( Tx = c(5,15,25), 
                                                   Trange = c(2,9,16), 
                                                   slope = c(0.1,0.2,0.3),
                                                   aspect = c(0.1,.5,1.0),
                                                   lat=c(0.1,.77,1.1)))

summary(PET_Looped.pca, digits = 2)
# 4.2 Graphical representation for PCA based analysis with 
# explanation in vignette. These graphs require the plot window to be larger
# and might give "Error in plot.new() : figure margins too large". 
# If so expand the plot window.
dev.off()
plot(PET_Looped.pca, graph = 1)
plot(PET_Looped.pca, graph = 2)
plot(PET_Looped.pca, graph = 3)
#
# 5.1 Polynomial reduction of the multivariate output
# Skip 5.1 Polynomial reduction for now and move on to
# 6 Alternative methods of sensitivity analysis
# 6.1 With Sobol2007 implemented in the package sensitivity
# 
library(sensitivity)
m <- 10000
Xb <- data.frame(Tx = runif(m, min = 5, max = 30), 
                 Trange = runif(m, min = 2,max = 16), 
                 slope = runif(m, min = 0.0, max = 0.2),
                 aspect = runif(m, min = 0.0, max = 0.2),
                 lat=runif(m, min = 0.0, max = 1.1))

PET_Looped.seq.sobol <- multisensi(design = sobol2007, model = Solar_Looped,
                                     reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                     design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                     analysis.args = list(keep.outputs = FALSE))
#
# Note, this is a good time time get a drink of water and/or pee as 
# it is running the function m=10,000 times (a few minutes).
#
print(PET_Looped.seq.sobol, digits = 2)
dev.off()
plot(PET_Looped.seq.sobol, normalized = TRUE, color = terrain.colors)

# NetRad()
NetRad_Looped <- function(X, Jday = J) {
  out <- matrix(nrow = nrow(X), ncol = length(Jday), NA)
  for (i in 1:nrow(X)) {
    out[i, ] <- NetRad(lat=X$lat[i],
                      Jday=Jday, Tx=X$Tx[i], 
                      Tn=(X$Tx[i]-X$Trange[i]), 
                      X$slope[i],X$aspect[i],units="Wm2")
  }
  out <- as.data.frame(out)
  names(out) <- paste("Jday", Jday, sep = "")
  return(out)
}
n <- 10
set.seed(1234)
X <- data.frame(Tx = runif(n, min = 5, max = 30), Trange = runif(n, min = 2,
                                                                 max = 16), slope = runif(n, min = 0.0, max = 0.2),
                aspect = runif(n, min = 0.0, max = 0.2),
                lat=runif(n, min = 0.0, max = 1.1))
Y <- NetRad_Looped(X,Jday = J)
par(cex.axis = 0.7, cex.lab = 0.8)
plot(J, Y[1, ], type = "l", xlab = "Day of Year", 
     ylab = "Daily Net Radiation(W/m^2)")
for (i in 2:n) {
  lines(J, Y[i, ], type = "l", col = i)
}

NetRad_Looped.seq <- multisensi(model=NetRad_Looped, reduction=NULL, center=FALSE,
                               design.args = list( Tx = c(5,15,25), 
                                                   Trange = c(2,9,16), 
                                                   slope = c(0.1,0.2,0.3),
                                                   aspect = c(0.1,.5,1.0),
                                                   lat=c(0.1,.77,1.1)))

print(NetRad_Looped.seq, digits = 2)
#
# 3.2 Graphical representation of sensitivity indices
#
dev.off() # Clean up previous par()
plot(NetRad_Looped.seq, normalized = TRUE, color = terrain.colors, gsi.plot = FALSE)#normalized the upper subplot shows the extreme (tirets), #inter-quartile (grey) and median (bold line) output values
title(xlab = "Days of the Year.")
plot(NetRad_Looped.seq, normalized = FALSE, color = terrain.colors, gsi.plot = FALSE)
title(xlab = "Days of the Year.")


#
# Take note of section 3.3 Calculating simulations apart, as there are 
# several ways to invoke the multisensi() function.
# First by building our X (design input) and Y (model run on X). Here 
# here is us doing the same sensitivities by passing previously run 
# models (Y)
X <- expand.grid(Tx = c(5,15,25), 
                 Trange = c(2,9,16), 
                 slope = c(0.1,0.2,0.3),
                 aspect = c(0.1,.5,1.0),
                 lat=c(0.1,.77,1.1))
# Look at our input
head(X,10)
Y <- NetRad_Looped(X,Jday=J) # can be performed outside R if necessary
# Look at our model output
head(Y,10)
# Notice on the next line that “model=Solar_Looped” is replaced with our 
# model output “Y” and we add in the input into “design=X”. This 
# is exactly the same as above, though with the model run 
# external to the multisensi() function:
NetRad_Looped.seq <- multisensi(design=X, model=Y, reduction=NULL, center=FALSE) 



# 4 Multivariate sensitivity analysis based on PCA
# read the vignette, though note we are using the multisensi() function
# to run our model (i.e. no “design” variable, and model=Solar_Looped)
NetRad_Looped.pca <- multisensi(model=NetRad_Looped, reduction=basis.ACP, scale=FALSE,
                               design.args = list( Tx = c(5,15,25), 
                                                   Trange = c(2,9,16), 
                                                   slope = c(0.1,0.2,0.3),
                                                   aspect = c(0.1,.5,1.0),
                                                   lat=c(0.1,.77,1.1)))

summary(NetRad_Looped.pca, digits = 2)
# 4.2 Graphical representation for PCA based analysis with 
# explanation in vignette. These graphs require the plot window to be larger
# and might give "Error in plot.new() : figure margins too large". 
# If so expand the plot window.
dev.off()
plot(NetRad_Looped.pca, graph = 1)
plot(NetRad_Looped.pca, graph = 2)
plot(NetRad_Looped.pca, graph = 3)
#
# 5.1 Polynomial reduction of the multivariate output
# Skip 5.1 Polynomial reduction for now and move on to
# 6 Alternative methods of sensitivity analysis
# 6.1 With Sobol2007 implemented in the package sensitivity
# 
library(sensitivity)
m <- 10000
Xb <- data.frame(Tx = runif(m, min = 5, max = 30), 
                 Trange = runif(m, min = 2,max = 16), 
                 slope = runif(m, min = 0.0, max = 0.2),
                 aspect = runif(m, min = 0.0, max = 0.2),
                 lat=runif(m, min = 0.0, max = 1.1))

NetRad_Looped.seq.sobol <- multisensi(design = sobol2007, model = NetRad_Looped,
                                     reduction = NULL, analysis = analysis.sensitivity, center = TRUE,
                                     design.args = list(X1 = Xb[1:(m/2), ], X2 = Xb[(1 + m/2):m, ], nboot = 100),
                                     analysis.args = list(keep.outputs = FALSE))
#
# Note, this is a good time time get a drink of water and/or pee as 
# it is running the function m=10,000 times (a few minutes).
#
print(NetRad_Looped.seq.sobol, digits = 2)
dev.off()
plot(NetRad_Looped.seq.sobol, normalized = TRUE, color = terrain.colors)
