rm (list=ls()) #clear everything
setwd ("/Users/fabianyii/Desktop/EyeQue Study/Data") #set working "data" as working directory
library("readxl") #attach readxl package
eyeque <- read_excel("/Users/fabianyii/Desktop/EyeQue Study/pvt/Data/eyeque.xlsx") #read excel data into R

#define Mfunction
Mfunction <- function (s, c) {round(
  s + c/2, digits = 2)}
#define J0 function
J0function <- function (c, a) {round(
  -c/2*cos(2*a*pi/180), digits = 2)}
#define J45 function
J45function <- function (c, a) {round(
  -c/2*sin(2*a*pi/180), digits = 2)}

#sphero-cylindrical notation to power vectors (right eye)
eyeque$right.m <- Mfunction (eyeque$RSphere, eyeque$RCylinder)
eyeque$right.j0 <- J0function (eyeque$RCylinder, eyeque$RAxis)
eyeque$right.j45 <- J45function (eyeque$RCylinder, eyeque$RAxis)
#sphero-cylindrical notation to power vectors (left eye)
eyeque$left.m <- Mfunction(eyeque$LSphere, eyeque$LCylinder)
eyeque$left.j0 <- J0function (eyeque$LCylinder, eyeque$LAxis)
eyeque$left.j45 <- J45function (eyeque$LCylinder, eyeque$LAxis)

library(RColorBrewer)

### FABIAN YII (FB) ###
#create scatter plots for right eye
FY <- data.frame (eyeque[c(1:10),])
par(mfrow = c(2,3), cex.main = 1.5, cex.lab = 1.2, font.lab = 4, font.main = 2, font.axis = 3, bty="n")
plot (FY$right.m, FY$right.j0, main = "FB: J0, M (Right Eye)",
      xlab = "M", ylab = "J0",
      xlim = c(-8.5, -6.5), ylim = c(-0.5,1.5),
      abline (h = mean(FY$right.j0), v = mean(FY$right.m), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(FY$right.m, FY$right.j0, labels = row.names(FY[,9:10]), pos=1, cex=0.7)
lines(FY$right.m, FY$right.j0, lty = 1, lwd = 0.1, col = "grey90")

plot (FY$right.m, FY$right.j45, main = "FB: J45, M (Right Eye)",
      xlab = "M", ylab = "J45",
      xlim = c(-8.5, -6.5), ylim = c(-0.5, 1.5),
      abline (h = mean(FY$right.j45), v = mean(FY$right.m), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(FY$right.m, FY$right.j45, labels = row.names(FY[,c(9,11)]), pos=1, cex=0.7)
lines(FY$right.m, FY$right.j45, lty = 1, lwd = 0.1, col = "grey90")

plot (FY$right.j0, FY$right.j45, main = "FB: J45, J0 (Right Eye)", 
      xlab = "J0", ylab = "J45", 
      xlim = c(-0.5,1.5), ylim = c(-0.5,1.5),
      abline (h = mean(FY$right.j45), v = mean(FY$right.j0), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(FY$right.j0, FY$right.j45, labels = row.names(FY[,10:11]), pos=1, cex=0.7)
lines(FY$right.j0, FY$right.j45, lty = 1, lwd = 0.1, col = "grey90")

plot (FY$RSphere, FY$RCylinder, main = "FB: Cylinder, Sphere (Right Eye)",
      xlab = "Sphere (DS)", ylab = "Cylinder (DC)",
      xlim = c(-7.5, -5.5), ylim = c(-2.5, -0.5),
      abline (h = mean(FY$RCylinder), v = mean(FY$RSphere), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(FY$RSphere, FY$RCylinder, labels = row.names(FY[,3:4]), pos=1, cex=0.7)
lines(FY$RSphere, FY$RCylinder, lty = 1, lwd = 0.1, col = "grey90")

#Plot for RE axes
FB_Rslope = data.frame((5*sin(FY$RAxis*pi/180))/(5*cos(FY$RAxis*pi/180)))
plot(0,0, cex=0.1, pch=3, xlab="90°", ylab="180°", main="FB: RE Axes", bty="n", xaxt="n", yaxt="n") + abline(h=0, v=0, lwd=0.5)
abline(a=0, b=FB_Rslope[1,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Rslope[2,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Rslope[3,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Rslope[4,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Rslope[5,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Rslope[6,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Rslope[7,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Rslope[8,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Rslope[9,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Rslope[10,], lty=1, lwd=1, col="red")

#Create scatterplots for left eye
par(mfrow = c(2,3), cex.main = 1.5, cex.lab = 1.2, font.lab = 4, font.main = 2, font.axis = 3, bty="n")
plot (FY$left.m, FY$left.j0, main = "FB: J0, M (Left Eye)",
      xlab = "M", ylab = "J0",
      xlim = c(-8.5, -6.5), ylim = c(-0.5, 1.5),
      abline (h = mean(FY$left.j0), v = mean(FY$left.m), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(FY$left.m, FY$left.j0, labels = row.names(FY[,12:13]), pos=1, cex=0.7)
lines (FY$left.m, FY$left.j0, lty = 1, lwd = 0.1)

plot (FY$left.m, FY$left.j45, main = "FB: J45, M (Left Eye)",
      xlab = "M", ylab = "J45",
      xlim = c(-8.5, -6.5), ylim = c(-0.5, 1.5),
      abline (h = mean(FY$left.j45), v = mean(FY$left.m), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(FY$left.m, FY$left.j45, labels = row.names(FY[,12:13]), pos=1, cex=0.7)
lines(FY$left.m, FY$left.j45, lty = 1, lwd= 0.1, col = "grey90")

plot (FY$left.j0, FY$left.j45, main = "FB: J45, J0 (Left Eye)",
      xlab = "J0", ylab = "J45",
      xlim = c(-0.5, 1.5), ylim = c(-0.5, 1.5),
      abline (h = mean (FY$left.j45), v = mean(FY$left.j0), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(FY$left.j0, FY$left.j45, labels = row.names(FY[,13:14]), pos=1, cex=0.7)
lines(FY$left.j0, FY$left.j45, lty = 1, lwd = 0.1, col = "grey90")

plot (FY$LSphere, FY$LCylinder, main = "FB: Cylinder, Sphere (Left Eye)",
      xlab = "Sphere (DS)", ylab = "Cylinder (DC)",
      xlim = c(-7.5, -5.5), ylim = c(-2.5, -0.5),
      abline (h = mean(FY$LCylinder), v = mean(FY$LSphere), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(FY$LSphere, FY$LCylinder, labels = row.names(FY[,6:7]), pos=2, cex=0.7)
lines(FY$LSphere, FY$LCylinder, lty = 1, lwd = 0.1, col = "grey90")

#Plot for LE axes
FB_Lslope = data.frame((5*sin(FY$LAxis*pi/180))/(5*cos(FY$LAxis*pi/180)))
plot(0,0, cex=0.1, pch=3, xlab="90°", ylab="180°", main="FB: LE Axes", bty="n", xaxt="n", yaxt="n") + abline(h=0, v=0, lwd=0.5)
abline(a=0, b=FB_Lslope[1,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Lslope[2,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Lslope[3,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Lslope[4,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Lslope[5,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Lslope[6,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Lslope[7,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Lslope[8,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Lslope[9,], lty=1, lwd=1, col="red")
abline(a=0, b=FB_Lslope[10,], lty=1, lwd=1, col="red")

#mean of right eye power vectors
paste("Mean RE M ± repeatability interval", "=",
      right.mean.m <- round(mean (FY$right.m), digits=2), "±", 
      r.interval.m <- round(1.96*sd(FY$right.m), digits=2))
paste("Mean RE J0 ± repeatability interval", "=", 
      right.mean.j0 <- round(mean (FY$right.j0), digits=2), "±",
      r.interval.j0 <- round(1.96*sd(FY$right.j0), digits=2))
paste("Mean RE J45 ± repeatability interval",
      right.mean.j45 <- round(mean (FY$right.j45), digits=2), "±",
      r.interval.j45 <- round(1.96*sd(FY$right.j45), digits=2))

#mean of left eye power vectors
paste("Mean LE M ± repeatability interval =",
      left.mean.m <- round(mean (FY$left.m), digits=2), "±",
      l.interval.m <- round(1.96*sd(FY$left.m), digits=2))
paste("Mean LE J0 ± repeatability interval =", 
      left.mean.j0 <- round(mean (FY$left.j0), digits=2), "±",
      l.interval.j0 <- round(1.96*sd(FY$left.j0), digits=2))
paste("Mean LE J45 ± repeatability interval =",
      left.mean.j45 <- round(mean (FY$left.j45), digits=2), "±",
      l.interval.j45 <- round(1.96*sd(FY$left.j45), digits=2))

#function: convert mean power vectors back to sphero-cylindrical notation (scn)
mean.sphere <- function (m, j0, j45) {round(
  m + sqrt(j0^2) + (j45^2), digits = 2)} #sphere
mean.cylinder <- function (j0, j45) {round(
  -2*sqrt((j0^2) + (j45^2)), digits = 2)} #cylinder
mean.axis <- function (j45, j0) {round(
  0.5*atan(j45/j0)*180/pi, digits = 0)} #axis

paste("RE mean sphero-cylindrical ± repeatability interval = ",
      right.mean.sphere <- mean.sphere (right.mean.m, right.mean.j0, right.mean.j45), "±", mean.sphere (r.interval.m, r.interval.j0, r.interval.j45),
      "/", right.mean.cylinder <- mean.cylinder (right.mean.j0, right.mean.j45), "±", mean.cylinder (r.interval.j0, r.interval.j45), "X",
      right.mean.axis <- mean.axis (right.mean.j45, right.mean.j0), "±", mean.axis (r.interval.j45, r.interval.j0)) 
show("True RE refractive error = -6.75 / -2.00 X 180")

paste("LE mean sphero-cylindrical ± repeatability interval =",
      left.mean.sphere <- mean.sphere(left.mean.m, left.mean.j0, left.mean.j45), "±", mean.sphere (l.interval.m, l.interval.j0, l.interval.j45),
      "/", left.mean.cylinder <- mean.cylinder (left.mean.j0, left.mean.j45), "±", mean.cylinder (l.interval.j0, l.interval.j45), "X",
      left.mean.axis <- mean.axis (left.mean.j45, left.mean.j0), "±", mean.axis (l.interval.j45, l.interval.j0))
show("True LE refractive error = -6.75 / -2.00 X 180")

### PAIVI MORGAN (PM) ###
#create scatter plots for right eye
PM <- data.frame (eyeque[c(11:20),])
par(mfrow = c(2,3), cex.main = 1.5, cex.lab = 1.2, font.lab = 4, font.main = 2, font.axis = 3, bty="n")
plot (PM$right.m, PM$right.j0, main = "PM: J0, M (Right Eye)",
      xlab = "M", ylab = "J0",
      abline (h = mean(PM$right.j0), v = mean(PM$right.m), lty = 1, lwd = 0.3),
      xlim = c(-7.0, -5.0), ylim = c(-1.0, 1.0),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(PM$right.m, PM$right.j0, labels = row.names(FY[,12:13]), pos=1, cex=0.7)
lines(PM$right.m, PM$right.j0, lty = 1, lwd = 0.1, col = "grey90")

plot (PM$right.m, PM$right.j45, main = "PM: J45, M (Right Eye)",
      xlab = "M", ylab = "J45",
      abline (h = mean(PM$right.j45), v = mean(PM$right.m), lty = 1, lwd = 0.3),
      xlim = c(-7.0, -5.0), ylim = c(-1.0, 1.0),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(PM$right.m, PM$right.j45, labels = row.names(PM[,9:10]), pos=1, cex=0.7)
lines(PM$right.m, PM$right.j45, lty = 1, lwd = 0.1, col = "grey90")

plot (PM$right.j0, PM$right.j45, main = "PM: J45, J0 (Right Eye)", 
      xlab = "J0", ylab = "J45", 
      abline (h = mean(PM$right.j45), v = mean(PM$right.j0), lty = 1, lwd = 0.3),
      xlim = c(-1.0, 1.0), ylim = c(-1.0, 1.0),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(PM$right.j0, PM$right.j45, labels = row.names(FY[,10:11]), pos=1, cex=0.7)
lines(PM$right.j0, PM$right.j45, lty = 1, lwd = 0.1, col = "grey90")

plot (PM$RSphere, PM$RCylinder, main = "PM: Cylinder, Sphere (Right Eye)",
      xlab = "Sphere (DS)", ylab = "Cylinder (DC)",
      xlim = c(-7.0, -5.0), ylim = c(-1.0, 1.0),
      abline (h = mean(PM$RCylinder), v = mean(PM$RSphere), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(PM$RSphere, PM$RCylinder, labels = row.names(PM[,3:4]), pos=1, cex=0.7)
lines(PM$RSphere, PM$RCylinder, lty = 1, lwd = 0.1, col = "grey90")

#Plot RE axes
PM_Rslope = data.frame((5*sin(PM$RAxis*pi/180))/(5*cos(PM$RAxis*pi/180)))
plot(0,0, cex=0.1, pch=3, xlab="90°", ylab="180°", main="PM: RE Axes", bty="n", xaxt="n", yaxt="n") + abline(h=0, v=0, lwd=0.5)
abline(a=0, b=PM_Rslope[1,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Rslope[2,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Rslope[3,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Rslope[4,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Rslope[5,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Rslope[6,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Rslope[7,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Rslope[8,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Rslope[9,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Rslope[10,], lty=1, lwd=1, col="red")

#Create scatterplots for left eye
par(mfrow = c(2,3), cex.main = 1.5, cex.lab = 1.2, font.lab = 4, font.main = 2, font.axis = 3, bty="n")
plot (PM$left.m, PM$left.j0, main = "PM: J0, M (Left Eye)",
      xlab = "M", ylab = "J0",
      abline (h = mean(PM$left.j0), v = mean(PM$left.m), lty = 1, lwd = 0.3),
      xlim = c(-6.5, -4.5), ylim = c(-1.0, 1.0),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(PM$left.m, PM$left.j0, labels = row.names(PM[,12:13]), pos=1, cex=0.7)
lines (PM$left.m, PM$left.j0, lty = 1, lwd = 0.1, col = "grey90")

plot (PM$left.m, PM$left.j45, main = "PM: J45, M (Left Eye)",
      xlab = "M", ylab = "J45",
      abline (h = mean(PM$left.j45), v = mean(PM$left.m), lty = 1, lwd = 0.3),
      xlim = c(-6.5, -4.5), ylim = c(-1.0, 1.0),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(PM$left.m, PM$left.j0, labels = row.names(PM[,12:14]), pos=2, cex=0.7)
lines(PM$left.m, PM$left.j45, lty = 1, lwd = 0.1, col = "grey90")

plot (PM$left.j0, PM$left.j45, main = "PM: J45, J0 (Left Eye)",
      xlab = "J0", ylab = "J45",
      xlim = c(-1.0, 1.0), ylim = c(-1.0, 1.0),
      abline (h = mean (PM$left.j45), v = mean(PM$left.j0), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(PM$left.j0, PM$left.j45, labels = row.names(PM[,13:14]), pos=2, cex=0.7)
lines(PM$left.j0, PM$left.j45, lty = 1, lwd = 0.1, col = "grey90")

plot (PM$LSphere, PM$LCylinder, main = "PM: Cylinder, Sphere (Left Eye)",
      xlab = "Sphere (DS)", ylab = "Cylinder (DC)",
      xlim = c(-6.0, -4.0), ylim = c(-1.0, 1.0),
      abline (h = mean(PM$LCylinder), v = mean(PM$LSphere), lty = 1, lwd = 0.3),
      pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu')
)
text(PM$LSphere, PM$LCylinder, labels = row.names(PM[,6:7]), pos=1, cex=0.7)
lines(PM$LSphere, PM$LCylinder, lty = 1 , lwd = 0.1, col = "grey90")

#Plot LE axes
PM_Lslope = data.frame((5*sin(PM$LAxis*pi/180))/(5*cos(PM$LAxis*pi/180)))
plot(0,0, cex=0.1, pch=3, xlab="90°", ylab="180°", main="PM: LE Axes", bty="n", xaxt="n", yaxt="n") + abline(h=0, v=0, lwd=0.5)
abline(a=0, b=PM_Lslope[1,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Lslope[2,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Lslope[3,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Lslope[4,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Lslope[5,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Lslope[6,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Lslope[7,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Lslope[8,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Lslope[9,], lty=1, lwd=1, col="red")
abline(a=0, b=PM_Lslope[10,], lty=1, lwd=1, col="red")

#mean of right eye power vectors
paste("Mean RE M ± repeatability interval", "=",
      right.mean.m <- round(mean (PM$right.m), digits=2), "±", 
      r.interval.m <- round(1.96*sd(PM$right.m), digits=2))
paste("Mean RE J0 ± repeatability interval", "=", 
      right.mean.j0 <- round(mean (PM$right.j0), digits=2), "±",
      r.interval.j0 <- round(1.96*sd(PM$right.j0), digits=2))
paste("Mean RE J45 ± repeatability interval",
      right.mean.j45 <- round(mean (PM$right.j45), digits=2), "±",
      r.interval.j45 <- round(1.96*sd(PM$right.j45), digits=2))

#mean of left eye power vectors
paste("Mean LE M ± repeatability interval =",
      left.mean.m <- round(mean (PM$left.m), digits=2), "±",
      l.interval.m <- round(1.96*sd(PM$left.m), digits=2))
paste("Mean LE J0 ± repeatability interval =", 
      left.mean.j0 <- round(mean (PM$left.j0), digits=2), "±",
      l.interval.j0 <- round(1.96*sd(PM$left.j0), digits=2))
paste("Mean LE J45 ± repeatability interval =",
      left.mean.j45 <- round(mean (PM$left.j45), digits=2), "±",
      l.interval.j45 <- round(1.96*sd(PM$left.j45), digits=2))

#function: convert mean power vectors back to sphero-cylindrical notation (scn)
mean.sphere <- function (m, j0, j45) {round(
  m + sqrt(j0^2) + (j45^2), digits = 2)} #sphere
mean.cylinder <- function (j0, j45) {round(
  -2*sqrt((j0^2) + (j45^2)), digits = 2)} #cylinder
mean.axis <- function (j45, j0) {round(
  0.5*atan(j45/j0)*180/pi, digits = 0)} #axis

paste("RE mean sphero-cylindrical ± repeatability interval = ",
      right.mean.sphere <- mean.sphere (right.mean.m, right.mean.j0, right.mean.j45), "±", mean.sphere (r.interval.m, r.interval.j0, r.interval.j45),
      "/", right.mean.cylinder <- mean.cylinder (right.mean.j0, right.mean.j45), "±", mean.cylinder (r.interval.j0, r.interval.j45), "X",
      right.mean.axis <- mean.axis (right.mean.j45, right.mean.j0), "±", mean.axis (r.interval.j45, r.interval.j0)) 

paste("LE mean sphero-cylindrical ± repeatability interval =",
      left.mean.sphere <- mean.sphere(left.mean.m, left.mean.j0, left.mean.j45), "±", mean.sphere (l.interval.m, l.interval.j0, l.interval.j45),
      "/", left.mean.cylinder <- mean.cylinder (left.mean.j0, left.mean.j45), "±", mean.cylinder (l.interval.j0, l.interval.j45), "X",
      left.mean.axis <- mean.axis (left.mean.j45, left.mean.j0), "±", mean.axis (l.interval.j45, l.interval.j0))