rm (list=ls())
setwd ("/Users/fabianyii/Desktop/EyeQue Study/pvt/Plots")
library("readxl") #attach readxl package
eyeque <- data.frame(read_excel("/Users/fabianyii/Desktop/EyeQue Study/pvt/Data/eyeque.xlsx"))

#define Mfunction
mfunction <- function (s, c) {round(
  s + c/2, digits = 2)}
#define J0 function
j0function <- function (c, a) {round(
  -c/2*cos(2*a*pi/180), digits = 2)}
#define J45 function
j45function <- function (c, a) {round(
  -c/2*sin(2*a*pi/180), digits = 2)}

#sphero-cylindrical notation to power vectors
eyeque$m <- mfunction (eyeque$sphere, eyeque$cyl)
eyeque$j0 <- j0function (eyeque$cyl, eyeque$axis)
eyeque$j45 <- j45function (eyeque$cyl, eyeque$axis)

#create plots
library(RColorBrewer)
pdf (file = "/Users/fabianyii/Desktop/EyeQue Study/pvt/Plots/Rplot.pdf", width = 12, height = 8)

unique <- unique(data.frame(name = eyeque$name, eye = eyeque$eye))
for(i in 1: length(unique$name)){
  print(i)
  idx <- which (eyeque$name == unique$name[i] & eyeque$eye == unique$eye[i])
  d <- eyeque [idx[1:length(idx)], 1:9]
  
  par(mfrow = c(2,3), cex.main = 1.5, cex.lab = 1.2, font.lab = 4, font.main = 2, font.axis = 3, bty="n")
  
  plot(d$m, d$j0, main = paste0(unique$name[i], "(",unique$eye[i],")", ":", " ", "J0, M"), 
       xlab = "M (D)", ylab = "J0 (D)", 
       xlim = c(min(d$m)-1, max(d$m)+1), ylim = c(min(d$j0)-1, max(d$j0)+1),
       abline (h = mean(d$j0), v = mean(d$m), lty = 1, lwd = 0.15),
       pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu'))
  lines (d$m, d$j0, lty = 1, lwd = 0.1)
  text(d$m, d$j0, labels = row.names(data.frame(d$name)), pos=2, cex=0.7)
  
  plot(d$m, d$j45, main = paste0(unique$name[i], "(",unique$eye[i],")", ":", " ", "J45, M"),
       xlab = "M (D)", ylab = "J45 (D)", 
       xlim = c(min(d$m)-1, max(d$m)+1), ylim = c(min(d$j45)-1, max(d$j45)+1),
       abline (h = mean(d$j45), v = mean(d$m), lty = 1, lwd = 0.15),
       pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu'))
  lines (d$m, d$j45, lty = 1, lwd = 0.1)
  text(d$m, d$j45, labels = row.names(data.frame(d$name)), pos=4, cex=0.7)
     
  plot(d$j0, d$j45, main = paste0(unique$name[i], "(",unique$eye[i],")", ":", " ", "J45, J0"),
       xlab = "J0 (D)", ylab = "J45 (D)", 
       xlim = c(min(d$j0)-1, max(d$j0)+1), ylim = c(min(d$j45)-1, max(d$j45)+1),
       abline (h = mean(d$j45), v = mean(d$j0), lty = 1, lwd = 0.15),
       pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu'))
  lines (d$j0, d$j45, lty = 1, lwd = 0.1)
  text(d$j0, d$j45, labels = row.names(data.frame(d$name)), pos=2, cex=0.7)
  
  plot(d$sphere, d$cyl, main = paste0(unique$name[i], "(",unique$eye[i],")", ":", " ", "Cylinder, Sphere"),
       xlab = "Sphere (D)", ylab = "Cylinder (D)", 
       xlim = c(min(d$sphere)-1, max(d$sphere)+1), ylim = c(min(d$cyl)-1, max(d$cyl)+1),
       abline (h = mean(d$cyl), v = mean(d$sphere), lty = 1, lwd = 0.15),
       pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu'))
  lines (d$sphere, d$cyl, lty = 1, lwd = 0.1,)
  text(d$sphere, d$cyl, labels = row.names(data.frame(d$name)), pos=2, cex=0.7)
  
  d$x <- 0
  d$y <- 0
  d$slope <- (5*sin(d$axis*pi/180))/(5*cos(d$axis*pi/180))
  plot(d$x, d$y, cex=0.1, pch=3, xlab="90°", ylab="180°", main = paste0(unique$name[i], "(",unique$eye[i],")", ":", " ", "Axis"), bty="n", xaxt="n", yaxt="n") + abline(h=0, v=0, lwd=0.5)
  abline(a=0, b=d$slope[1], lty=1, lwd=1, col="red")
  abline(a=0, b=d$slope[2], lty=1, lwd=1, col="red")
  abline(a=0, b=d$slope[3], lty=1, lwd=1, col="red")
  abline(a=0, b=d$slope[4], lty=1, lwd=1, col="red")
  abline(a=0, b=d$slope[5], lty=1, lwd=1, col="red")
  abline(a=0, b=d$slope[6], lty=1, lwd=1, col="red")
  abline(a=0, b=d$slope[7], lty=1, lwd=1, col="red")
  abline(a=0, b=d$slope[8], lty=1, lwd=1, col="red")
  abline(a=0, b=d$slope[9], lty=1, lwd=1, col="red")
  abline(a=0, b=d$slope[10], lty=1, lwd=1, col="red")
}
dev.off()