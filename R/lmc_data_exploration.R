# Adapted from http://astrostatistics.psu.edu/RLectures/day2.pdf

# Load data from web.
LMC = read.table('http://astrostatistics.psu.edu/datasets/LMCmod.txt',header=T)

# Explore the table.
dim(LMC)
names(LMC)
summary(LMC)
LMC

# Computing the mean of 'Dist' data.
mean(LMC[,2])
mean(LMC[,"Dist"])
with(LMC, mean(Dist))
weighted.mean(LMC[,"Dist"], 1/LMC[,"Err"])

# Other manual summary statistics
median(LMC[,"Dist"])
sd(LMC[,"Dist"])
var(LMC[,"Dist"])
var(LMC[,"Err"])

mad(LMC[,"Dist"])

cov(LMC[,"Dist"],LMC[,"Err"])
cor(LMC[,"Dist"],LMC[,"Err"])

# Boxplots
boxplot(LMC[,"Dist"])
boxplot(LMC[,"Err"])

# Dot chart and box plots of another data set
asteroids = read.table(
  "http://astrostatistics.psu.edu/MSMA/datasets/asteroid_dens.dat",
  header=T)

astnames = asteroids[,1]
dens = asteroids[,2]
err = asteroids[,3]

dotchart(dens, labels=astnames, cex=0.9, xlab='Density (g/cm^3)')
boxplot(asteroids[,2:3], varwidth=T,
        xlab="Asteroids", ylab="Density",
        pars=list(boxwex=0.3,
          boxlwd=1.5,
          whisklwd=1.5,
          staplelwd=1.5,
          outlwd=1.5,
          font=2))

# Histogram
GC_MWG = read.table(
  'http://astrostatistics.psu.edu/MSMA/datasets/GlobClus_MWG.dat',
  header=T)
KGC_MWG = GC_MWG[,2] ;
kseq = seq(-15.0, -5.0, 0.25)
hist(KGC_MWG, breaks=kseq, ylim=c(0,10), main='',
     xlab='K mag', ylab='N')

# Scatter plots
# Construct and plot data set of SDSS quasars with 18<i<22
qso = read.table(
  'http://astrostatistics.psu.edu/MSMA/datasets/SDSS_17K.dat',
  header=T,fill=T)
qso = na.omit(qso)
summary(qso)
qso1 = qso[((qso[,6]<22) & (qso[,6]>18)),]
dim(qso1) ; summary(qso1) ; attach(qso1)
attach(qso1)
Err_z[which(Err_z<=0.03)] = 0.03
plot(i_mag, z_mag, pch=20, cex=0.1, col=grey(0.5),
     xlab="SDSS i (mag)", ylab="SDSS z (mag)")

errplot = function(x,y,xerrlo,yerrlo,xerrhi=xerrlo,yerrhi=yerrlo,...) {
  plot(x,y,xlim=range(x-xerrlo,x+xerrhi),
       ylim=range(y-yerrlo,y+yerrhi),...)
  segments(x,y-yerrlo,x,y+yerrhi)
  segments(x-xerrlo,y,x+xerrhi,y)
}
errplot(i_mag, z_mag, Err_i, Err_z, pch=20, cex=0.1, col=grey(0.5),
        xlab="SDSS i (mag)", ylab="SDSS z (mag)")


