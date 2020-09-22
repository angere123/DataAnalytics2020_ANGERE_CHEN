#Reading the data
data1<-read.csv("Fall 2020/Data Analytics/EPI_data.csv", header=T)
data1

summary(data1$EPI)
boxplot(data1$EPI)
fivenum(data1$EPI,na.rm=TRUE)
stem(data1$EPI)
hist(data1$EPI)
hist(data1$EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(data1$EPI, na.rm=TRUE, bw=1))
lines(density(data1$EPI, na.rm=TRUE, bw="SJ"))
rug(data1$EPI)

#fitting a distribution EPI
plot(ecdf(data1$EPI), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(data1$EPI); qqline(data1$EPI)
x <-seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot")
qqline(x)

#fitting a distribution DALY
plot(ecdf(data1$DALY), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(data1$DALY); qqline(data1$DALY)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot")
qqline(x)

#fitting a distribution WATER_H
plot(ecdf(data1$WATER_H), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(data1$WATER_H); qqline(data1$WATER_H)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot")
qqline(x)

boxplot(data1$EPI, data1$DALY)
qqplot(data1$EPI, data1$DALY)

#Inter-compare
boxplot(data1$EPI, data1$ENVHEALTH, data1$ECOSYSTEM, data1$DALY, data1$AIR_H, data1$WATER_H, data1$AIR_E, data1$WATER_E, data1$BIODIVERSITY)


#narrow down the data
EPILand<-data1$EPI[!data1$Landlock]
ELand <- EPILand[!is.na(EPILand)]
summary(ELand)
boxplot(ELand)
fivenum(ELand,na.rm=TRUE)
stem(ELand)
hist(ELand)
hist(ELand, seq(30., 95., 1.0), prob=TRUE)
lines(density(ELand, na.rm=TRUE, bw=1))
lines(density(ELand, na.rm=TRUE, bw="SJ"))
rug(ELand)

#fitting a distribution EPI
plot(ecdf(ELand), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(ELand); qqline(data1$EPI)
x <-seq(30, 95, 1)
qqplot(qt(ppoints(250), df=5), x, xlab="Q-Q plot")
qqline(x)

#Repeat for other columns
#No_surface_water
EPINSW <- data1$EPI[!data1$No_surface_water]
EWater <- EPINSW[!is.na(EPINSW)]
hist(EWater)
hist(EWater, seq(30., 95., 1.0), prob = TRUE)

#Desert
EPIDesert <- data1$EPI[!data1$Desert]
EDes <- EPIDesert[!is.na(EPIDesert)]
hist(EDes)
hist(EDes, seq(30., 95., 1.0), prob = TRUE)

#High_Population_Density
EPIHPD <- data1$EPI[!data1$High_Population_Density]
EHPD <- EPIHPD[!is.na(EPIHPD)]
hist(EHPD)
hist(EHPD, seq(30., 95., 1.0), prob = TRUE)

#LAB1 PART2
EPIAIRH<-data1$EPI[!data1$AIR_H]
EAIR <- EPIAIRH[!is.na(EPIAIRH)]

plot(ecdf(EAIR),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EAIR),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")
qqnorm(EAIR)
qqline(EAIR) # adding the line on the Q-Q plot
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)
boxplot(EAIR,ELand)


#ggplot example
plot(data1$ECOSYSTEM, data1$ENVHEALTH)
library(ggplot2)
qplot(data1$ECOSYSTEM, data1$ENVHEALTH)
qplot(data1$ECOSYSTEM, data1$ENVHEALTH, data=data1)
ggplot(data1, aes(x=data1$ECOSYSTEM, y=data1$ENVHEALTH)) +goem_point()
#plot(pressure$temperature, pressure$pressure, type = "l")
#points(pressure$temperature, pressure$pressure)


