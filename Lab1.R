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
 
