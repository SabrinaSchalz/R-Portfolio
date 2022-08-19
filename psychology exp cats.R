#d' scores

#assuming differencing strategy instead of independent observer strategy
library(psyphy)
dprime.SD(H, FA, method = "diff")

#H=numeric hit rate
#FA=numeric false alarm rate


#returns value of d'


daten<-read.table("Cats Condition 3 Dscores.csv",header=TRUE,sep=";")
daten

#basic values
d<-daten$ï..d
mean(d)
#confidence intervals
a<-mean(d)
s<-sd(d, na.rm = FALSE)
n<-25
#falls normalverteilt
error<-qnorm(0.975)*s/sqrt(n)
#falls nicht
error<-qt(0.975,df=n-1)*s/sqrt(n)
left<-a-error
right<-a+error

#Tests
#normalverteilung (falls p größer als 0.05 normalverteilt)
shapiro.test(d)
#One-sample-t-test
t.test(d, mu = 0, alternative = "two.sided")
wilcox.test(d, mu = 0, alternative = "two.sided")

daten<-read.table("Cats Conditions Dscores.csv",header=TRUE,sep=";")
daten
d<-daten$ï..d
kruskal.test(ï..d ~ Condition, data = daten)
pairwise.wilcox.test(daten$ï..d, daten$Condition,
                 p.adjust.method = "BH")


daten<-read.table("Cats Condition 1 Dscores.csv",header=TRUE,sep=";")
daten2<-read.table("Cats Condition 2 Dscores.csv",header=TRUE,sep=";")
daten3<-read.table("Cats Condition 3 Dscores.csv",header=TRUE,sep=";")
d1<-daten$ï..d
d2<-daten2$ï..d
d3<-daten3$ï..d
boxplot(d1,d2,d3, ylim=c(0,6), ylab="d'", names=c("1","2", "3"))


#trend
daten<-read.table("Cats Condition 1 Learning.csv",header=TRUE,sep=";")
correct<-daten$Correct
daten2<-read.table("Cats Condition 2 Learning.csv",header=TRUE,sep=";")
correct2<-daten2$Correct
daten3<-read.table("Cats Condition 3 Learning.csv",header=TRUE,sep=";")
correct3<-daten3$Correct

plot(c(correct), type="b",lty=3,  ylab="percentage of correct answers", xlab="trials",ylim=c(30,100),col="black", main="Condition 1")

lines(correct2, type="b",lty=3, col="orange")

trial<-daten$ï..Trial
#linear regression falls normalverteilt
a<-lm(correct~trial)
summary(a)
abline(lm(correct ~ trial),col="red")


