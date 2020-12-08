library(brms)
library(rethinking)
d<-data.frame(v1=runif(100,0,100),v2=runif(100,0,100),v3=runif(100,0,100))
library(HDInterval)


mod<-brm(v3~v1*v2,data=d,chains=1)

p<-posterior_samples(mod)

str(p)

newdata<-data.frame(v1=seq(0,100,1),v2=25)

v2low<-apply(newdata,1,function(x) p$b_v1*x[1]+p$b_v2*x[2]+p$"b_v1:v2"*x[1]*x[2])
meanlow<-t(apply(v2low,2,mean))
hdilow<-t(apply(v2low,2,hdi))

v2sim<-sapply(1:1000,function(x) rnorm(1,mean=v2low[x,1],sd=p$sigma[x]))


newdata<-data.frame(v1=seq(0,100,1),v2=75)
v2high<-apply(newdata,1,function(x) p$b_v1*x[1]+p$b_v2*x[2]+p$"b_v1:v2"*x[1]*x[2])

plot(0,0,xlim=c(0,100),ylim=c(0,100),type="n")
lines(newdata$v1,meanlow)
polygon(c(newdata$v1,rev(newdata$v1)),c(hdilow[,1],rev(hdilow[,2])))


pred<-predict(mod,newdata)

plot(pred[,1],meanlow)

BCInt_Comf <- brm(comfortablelendingt2 ~ comfortablelendingt1 + signal2 * conflict - 1, cores = 4, chains = 4, data = d0)

newdata<-expand.grid(comfortablelendingt1=mean(d0$comfortablelendingt1),signal2=seq(1:8),conflict=c(1,2))

newdataNC<-expand.grid(comfortablelendingt1=mean(d0$comfortablelendingt1),signal2=c(1:8),conflict=1)

newdataC<-expand.grid(comfortablelendingt1=mean(d0$comfortablelendingt1),signal2=c(1:8),conflict=2)


