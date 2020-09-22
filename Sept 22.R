library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$pct_LDS <- c(0.75, 4.53, 6.18, 1, 2.01, 2.82, 0.43, 0.55, 0.38,
               0.75, 0.82, 5.18, 26.35, 0.44, 0.66, 0.87, 1.25, 0.77, 0.64, 0.81,
               0.72, 0.39, 0.44, 0.58, 0.72, 1.14, 4.78, 1.29, 0.61, 0.37, 3.34,
               0.41, 0.82, 1.48, 0.52, 1.2, 3.85, 0.4, 0.37, 0.83, 1.27, 0.75,
               1.21, 67.97, 0.74, 1.13, 3.99, 0.92, 0.44, 11.5 )

m_5M4 <- map(
  alist(
    Divorce ~ dnorm(mu,sigma),
    mu <- a + bR*Marriage + bA*MedianAgeMarriage + bM*pct_LDS,
    a ~ dnorm(0,100),
    c(bA,bR,bM) ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=d )
precis( m_5M4 )

m_lm<-lm(Divorce~Marriage+MedianAgeMarriage+pct_LDS,data=d)
summary(m_lm)

post<-extract.samples(m_5M4)
hist(post$bM)
hist(post$bR)


#Hard
library(rethinking) 4.3
data(foxes)
d <- foxes
m1 <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + ba*area ,
    a ~ dnorm(0,100),
    ba ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
m2 <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + bg*groupsize ,
    a ~ dnorm(0,100),
    bg ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(m1)

par(mfrow=c(1,2))

x.seq <- seq(from=0,to=6,by=0.025)
mu <- link( m1 , data=list(area=x.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.ci <- apply( mu , 2 , PI )
plot( weight ~ area , data=d , col="slateblue" )
lines( x.seq , mu.mean )
lines( x.seq , mu.ci[1,] , lty=2 )
lines( x.seq , mu.ci[2,] , lty=2 )

x.seq <- seq(from=1,to=9,by=0.5)
mu <- link( m2 , data=list(groupsize=x.seq) )
mu.mean <- apply( mu , 2 , mean )
mu.ci <- apply( mu , 2 , PI )
plot( weight ~ groupsize , data=d , col="slateblue" )
lines( x.seq , mu.mean )
lines( x.seq , mu.ci[1,] , lty=2 )
lines( x.seq , mu.ci[2,] , lty=2 )


m3 <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + ba*area + bg*groupsize,
    a ~ dnorm(0,100),
    ba ~ dnorm(0,10),
    bg ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(m3)

par(mfrow=c(1,2))
area.seq <- seq(from=1,to=6,by=0.5)
pred.dat <- data.frame( area=area.seq , groupsize=mean(d$groupsize) )
mu <- link( m3 , data=pred.dat )
mu.mean <- apply( mu , 2 , mean )
mu.ci <- apply( mu , 2 , PI )
plot( weight ~ area , data=d , type="n" )
lines( area.seq , mu.mean )
lines( area.seq , mu.ci[1,] , lty=2 )
lines( area.seq , mu.ci[2,] , lty=2 )

#Add points for the conditonal effect (substracting effect of groupsize and replacing with mean groupsize)
post<-extract.samples(m3)
correctedweight<-d$weight-d$groupsize*mean(post$bg)+mean(d$groupsize)*mean(post$bg)
#in class I forgot to add back on the effect of mean groupsize "mean(d$groupsize)*mean(post$bg)"
points(d$area,correctedweight)



gs.seq <- seq(from=1,to=9,by=0.5)
pred.dat <- data.frame( area=mean(d$area) , groupsize=gs.seq )
mu <- link( m3 , data=pred.dat )
mu.mean <- apply( mu , 2 , mean )
mu.ci <- apply( mu , 2 , PI )
plot( weight ~ groupsize , data=d , type="n" )
lines( gs.seq , mu.mean )
lines( gs.seq , mu.ci[1,] , lty=2 )
lines( gs.seq , mu.ci[2,] , lty=2 )

post<-extract.samples(m3)
correctedweight<-d$weight-d$area*mean(post$ba)+mean(d$area)*mean(post$ba)
points(d$groupsize,correctedweight)


m4 <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + ba*area + bg*groupsize + bf*avgfood,
    a ~ dnorm(0,100),
    ba ~ dnorm(0,10),
    bg ~ dnorm(0,10),
    bf ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(m4)

m5 <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + bf*avgfood + bg*groupsize,
    a ~ dnorm(0,100),
    c(bg,bf) ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(m5)

d$foodi<-d$avgfood/d$groupsize
d$areai<-d$area/d$groupsize

m6 <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + bf*foodi + bg*areai,
    a ~ dnorm(0,100),
    c(bg,bf) ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=data.frame(apply(d,2,scale)) )
precis(m6)

m6f <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + bf*foodi,
    a ~ dnorm(0,100),
    bf ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(m6f)

m6a <- map(
  alist(
    weight ~ dnorm( mu , sigma ) ,
    mu <- a + bg*areai,
    a ~ dnorm(0,100),
    bg ~ dnorm(0,10),
    sigma ~ dunif(0,50)
  ) , data=d )
precis(m6a)

