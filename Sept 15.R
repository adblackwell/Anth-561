library(rethinking)

data("Howell1")
d<-Howell1

mod1<-lm(height~weight,data=d)

# height~a + B*weight + error

plot(height~weight,data=d)
abline(mod1)

mod2<-quap(alist(
  height ~ dnorm( mu , sigma ) ,
  mu <- a + b*weight,
  a ~ dnorm( 100 , 50 ) ,
  b ~ dlnorm( 0 , 10 ) ,
  sigma ~ dunif( 0 , 50 )
) ,data=d )


d$heightStand<-(d$height-mean(d$height,na.rm=TRUE))/sd(d$height,na.rm=TRUE)
d$heightStand<-scale(d$height)



mod2<-quap(alist(
  height ~ dnorm( mu , sigma ) ,
  mu <- a + b*weight,
  a ~ dunif( -1000 , 1000 ) ,
  b ~ dnorm( 10 , 0.1) ,
  sigma ~ dunif( 0 , 500 )
) ,data=d )


precis(mod2)      


library(brms)
mod3<-brm(height~weight,data=d,family=gaussian(),chains=3,cores=3)

get_prior(height~weight,data=d,family=gaussian())

prior<-c(prior(normal(100,50),class=Intercept),
         prior(lognormal(0,10),coef=weight))

mod3<-brm(height~weight,data=d,family=gaussian(),chains=3,cores=3)

#Medium
# 4M1. For the model definition below, simulate observed y values from the prior (not the posterior).
#yi ∼ Normal(μ, σ)
#μ ∼ Normal(0, 10)
#σ ∼ Exponential(1)
mu_prior <- rnorm( 1e4 , 0 , 10 )
sigma_prior <- runif( 1e4 , 0, 10 )
h_sim <- rnorm( 1e4 , mu_prior , sigma_prior )
dens( h_sim )
lines(density(mu_prior))


b ~ dnorm(0,)


#Hard
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$weight>=25,]

#Quadratic Approx Posterior
m <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 100 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )

#Extract Samples from Quadratic Posterior

post<-extract.samples(m, n=1e4)
head(post)

#Plug in the weights
new.weights<-c(46.95,43.72,64.78,32.59,54.63)
t(sapply(new.weights, function(x){
  y <- rnorm( 1e5 , post$a + post$b*x , post$sigma )
  return(c(mean(y), HPDI(y,prob=0.89)))
} ))

new.weights<-c(46.95,43.72,64.78,32.59,54.63)
t(sapply(new.weights, function(x){
  y <- post$a + post$b*x
  return(c(mean(y), HPDI(y,prob=0.89)))
} ))




d3 <- d[ d$age < 18 , ]
m <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 100 , 100 ),
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d3 )
precis(m)

plot(height~weight, data=d3)

post <- extract.samples( m )
w.seq <- seq(from=1,to=45,length.out=50)
mu <- sapply( w.seq , function(z) mean( post$a + post$b*z ) )
mu.ci <- sapply( w.seq , function(z)
  HPDI( post$a + post$b*z , prob=0.9 ) )
pred.ci <- sapply( w.seq , function(z)
  HPDI( rnorm(10000,post$a + post$b*z,post$sigma) , 0.9 ) )


plot( height ~ weight , data=d3 ,
      col=col.alpha("slateblue",0.5) , cex=0.5 )
lines( w.seq , mu )
lines( w.seq , mu.ci[1,] , lty=2 )
lines( w.seq , mu.ci[2,] , lty=2 )
lines( w.seq , pred.ci[1,] , lty=2 )
lines( w.seq , pred.ci[2,] , lty=2 )

library(brms)
m<-brm(height~weight,data=d3)
post<-posterior_samples(m)

w.seq <- seq(from=1,to=45,length.out=50)
mu <- sapply( w.seq , function(z) mean( post$b_Intercept + post$b_weight*z ) )
mu.ci <- sapply( w.seq , function(z)
  HPDI( post$b_Intercept + post$b_weight*z , prob=0.9 ) )
pred.ci <- sapply( w.seq , function(z)
  HPDI( rnorm(10000,post$b_Intercept + post$b_weight*z,post$sigma) , 0.9 ) )


plot( height ~ weight , data=d3 ,
      col=col.alpha("slateblue",0.5) , cex=0.5 )
lines( w.seq , mu )
lines( w.seq , mu.ci[1,] , lty=2 )
lines( w.seq , mu.ci[2,] , lty=2 )
lines( w.seq , pred.ci[1,] , lty=2 )
lines( w.seq , pred.ci[2,] , lty=2 )
