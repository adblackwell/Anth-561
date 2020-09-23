## R code 2.1
p_grid <- seq( from=0 , to=1 , length.out=100 )
# likelihood of 3 water in 3 tosses
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
prior <- rep(1,100) # uniform prior
posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize

## R code 2.2
plot( posterior ~ p_grid , type="l" )

## R code 2.3
# likelihood of 3 water in 4 tosses
likelihood <- dbinom( 3 , size=4 , prob=p_grid )

## R code 2.4
# likelihood of 5 water in 7 tosses
likelihood <- dbinom( 5 , size=7 , prob=p_grid )

## R code 2.5
p_grid <- seq( from=0 , to=1 , length.out=100 )
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
prior <- ifelse( p_grid < 0.5 , 0 , 1 ) # new prior
posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize
plot( posterior ~ p_grid , type="l" )

## R code 2.6
0.3*0.5 / ( 0.3*0.5 + 1*0.5 )

## R code 3.1
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

## R code 3.2
sum( samples < 0.2 )

## R code 3.3
sum( samples < 0.2 ) / 1e4

## R code 3.4
sum( samples > 0.8 ) / 1e4

## R code 3.5
sum( samples > 0.2 & samples < 0.8 ) / 1e4

## R code 3.6
quantile( samples , 0.2 )

## R code 3.7
sum( samples < 0.52  ) / 1e4

## R code 3.8
quantile( samples , 0.8 )

## R code 3.9
sum( samples > 0.75  ) / 1e4

## R code 3.10
HPDI( samples , prob=0.66 )

## R code 3.11
PI( samples , prob=0.66 )

## R code 3.12
interval1 <- HPDI( samples , prob=0.66 )
interval2 <- PI( samples , prob=0.66 )
width1 <- interval1[2] - interval1[1]
width2 <- interval2[2] - interval2[1]
cbind(width1,width2)

## R code 3.13
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

## R code 3.14
plot( posterior ~ p_grid , type="l" )

## R code 3.15
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

## R code 3.16
HPDI( samples , prob=0.9 )

## R code 3.17
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
w <- rbinom( 1e4 , size=15 , prob=samples )

## R code 3.18
sum( w==8 ) / 1e4

## R code 3.19
simplehist(w)

## R code 3.20
w <- rbinom( 1e4 , size=9 , prob=samples )
sum( w==6 ) / 1e4

## R code 3.21
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- ifelse( p_grid < 0.5 , 0 , 1 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot( posterior ~ p_grid , type="l" )

## R code 3.22
HPDI( samples , prob=0.9 )

## R code 3.23
w <- rbinom( 1e4 , size=15 , prob=samples )
simplehist(w)

## R code 3.24
p <- seq( from=0 , to=1 , length.out=1000 )

## R code 3.25
prior <- rep(1,length(p))

## R code 3.26
library(rethinking)
data(homeworkch3)
boys <- sum(birth1) + sum(birth2)
likelihood <- dbinom( boys , size=200 , prob=p )

## R code 3.27
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

## R code 3.28
plot( posterior ~ p , type="l" )
abline( v=0.5 , lty=2 )

## R code 3.29
p[ which.max(posterior) ]

## R code 3.30
p.samples <- sample( p , size=10000 , replace=TRUE , prob=posterior )
HPDI(p.samples,prob=0.50)
HPDI(p.samples,prob=0.89)
HPDI(p.samples,prob=0.97)

## R code 3.31
p.samples <- sample( p , size=10000 , replace=TRUE , prob=posterior )
bsim <- rbinom( 10000 , size=200 , prob=p.samples )

## R code 3.32
# adj value makes a strict histogram, with spikes at integers
dens( bsim , adj=0.1 )
abline( v=sum(birth1)+sum(birth2) , col="red" )

## R code 3.33
b1sim <- rbinom( 10000 , size=100 , prob=p.samples )
dens( b1sim , adj=0.1 )
abline( v=sum(birth1) , col="red" )

## R code 3.34
b01 <- birth2[birth1==0]
b01sim <- rbinom( 10000 , size=length(b01) , prob=p.samples )
dens(b01sim,adj=0.1)
abline( v=sum(b01) , col="red" )

## R code 4.1
mu_prior <- rnorm( 1e4 , 0 , 10 )
sigma_prior <- rexp( 1e4 , 1 )

## R code 4.2
h_sim <- rnorm( 1e4 , mu_prior , sigma_prior )
dens( h_sim )

## R code 4.3
f <- alist(
    y ~ dnorm( mu , sigma ),
    mu ~ dnorm( 0 , 10 ),
    sigma ~ dexp( 1 )
)

## R code 4.4
n <- 50
a <- rnorm( n , 100 , 10 )
b <- rnorm( n , 0 , 10 )
s <- rexp( n , 1 )

## R code 4.5
y <- 1:3
ybar <- mean(y)
# matrix of heights, students in rows and years in columns
h <- matrix( NA , nrow=n , ncol=3 )
for ( i in 1:n ) for ( j in 1:3 )
    h[ i , j ] <- rnorm( 1 , a[i] + b[i]*( y[j] - ybar ) , s[i] )

## R code 4.6
plot( NULL , xlim=c(1,3) , ylim=range(h) , xlab="year" , ylab="height (cm)" )
for ( i in 1:n ) lines( 1:3 , h[i,] )

## R code 4.7
n <- 50
a <- rnorm( n , 100 , 10 )
b <- rlnorm( n , 1 , 0.5 )
s <- rexp( n , 1 )

y <- 1:3
ybar <- mean(y)
# matrix of heights, students in rows and years in columns
h <- matrix( NA , nrow=n , ncol=3 )
for ( i in 1:n ) for ( j in 1:3 )
    h[ i , j ] <- rnorm( 1 , a[i] + b[i]*( y[j] - ybar ) , s[i] )

plot( NULL , xlim=c(1,3) , ylim=range(h) , xlab="year" , ylab="height (cm)" )
for ( i in 1:n ) lines( 1:3 , h[i,] )

## R code 4.8
library(rethinking)
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]
m4.3b <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*weight ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d2 )
precis( m4.3b )

## R code 4.9
precis( m4.3 )

## R code 4.10
round( vcov( m4.3b ) , 2 )

## R code 4.11
round( cov2cor(vcov( m4.3b )) , 2 )

## R code 4.12
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[d$age>=18,]
xbar <- mean( d2$weight )

m <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( weight  - xbar ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d2 )

## R code 4.13
post <- extract.samples( m )
str(post)

## R code 4.14
y <- rnorm( 1e5 , post$a + post$b*( 46.95 - xbar ) , post$sigma )
mean(y)
PI(y,prob=0.89)

## R code 4.15
f <- function( weight ) {
    y <- rnorm( 1e5 , post$a + post$b*( weight - xbar ) , post$sigma )
    return( c( mean(y) , PI(y,prob=0.89) ) )
}
weight_list <- c(46.95,43.72,64.78,32.59,54.63)
result <- sapply( weight_list , f )

## R code 4.16
rtab <- cbind( weight_list , t( result ) )
colnames(rtab) <- c("weight","height","5%","94%")
rtab

## R code 4.17
library(rethinking)
data(Howell1)
d <- Howell1
d3 <- d[ d$age < 18 , ]
str(d3)

## R code 4.18
xbar <- mean( d3$weight )
m <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( weight - xbar ) ,
        a ~ dnorm( 178 , 20 ),
        b ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d3 )
precis(m)

## R code 4.19
post <- extract.samples( m )
w.seq <- seq(from=1,to=45,length.out=50)
mu <- sapply( w.seq , function(z) mean( post$a + post$b*(z-xbar) ) )
mu.ci <- sapply( w.seq , function(z)
    PI( post$a + post$b*(z-xbar) , prob=0.89 ) )
pred.ci <- sapply( w.seq , function(z)
    PI( rnorm( 10000 , post$a + post$b*(z-xbar) , post$sigma) , 0.89 ) )

## R code 4.20
plot( height ~ weight , data=d3 ,
    col=col.alpha("slateblue",0.5) , cex=0.5 )
lines( w.seq , mu )
lines( w.seq , mu.ci[1,] , lty=2 )
lines( w.seq , mu.ci[2,] , lty=2 )
lines( w.seq , pred.ci[1,] , lty=2 )
lines( w.seq , pred.ci[2,] , lty=2 )

## R code 4.21
library(rethinking)
data(Howell1)
d <- Howell1
logxbar <- mean( log(d$weight) )
mlw <- quap(
    alist(
        height ~ dnorm( mean=mu , sd=sigma ) ,
        mu <- a + b*( log(weight) - logxbar ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dnorm( 0 , 10 ) ,
        sigma ~ dunif( 0 , 50 )
    ) , data=d )
precis(mlw)

## R code 4.22
post <- extract.samples(mlw)
w.seq <- seq(from=4,to=63,length.out=50)
mu <- sapply( w.seq , function(z) mean( post$a+post$b*(log(z)-logxbar) ) )
mu.ci <- sapply( w.seq , function(z) PI( post$a+post$b*(log(z)-logxbar) ) )
h.ci <- sapply( w.seq , function(z)
    PI( rnorm(10000,post$a+post$b*(log(z)-logxbar),post$sigma) ) )

## R code 4.23
plot( height ~ weight , data=d , col=col.alpha("slateblue",0.4) )
lines( w.seq , mu )
lines( w.seq , mu.ci[1,] , lty=2 )
lines( w.seq , mu.ci[2,] , lty=2 )
lines( w.seq , h.ci[1,] , lty=2 )
lines( w.seq , h.ci[2,] , lty=2 )

## R code 4.24
## R code 4.25
## R code 4.26
## R code 4.27
## R code 4.28
library(rethinking)
data(cherry_blossoms)
colSums( is.na(cherry_blossoms) )

## R code 4.29
d <- cherry_blossoms
d2 <- d[ complete.cases( d$doy , d$temp ) , c("doy","temp") ]

## R code 4.30
num_knots <- 30
knot_list <- quantile( d2$temp , probs=seq(0,1,length.out=num_knots) )
library(splines)
B <- bs(d2$temp,
    knots=knot_list[-c(1,num_knots)] ,
    degree=3 , intercept=TRUE )

## R code 4.31
m4H5 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + B %*% w ,
        a ~ dnorm(100,10),
        w ~ dnorm(0,10),
        sigma ~ dexp(1)
    ), data=list( D=d2$doy , B=B ) ,
    start=list( w=rep( 0 , ncol(B) ) ) )

## R code 4.32
mu <- link( m4H5 )
mu_mean <- apply( mu , 2 , mean )
mu_PI <- apply( mu , 2 , PI, 0.97 )
plot( d2$temp , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 ,
     xlab="temp" , ylab="doy" )
o <- order( d2$temp )
lines( d2$temp[o] , mu_mean[o] , lwd=3 )
shade( mu_PI[,o] , d2$temp[o] , col=grau(0.3) )

## R code 4.33
library(rethinking)
data(cherry_blossoms)
d <- cherry_blossoms
d2 <- d[ complete.cases(d$doy) , ] # complete cases on doy
num_knots <- 15
knot_list <- quantile( d2$year , probs=seq(0,1,length.out=num_knots) )
library(splines)
B <- bs(d2$year,
    knots=knot_list[-c(1,num_knots)] ,
    degree=3 , intercept=TRUE )

m4.7 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + B %*% w ,
        a ~ dnorm(100,10),
        w ~ dnorm(0,10),
        sigma ~ dexp(1)
    ), data=list( D=d2$doy , B=B ) ,
    start=list( w=rep( 0 , ncol(B) ) ) )

## R code 4.34
p <- extract.prior( m4.7 )
mu <- link( m4.7 , post=p )
mu_mean <- apply( mu , 2 , mean )
mu_PI <- apply( mu , 2 , PI, 0.97 )
plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 ,
     xlab="year" , ylab="doy" , ylim=c(60,140) )
lines( d2$year , mu_mean , lwd=3 )
shade( mu_PI , d2$year , col=grau(0.3) )

## R code 4.35
p <- extract.prior( m4.7 )
mu <- link( m4.7 , post=p )
plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 ,
     xlab="year" , ylab="day in year" , ylim=c(60,140) )
for ( i in 1:20 ) lines( d2$year , mu[i,] , lwd=1 )

## R code 4.36
m4.7b <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + B %*% w ,
        a ~ dnorm(100,10),
        w ~ dnorm(0,1),
        sigma ~ dexp(1)
    ), data=list( D=d2$doy , B=B ) ,
    start=list( w=rep( 0 , ncol(B) ) ) )

p <- extract.prior( m4.7b )
mu <- link( m4.7b , post=p )
plot( d2$year , d2$doy , col=col.alpha(rangi2,0.3) , pch=16 ,
     xlab="year" , ylab="day in year" , ylim=c(60,140) )
for ( i in 1:20 ) lines( d2$year , mu[i,] , lwd=1 )

## R code 4.37
library(rethinking)
data(cherry_blossoms)
d <- cherry_blossoms
d2 <- d[ complete.cases(d$doy) , ] # complete cases on doy
num_knots <- 15
knot_list <- quantile( d2$year , probs=seq(0,1,length.out=num_knots) )
library(splines)
B <- bs(d2$year,
    knots=knot_list[-c(1,num_knots)] ,
    degree=3 , intercept=TRUE )

m4H7 <- quap(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- 0 + B %*% w ,
        w ~ dnorm(100,10),
        sigma ~ dexp(1)
    ), data=list( D=d2$doy , B=B ) ,
    start=list( w=rep( 0 , ncol(B) ) ) )

## R code 5.1
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$pct_LDS <- c(0.75, 4.53, 6.18, 1, 2.01, 2.82, 0.43, 0.55, 0.38,
  0.75, 0.82, 5.18, 26.35, 0.44, 0.66, 0.87, 1.25, 0.77, 0.64, 0.81,
  0.72, 0.39, 0.44, 0.58, 0.72, 1.14, 4.78, 1.29, 0.61, 0.37, 3.34,
  0.41, 0.82, 1.48, 0.52, 1.2, 3.85, 0.4, 0.37, 0.83, 1.27, 0.75,
  1.21, 67.97, 0.74, 1.13, 3.99, 0.92, 0.44, 11.5 )
d$L <- standardize( d$pct_LDS )
d$A <- standardize( d$MedianAgeMarriage )
d$M <- standardize( d$Marriage )
d$D <- standardize( d$Divorce )

## R code 5.2
m_5M4 <- quap(
    alist(
        D ~ dnorm(mu,sigma),
        mu <- a + bM*M + bA*A + bL*L,
        a ~ dnorm(0,0.2),
        c(bA,bM,bL) ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ), data=d )
precis( m_5M4 )

## R code 5.3
library(dagitty)
dag_5H1 <- dagitty("dag{M->A->D}")
impliedConditionalIndependencies(dag_5H1)

## R code 5.4
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )
d$A <- standardize( d$MedianAgeMarriage )

## R code 5.5
m5H2 <- quap(
    alist(
       # A -> D
        D ~ dnorm( muD , sigmaD ),
        muD <- aD + bAD*A,
       # M -> A
        A ~ dnorm( muA , sigmaA ),
        muA <- aA + bMA*M,
       # priors
        c(aD,aA) ~ dnorm(0,0.2),
        c(bAD,bMA) ~ dnorm(0,0.5),
        c(sigmaD,sigmaA) ~ dexp(1)
    ) , data=d )
precis(m5H2)

## R code 5.6
M_seq <- seq( from=-3 , to=3 , length.out=30 )

sim_dat <- data.frame( M=M_seq )

s <- sim( m5H2 , data=sim_dat , vars=c("A","D") )

plot( sim_dat$M , colMeans(s$A) , ylim=c(-2,2) , type="l" ,
    xlab="manipulated M" , ylab="counterfactual A"  )
shade( apply(s$A,2,PI) , sim_dat$M )
mtext( "Counterfactual M -> A" )

plot( sim_dat$M , colMeans(s$D) , ylim=c(-2,2) , type="l" ,
    xlab="manipulated M" , ylab="counterfactual D"  )
shade( apply(s$D,2,PI) , sim_dat$M )
mtext( "Counterfactual M -> A -> D" )

## R code 5.7
(10 - mean(d$Marriage))/sd(d$Marriage)

## R code 5.8
M_seq <- c( 0 , -2.67 )
sim_dat <- data.frame( M=M_seq )
s <- sim( m5H2 , data=sim_dat , vars=c("A","D") )
diff <- s$D[,2] - s$D[,1]
mean( diff )

## R code 5.9
library(rethinking)
data(milk)
d <- milk
d$K <- standardize( d$kcal.per.g )
d$N <- standardize( d$neocortex.perc )
d$M <- standardize( log(d$mass) )
d2 <- d[ complete.cases( d$K , d$N , d$M ) , ]

## R code 5.10
m5H3 <- quap(
    alist(
       # M -> K <- N
        K ~ dnorm( muK , sigmaK ),
        muK <- aK + bMK*M + bNK*N,
       # M -> N
        N ~ dnorm( muN , sigmaN ),
        muN <- aN + bMN*M,
       # priors
        c(aK,aN) ~ dnorm( 0 , 0.2 ),
        c(bMK,bNK,bMN) ~ dnorm( 0 , 0.5 ),
        c(sigmaK,sigmaN) ~ dexp( 1 )
    ) , data=d2 )
precis( m5H3 )

## R code 5.11
(log(c(15,30)) - mean(log(d$mass)))/sd(log(d$mass))

## R code 5.12
M_seq <- c( 0.75 , 1.15 )
sim_dat <- data.frame( M=M_seq )
s <- sim( m5H3 , data=sim_dat , vars=c("N","K") )
diff <- s$K[,2] - s$K[,1]
quantile( diff , probs=c( 0.05 , 0.5 , 0.94 ) )

M_seq <- seq( from=-3 , to=3 , length.out=30 )
sim_dat <- data.frame( M=M_seq )
s <- sim( m5H3 , data=sim_dat , vars=c("N","K") )

## R code 5.13
0.61*0.68 - 0.7

## R code 5.14
( 0.61*0.68 - 0.7 )*0.4

## R code 5.15
library(dagitty)
dag_5H4 <- dagitty("dag{ M <- A -> D; A <- S -> D; M -> D }")
coordinates(dag_5H4) <- list(
    x=c(A=0,M=1,D=2,S=1),
    y=c(A=1,M=0,D=1,S=2) )
drawdag( dag_5H4 )

## R code 5.16
impliedConditionalIndependencies( dag_5H4 )

## R code 5.17
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )
d$A <- standardize( d$MedianAgeMarriage )
d$S <- d$South

m_5H4 <- quap(
    alist(
        M ~ dnorm( mu , sigma ),
        mu <- a + bS*S + bA*A,
        a ~ dnorm( 0 , 0.2 ),
        c(bS,bA) ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp( 1 )
    ) , data=d )
precis( m_5H4 )

## R code 6.1
library(dagitty)
dag_6M1 <- dagitty("dag{
    U [unobserved]
    V [unobserved]
    X -> Y
    X <- U -> B <- C -> Y
    U <- A -> C
    C <- V -> Y }")
coordinates(dag_6M1) <- list(
    x=c(X=0,Y=2,U=0,A=1,B=1,C=2,V=2.5),
    y=c(X=2,Y=2,U=1,A=0.5,B=1.5,C=1,V=1.5) )
drawdag(dag_6M1)

## R code 6.2
adjustmentSets( dag_6M1 , exposure="X" , outcome="Y" )

## R code 6.3
N <- 1000
X <- rnorm(N)
Z <- rnorm(N,X,0.1)
Y <- rnorm(N,Z)
cor(X,Z)

## R code 6.4
m_6M2 <- quap(
    alist(
        Y ~ dnorm( mu , sigma ),
        mu <- a + bX*X + bZ*Z,
        c(a,bX,bZ) ~ dnorm(0,1),
        sigma ~ dexp(1)
    ) , data=list(X=X,Y=Y,Z=Z) )
precis( m_6M2 )

## R code 6.5
library(dagitty)
dag_6H1 <- dagitty("dag{A -> D; A -> M -> D; A <- S -> D; S -> W -> D}")
coordinates(dag_6H1) <- list(
    x=c(A=0,M=1,D=2,S=1,W=2),
    y=c(A=0.75,M=0.5,D=1,S=0,W=0) )
drawdag(dag_6H1)

## R code 6.6
impliedConditionalIndependencies( dag_6H1 )

## R code 6.7
## R code 6.8
## R code 6.9
## R code 7.1
# define probabilities of heads and tails
p <- c( 0.7 , 0.3 )

# compute entropy
-sum( p*log(p) )

## R code 7.2
# define probabilities of sides
p <- c( 0.2 , 0.25 , 0.25 , 0.3 )

# compute entropy
-sum( p*log(p) )

## R code 7.3
# define probabilities of sides
p <- c( 1/3 , 1/3 , 1/3 )

# compute entropy
-sum( p*log(p) )

## R code 7.4
y <- rnorm(10) # execute just once, to get data

# repeat this, changing sigma each time
m <- quap(
    alist(
        y ~ dnorm(mu,1),
        mu ~ dnorm(0,sigma)
    ), data=list(y=y,sigma=10) )
WAIC(m)

## R code 7.5
library(rethinking)
data(Laffer)
d <- Laffer
d$T <- standardize( d$tax_rate )
d$R <- standardize( d$tax_revenue )

# linear model
m7H1a <- quap(
    alist(
        R ~ dnorm( mu , sigma ),
        mu <- a + b*T,
        a ~ dnorm( 0 , 0.2 ),
        b ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp(1)
    ) , data=d )

# quadratic model
m7H1b <- quap(
    alist(
        R ~ dnorm( mu , sigma ),
        mu <- a + b*T + b2*T^2,
        a ~ dnorm( 0 , 0.2 ),
        c(b,b2) ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp(1)
    ) , data=d )

# cubic model
m7H1c <- quap(
    alist(
        R ~ dnorm( mu , sigma ),
        mu <- a + b*T + b2*T^2 + b3*T^3,
        a ~ dnorm( 0 , 0.2 ),
        c(b,b2,b3) ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp(1)
    ) , data=d )

## R code 7.6
compare( m7H1a , m7H1b , m7H1c , func=PSIS )

## R code 7.7
PSISk(m7H1a)

## R code 7.8
T_seq <- seq( from=-3.2 , to=1.2 , length.out=30 )
la <- link( m7H1a , data=list(T=T_seq) )
lb <- link( m7H1b , data=list(T=T_seq) )
lc <- link( m7H1c , data=list(T=T_seq) )

plot( d$T , d$R , xlab="tax rate" , ylab="revenue" )
mtext( "linear model" )
lines( T_seq , colMeans(la) )
shade( apply( la , 2 , PI ) , T_seq )

plot( d$T , d$R , xlab="tax rate" , ylab="revenue" )
mtext( "quadratic model" )
lines( T_seq , colMeans(lb) )
shade( apply( lb , 2 , PI ) , T_seq )

plot( d$T , d$R , xlab="tax rate" , ylab="revenue" )
mtext( "cubic model" )
lines( T_seq , colMeans(lc) )
shade( apply( lc , 2 , PI ) , T_seq )

## R code 7.9
num_knots <- 15
knot_list <- quantile( d$T , probs=seq(0,1,length.out=num_knots) )

library(splines)
B <- bs(d$T,
    knots=knot_list[-c(1,num_knots)] ,
    degree=3 , intercept=TRUE )

m7H1s <- quap(
    alist(
        R ~ dnorm( mu , sigma ) ,
        mu <- a + B %*% w ,
        a ~ dnorm(0,1),
        w ~ dnorm(0,1),
        sigma ~ dexp(1)
    ), data=list( R=d$R , B=B ) ,
    start=list( w=rep( 0 , ncol(B) ) ) )

## R code 7.10
compare( m7H1a , m7H1b , m7H1c , m7H1s , func=PSIS )

## R code 7.11
mu <- link( m7H1s )
mu_PI <- apply( mu , 2 , PI )
plot( d$T , d$R , xlab="tax rate" , ylab="revenue" )
mtext( "basis spline" )
o <- order( d$T )
lines( d$T[o] , colMeans(mu)[o] )
shade( mu_PI[,o] , d$T[o] )

## R code 7.12
# linear model with student-t
m7H1d <- quap(
    alist(
        R ~ dstudent( 2 , mu , sigma ),
        mu <- a + b*T,
        a ~ dnorm( 0 , 0.2 ),
        b ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp(1)
    ) , data=d )

## R code 7.13
compare( m7H1a , m7H1b , m7H1c , m7H1s , m7H1d , func=PSIS )

## R code 7.14
## R code 7.15
## R code 7.16
## R code 7.17
## R code 7.18
## R code 7.19
## R code 7.20
## R code 7.21
## R code 8.1
library(rethinking)
data(tulips)
d <- tulips
d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)

m8M4a <- quap(
    alist(
        blooms_std ~ dnorm( mu , sigma ) ,
        mu <- a + bw*water_cent - bs*shade_cent - bws*water_cent*shade_cent ,
        a ~ dnorm( 0.5 , 0.25 ) ,
        bw ~ dlnorm( 0 , 0.25 ) ,
        bs ~ dlnorm( 0 , 0.25 ) ,
        bws ~ dlnorm( 0 , 0.25 ) ,
        sigma ~ dexp( 1 )
    ) , data=d )

## R code 8.2
p <- extract.prior( m8M4a )
par(mfrow=c(1,3),cex=1.1) # 3 plots in 1 row
for ( s in -1:1 ) {
    idx <- which( d$shade_cent==s )
    plot( d$water_cent[idx] , d$blooms_std[idx] , xlim=c(-1,1) , ylim=c(0,1) ,
        xlab="water" , ylab="blooms" , pch=16 , col=rangi2 )
    mtext( concat( "shade = " , s ) )
    mu <- link( m8M4 , post=p , data=data.frame( shade_cent=s , water_cent=-1:1 ) )
    for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
}

## R code 8.3
m8M4b <- quap(
    alist(
        blooms_std ~ dnorm( mu , sigma ) ,
        mu <- a + bw*water_cent - bs*shade_cent - bws*water_cent*shade_cent ,
        a ~ dnorm( 0.5 , 0.25 ) ,
        bw ~ dlnorm( -2 , 0.25 ) ,
        bs ~ dlnorm( -2 , 0.25 ) ,
        bws ~ dlnorm( -2 , 0.25 ) ,
        sigma ~ dexp( 1 )
    ) , data=d )

## R code 8.4
library(rethinking)
data(tulips)
d <- tulips
d$bed

## R code 8.5
d$bed_b <- ifelse( d$bed=="b" , 1 , 0 )
d$bed_c <- ifelse( d$bed=="c" , 1 , 0 )

## R code 8.6
d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)
m1 <- quap(
    alist(
        blooms_std ~ dnorm( mu , sigma ),
        mu <- a + bw*water_cent + bs*shade_cent +
              bws*water_cent*shade_cent +
              b_bed_b*bed_b + b_bed_c*bed_c ,
        a ~ dnorm( 0 , 0.25 ),
        c(bw,bs,bws,b_bed_b,b_bed_c) ~ dnorm(0,0.25),
        sigma ~ dexp( 1 )
    ) , data=d )
precis(m1)

## R code 8.7
( d$bed_idx <- coerce_index( d$bed ) )

## R code 8.8
m2 <- quap(
    alist(
        blooms_std ~ dnorm( mu , sigma ),
        mu <- a[bed_idx] + bw*water_cent + bs*shade_cent +
              bws*water_cent*shade_cent ,
        a[bed_idx] ~ dnorm( 0 , 0.25 ),
        c(bw,bs,bws) ~ dnorm( 0 , 0.25 ),
        sigma ~ dexp( 1 )
    ) , data=d )
precis(m2,depth=2)

## R code 8.9
post <- extract.samples(m2)
diff_b_c <- post$a[,2] - post$a[,3]
PI( diff_b_c )

## R code 8.10
m3 <- quap(
    alist(
        blooms_std ~ dnorm( mu , sigma ),
        mu <- a + bw*water_cent + bs*shade_cent +
              bws*water_cent*shade_cent ,
        a ~ dnorm( 0 , 0.25 ),
        c(bw,bs,bws) ~ dnorm( 0 , 0.25 ),
        sigma ~ dexp( 1 )
    ) , data=d )
compare(m2,m3)

## R code 8.11
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

m8.3 <- quap(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
        a[cid] ~ dnorm( 1 , 0.1 ) ,
        b[cid] ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dexp( 1 )
    ) , data=dd )

## R code 8.12
k <- PSISk( m8.3 )
o <- order( k , decreasing=TRUE )
data.frame( country=as.character( dd$isocode ) ,
    k=k , dd$rugged_std , dd$log_gdp_std )[o,]

## R code 8.13
m8.3t <- quap(
    alist(
        log_gdp_std ~ dstudent( 2 , mu , sigma ) ,
        mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
        a[cid] ~ dnorm( 1 , 0.1 ) ,
        b[cid] ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dexp( 1 )
    ) , data=dd )

## R code 8.14
postN <- extract.samples( m8.3 )
postT <- extract.samples( m8.3t )
diffN <- postN$b[,1] - postN$b[,2]
diffT <- postT$b[,1] - postT$b[,2]
dens( diffN )
dens( diffT , add=TRUE , col="blue" )

## R code 8.15
library(rethinking)
data(nettle)
d <- nettle
d$L <- standardize( log( d$num.lang / d$k.pop ) )
d$A <- standardize( log(d$area) )
d$G <- standardize( d$mean.growing.season )

m0 <- quap(
    alist(
        L ~ dnorm(mu,sigma),
        mu <- a + 0,
        a ~ dnorm(0,0.2),
        sigma ~ dexp(1)
    ) , data=d )

m1 <- quap(
    alist(
        L ~ dnorm(mu,sigma),
        mu <- a + bG*G,
        a ~ dnorm(0,0.2),
        bG ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ) , data=d )

m2 <- quap(
    alist(
        L ~ dnorm(mu,sigma),
        mu <- a + bG*G + bA*A,
        a ~ dnorm(0,0.2),
        c(bG,bA) ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ) , data=d )

compare(m0,m1,m2)

## R code 8.16
coeftab(m1,m2)

## R code 8.17
G_seq <- seq( from=-2.5 , to=2 , length.out=30 )
new_dat <- data.frame( A=0 , G=G_seq )

mu <- link( m2 , data=new_dat )

plot( L ~ G , data=d , col="slateblue" )
lines( G_seq , colMeans(mu) )

# plot several intervals with shading
for ( p in c(0.5,0.79,0.95) ) {
mu_PI <- apply( mu , 2 , PI , prob=p )
shade( mu_PI , G_seq )
}

## R code 8.18
d$S <- standardize( d$sd.growing.season )
m4 <- quap(
    alist(
        L ~ dnorm(mu,sigma),
        mu <- a + bS*S,
        a ~ dnorm(0,0.2),
        bS ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ) , data=d )
m5 <- quap(
    alist(
        L ~ dnorm(mu,sigma),
        mu <- a + bS*S + bA*A,
        a ~ dnorm(0,0.2),
        c(bS,bA) ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ) , data=d )
compare(m0,m4,m5)

## R code 8.19
precis(m5)

## R code 8.20
m6 <- quap(
    alist(
        L ~ dnorm(mu,sigma),
        mu <- a + bG*G + bS*S + bGS*G*S,
        a ~ dnorm(0,0.2),
        c(bG,bS,bGS) ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ) , data=d )

## R code 8.21
precis(m6)

## R code 8.22
# pull out 10%, 50%, and 95% quantiles of sd.growing.season
# these values will be used to make the three plots
S_seq <- quantile(d$S,c(0.1,0.5,0.95))

# now loop over the three plots
# draw languages against mean.growing.season in each
G_seq <- seq(from=-2.5,to=2,length.out=30)
par(mfrow=c(1,3),cex=1.1) # set up plot window for row of 3 plots
for ( i in 1:3 ) {
    S_val <- S_seq[i] # select out value for this plot
    new.dat <- data.frame(
        G = G_seq,
        S = S_val )
    mu <- link( m6 , data=new.dat )
    mu.mean <- apply( mu , 2 , mean )
    mu.PI <- apply( mu , 2 , PI )

    # fade point color as function of distance from sd.val
    cols <- col.dist( d$S , S_val , 2 , "slateblue" )

    plot( L ~ G , data=d , col=cols , lwd=2 )
    mtext( paste("S =",round(S_val,2)) , 3 )
    lines( G_seq , mu.mean )
    shade( mu.PI , G_seq )
}

## R code 8.23
# pull out 10%, 50%, and 95% quantiles of mean.growing.season
G_seq <- quantile(d$G,c(0.1,0.5,0.95))

# now loop over the three plots
x.seq <- seq(from=-1.7,to=4,length.out=30)
par(mfrow=c(1,3),cex=1.1) # set up plot window for row of 3 plots
for ( i in 1:3 ) {
    G_val <- G_seq[i] # select out value for this plot
    new.dat <- data.frame(
        G = G_val ,
        S = x.seq )
    mu <- link( m6 , data=new.dat )
    mu.mean <- apply( mu , 2 , mean )
    mu.PI <- apply( mu , 2 , PI )

    # fade point color as function of distance from sd.val
    cols <- col.dist( d$G , G_val , 5 , "slateblue" )

    plot( L ~ S , data=d , col=cols , lwd=2 )
    mtext( paste("G =",round(G_val,2)) , 3 )
    lines( x.seq , mu.mean )
    shade( mu.PI , x.seq )
}

## R code 8.24
## R code 8.25
## R code 8.26
## R code 8.27
## R code 8.28
## R code 8.29
## R code 8.30
## R code 8.31
## R code 8.32
## R code 8.33
## R code 8.34
## R code 8.35
## R code 8.36
## R code 9.1
# load and rep data
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

dat_slim <- list(
    log_gdp_std = dd$log_gdp_std,
    rugged_std = dd$rugged_std,
    cid = as.integer( dd$cid ) )

# new model with uniform prior on sigma
m9.1_unif <- ulam(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
        a[cid] ~ dnorm( 1 , 0.1 ) ,
        b[cid] ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dunif( 0 , 1 )
    ) , data=dat_slim , chains=4 , cores=4 )

## R code 9.2
m9.1_exp <- ulam(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
        a[cid] ~ dnorm( 1 , 0.1 ) ,
        b[cid] ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dexp( 1 )
    ) , data=dat_slim , chains=4 , cores=4 )

## R code 9.3
curve( dexp(x,1) , from=0 , to=7 ,
    xlab="sigma" , ylab="Density" , ylim=c(0,1) )
curve( dunif(x,0,1) , add=TRUE , col="red" )
mtext( "priors" )

## R code 9.4
post <- extract.samples( m9.1_exp )
dens( post$sigma , xlab="sigma" )
post <- extract.samples( m9.1_unif )
dens( post$sigma , add=TRUE , col="red" )
mtext( "posterior" )

## R code 9.5
m9M2 <- ulam(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
        a[cid] ~ dnorm( 1 , 0.1 ) ,
        b[cid] ~ dexp( 0.3 ) ,
        sigma ~ dexp( 1 )
    ) , data=dat_slim , chains=4 , cores=4 )
precis( m9M2 , 2 )

## R code 9.6
# compile model
# use fixed start values for comparability of runs
start <- list(a=c(1,1),b=c(0,0),sigma=1)
m9.1 <- ulam(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
        a[cid] ~ dnorm( 1 , 0.1 ) ,
        b[cid] ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dexp( 1 )
    ) , start=start , data=dat_slim , chains=1 , iter=1 )

# define warmup values to run through
warm_list <- c(5,10,100,500,1000)

# first make matrix to hold n_eff results
n_eff <- matrix( NA , nrow=length(warm_list) , ncol=5 )

# loop over warm_list and collect n_eff
for ( i in 1:length(warm_list) ) {
    w <- warm_list[i]
    m_temp <- ulam( m9.1 , chains=1 ,
        iter=1000+w , warmup=w , refresh=-1 , start=start )
    n_eff[i,] <- precis(m_temp,2)$n_eff
}

# add some names to rows and cols of result
# just to make it pretty
# there's always time to make data pretty
colnames(n_eff) <- rownames(precis(m_temp,2))
rownames(n_eff) <- warm_list

## R code 9.7
N <- 100                          # number of individuals
height <- rnorm(N,10,2)           # sim total height of each
leg_prop <- runif(N,0.4,0.5)      # leg as proportion of height
leg_left <- leg_prop*height +     # sim left leg as proportion + error
    rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height +    # sim right leg as proportion + error
    rnorm( N , 0 , 0.02 )
                                  # combine into data frame
d <- data.frame(height,leg_left,leg_right)

## R code 9.8
m <- ulam(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + bl*leg_left + br*leg_right ,
        a ~ dnorm( 10 , 100 ) ,
        bl ~ dnorm( 2 , 10 ) ,
        br ~ dnorm( 2 , 10 ) ,
        sigma ~ dunif( 0 , 10 )
    ) , data=d , iter=1 )

warm_list <- c(5,10,100,500,1000)
n_eff <- matrix( NA , nrow=length(warm_list) , ncol=4 )
for ( i in 1:length(warm_list) ) {
    w <- warm_list[i]
    m_temp <- ulam( m , chains=1 ,
        iter=1000+w , warmup=w , refresh=-1 )
    n_eff[i,] <- precis(m_temp,2)$n_eff
}
colnames(n_eff) <- rownames(precis(m_temp,2))
rownames(n_eff) <- warm_list
round(n_eff,1)

## R code 9.9
precis(mp)

## R code 9.10
traceplot( mp , n_col=2 , lwd=2 )

## R code 9.11
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )
d$A <- standardize( d$MedianAgeMarriage )
d_trim <- list(D=d$D,M=d$M,A=d$A)

## R code 9.12
m5.1_stan <- ulam(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bA * A ,
        a ~ dnorm( 0 , 0.2 ) ,
        bA ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data=d_trim , chains=4 , cores=4 , log_lik=TRUE )
m5.2_stan <- ulam(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bM * M ,
        a ~ dnorm( 0 , 0.2 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data=d_trim , chains=4 , cores=4 , log_lik=TRUE )
m5.3_stan <- ulam(
    alist(
        D ~ dnorm( mu , sigma ) ,
        mu <- a + bM*M + bA*A ,
        a ~ dnorm( 0 , 0.2 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        bA ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
    ) , data=d_trim , chains=4 , cores=4 , log_lik=TRUE )

## R code 9.13
compare( m5.1_stan , m5.2_stan , m5.3_stan , func=PSIS )

## R code 9.14
compare( m5.1_stan , m5.2_stan , m5.3_stan , func=WAIC )

## R code 9.15
precis(m5.3_stan)

## R code 9.16
N <- 100                          # number of individuals
set.seed(909)
height <- rnorm(N,10,2)           # sim total height of each
leg_prop <- runif(N,0.4,0.5)      # leg as proportion of height
leg_left <- leg_prop*height +     # sim left leg as proportion + error
    rnorm( N , 0 , 0.02 )
leg_right <- leg_prop*height +    # sim right leg as proportion + error
    rnorm( N , 0 , 0.02 )
                                  # combine into data frame
d <- data.frame(height,leg_left,leg_right)

## R code 9.17
compare( m5.8s , m5.8s2 )

## R code 9.18
pop_size <- sample( 1:10 )

## R code 9.19
num_weeks <- 1e5
positions <- rep(0,num_weeks)
pop_size <- sample( 1:10 )
current <- 10
for ( i in 1:num_weeks ) {
    # record current position
    positions[i] <- current

    # flip coin to generate proposal
    proposal <- current + sample( c(-1,1) , size=1 )
    # now make sure he loops around the archipelago
    if ( proposal < 1 ) proposal <- 10
    if ( proposal > 10 ) proposal <- 1

    # move?
    prob_move <- pop_size[proposal]/pop_size[current]
    current <- ifelse( runif(1) < prob_move , proposal , current )
}

## R code 9.20
# compute frequencies
f <- table( positions )

# plot frequencies against relative population sizes
# label each point with island index
plot( as.vector(f) , pop_size , type="n" ,
    xlab="frequency" , ylab="population size" ) # empty plot
text(  , x=f , y=pop_size )

## R code 9.21
num_samples <- 1e4
p_samples <- rep(NA,num_samples)
p <- 0.5 # initialize chain with p=0.5
for ( i in 1:num_samples ) {
    # record current parameter value
    p_samples[i] <- p

    # generate a uniform proposal from -0.1 to +0.1
    proposal <- p + runif(1,-0.1,0.1)
    # now reflect off boundaries at 0 and 1
    # this is needed so proposals are symmetric
    if ( proposal < 0 ) proposal <- abs(proposal)
    if ( proposal > 1 ) proposal <- 1-(proposal-1)

    # compute posterior prob of current and proposal
    prob_current <- dbinom(6,size=9,prob=p) * dunif(p,0,1)
    prob_proposal <- dbinom(6,size=9,prob=proposal) * dunif(proposal,0,1)

    # move?
    prob_move <- prob_proposal/prob_current
    p <- ifelse( runif(1) < prob_move , proposal , p )
}

## R code 9.22
plot( p_samples , type="l" , ylab="probability water" )

## R code 9.23
dens( p_samples , xlab="probability water" )
curve( dbeta(x,7,4) , add=TRUE , col="red" )

## R code 9.24
# simulate some data
# 100 observations with mean 5 and sd 3
y <- rnorm( 100 , 5 , 3 )

# now chain to sample from posterior
num_samples <- 1e4
mu_samples <- rep(NA,num_samples)
sigma_samples <- rep(NA,num_samples)
mu <- 0
sigma <- 1
for ( i in 1:num_samples ) {
    # record current parameter values
    mu_samples[i] <- mu
    sigma_samples[i] <- sigma

    # proposal for mu
    mu_prop <- mu + runif(1,-0.1,0.1)

    # compute posterior prob of mu and mu_prop
    # this is done treating sigma like a constant
    # will do calculations on log scale, as we should
    # so log priors get added to log likelihood
    log_prob_current <- sum(dnorm(y,mu,sigma,TRUE)) +
                        dnorm(mu,0,10,TRUE) + dunif(sigma,0,10,TRUE)
    log_prob_proposal <- sum(dnorm(y,mu_prop,sigma,TRUE)) +
                        dnorm(mu_prop,0,10,TRUE) + dunif(sigma,0,10,TRUE)
    # move?
    prob_move <- exp( log_prob_proposal - log_prob_current )
    mu <- ifelse( runif(1) < prob_move , mu_prop , mu )

    # proposal for sigma
    sigma_prop <- sigma + runif(1,-0.1,0.1)
    # reflect off boundary at zero
    if ( sigma_prop < 0 ) sigma_prop <- abs(sigma_prop)

    # compute posterior probabilities
    log_prob_current <- sum(dnorm(y,mu,sigma,TRUE)) +
                        dnorm(mu,0,10,TRUE) + dunif(sigma,0,10,TRUE)
    log_prob_proposal <- sum(dnorm(y,mu,sigma_prop,TRUE)) +
                        dnorm(mu,0,10,TRUE) + dunif(sigma_prop,0,10,TRUE)
    # move?
    prob_move <- exp( log_prob_proposal - log_prob_current )
    sigma <- ifelse( runif(1) < prob_move , sigma_prop , sigma )
}

## R code 9.25
plot( mu_samples , sigma_samples , type="l" )

## R code 9.26
# y is successes, n is trials
U_globe <- function( q ) {
    U <- dbinom(y,size=n,prob=q,log=TRUE) + dunif(q,0,1,log=TRUE)
    return( -U )
}

## R code 9.27
U_globe_gradient <- function( q ) {
    G <- ( y - n*q )/( q*(1-q) )
    return( -G )
}

## R code 9.28
# data
y <- 6
n <- 9

# initialize everything
Q <- list()
Q$q <- 0.5
n_samples <- 20
plot( NULL , xlab="time" , ylab="p" , ylim=c(0,1) , xlim=c(0,n_samples) )
path_col <- col.alpha("black",0.5)
points( 0 , Q$q , pch=4 , col="black" )

step <- 0.03
L <- 10

samples <- rep(NA,n_samples)

show_trajectory <- TRUE
for ( i in 1:n_samples ) {
    Q <- HMC2( U_globe , U_globe_gradient , step , L , Q$q )
    # plot trajectory
    if ( show_trajectory==TRUE )
      for ( j in 1:L ) {
        K0 <- sum(Q$ptraj[j,]^2)/2 # kinetic energy
        tx <- (i-1) + c( 1/L*(j-1) , 1/L*j )
        lines( tx , Q$traj[j:(j+1),1] , col=path_col , lwd=1+2*K0 )
      }
    points( i , Q$traj[L+1,1] , pch=ifelse( Q$accept==1 , 16 , 1 ) )
    if ( Q$accept==1 ) samples[i] <- Q$q
}

## R code 11.1
log( 0.35 / (1-0.35) )

## R code 11.2
logistic( 3.2 )

## R code 11.3
exp( 1.7 )

## R code 11.4
library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$treatment <- 1 + d$prosoc_left + 2*d$condition
dat_list <- list(
    pulled_left = d$pulled_left,
    actor = d$actor,
    treatment = as.integer(d$treatment) )

m11.4q <- quap(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a[actor] + b[treatment] ,
        a[actor] ~ dnorm( 0 , 1.5 ),
        b[treatment] ~ dnorm( 0 , 0.5 )
    ) , data=dat_list )

## R code 11.5
pr <- precis( m11.4 , 2 )[,1:4]
prq <- precis( m11.4q , 2 )
round( cbind( pr , prq ) , 2 )

## R code 11.6
post <- extract.samples( m11.4 )
postq <- extract.samples( m11.4q )
dens( post$a[,2] , lwd=2 )
dens( postq$a[,2] , add=TRUE , lwd=2 , col=rangi2 )

## R code 11.7
m11M7q <- quap(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a[actor] + b[treatment] ,
        a[actor] ~ dnorm( 0 , 10 ),
        b[treatment] ~ dnorm( 0 , 0.5 )
    ) , data=dat_list )
m11M7u <- ulam(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a[actor] + b[treatment] ,
        a[actor] ~ dnorm( 0 , 10 ),
        b[treatment] ~ dnorm( 0 , 0.5 )
    ) , data=dat_list , cores=4 , chains=4 )

## R code 11.8
post <- extract.samples( m11M7u )
postq <- extract.samples( m11M7q )
dens( post$a[,2] , lwd=2 , ylim=c(0,0.12) )
dens( postq$a[,2] , add=TRUE , lwd=2 , col=rangi2 )

## R code 11.9
precis( glm( pulled_left ~ as.factor(actor) + as.factor(treatment) ,
    data=dat_list , family=binomial ) )

## R code 11.10
library(rethinking)
data(Kline)
d <- Kline
d$P <- standardize( log(d$population) )
d$contact_id <- ifelse( d$contact=="high" , 2 , 1 )
d2 <- d[ d$culture!="Hawaii" , ]

dat2 <- list(
    T = d2$total_tools ,
    P = d2$P ,
    cid = d2$contact_id )

m11.10b <- ulam(
    alist(
        T ~ dpois( lambda ),
        log(lambda) <- a[cid] + b[cid]*P,
        a[cid] ~ dnorm( 3 , 0.5 ),
        b[cid] ~ dnorm( 0 , 0.2 )
    ), data=dat2 , chains=4 )

## R code 11.11
precis(m11.10b,2)

## R code 11.12
library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$treatment <- 1 + d$prosoc_left + 2*d$condition
dat_list <- list(
    pulled_left = d$pulled_left,
    actor = d$actor,
    treatment = as.integer(d$treatment) )

## R code 11.13
m11.1 <- ulam(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a ,
        a ~ dnorm( 0 , 10 )
    ) , data=dat_list , chains=4 , log_lik=TRUE )
m11.2 <- ulam(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a + b[treatment] ,
        a ~ dnorm( 0 , 1.5 ),
        b[treatment] ~ dnorm( 0 , 10 )
    ) , data=dat_list , chains=4 , log_lik=TRUE  )
m11.3 <- ulam(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a + b[treatment] ,
        a ~ dnorm( 0 , 1.5 ),
        b[treatment] ~ dnorm( 0 , 0.5 )
    ) , data=dat_list , chains=4 , log_lik=TRUE )
m11.4 <- ulam(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a[actor] + b[treatment] ,
        a[actor] ~ dnorm( 0 , 1.5 ),
        b[treatment] ~ dnorm( 0 , 0.5 )
    ) , data=dat_list , chains=4 , log_lik=TRUE )

## R code 11.14
compare( m11.1 , m11.2 , m11.3 , m11.4 , func=PSIS )

## R code 11.15
library(rethinking)
library(MASS)
data(eagles)
d <- eagles
d$pirateL <- ifelse( d$P=="L" , 1 , 0 )
d$victimL <- ifelse( d$V=="L" , 1 , 0 )
d$pirateA <- ifelse( d$A=="A" , 1 , 0 )

## R code 11.16
f <- alist(
        y ~ dbinom( n , p ),
        logit(p) <- a + bP*pirateL + bV*victimL + bA*pirateA ,
        a ~ dnorm(0,1.5),
        bP ~ dnorm(0,1),
        bV ~ dnorm(0,1),
        bA ~ dnorm(0,1) )

m1 <- quap( f , data=d )

m1_stan <- ulam( f , data=d , chains=4 , log_lik=TRUE )

precis(m1)
precis(m1_stan)

## R code 11.17
f <- alist(
        y ~ dbinom( n , p ),
        logit(p) <- a + bP*pirateL + bV*victimL + bA*pirateA ,
        a ~ dnorm(0,10),
        bP ~ dnorm(0,10),
        bV ~ dnorm(0,10),
        bA ~ dnorm(0,10) )

## R code 11.18
precis(m1)
precis(m1_stan)

## R code 11.19
post <- extract.samples( m1_stan )
quantile( logistic( post$a ) , probs=c(0.055,0.5,0.945) )

## R code 11.20
quantile( logistic( post$a + post$bP ) , probs=c(0.055,0.5,0.945) )

## R code 11.21
d$psuccess <- d$y / d$n

p <- link(m1_stan)
y <- sim(m1_stan)

p.mean <- apply( p , 2 , mean )
p.PI <- apply( p , 2 , PI )
y.mean <- apply( y , 2 , mean )
y.PI <- apply( y , 2 , PI )

# plot raw proportions success for each case
plot( d$psuccess , col=rangi2 ,
    ylab="successful proportion" , xlab="case" , xaxt="n" ,
    xlim=c(0.75,8.25) , pch=16 )

# label cases on horizontal axis
axis( 1 , at=1:8 ,
    labels=c( "LAL","LAS","LIL","LIS","SAL","SAS","SIL","SIS" ) )

# display posterior predicted proportions successful
points( 1:8 , p.mean )
for ( i in 1:8 ) lines( c(i,i) , p.PI[,i] )

## R code 11.22
# plot raw counts success for each case
plot( d$y , col=rangi2 ,
    ylab="successful attempts" , xlab="case" , xaxt="n" ,
    xlim=c(0.75,8.25) , pch=16 )

# label cases on horizontal axis
axis( 1 , at=1:8 ,
    labels=c( "LAL","LAS","LIL","LIS","SAL","SAS","SIL","SIS" ) )

# display posterior predicted successes
points( 1:8 , y.mean )
for ( i in 1:8 ) lines( c(i,i) , y.PI[,i] )

## R code 11.23
m2 <- ulam(
    alist(
        y ~ dbinom( n , p ),
        logit(p) <- a + bP*pirateL + bV*victimL +
            bA*pirateA + bPA*pirateL*pirateA ,
        a ~ dnorm(0,1.5),
        bP ~ dnorm(0,1),
        bV ~ dnorm(0,1),
        bA ~ dnorm(0,1),
        bPA ~ dnorm(0,1)
    ) , data=d , chains=4 , log_lik=TRUE )

compare( m1_stan , m2 )

## R code 11.24
precis(m2)

## R code 11.25
data(salamanders)
d <- salamanders
d$C <- standardize(d$PCTCOVER)
d$A <- standardize(d$FORESTAGE)

## R code 11.26
f <- alist(
    SALAMAN ~ dpois( lambda ),
    log(lambda) <- a + bC*C,
    a ~ dnorm(0,1),
    bC ~ dnorm(0,1) )

## R code 11.27
N <- 50 # 50 samples from prior
a <- rnorm( N , 0 , 1 )
bC <- rnorm( N , 0 , 1 )
C_seq <- seq( from=-2 , to=2 , length.out=30 )
plot( NULL , xlim=c(-2,2) , ylim=c(0,20) ,
    xlab="cover (stanardized)" , ylab="salamanders" )
for ( i in 1:N )
    lines( C_seq , exp( a[i] + bC[i]*C_seq ) , col=grau() , lwd=1.5 )

## R code 11.28
bC <- rnorm( N , 0 , 0.5 )
plot( NULL , xlim=c(-2,2) , ylim=c(0,20) ,
    xlab="cover (stanardized)" , ylab="salamanders" )
for ( i in 1:N )
    lines( C_seq , exp( a[i] + bC[i]*C_seq ) , col=grau() , lwd=1.5 )

## R code 11.29
f <- alist(
    SALAMAN ~ dpois( lambda ),
    log(lambda) <- a + bC*C,
    a ~ dnorm(0,1),
    bC ~ dnorm(0,0.5) )
m1 <- ulam( f , data=d , chains=4 )
precis(m1)

## R code 11.30
plot( d$C , d$SALAMAN , col=rangi2 , lwd=2 ,
    xlab="cover (standardized)" , ylab="salamanders observed" )
C_seq <- seq( from=-2 , to=2 , length.out=30 )
l <- link( m1 , data=list(C=C_seq) )
lines( C_seq , colMeans( l ) )
shade( apply( l , 2 , PI ) , C_seq )

## R code 11.31
f2 <- alist(
    SALAMAN ~ dpois( lambda ),
    log(lambda) <- a + bC*C + bA*A,
    a ~ dnorm(0,1),
    c(bC,bA) ~ dnorm(0,0.5) )
m2 <- ulam( f2 , data=d , chains=4 )
precis(m2)

## R code 11.32
## R code 11.33
## R code 11.34
## R code 11.35
## R code 11.36
## R code 11.37
## R code 11.38
## R code 11.39
## R code 11.40
## R code 11.41
## R code 11.42
## R code 12.1
n <- c( 12, 36 , 7 , 41 )
q <- n / sum(n)
q

## R code 12.2
sum(q)

## R code 12.3
p <- cumsum(q)
p

## R code 12.4
log(p/(1-p))

## R code 12.5
plot( 1:4 , p , xlab="rating" , ylab="cumulative proportion" ,
    xlim=c(0.7,4.3) , ylim=c(0,1) , xaxt="n" )
axis( 1 , at=1:4 , labels=1:4 )

# plot gray cumulative probability lines
for ( x in 1:4 ) lines( c(x,x) , c(0,p[x]) , col="gray" , lwd=2 )

# plot blue discrete probability segments
for ( x in 1:4 )
    lines( c(x,x)+0.1 , c(p[x]-q[x],p[x]) , col="slateblue" , lwd=2 )

# add number labels
text( 1:4+0.2 , p-q/2 , labels=1:4 , col="slateblue" )

## R code 12.6
library(rethinking)
data(Hurricanes)
d <- Hurricanes
d$fmnnty_std <- ( d$femininity - mean(d$femininity) )/sd(d$femininity)
dat <- list( D=d$deaths , F=d$fmnnty_std )

# model formula - no fitting yet
f <- alist(
        D ~ dpois(lambda),
        log(lambda) <- a + bF*F,
        a ~ dnorm(1,1),
        bF ~ dnorm(0,1) )

## R code 12.7
N <- 100
a <- rnorm(N,1,1)
bF <- rnorm(N,0,1)
F_seq <- seq( from=-2 , to=2 , length.out=30 )
plot( NULL , xlim=c(-2,2) , ylim=c(0,500) ,
    xlab="name femininity (std)" , ylab="deaths" )
for ( i in 1:N ) lines( F_seq , exp( a[i] + bF[i]*F_seq ) , col=grau() , lwd=1.5 )

## R code 12.8
m1 <- ulam( f , data=dat , chains=4 , log_lik=TRUE )
precis( m1 )

## R code 12.9
# plot raw data
plot( dat$F , dat$D , pch=16 , lwd=2 ,
    col=rangi2 , xlab="femininity (std)" , ylab="deaths" )

# compute model-based trend
pred_dat <- list( F=seq(from=-2,to=1.5,length.out=30) )
lambda <- link( m1 , data=pred_dat )
lambda.mu <- apply(lambda,2,mean)
lambda.PI <- apply(lambda,2,PI)

# superimpose trend
lines( pred_dat$F , lambda.mu )
shade( lambda.PI , pred_dat$F )

# compute sampling distribution
deaths_sim <- sim(m1,data=pred_dat)
deaths_sim.PI <- apply(deaths_sim,2,PI)

# superimpose sampling interval as dashed lines
lines( pred_dat$F , deaths_sim.PI[1,] , lty=2 )
lines( pred_dat$F , deaths_sim.PI[2,] , lty=2 )

## R code 12.10
stem( PSISk( m1 ) )

## R code 12.11
library(rethinking)
data(Hurricanes)
d <- Hurricanes
d$fmnnty_std <- ( d$femininity - mean(d$femininity) )/sd(d$femininity)
dat <- list( D=d$deaths , F=d$fmnnty_std )

m2 <- ulam(
    alist(
        D ~ dgampois( lambda , scale ),
        log(lambda) <- a + bF*F,
        a ~ dnorm(1,1),
        bF ~ dnorm(0,1),
        scale ~ dexp(1)
    ), data=dat , chains=4 , log_lik=TRUE )

## R code 12.12
precis(m2)

## R code 12.13
plot(coeftab(m1,m2))

## R code 12.14
# plot raw data
plot( dat$F , dat$D , pch=16 , lwd=2 ,
    col=rangi2 , xlab="femininity (std)" , ylab="deaths" )

# compute model-based trend
pred_dat <- list( F=seq(from=-2,to=1.5,length.out=30) )
lambda <- link(m2,data=pred_dat)
lambda.mu <- apply(lambda,2,mean)
lambda.PI <- apply(lambda,2,PI)

# superimpose trend
lines( pred_dat$F , lambda.mu )
shade( lambda.PI , pred_dat$F )

# compute sampling distribution
deaths_sim <- sim(m2,data=pred_dat)
deaths_sim.PI <- apply(deaths_sim,2,PI)

# superimpose sampling interval as dashed lines
lines( pred_dat$F , deaths_sim.PI[1,] , lty=2 )
lines( pred_dat$F , deaths_sim.PI[2,] , lty=2 )

## R code 12.15
post <- extract.samples(m2)

fem <- (-1) # 1 stddev below mean
for ( i in 1:100 )
    curve( dgamma2(x,exp(post$a[i]+post$bF[i]*fem),post$scale[i]) ,
        from=0 , to=70 , xlab="mean deaths" , ylab="Density" ,
        ylim=c(0,0.19) , col=col.alpha("black",0.2) ,
        add=ifelse(i==1,FALSE,TRUE) )
mtext( concat("femininity = ",fem) )

## R code 12.16
# standardize new predictor
# add to dat from previous problem
dat$P <- standardize(d$min_pressure)

# interaction model
m12H3a <- ulam(
    alist(
        D ~ dgampois( lambda , scale ),
        log(lambda) <- a + bF*F + bP*P + bFP*F*P,
        a ~ dnorm(1,1),
        c(bF,bP,bFP) ~ dnorm(0,1),
        scale ~ dexp(1)
    ), data=dat , chains=4 )
precis( m12H3a )

## R code 12.17
P_seq <- seq(from=-3,to=2,length.out=30)

# 'masculine' storms
d_pred <- data.frame( F=-1 , P=P_seq )
lambda_m <- link( m12H3a , data=d_pred )
lambda_m.mu <- apply(lambda_m,2,mean)
lambda_m.PI <- apply(lambda_m,2,PI)

# 'feminine' storms
d_pred <- data.frame( F=1, P=P_seq )
lambda_f <- link( m12H3a , data=d_pred )
lambda_f.mu <- apply(lambda_f,2,mean)
lambda_f.PI <- apply(lambda_f,2,PI)

# now try plotting together
# will use sqrt scale for deaths,
#   to make differences easier to see
#   cannot use log scale, bc of zeros in data
#   note uses of sqrt() throughout code
plot( dat$P , sqrt(dat$D) ,
    pch=1 , lwd=2 , col=ifelse(dat$F>0,"red","dark gray") ,
    xlab="minimum pressure (std)" , ylab="sqrt(deaths)" )
lines( P_seq , sqrt(lambda_m.mu) , lty=2 )
shade( sqrt(lambda_m.PI) , P_seq )
lines( P_seq , sqrt(lambda_f.mu) , lty=1 , col="red" )
shade( sqrt(lambda_f.PI) , P_seq , col=col.alpha("red",0.2) )

## R code 12.18
identify( dat$P , sqrt(dat$D) , labels=d$name )

## R code 12.19
# standardize predictor
dat$S <- standardize(log(d$damage_norm))

m12H3b <- ulam(
    alist(
        D ~ dgampois( lambda , scale ),
        log(lambda) <- a + bF*F + bS*S + bFS*F*S,
        a ~ dnorm(1,1),
        c(bF,bS,bFS) ~ dnorm(0,1),
        scale ~ dexp(1)
    ), data=dat , chains=4 , log_lik=TRUE )
precis( m12H3b )

## R code 12.20
S_seq <- seq(from=-3.1,to=1.9,length.out=30)

# 'masculine' storms
d_pred <- data.frame( F=-1 , S=S_seq )
lambda_m <- link( m12H3b , data=d_pred )
lambda_m.mu <- apply(lambda_m,2,mean)
lambda_m.PI <- apply(lambda_m,2,PI)

# 'feminine' storms
d_pred <- data.frame( F=1 , S=S_seq )
lambda_f <- link( m12H3b , data=d_pred )
lambda_f.mu <- apply(lambda_f,2,mean)
lambda_f.PI <- apply(lambda_f,2,PI)

# plot
plot( dat$S , sqrt(dat$D) ,
    pch=1 , lwd=2 , col=ifelse(dat$F>0,"red","dark gray") ,
    xlab="normalized damage (std)" , ylab="sqrt(deaths)" )
lines( S_seq , sqrt(lambda_m.mu) , lty=2 )
shade( sqrt(lambda_m.PI) , S_seq )
lines( S_seq , sqrt(lambda_f.mu) , lty=1 , col="red" )
shade( sqrt(lambda_f.PI) , S_seq , col=col.alpha("red",0.2) )

# label some points
identify( dat$S , sqrt(dat$D) , labels=d$name )

## R code 12.21
dat$S2 <- standardize(d$damage_norm)

m12H3c <- ulam(
    alist(
        D ~ dgampois( lambda , scale ),
        log(lambda) <- a + bF*F + bS*S2 + bFS*F*S2,
        a ~ dnorm(1,1),
        c(bF,bS,bFS) ~ dnorm(0,1),
        scale ~ dexp(1)
    ), data=dat , chains=4 , log_lik=TRUE )

compare( m12H3b , m12H3c , func=PSIS )

## R code 12.22
S2_seq <- seq(from=-0.6,to=5.3,length.out=30)

# 'masculine' storms
d_pred <- data.frame( F=-1 , S2=S2_seq )
lambda_m <- link( m12H3c , data=d_pred )
lambda_m.mu <- apply(lambda_m,2,mean)
lambda_m.PI <- apply(lambda_m,2,PI)

# 'feminine' storms
d_pred <- data.frame( F=1 , S2=S2_seq )
lambda_f <- link( m12H3c , data=d_pred )
lambda_f.mu <- apply(lambda_f,2,mean)
lambda_f.PI <- apply(lambda_f,2,PI)

# plot
plot( dat$S2 , sqrt(dat$D) ,
    pch=1 , lwd=2 , col=ifelse(dat$F>0,"red","dark gray") ,
    xlab="normalized damage (std)" , ylab="sqrt(deaths)" )
lines( S2_seq , sqrt(lambda_m.mu) , lty=2 )
shade( sqrt(lambda_m.PI) , S2_seq )
lines( S2_seq , sqrt(lambda_f.mu) , lty=1 , col="red" )
shade( sqrt(lambda_f.PI) , S2_seq , col=col.alpha("red",0.2) )

# label some points
identify( dat$S2 , sqrt(dat$D) , labels=d$name )

## R code 12.23
library(rethinking)
data(Trolley)
d <- Trolley
dat <- list(
    R = d$response,
    A = d$action,
    I = d$intention,
    C = d$contact )
dat$Gid <- ifelse( d$male==1 , 1L , 2L )

## R code 12.24
dat$F <- 1L - d$male
m12H5 <- ulam(
    alist(
        R ~ dordlogit( phi , cutpoints ),
        phi <- a*F + bA[Gid]*A + bC[Gid]*C + BI*I ,
        BI <- bI[Gid] + bIA[Gid]*A + bIC[Gid]*C ,
        c(bA,bI,bC,bIA,bIC)[Gid] ~ dnorm( 0 , 0.5 ),
        a ~ dnorm( 0 , 0.5 ),
        cutpoints ~ dnorm( 0 , 1.5 )
    ) , data=dat , chains=4 , cores=4 )

## R code 12.25
precis( m12H5 )

## R code 12.26
library(rethinking)
data(Fish)
d <- Fish
str(d)

## R code 12.27
d$loghours <- log(d$hours)
m12H6a <- ulam(
    alist(
        fish_caught ~ dzipois( p , mu ),
        logit(p) <- a0 + bp0*persons + bc0*child,
        log(mu) <- a + bp*persons + bc*child + loghours,
        c(a0,a) ~ dnorm(0,1),
        c(bp0,bc0,bp,bc) ~ dnorm(0,0.5)
    ) , data=d , chains=4 )

## R code 12.28
precis(m12H6a)

## R code 12.29
zip_link <- link(m12H6a)
str(zip_link)

## R code 12.30
zeros <- rbinom(1e4,1,0.5)
obs_fish <- (1-zeros)*rpois(1e4,1)
simplehist(obs_fish)

## R code 12.31
fish_sim <- sim(m12H6a)
str(fish_sim)

## R code 12.32
# new data
pred_dat <- list(
    loghours=log(1), # note that this is zero, the baseline rate
    persons=1,
    child=0 )

# sim predictions - want expected number of fish, but must use both processes
fish_link <- link( m12H6a , data=pred_dat )

# summarize
p <- fish_link$p
mu <- fish_link$mu
( expected_fish_mean <- mean( (1-p)*mu ) )
( expected_fish_PI <- PI( (1-p)*mu ) )

## R code 12.33
## R code 12.34
## R code 12.35
## R code 12.36
## R code 13.1
curve( dnorm(x,0,1) , from=-5 , to=5 , ylab="Density" )
curve( dnorm(x,0,2) , add=TRUE , lty=2 )

## R code 13.2
## R code 13.3
## R code 13.4
## R code 13.5
## R code 13.6
## R code 13.7
m1.1c <- ulam(
    alist(
        S ~ binomial( n , p ),
        logit(p) <- a[tank],
        a[tank] ~ dcauchy( a_bar , sigma ),
        a_bar ~ normal( 0 , 1 ),
        sigma ~ exponential( 1 )
    ), data=dat , chains=4 , cores=4 ,
    log_lik=TRUE , control=list(adapt_delta=0.99) )

## R code 13.8
post1 <- extract.samples(m1.1)
a_tank1 <- apply(post1$a,2,mean)
post2 <- extract.samples(m1.1c)
a_tank2 <- apply(post2$a,2,mean)
plot( a_tank1 , a_tank2 , pch=16 , col=rangi2 ,
    xlab="intercept (Gaussian prior)" , ylab=" intercept (Cauchy prior)" )
abline(a=0,b=1,lty=2)

## R code 13.9
m1.1t <- ulam(
    alist(
        S ~ binomial( n , p ),
        logit(p) <- a[tank],
        transpars> vector[tank]:a <<- a_bar + z*sigma,
        z[tank] ~ dstudent( 2 , 0 , 1 ),
        a_bar ~ normal( 0 , 1 ),
        sigma ~ exponential( 1 )
    ), data=dat , chains=4 , cores=4 ,
    log_lik=TRUE , control=list(adapt_delta=0.99) )

## R code 13.10
postn <- extract.samples( m1.1 )
an <- colMeans( postn$a )
postc <- extract.samples( m1.1c )
ac <- colMeans( postc$a )
postt <- extract.samples( m1.1t )
at <- colMeans( postt$a )
plot( NULL , xlim=c(1,48) , ylim=range(c(an,ac,at)) ,
    xlab="tank" , ylab="intercept" )
points( 1:48 , an )
points( 1:48 , ac , pch=16 )
points( 1:48 , at , pch=2 )

## R code 13.11
library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$treatment <- 1 + d$prosoc_left + 2*d$condition
dat_list <- list(
    pulled_left = d$pulled_left,
    actor = d$actor,
    block_id = d$block,
    treatment = as.integer(d$treatment) )

m13M5 <- ulam(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a[actor] + g[block_id] + b[treatment] ,
        b[treatment] ~ dnorm( 0 , 0.5 ),
      ## adaptive priors
        a[actor] ~ dnorm( a_bar , sigma_a ),
        g[block_id] ~ dnorm( g_bar , sigma_g ),
      ## hyper-priors
        a_bar ~ dnorm( 0 , 1.5 ),
        g_bar ~ dnorm( 0 , 1.5 ),
        sigma_a ~ dexp(1),
        sigma_g ~ dexp(1)
    ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )

## R code 13.12
m13.4 <- ulam(
    alist(
        pulled_left ~ dbinom( 1 , p ) ,
        logit(p) <- a[actor] + g[block_id] + b[treatment] ,
        b[treatment] ~ dnorm( 0 , 0.5 ),
      ## adaptive priors
        a[actor] ~ dnorm( a_bar , sigma_a ),
        g[block_id] ~ dnorm( 0 , sigma_g ),
      ## hyper-priors
        a_bar ~ dnorm( 0 , 1.5 ),
        sigma_a ~ dexp(1),
        sigma_g ~ dexp(1)
    ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE )

## R code 13.13
precis(m13.4 , 2 , pars=c("a_bar","b") )
precis(m13M5 , 2 , pars=c("a_bar","b","g_bar") )

## R code 13.14
mtt <- ulam(
    alist(
        y ~ dstudent(2,mu,1),
        mu ~ dstudent(2,10,1)
    ), data=list(y=0) , chains=4 )

mnn <- ulam(
    alist(
        y ~ dnorm(mu,1),
        mu ~ dnorm(10,1)
    ), data=list(y=0) , chains=4 )

mtn <- ulam(
    alist(
        y ~ dstudent(2,mu,1),
        mu ~ dnorm(10,1)
    ), data=list(y=0) , chains=4 )

mnt <- ulam(
    alist(
        y ~ dnorm(mu,1),
        mu ~ dstudent(2,10,1)
    ), data=list(y=0) , chains=4 )

## R code 13.15
par(mfrow=c(2,2),cex=1.05)

p <- extract.samples(mtt)
dens(p$mu , xlim=c(-5,15) , ylim=c(0,0.35) , lwd=2 , col=rangi2 , xlab="posterior mu" )
mtext("tt")
curve( dstudent(0,2,x,1) , add=TRUE , lty=1 ) # lik
curve( dstudent(x,2,10,1) , add=TRUE , lty=2 ) # prior

p <- extract.samples(mnn)
dens(p$mu, xlim=c(-5,15), ylim=c(0,0.55), lwd=2 , col=rangi2, xlab="posterior mu")
mtext("nn")
curve( dnorm(0,x,1) , add=TRUE , lty=1 ) # lik
curve( dnorm(x,10,1) , add=TRUE , lty=2 ) # prior

p <- extract.samples(mnt)
dens(p$mu, xlim=c(-5,15), ylim=c(0,0.4), lwd=2 , col=rangi2, xlab="posterior mu")
mtext("nt")
curve( dnorm(0,x,1) , add=TRUE , lty=1 ) # lik
curve( dstudent(x,2,10,1) , add=TRUE , lty=2 ) # prior

p <- extract.samples(mtn)
dens(p$mu, xlim=c(-5,15), ylim=c(0,0.4), lwd=2 , col=rangi2, xlab="posterior mu")
mtext("tn")
curve( dstudent(0,2,x,1) , add=TRUE , lty=1 ) # lik
curve( dnorm(x,10,1) , add=TRUE , lty=2 ) # prior

## R code 13.16
## R code 13.17
## R code 13.18
## R code 13.19
## R code 13.20
identify( 1:nd , p_raw , labels=n_per_district )

## R code 13.21
## R code 13.22
## R code 13.23
## R code 13.24
## R code 13.25
## R code 13.26
## R code 13.27
## R code 13.28
library(rethinking)
data(Trolley)
d <- Trolley
dat <- list(
    R = d$response,
    A = d$action,
    I = d$intention,
    C = d$contact,
    Sid = coerce_index(d$story),
    id = coerce_index(d$id) )

## R code 13.29
m13H3 <- ulam(
    alist(
        R ~ dordlogit( phi , cutpoints ),
        phi <- z[id]*sigma + s[Sid] + bA*A + bC*C + BI*I ,
        BI <- bI + bIA*A + bIC*C ,
        z[id] ~ normal( 0 , 1 ),
        s[Sid] ~ normal( 0 , tau ),
        c(bA,bI,bC,bIA,bIC) ~ dnorm( 0 , 0.5 ),
        cutpoints ~ dnorm( 0 , 1.5 ),
        sigma ~ exponential(1),
        tau ~ exponential(1)
    ) , data=dat , chains=4 , cores=4 , log_lik=TRUE )

m13H3z <- ulam(
    alist(
        R ~ dordlogit( phi , cutpoints ),
        phi <- z[id]*sigma + sz[Sid]*tau + bA*A + bC*C + BI*I ,
        BI <- bI + bIA*A + bIC*C ,
        z[id] ~ normal( 0 , 1 ),
        sz[Sid] ~ normal( 0 , 1 ),
        c(bA,bI,bC,bIA,bIC) ~ dnorm( 0 , 0.5 ),
        cutpoints ~ dnorm( 0 , 1.5 ),
        sigma ~ exponential(1),
        tau ~ exponential(1)
    ) , data=dat , chains=4 , cores=4 , log_lik=TRUE )

## R code 13.30
precis(m13H3z)

## R code 13.31
precis( m1.2 )

## R code 13.32
precis( m1.3 , 2 , pars="s" )

## R code 13.33
precis( m1.4 , 2 , pars=c("bp","s") )

## R code 13.34
precis( m1.5 , 2 , pars=c("bp","s") )

## R code 13.35
post <- extract.samples( m1.5 )
quantile( post$bp[,2] - post$bp[,1] , c(0.055,0.5,0.945) )

## R code 14.1
N_individuals <- 100
N_scores_per_individual <- 10

# simulate abilities
ability <- rnorm(N_individuals,0,0.1)

# simulate observed test scores
# sigma here large relative to sigma of ability
N <- N_scores_per_individual * N_individuals
id <- rep(1:N_individuals,each=N_scores_per_individual)
score <- round( rnorm(N,ability[id],1) , 2 )

# put observable variables in a data frame
d <- data.frame(
    id = id,
    score = score )

## R code 14.2
m_nopool <- ulam(
    alist(
        score ~ dnorm(mu,sigma),
        mu <- a_id[id],
        a_id[id] ~ dnorm(0,10),
        sigma ~ dexp(1)
    ), data=d , chains=4 , log_lik=TRUE )

## R code 14.3
m_partpool <- ulam(
    alist(
        score ~ dnorm(mu,sigma),
        mu <- a + z_id[id]*sigma_id,
        z_id[id] ~ dnorm(0,1),
        a ~ dnorm(0,10),
        sigma ~ dexp(1),
        sigma_id ~ dexp(1)
    ), data=d , chains=4 , log_lik=TRUE )

## R code 14.4
compare( m_nopool , m_partpool )

## R code 14.5
precis(m_partpool)

## R code 14.6
# set up parameters of population
a <- 3.5            # average morning wait time
b <- (-1)           # average difference afternoon wait time
sigma_a <- 1        # std dev in intercepts
sigma_b <- 0.5      # std dev in slopes
rho <- (0)          # correlation between intercepts and slopes
Mu <- c( a , b )
cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix( c(sigma_a^2,cov_ab,cov_ab,sigma_b^2) , ncol=2 )

# simulate observations
N_cafes <- 20
library(MASS)
set.seed(6) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]
N_visits <- 10
afternoon <- rep(0:1,N_visits*N_cafes/2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5  # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )

# package into  data frame
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )

## R code 14.7
m14.1 <- ulam(
    alist(
        wait ~ normal( mu , sigma ),
        mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
        c(a_cafe,b_cafe)[cafe] ~ multi_normal( c(a,b) , Rho , sigma_cafe ),
        a ~ normal(5,2),
        b ~ normal(-1,0.5),
        sigma_cafe ~ exponential(1),
        sigma ~ exponential(1),
        Rho ~ lkj_corr(2)
    ) , data=d , chains=4 , cores=4 )

## R code 14.8
post <- extract.samples(m14.1)
dens( post$Rho[,1,2] , xlab="rho" )

## R code 14.9
# set up parameters of population
a <- 3.5            # average morning wait time
b <- (-1)           # average difference afternoon wait time
sigma_a <- 1        # std dev in intercepts
sigma_b <- 0.5      # std dev in slopes
rho <- (-0.7)          # correlation between intercepts and slopes
Mu <- c( a , b )
cov_ab <- sigma_a*sigma_b*rho
Sigma <- matrix( c(sigma_a^2,cov_ab,cov_ab,sigma_b^2) , ncol=2 )

# simulate observations
N_cafes <- 20
library(MASS)
set.seed(5) # used to replicate example
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]
N_visits <- 10
afternoon <- rep(0:1,N_visits*N_cafes/2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5  # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )

# package into  data frame
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )

## R code 14.10
m14M2 <- ulam(
    alist(
        wait ~ dnorm( mu , sigma ),
        mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
        a_cafe[cafe] ~ dnorm(a,sigma_alpha),
        b_cafe[cafe] ~ dnorm(b,sigma_beta),
        a ~ dnorm(0,10),
        b ~ dnorm(0,10),
        sigma ~ dexp(1),
        sigma_alpha ~ dexp(1),
        sigma_beta ~ dexp(1)
    ) , data=d , chains=4 , cores=4 )

## R code 14.11
m14.1 <- ulam(
    alist(
        wait ~ normal( mu , sigma ),
        mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
        c(a_cafe,b_cafe)[cafe] ~ multi_normal( c(a,b) , Rho , sigma_cafe ),
        a ~ normal(5,2),
        b ~ normal(-1,0.5),
        sigma_cafe ~ exponential(1),
        sigma ~ exponential(1),
        Rho ~ lkj_corr(2)
    ) , data=d , chains=4 , cores=4 )

## R code 14.12
post1 <- extract.samples(m14.1)
a1 <- apply( post1$a_cafe , 2 , mean )
b1 <- apply( post1$b_cafe , 2 , mean )

post2 <- extract.samples(m14M2)
a2 <- apply( post2$a_cafe , 2 , mean )
b2 <- apply( post2$b_cafe , 2 , mean )

plot( a1 , b1 , xlab="intercept" , ylab="slope" ,
    pch=16 , col=rangi2 , ylim=c( min(b1)-0.05 , max(b1)+0.05 ) ,
    xlim=c( min(a1)-0.1 , max(a1)+0.1 ) )
points( a2 , b2 , pch=1 )

## R code 14.13
library(rethinking)
data(UCBadmit)
d <- UCBadmit
dat_list <- list(
    admit = d$admit,
    applications = d$applications,
    male = ifelse( d$applicant.gender=="male" , 1 , 0 ),
    dept_id = rep(1:6,each=2)
)

m14M3 <- ulam(
    alist(
        admit ~ dbinom( applications , p ) ,
        logit(p) <- delta[dept_id] + bm[dept_id]*male,
        a ~ dnorm( 0 , 1.5 ) ,
        c(delta,bm)[dept_id] ~ multi_normal( c(a,bm_bar) , Rho , sigma_dept ),
        bm_bar ~ dnorm(0,1),
        sigma_dept ~ dexp(1),
        Rho ~ dlkjcorr(2)
    ) , data=dat_list , chains=4 , cores=4 )
precis( m14M3 , 3 )

## R code 14.14
m14M3nc <- ulam(
    alist(
        admit ~ dbinom( applications , p ) ,
        logit(p) <- a + v[dept_id,1] + (bm_bar + v[dept_id,2])*male,
        transpars> matrix[dept_id,2]:v <-
            compose_noncentered( sigma_dept , L_Rho , z ),
        matrix[2,dept_id]:z ~ dnorm( 0 , 1 ),
        a ~ dnorm( 0 , 1.5 ) ,
        bm_bar ~ dnorm(0,1),
        vector[2]:sigma_dept ~ dexp(1),
        cholesky_factor_corr[2]:L_Rho ~ lkj_corr_cholesky( 2 )
    ) , data=dat_list , chains=4 , cores=4 )

## R code 14.15
precis( m14M3nc , 3 )

## R code 14.16
compare( m11.11 , m14.8 )

## R code 14.17
library(rethinking)
data(Primates301)
data(Primates301_nex)
d <- Primates301
d$name <- as.character(d$name)
dstan <- d[ complete.cases( d$group_size , d$body , d$brain ) , ]
spp_obs <- dstan$name

dat_list <- list(
    N_spp = nrow(dstan),
    M = standardize(log(dstan$body)),
    B = standardize(log(dstan$brain)),
    G = standardize(log(dstan$group_size)),
    Imat = diag(nrow(dstan)) )

library(ape)
tree_trimmed <- keep.tip( Primates301_nex, spp_obs )
Rbm <- corBrownian( phy=tree_trimmed )
V <- vcv(Rbm)
Dmat <- cophenetic( tree_trimmed )

dat_list$V <- V[ spp_obs , spp_obs ]
dat_list$R <- dat_list$V / max(V)

# ordinary regression
m14M5.0 <- ulam(
    alist(
        G ~ multi_normal( mu , SIGMA ),
        mu <- a + bM*M + bB*B,
        matrix[N_spp,N_spp]: SIGMA <- Imat * sigma_sq,
        a ~ normal( 0 , 1 ),
        c(bM,bB) ~ normal( 0 , 0.5 ),
        sigma_sq ~ exponential( 1 )
    ), data=dat_list , chains=4 , cores=4 )

# Brownian motion model
m14M5.1 <- ulam(
    alist(
        G ~ multi_normal( mu , SIGMA ),
        mu <- a + bM*M + bB*B,
        matrix[N_spp,N_spp]: SIGMA <- R * sigma_sq,
        a ~ normal( 0 , 1 ),
        c(bM,bB) ~ normal( 0 , 0.5 ),
        sigma_sq ~ exponential( 1 )
    ), data=dat_list , chains=4 , cores=4 )

# OU model
dat_list$Dmat <- Dmat[ spp_obs , spp_obs ] / max(Dmat)
m14M5.2 <- ulam(
    alist(
        G ~ multi_normal( mu , SIGMA ),
        mu <- a + bM*M + bB*B,
        matrix[N_spp,N_spp]: SIGMA <- cov_GPL1( Dmat , etasq , rhosq , 0.01 ),
        a ~ normal(0,1),
        c(bM,bB) ~ normal(0,0.5),
        etasq ~ half_normal(1,0.25),
        rhosq ~ half_normal(3,0.25)
    ), data=dat_list , chains=4 , cores=4 )

## R code 14.18
plot( coeftab(m14M5.0,m14M5.1,m14M5.2) , pars=c("bM","bB") )

## R code 14.19
post <- extract.samples(m14M5.2)
plot( NULL , xlim=c(0,max(dat_list$Dmat)) , ylim=c(0,1.5) ,
    xlab="phylogenetic distance" , ylab="covariance" )

# posterior
for ( i in 1:30 )
    curve( post$etasq[i]*exp(-post$rhosq[i]*x) , add=TRUE , col=rangi2 )

# prior mean and 89% interval
eta <- abs(rnorm(1e3,1,0.25))
rho <- abs(rnorm(1e3,3,0.25))
d_seq <- seq(from=0,to=1,length.out=50)
K <- sapply( d_seq , function(x) eta*exp(-rho*x) )
lines( d_seq , colMeans(K) , lwd=2 )
shade( apply(K,2,PI) , d_seq )
text( 0.5 , 0.5 , "prior" )
text( 0.2 , 0.1 , "posterior" , col=rangi2 )

## R code 14.20
# OU model with different GP prior
m14M5.3 <- ulam(
    alist(
        G ~ multi_normal( mu , SIGMA ),
        mu <- a + bM*M + bB*B,
        matrix[N_spp,N_spp]: SIGMA <- cov_GPL1( Dmat , etasq , rhosq , 0.01 ),
        a ~ normal(0,1),
        c(bM,bB) ~ normal(0,0.5),
        etasq ~ half_normal(0.25,0.25),
        rhosq ~ half_normal(3,0.25)
    ), data=dat_list , chains=4 , cores=4 )

## R code 14.21
## R code 14.22
## R code 14.23
## R code 14.24
## R code 14.25
## R code 14.26
## R code 14.27
## R code 14.28
## R code 14.29
## R code 14.30
## R code 14.31
library(rethinking)
data(Oxboys)
d <- Oxboys
d$A <- standardize( d$age )
d$id <- coerce_index( d$Subject )

m14H4.1 <- ulam(
    alist(
        height ~ dnorm( mu , sigma ),
        mu <- a_bar + a[id] + (b_bar + b[id])*A,
        a_bar ~ dnorm(150,10),
        b_bar ~ dnorm(0,10),
        c(a,b)[id] ~ multi_normal( 0 , Rho_id , sigma_id ),
        sigma_id ~ dexp(1),
        Rho_id ~ dlkjcorr(2),
        sigma ~ dexp(1)
    ), data=d , chains=4 , cores=4 , iter=4000 )

## R code 14.32
precis( m14H4.1 , depth=2 , pars=c("a_bar","b_bar","sigma_id") )

## R code 14.33
plot( height ~ age , type="n" , data=d )
for ( i in 1:26 ) {
    h <- d$height[ d$Subject==i ]
    a <- d$age[ d$Subject==i ]
    lines( a , h , col=col.alpha("slateblue",0.5) , lwd=2 )
}

## R code 14.34
precis( m14H4.1 , depth=3 , pars="Rho_id" )

## R code 14.35
rho <- extract.samples(m14H4.1)$Rho_id[,1,2]
dens( rho , xlab="rho" , xlim=c(-1,1) )

## R code 14.36
post <- extract.samples(m14H4.1)
rho <- mean( post$Rho_id[,1,2] )
sb <- mean( post$sigma_id[,2] )
sa <- mean( post$sigma_id[,1] )
sigma <- mean( post$sigma )
a <- mean( post$a_bar )
b <- mean( post$b_bar )

## R code 14.37
S <- matrix( c( sa^2 , sa*sb*rho , sa*sb*rho , sb^2 ) , nrow=2 )
round( S , 2 )

## R code 14.38
library(MASS)
ve <- mvrnorm( 10 , c(0,0) , Sigma=S )
ve

## R code 14.39
age.seq <- seq(from=-1,to=1,length.out=9)
plot( 0 , 0 , type="n" , xlim=range(d$age) , ylim=range(d$height) ,
    xlab="age (centered)" , ylab="height" )

## R code 14.40
for ( i in 1:nrow(ve) ) {
    h <- rnorm( 9 ,
        mean=a + ve[i,1] + (b + ve[i,2])*age.seq ,
        sd=sigma )
    lines( age.seq , h , col=col.alpha("slateblue",0.5) )
}

## R code 15.1
library(rethinking)
data(milk)
d <- milk
d$neocortex.prop <- d$neocortex.perc / 100
d$logmass <- log(d$mass)
dat_list2 <- list(
    K = standardize( d$kcal.per.g ),
    P = d$neocortex.prop ,
    M = standardize( d$logmass ) )

m15M2.1 <- ulam(
    alist(
        K ~ dnorm( mu , sigma ),
        mu <- a + bP*(P-0.67) + bM*M,
        P ~ dbeta2( nu , theta ),
        nu ~ dbeta( 2 , 2 ),
        a ~ dnorm( 0 , 0.5 ),
        bM ~ dnorm( 0, 0.5 ),
        bP ~ dnorm( 0 , 10 ),
        theta ~ dexp( 1 ),
        sigma ~ dexp( 1 ),
        vector[12]:P_impute ~ uniform(0,1)
    ) , data=dat_list2 , chains=4 , cores=4 , iter=2000 )

## R code 15.2
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
dlist <- list(
    D_obs = standardize( d$Divorce ),
    D_sd = d$Divorce.SE / sd( d$Divorce ),
    M = standardize( d$Marriage ),
    A = standardize( d$MedianAgeMarriage ),
    N = nrow(d) )

m15M3.1 <- ulam(
    alist(
        D_obs ~ dnorm( D_true , D_sd*2.0 ),
        vector[N]:D_true ~ dnorm( mu , sigma ),
        mu <- a + bA*A + bM*M,
        a ~ dnorm(0,0.2),
        bA ~ dnorm(0,0.5),
        bM ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ) , data=dlist , chains=4 , cores=4 , iter=4000 )

## R code 15.3
m15M3.2 <- ulam(
    alist(
        D_obs ~ dnorm( mu + z_true*sigma , D_sd*2.0 ),
        vector[N]:z_true ~ dnorm( 0 , 1 ),
        mu <- a + bA*A + bM*M,
        a ~ dnorm(0,0.2),
        bA ~ dnorm(0,0.5),
        bM ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ) , data=dlist , chains=4 , cores=4 , iter=4000 ,
    control=list(max_treedepth=14) )

## R code 15.4
precis(m15.1) # original
precis(m15M3.2) # double standard error

## R code 15.5
N <- 500
X <- rnorm(N)
Y <- rnorm(N,X)
Z <- rnorm(N,Y)
d <- list(X=X,Y=Y,Z=Z)

## R code 15.6
m15M4 <- ulam(
    alist(
        Y ~ dnorm( mu , sigma ),
        mu <- a + bX*X + bZ*Z,
        c(a,bX,bZ) ~ dnorm(0,1),
        sigma ~ dexp(1)
    ) , data=d , chains=4 )
precis(m15M4)

## R code 15.7
post <- extract.samples(m15.9)
PrC1_mean <- colMeans( post$PrC1 )
PrC1_PI <- apply( post$PrC1 , 2 , PI )
o <- order( PrC1_mean )
plot( NULL , xlim=c(1,100) , ylim=c(0,1) , xlab="cat" , ylab="Prob cat present" )
for ( i in 1:100 ) lines( rep(i,2) , PrC1_PI[,o[i]] )
for ( i in 1:100 ) points( i , PrC1_mean[o][i] , pch=ifelse( cat[o][i]==1 , 16 , 21 ) ,
    lwd=1.5 , bg="white" )

## R code 15.8
notes <- rpois( N_houses , 10*( alpha + beta*cat ) )

## R code 15.9
N <- 100
S <- rnorm( N )
H <- rbinom( N , size=10 , inv_logit(S) )
D <- rbern( N ) # dogs completely random
Hm <- H
Hm[D==1] <- NA

## R code 15.10
obs <- which( !is.na( Hm ) )
m15M6.1 <- ulam(
    alist(
        H ~ binomial( 10 , p ),
        logit(p) <- a + bS*S,
        a ~ normal( 0 , 1.5 ),
        bS ~ normal( 0 , 0.5 )
    ) , data=list( H=Hm[obs] , S=S[obs] ) , chains=4 )
precis( m15M6.1 )

## R code 15.11
D <- ifelse( S > 0 , 1 , 0 )
Hm <- H
Hm[D==1] <- NA

## R code 15.12
obs <- which( !is.na( Hm ) )
m15M6.2 <- ulam(
    alist(
        H ~ binomial( 10 , p ),
        logit(p) <- a + bS*S,
        a ~ normal( 0 , 1.5 ),
        bS ~ normal( 0 , 0.5 )
    ) , data=list( H=Hm[obs] , S=S[obs] ) , chains=4 )
precis( m15M6.2 )

## R code 15.13
N <- 100
S <- rnorm(N)
H <- rbinom( N , size=10 , inv_logit(S) )
D <- ifelse( H < 5 , 1 , 0 )
Hm <- H; Hm[D==1] <- NA

## R code 15.14
obs <- which( !is.na( Hm ) )
m15M6.3 <- ulam(
    alist(
        H ~ binomial( 10 , p ),
        logit(p) <- a + bS*S,
        a ~ normal( 0 , 1.5 ),
        bS ~ normal( 0 , 0.5 )
    ) , data=list( H=Hm[obs] , S=S[obs] ) , chains=4 )
precis( m15M6.3 )

## R code 15.15
Hmm <- Hm
Hmm[ is.na(Hmm) ] <- (-9)

c15M6.4 <- "
data{
    int H[100];
    vector[100] S;
}
parameters{
    real a;
    real bS;
}
model{
    vector[100] p;
    bS ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 1.5 );
    for ( i in 1:100 ) {
        p[i] = a + bS * S[i];
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:100 ) {
        if ( H[i] > -1 ) H[i] ~ binomial( 10 , p[i] );
        if ( H[i] < 0 ) {
            vector[5] pv;
            for ( j in 0:4 ) pv[j+1] = binomial_lpmf( j | 10 , p[i] );
            target += log_sum_exp( pv );
        }
    }
}
"

m15M6.4 <- stan( model_code=c15M6.4 , data=list( H=Hmm , S=S ) , chains=4 )
precis( m15M6.4 )

## R code 15.16
gq_code <- "
generated quantities{
    int H_impute[100];
    for ( i in 1:100 ) {
        real p = inv_logit(a + bS * S[i]);
        if ( H[i] > -1 ) H_impute[i] = H[i];
        if ( H[i] < 0 ) {
           // compute Pr( H==j | p , H < 5 )
           vector[5] lbp;
           real Z;
           for ( j in 0:4 ) lbp[j+1] = binomial_lpmf( j | 10 , p );
           // convert to probabilities by normalizing
           Z = log_sum_exp( lbp );
           for ( j in 1:5 ) lbp[j] = exp( lbp[j] - Z );
           // generate random sample from posterior
           H_impute[i] = categorical_rng( lbp ) - 1;
        }
    }
}
"

## R code 15.17
code_new <- concat( c15M6.4 , gq_code )
m15M6.5 <- stan( model_code=code_new , data=list( H=Hmm , S=S ) , chains=4 )
post <- extract.samples( m15M6.5 )

## R code 15.18
par(mfrow=c(3,6),cex=1.05)
j <- which( is.na(Hm) )
for ( i in 1:18 ) {
    k <- H[j[i]] + 1
    col_vec <- rep( "black" , 5 )
    col_vec[k] <- "red"
    plot( table( post$H_impute[,j[i]] ) , col=col_vec , ylab="freq" , lwd=4 )
    mtext( j[i] )
}

## R code 15.19
library(rethinking)
data(elephants)
d <- elephants
str(d)

## R code 15.20
m15H1.1 <- ulam(
    alist(
        MATINGS ~ dpois(lambda),
        lambda <- exp(a)*(AGE-20)^bA,
        a ~ dnorm(0,1),
        bA ~ dnorm(0,1)
    ), data=d , chains=4 )
precis( m15H1.1 )

## R code 15.21
A_seq <- seq( from=25 , to=55 , length.out=30 )
lambda <- link( m15H1.1 , data=list(AGE=A_seq) )
lambda_mu <- apply(lambda,2,mean)
lambda_PI <- apply(lambda,2,PI)
plot( d$AGE , d$MATINGS , pch=16 , col=rangi2 ,
    xlab="age" , ylab="matings" )
lines( A_seq , lambda_mu )
shade( lambda_PI , A_seq )

## R code 15.22
d$AGE0 <- d$AGE - 20
m15H1.2 <- ulam(
    alist(
        MATINGS ~ dpois(lambda),
        lambda <- exp(a)*AGE_est[i]^bA,
        AGE0 ~ dnorm( AGE_est , 5 ),
        vector[41]:AGE_est ~ dunif( 0 , 50 ),
        a ~ dnorm(0,1),
        bA ~ dnorm(0,1)
    ), data=d , chains=4 )
precis( m15H1.2 )

## R code 15.23
post <- extract.samples(m15H1.2)
AGE_est <- apply(post$AGE_est,2,mean) + 20

# make jittered MATINGS variable
MATINGS_j <- jitter(d$MATINGS)

# observed
plot( d$AGE , MATINGS_j , pch=16 , col=rangi2 ,
    xlab="age" , ylab="matings" , xlim=c(23,55) )

# posterior means
points( AGE_est , MATINGS_j )

# line segments
for ( i in 1:nrow(d) )
    lines( c(d$AGE[i],AGE_est[i]) , rep(MATINGS_j[i],2) )

# regression trend - computed earlier
lines( A_seq , lambda_mu )

## R code 15.24
m15H2.1 <- ulam(
    alist(
        MATINGS ~ dpois(lambda),
        lambda <- exp(a)*AGE_est[i]^bA,
        AGE0 ~ dnorm( AGE_est , 10 ),
        vector[41]:AGE_est ~ dunif( 0 , 50 ),
        a ~ dnorm(0,1),
        bA ~ dnorm(0,1)
    ), data=d , chains=4 )
precis( m15H2.1 )

## R code 15.25
set.seed(100)
x <- c( rnorm(10) , NA )
y <- c( rnorm(10,x) , 100 )
d <- list(x=x,y=y)
show(d)

## R code 15.26
precis( lm(y~x,d) )

## R code 15.27
m15H3 <- ulam(
    alist(
        y ~ dnorm(mu,sigma),
        mu <- a + b*x,
        x ~ dnorm(0,1),
        c(a,b) ~ dnorm(0,100),
        sigma ~ dexp(1)
    ), data=d , chains=4 , iter=4000 ,
    control=list(adapt_delta=0.99) )
precis(m15H3)

## R code 15.28
pairs(m15H3)

## R code 15.29
post <- extract.samples(m15H3)
post_pos <- post
post_neg <- post
for ( i in 1:length(post) ) {
    post_pos[[i]] <- post[[i]][post$b>0]
    post_neg[[i]] <- post[[i]][post$b<0]
}

## R code 15.30
x_seq <- seq(from=-2.6,to=4,length.out=30)
mu_link <- function(x,post) post$a + post$b*x
mu <- sapply( x_seq , mu_link , post=post_pos )
mu_mu <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)

## R code 15.31
x_impute <- mean(post_pos$x_impute)
plot( y ~ x , d , pch=16 , col=rangi2 , xlim=c(-0.85,x_impute) )
points( x_impute , 100 )
lines( x_seq , mu_mu )
shade( mu_PI , x_seq )

## R code 15.32
x_seq <- seq(from=-4,to=4,length.out=50)
mu <- sapply( x_seq , mu_link , post=post_neg )
mu_mu <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
x_impute <- mean(post_neg$x_impute)
plot( y ~ x , d , pch=16 , col=rangi2 , xlim=c(-3.7,0.9) )
points( x_impute , 100 )
lines( x_seq , mu_mu )
shade( mu_PI , x_seq )

## R code 15.33
## R code 15.34
## R code 15.35
## R code 15.36
## R code 15.37
## R code 15.38
## R code 15.39
## R code 15.40
## R code 15.41
## R code 15.42
## R code 15.43
## R code 15.44
## R code 15.45
## R code 15.46
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
dlist <- list(
    D_obs = standardize( d$Divorce ),
    D_sd = d$Divorce.SE / sd( d$Divorce ),
    M_obs = standardize( d$Marriage ),
    M_sd = d$Marriage.SE / sd( d$Marriage ),
    A = standardize( d$MedianAgeMarriage ),
    N = nrow(d) )

## R code 15.47
m15H6.1 <- ulam(
    alist(
        D_obs ~ dnorm( D_true , D_sd ),
        vector[N]:D_true ~ dnorm( mu , sigma ),
        mu <- a + bA*A + bM*M_true[i],
        M_obs ~ dnorm( M_true , M_sd ),

        vector[N]:M_true ~ dnorm( muM , sigmaM ),
        muM <- aM + bAM*A,

        c(a,aM) ~ dnorm(0,0.2),
        c(bA,bM,bAM) ~ dnorm(0,0.5),
        sigma ~ dexp( 1 ),
        sigmaM ~ dexp( 1 )
    ) , data=dlist , chains=4 , cores=4 )

## R code 15.48
precis( m15H6.1 )

## R code 15.49
precis( m15.2 )

## R code 15.50
post1 <- extract.samples(m15H6.1)
post2 <- extract.samples(m15.2)
Mtrue1 <- colMeans( post1$M_true )
Mtrue2 <- colMeans( post2$M_true )
plot( Mtrue2 , Mtrue1 , xlab="M_true (m15.2)" , ylab="M_true (m15H6.1)" )
abline( a=0 , b=1 , lty=2 )

## R code 15.51
identify( Mtrue2 , Mtrue1 , d$Loc )

## R code 15.52
lstates <- c("DC","DE","HI")
plot( dlist$A , dlist$M_obs , pch=ifelse(d$Loc %in% lstates,16,1) )

## R code 15.53
y <- c( 18 , 19 , 22 , NA , NA , 19 , 20 , 22 )

## R code 15.54
sum(y,na.rm=TRUE)

## R code 15.55
library(gtools)
p <- rdirichlet( 1e3 , alpha=rep(4,8) )
plot( NULL , xlim=c(1,8) , ylim=c(0,0.3) , xlab="outcome" , ylab="probability" )
for ( i in 1:10 ) lines( 1:8 , p[i,] , type="b" , col=grau() , lwd=2 )

## R code 15.56
twicer <- function( p ) {
    o <- order( p )
    if ( p[o][8]/p[o][1] > 2 ) return( TRUE ) else return( FALSE )
}
sum( apply( p , 1 , twicer ) )

## R code 15.57
p <- rdirichlet( 1e3 , alpha=rep(50,8) )
sum( apply( p , 1 , twicer ) )

## R code 15.58
plot( NULL , xlim=c(1,8) , ylim=c(0,0.3) , xlab="outcome" , ylab="probability" )
for ( i in 1:10 ) lines( 1:8 , p[i,] , type="b" , col=grau() , lwd=2 )

## R code 15.59
code15H7 <- '
data{
    int N;
    int y[N];
    int y_max; // consider at most this many spins for y4 and y5
    int S_mean;
}
parameters{
    simplex[N] p;   // probabilities of each outcome
}
model{
    vector[(1+y_max)*(1+y_max)] terms;
    int k = 1;

    p ~ dirichlet( rep_vector(50,N) );

    // loop over possible values for unknown cells 4 and 5
    // this code updates posterior of p
    for ( y4 in 0:y_max ) {
        for ( y5 in 0:y_max ) {
            int Y[N] = y;
            Y[4] = y4;
            Y[5] = y5;
            terms[k] = poisson_lpmf(y4+y5|S_mean-120) + multinomial_lpmf(Y|p);
            k = k + 1;
        }//y5
    }//y4
    target += log_sum_exp(terms);
}
generated quantities{
    matrix[y_max+1,y_max+1] P45; // prob y4,y5 takes joint values
    // now compute Prob(y4,y5|p)
    {
        matrix[(1+y_max),(1+y_max)] terms;
        int k = 1;
        real Z;
        for ( y4 in 0:y_max ) {
            for ( y5 in 0:y_max ) {
              int Y[N] = y;
              Y[4] = y4;
              Y[5] = y5;
              terms[y4+1,y5+1] = poisson_lpmf(y4+y5|S_mean-120) + multinomial_lpmf(Y|p);
            }//y5
        }//y4
        Z = log_sum_exp( to_vector(terms) );
        for ( y4 in 0:y_max )
            for ( y5 in 0:y_max )
                P45[y4+1,y5+1] = exp( terms[y4+1,y5+1] - Z );
    }
}
'

## R code 15.60
y <- c(18,19,22,-1,-1,19,20,22)
dat <- list(
    N = length(y),
    y = y,
    S_mean = 160,
    y_max = 40 )

## R code 15.61
m15H7 <- stan( model_code=code15H7 , data=dat , chains=4 , cores=4 )
post <- extract.samples(m15H7)
y_max <- dat$y_max
plot( NULL , xlim=c(10,y_max-10) , ylim=c(10,y_max-10) ,
    xlab="number of 4s" , ylab="number of 5s" )
mtext( "posterior distribution of 4s and 5s" )
for ( y4 in 0:y_max ) for ( y5 in 0:y_max ) {
    k <- grau( mean( post$P45[,y4+1,y5+1] )/0.01 )
    points( y4 , y5 , col=k , pch=16 , cex=1.5 )
}

## R code 16.1
m16M1.1 <- ulam(
    alist(
        w ~ dlnorm( mu , sigma ),
        exp(mu) <- 3.141593 * k * p^2 * h^a,
        p ~ beta( 2 , 18 ),
        k ~ exponential( 0.5 ),
        sigma ~ exponential( 1 ),
        a ~ exponential( 1 )
    ), data=d , chains=4 , cores=4 )
precis( m16M1.1 )

## R code 16.2
h_seq <- seq( from=0 , to=max(d$h) , length.out=30 )
w_sim <- sim( m16M1.1 , data=list(h=h_seq) )
mu_mean <- apply( w_sim , 2 , mean )
w_CI <- apply( w_sim , 2 , PI )
plot( d$h , d$w , xlim=c(0,max(d$h)) , ylim=c(0,max(d$w)) , col=rangi2 ,
    lwd=2 , xlab="height (scaled)" , ylab="weight (scaled)" )
lines( h_seq , mu_mean )
shade( w_CI , h_seq )

## R code 16.3
N <- 50
p <- rbeta( N , 2 , 18 )
k <- rexp( N , 0.5 )
sigma <- rexp( N , 1 )
prior <- list( p=p , k=k , sigma=sigma )

## R code 16.4
h_seq <- seq( from=0 , to=max(d$h) , length.out=30 )
plot( d$h , d$w , xlim=c(0,max(d$h)) , ylim=c(0,max(d$w)) , col=rangi2 ,
    lwd=2 , xlab="height (scaled)" , ylab="weight (scaled)" )
for ( i in 1:N ) {
    with( prior ,
        curve( 3.141593 * k[i] * p[i]^2 * x^3 ,
            add=TRUE , lwd=1.5 , col=grau() ) )
}

## R code 16.5
N <- 50
p <- rbeta( N , 4 , 18 )
k <- rexp( N , 1/4 )
sigma <- rexp( N , 1 )
prior <- list( p=p , k=k , sigma=sigma )

## R code 16.6
N <- 50
p <- rbeta( N , 4 , 18 )
k <- rlnorm( N , log(7) , 0.2 )
sigma <- rexp( N , 1 )
prior <- list( p=p , k=k , sigma=sigma )

## R code 16.7
N <- 12
theta <- matrix( NA , nrow=N , ncol=4 )
theta[,1] <- rnorm( N , 1 , 0.5 )
theta[,3] <- rnorm( N , 1 , 0.5 )
theta[,2] <- rnorm( N , 0.05 , 0.05 )
theta[,4] <- rnorm( N , 0.05 , 0.05 )

theta[,1] <- rnorm( N , 0.5 , 0.05 )
theta[,3] <- rnorm( N , 0.025 , 0.05 )
theta[,2] <- rnorm( N , 0.05 , 0.05 )
theta[,4] <- rnorm( N , 0.5 , 0.05 )

## R code 16.8
par(mfrow=c(4,3),cex=1.05)
par(mgp = c(1, 0.2, 0), mar = c(1, 2, 1, 1) + 0.1,
        tck = -0.02, cex.axis = 0.8, bty = "l" )

for ( s in 1:N ) {
    z <- sim_lynx_hare( 1e4 , as.numeric(Lynx_Hare[1,2:3]) , theta[s,] )
    ymax <- max(z)
    if ( ymax == Inf ) ymax <- 1.79e+300
    plot( NULL , ylim=c(0,ymax) , xlim=c(0,1e4) , xaxt="n" , ylab="" , xlab="" )
    lines( z[,2] , col="black" , lwd=2 )
    lines( z[,1] , col=rangi2 , lwd=2 )
}

## R code 16.9
N <- 12
theta <- matrix( NA , nrow=N , ncol=4 )
theta[,1] <- rnorm( N , 0.5 , 0.1 )
theta[,3] <- rnorm( N , 0.025 , 0.05 )
theta[,2] <- rnorm( N , 0.05 , 0.05 )
theta[,4] <- rnorm( N , 0.5 , 0.1 )

## R code 16.10
m16M4.1 <- ulam(
    alist(
        w ~ dlnorm( mu , sigma ),
        exp(mu) <- k * 3.141593/6 * h^3,
        k ~ exponential( 0.5 ),
        sigma ~ exponential( 1 )
    ), data=d , chains=4 , cores=4 )
precis( m16M4.1 )

## R code 16.11
h_seq <- seq( from=0 , to=max(d$h) , length.out=30 )
w_sim <- sim( m16M4.1 , data=list(h=h_seq) )
mu_mean <- apply( w_sim , 2 , mean )
w_CI <- apply( w_sim , 2 , PI )
plot( d$h , d$w , xlim=c(0,max(d$h)) , ylim=c(0,max(d$w)) , col=rangi2 ,
    lwd=2 , xlab="height (scaled)" , ylab="weight (scaled)" )
lines( h_seq , mu_mean )
shade( w_CI , h_seq )

## R code 16.12
library(rethinking)
data(Panda_nuts)
dat_list <- list(
    n = as.integer( Panda_nuts$nuts_opened ),
    age = Panda_nuts$age / max(Panda_nuts$age),
    seconds = Panda_nuts$seconds )

## R code 16.13
dat_list$male_id <- ifelse( Panda_nuts$sex=="m" , 1L , 0L )

## R code 16.14
m16H1.1 <- ulam(
    alist(
        n ~ poisson( lambda ),
        lambda <- seconds*(1+pm*male_id)*phi*(1-exp(-k*age))^theta,
        phi ~ lognormal( log(1) , 0.1 ),
        pm ~ exponential( 2 ),
        k ~ lognormal( log(2) , 0.25 ),
        theta ~ lognormal( log(5) , 0.25 )
    ), data=dat_list , chains=4 )
precis(m16H1.1)

## R code 16.15
post <- extract.samples(m16H1.1)
plot( NULL , xlim=c(0,1) , ylim=c(0,1.5) , xlab="age" ,
    ylab="nuts per second" , xaxt="n" )
at <- c(0,0.25,0.5,0.75,1,1.25,1.5)
axis( 1 , at=at , labels=round(at*max(Panda_nuts$age)) )

pts <- dat_list$n / dat_list$seconds
point_size <- normalize( dat_list$seconds )
points( jitter(dat_list$age) , pts , lwd=2 , cex=point_size*3 ,
    col=ifelse(dat_list$male_id==1,"black","red") )

# 10 female curves
for ( i in 1:10 ) with( post ,
    curve( phi[i]*(1-exp(-k[i]*x))^theta[i] , add=TRUE , col="red" ) )

# 10 male curves
for ( i in 1:10 ) with( post ,
    curve( (1+pm[i])*phi[i]*(1-exp(-k[i]*x))^theta[i] , add=TRUE , col=grau() ) )

## R code 16.16
dat_list$id <- Panda_nuts$chimpanzee

## R code 16.17
m16H2.1 <- ulam(
    alist(
        n ~ poisson( lambda ),
        lambda <- seconds*(phi*z[id]*tau)*(1-exp(-k*age))^theta,
        phi ~ lognormal( log(1) , 0.1 ),
        z[id] ~ exponential( 1 ),
        tau ~ exponential(1),
        k ~ lognormal( log(2) , 0.25 ),
        theta ~ lognormal( log(5) , 0.25 ),
        gq> vector[id]:zz <<- z*tau # rescaled
    ), data=dat_list , chains=4 , cores=4 ,
    control=list(adapt_delta=0.99) , iter=4000 )
precis(m16H2.1)

## R code 16.18
plot( precis( m16H2.1 , 2 , pars="zz" ) )
abline( v=1 , lty=2 )

## R code 16.19
m16H2.2 <- ulam(
    alist(
        n ~ poisson( lambda ),
        lambda <- seconds*exp(phi+z[id]*tau)*(1-exp(-k*age))^theta,
        phi ~ normal( 0 , 0.5 ),
        z[id] ~ normal( 0 , 1 ),
        tau ~ exponential(1),
        k ~ lognormal( log(2) , 0.25 ),
        theta ~ lognormal( log(5) , 0.25 ),
        gq> vector[id]:zz <<- z*tau # rescaled
    ), data=dat_list , chains=4 , cores=4 ,
    control=list(adapt_delta=0.99) , iter=4000 )
precis(m16H2.2)

## R code 16.20
plot( precis( m16H2.2 , 2 , pars="zz" ) )
abline( v=0 , lty=2 )

## R code 16.21
data(Lynx_Hare)
dat_ar1 <- list(
    lynx = Lynx_Hare$Lynx[2:21],
    lynx_lag1 = Lynx_Hare$Lynx[1:20],
    hare = Lynx_Hare$Hare[2:21],
    hare_lag1 = Lynx_Hare$Hare[1:20] )

m16H3.1 <- ulam(
    alist(
        hare ~ lognormal( log(muh) , sigmah ),
        lynx ~ lognormal( log(mul) , sigmal ),
        muh <- ah + phi_hh*hare_lag1 + phi_hl*lynx_lag1,
        mul <- al + phi_ll*lynx_lag1 + phi_lh*hare_lag1,
        c(ah,al) ~ normal(0,1),
        phi_hh ~ normal(1,0.5),
        phi_hl ~ normal(-1,0.5),
        phi_ll ~ normal(1,0.5),
        phi_lh ~ normal(1,0.5),
        c(sigmah,sigmal) ~ exponential(1)
    ) , data=dat_ar1 , chains=4 , cores=4 )
precis( m16H3.1 )

## R code 16.22
post <- extract.samples(m16H3.1)
plot( dat_ar1$hare , pch=16 , xlab="Year" , ylab="pelts (thousands)" , ylim=c(0,100) )
points( dat_ar1$lynx , pch=16 , col=rangi2 )
mu <- link(m16H3.1)
for ( s in 1:21 ) {
    lines( 1:20 , mu$muh[s,] , col=col.alpha("black",0.2) , lwd=2 )
    lines( 1:20 , mu$mul[s,] , col=col.alpha(rangi2,0.4) , lwd=2 )
}

## R code 16.23
m16H3.2 <- ulam(
    alist(
        hare ~ lognormal( log(muh) , sigmah ),
        lynx ~ lognormal( log(mul) , sigmal ),
        muh <- ah + phi_hh*hare_lag1 + phi_hl*lynx_lag1*hare_lag1,
        mul <- al + phi_ll*lynx_lag1 + phi_lh*hare_lag1*lynx_lag1,
        c(ah,al) ~ normal(0,1),
        phi_hh ~ normal(1,0.5),
        phi_hl ~ normal(-1,0.5),
        phi_ll ~ normal(1,0.5),
        phi_lh ~ normal(1,0.5),
        c(sigmah,sigmal) ~ exponential(1)
    ) , data=dat_ar1 , chains=4 , cores=4 )

## R code 16.24
m16H3.3 <- ulam(
    alist(
        hare ~ lognormal( log(muh) , sigmah ),
        lynx ~ lognormal( log(mul) , sigmal ),
        muh <- phi_hh*hare_lag1 + phi_hl*lynx_lag1*hare_lag1,
        mul <- phi_ll*lynx_lag1 + phi_lh*hare_lag1*lynx_lag1,
        phi_hh ~ normal(1,0.5),
        phi_hl ~ normal(-1,0.5),
        phi_ll ~ normal(1,0.5),
        phi_lh ~ normal(1,0.5),
        c(sigmah,sigmal) ~ exponential(1)
    ) , data=dat_ar1 , chains=4 , cores=4 )

## R code 16.25
dat_ar2 <- list(
    lynx = Lynx_Hare$Lynx[3:21],
    lynx_lag1 = Lynx_Hare$Lynx[2:20],
    lynx_lag2 = Lynx_Hare$Lynx[1:19],
    hare = Lynx_Hare$Hare[3:21],
    hare_lag1 = Lynx_Hare$Hare[2:20],
    hare_lag2 = Lynx_Hare$Hare[1:19] )

m16H4.1 <- ulam(
    alist(
        hare ~ lognormal( log(muh) , sigmah ),
        lynx ~ lognormal( log(mul) , sigmal ),
        muh <- ah + phi_hh*hare_lag1 + phi_hl*lynx_lag1 +
                    phi2_hh*hare_lag2 + phi2_hl*lynx_lag2,
        mul <- al + phi_ll*lynx_lag1 + phi_lh*hare_lag1 +
                    phi2_ll*lynx_lag2 + phi2_lh*hare_lag2,
        c(ah,al) ~ normal(0,1),
        phi_hh ~ normal(1,0.5),
        phi_hl ~ normal(-1,0.5),
        phi_ll ~ normal(1,0.5),
        phi_lh ~ normal(1,0.5),
        phi2_hh ~ normal(0,0.5),
        phi2_hl ~ normal(0,0.5),
        phi2_ll ~ normal(0,0.5),
        phi2_lh ~ normal(0,0.5),
        c(sigmah,sigmal) ~ exponential(1)
    ) , data=dat_ar2 , chains=4 , cores=4 )
precis( m16H4.1 )

## R code 16.26
plot( dat_ar2$hare , pch=16 , xlab="Year" , ylab="pelts (thousands)" , ylim=c(0,100) )
points( dat_ar2$lynx , pch=16 , col=rangi2 )
mu <- link(m16H4.1)
for ( s in 1:21 ) {
    lines( 1:19 , mu$muh[s,] , col=col.alpha("black",0.2) , lwd=2 )
    lines( 1:19 , mu$mul[s,] , col=col.alpha(rangi2,0.4) , lwd=2 )
}

## R code 16.27
m16H4.2 <- ulam(
    alist(
        hare ~ lognormal( log(muh) , sigmah ),
        lynx ~ lognormal( log(mul) , sigmal ),
        muh <- ah + phi_hh*hare_lag1 + phi_hl*lynx_lag1*hare_lag1 +
                    phi2_hh*hare_lag2 + phi2_hl*lynx_lag2*hare_lag2,
        mul <- al + phi_ll*lynx_lag1 + phi_lh*hare_lag1*lynx_lag1 +
                    phi2_ll*lynx_lag2 + phi2_lh*hare_lag2*lynx_lag2,
        c(ah,al) ~ normal(0,1),
        phi_hh ~ normal(1,0.5),
        phi_hl ~ normal(-1,0.5),
        phi_ll ~ normal(1,0.5),
        phi_lh ~ normal(1,0.5),
        phi2_hh ~ normal(0,0.5),
        phi2_hl ~ normal(0,0.5),
        phi2_ll ~ normal(0,0.5),
        phi2_lh ~ normal(0,0.5),
        c(sigmah,sigmal) ~ exponential(1)
    ) , data=dat_ar2 , chains=4 , cores=4 )

## R code 16.28
library(rethinking)
data(Mites)

plot( Mites$day , Mites$prey )
points( Mites$day , Mites$predator , pch=16 )

## R code 16.29
sim_mites <- function( n_steps , init , theta , dt=0.002 ) {
    L <- rep(NA,n_steps)
    H <- rep(NA,n_steps)
    L[1] <- init[1]
    H[1] <- init[2]
    for ( i in 2:n_steps ) {
        L[i] <- L[i-1] + dt*L[i-1]*( theta[3]*H[i-1] - theta[4] )
        H[i] <- H[i-1] + dt*H[i-1]*( theta[1] - theta[2]*L[i-1] )
    }
    return( cbind(L,H) )
}

## R code 16.30
N <- 16
theta <- matrix( NA , N , 4 )
theta[,1] <- rnorm( N , 1.5 , 1 )
theta[,2] <- rnorm( N , 0.005 , 0.1 )
theta[,3] <- rnorm( N , 0.0005 , 0.1 )
theta[,4] <- rnorm( N , 0.5 , 1 )

par(mfrow=c(4,4),cex=1.05)
par(mgp = c(1, 0.2, 0), mar = c(1, 2, 1, 1) + 0.1,
        tck = -0.02, cex.axis = 0.8, bty = "l" )

for ( i in 1:N ) {
    z <- sim_mites( n_steps=1e4 , init=as.numeric(Mites[1,3:2]) , theta=theta[i,] )
    # preds in blue, prey in black
    ymax <- max(z)
    if ( ymax == Inf ) ymax <- 1.79e+300
    plot( NULL , ylim=c(0,ymax) , xlim=c(0,1e4) , xaxt="n" , ylab="" , xlab="" )
    lines( z[,2] , col="black" , lwd=2 )
    lines( z[,1] , col=rangi2 , lwd=2 )
    mtext( "time" , 1 )
}

## R code 16.31
dat_mites <- list(
    N = nrow(Mites),
    mites = as.matrix( Mites[,3:2] ),
    days = Mites[,1]/7 )

m16H5.1 <- stan( file="Mites.stan" , data=dat_mites , chains=4 , cores=4 , iter=2000 ,
    control=list( adapt_delta=0.99 ) )

precis(m16H5.1,2)

## R code 16.32
post <- extract.samples(m16H5.1)

mites <- dat_mites$mites
plot( dat_mites$days , mites[,2] , pch=16 , ylim=c(0,3000) ,
    xlab="time (week)" , ylab="mites" )
points( dat_mites$days , mites[,1] , col=rangi2 , pch=16 )

for ( s in 1:21 ) {
    lines( dat_mites$days , post$pop[s,,2] , col=col.alpha("black",0.2) , lwd=2 )
    lines( dat_mites$days , post$pop[s,,1] , col=col.alpha(rangi2,0.3) , lwd=2 )
}

