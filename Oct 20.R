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
  cid = as.integer( dd$cid )
)
str(dat_slim)

m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dat_slim , chains=1 )

precis(m9.1,2)

pairs(m9.1)
plot(m9.1)
traceplot(m9.1)

p<-extract.samples(m9.1)
plot(density(p$b[,2]))

m9.2 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dat_slim , chains=3,cores=3 )

traceplot(m9.2)

usecores <- parallel::detectCores() - 1

library(brms)

m9brm<-brm(log_gdp_std ~ cid+ cid:rugged_std,data=dat_slim,chains=3,cores=3)
#correct model below
dat_slim$cid<-as.factor(dat_slim$cid)
m92brm<-brm(log_gdp_std ~ -1 + cid+ cid:rugged_std,data=dat_slim,chains=3,cores=3)
plot(m9brm)
pairs(m9brm)

summary(m9brm)

priors<-get_prior(log_gdp_std ~ cid+ cid:rugged_std,data=dat_slim)
prior<-c(prior(normal(1,0.1), class = b, coef = cid),
        prior(normal(1,2), class = b, coef = cid:rugged_std)      )
m9brm<-brm(log_gdp_std ~ cid+ cid:rugged_std,data=dat_slim,chains=3,cores=3,prior=prior)


#9H1
mp <- ulam(alist(
              a ~ dnorm(0,1),
              b ~ dcauchy(0,1)
            ), data=list(y=1) , chains=1 )

traceplot(mp)
p<-extract.samples(mp)
hist(p$a)
hist(p$b)

#9H2
data(WaffleDivorce)
d <- WaffleDivorce
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )
d$A <- standardize( d$MedianAgeMarriage )
d_trim <- list(D=d$D,M=d$M,A=d$A)

m5.1_stan <- ulam(alist(
                     D ~ dnorm( mu , sigma ) ,
                     mu <- a + bA * A ,
                     a ~ dnorm( 0 , 0.2 ) ,
                     bA ~ dnorm( 0 , 0.5 ) ,
                     sigma ~ dexp( 1 )
                   ) , data=d_trim , chains=4 , cores=4 , log_lik=TRUE )

m5.2_stan <- ulam(alist(D ~ dnorm( mu , sigma ) ,
        mu <- a + bM * M ,
        a ~ dnorm( 0 , 0.2 ) ,
        bM ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dexp( 1 )
  ) , data=d_trim , chains=4 , cores=4 , log_lik=TRUE )

m5.3_stan <- ulam(alist(D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=d_trim , chains=4 , cores=4 , log_lik=TRUE )

compare( m5.1_stan , m5.2_stan , m5.3_stan , func=PSIS )
compare( m5.1_stan , m5.2_stan , m5.3_stan , func=WAIC )