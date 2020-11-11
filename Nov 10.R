library(rethinking)
a_bar <- 1.5
sigma <- 1.5
nponds <- 60
Ni <- as.integer( rep( c(5,10,25,35) , each=15 ) )

set.seed(5005)
a_pond <- rnorm( nponds , mean=a_bar , sd=sigma )
dsim <- data.frame( pond=1:nponds , Ni=Ni , true_a=a_pond )

dsim$Si <- rbinom( nponds , prob=logistic(dsim$true_a) , size=dsim$Ni )

#13.3.2 I disagree with Richard. Using random effect for treatment implies they come from some distribuiton of treatments, rather than discrete conditions.


library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$treatment <- 1 + d$prosoc_left + 2*d$condition
dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  block_id = d$block,
  treatment = as.integer(d$treatment) )


set.seed(13)
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


set.seed(13)
m13.4nc <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a_bar + z[actor]*sigma_a + # actor intercepts
      x[block_id]*sigma_g + # block intercepts
      b[treatment] ,
    b[treatment] ~ dnorm( 0 , 0.5 ),
    z[actor] ~ dnorm( 0 , 1 ),
    x[block_id] ~ dnorm( 0 , 1 ),
    a_bar ~ dnorm( 0 , 1.5 ),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1),
    gq> vector[actor]:a <<- a_bar + z*sigma_a,
    gq> vector[block_id]:g <<- x*sigma_g
  ) , data=dat_list , chains=4 , cores=4 )


library(brms)
get_prior(pulled_left~(1|actor)+(1|block_id) + treatment,data=dat_list)
pri<-c(prior(exponential(1), class = "sd"),
       prior(normal(0,1),class="b",coef="treatment") )
model<-brm(pulled_left~(1|actor)+(1|block_id) + treatment,data=dat_list,prior=pri)


#Get variance from random effects
ParasitesV<-brm_multiple(Parasites ~ 1 + (1|Family) + (1|Village), data=dsets, family=gaussian(), chains=4,cores=4,control = list(adapt_delta = 0.99, max_treedepth=12))

unlist(VarCorr(ParasitesV))[c(5,1,9)]^2/sum(unlist(VarCorr(ParasitesV))[c(5,1,9)]^2)

#cross-classified vs hierarchical

ParasitesV<-brm_multiple(Parasites ~ 1 + (1|Family) + (1|Village), data=dsets, family=gaussian(), chains=4,cores=4,control = list(adapt_delta = 0.99, max_treedepth=12))


ParasitesV<-brm_multiple(Parasites ~ 1 + (1|Family/Village), data=dsets, family=gaussian(), chains=4,cores=4,control = list(adapt_delta = 0.99, max_treedepth=12))


library(rethinking)
data(reedfrogs)
d <- reedfrogs
dat <- list(
  S = d$surv,
  n = d$density,
  tank = 1:nrow(d),
  pred = ifelse( d$pred=="no" , 0L , 1L ),
  size_ = ifelse( d$size=="small" , 1L , 2L )
)


m1.1 <- ulam( alist(
                S ~ binomial( n , p ),
                logit(p) <- a[tank],
                a[tank] ~ normal( a_bar , sigma ),
                a_bar ~ normal( 0 , 1.5 ),
                sigma ~ exponential( 1 )
              ), data=dat , chains=4 , cores=4 , log_lik=TRUE )

# pred 13.4
m1.2 <- ulam(
  alist(
    S ~ binomial( n , p ),
    logit(p) <- a[tank] + bp*pred,
    a[tank] ~ normal( a_bar , sigma ),
    bp ~ normal( -0.5 , 1 ),
    a_bar ~ normal( 0 , 1.5 ),
    sigma ~ exponential( 1 )
  ), data=dat , chains=4 , cores=4 , log_lik=TRUE )


# size
m1.3 <- ulam(
  alist(
    S ~ binomial( n , p ),
    logit(p) <- a[tank] + s[size_],
    a[tank] ~ normal( a_bar , sigma ),
    s[size_] ~ normal( 0 , 0.5 ),
    a_bar ~ normal( 0 , 1.5 ),
    sigma ~ exponential( 1 )
  ), data=dat , chains=4 , cores=4 , log_lik=TRUE )
# pred + size
m1.4 <- ulam(
  alist(
    S ~ binomial( n , p ),
    logit(p) <- a[tank] + bp*pred + s[size_],
    a[tank] ~ normal( a_bar , sigma ),
    bp ~ normal( -0.5 , 1 ),
    s[size_] ~ normal( 0 , 0.5 ),
    a_bar ~ normal( 0 , 1.5 ),
    sigma ~ exponential( 1 )
  ), data=dat , chains=4 , cores=4 , log_lik=TRUE )
# pred + size + interaction
m1.5 <- ulam(
  alist(
    S ~ binomial( n , p ),
    logit(p) <- a_bar + z[tank]*sigma + bp[size_]*pred + s[size_],
    z[tank] ~ normal( 0 , 1 ),
    bp[size_] ~ normal( -0.5 , 1 ),
    s[size_] ~ normal( 0 , 0.5 ),
    a_bar ~ normal( 0 , 1.5 ),
    sigma ~ exponential( 1 )
  ), data=dat , chains=4 , cores=4 , log_lik=TRUE )


plot( coeftab( m1.1 , m1.2 , m1.3 , m1.4 , m1.5 ), pars="sigma" )

