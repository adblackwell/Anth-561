library(rethinking)
data(chimpanzees)
d <- chimpanzees

mc.cores = parallel::detectCores()
rstan::rstan_options(auto_write = TRUE)

d$treatment <- 1 + d$prosoc_left + 2*d$condition

# trimmed data list 11.10
dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  treatment = as.integer(d$treatment) )

m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + b[treatment] ,
    a[actor] ~ dnorm( 0 , 1.5 ),
    b[treatment] ~ dnorm( 0 , 0.5 )
  ) , data=dat_list , chains=4 , log_lik=TRUE )
precis( m11.4 , depth=2 )

d2<-d
d2$actor<-as.factor(d2$actor)
d2$treatment<-as.factor(d2$treatment)
#can also specify link as binomial(link="logit")
library(brms)
brm11.4<-brm(chose_prosoc~ 0 + actor + treatment, family=binomial(),data=d2,chains=4,cores=mc.cores - 1,sample_prior = TRUE)

prior<-set_prior("normal(0,1.5)",class="b")

brm11.4<-brm(chose_prosoc ~ 0 + actor + treatment, family=bernoulli(),data=d2,chains=4,cores=mc.cores - 1,prior=prior,sample_prior = TRUE)
summary(brm11.4)

brm11.4<-brm(chose_prosoc | trials(1) ~ 0 + actor + treatment, family=binomial(),data=d2,chains=4,cores=mc.cores - 1,prior=prior,sample_prior = TRUE)

data(UCBadmit)
d <- UCBadmit

modadmit<-brm(admit|trials(applications) ~ applicant.gender + dept, data=d, family=binomial(),chains=1)


brm11.4_2<-brm(pulled_left~ actor*prosoc_left, family=binomial(),data=d2,chains=4,cores=mc.cores - 1,sample_prior = TRUE)

prior_samples()

family="poisson"
family=poisson()
family="categorical"


m11.4q<- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + b[treatment] ,
    a[actor] ~ dnorm( 0 , 1.5 ),
    b[treatment] ~ dnorm( 0 , 0.5 )
  ) , data=dat_list )


m11.4q<- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + b[treatment] ,
    a[actor] ~ dnorm( 0 , 10 ),
    b[treatment] ~ dnorm( 0 , 0.5 )
  ) , data=dat_list )

m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + b[treatment] ,
    a[actor] ~ dnorm( 0 , 10),
    b[treatment] ~ dnorm( 0 , 10 )
  ) , data=dat_list , chains=4 , log_lik=TRUE )
  
precis( m11.4 , depth=2 )

glm1<-glm(pulled_left~ actor*prosoc_left, family=binomial(),data=d2)

precis(m11.4q,2)

#categorical
# simulate career choices among 500 individuals
N <- 500 # number of individuals
income <- c(1,2,5) # expected income of each career
score <- 0.5*income # scores for each career, based on income
# next line converts scores to probabilities
p <- softmax(score[1],score[2],score[3])
# now simulate choice
# outcome career holds event type values, not counts
career <- rep(NA,N) # empty vector of choices for each individual
# sample chosen career for each individual
set.seed(34302)
for ( i in 1:N ) career[i] <- sample( 1:3 , size=1 , prob=p )

dat_list <- list( N=N , K=3 , career=career , career_income=income )

dat_list <- list( N=N , K=3 , career=career , career_income=income )

d<-data.frame(career=career,career_income=c(1,2,5)[career])
library(brms)
mod<-brm(career~career_income,data=d,family="categorical",chains=1)

d<-data.frame(career=career,career_income=c(1,2,5)[career])
d$career2<-as.numeric(d$career==2)
d$career3<-as.numeric(d$career==3)

mod2<-brm(bf(career2~career_income) + bf(career3~career_income),data=d,family="bernoulli",chains=1)



library(MASS)
data(eagles)
d<-eagles
d$pirateL <- ifelse( d$P=="L" , 1 , 0 )
d$victimL <- ifelse( d$V=="L" , 1 , 0 )
d$pirateA <- ifelse( d$A=="A" , 1 , 0 )


f <- alist(
  y ~ dbinom( n , p ),
  logit(p) <- a + bP*pirateL + bV*victimL + bA*pirateA ,
  a ~ dnorm(0,1.5),
  bP ~ dnorm(0,1),
  bV ~ dnorm(0,1),
  bA ~ dnorm(0,1) )

m1 <- quap( f , data=d )
m1_stan <- ulam( f , data=d , chains=4,cores=4 , log_lik=TRUE )

precis(m1)
precis(m1_stan)

get_prior(y|trials(n) ~ pirateL + victimL + pirateA, data=d, family=binomial())

prior<-c(set_prior("normal(0,1)",class="b"),
         set_prior("normal(0,1.5)",class="Intercept"))
  
m1_brm<-brm(y|trials(n) ~ pirateL + victimL + pirateA, data=d, chains=4, cores=4, family=binomial(), prior=prior)


m2_brm<-brm(y|trials(n) ~  victimL + pirateL * pirateA, data=d, chains=4, cores=4, family=binomial(), prior=prior)

waic(m1_brm,m2_brm)
loo(m1_brm,m2_brm)


