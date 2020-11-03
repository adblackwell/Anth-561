library(rethinking)

# define parameters
prob_drink <- 0.2 # 20% of days
rate_work <- 1 # average 1 manuscript per day
# sample one year of production
N <- 365
# simulate days monks drink
set.seed(365)
drink <- rbinom( N , 1 , prob_drink )
# simulate manuscripts completed
y <- (1-drink)*rpois( N , rate_work )

simplehist( y , xlab="manuscripts completed" , lwd=4 )
zeros_drink <- sum(drink)
zeros_work <- sum(y==0 & drink==0)
zeros_total <- sum(y==0)
lines( c(0,0) , c(zeros_work,zeros_total) , lwd=4 , col=rangi2 )

m12.3 <- ulam( 12.9
               alist(
                 y ~ dzipois( p , lambda ),
                 logit(p) <- ap,
                 log(lambda) <- al,
                 ap ~ dnorm( -1.5 , 1 ),
                 al ~ dnorm( 1 , 0.5 )
               ) , data=list(y=y) , chains=4 )
precis( m12.3 )

#brms zero inflated Poisson
library(brms)
fit_zinb2 <- brm(y ~ al, data = list(y=y), family = zero_inflated_poisson())

fit_zinb2 <- brm(bf(y ~ al, zi ~ ap), data = list(y=y), family = zero_inflated_poisson())

fit_zinb2 <- brm(bf(y ~ al, hu ~ ap), data = list(y=y), family = hurdle_poisson())






hurdle_poisson(link = "log")
hurdle_negbinomial(link = "log", link_shape = "log", link_hu = "logit")
hurdle_gamma(link = "log", link_shape = "log", link_hu = "logit")
hurdle_lognormal(link = "identity", link_sigma = "log", link_hu = "logit")

zero_inflated_beta(link = "logit", link_phi = "log", link_zi = "logit")
zero_one_inflated_beta(
  link = "logit",
  link_phi = "log",
  link_zoi = "logit",
  link_coi = "logit"
)
zero_inflated_poisson(link = "log", link_zi = "logit")
zero_inflated_negbinomial(link = "log", link_shape = "log", link_zi = "logit")
zero_inflated_binomial(link = "logit", link_zi = "logit")



#brms cumulative /ordinal
data(Trolley)
d <- Trolley

# start values for b11.2
inits <- list(`Intercept[1]` = -1.9,
              `Intercept[2]` = -1.2,
              `Intercept[3]` = -0.7,
              `Intercept[4]` =  0.2,
              `Intercept[5]` =  0.9,
              `Intercept[6]` =  1.8,
              action         =  0,
              intention      =  0,
              contact        =  0)

b11.2 <- brm(response ~ 1 + action + intention + contact,data = d, family = cumulative,
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b)),
      iter = 2000, warmup = 1000, cores = 2, chains = 2,
      inits = list(inits, inits),
      seed = 11)

# start values for b11.3
inits <- list(`Intercept[1]`      = -1.9,
              `Intercept[2]`      = -1.2,
              `Intercept[3]`      = -0.7,
              `Intercept[4]`      =  0.2,
              `Intercept[5]`      =  0.9,
              `Intercept[6]`      =  1.8,
              action              =  0,
              intention           =  0,
              contact             =  0,
              `action:intention`  =  0,
              `contact:intention` =  0)

b11.3 <- update(b11.2,formula = response ~ 1 + action + intention + contact + action:intention + contact:intention,inits = list(inits, inits))

#brms negative binomial
brm(admit ~ 1 + applicant.gender,
  data = d, family = negbinomial,
    prior = c(prior(normal(0, 10), class = Intercept),
              prior(normal(0, 1), class = b),
              prior(gamma(0.01, 0.01), class = shape)),  # this is the brms default
    iter = 4000, warmup = 1000, cores = 2, chains = 2, seed = 11)

n <- c( 12, 36 , 7 , 41 )
q <- n / sum(n)
p <- cumsum(q)

#12M2
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

library(rethinking)
data(Hurricanes)
d<-Hurricanes
library(brms)
d$femininity<-scale(d$femininity)
mod<-brm(deaths~femininity,data=d,family=poisson(),chains=4,cores=4)

mod2<-brm(deaths~femininity,data=d,family=negbinomial(),chains=4,cores=4)

mod3<-brm(bf(deaths~femininity,hu~femininity),data=d,family=hurdle_poisson(),chains=4,cores=4)

pri<-c(prior(normal(1,1),class="Intercept"),prior(normal(0,1),class="b",coef="femininity"))

mod_p<-brm(deaths~femininity,data=d,family=poisson(),chains=4,cores=4,prior=pri)


mod2<-brm(deaths~femininity,data=d,family=negbinomial(),chains=4,cores=4)

plot(d$min_pressure,d$damage_norm)
plot(d$femininity,d$damage_norm)

d$damage_norm<-scale(d$damage_norm)
d$min_pressure<-scale(d$min_pressure)

mod4<-brm(bf(deaths~femininity+min_pressure+damage_norm,hu~femininity+min_pressure+damage_norm),data=d,family=hurdle_poisson(),chains=4,cores=4)

mod5<-brm(bf(deaths~femininity+min_pressure+damage_norm),data=d,family=negbinomial(),chains=4,cores=4)

mod5<-brm(bf(deaths~femininity*damage_norm),data=d,family=negbinomial(),chains=4,cores=4,inits="0")


data(Hurricanes)
d<-Hurricanes
d$fmnnty_std <- ( d$femininity - mean(d$femininity) )/sd(d$femininity)
dat <- list( D=d$deaths , F=d$fmnnty_std )
f <- alist(
  D ~ dpois(lambda),
  log(lambda) <- a + bF*F,
  a ~ dnorm(1,1),
  bF ~ dnorm(0,1) )

m1 <- ulam( f , data=dat , chains=4 , log_lik=TRUE )
precis( m1 )

# plot raw data 12.9
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

stem( PSISk( m1 ) )
