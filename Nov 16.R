
library(brms)

brm(wait ~ afternoon + (afternoon | cafe))

bf(inflammation ~ disgust + marketintegration + (disgust|Village) + (1|household))

(1|cafe)
(afternoon|cafe)
(afternoon + customer number | cafe)
(afternoon | cafe) + (customer number | cafe)
(0 + afternoon|cafe)
(-1 + afternoon|cafe)

# set up parameters of population 14.6
a <- 3.5 # average morning wait time
b <- (-1) # average difference afternoon wait time
sigma_a <- 1 # std dev in intercepts
sigma_b <- 0.5 # std dev in slopes
rho <- (0) # correlation between intercepts and slopes
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
sigma <- 0.5 # std dev within cafes
wait <- rnorm( N_visits*N_cafes , mu , sigma )
# package into data frame
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )

b14.1 <- 
  brm(data = d, 
      family = gaussian,
      wait ~ 1 + afternoon + (1 + afternoon | cafe),
      prior = c(prior(normal(5, 2), class = Intercept),
                prior(normal(-1, 0.5), class = b),
                prior(exponential(1), class = sd),
                prior(exponential(1), class = sigma),
                prior(lkj(2), class = cor)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 867530)


library(rethinking)

set.seed(73)
N <- 500
U_sim <- rnorm( N )
Q_sim <- sample( 1:4 , size=N , replace=TRUE )
E_sim <- rnorm( N , U_sim + Q_sim )
W_sim <- rnorm( N , U_sim + 0*E_sim )
dat_sim <- list(
  U=standardize(U_sim) ,
  W=standardize(W_sim) ,
  E=standardize(E_sim) ,
  Q=standardize(Q_sim) )

summary(lm(W~E,data=dat_sim))
summary(lm(W~E+U,data=dat_sim))

library(AER)
summary(ivreg(W~E | Q,data=dat_sim))

m14.6 <- ulam(
  alist(
    c(W,E) ~ multi_normal( c(muW,muE) , Rho , Sigma ),
    muW <- aW + bEW*E,
    muE <- aE + bQE*Q,
    c(aW,aE) ~ normal( 0 , 0.2 ),
    c(bEW,bQE) ~ normal( 0 , 0.5 ),
    Rho ~ lkj_corr( 2 ),
    Sigma ~ exponential( 1 )
  ), data=dat_sim , chains=4 , cores=4 )
precis( m14.6 , depth=3 )

#do this
m<-brm(bf(W ~ E) + bf(E ~ Q) + set_rescor(TRUE),data=dat_sim , chains=4 , cores=4 )

#not this
m2<-brm(bf(W ~ E) + bf(E ~ Q) + set_rescor(FALSE),data=dat_sim , chains=4 , cores=4 )

#14.4
library(rethinking)
data(KosterLeckie)
kl_data <- list(
                 N = nrow(kl_dyads),
                 N_households = max(kl_dyads$hidB),
                 did = kl_dyads$did,
                 hidA = kl_dyads$hidA,
                 hidB = kl_dyads$hidB,
                 giftsAB = kl_dyads$giftsAB,
                 giftsBA = kl_dyads$giftsBA
)

m14.7 <- ulam(
  alist(
    giftsAB ~ poisson( lambdaAB ),
    giftsBA ~ poisson( lambdaBA ),
    log(lambdaAB) <- a + gr[hidA,1] + gr[hidB,2] + d[did,1] ,
    log(lambdaBA) <- a + gr[hidB,1] + gr[hidA,2] + d[did,2] ,
    a ~ normal(0,1),
    ## gr matrix of varying effects
    vector[2]:gr[N_households] ~ multi_normal(0,Rho_gr,sigma_gr),
    Rho_gr ~ lkj_corr(4),
    sigma_gr ~ exponential(1),
    ## dyad effects
    transpars> matrix[N,2]:d <-
      compose_noncentered( rep_vector(sigma_d,2) , L_Rho_d , z ),
    matrix[2,N]:z ~ normal( 0 , 1 ),
    cholesky_factor_corr[2]:L_Rho_d ~ lkj_corr_cholesky( 8 ),
    sigma_d ~ exponential(1),
    ## compute correlation matrix for dyads
    gq> matrix[2,2]:Rho_d <<- Chol_to_Corr( L_Rho_d )
  ), data=kl_data , chains=4 , cores=4 , iter=2000 )
precis( m14.7 , depth=3 , pars=c("Rho_gr","sigma_gr") )

d2<-kl_dyads[,1:5]
names(d2)<-c("hidB","hidA","did","giftsBA","giftsAB")
d3<-rbind(kl_dyads[,1:5],d2)
d3[d3$did==1,]

m3<-brm(bf(giftsAB ~ (1|q|hidA)+(1|p|hidB)+(1|r|did))+
        bf(giftsBA ~ (1|q|hidA)+(1|p|hidB)+(1|r|did)),family="poisson",data=kl_dyads,cores=4)

m3<-brm(bf(giftsAB ~ (1|q|hidA)+(1|p|hidB)+(1|r|did))+
          bf(giftsBA ~ (1|q|hidA)+(1|p|hidB)+(1|r|did)),family="poisson",data=d3,cores=4)

m3<-brm(bf(giftsAB ~ (1|q|hidA:did)+(1|p|hidB:did)+(1|r|did))+
          bf(giftsBA ~ (1|q|hidA:did)+(1|p|hidB:did)+(1|r|did)),family="poisson",data=d3,cores=4)

m4<-brm(bf(giftsAB ~ (1|q|hidA:hidB)+(1|p|hidB:did)+(1|r|did))+
          bf(giftsBA ~ (1|q|hidA:did)+(1|p|hidB:did)+(1|r|did)),family="poisson",data=d3,cores=4)



bf(tools~gp(long,lat))

A <- solve(inv.phylo$Ainv)
rownames(A) <- rownames(inv.phylo$Ainv)

model_simple <- brm(
  phen ~ cofactor + (1|phylo), data = data_simple, 
  family = gaussian(), cov_ranef = list(phylo = A),
  prior = c(
    prior(normal(0, 10), "b"),
    prior(normal(0, 50), "Intercept"),
    prior(student_t(3, 0, 20), "sd"),
    prior(student_t(3, 0, 20), "sigma")
  )
)
