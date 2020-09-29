library(rethinking)
## R code 6.13
set.seed(71)
# number of plants
N <- 100

# simulate initial heights
h0 <- rnorm(N,10,2)

# assign treatments and simulate fungus and growth
treatment <- rep( 0:1 , each=N/2 )
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 )
h1 <- h0 + rnorm(N, 5 - 3*fungus) + rnorm(N, 5 + 1*treatment)

# compose a clean data frame
d <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )
precis(d)

## R code 6.14
sim_p <- rlnorm( 1e4 , 0 , 0.25 )
precis( data.frame(sim_p) )

## R code 6.15
m6.6 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0*p,
    p ~ dlnorm( 0 , 0.25 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.6)

## R code 6.16
m6.7 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0 * p,
    p <- a + bt*treatment + bf*fungus,
    a ~ dlnorm( 0 , 0.2 ) ,
    bt ~ dnorm( 0 , 0.5 ),
    bf ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.7)

## R code 6.17
m6.8 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0 * p,
    p <- a + bt*treatment,
    a ~ dlnorm( 0 , 0.2 ),
    bt ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.8)


m6.8f <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ),
    mu <- h0 * p,
    p <- a + bf*fungus,
    a ~ dlnorm( 0 , 0.2 ),
    bf ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.8f)

##
m6.8b <- quap(
  alist(
    fungus ~ dnorm( mu , sigma ),
    mu <- a + bt*treatment,
    a ~ dnorm( 0 , 1 ),
    bt ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )
  ), data=d )
precis(m6.8b)


library(brms)
library(sjstats)
mod<-brm(bf(h1~h0+fungus+treatment) +
           bf(fungus~treatment) + set_rescor(FALSE),data=d,chains=3,cores=3)

mediation(mod)

## R code 6.18
library(dagitty)
plant_dag <- dagitty( "dag {
    H_0 -> H_1
    F -> H_1
    T -> F
}")
coordinates( plant_dag ) <- list( x=c(H_0=0,T=2,F=1.5,H_1=1) ,
                                  y=c(H_0=0,T=0,F=0,H_1=0) )
drawdag( plant_dag )

## R code 6.19
impliedConditionalIndependencies(plant_dag)



#6M
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

adjustmentSets( dag_6M1 , exposure="X" , outcome="Y" )

#6M2
N <- 1000
X <- rnorm(N)
Z <- rnorm(N,X,0.1)
Y <- rnorm(N,Z)
cor(X,Z)

m_6M2 <- quap(alist(
                 Y ~ dnorm( mu , sigma ),
                 mu <- a + bX*X + bZ*Z,
                 c(a,bX,bZ) ~ dnorm(0,1),
                 sigma ~ dexp(1)
               ) , data=list(X=X,Y=Y,Z=Z) )
precis( m_6M2 )


#6H1
data("WaffleDivorce")
d<-WaffleDivorce


mW<-quap(alist(
  Divorce~ dnorm(mu,sigma),
  mu <- a + bW*WaffleHouses + bS*South + bPS*PropSlaves1860,
  c(a,bW,bS,bPS) ~ dnorm(0,100),
  sigma ~ dexp(1)), data=d)

mW2<-quap(alist(
  Divorce~ dnorm(mu,sigma),
  mu <- a + bW*WaffleHouses + bS*South,
  c(a,bW,bS) ~ dnorm(0,100),
  sigma ~ dexp(1)), data=d)
precis(mW2)

mW3<-quap(alist(
  Divorce~ dnorm(mu,sigma),
  mu <- a + bW*WaffleHouses + bS*South + bPS*PropSlaves1860 + bM*Marriage,
  c(a,bW,bS,bPS,bM) ~ dnorm(0,100),
  sigma ~ dexp(1)), data=d)
precis(mW3)
