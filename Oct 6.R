#Interaction Models
## stepwise selection of interaction terms
dep2<- update(dep,formula. = ~ . + GiniZ:male,cores=3)
dep3<- update(dep,formula. = ~ . + male:HHWealthZ.vil,cores=3)
dep4<- update(dep,formula. = ~ . + GiniZ:HHWealthZ.vil,cores=3)
dep5<- update(dep,formula. = ~ . + GiniZ:male + male:HHWealthZ.vil,cores=3)
dep6<- update(dep,formula. = ~ . + GiniZ:HHWealthZ.vil + male:HHWealthZ.vil,cores=3)
dep7<- update(dep,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male,cores=3)
dep8<- update(dep,formula. = ~ . + GiniZ:HHWealthZ.vil + GiniZ:male + male:HHWealthZ.vil,cores=3)
loo(dep, dep2, dep3, dep4, dep5, dep6,dep7,dep8, reloo=FALSE) 
waic(dep, dep2, dep3, dep4, dep5, dep6,dep7,dep8, reloo=FALSE) 
deploo<-loo(dep, dep2, dep3, dep4, dep5, dep6,dep7,dep8, reloo=TRUE) 


#from Michael
library(rethinking)
data("Laffer")
L <- Laffer

L$Trev <- scale( L$tax_revenue )
L$TRate <- scale( L$tax_rate )

m7H1L <- quap(
  alist(
    Trev ~ dnorm( mu , sigma ) ,
    mu <- a + bTRate * TRate,
    a ~ dnorm( 0 , 50 ) ,
    bTRate ~ dnorm( 0 , 15 ) ,
    sigma ~ dexp( 1 )
  ) , data = L )
precis(m7H1L)

L$tax_rate_sq <- L$TRate ^2
m7H1Q <- quap(
  alist(
    Trev ~ dnorm( mu , sigma ) ,
    mu <- a + bTRate1 * TRate + bTRate2 * tax_rate_sq,
    a ~ dnorm( 0 , 50 ) ,
    bTRate1 ~ dnorm( 0 , 15 ) ,
    bTRate2 ~ dnorm( 0 , 15 ) , 
    sigma ~ dexp( 1 )
  ) , data = L )
precis(m7H1Q)

m7H13 <- quap(
  alist(
    Trev ~ dnorm( mu , sigma ) ,
    mu <- a + bTRate * TRate + bTRate2 * TRate^2 + bTRate3 * TRate^3,
    a ~ dnorm( 0 , 50 ) ,
    c(bTRate,bTRate2,bTRate3) ~ dnorm( 0 , 15 ) ,
    sigma ~ dexp( 1 )
  ) , data = L )
precis(m7H13)



set.seed(77)
compare( m7H1L , m7H1Q, m7H13, func=WAIC )
compare( m7H1L , m7H1Q, m7H13, func=PSIS )

T_seq <- seq( from=-3.2 , to=1.2 , length.out=30 )
LL <- link( m7H1L , data=list(TRate=T_seq, tax_rate_sq=T_seq^2) )
LQ <- link( m7H1Q , data=list(TRate=T_seq, tax_rate_sq=T_seq^2) )
L3 <- link( m7H13 , data=list(TRate=T_seq, tax_rate_sq=T_seq^2) )

plot( L$TRate , L$Trev , xlab="tax rate" , ylab="revenue" )
mtext( "linear model" )
lines( T_seq , colMeans(LL) )
shade( apply( LL , 2 , PI ) , T_seq )

plot( L$TRate , L$Trev , xlab="tax rate" , ylab="revenue" )
mtext( "quadratic model" )
lines( T_seq , colMeans(LQ) )
shade( apply( LQ , 2 , PI ) , T_seq )


plot( L$TRate , L$Trev , xlab="tax rate" , ylab="revenue" )
mtext( "quadratic model" )
lines( T_seq , colMeans(L3) )
shade( apply( L3 , 2 , PI ) , T_seq )

pointWAIC<-WAIC(m7H1Q,pointwise=TRUE)


m7H1d <- quap(
  alist(
    Trev ~ dstudent( 2 , mu , sigma ),
    mu <- a + b*TRate,
    a ~ dnorm( 0 , 0.2 ),
    b ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp(1)
  ) , data=L )
precis(m7H1d)
compare( m7H1L , m7H1Q, m7H13,m7H1d, func=WAIC )

Ld <- link( m7H1d , data=list(TRate=T_seq, tax_rate_sq=T_seq^2) )
plot( L$TRate , L$Trev , xlab="tax rate" , ylab="revenue" )
mtext( "quadratic model" )
lines( T_seq , colMeans(Ld) )
shade( apply( Ld , 2 , PI ) , T_seq )


# define probabilities of sides 7.3
p1 <- c( 0.2 , 0.2 , 0.2, 0.2, 0.2)
p2 <- c(0.8,0.1,0.05,0.025,0.025)
p3<- c(0.05,0.15,0.7,0.05,0.05)
# compute entropy
-sum( p1*log(p1) )
-sum( p2*log(p2) )
-sum( p3*log(p3) )

IB<-list(p1,p2,p3)

DKL <- function(p,q) sum( p*(log(p)-log(q)) )

Dm <- matrix( NA , nrow=3 , ncol=3 )
for ( i in 1:3 ) for ( j in 1:3 ) Dm[i,j] <- DKL( IB[[j]] , IB[[i]] )
round( Dm , 2 )




