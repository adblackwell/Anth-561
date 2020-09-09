n<-20
p_grid <- seq( from=0 , to=1 , length.out=n )
# likelihood of 3 water in 3 tosses
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
prior <- rep(1,n) # uniform prior
posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize
plot( posterior ~ p_grid , type="l" )
# likelihood of 3 water in 4 tosses
likelihood <- dbinom( 3 , size=4 , prob=p_grid )
# likelihood of 5 water in 7 tosses 1.4
likelihood <- dbinom( 5 , size=7 , prob=p_grid )

prior <- ifelse( p_grid < 0.5 , 0 , 1 ) # new prior
likelihood <- dbinom( 1 , size=4 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize
plot( posterior ~ p_grid , type="l" )



prior <- c( rep(0,n/2), rep(1,n/2) )

prior <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 )


prior<-posterior
likelihood <- dbinom( 2 , size=4 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize
plot( posterior ~ p_grid , type="l" )


p_grid <- seq( from=0 , 
               to=1 , 
               length.out=n )


par(mfrow=c(3,3))
n<-20
p_grid <- seq( from=0 , to=1 , length.out=n )
prior <- rep(1,n) # uniform prior
for(i in 1:9){
  likelihood <- dbinom( 3 , size=4 , prob=p_grid )
  posterior <- likelihood * prior
  posterior <- posterior / sum(posterior) # standardize
  plot( posterior ~ p_grid , type="l" )
  prior<-posterior
}

par(mfrow=c(1,2))
plot( posterior ~ p_grid , type="l" )

prior <- rep(1,n) # uniform prior
likelihood <- dbinom( 27 , size=36 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize
plot( posterior ~ p_grid , type="l" )

