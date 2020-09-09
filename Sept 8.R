library(rethinking)
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e5 , replace=TRUE )

sum(samples<0.2)/length(samples)

sum(samples>0.8)/length(samples)

quantile(samples,c(0.8, 1))

quantile(samples,c(0.025,0.5, 0.975))

quantile(samples, c(0.2, 0.8))

PI(samples,prob=0.6)

# add up posterior probability where p < 0.5
sum( posterior[ p_grid < 0.5 ] )
sum(samples<0.5)/length(samples)

PI(samples, prob=0.66)
HPDI(samples, prob=0.66)

#3M1
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
HPDI(samples, prob = 0.9)

w <- rbinom( 1e4 , size=9 , prob=samples )
simplehist(w)
sum(w == 6) /10000


#3M5
p_grid2 <- seq( from=0 , to=1 , length.out=1000 )
prior2 <- ifelse( p_grid2 < 0.5 , 0 , 1 )
likelihood2 <- dbinom( 8 , size=15 , prob=p_grid2 )
posterior2 <- likelihood2 * prior2
posterior2 <- posterior2 / sum(posterior2)
set.seed(100)
samples2 <- sample( p_grid , prob=posterior2 , size=1e4 , replace=TRUE )

plot(posterior2 ~ p_grid2, type = 'l')

HPDI(samples2, prob=.9)

w2<- rbinom(1e4, size=15, prob=samples2)
sum(w2==8)/1e4 #about 16% compared to ~14.5%
table(w2)/1e4
simplehist(w2)


#3M6
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 32 , size=60 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
HPDI(samples, prob = 0.9)


#Hard Questions
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
            1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
            1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
            0,0,0,1,1,1,0,0,0,0)

n <- length(birth1) + length(birth2)
p_grid <- seq( from=0 , to=1 , length.out=n) 
prior <- rep( 1 , n ) 
likelihood <- dbinom( sum(birth1) + sum(birth2) , size= n , prob=p_grid ) 
posterior <- likelihood * prior 
posterior <- posterior / sum(posterior) 

plot( posterior ~ p_grid , type="l" )
p_grid[ which.max(posterior) ]

par.samples <- sample( p_grid , size=10000 , replace=TRUE , prob=posterior )
HPDI(par.samples,prob=0.50)
HPDI(par.samples,prob=0.89)
HPDI(par.samples,prob=0.97)

w <- rbinom(1e4,200,prob=par.samples)
dens(w)
abline(v=111)

w <- rbinom(1e4,100,prob=par.samples)
dens(w)
abline( v=sum(birth1) , col="red" )

#3H5
firstgirls<-sum(birth1==0)
table(birth1,birth2)
sum(birth2[birth1==0])

w <- rbinom(1e4,firstgirls,prob=par.samples)
dens(w)
abline( v=sum(birth2[birth1==0]) , col="red" )
