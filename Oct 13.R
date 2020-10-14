library(rethinking)
data(rugged)
d <- rugged

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )

# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# rescale variables
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)



m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + b1*cont_africa + b2*( rugged_std - 0.215 ) + b3*cont_africa*( rugged_std - 0.215 ) ,
    a ~ dnorm( 1 , 0.1 ) ,
    c(b1,b2,b3) ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )

precis(m8.3)

library(brms)
mod<-brm(log_gdp_std~cont_africa*rugged_std,cores=3,chains=3,data=dd)
mod2<-brm(log_gdp_std~rugged_std+cont_africa:rugged_std,cores=3,chains=3,data=dd)

model_weights(mod,mod2,weights="waic")
waic(mod,mod2)

dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

m8.3b <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )

post <- extract.samples(m8.3b)
diff_a1_a2 <- post$a[,1] - post$a[,2]
PI( diff_a1_a2 )
mean(diff_a1_a2)

dd$cont_africa2 <- dd$cont_africa-0.5


m8.3c <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + b1*cont_africa2 + b2*( rugged_std - 0.215 ) + b3*cont_africa2*( rugged_std - 0.215 ) ,
    a ~ dnorm( 1 , 0.1 ) ,
    c(b1,b2,b3) ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )

precis(m8.3c)


#equation for value not in Africa
# a + b1 * -0.5 + b2*( rugged_std - 0.215 ) + b3 ( rugged_std - 0.215 ) * -0.5
#value in Africa
# a + b1 * 0.5 + b2*( rugged_std - 0.215 ) + b3 ( rugged_std - 0.215 ) * 0.5

mod<-brm(log_gdp_std~cont_africa*rugged_std,cores=3,chains=3,data=dd)
mod<-brm(log_gdp_std~cont_africa + rugged_std + cont_africa:rugged_std,cores=3,chains=3,data=dd)

mod<-brm(log_gdp_std~rugged_std + cont_africa:rugged_std,cores=3,chains=3,data=dd)

mod<-brm(log_gdp_std~ -1 + rugged_std + cont_africa:rugged_std,cores=3,chains=3,data=dd)


a + bw*water_cent + bs*shade_cent

(a + bw*water_cent + bs*shade_cent)(1-H)

a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent + a*H + bw*water_cent*H + bs*shade_cent*H + bws*water_cent*shade_cent*H


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

p <- extract.prior( m8M4a )
par(mfrow=c(1,3),cex=1.1) # 3 plots in 1 row
for ( s in -1:1 ) {
  idx <- which( d$shade_cent==s )
  plot( d$water_cent[idx] , d$blooms_std[idx] , xlim=c(-1,1) , ylim=c(0,1) ,
        xlab="water" , ylab="blooms" , pch=16 , col=rangi2 )
  mtext( concat( "shade = " , s ) )
  mu <- link( m8M4a , post=p , data=data.frame( shade_cent=s , water_cent=-1:1 ) )
  for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("black",0.3) )
}


library(rethinking)
data(tulips)
d <- tulips
d$bed

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

k <- PSISk( m8.3 )
o <- order( k , decreasing=TRUE )
data.frame( country=as.character( dd$isocode ) ,
            k=k , dd$rugged_std , dd$log_gdp_std )[o,]


m8.3t <- quap(
  alist(
    log_gdp_std ~ dstudent( 2 , mu , sigma ) ,
    mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
    a[cid] ~ dnorm( 1 , 0.1 ) ,
    b[cid] ~ dnorm( 0 , 0.3 ) ,
    sigma ~ dexp( 1 )
  ) , data=dd )


data(Wines2012)
d <- Wines2012
dat_list <- list(
  S = standardize(d$score),
  jid = as.integer(d$judge),
  wid = as.integer(d$wine)
)

m1 <- quap(
  alist(
    S ~ dnorm( mu , sigma ),
    mu <- a[jid] + w[wid],
    a[jid] ~ dnorm(0,0.5),
    w[wid] ~ dnorm(0,0.5),
    sigma ~ dexp(1)
  ), data=dat_list )

pp<-precis( m1 , 2 )
plot( precis( m1 , 2 ) )
plotpost( precis( m1 , 2 ) )
precis_plot( precis( m1 , 2 ) )


brm(y~s(x))
