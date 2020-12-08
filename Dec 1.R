
y ~ -1 + x + z + x:z 

post <- extract.samples(m4.3)
newdat<-expand.grid(x=c("schizophrenia","sdsa","sadsa"),z=c("1","2"))

sim.height <- apply( newdat,1,function(j)
  rnorm(
  n=nrow(post) ,  
    mean=post$b_x*j[1]+post$b_z*j[2]+post$b_x:z*j[1]*j[2] ,
    sd=post$sigma ) )

height.PI <- apply( sim.height , 2 , PI , prob=0.89 )



runmod<-function(x){
  mod1<-brm(x~Infsex...)
  mod2<-brm(x~Infsex...)
  return(list(mod1,mod2))
}

modIga<-runmod(d$IgA)
