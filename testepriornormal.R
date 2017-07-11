source("/home/susan/Dropbox/Susan/code/tools.R")
dat <- genpower(2000, c(0,1), 0.4, "pp")[,-3]
y = dat$y; x = dat$V1

###################################################################################
Iterations<-20000
mu.theta<-c(0,0,0); s.theta<-c(100,100,0.5) #parameters of priors
prop.s<-c(0.1,0.1,0.1) #set proposal tuning parameters
theta <- matrix(nrow=Iterations, ncol=3) #matrix of parameters
acc.prob <- c(0,0,0) #init. counter of acceptance probability
current.theta<-c(0,0,0.5) #inits $theta**(0)$
for (t in 1:Iterations){
  prop.theta<- current.theta
  
  prop.theta[1]<- rnorm(1, current.theta[1],prop.s[1] )
  cur.eta <-current.theta[1]+current.theta[2]*x
  prop.eta<- prop.theta[1] +prop.theta[2] *x
  
  loga <- (
    #sum( y*prop.eta - log(1+exp(prop.eta)))
    sum(y*log(pnorm(prop.eta)**exp(prop.theta[3]))+(1-y)*log(1-pnorm(prop.eta)**exp(prop.theta[3])))
    #-sum( y*cur.eta - log(1+exp(cur.eta)))
    -sum(y*log(pnorm(cur.eta)**exp(current.theta[3]))+(1-y)*log(1-pnorm(cur.eta)**exp(current.theta[3])))
    #+sum(dnorm(prop.beta, mu.beta,s.beta,log=TRUE))
    +sum(dnorm(prop.theta[1], mu.theta[1],s.theta[1],log=TRUE),
         dnorm(prop.theta[2], mu.theta[2],s.theta[2],log=TRUE),
         dnorm(prop.theta[3], mu.theta[3],s.theta[3],log=TRUE))
    #-sum(dnorm(current.beta, mu.beta, s.beta, log=TRUE)))
    -sum(dnorm(current.theta[1], mu.theta[1],s.theta[1],log=TRUE),
         dnorm(current.theta[2], mu.theta[2],s.theta[2],log=TRUE),
         dnorm(current.theta[3], mu.theta[3],s.theta[3],log=TRUE))
  )
  u<-runif(1)
  u<-log(u)
  if( u < loga) {
    current.theta<-prop.theta
    acc.prob[1] <- acc.prob[1]+1
  }
  prop.theta<- current.theta
  prop.theta[2]<- rnorm(1, current.theta[2],prop.s[2])
  cur.eta <-current.theta[1]+current.theta[2]*x
  prop.eta<-prop.theta[1] + prop.theta[2]*x
  loga <- (
    sum(y*log(pnorm(prop.eta)**exp(prop.theta[3]))+(1-y)*log(1-pnorm(prop.eta)**exp(prop.theta[3])))
    -sum(y*log(pnorm(cur.eta)**exp(current.theta[3]))+(1-y)*log(1-pnorm(cur.eta)**exp(current.theta[3])))
    +sum(dnorm(prop.theta[1], mu.theta[1],s.theta[1],log=TRUE),
         dnorm(prop.theta[2], mu.theta[2],s.theta[2],log=TRUE),
         dnorm(prop.theta[3], mu.theta[3],s.theta[3],log=TRUE))
    -sum(dnorm(current.theta[1], mu.theta[1],s.theta[1],log=TRUE),
         dnorm(current.theta[2], mu.theta[2],s.theta[2],log=TRUE),
         dnorm(current.theta[3], mu.theta[3],s.theta[3],log=TRUE))
  )
  u<-runif (1)
  u<-log(u)
  if( u < loga ) {
    current.theta<-prop.theta
    acc.prob[2] <- acc.prob[2]+1
  }
  prop.theta<- current.theta
  prop.theta[3]<- rnorm(1,current.theta[3],prop.s[3])
  cur.eta <-current.theta[1]+current.theta[2]*x
  prop.eta<-prop.theta[1] + prop.theta[2]*x
  loga <- (
    sum(y*log(pnorm(prop.eta)**exp(prop.theta[3]))+(1-y)*log(1-pnorm(prop.eta)**exp(prop.theta[3])))
    -sum(y*log(pnorm(cur.eta)**exp(current.theta[3]))+(1-y)*log(1-pnorm(cur.eta)**exp(current.theta[3])))
    +sum(dnorm(prop.theta[1], mu.theta[1],s.theta[1],log=TRUE),
         dnorm(prop.theta[2], mu.theta[2],s.theta[2],log=TRUE),
         dnorm(prop.theta[3], mu.theta[3],s.theta[3],log=TRUE))
    -sum(dnorm(current.theta[1], mu.theta[1],s.theta[1],log=TRUE),
         dnorm(current.theta[2], mu.theta[2],s.theta[2],log=TRUE),
         dnorm(current.theta[3], mu.theta[3],s.theta[3],log=TRUE))
  )
  u<-runif (1)
  u<-log(u)
  if( u < loga ) {
    current.theta<-prop.theta
    acc.prob[3] <- acc.prob[3]+1
  }
  
  theta[t,]<-current.theta
}
