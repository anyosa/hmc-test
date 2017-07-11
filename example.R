source("/home/susan/Dropbox/code/powdist.R")

x <- 1:10
funs <- list(
  sum = sum,
  mean = mean,
  median = median
)
lapply(funs, function(f) f(x))


probs = list( ppl, pplr, ppn, ppgma)
mapply (probs, c(1,2),c(1,2))


ppower = function(type,eta, lambda){
  
  probs = list( ppl, pplr, ppn, ppgma)
  
  "ppl"=1
  "pplr"=2
  "ppn"=3
  "ppgma"=4
  
  val=lapply(probs[k],function(f) f(eta,lambda))
  return(val)
}

k = 1
a=1.5
b = 3
lapply(probs[k],function(f) f(eta,lambda))


ppower <- function(type,eta,lambda){
require(VGAM)
probs <- list(
  pl = function(eta,lambda) plogis(eta)**lambda,
  plr = function(eta,lambda) 1-plogis(-eta)**lambda,
  pn = function(eta,lambda) pnorm(eta)**lambda,
  pnr = function(eta,lambda) 1-pnorm(-eta)**lambda,
  pc = function(eta,lambda) pcauchy(eta)**lambda,
  pcr = function(eta,lambda) 1-cauchy(-eta)**lambda,
  pgma = function(eta,lambda) pgumbel(eta)**lambda,
  pgmar = function(eta,lambda) 1-pgumbel(-eta)**lambda,
  pgmi = function(eta,lambda) (1-pgumbel(-eta))**lambda,
  pgmir = function(eta,lambda) 1-(1-pgumbel(eta))**lambda
)
return(probs[[type]](eta,lambda))
}
