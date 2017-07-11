dat = genpower(100,c(1,-4.5),1,"pl")[,-3]


U <- function(q){
#  q eh um vetor pra dois valor bo e b1
  U <- c()
  vx = dat[,1]
  vy = dat[,2]
  U <- (q[1]**2+q[2]**2)/(2*(100**2)) -q[1]*sum(vy) -q[2]*sum(vx*vy) + sum(log(1+exp(q[1]+q[2]*vx)))
  U = matrix(U,1,1)
  return(U)
  
}

grad_U <- function(q){
  grad_U <- c()
  vx = dat[,1]
  vy = dat[,2]
  grad_U[1] = q[1]**2/(100**2) - sum(vy) + sum(plogis(q[1]+q[2]*vx))
  
  grad_U[2] = q[2]**2/(100**2) - sum(vx*vy) + sum(vx*plogis(q[1]+q[2]*vx))
  grad_U = matrix(grad_U,2,1)
  return(grad_U)  
}  

######
epsilon = 0.25
L = 25
current_q = matrix(c(0,0),2,1)

q1 <- c() #dim 1
q2 <- c() #dim 2
q1[1] <- current_q[1,1]
q2[1] <- current_q[2,1]  

p1 <- c() #dim 3
p2 <- c() #dim 4



HMC(U, grad_U, epsilon, L, current_q)

for(j in 2:10000){
  current_q <- matrix(HMC(U, grad_U, epsilon, L, current_q)[,1],2,1)
  q1[j] <- current_q[1,1]
  q2[j] <- current_q[2,1]
}

mean(q1)
mean(q2)
