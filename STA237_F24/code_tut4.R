set.seed(123)
N <- 5000
sim <- NULL
simA <- rep(0,N)
simA2 <- rep(0,N)
simB <- rep(0,N)
for(i in 1:N){
  x <- sample(0:2, size = 1, prob=c(1/3,1/6,1/2)) 
  flips<-sample(0:1,size=2*x, replace=TRUE)
  
#Estimating A intersecting B
  if (sum(flips)>1 & x==1){ 
    success <- 1
  } else {
    success <- 0
  }
  sim[i] <- success

  #Estimating prob. of A
  if(sum(flips)>1){simA[i] <- 1}
  #Estimating prob. of A using a different method
  if(x==1){if(sum(flips)<1){simA2[i] <- 1}}
  if(x==2){if(sum(flips)<3){simA2[i] <- 1}}
  
  #Estimating prob. of B
  if(x==1){simB[i] <- 1}
}
mean(sim)


#Estimating prob. of A = (11/32)+(1/24)
mean(simA)
mean(simA2)
#Estimating prob. of B = 1/6
mean(simB)
#Estimating prob. of A intersection B = (1/6)*(1/4)
mean((simA+simB)==2)
#Estimating prob. of AuB
mean((simA+simB)>0)
#Estimating prob. of A|B
mean((simA+simB)==2)/mean(simB) #Option 1
mean(simA[simB==1]) #option 2
#Estimating prob. of B|A
mean((simA+simB)==2)/mean(simA) #Option 1
mean(simB[simA==1]) #Option 2
