targetnromal2 <- function(x) {
  0.368047 * (exp(-x^2) + (sin(x)^2 / (1 + abs(x)^3)))}

targetnromal33 <- function(x) {
  (1/1.509988)* exp(-(x^4 + x^6 + x^8))}



targetnromal3 <-function(x){
  dnorm(x, mean = 1.5, sd = 0.25)
}



proposalnormal<- function (x,mean,sd){
  dnorm(x, mean = mean, sd = sd)
}



sampletarget <-function(mean,sd){
  rnorm(1,mean = mean, sd = sd)
}



sampleproposalnormal <- function(mean,sd){
  rnorm(1,mean = mean, sd = sd)}



MCMC <- function(n,targetnormal,sdp){
  accept = vector("logical",n)
  array2 = vector("numeric",n)
  data = vector("numeric",n)
  data2 = vector("numeric",n)
  meanp=0
  array2[1]=sampleproposalnormal(meanp,sdp)
  for (i in 1:n){
    test1=array2[i]
    proposal = sampleproposalnormal(test1,sdp)
    meanp1=proposal
    probability = min(1,targetnormal(proposal)*proposalnormal(test1,proposal,sdp)/(targetnormal(test1)*proposalnormal(proposal,test1,sdp)))
    
    
    test=rbinom(1, 1, probability)
    if(is.na(test)){
      i=i-1
      next}
    if(test==1){
      array2[i+1]= proposal
      accept[i] = TRUE
      data[i]=proposal
    }
    else{
      array2[i+1]= array2[i]
      accept[i] = FALSE
      object = array2[i]
      data[i]=object
    }
  } 
  
  return(data)
  
}



Test <- function(n,n2,sdp,targetnormal){
  list = list()
  for( i in  1:n){
    list [[i ]]=MCMC(n2,targetnormal,sdp)
  }
  return(list)
}



plotfunc<-function(list){
  hist(list[[1]],probability = TRUE, col = rgb(0.5, 0.5, 0.5, 0.5), xlab = "Value", ylab = "Density", 
       main = "Histogram of Simulations", breaks = "Sturges")
  for(i in 1:length(list)-1){
    hist(list[[i+1]], probability = TRUE,col = rgb(0.5, 0.5, 0.5, 0.5), add = TRUE, breaks = "Sturges")
  }
}

plotburnin <-function(n,xlim1,k){
plot(k[[1]],xlim=c(0,xlim1),pch = NA,type = "l")
colors <- rainbow(n)
if(n>1){
for (i in 2:n) {
  lines(k[[i]], col = colors[i])
}}}
