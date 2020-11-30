cols<-c("darkgrey","steelblue","salmon","orange","greenyellow","mediumorchid","lightcyan","brown")

generateMeans<-function(n=c(1,2,5,10,50,100,500,1000),col=cols,N=1e4,rDistFun,xlim=NULL,...){
  sim<-matrix(nrow=N,ncol=length(n))
  for(j in 1:length(n)){for(i in 1:N){
    sim[i,j]<-mean(rDistFun(n[j],...))
  }}
  
  par(mar=c(3,2.5,1.5,0.5),mfrow=c(length(n),1))
  for(j in 1:length(n)){
    if(length(xlim)<2){
      hist(breaks=100,sim[,j],main=paste(sep="","n = ",n[j]),yaxt="n",col=col[j])
    }else{
      hist(breaks=100,sim[,j],main=paste(sep="","n = ",n[j]),xlim=xlim,yaxt="n",col=col[j])
    }
  }
}

cltNorm<-function(n){
  generateMeans(n=n,rDistFun=rnorm,xlim=c(-3,3))
}

cltBeta<-function(n){
  ggenerateMeans(n=n,rDistFun=rbeta,shape1=0.5,shape2=0.5,xlim=c(0,1))
}

cltBinom<-function(n){
  generateMeans(n=n,rDistFun=rbinom,size=2,prob=0.25,xlim=c(0,2))
}

cltExp<-function(n){
  generateMeans(n=n,rDistFun=rexp,rate=0.5,xlim=c(0,15))
}

