library(manipulate)
myhist<-function(mu) {
  g<-ggplot(Galton,aes(x=child))
  g<-g+geom_histogram(fill="salmon",binwidth=1,aes(y=..density..),colour="black")
  g<-g+geom_density(size=2)
  g<-g+geom_vline(xintercept=mu,size=2)
  mse<-round(mean((Galton$child-mu)^2),3)
  g<-g+labs(title=paste('mu= ', mu, 'MSE= ', mse))
  g
}
manipulate(myhist(mu), mu=slider(62,74,step=0.5))
data(father.son)
x<-father.son$sheight
(mean(x)+c(-1,1)*qnorm(0.975)*sd(x)/sqrt(length(x)))/12
binom.test(56,100)$conf.int
n<-20
pvals<-seq(0.1,0.9,by=0.05)
nosim<-1000
coverage<-sapply(pvals,function(p){
  phat<-rbinom(nosim,prob=p,size=n)/n
  ll<-phat-qnorm(0.975)*sqrt(phat*(1-phat)/n)
  ul<-phat+qnorm(0.975)*sqrt(phat*(1-phat)/n)
  mean(ll>p & ul>p)
})