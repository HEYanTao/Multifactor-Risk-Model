x<-matrix(1,5,1)
x[1]<-0.8
x[2]<-1
x[3]<-1.1
x[4]<-1.2
x[5]<-1.3

w<-matrix(0.5,1,5)

pars<-w
#start here

fun<-function(w){
      -as.numeric(w%*%x)
}
beta<-matrix(1,5,1)
beta[1]<-1
beta[2]<-(-1)
beta[3]<-1
beta[4]<-1
beta[5]<-(-2)

theta<-2*beta

inequal<-function(w){
      a<-as.numeric(w%*%beta)
      b<-as.numeric(w%*%theta)
      c(a,b)
}
equal<-function(w){
      sum(w)
}

result_testonly<-solnp(pars=pars,fun=fun,eqfun=equal,eqB=1,ineqfun=inequal
                       ,ineqLB=(c(-0.05,-0.1)),ineqUB=(c(0.05,0.1)),LB=rep(0,5),UB=rep(1,5))
