library(parallel)
library(foreach)
library(doParallel)
library(truncnorm)
library(Rsolnp)
cl <- makeCluster(2)
registerDoParallel(cl)
testmulti <- foreach(x=1:30,.combine='rbind') %dopar% getporto(x)
stopCluster(cl)

getporto<-function(i){
      library(truncnorm)
      library(Rsolnp)
      print(i)
 gainfun<-function(w){
            -(as.numeric(w%*%as.numeric(gain[i,-1]))-zcf1_CLOSE_p[i]/100)
      }
 equalcon<-function(w){
            sum(w)
      }
# inequalcon<-function(w){
#             a<-(as.numeric((w)%*%as.numeric(beta[i,-1])))
#             b<-(as.numeric((w)%*%as.numeric(momentum[i,-1])))
#             c<-(as.numeric((w)%*%as.numeric(size[i,-1])))
#             d<-(as.numeric((w)%*%as.numeric(earningsyield[i,-1])))
#             e<-(as.numeric((w)%*%as.numeric(volatility[i,-1])))
#             f<-(as.numeric((w)%*%as.numeric(growth[i,-1])))
#             g<-(as.numeric((w)%*%as.numeric(value[i,-1])))
#             h<-(as.numeric((w)%*%as.numeric(leverage[i,-1])))
#             i<-(as.numeric((w)%*%as.numeric(news[i,-1])))
#             return(c(a,b,c,d,e,f,g,h,i))
#       }
inequalcon<-function(w){
      a<-(as.numeric((w)%*%as.numeric(beta[i,-1])))/(sizeweight_l%*%as.numeric(beta[i,-1]))-1
      b<-(as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1
      c<-(as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1
      d<-(as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1
      e<-(as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1
      f<-(as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1
      g<-(as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1
      h<-(as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1
      i<-(as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1
      return(c(a,b,c,d,e,f,g,h,i))
}
portofolio<-solnp(pars=rep(1/num.stock,num.stock),fun=gainfun
                        ,eqfun=equalcon,eqB=1,ineqfun=inequalcon
                        ,ineqLB=(c(-1,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5))
                        ,ineqUB=(c(1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5)),
                        LB=rep(0,num.stock),UB=rep(1,num.stock))
print(i)     
return(portofolio$par)
}
