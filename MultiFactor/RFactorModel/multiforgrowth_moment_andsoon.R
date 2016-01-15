library(parallel)
library(foreach)
library(doParallel)
library(truncnorm)
library(Rsolnp)

tindex<-as.numeric(tapply(as.numeric(indusweight[1,]),indusweight[4,],sum))
getporto_size<-function(i){
      library(truncnorm)
      library(Rsolnp)
      print(i)
      gainfun<-function(w){
            -(as.numeric(0.2*w%*%as.numeric(gain[i-2,-1])+0.3*w%*%as.numeric(gain[i-1,-1])+0.5*w%*%as.numeric(gain[i,-1]))-(0.5*zcf1_CLOSE_p[i]/100+0.3*zcf1_CLOSE_p[i-1]/100+0.2*zcf1_CLOSE_p[i-2]/100))
      }
      equalcon<-function(w){
            sum(w)
      }
      inequalcon<-function(w){
            a<-0.5*((as.numeric((w)%*%as.numeric(beta[i,-1])))/(sizeweight_l%*%as.numeric(beta[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(beta[i-1,-1])))/(sizeweight_l%*%as.numeric(beta[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(beta[i-2,-1])))/(sizeweight_l%*%as.numeric(beta[i-2,-1]))-1)
            b<-0.5*((as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(momentum[i-1,-1])))/(sizeweight_l%*%as.numeric(momentum[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(momentum[i-2,-1])))/(sizeweight_l%*%as.numeric(momentum[i-2,-1]))-1)
            c<-0.5*((as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(size[i-1,-1])))/(sizeweight_l%*%as.numeric(size[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(size[i-2,-1])))/(sizeweight_l%*%as.numeric(size[i-2,-1]))-1)
            d<-0.5*((as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(earningsyield[i-1,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(earningsyield[i-2,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-2,-1]))-1)
            e<-0.5*((as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(volatility[i-1,-1])))/(sizeweight_l%*%as.numeric(volatility[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(volatility[i-2,-1])))/(sizeweight_l%*%as.numeric(volatility[i-2,-1]))-1)
            f<-0.5*((as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(growth[i-1,-1])))/(sizeweight_l%*%as.numeric(growth[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(growth[i-2,-1])))/(sizeweight_l%*%as.numeric(growth[i-2,-1]))-1)
            g<-0.5*((as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(value[i-1,-1])))/(sizeweight_l%*%as.numeric(value[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(value[i-2,-1])))/(sizeweight_l%*%as.numeric(value[i-2,-1]))-1)
            h<-0.5*((as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(leverage[i-1,-1])))/(sizeweight_l%*%as.numeric(leverage[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(leverage[i-2,-1])))/(sizeweight_l%*%as.numeric(leverage[i-2,-1]))-1)
            i<-0.5*((as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(news[i-1,-1])))/(sizeweight_l%*%as.numeric(news[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(news[i-2,-1])))/(sizeweight_l%*%as.numeric(news[i-2,-1]))-1)
            tt<-rbind(as.numeric(w),indusweight[4,])
            treal<-as.numeric(tapply(as.numeric(tt[1,]),tt[2,],sum))
            industry<-sum(abs(treal-tindex))
            
            return(c(a,b,c,d,e,f,g,h,i,industry))
      }
      portofolio<-solnp(pars=rep(1/num.stock,num.stock),fun=gainfun
                        ,eqfun=equalcon,eqB=1,ineqfun=inequalcon
                        ,ineqLB=(c(-0.05,-1,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,0.01))
                        ,ineqUB=(c(0.05,1,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.01)),
                        LB=rep(0,num.stock),UB=rep(1,num.stock))
      print(i)     
      return(portofolio$par)
}
cl <- makeCluster(4)
registerDoParallel(cl)
testmulti_momentum <- foreach(x=3:32,.combine='rbind') %dopar% getporto_size(x)
stopCluster(cl)

getporto_value<-function(i){
      library(truncnorm)
      library(Rsolnp)
      print(i)
      gainfun<-function(w){
            -(as.numeric(0.2*w%*%as.numeric(gain[i-2,-1])+0.3*w%*%as.numeric(gain[i-1,-1])+0.5*w%*%as.numeric(gain[i,-1]))-(0.5*zcf1_CLOSE_p[i]/100+0.3*zcf1_CLOSE_p[i-1]/100+0.2*zcf1_CLOSE_p[i-2]/100))
      }
      equalcon<-function(w){
            sum(w)
      }
      inequalcon<-function(w){
            a<-0.5*((as.numeric((w)%*%as.numeric(beta[i,-1])))/(sizeweight_l%*%as.numeric(beta[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(beta[i-1,-1])))/(sizeweight_l%*%as.numeric(beta[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(beta[i-2,-1])))/(sizeweight_l%*%as.numeric(beta[i-2,-1]))-1)
            b<-0.5*((as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(momentum[i-1,-1])))/(sizeweight_l%*%as.numeric(momentum[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(momentum[i-2,-1])))/(sizeweight_l%*%as.numeric(momentum[i-2,-1]))-1)
            c<-0.5*((as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(size[i-1,-1])))/(sizeweight_l%*%as.numeric(size[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(size[i-2,-1])))/(sizeweight_l%*%as.numeric(size[i-2,-1]))-1)
            d<-0.5*((as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(earningsyield[i-1,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(earningsyield[i-2,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-2,-1]))-1)
            e<-0.5*((as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(volatility[i-1,-1])))/(sizeweight_l%*%as.numeric(volatility[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(volatility[i-2,-1])))/(sizeweight_l%*%as.numeric(volatility[i-2,-1]))-1)
            f<-0.5*((as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(growth[i-1,-1])))/(sizeweight_l%*%as.numeric(growth[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(growth[i-2,-1])))/(sizeweight_l%*%as.numeric(growth[i-2,-1]))-1)
            g<-0.5*((as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(value[i-1,-1])))/(sizeweight_l%*%as.numeric(value[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(value[i-2,-1])))/(sizeweight_l%*%as.numeric(value[i-2,-1]))-1)
            h<-0.5*((as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(leverage[i-1,-1])))/(sizeweight_l%*%as.numeric(leverage[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(leverage[i-2,-1])))/(sizeweight_l%*%as.numeric(leverage[i-2,-1]))-1)
            i<-0.5*((as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(news[i-1,-1])))/(sizeweight_l%*%as.numeric(news[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(news[i-2,-1])))/(sizeweight_l%*%as.numeric(news[i-2,-1]))-1)
            tt<-rbind(as.numeric(w),indusweight[4,])
            treal<-as.numeric(tapply(as.numeric(tt[1,]),tt[2,],sum))
            industry<-sum(abs(treal-tindex))
            
            return(c(a,b,c,d,e,f,g,h,i,industry))
      }
      portofolio<-solnp(pars=rep(1/num.stock,num.stock),fun=gainfun
                        ,eqfun=equalcon,eqB=1,ineqfun=inequalcon
                        ,ineqLB=(c(-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-1,-0.05,-0.05,0.01))
                        ,ineqUB=(c(0.05,0.05,0.05,0.05,0.05,0.05,1,0.05,0.05,0.01)),
                        LB=rep(0,num.stock),UB=rep(1,num.stock))
      print(i)     
      return(portofolio$par)
}
cl <- makeCluster(4)
registerDoParallel(cl)
testmulti_value <- foreach(x=3:32,.combine='rbind') %dopar% getporto_value(x)
stopCluster(cl)

getporto_expectation<-function(i){
      library(truncnorm)
      library(Rsolnp)
      print(i)
      gainfun<-function(w){
            -(as.numeric(0.2*w%*%as.numeric(gain[i-2,-1])+0.3*w%*%as.numeric(gain[i-1,-1])+0.5*w%*%as.numeric(gain[i,-1]))-(0.5*zcf1_CLOSE_p[i]/100+0.3*zcf1_CLOSE_p[i-1]/100+0.2*zcf1_CLOSE_p[i-2]/100))
      }
      equalcon<-function(w){
            sum(w)
      }
      inequalcon<-function(w){
            a<-0.5*((as.numeric((w)%*%as.numeric(beta[i,-1])))/(sizeweight_l%*%as.numeric(beta[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(beta[i-1,-1])))/(sizeweight_l%*%as.numeric(beta[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(beta[i-2,-1])))/(sizeweight_l%*%as.numeric(beta[i-2,-1]))-1)
            b<-0.5*((as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(momentum[i-1,-1])))/(sizeweight_l%*%as.numeric(momentum[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(momentum[i-2,-1])))/(sizeweight_l%*%as.numeric(momentum[i-2,-1]))-1)
            c<-0.5*((as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(size[i-1,-1])))/(sizeweight_l%*%as.numeric(size[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(size[i-2,-1])))/(sizeweight_l%*%as.numeric(size[i-2,-1]))-1)
            d<-0.5*((as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(earningsyield[i-1,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(earningsyield[i-2,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-2,-1]))-1)
            e<-0.5*((as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(volatility[i-1,-1])))/(sizeweight_l%*%as.numeric(volatility[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(volatility[i-2,-1])))/(sizeweight_l%*%as.numeric(volatility[i-2,-1]))-1)
            f<-0.5*((as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(growth[i-1,-1])))/(sizeweight_l%*%as.numeric(growth[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(growth[i-2,-1])))/(sizeweight_l%*%as.numeric(growth[i-2,-1]))-1)
            g<-0.5*((as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(value[i-1,-1])))/(sizeweight_l%*%as.numeric(value[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(value[i-2,-1])))/(sizeweight_l%*%as.numeric(value[i-2,-1]))-1)
            h<-0.5*((as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(leverage[i-1,-1])))/(sizeweight_l%*%as.numeric(leverage[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(leverage[i-2,-1])))/(sizeweight_l%*%as.numeric(leverage[i-2,-1]))-1)
            i<-0.5*((as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(news[i-1,-1])))/(sizeweight_l%*%as.numeric(news[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(news[i-2,-1])))/(sizeweight_l%*%as.numeric(news[i-2,-1]))-1)
            tt<-rbind(as.numeric(w),indusweight[4,])
            treal<-as.numeric(tapply(as.numeric(tt[1,]),tt[2,],sum))
            industry<-sum(abs(treal-tindex))
            
            return(c(a,b,c,d,e,f,g,h,i,industry))
      }
      portofolio<-solnp(pars=rep(1/num.stock,num.stock),fun=gainfun
                        ,eqfun=equalcon,eqB=1,ineqfun=inequalcon
                        ,ineqLB=(c(-0.05,-0.05,-0.05,-1,-0.05,-0.05,-0.05,-0.05,-0.05,0.01))
                        ,ineqUB=(c(0.05,0.05,0.05,1,0.05,0.05,0.05,0.05,0.05,0.01)),
                        LB=rep(0,num.stock),UB=rep(1,num.stock))
      print(i)     
      return(portofolio$par)
}
cl <- makeCluster(4)
registerDoParallel(cl)
testmulti_expectation<- foreach(x=3:32,.combine='rbind') %dopar% getporto_expectation(x)
stopCluster(cl)

getporto_final<-function(i){
      library(truncnorm)
      library(Rsolnp)
      print(i)
      gainfun<-function(w){
            -(as.numeric(0.2*w%*%as.numeric(gain[i-2,-1])+0.3*w%*%as.numeric(gain[i-1,-1])+0.5*w%*%as.numeric(gain[i,-1]))-(0.5*zcf1_CLOSE_p[i]/100+0.3*zcf1_CLOSE_p[i-1]/100+0.2*zcf1_CLOSE_p[i-2]/100))
      }
      equalcon<-function(w){
            sum(w)
      }
      inequalcon<-function(w){
            a<-0.5*((as.numeric((w)%*%as.numeric(beta[i,-1])))/(sizeweight_l%*%as.numeric(beta[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(beta[i-1,-1])))/(sizeweight_l%*%as.numeric(beta[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(beta[i-2,-1])))/(sizeweight_l%*%as.numeric(beta[i-2,-1]))-1)
            b<-0.5*((as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(momentum[i-1,-1])))/(sizeweight_l%*%as.numeric(momentum[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(momentum[i-2,-1])))/(sizeweight_l%*%as.numeric(momentum[i-2,-1]))-1)
            c<-0.5*((as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(size[i-1,-1])))/(sizeweight_l%*%as.numeric(size[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(size[i-2,-1])))/(sizeweight_l%*%as.numeric(size[i-2,-1]))-1)
            d<-0.5*((as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(earningsyield[i-1,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(earningsyield[i-2,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-2,-1]))-1)
            e<-0.5*((as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(volatility[i-1,-1])))/(sizeweight_l%*%as.numeric(volatility[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(volatility[i-2,-1])))/(sizeweight_l%*%as.numeric(volatility[i-2,-1]))-1)
            f<-0.5*((as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(growth[i-1,-1])))/(sizeweight_l%*%as.numeric(growth[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(growth[i-2,-1])))/(sizeweight_l%*%as.numeric(growth[i-2,-1]))-1)
            g<-0.5*((as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(value[i-1,-1])))/(sizeweight_l%*%as.numeric(value[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(value[i-2,-1])))/(sizeweight_l%*%as.numeric(value[i-2,-1]))-1)
            h<-0.5*((as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(leverage[i-1,-1])))/(sizeweight_l%*%as.numeric(leverage[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(leverage[i-2,-1])))/(sizeweight_l%*%as.numeric(leverage[i-2,-1]))-1)
            i<-0.5*((as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(news[i-1,-1])))/(sizeweight_l%*%as.numeric(news[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(news[i-2,-1])))/(sizeweight_l%*%as.numeric(news[i-2,-1]))-1)
            tt<-rbind(as.numeric(w),indusweight[4,])
            treal<-as.numeric(tapply(as.numeric(tt[1,]),tt[2,],sum))
            industry<-sum(abs(treal-tindex))
            
            return(c(a,b,c,d,e,f,g,h,i,industry))
      }
      portofolio<-solnp(pars=rep(1/num.stock,num.stock),fun=gainfun
                        ,eqfun=equalcon,eqB=1,ineqfun=inequalcon
                        ,ineqLB=(c(-0.05,-1,-0.05,-0.5,-0.05,-0.05,-1,-0.05,-0.05,0.01))
                        ,ineqUB=(c(0.05,1,0.05,0.5,0.05,0.05,1,0.05,0.05,0.01)),
                        LB=rep(0,num.stock),UB=rep(1,num.stock))
      print(i)     
      return(portofolio$par)
}
cl <- makeCluster(4)
registerDoParallel(cl)
testmulti_final<- foreach(x=3:32,.combine='rbind') %dopar% getporto_final(x)
stopCluster(cl)





getporto_growth<-function(i){
      library(truncnorm)
      library(Rsolnp)
      print(i)
      gainfun<-function(w){
            -(as.numeric(0.2*w%*%as.numeric(gain[i-2,-1])+0.3*w%*%as.numeric(gain[i-1,-1])+0.5*w%*%as.numeric(gain[i,-1]))-(0.5*zcf1_CLOSE_p[i]/100+0.3*zcf1_CLOSE_p[i-1]/100+0.2*zcf1_CLOSE_p[i-2]/100))
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
            #  a<-0.5*((as.numeric((w)%*%as.numeric(beta[i,-1])))/(sizeweight_l%*%as.numeric(beta[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(beta[i-1,-1])))/(sizeweight_l%*%as.numeric(beta[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(beta[i-2,-1])))/(sizeweight_l%*%as.numeric(beta[i-2,-1]))-1)
            b<-0.5*((as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(momentum[i-1,-1])))/(sizeweight_l%*%as.numeric(momentum[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(momentum[i-2,-1])))/(sizeweight_l%*%as.numeric(momentum[i-2,-1]))-1)
            c<-0.5*((as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(size[i-1,-1])))/(sizeweight_l%*%as.numeric(size[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(size[i-2,-1])))/(sizeweight_l%*%as.numeric(size[i-2,-1]))-1)
            d<-0.5*((as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(earningsyield[i-1,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(earningsyield[i-2,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-2,-1]))-1)
            e<-0.5*((as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(volatility[i-1,-1])))/(sizeweight_l%*%as.numeric(volatility[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(volatility[i-2,-1])))/(sizeweight_l%*%as.numeric(volatility[i-2,-1]))-1)
            f<-0.5*((as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(growth[i-1,-1])))/(sizeweight_l%*%as.numeric(growth[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(growth[i-2,-1])))/(sizeweight_l%*%as.numeric(growth[i-2,-1]))-1)
            g<-0.5*((as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(value[i-1,-1])))/(sizeweight_l%*%as.numeric(value[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(value[i-2,-1])))/(sizeweight_l%*%as.numeric(value[i-2,-1]))-1)
            h<-0.5*((as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(leverage[i-1,-1])))/(sizeweight_l%*%as.numeric(leverage[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(leverage[i-2,-1])))/(sizeweight_l%*%as.numeric(leverage[i-2,-1]))-1)
            i<-0.5*((as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(news[i-1,-1])))/(sizeweight_l%*%as.numeric(news[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(news[i-2,-1])))/(sizeweight_l%*%as.numeric(news[i-2,-1]))-1)
            
            #             b<-(as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1
            #             c<-(as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1
            #             d<-(as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1
            #             e<-(as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1
            #             f<-(as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1
            #             g<-(as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1
            #             h<-(as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1
            #             i<-(as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1
            return(c(b,c,d,e,f,g,h,i))
      }
      portofolio<-solnp(pars=rep(1/num.stock,num.stock),fun=gainfun
                        ,eqfun=equalcon,eqB=1,ineqfun=inequalcon
                        ,ineqLB=(c(-0.05,-0.05,-0.05,-0.05,-1,-0.05,-0.05,-0.05))
                        ,ineqUB=(c(0.05,0.05,0.05,0.05,1,0.05,0.05,0.05)),
                        LB=rep(0,num.stock),UB=rep(1,num.stock))
      print(i)     
      return(portofolio$par)
}
cl <- makeCluster(4)
registerDoParallel(cl)
testmulti_growth <- foreach(x=3:32,.combine='rbind') %dopar% getporto_size(x)
stopCluster(cl)

getporto_momentum<-function(i){
      library(truncnorm)
      library(Rsolnp)
      print(i)
      gainfun<-function(w){
            -(as.numeric(0.2*w%*%as.numeric(gain[i-2,-1])+0.3*w%*%as.numeric(gain[i-1,-1])+0.5*w%*%as.numeric(gain[i,-1]))-(0.5*zcf1_CLOSE_p[i]/100+0.3*zcf1_CLOSE_p[i-1]/100+0.2*zcf1_CLOSE_p[i-2]/100))
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
            #  a<-0.5*((as.numeric((w)%*%as.numeric(beta[i,-1])))/(sizeweight_l%*%as.numeric(beta[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(beta[i-1,-1])))/(sizeweight_l%*%as.numeric(beta[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(beta[i-2,-1])))/(sizeweight_l%*%as.numeric(beta[i-2,-1]))-1)
            b<-0.5*((as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(momentum[i-1,-1])))/(sizeweight_l%*%as.numeric(momentum[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(momentum[i-2,-1])))/(sizeweight_l%*%as.numeric(momentum[i-2,-1]))-1)
            c<-0.5*((as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(size[i-1,-1])))/(sizeweight_l%*%as.numeric(size[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(size[i-2,-1])))/(sizeweight_l%*%as.numeric(size[i-2,-1]))-1)
            d<-0.5*((as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(earningsyield[i-1,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(earningsyield[i-2,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i-2,-1]))-1)
            e<-0.5*((as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(volatility[i-1,-1])))/(sizeweight_l%*%as.numeric(volatility[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(volatility[i-2,-1])))/(sizeweight_l%*%as.numeric(volatility[i-2,-1]))-1)
            f<-0.5*((as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(growth[i-1,-1])))/(sizeweight_l%*%as.numeric(growth[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(growth[i-2,-1])))/(sizeweight_l%*%as.numeric(growth[i-2,-1]))-1)
            g<-0.5*((as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(value[i-1,-1])))/(sizeweight_l%*%as.numeric(value[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(value[i-2,-1])))/(sizeweight_l%*%as.numeric(value[i-2,-1]))-1)
            h<-0.5*((as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(leverage[i-1,-1])))/(sizeweight_l%*%as.numeric(leverage[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(leverage[i-2,-1])))/(sizeweight_l%*%as.numeric(leverage[i-2,-1]))-1)
            i<-0.5*((as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1)+0.3*((as.numeric((w)%*%as.numeric(news[i-1,-1])))/(sizeweight_l%*%as.numeric(news[i-1,-1]))-1)+0.2*((as.numeric((w)%*%as.numeric(news[i-2,-1])))/(sizeweight_l%*%as.numeric(news[i-2,-1]))-1)
            
            #             b<-(as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1
            #             c<-(as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1
            #             d<-(as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1
            #             e<-(as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1
            #             f<-(as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1
            #             g<-(as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1
            #             h<-(as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1
            #             i<-(as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1
            return(c(b,c,d,e,f,g,h,i))
      }
      portofolio<-solnp(pars=rep(1/num.stock,num.stock),fun=gainfun
                        ,eqfun=equalcon,eqB=1,ineqfun=inequalcon
                        ,ineqLB=(c(-1,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05))
                        ,ineqUB=(c(1,0.05,0.05,0.05,0.05,0.05,0.05)),
                        LB=rep(0,num.stock),UB=rep(1,num.stock))
      print(i)     
      return(portofolio$par)
}
cl <- makeCluster(4)
registerDoParallel(cl)
testmulti_momentum <- foreach(x=3:32,.combine='rbind') %dopar% getporto_size(x)
stopCluster(cl)

# getporto_growth<-function(i){
#       library(truncnorm)
#       library(Rsolnp)
#       print(i)
#       gainfun<-function(w){
#             -(as.numeric(w%*%as.numeric(gain[i,-1]))-zcf1_CLOSE_p[i]/100)
#       }
#       equalcon<-function(w){
#             sum(w)
#       }
#       # inequalcon<-function(w){
#       #             a<-(as.numeric((w)%*%as.numeric(beta[i,-1])))
#       #             b<-(as.numeric((w)%*%as.numeric(momentum[i,-1])))
#       #             c<-(as.numeric((w)%*%as.numeric(size[i,-1])))
#       #             d<-(as.numeric((w)%*%as.numeric(earningsyield[i,-1])))
#       #             e<-(as.numeric((w)%*%as.numeric(volatility[i,-1])))
#       #             f<-(as.numeric((w)%*%as.numeric(growth[i,-1])))
#       #             g<-(as.numeric((w)%*%as.numeric(value[i,-1])))
#       #             h<-(as.numeric((w)%*%as.numeric(leverage[i,-1])))
#       #             i<-(as.numeric((w)%*%as.numeric(news[i,-1])))
#       #             return(c(a,b,c,d,e,f,g,h,i))
#       #       }
#       inequalcon<-function(w){
#             a<-(as.numeric((w)%*%as.numeric(beta[i,-1])))/(sizeweight_l%*%as.numeric(beta[i,-1]))-1
#             b<-(as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1
#             c<-(as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1
#             d<-(as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1
#             e<-(as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1
#             f<-(as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1
#             g<-(as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1
#             h<-(as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1
#             i<-(as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1
#             return(c(a,b,c,d,e,f,g,h,i))
#       }
#       portofolio<-solnp(pars=rep(1/num.stock,num.stock),fun=gainfun
#                         ,eqfun=equalcon,eqB=1,ineqfun=inequalcon
#                         ,ineqLB=(c(-0.05,-0.05,-0.05,-0.05,-0.05,-1,-0.05,-0.05,-0.05))
#                         ,ineqUB=(c(0.05,0.05,0.05,0.05,0.05,1,0.05,0.05,0.05)),
#                         LB=rep(0,num.stock),UB=rep(1,num.stock))
#       print(i)     
#       return(portofolio$par)
# }
# cl <- makeCluster(4)
# registerDoParallel(cl)
# testmulti_growth <- foreach(x=1:30,.combine='rbind') %dopar% getporto_growth(x)
# stopCluster(cl)

getporto_momentum<-function(i){
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
           # a<-(as.numeric((w)%*%as.numeric(beta[i,-1])))/(sizeweight_l%*%as.numeric(beta[i,-1]))-1
            b<-(as.numeric((w)%*%as.numeric(momentum[i,-1])))/(sizeweight_l%*%as.numeric(momentum[i,-1]))-1
            c<-(as.numeric((w)%*%as.numeric(size[i,-1])))/(sizeweight_l%*%as.numeric(size[i,-1]))-1
            d<-(as.numeric((w)%*%as.numeric(earningsyield[i,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i,-1]))-1
            e<-(as.numeric((w)%*%as.numeric(volatility[i,-1])))/(sizeweight_l%*%as.numeric(volatility[i,-1]))-1
            f<-(as.numeric((w)%*%as.numeric(growth[i,-1])))/(sizeweight_l%*%as.numeric(growth[i,-1]))-1
            g<-(as.numeric((w)%*%as.numeric(value[i,-1])))/(sizeweight_l%*%as.numeric(value[i,-1]))-1
            h<-(as.numeric((w)%*%as.numeric(leverage[i,-1])))/(sizeweight_l%*%as.numeric(leverage[i,-1]))-1
            i<-(as.numeric((w)%*%as.numeric(news[i,-1])))/(sizeweight_l%*%as.numeric(news[i,-1]))-1
            return(c(b,c,d,e,f,g,h,i))
      }
      portofolio<-solnp(pars=rep(1/num.stock,num.stock),fun=gainfun
                        ,eqfun=equalcon,eqB=1,ineqfun=inequalcon
                        ,ineqLB=(c(-1,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05,-0.05))
                        ,ineqUB=(c(1,0.05,0.05,0.05,0.05,0.05,0.05,0.05)),
                        LB=rep(0,num.stock),UB=rep(1,num.stock))
      print(i)     
      return(portofolio$par)
}
cl <- makeCluster(4)
registerDoParallel(cl)
testmulti_momentum <- foreach(x=3:32,.combine='rbind') %dopar% getporto_momentum(x)
stopCluster(cl)

#-----------------------
gain_extra<-rep(1,(nrow(size)-2))
factor_actual<-matrix(0,(nrow(size)-2),9)
for(i in 1:(nrow(size)-2)){ 
      gainfun_real<-function(w){
            -(as.numeric(w%*%as.numeric(gain[i+1,-1]))-zcf1_CLOSE_p[i+1]/100)
      }
      inequalcon_real<-function(w){
            a<-(as.numeric((w)%*%as.numeric(beta[i+1,-1])))/(sizeweight_l%*%as.numeric(beta[i+1,-1]))-1
            b<-(as.numeric((w)%*%as.numeric(momentum[i+1,-1])))/(sizeweight_l%*%as.numeric(momentum[i+1,-1]))-1
            c<-(as.numeric((w)%*%as.numeric(size[i+1,-1])))/(sizeweight_l%*%as.numeric(size[i+1,-1]))-1
            d<-(as.numeric((w)%*%as.numeric(earningsyield[i+1,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i+1,-1]))-1
            e<-(as.numeric((w)%*%as.numeric(volatility[i+1,-1])))/(sizeweight_l%*%as.numeric(volatility[i+1,-1]))-1
            f<-(as.numeric((w)%*%as.numeric(growth[i+1,-1])))/(sizeweight_l%*%as.numeric(growth[i+1,-1]))-1
            g<-(as.numeric((w)%*%as.numeric(value[i+1,-1])))/(sizeweight_l%*%as.numeric(value[i+1,-1]))-1
            h<-(as.numeric((w)%*%as.numeric(leverage[i+1,-1])))/(sizeweight_l%*%as.numeric(leverage[i+1,-1]))-1
            i<-(as.numeric((w)%*%as.numeric(news[i+1,-1])))/(sizeweight_l%*%as.numeric(news[i+1,-1]))-1
            return(c(a,b,c,d,e,f,g,h,i))
      }
      factor_actual[i,]<-inequalcon_real(testmulti_momentum[i,])#actual beta
      gain_extra[i]<-(-gainfun_real(testmulti_momentum[i,]))#actual gain
}
factor_avg<-apply(factor_actual,2,mean)
gain_avg<-sum(gain_extra)/(nrow(beta)-2)
#plot
par(mfrow=c(1,1),mar=c(3,2,1,1))
gain_index<-matrix(1,num.row-1,1)
gain_factor<-matrix(1,num.row-1,1)
for(j in 1:(num.row-2)){
      gain_index[j+1]<-gain_index[j]*(1+zcf1_CLOSE_p[j+1]/100)
      gain_factor[j+1]<-gain_factor[j]*(1+gain_extra[j])
}
for(k in 1:length(factor_actual)){
      if(factor_actual[k]>=0){
            factor_actual[k]<-1
      }else{
            factor_actual[k]<-0
      }
      
}
factor_actual<-c(0.5,factor_actual)
for(k in 1:length(gain_extra)){
      if(gain_extra[k]>=0){
            gain_extra[k]<-1
      }else{
            gain_extra[k]<-0
      }
      
}
winratio<-sum(gain_extra)/length(gain_extra)
out<-data.frame(factor_actual=factor_avg,gain_extra=gain_avg,win_ratio=winratio)
gain_extra<-c(0.5,gain_extra)
plot(as.Date(plotdate[-1]),gain_extra,type="p",ylim=c(-0.5,3),pch=45,col="red",xlab="",ylab="",main="beta portofolio")
lines(as.Date(plotdate[-1]),gain_index,col="red")
lines(as.Date(plotdate[-1]),gain_factor,col="blue")
legend("topleft",col=c("red","blue"),lty=1,legend=c("index","factor"))
print(out)