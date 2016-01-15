regdatafun<-function(){
      r_extragain=as.numeric(gain[2,-1])-(as.numeric(zcf1_CLOSE_p[2])/100)
      r_beta=as.numeric(beta[1,-1])
      r_momentum=as.numeric(momentum[1,-1])
      r_size=as.numeric(size[1,-1])
      r_expectation=as.numeric(earningsyield[1,-1])
      r_volatility=as.numeric(volatility[1,-1])
      r_growth=as.numeric(growth[1,-1])
      r_value=as.numeric(value[1,-1])
      r_leverage=as.numeric(leverage[1,-1])
      r_news=as.numeric(news[1,-1])
      for(i in 2:(nrow(beta)-1)){
            r_extragain=c(r_extragain,as.numeric(gain[i+1,-1])-(as.numeric(zcf1_CLOSE_p[i+1])/100))
            r_beta=c(r_beta,as.numeric(beta[i,-1]))
            r_momentum=c(r_momentum,as.numeric(momentum[i,-1]))
            r_size=c(r_size,as.numeric(size[i,-1]))
            r_expectation=c(r_expectation,as.numeric(earningsyield[i,-1]))
            r_volatility=c(r_volatility,as.numeric(volatility[i,-1]))
            r_growth=c(r_growth,as.numeric(growth[i,-1]))
            r_value=c(r_value,as.numeric(value[i,-1]))
            r_leverage=c(r_leverage,as.numeric(leverage[i,-1]))
            r_news=c(r_news,as.numeric(news[i,-1]))
      }
      r_indus=factor(rep(as.character(stock_industry[3,]),(nrow(beta)-1)))
regdata<-data.frame(extragain=r_extragain,
                    beta=r_beta,
                    momentum=r_momentum,
                    size=r_size,
                    expectation=r_expectation,
                    volatility=r_volatility,
                    growth=r_growth,
                    value=r_value,
                    leverage=r_leverage,
                    news=r_news
                    ,industry=r_indus)
return(regdata)
}
regdatafun<-function(i){
      regdata<-data.frame(extragain=as.numeric(gain[i+1,-1])-(as.numeric(zcf1_CLOSE_p[i+1])/100),
                          beta=as.numeric(beta[i,-1]),
                          momentum=as.numeric(momentum[i,-1]),
                          size=as.numeric(size[i,-1]),
                          expectation=as.numeric(earningsyield[i,-1]),
                          volatility=as.numeric(volatility[i,-1]),
                          growth=as.numeric(growth[i,-1]),
                          value=as.numeric(value[i,-1]),
                          leverage=as.numeric(leverage[i,-1]),
                          news=as.numeric(news[i,-1])
      )##,r_indus=factor(as.character(stock_industry[3,])))
      return(regdata)
}

regdata<-regdatafun(1)
full.model<-lm(extragain~.,data=regdata)
best.model<-step(full.model,direction="backward")

summary(full.model)
plot(full.model)

#============================#
regplot<-function(i){
regdatafun<-function(i){
      regdata<-data.frame(extragain=as.numeric(gain[i+1,-1])-(as.numeric(zcf1_CLOSE_p[i+1])/100),
                          beta=as.numeric(beta[i,-1]),
                          momentum=as.numeric(momentum[i,-1]),
                          size=as.numeric(size[i,-1]),
                          expectation=as.numeric(earningsyield[i,-1]),
                          volatility=as.numeric(volatility[i,-1]),
                          growth=as.numeric(growth[i,-1]),
                          value=as.numeric(value[i,-1]),
                          leverage=as.numeric(leverage[i,-1]),
                          news=as.numeric(news[i,-1])
      ,indus=factor(as.character(stock_industry[3,])))
      return(regdata)
}
regdata<-regdatafun(i)
full.model<-lm(extragain~.,data=regdata)
# best.model<-step(full.model,direction="backward")
par(mfrow=c(2,2),mar=c(2,4,2,2))
plot(full.model)
return(summary(full.model))
}

library(manipulate)
manipulate(
      regplot(factor),
      factor=slider(1,32)
)

#Get data ready!#
{
      t<-regplot(1)
      betagain<-t$coefficients[1,1]
      momentumgain<-t$coefficients[2,1]
      sizegain<-t$coefficients[3,1]
      expectationgain<-t$coefficients[4,1]
      volatilitygain<-t$coefficients[5,1]
      growthgain<-t$coefficients[6,1]
      valuegain<-t$coefficients[7,1]
      leveragegain<-t$coefficients[8,1]
      newsgain<-t$coefficients[9,1]
      
      betat<-t$coefficients[1,3]
      momentumt<-t$coefficients[2,3]
      sizet<-t$coefficients[3,3]
      expectationt<-t$coefficients[4,3]
      volatilityt<-t$coefficients[5,3]
      growtht<-t$coefficients[6,3]
      valuet<-t$coefficients[7,3]
      leveraget<-t$coefficients[8,3]
      newst<-t$coefficients[9,3]
      
}
for(i in 2:(nrow(beta)-1)){
      t<-regplot(i)
      betagain<-c(betagain,t$coefficients[1,1])
      momentumgain<-c(momentumgain,t$coefficients[2,1])
      sizegain<-c(sizegain,t$coefficients[3,1])
      expectationgain<-c(expectationgain,t$coefficients[4,1])
      volatilitygain<-c(volatilitygain,t$coefficients[5,1])
      growthgain<-c(growthgain,t$coefficients[6,1])
      valuegain<-c(valuegain,t$coefficients[7,1])
      leveragegain<-c(leveragegain,t$coefficients[8,1])
      newsgain<-c(newsgain,t$coefficients[9,1])
      
      betat<-c(betat,t$coefficients[1,3])
      momentumt<-c(momentumt,t$coefficients[2,3])
      sizet<-c(sizet,t$coefficients[3,3])
      expectationt<-c(expectationt,t$coefficients[4,3])
      volatilityt<-c(volatilityt,t$coefficients[5,3])
      growtht<-c(growtht,t$coefficients[6,3])
      valuet<-c(valuet,t$coefficients[7,3])
      leveraget<-c(leveraget,t$coefficients[8,3])
      newst<-c(newst,t$coefficients[9,3])
}
{
      betaperform<-c(1,1+betagain[1])
      momentumperform<-c(1,1+momentumgain[1])
      sizeperform<-c(1,sizegain[1])
      expectationperform<-c(1,expectationgain[1])
      volatilityperform<-c(1,volatilitygain[1])
      growthperform<-c(1,growthgain[1])
      valueperform<-c(1,valuegain[1])
      leverageperform<-c(1,leveragegain[1])
      newsperform<-c(1,newsgain[1])
}
for(i in 2:(nrow(beta)-1)){
      betaperform<-c(betaperform,betagain[i])
      momentumperform<-c(momentumperform,momentumgain[i])
      sizeperform<-c(sizeperform,sizegain[i])
      expectationperform<-c(expectationperform,expectationgain[i])
      volatilityperform<-c(volatilityperform,volatilitygain[i])
      growthperform<-c(growthperform,growthgain[i])
      valueperform<-c(valueperform,valuegain[i])
      leverageperform<-c(leverageperform,leveragegain[i])
      newsperform<-c(newsperform,newsgain[i])
}
#===============#

t_and_perform<-function(factorname){
library(ggplot2)

qplot(substr(beta[-nrow(beta),1],3,7),get(paste0(factorname,"t")),geom=c("point","smooth"),main=factorname,method="loess",span=1/11)
qplot(substr(beta[-nrow(beta),1],3,7),get(paste0(factorname,"perform")),geom="line",main=factorname,col="blue")
}


