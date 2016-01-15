
#solve the factor gain#
close1<-RemoveNA(gain)
kanyixia<-function(factor){
sum<-c(1:30)
for(i in 1:30){
l<-
      summary(lm(as.numeric(close1[i,])~as.numeric(size[i,-1])+as.numeric(value[i,-1])+as.numeric(momentum[i,-1])+as.numeric(growth[i,-1])+as.numeric(leverage[i,-1])+as.numeric(volatility[i,-1])+as.numeric(news[i,-1])+as.numeric(earningsyield[i,-1])+as.numeric(factor_bank)+as.numeric(factor_realestate)+as.numeric(factor_medic)+as.numeric(factor_restaurant)+as.numeric(factor_retail)+as.numeric(factor_machine)+as.numeric(factor_buildmaterial)+as.numeric(factor_homeappliance)+as.numeric(factor_cloth)+as.numeric(factor_food)+as.numeric(factor_electronic)+as.numeric(factor_transport)+as.numeric(factor_auto)+as.numeric(factor_lightindustry)+as.numeric(factor_electric)+as.numeric(factor_integrate)+as.numeric(factor_communicate)+as.numeric(factor_oil)+as.numeric(factor_coloredmetal)+as.numeric(factor_agriculture)+as.numeric(factor_architecture)+as.numeric(factor_computer)+as.numeric(factor_chemical)++as.numeric(factor_coal)+as.numeric(factor_electricequipment)+as.numeric(factor_military)+as.numeric(factor_nonbank)+as.numeric(factor_steel)+as.numeric(factor_media)))

sum[i]<-l$coefficients[2,1]
}
sum
}
zcf1_CLOSE_p
#------------------------------------------------#
calgain<-function(variables,i,factor){
      gain<-variables*as.numeric(factor[i,-1])
      gain<-removeNA(gain)
      gain
}

difference<-function(variables,i,factor,real){
      diff<-((real[i,-1]-calgain(variables,i,factor))^2)%*%t(t(sizeweight_l))
     # diff<-sum(((real[i,-1]-calgain(variables,i,factor))^2))
      diff
}
variables<-1
optimresult<-optim(variables,difference,i=1,factor=size,real=gain[-1,],method="Brent",lower=-5,upper=5)

subweight<-optimresult$par