
biaohao<-function(factor,groupnum){
order<-1:num.stock
groupdata<-rbind(factor,order)
group<-rep(1:groupnum,each=ceiling(num.stock/groupnum))
groupdata<-groupdata[,order(groupdata[1,])]
groupdata<-rbind(groupdata,group[1:num.stock])
groupdata<-groupdata[,order(groupdata[2,])]
groupdata<-groupdata[3,]
return(groupdata)
}
standard<-function(data){
      getmean<-apply(data[,-1],1,mean,na.rm=TRUE)
      getmid<-apply(data[,-1],1,median,na.rm=TRUE)
      data1<-data[,-1]
      for(i in 1:length(getmid)){
            
            data1[i,data1[i,]>5.2*abs(getmid[i])]<-5.2*abs(getmid[i])
            data1[i,data1[i,]<(-5.2*abs(getmid[i]))]<-(-5.2*abs(getmid[i]))
            
      }
      data[,-1]<-data1
      data[,-1]<-data[,-1]-getmean
      getvar<-apply(data[,-1],1,var,na.rm=TRUE)
      data[,-1]<-data[,-1]/sqrt(getvar)
      data
}
beta<-function(y,stocklist){
      data<-w.wsd(stocklist,"beta_100w",startdatefunwithhash(y),Sys.Date()
                  ,"Period=W;Fill=Previous;PriceAdj=F")$Data
    #  data<-removeNA(data)
   #   data<-standard(data)
      data<-removeNA(data)
      data
}

beta<-beta(y,stocklist)

beta[,-1]<-apply(beta[,-1],1,biaohao,groupnum=10)