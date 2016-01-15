library(WindR)
w.start(showmenu=F)

#Public functions#
{
      savedata<-function(x){
            write.csv(get(x),file=paste0("C:\\Users\\shixi19\\Desktop\\FactorData\\",x,".csv"))
      }##give the name of the variable as a character 
      readdata<-function(x){
            data<-read.csv(file=paste0("C:\\Users\\shixi19\\Desktop\\FactorData\\",x,".csv"))
            data<-data[,-1]
            names(data)<-substr(names(data),2,10)
            return(data)
      }##give the name of the variable as a character 
      startdatefun<-function(y){
            year<-substr(Sys.Date(),1,4)
            year<-as.numeric(year)
            year<-year-y
            year<-as.character(year)
            startdate<-paste0(year,"0101")
            startdate}
      startdatefunwithhash<-function(y){
            year<-substr(Sys.Date(),1,4)
            year<-as.numeric(year)
            year<-year-y
            year<-as.character(year)
            startdate<-paste0(year,"-01-01")
            return(startdate)}
      startdatefunwithnohash<-function(y){
            year<-substr(Sys.Date(),1,4)
            year<-as.numeric(year)
            year<-year-y
            year<-as.character(year)
            startdate<-paste0(year,"0101")
            return(startdate)}
      datepoint<-function(y){
            close<-w.wsd("000001.SH","pct_chg",startdatefun(y),
                         Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
            plotdate<-as.character(close[,1])
            plotdate
      }
      #Read in all A stocks at the time of startdate and follow the industry 
      #classification of citic 
      stocklistfun<-function(y){
            startdate<-startdatefun(y)
            #       stocklist<-w.wset('SectorConstituent',
            #                         paste0('date=',startdate,';sectorId=a001010100000000'))
            #      stocklist<-w.wset('SectorConstituent',
            #                        paste0('date=',startdate,';windcode=000300.SH'))
            stocklist<-w.wset('SectorConstituent',
                              paste0('date=',startdate,';windcode=000906.SH'))
            stock<-stocklist$Data$wind_code#Get the code for all A stock
            stockname<-stocklist$Data$sec_name#Get the name for all A stock
            industrycode<-w.wss(stock,'industry_citiccode,industry_citic',
                                paste0('tradeDate=',startdate),'industryType=1')$Data$INDUSTRY_CITICCODE
            industryname<-w.wss(stock,'industry_citiccode,industry_citic',
                                paste0('tradeDate=',startdate),'industryType=1')$Data$INDUSTRY_CITIC
            stock_industry<-rbind(stock,industrycode,industryname)
            stock_industry<-stock_industry[,stock_industry[3,]!="NaN"]
            stock_industry
      }#Change the initial stock pool here!#
      removeNA<-function(data){
            data[is.na(data)]=0
            data
      }
      standard<-function(data){
            getmid<-apply(data[,-1],1,median,na.rm=TRUE)
            getmid<-removeNA(getmid)#Since we have set apply function as na.rm=TRUE,Here we can let         
            #na equals zero because they must come from those row with straight NA
            middistance<-abs(data[,-1]-getmid)
            mid<-apply(middistance,1,median,na.rm=TRUE)
            mid<-removeNA(mid)
            data1<-data[,-1]
            data1<-removeNA(data1)
            for(i in 1:length(getmid)){
                  
                  data1[i,data1[i,]-getmid[i]>3*abs(mid[i])]<-getmid[i]+3*abs(mid[i])
                  data1[i,data1[i,]-getmid[i]<(-3*abs(mid[i]))]<-getmid[i]+(-3*abs(mid[i]))
                  
            }
            data[,-1]<-data1
            getmean<-apply(data[,-1],1,mean,na.rm=TRUE)
            getmean<-removeNA(getmean)
            data[,-1]<-data[,-1]-getmean
            getvar<-apply(data[,-1],1,var,na.rm=TRUE)
            getvar[is.na(getvar)]<-1
            getvar[getvar==0]<-1
            data[,-1]<-data[,-1]/sqrt(getvar)
            data
      }
      RemoveNA<-function(x){
            x<-x[,-1]
            x[is.na(x)]<-0
            x
      }
      sizeweight<-function(stocklist){
            weight<-w.wss(stocklist,'total_shares',paste0('tradeDate=',Sys.Date()))$Data$TOTAL_SHARES
            tot<-sum(weight)
            weight<-weight/tot
            weight
      }
      #Gain#
      gainf<-function(y,stocklist){
            data<-w.wsd(stocklist,"pct_chg",startdatefunwithhash(y),Sys.Date()
                        ,"Period=M;Fill=Previous;PriceAdj=F")$Data
            data[,-1]<-data[,-1]/100
            data<-removeNA(data)
            data
      }
}
#Determine the public variables#
#=============================================================================#
y<-2
#This indicates how many years to look back which has to be less than 10
#Strongly suggest to set y<-2 since the data of expected level are available
#since 2013
#-----------------------------------------------------------------------------#
stock_industry<-stocklistfun(y)
stocklist<-stock_industry[1,]#Use it too often so that I list it seperately
num.industry<-as.numeric(length(unique(stock_industry[3,])))
num.stock<-as.numeric(length(stock_industry[1,]))
#checkdate<-datepoint(y)
savedata("stocklist")
sizeweight_l<-sizeweight(stocklist)
#Performance#
{
      gain<-gainf(y,stocklist)
}
savedata("gain")
#!Change the base index here!##There is another at the latter part!#
{
      zcf1_CLOSE_p<-w.wsd("000906.SH","pct_chg",startdatefunwithhash(y),
                          Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data$PCT_CHG
}
#=============================================================================#
#Put the factor here
#Example:
pvtfun<-function(y,stocklist){
      data<-w.wsd(stocklist,"PVT",startdatefunwithhash(y),Sys.Date()
                  ,"Period=M;Fill=Previous;PriceAdj=F")$Data
      #       print(paste0("beta missing value:",sum(is.na(data)),"/",nrow(data)*ncol(data)))
      data<-standard(data)
      data<-removeNA(data)
      data
}
#pvt#
{
      pvt<-pvtfun(y,stocklist)
}
savedata("pvt")
#=============================================================================#
#Analyze functions
plotfactor_t<-function(factor,groupnum){
      num_stock<-ncol(factor)-1
      num_periods<-nrow(factor)
      #Get startdate and end date
      year<-substr(Sys.Date(),1,4)
      year<-as.numeric(year)
      year<-year-y
      year<-as.character(year)
      startdate<-paste0(year,"-01-01")
      #end sub
      close<-gain
      plotdate<-as.character(close[,1])
      plotdate<-c(startdate,plotdate)
      plotdate<-plotdate[1:(length(plotdate)-1)]
      
      # zcf1_CLOSE_p<-w.wsd("000300.SH","pct_chg",factor[1,1],
      #                     Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data$PCT_CHG
      # #end sub#
      plotdata<-RemoveNA(factor)
      gain<-matrix(1,num_periods,groupnum)
      gain_index<-matrix(1,num_periods,1)
      adjustgain<-matrix(1,1,groupnum)
      for(i in 1:(num_periods-1)){
            gain_index[i+1]<-gain_index[i]*(1+zcf1_CLOSE_p[i+1]/100)
            #group<-cutree(hclust(dist(as.numeric(plotdata[i,]))),groupnum)
            group<-rep(1:groupnum,each=ceiling(length(close[i+1,-1])/groupnum))
            groupdata<-rbind(close[i+1,-1],plotdata[i,])
            groupdata<-groupdata[,order(groupdata[2,])]
            groupdata<-rbind(groupdata,group[1:length(close[i+1,-1])])
            m<-tapply(as.numeric(groupdata[1,]),
                      as.factor(as.numeric(groupdata[3,])),mean,na.rm=TRUE)
            for(j in 1:groupnum){
                  if(is.na(m[j])){
                        m[j]<-1
                        gain[i+1,j]<-gain[i,j]*m[j]
                  }else{
                        m[j]<-1+m[j]
                        gain[i+1,j]<-gain[i,j]*m[j]
                  }
            }
      }
      for(j in 1:groupnum){
            adjustgain[j]<-sqrt(var(gain[,j]))
      }
      output<-rbind(gain=gain[num_periods,],risk=adjustgain)
      par(mfrow=c(groupnum,1),mar=c(2,2,1,1))
      for(i in 1:groupnum){
            plot(as.Date(plotdate),gain[,i],type="p",ylim=c(0,max(max(gain_index),max(gain))))
            lines(as.Date(plotdate),gain[,i])
            lines(as.Date(plotdate),gain_index,col="red")
            legend("topleft",legend=as.character(i))
      }
      #       dev.off()
      output
}
regression<-function(factor,t_threshold){
      #================#
      plotdate<-as.character(factor[,1])
      count<-0
      sum_t<-0
      sum_r<-0
      regressdata<-RemoveNA(factor)
      num.col<-as.numeric(ncol(regressdata))
      num.row<-as.numeric(nrow(regressdata))
      close1<-RemoveNA(gain)
      timeline<-rep(0,(num.row-2))
      efficientrow<-num.row-2
      factorgain<-matrix(0,num.row,1)
      for(i in 1:(num.row-2)){
            if(sum(is.na(regressdata[i,]))>(0.5*length(regressdata[i,]))|sum(abs(regressdata[i,]))==0){
                  efficientrow<-efficientrow-1##To determine if this data is meaningful
                  timeline[i]<-1
            }
            else{
                  regressdata[is.na(regressdata)]<-0
                  lm_temp<-lm((as.numeric(close1[i,]))~as.numeric(regressdata[i,]),model=TRUE)
                  if(nrow(summary(lm_temp)$coefficients)>=1){
                        tvalue<-summary(lm_temp)$coefficients[2,3]
                        factorgain[i]<-summary(lm_temp)$coefficients[2,1]
                        if(abs(tvalue)>t_threshold){
                              count<-count+1
                              timeline[i]<-2
                        }
                        sum_t<-sum_t+abs(tvalue)
                        sum_r<-abs(summary(lm_temp)$r.squared)+sum_r
                  }else{
                        efficientrow<-efficientrow-1
                        timeline[i]<-1
                  }}}
      average<-sum_t/(efficientrow)
      count<-count/(efficientrow)
      sum_r<-sum_r/(efficientrow)
      gain_factor<-matrix(1,num.row-1,1)
      gain<-matrix(1,num.row-2,1)
      sum_f<-0
      for(i in 1:(num.row-2)){
            sum_f<-sum_f+factorgain[i]
            gain_factor[i+1]<-gain_factor[i]*(1+factorgain[i])
            gain[i]<-factorgain[i]
            
      }
      avg_f<-(sum_f/(efficientrow))
      avg_f<-((1+avg_f)^12-1)*100
      vol_f<-sqrt(var(gain[gain!=0]*100))*sqrt(12)
      out<-data.frame(average_t_value=average,
                      times_larger_than_threshold=count,rsquare=sum_r,
                      average_factor_gain_year=avg_f,
                      volatility_factor_gain=vol_f )
      #plot
      par(mfrow=c(1,1),mar=c(3,2,1,1))
      gain_index<-matrix(1,num.row-1,1)
      for(i in 1:(num.row-2)){
            gain_index[i+1]<-gain_index[i]*(1+zcf1_CLOSE_p[i+1]/100)
      }
      timeline<-c(1,timeline)
      plot(as.Date(plotdate[-1]),timeline,type="p",ylim=c(0,max(max(gain_index),max(gain_factor))))
      lines(as.Date(plotdate[-1]),gain_index,col="red")
      lines(as.Date(plotdate[-1]),gain_factor,col="blue")
      legend("topleft",col=c("red","blue"),lty=1,legend=c("index","factor"))
      points(as.Date(plotdate[-1]),timeline,type="p")
      print(out)
}

#=============================================================================#
#Analyze
#For example
print("beta")
plotfactor_t(beta,5)
regression(beta,2)




