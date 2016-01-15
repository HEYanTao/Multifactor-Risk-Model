#This is the multi-factor stock picking system developed by MarkHE
#Preparation
setwd("C:\\Users\\shixi19\\Desktop\\RFactorModel")
library(WindR)
w.start(showmenu=F)

#Set the stocks to calculate
y<-3#use this variable to determine how many years to look back

stocklist<-w.wset('SectorConstituent','date=20150813;sectorId=a001010100000000;
                  field=wind_code,sec_name')
stock<-stocklist$Data$wind_code#Get the code for all A stock
stockname<-stocklist$Data$sec_name#Get the name for all A stock
##=================================================================##
#Read in data 

#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
num_factors<-0##Indicate how many factors are taken into consideration
#------------------------------------------------------------#
TotalShare<-w.wsd(stock,"share_totala",startdate,Sys.Date(),
                  "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
#-------------------------------------------------------------#
PE<-w.wsd(stock,"pe_ttm",startdate,Sys.Date(),
          "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
#------------------------------------------------------------#
PB<-w.wsd(stock,"pb_lf",startdate,Sys.Date(),
          "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-------------------------------------------------------------#
#sub to move to the end of the year
year1<-substr(startdate,1,4)
enddate<-paste0(year1,"-12-31")
#end sub
EPE<-w.wsd(stock,"pe_est",startdate,enddate,
           paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
for (i in 2:y-1){
      #sub to add one year!
      year1<-substr(startdate,1,4)
      year1<-as.numeric(year1)
      year1<-year1+1
      year1<-as.character(year1)
      startdate<-paste0(year1,"-01-01")
      #sub end
      #sub to move to the end of the year
      year1<-substr(startdate,1,4)
      enddate<-paste0(year1,"-12-31")
      #end sub
      #sub to add one year
      year<-as.numeric(year)
      year<-year+1
      year<-as.character(year)
      #end sub
      Temp<-w.wsd(stock,"pe_est",startdate,enddate,
                paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
      EPE<-rbind(EPE,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"pe_est",startdate,enddate,
            paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
EPE<-rbind(EPE,Temp)
num_factors<-num_factors+1
##------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
#sub to move to the end of the year
year1<-substr(startdate,1,4)
enddate<-paste0(year1,"-12-31")
#end sub
EPB<-w.wsd(stock,"estpb",startdate,enddate,
           paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
for (i in 2:y-1){
      #sub to add one year!
      year1<-substr(startdate,1,4)
      year1<-as.numeric(year1)
      year1<-year1+1
      year1<-as.character(year1)
      startdate<-paste0(year1,"-01-01")
      #sub end
      #sub to move to the end of the year
      year1<-substr(startdate,1,4)
      enddate<-paste0(year1,"-12-31")
      #end sub
      #sub to add one year
      year<-as.numeric(year)
      year<-year+1
      year<-as.character(year)
      #end sub
      Temp<-w.wsd(stock,"estpb",startdate,enddate,
                  paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
      EPB<-rbind(EPB,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"estpb",startdate,enddate,
            paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
EPB<-rbind(EPB,Temp)
num_factors<-num_factors+1
##-------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
#sub to move to the end of the year
year1<-substr(startdate,1,4)
enddate<-paste0(year1,"-12-31")
#end sub
TURN<-w.wsd(stock,"turn",startdate,enddate,
           paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
for (i in 2:y-1){
      #sub to add one year!
      year1<-substr(startdate,1,4)
      year1<-as.numeric(year1)
      year1<-year1+1
      year1<-as.character(year1)
      startdate<-paste0(year1,"-01-01")
      #sub end
      #sub to move to the end of the year
      year1<-substr(startdate,1,4)
      enddate<-paste0(year1,"-12-31")
      #end sub
      #sub to add one year
      year<-as.numeric(year)
      year<-year+1
      year<-as.character(year)
      #end sub
      Temp<-w.wsd(stock,"turn",startdate,enddate,
                  paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
      TURN<-rbind(TURN,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"turn",startdate,enddate,
            paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
TURN<-rbind(TURN,Temp)

#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
#sub to move to the end of the year
year1<-substr(startdate,1,4)
enddate<-paste0(year1,"-12-31")
#end sub
TURN<-w.wsd(stock,"turn",startdate,enddate,
            paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
for (i in 2:y-1){
      #sub to add one year!
      year1<-substr(startdate,1,4)
      year1<-as.numeric(year1)
      year1<-year1+1
      year1<-as.character(year1)
      startdate<-paste0(year1,"-01-01")
      #sub end
      #sub to move to the end of the year
      year1<-substr(startdate,1,4)
      enddate<-paste0(year1,"-12-31")
      #end sub
      #sub to add one year
      year<-as.numeric(year)
      year<-year+1
      year<-as.character(year)
      #end sub
      Temp<-w.wsd(stock,"turn",startdate,enddate,
                  paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
      TURN<-rbind(TURN,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"turn",startdate,enddate,
            paste("year=",year,";Period=Q;Fill=Previous;PriceAdj=F"))$Data
TURN<-rbind(TURN,Temp)
num_factors<-num_factors+1
##--------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"0101")
enddate<-paste0(year,"0401")
#end sub
VOL<-EPB
TEMP<-w.wss(stock,'stdevry',paste0('startDate=',startdate),
             paste0('endDate=',enddate),'period=1','returnType=1')$Data
VOL[1,-1]<-t(TEMP$STDEVR)
accumulation<-1
while(as.numeric(substr(enddate,1,4))<as.numeric(substr(Sys.Date(),1,4))|
            as.numeric(substr(enddate,5,6))+3<as.numeric(substr(Sys.Date(),6,7)))
      {
      accumulation<-accumulation+1
      #sub to move to the next quarter
      year2<-substr(startdate,1,4)
      year2<-as.numeric(year2)
      month1<-substr(startdate,5,6)
      monthend<-month1
      day1<-substr(startdate,7,8)
      dayend<-day1
      switch(month1,
             "01"={month1<-"04";monthend<-"06";dayend<-"30"},
             "04"={month1<-"07";monthend<-"09";dayend<-"30"},
             "07"={month1<-"10";monthend<-"12";dayend<-"31"},
             "10"={month1<-"01";year2<-year2+1;monthend<-"03";dayend<-"31"}
      )
      year2<-as.character(year2)
      startdate<-paste0(year2,month1,"01")
      enddate<-paste0(year2,monthend,dayend)   
      #end sub
      TEMP<-w.wss(stock,'stdevry',paste0('startDate=',startdate),
                     paste0('endDate=',enddate),'period=1','returnType=1')$Data
      VOL[accumulation,-1]<-t(TEMP$STDEVR)      
}
#sub to move to the next quarter
year2<-substr(startdate,1,4)
year2<-as.numeric(year2)
month1<-substr(startdate,5,6)
monthend<-month1
day1<-substr(startdate,7,8)
dayend<-day1
switch(month1,
       "01"={month1<-"04";monthend<-"06";dayend<-"30"},
       "04"={month1<-"07";monthend<-"09";dayend<-"30"},
       "07"={month1<-"10";monthend<-"12";dayend<-"31"},
       "10"={month1<-"01";year2<-year2+1;monthend<-"03";dayend<-"31"}
)
year2<-as.character(year2)
startdate<-paste0(year2,month1,"01")
enddate<-as.character(Sys.Date())   
enddate<-gsub("-","",enddate)
#end sub
TEMP<-w.wss(stock,'stdevry',paste0('startDate=',startdate),
            paste0('endDate=',enddate),'period=1','returnType=1')$Data
VOL[accumulation+1,-1]<-t(TEMP$STDEVR)
num_factors<-num_factors+1
##---------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
CH3<-w.wsd(stock,"pct_chg",startdate,Sys.Date(),
           "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-----------------------------------------------------------------##
CH1<-CH3
CH1date<-CH1$DATETIME
#Get startdate and end date
for(i in 1:nrow(CH1)){
      enddate<-CH1date[i]
      CH1[i,-1]<-w.wsd(stock,"pct_chg","ED-1M",as.character(enddate),
                 "Period=M;Fill=Previous;PriceAdj=F")$Data[2,-1]}
num_factors<-num_factors+1
##----------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
PROGR<-w.wsd(stock,"yoyprofit",startdate,Sys.Date()
          ,"Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-----------------------------------------------------------------##
REVGR<-w.wsd(stock,"yoy_or",startdate,Sys.Date()
        ,"Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-----------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
#sub to move to the end of the year
year1<-substr(startdate,1,4)
enddate<-paste0(year1,"-12-31")
#end sub
EPROGR<-w.wsd(stock,"west_yoynetprofit",startdate,enddate,
paste0(paste0("year=",year),";westPeriod=180;Period=Q;Fill=Previous;PriceAdj=F"))$Data
for (i in 2:y-1){
      #sub to add one year!
      year1<-substr(startdate,1,4)
      year1<-as.numeric(year1)
      year1<-year1+1
      year1<-as.character(year1)
      startdate<-paste0(year1,"-01-01")
      #sub end
      #sub to move to the end of the year
      year1<-substr(startdate,1,4)
      enddate<-paste0(year1,"-12-31")
      #end sub
      #sub to add one year
      year<-as.numeric(year)
      year<-year+1
      year<-as.character(year)
      #end sub
      Temp<-w.wsd(stock,"west_yoynetprofit",startdate,enddate,
       paste0(paste0("year=",year),";westPeriod=180;Period=Q;Fill=Previous;PriceAdj=F"))$Data
      EPROGR<-rbind(EPROGR,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"west_yoynetprofit",startdate,enddate,
             paste0(paste0("year=",year),";westPeriod=180;Period=Q;Fill=Previous;PriceAdj=F"))$Data
EPROGR<-rbind(EPROGR,Temp)
num_factors<-num_factors+1
##--------------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
#sub to move to the end of the year
year1<-substr(startdate,1,4)
enddate<-paste0(year1,"-12-31")
#end sub
EREVGR<-w.wsd(stock,"west_yoysales",startdate,enddate,
              paste0(paste0("year=",year),";westPeriod=180;Period=Q;Fill=Previous;PriceAdj=F"))$Data
for (i in 2:y-1){
      #sub to add one year!
      year1<-substr(startdate,1,4)
      year1<-as.numeric(year1)
      year1<-year1+1
      year1<-as.character(year1)
      startdate<-paste0(year1,"-01-01")
      #sub end
      #sub to move to the end of the year
      year1<-substr(startdate,1,4)
      enddate<-paste0(year1,"-12-31")
      #end sub
      #sub to add one year
      year<-as.numeric(year)
      year<-year+1
      year<-as.character(year)
      #end sub
      Temp<-w.wsd(stock,"west_yoysales",startdate,enddate,
                  paste0(paste0("year=",year),";westPeriod=180;Period=Q;Fill=Previous;PriceAdj=F"))$Data
      EREVGR<-rbind(EREVGR,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"west_yoysales",startdate,enddate,
            paste0(paste0("year=",year),";westPeriod=180;Period=Q;Fill=Previous;PriceAdj=F"))$Data
EREVGR<-rbind(EREVGR,Temp)
num_factors<-num_factors+1
##---------------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
ROA<-w.wsd(stock,"roa",startdate,Sys.Date()
          ,"Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##------------------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
ROE<-w.wsd(stock,"roe_avg",startdate,Sys.Date()
           ,"Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##------------------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
NPS<-w.wsd(stock,"netprofitmargin",startdate,Sys.Date(),
           "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##------------------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
OM<-w.wsd(stock,"grossprofitmargin",startdate,Sys.Date(),
           "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-----------------------------------------------------------------------##
TA<-w.wsd(stock,"assetsturn",startdate,Sys.Date(),
          "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-----------------------------------------------------------------------##
AN<-w.wsd(stock,"assetstoequity",startdate,Sys.Date(),
          "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-----------------------------------------------------------------------##

##=======================================================================##
#Group stocks in accordance with factors
##!!!!!Change the group number here
groupnum<-5
num_stock<-length(stock)
num_periods<-nrow(OM)
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
close<-w.wsd(stock,"pct_chg",startdate,
             Sys.Date(),"Period=Q;Fill=Previous;PriceAdj=F")$Data
plotdate<-as.character(close[,1])
plotdate<-c(startdate,plotdate)
plotdate<-plotdate[1:length(plotdate)-1]

#Sub to put NA into 0
RemoveNA<-function(x){
      x<-x[,-1]
      x[is.na(x)]<-0
      x
}
#End Sub
plotfactor<-function(factor){
plotdata<-RemoveNA(factor)
gain<-matrix(1,num_periods,groupnum)
adjustgain<-matrix(1,1,groupnum)
for(i in 1:(num_periods-1)){
#group<-cutree(hclust(dist(as.numeric(plotdata[i,]))),groupnum)
group<-rep(1:groupnum,each=ceiling(length(close[i+1,-1])/groupnum))
groupdata<-rbind(close[i+1,-1]/100,plotdata[i,])
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
      adjustgain[j]<-sqrt(gain[num_periods,j]/var(gain[,j]))
}
output<-rbind(gain[num_periods,],adjustgain)
jpeg(file=paste(count,".jpeg"))
par(mfrow=c(groupnum,1),mar=c(1,2,1,1))
for(i in 1:groupnum){
      plot(as.Date(plotdate),gain[,i],type="p",ylim=c(0,max(gain)))
      lines(as.Date(plotdate),gain[,i])
}
dev.off()
count=count+1
output
}
##=====================================================================##

result_AN<-plotfactor(AN)
result_CH1<-plotfactor(CH1)
result_CH3<-plotfactor(CH3)
result_NPS<-plotfactor(NPS)
result_OM<-plotfactor(OM)
result_REVGR<-plotfactor(REVGR)
result_ROA<-plotfactor(ROA)
result_ROE<-plotfactor(ROE)
result_TA<-plotfactor(TA)
result_TURN<-plotfactor(TURN)

##===================================================================##
zcf<-function(factor){
      zcf_temp<-apply(RemoveNA(factor),MARGIN=1,mean)
      zcf_temp<-(zcf_temp)/sqrt(var(zcf_temp))
      zcf_temp
}
zcf_CH3<-zcf(CH3)
zcf_NPS<-zcf(NPS)
zcf_OM<-zcf(OM)
zcf_REVGR<-zcf(REVGR)
zcf_ROA<-zcf(ROA)
#zcf_ROE<-zcf(ROE)
zcf_TURN<-zcf(TURN)
zcf_CLOSE<-zcf(close)

zcfdata<-data.frame(CH3=zcf_CH3,NPS=zcf_NPS,OM=zcf_NPS,REVGR=zcf_REVGR,
                    ROA=zcf_REVGR,TURN=zcf_TURN)#ROE=zcf_ROE,
zcf_result<-princomp(zcfdata,cor=TRUE)
summary(zcf_result,loadings=TRUE)
zcf_result<-as.matrix(zcf_result$loadings)
zcf_result[,1]
#THis is the indicating matrix
factor_1<-zcf_CH3*zcf_result[1,1]+zcf_NPS*zcf_result[2,1]+
      zcf_OM*zcf_result[3,1]+zcf_REVGR*zcf_result[4,1]+
      zcf_ROA*zcf_result[5,1]+zcf_TURN*zcf_result[6,1]
factor_2<-zcf_CH3*zcf_result[1,2]+zcf_NPS*zcf_result[2,2]+
      zcf_OM*zcf_result[3,2]+zcf_REVGR*zcf_result[4,2]+
      zcf_ROA*zcf_result[5,2]+zcf_TURN*zcf_result[6,2]
factor_3<-zcf_CH3*zcf_result[1,3]+zcf_NPS*zcf_result[2,3]+
      zcf_OM*zcf_result[3,3]+zcf_REVGR*zcf_result[4,3]+
      zcf_ROA*zcf_result[5,3]+zcf_TURN*zcf_result[6,3]
factor_4<-zcf_CH3*zcf_result[1,4]+zcf_NPS*zcf_result[2,4]+
      zcf_OM*zcf_result[3,4]+zcf_REVGR*zcf_result[4,4]+
      zcf_ROA*zcf_result[5,4]+zcf_TURN*zcf_result[6,4]


#=============================================================#
##Regression to determine the weight of each factor
lmsol<-lm(as.numeric(zcf_CLOSE)~as.numeric(factor_1)+
                as.numeric(factor_2)+as.numeric(factor_3)
          +as.numeric(factor_4))
summary(lmsol)
point<-data.frame(factor_1=as.numeric(factor_1[14]),
                  factor_2=as.numeric(factor_2[14]),
                  factor_3=as.numeric(factor_3[14]),
                  factor_4=as.numeric(factor_4[14]))
predict(lmsol,point,interval="prediction",level=0.95)


