#This is the multi-factor stock picking system developed by MarkHE
#Preparation
setwd("C:\\Users\\shixi19\\Desktop\\RFactorModel")
library(WindR)
w.start(showmenu=F)

#Set the stocks to calculate
y<-9#use this variable to determine how many years to look back
#!!y has to be less than 10
#In order to avoid the survivior problem so that we set the date to 
#be our startdate 
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"0101")
#end sub
# stocklist<-w.wset('SectorConstituent',
#                   paste0('date=',startdate,';windcode=000906.SH'))
stocklist<-w.wset('SectorConstituent',
                  paste0('date=',startdate,';sectorId=a001010100000000'))
stock<-stocklist$Data$wind_code#Get the code for all A stock
stockname<-stocklist$Data$sec_name#Get the name for all A stock

industrycode<-w.wss(stock,'industry_citiccode,industry_citic',
                paste0('tradeDate=',startdate),'industryType=1')$Data$INDUSTRY_CITICCODE
industryname<-w.wss(stock,'industry_citiccode,industry_citic',
                    paste0('tradeDate=',startdate),'industryType=1')$Data$INDUSTRY_CITIC
stock_industry<-rbind(stock,industrycode,industryname)
################
# for(t in unique(industryname)){ 
# stock<-stock_industry[1,stock_industry[3,]==t]
# print(t)
# }
t<-unique(industryname)[1]
stock<-stock_industry[1,stock_industry[3,]==t]
print(t)
################
##=================================================================##
#Read in factor data 

#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
##-----------------------------------------------------------------------##
AN<-w.wsd(stock,"assetstoequity",startdate,Sys.Date(),
          "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-----------------------------------------------------------------------##
num_factors<-0##Indicate how many factors are taken into consideration
#-----------------------------------------------------------------#
#-----------------------------------------------------------------#
#MarketAttitude
Rating<-w.wsd(stock,"rating_avg",
                  startdate,Sys.Date(),"Period=Q;Fill=Previous")$Data
num_factors<-num_factors+1
#----------------------------------#
Focus<-w.wsd(stock,"xq_WOW_focus",startdate,
                  Sys.Date(),"Period=Q;Fill=Previous")$Data
#----------------------------------#
Comments<-w.wsd(stock,"xq_WOW_comments",startdate,
                  Sys.Date(),"Period=Q;Fill=Previous")$Data
#-----------------------------------------------------------------#
#-----------------------------------------------------------------#
#Financial Indicators
TotalShare<-w.wsd(stock,"share_totala",startdate,Sys.Date(),
                  "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
#-----------------------------------------------------------------#
#Valuation Level
PE<-w.wsd(stock,"pe_ttm",startdate,Sys.Date(),
          "Period=Q;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
#-----------------------------------------------------------------#
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
for (i in 2:(y-1)){
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
for (i in 2:(y-1)){
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
for (i in 2:(y-1)){
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
for (i in 2:(y-1)){
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
VOL<-AN
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
for (i in 2:(y-1)){
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
for (i in 2:(y-1)){
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


##=======================================================================##
#Group stocks in accordance with factors
##!!!!!Change the group number here
# groupnum<-5
# num_stock<-length(stock)
# num_periods<-nrow(OM)
# #Get startdate and end date
# year<-substr(Sys.Date(),1,4)
# year<-as.numeric(year)
# year<-year-y
# year<-as.character(year)
# startdate<-paste0(year,"-01-01")
# #end sub
 close<-w.wsd(stock,"pct_chg",startdate,
              Sys.Date(),"Period=Q;Fill=Previous;PriceAdj=F")$Data
# plotdate<-as.character(close[,1])
# plotdate<-c(startdate,plotdate)
# plotdate<-plotdate[1:length(plotdate)-1]

#Sub to put NA into 0
RemoveNA<-function(x){
      x<-x[,-1]
      x[is.na(x)]<-0
      x
}
# #End Sub
# plotfactor<-function(factor){
#       plotdata<-RemoveNA(factor)
#       gain<-matrix(1,num_periods,groupnum)
#       adjustgain<-matrix(1,1,groupnum)
#       for(i in 1:(num_periods-1)){
#             #group<-cutree(hclust(dist(as.numeric(plotdata[i,]))),groupnum)
#             group<-rep(1:groupnum,each=ceiling(length(close[i+1,-1])/groupnum))
#             groupdata<-rbind(close[i+1,-1]/100,plotdata[i,])
#             groupdata<-groupdata[,order(groupdata[2,])]
#             groupdata<-rbind(groupdata,group[1:length(close[i+1,-1])])
#             m<-tapply(as.numeric(groupdata[1,]),
#               as.factor(as.numeric(groupdata[3,])),mean,na.rm=TRUE)
#             for(j in 1:groupnum){
#                   if(is.na(m[j])){
#                         m[j]<-1
#                         gain[i+1,j]<-gain[i,j]*m[j]
#                   }else{
#                         m[j]<-1+m[j]
#                         gain[i+1,j]<-gain[i,j]*m[j]
#                   }
#             }
#       }
#       for(j in 1:groupnum){
#             adjustgain[j]<-sqrt(gain[num_periods,j]/var(gain[,j]))
#       }
#       output<-rbind(gain[num_periods,],adjustgain)
#       jpeg(file=paste(count,".jpeg"))
#       par(mfrow=c(groupnum,1),mar=c(1,2,1,1))
#       for(i in 1:groupnum){
#             plot(as.Date(plotdate),gain[,i],type="p",ylim=c(0,max(gain)))
#             lines(as.Date(plotdate),gain[,i])
#       }
#       dev.off()
#       output
# }
##=====================================================================##
# count=1
# 
# result_AN<-plotfactor(AN)
# count=count+1
# result_CH1<-plotfactor(CH1)
# count=count+1
# result_CH3<-plotfactor(CH3)
# count=count+1
# result_NPS<-plotfactor(NPS)
# count=count+1
# result_OM<-plotfactor(OM)
# count=count+1
# result_REVGR<-plotfactor(REVGR)
# count=count+1
# result_ROA<-plotfactor(ROA)
# count=count+1
# result_ROE<-plotfactor(ROE)
# count=count+1
# result_TA<-plotfactor(TA)
# count=count+1
# result_TURN<-plotfactor(TURN)

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

# zcfdata<-data.frame(CH3=zcf_CH3,NPS=zcf_NPS,OM=zcf_NPS,REVGR=zcf_REVGR,
#                     ROA=zcf_REVGR,TURN=zcf_TURN)#ROE=zcf_ROE,
# zcf_result<-princomp(zcfdata,cor=TRUE)
# summary(zcf_result,loadings=TRUE)
# zcf_result<-as.matrix(zcf_result$loadings)
# zcf_result[,1]
# #This is the indicating matrix
# factor_1<-zcf_CH3*zcf_result[1,1]+zcf_NPS*zcf_result[2,1]+
#       zcf_OM*zcf_result[3,1]+zcf_REVGR*zcf_result[4,1]+
#       zcf_ROA*zcf_result[5,1]+zcf_TURN*zcf_result[6,1]
# factor_2<-zcf_CH3*zcf_result[1,2]+zcf_NPS*zcf_result[2,2]+
#       zcf_OM*zcf_result[3,2]+zcf_REVGR*zcf_result[4,2]+
#       zcf_ROA*zcf_result[5,2]+zcf_TURN*zcf_result[6,2]
# factor_3<-zcf_CH3*zcf_result[1,3]+zcf_NPS*zcf_result[2,3]+
#       zcf_OM*zcf_result[3,3]+zcf_REVGR*zcf_result[4,3]+
#       zcf_ROA*zcf_result[5,3]+zcf_TURN*zcf_result[6,3]
# factor_4<-zcf_CH3*zcf_result[1,4]+zcf_NPS*zcf_result[2,4]+
#       zcf_OM*zcf_result[3,4]+zcf_REVGR*zcf_result[4,4]+
#       zcf_ROA*zcf_result[5,4]+zcf_TURN*zcf_result[6,4]
# 
# 
# #=============================================================#
# ##Regression to determine the weight of each factor
# lmsol<-lm(as.numeric(zcf_CLOSE)~as.numeric(factor_1)+
#                 as.numeric(factor_2)+as.numeric(factor_3)
#           +as.numeric(factor_4))
# summary(lmsol)
# point<-data.frame(factor_1=as.numeric(factor_1[14]),
#                   factor_2=as.numeric(factor_2[14]),
#                   factor_3=as.numeric(factor_3[14]),
#                   factor_4=as.numeric(factor_4[14]))
# predict(lmsol,point,interval="prediction",level=0.95)
# 
# #============================================================#

##HERE I use zcf function and the data of zcf_CLOSE
##PLEASE remember to put it into this sub if decided not
##to use the zcf part!!!!!!

 zcf1<-function(factor){
             zcf1_temp<-apply(RemoveNA(factor),MARGIN=1,mean)
             zcf1_temp<-(zcf1_temp-mean(zcf1_temp))/sqrt(var(zcf1_temp))
             zcf1_temp
       }
 zcf1_CLOSE<-zcf1(close)
 t_threshold<-2 #how large a t value is notable
 t_percent_threshold<-0.7#How many percent of notable is notable
 regression<-function(factor){
            count<-0
            sum_t<-0
            sum_r<-0
            regressdata<-RemoveNA(factor)
            num.col<-as.numeric(ncol(regressdata))
            num.row<-as.numeric(nrow(regressdata))
            close1<-RemoveNA(close)
            for(i in 1:(num.row-1)){
#       close1[i+1,]<-(close1[i+1,]-mean(close1[i+1,]))/sqrt(var(close1[i+1,]))
#       regressdata[i,]<-(regressdata[i,]-
#                         (mean(regressdata[i,]))/sqrt(var(regressdata[i,])))
 lm_temp<-lm(as.numeric(close1[i+1,])~as.numeric(rep(zcf1_CLOSE[i+1],num.col)
                                   +as.numeric(regressdata[i,])))
#   lm_temp<-lm(as.numeric(close1[i+1,])~as.numeric(rep(1,num.col)
#                                     +as.numeric(regressdata[i,])))
                        if(nrow(summary(lm_temp)$coefficients)>1){
                              tvalue<-summary(lm_temp)$coefficients[2,3]
                                if(abs(tvalue)>t_threshold){
                                           count<-count+1
                                      }
                           sum_t<-sum_t+tvalue
                           sum_r<-summary(lm_temp)$r.squared+sum_r
                         }}
             average<-sum_t/(num.row-1)
             count<-count/(num.row-1)
             sum_r<-sum_r/(num.row-1)
             out<-data.frame(average_t_value=average,
                             times_larger_than_threshold=count,rsquare=sum_r)
       }
TURNXZ<-regression(TURN)
ANXZ<-regression(AN)
CH1XZ<-regression(CH1)
CH3XZ<-regression(CH3)
NPSXZ<-regression(NPS)
OMXZ<-regression(OM)
REVGRXZ<-regression(REVGR)
ROAXZ<-regression(ROA)
ROEXZ<-regression(ROE)
TAXZ<-regression(TA)
VOLXZ<-regression(VOL)





























