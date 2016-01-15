#This is the multi-factor stock picking system developed by MarkHE
#Preparation
setwd("C:\\Users\\shixi19\\Desktop\\RFactorModel")
library(WindR)
w.start(showmenu=F)

#Set the stocks to calculate
y<-7#use this variable to determine how many years to look back
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
num_factors<-0
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
          "Period=M;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-----------------------------------------------------------------------##
num_factors<-0##Indicate how many factors are taken into consideration
#-----------------------------------------------------------------#
#-----------------------------------------------------------------#
#MarketAttitude
Rating<-w.wsd(stock,"rating_avg",
              startdate,Sys.Date(),"Period=M;Fill=Previous")$Data
num_factors<-num_factors+1
#----------------------------------#
Focus<-w.wsd(stock,"xq_WOW_focus",startdate,
             Sys.Date(),"Period=M;Fill=Previous")$Data
#----------------------------------#
Comments<-w.wsd(stock,"xq_WOW_comments",startdate,
                Sys.Date(),"Period=M;Fill=Previous")$Data
#-----------------------------------------------------------------#
#-----------------------------------------------------------------#
#Financial Indicators
TotalShare<-w.wsd(stock,"share_totala",startdate,Sys.Date(),
                  "Period=M;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
#-----------------------------------------------------------------#
#Valuation Level
PE<-w.wsd(stock,"pe_ttm",startdate,Sys.Date(),
          "Period=M;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
#-----------------------------------------------------------------#
PB<-w.wsd(stock,"pb_lf",startdate,Sys.Date(),
          "Period=M;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-------------------------------------------------------------#

#sub to move to the end of the year
year1<-substr(startdate,1,4)
enddate<-paste0(year1,"-12-31")
#end sub
EPE<-w.wsd(stock,"pe_est",startdate,enddate,
           paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
for (i in 1:(y-1)){
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
                  paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
      EPE<-rbind(EPE,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"pe_est",startdate,enddate,
            paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
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
           paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
for (i in 1:(y-1)){
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
                  paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
      EPB<-rbind(EPB,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"estpb",startdate,enddate,
            paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
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
            paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
for (i in 1:(y-1)){
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
                  paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
      TURN<-rbind(TURN,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"turn",startdate,enddate,
            paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
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
            paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
for (i in 1:(y-1)){
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
                  paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
      TURN<-rbind(TURN,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"turn",startdate,enddate,
            paste("year=",year,";Period=M;Fill=Previous;PriceAdj=F"))$Data
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
dateout<-VOL[,1]
TEMP<-w.wss(stock,'stdevry',paste0('startDate=',startdate),
            paste0('endDate=',enddate),'period=1','returnType=1')$Data
VOL[1,-1]<-t(TEMP$STDEVR)
accumulation<-1
while(as.numeric(substr(enddate,1,4))<as.numeric(substr(Sys.Date(),1,4))|
            as.numeric(substr(enddate,5,6))+1<as.numeric(substr(Sys.Date(),6,7)))
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
             "01"={month1<-"02";monthend<-"03";dayend<-"01"},
             "02"={month1<-"03";monthend<-"04";dayend<-"01"},
             "03"={month1<-"04";monthend<-"05";dayend<-"01"},
             "04"={month1<-"05";monthend<-"06";dayend<-"01"},
             "05"={month1<-"06";monthend<-"07";dayend<-"01"},
             "06"={month1<-"07";monthend<-"08";dayend<-"01"},
             "07"={month1<-"08";monthend<-"09";dayend<-"01"},
             "08"={month1<-"09";monthend<-"10";dayend<-"01"},
             "09"={month1<-"10";monthend<-"11";dayend<-"01"},
             "10"={month1<-"11";monthend<-"12";dayend<-"01"},
             "11"={month1<-"12";year2<-year2+1;monthend<-"01";dayend<-"01"},
             "12"={month1<-"01";monthend<-"02";dayend<-"01"}
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
       "01"={month1<-"02";monthend<-"03";dayend<-"01"},
       "02"={month1<-"03";monthend<-"04";dayend<-"01"},
       "03"={month1<-"04";monthend<-"05";dayend<-"01"},
       "04"={month1<-"05";monthend<-"06";dayend<-"01"},
       "05"={month1<-"06";monthend<-"07";dayend<-"01"},
       "06"={month1<-"07";monthend<-"08";dayend<-"01"},
       "07"={month1<-"08";monthend<-"09";dayend<-"01"},
       "08"={month1<-"09";monthend<-"10";dayend<-"01"},
       "09"={month1<-"10";monthend<-"11";dayend<-"01"},
       "10"={month1<-"11";monthend<-"12";dayend<-"01"},
       "11"={month1<-"12";year2<-year2+1;monthend<-"01";dayend<-"01"},
       "12"={month1<-"01";monthend<-"02";dayend<-"01"}
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
CH1<-w.wsd(stock,"pct_chg",startdate,
           Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##----------------------------------------------------------------##
CH3<-CH1
CH1date<-CH1$DATETIME
#Get startdate and end date
for(i in 1:nrow(CH1)){
      enddate<-CH1date[i]
      CH3[i,-1]<-w.wsd(stock,"pct_chg","ED-3M",as.character(enddate),
                       "Period=M;Fill=Previous;PriceAdj=F")$Data[2,-1]}
num_factors<-num_factors+1
##-----------------------------------------------------------------##
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
PROGR<-w.wsd(stock,"yoyprofit",startdate,Sys.Date()
             ,"Period=M;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-----------------------------------------------------------------##
REVGR<-w.wsd(stock,"yoy_or",startdate,Sys.Date()
             ,"Period=M;Fill=Previous;PriceAdj=F")$Data
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
              paste0(paste0("year=",year),";westPeriod=180;Period=M;Fill=Previous;PriceAdj=F"))$Data
for (i in 1:(y-1)){
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
                  paste0(paste0("year=",year),";westPeriod=180;Period=M;Fill=Previous;PriceAdj=F"))$Data
      EPROGR<-rbind(EPROGR,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"west_yoynetprofit",startdate,enddate,
            paste0(paste0("year=",year),";westPeriod=180;Period=M;Fill=Previous;PriceAdj=F"))$Data
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
              paste0(paste0("year=",year),";westPeriod=180;Period=M;Fill=Previous;PriceAdj=F"))$Data
for (i in 1:(y-1)){
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
                  paste0(paste0("year=",year),";westPeriod=180;Period=M;Fill=Previous;PriceAdj=F"))$Data
      EREVGR<-rbind(EREVGR,Temp)
}
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
startdate<-paste0(year,"-01-01")
enddate<-Sys.Date()
#end sub
Temp<-w.wsd(stock,"west_yoysales",startdate,enddate,
            paste0(paste0("year=",year),";westPeriod=180;Period=M;Fill=Previous;PriceAdj=F"))$Data
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
           ,"Period=M;Fill=Previous;PriceAdj=F")$Data
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
           ,"Period=M;Fill=Previous;PriceAdj=F")$Data
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
           "Period=M;Fill=Previous;PriceAdj=F")$Data
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
          "Period=M;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1
##-----------------------------------------------------------------------##
TA<-w.wsd(stock,"assetsturn",startdate,Sys.Date(),
          "Period=M;Fill=Previous;PriceAdj=F")$Data
num_factors<-num_factors+1


##=======================================================================##
#Group stocks in accordance with factors
##!!!!!Change the group number here
groupnum<-5
num_stock<-length(stock)
num_periods<-nrow(VOL)
#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
close<-w.wsd(stock,"pct_chg",startdate,
             Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
plotdate<-as.character(close[,1])
plotdate<-c(startdate,plotdate)
plotdate<-plotdate[1:(length(plotdate)-1)]

#Sub to put NA into 0
RemoveNA<-function(x){
      x<-x[,-1]
      x[is.na(x)]<-0
      x
}
# zcfplot<-function(factor){
#       zcf1_temp<-apply(RemoveNA(factor),MARGIN=1,mean)
#       zcf1_temp
# }
# zcf1_CLOSE_p<-zcfplot(close)
zcf1_CLOSE_p<-w.wsd("CI005021.WI","pct_chg",plotdate[1],
                    Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data$PCT_CHG
# #End Sub
plotfactor<-function(factor){
      plotdata<-RemoveNA(factor)
      gain<-matrix(1,num_periods,groupnum)
      gain_index<-matrix(1,num_periods,1)
      adjustgain<-matrix(1,1,groupnum)
      for(i in 1:(num_periods-1)){
            gain_index[i+1]<-gain_index[i]*(1+zcf1_CLOSE_p[i+1]/100)
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
      #       jpeg(file=paste(count,".jpeg"))
      par(mfrow=c(groupnum,1),mar=c(1,2,1,1))
      for(i in 1:groupnum){
            plot(as.Date(plotdate),gain[,i],type="p",ylim=c(0,max(gain)))
            lines(as.Date(plotdate),gain[,i])
            lines(as.Date(plotdate),gain_index,col="red")
            legend("topleft",legend=as.character(i))
      }
      #       dev.off()
      output
}
library(manipulate)
manipulate(
      plotfactor(get(factor)),
      factor=picker("AN","CH1","CH3","Comments","EPB","EPE","EPROGR","EREVGR","Focus"
                    ,"NPS","OM","PB","PE","PROGR","REVGR","ROA","ROE","Rating","TA"
                    ,"TURN","TotalShare","VOL")
)
#===========================================================================#

# zcf1<-function(factor){
#       zcf1_temp<-apply(RemoveNA(factor),MARGIN=1,mean)
#       zcf1_temp<-(zcf1_temp-mean(zcf1_temp))/sqrt(var(zcf1_temp))
#       zcf1_temp
# }
# zcf1_CLOSE<-zcf1(close)

#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub
close<-w.wsd(stock,"pct_chg",startdate,
             Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
plotdate<-as.character(close[,1])
plotdate<-c(startdate,plotdate)
plotdate<-plotdate[1:length(plotdate)-1]
#-----------------------------------------------------------------#
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
      timeline<-rep(0,(num.row-2))
      efficientrow<-num.row-2
      for(i in 1:(num.row-2)){
            #       close1[i+1,]<-(close1[i+1,]-mean(close1[i+1,]))/sqrt(var(close1[i+1,]))
                    regressdata[i,]<-((as.numeric(regressdata[i,]))-
                         (mean(as.numeric(regressdata[i,]))))/sqrt(var(as.numeric(regressdata[i,])))
                    if(sum(is.na(regressdata[i,]))>(0.5*length(regressdata[i,]))|sum(abs(regressdata[i,]))==0){
                          efficientrow<-efficientrow-1##To determine if this data is meaningful
                    timeline[i]<-0.5
                    }
                    else{
                    regressdata[is.na(regressdata)]<-0
        lm_temp<-lm(as.numeric(close1[i+1,])~as.numeric(rep(zcf1_CLOSE_p[i+1],num.col))
                                            +as.numeric(regressdata[i,]),model=TRUE)

       #        a<-(as.numeric(close1[i+2,])-as.numeric(zcf1_CLOSE_p[i+1]))
#        b<-((as.numeric(regressdata[i,]))-(mean(as.numeric(regressdata[i,]))))
#        b1<-((as.numeric(regressdata[i+1,]))-(mean(as.numeric(regressdata[i,]))))
#        c<-rbind(a,(b1-b)/b,b)
#        c[2,c[3,]==0]<-0
#        lm_temp<-lm(c[1,]~c[2,])

            #lm_temp<-lm(as.numeric(close1[i,])~as.numeric(regressdata[i,]),model=TRUE)
            #   lm_temp<-lm(as.numeric(close1[i+1,])~as.numeric(rep(1,num.col)
            #                                     +as.numeric(regressdata[i,])))
            if(nrow(summary(lm_temp)$coefficients)>=1){
                  tvalue<-summary(lm_temp)$coefficients[2,3]
                  if(abs(tvalue)>t_threshold){
                        count<-count+1
                        timeline[i]<-1
                  }
                                    sum_t<-sum_t+abs(tvalue)
                  sum_r<-abs(summary(lm_temp)$r.squared)+sum_r
            }else{
                  efficientrow<-efficientrow-1
                  timeline[i]<-0.5
            }}}
      average<-sum_t/(efficientrow)
      count<-count/(efficientrow)
      sum_r<-sum_r/(efficientrow)
      out<-data.frame(average_t_value=average,
                      times_larger_than_threshold=count,rsquare=sum_r)
      #plot
      par(mfrow=c(1,1),mar=c(1,2,1,1))
      gain_index<-matrix(1,num.row-2,1)
      for(i in 1:(num.row-2)){
            gain_index[i+1]<-gain_index[i]*(1+zcf1_CLOSE_p[i+1]/100)
      }
      timeline<-c(0,timeline)
      plot(as.Date(plotdate[-1]),timeline,type="p",ylim=c(0,1.5))
      lines(as.Date(plotdate[-1]),gain_index,col="red")
      
      print(out)
      
}
library(manipulate)
manipulate(
      regression(get(factor)),
      factor=picker("AN","CH1","CH3","Comments","EPB","EPE","EPROGR","EREVGR","Focus"
                    ,"NPS","OM","PB","PE","PROGR","REVGR","ROA","ROE","Rating","TA"
                    ,"TURN","TotalShare","VOL")
)
TURNXZ<-regression(TURN)
ANXZ<-regression(AN)
CH1XZ<-regression(CH1)
CH3XZ<-regression(CH3)
CommentsXZ<-regression(Comments)
EPBXZ<-regression(EPB)
EPEXZ<-regression(EPE)
EPROGRXZ<-regression(EPROGR)
EREVGRXZ<-regression(EREVGR)
FocusXZ<-regression(Focus)
NPSXZ<-regression(NPS)
OMXZ<-regression(OM)
PBXZ<-regression(PB)
PEXZ<-regression(PE)
PROGRXZ<-regression(PROGR)
REVGRXZ<-regression(REVGR)
ROAXZ<-regression(ROA)
ROEXZ<-regression(ROE)
RatingXZ<-regression(Rating)
TAXZ<-regression(TA)
TotalShareXZ<-regression(TotalShare)
VOLXZ<-regression(VOL)
