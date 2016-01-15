#This is the multi-factor stock picking system developed by MarkHE
#Ready two functions, use the

#Preparation#
#=============================================================================#
{
setwd("C:\\Users\\shixi19\\Desktop\\RFactorModel")
library(WindR)
w.start(showmenu=F)
#options(warn=-1)
}
#=============================================================================#

#Public Functions#
#=============================================================================#
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
startdate<-paste0(year,"-01-01")}
startdatefunwithnohash<-function(y){
      year<-substr(Sys.Date(),1,4)
      year<-as.numeric(year)
      year<-year-y
      year<-as.character(year)
      startdate<-paste0(year,"0101")}
datepoint<-function(y){
      close<-w.wsd("000001.SH","pct_chg",startdatefun(y),
                   Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      plotdate<-as.character(close[,1])
      plotdate
}
#Read in all A stocks at the time of startdate and follow the industry 
#classification of citic 
stocklist<-function(y){
      startdate<-startdatefun(y)
#       stocklist<-w.wset('SectorConstituent',
#                         paste0('date=',startdate,';sectorId=a001010100000000'))
      stocklist<-w.wset('SectorConstituent',
                        paste0('date=',startdate,';windcode=000300.SH'))
#       stocklist<-w.wset('SectorConstituent',
#                         paste0('date=',startdate,';windcode=000906.SH'))
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
      getmean<-apply(data[,-1],1,mean,na.rm=TRUE)
      data[,-1]<-data[,-1]-getmean
      getvar<-apply(data[,-1],1,var,na.rm=TRUE)
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

#=============================================================================#

#Determine the public variables#
#=============================================================================#
y<-2
#This indicates how many years to look back which has to be less than 10
#Strongly suggest to set y<-2 since the data of expected level are available
#since 2013
#-----------------------------------------------------------------------------#
stock_industry<-stocklist(y)
stocklist<-stock_industry[1,]#Use it too often so that I list it seperately
num.industry<-as.numeric(length(unique(stock_industry[3,])))
num.stock<-as.numeric(length(stock_industry[1,]))
checkdate<-datepoint(y)
sizeweight_l<-sizeweight(stocklist)
#=============================================================================#

#Factors#
#=============================================================================#
#Group One: The industry factors#

#Create industry factors#
#Can only run one time!!#
{
x<-rep(0,num.stock)
factor_bank<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_realestate<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_medic<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_restaurant<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_retail<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_machine<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_buildmaterial<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_homeappliance<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_cloth<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_food<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_electronic<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_transport<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_auto<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_lightindustry<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_electric<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_integrate<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_communicate<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_oil<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_coloredmetal<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_agriculture<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_architecture<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_computer<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_chemical<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_coal<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_electricequipment<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_military<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_nonbank<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_steel<-factor(x,levels=c(0,1),labels=c("no","yes"))
factor_media<-factor(x,levels=c(0,1),labels=c("no","yes"))
factorname<-c("factor_bank","factor_realestate","factor_medic",
              "factor_restaurant","factor_retail","factor_machine",
              "factor_buildmaterial","factor_homeappliance","factor_cloth",
              "factor_food","factor_electronic","factor_transport","factor_auto",
              "factor_lightindustry","factor_electric","factor_integrate",
              "factor_communicate","factor_oil","factor_coloredmetal",
              "factor_agriculture","factor_architecture","factor_computer",
              "factor_chemical","factor_coal","factor_electricequipment",
              "factor_military","factor_nonbank","factor_steel","factor_media")
factornamech<-unique(stock_industry[3,])
factorcode<-unique(stock_industry[2,])
factorlist_industry<-rbind(factorname,factorcode,factornamech)
}
#Create industry factors end#

#Initialize industry factors#
{
for(i in 1:num.stock)
{
          switch(stock_industry[3,i],
            "银行"={stock_industry[3,i]="bank";factor_bank[i]="yes"},
            "房地产"={stock_industry[3,i]="realestate";factor_realestate[i]="yes"},
            "医药"={stock_industry[3,i]="medic";factor_medic[i]="yes"},
            "餐饮旅游"={stock_industry[3,i]="restaurant";factor_restaurant[i]="yes"},
            "商贸零售"={stock_industry[3,i]="retail";factor_retail[i]="yes"},
            "机械"={stock_industry[3,i]="machine";factor_machine[i]="yes"},
            "建材"={stock_industry[3,i]="buildmaterial";factor_buildmaterial[i]="yes"},
            "家电"={stock_industry[3,i]="homeappliance";factor_homeappliance[i]="yes"},
            "纺织服装"={stock_industry[3,i]="cloth";factor_cloth[i]="yes"},
            "食品饮料"={stock_industry[3,i]="food";factor_food[i]="yes"},
            "电子元器件"={stock_industry[3,i]="electronic";factor_electronic[i]="yes"},
            "交通运输"={stock_industry[3,i]="transport";factor_transport[i]="yes"},
            "汽车"={stock_industry[3,i]="auto";factor_auto[i]="yes"},
            "轻工制造"={stock_industry[3,i]="lightindustry";factor_lightindustry[i]="yes"},
            "电力及公用事业"={stock_industry[3,i]="electric";factor_electric[i]="yes"},
            "综合"={stock_industry[3,i]="integrate";factor_integrate[i]="yes"},
            "通信"={stock_industry[3,i]="communicate";factor_communicate[i]="yes"},
            "石油石化"={stock_industry[3,i]="oil";factor_oil[i]="yes"},
            "有色金属"={stock_industry[3,i]="coloredmetal";factor_coloredmetal[i]="yes"},
            "农林牧渔"={stock_industry[3,i]="agriculture";factor_agriculture[i]="yes"},
            "建筑"={stock_industry[3,i]="architecture";factor_architecture[i]="yes"},
            "计算机"={stock_industry[3,i]="computer";factor_computer[i]="yes"},
            "基础化工"={stock_industry[3,i]="chemical";factor_chemical[i]="yes"},
            "煤炭"={stock_industry[3,i]="coal";factor_coal[i]="yes"},
            "电力设备"={stock_industry[3,i]="electricequipment";factor_electricequipment[i]="yes"},
            "国防军工"={stock_industry[3,i]="military";factor_military[i]="yes"},
            "非银行金融"={stock_industry[3,i]="nonbank";factor_nonbank[i]="yes"},
            "钢铁"={stock_industry[3,i]="steel";factor_steel[i]="yes"},
            "传媒"={stock_industry[3,i]="media";factor_media[i]="yes"}
          )
}
factornameen<-unique(stock_industry[3,])
factorlist_industry<-rbind(factorlist_industry,factornameen)
}
#Initialize industry factors end#
#=============================================================================#

#Group Two: The Trait factors#
#=============================================================================#
#Functions to use#
{
stockperform250day<-function(stocklist,checkdate){
      data<-w.wsd(stocklist,"pct_chg","ED-250D",
            checkdate,"Fill=Previous;PriceAdj=F")$Data
      data<-removeNA(data)
      data
}
indexperform250day<-function(indexname,checkdate){
      data<-w.wsd(as.character(indexname),"pct_chg","ED-250D",checkdate
            ,"Fill=Previous;PriceAdj=F")$Data
      data<-removeNA(data)
      data
}#indexname:"000300.SH"or"000906.SH"
weight<-function(num,groupnum){
      weight<-rep(1:groupnum,each=ceiling(num/groupnum))
      weight<-weight[1:num]
      sum<-sum(weight)
      weight<-weight/sum
      weight
}
# stockperformance<-stockperform250day(stocklist,checkdate)
# indexperformance<-indexperformance250day("000300.SH",checkdate)

#Gain#
gainf<-function(y,stocklist){
      data<-w.wsd(stocklist,"pct_chg",startdatefunwithhash(y),Sys.Date()
            ,"Period=M;Fill=Previous;PriceAdj=F")$Data
      data[,-1]<-data[,-1]/100
      data<-removeNA(data)
      data
}
#Beta#
beta<-function(y,stocklist){
      data<-w.wsd(stocklist,"beta_100w",startdatefunwithhash(y),Sys.Date()
            ,"Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-standard(data)
      data<-removeNA(data)
      data
}
#Momentum#
momentum<-function(y,stocklist){
      data<-w.wsd(stocklist,"annualyeild_100w",startdatefunwithhash(y),Sys.Date()
            ,"Period=M;Fill=Previous;PriceAdj=F")$Data
      data[,-1]<-data[,-1]/100
      data<-standard(data)
      data<-removeNA(data)
      data
}
#Size#
size<-function(y,stocklist){
      data<-w.wsd(stocklist,"total_shares",startdatefunwithhash(y),Sys.Date(),
            "Period=M;Fill=Previous;PriceAdj=F")$Data
      data[,-1]<-log10(data[,-1])
      data<-standard(data)
      data<-removeNA(data)
      data
}
#Earnings Yield#
eroe<-function(y,stocklist){
      data<-w.wsd(stocklist,"west_avgroe_FY1",startdatefunwithhash(y),Sys.Date(),
                  "Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-standard(data)
      data<-removeNA(data)
      data
}
eeps<-function(y,stocklist){
      data<-w.wsd(stocklist,"west_eps_FY1",startdatefunwithhash(y),Sys.Date(),
                  "Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-standard(data)
      data<-removeNA(data)
      data
}
pe<-function(y,stocklist){
      data<-w.wsd(stocklist,"pe_ttm",startdatefunwithhash(y),Sys.Date(),
                  "Period=M;Fill=Previous;PriceAdj=F")$Data
      data[,-1]<-data[,-1]^(-1)
      data<-standard(data)
      data<-removeNA(data)
      data
}
#Volatility#
vol1<-function(y,stocklist){
      data<-w.wsd(stocklist,"annualstdevr_100w",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-standard(data)
      data<-removeNA(data)
      data
}
vol2<-function(y,stocklist){
      data<-w.wsd(stocklist,"annualstdevr_24m",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-standard(data)
      data<-removeNA(data)
      data
}
turn<-function(y,stocklist){
      stock<-stocklist
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
      data<-TURN
      data<-standard(data)
      data<-removeNA(data)
      data
}
#Growth#
eprofit<-function(y,stocklist){
      data<-w.wsd(stocklist,"west_netprofit_CAGR",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-standard(data)
      data<-removeNA(data)
      data
}
esales<-function(y,stocklist){
      data<-w.wsd(stocklist,"west_sales_CAGR",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-standard(data)
      data<-removeNA(data)
      data
}
growthroe<-function(y,stocklist){
      data<-w.wsd(stocklist,"growth_roe",startdatefunwithhash(y),
                  Sys.Date(),"N=3;Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-standard(data)
      data<-removeNA(data)
      data
}
growthgr<-function(y,stocklist){
      data<-w.wsd(stocklist,"growth_gr",startdatefunwithhash(y),
                  Sys.Date(),"N=3;Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-standard(data)
      data<-removeNA(data)
      data
}
#Value#
value<-function(y,stocklist){
      data<-w.wsd(stocklist,"pb",startdatefunwithhash(y)
                  ,Sys.Date(),"ruleType=9;Period=M;Fill=Previous;PriceAdj=F")$Data
      data[,-1]<-data[,-1]^(-1)
      data<-standard(data)
      data<-removeNA(data)
      data
}
#Leverage#
longdebt<-function(y,stocklist){
      data<-w.wsd(stocklist,"longdebttoworkingcapital",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data[,-1]<-data[,-1]^(-1)
      data<-standard(data)
      data<-removeNA(data)
      data
}
netdebt<-function(y,stocklist){
      data<-w.wsd(stocklist,"netdebttoev",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data[,-1]<-data[,-1]^(-1)
      data<-standard(data)
      data<-removeNA(data)
      data
}
lldebt<-function(y,stocklist){
      data<-w.wsd(stocklist,"longdebttolongcaptial",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data[,-1]<-data[,-1]^(-1)
      data<-standard(data)
      data<-removeNA(data)
      data
}
assetdebt<-function(y,stocklist){
      data<-w.wsd(stocklist,"debttoassets",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data[,-1]<-data[,-1]^(-1)
      data<-standard(data)
      data<-removeNA(data)
      data
}
#News&Others#
focus<-function(y,stocklist){
      data<-w.wsd(stocklist,"xq_WOW_focus",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-removeNA(data)
      data<-standard(data)
      data<-removeNA(data)
      data
}
comments<-function(y,stocklist){
      data<-w.wsd(stocklist,"xq_WOW_comments",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-removeNA(data)
      data<-standard(data)
      data<-removeNA(data)
      data
}
shares<-function(y,stocklist){
      data<-w.wsd(stocklist,"xq_WOW_shares",startdatefunwithhash(y),
                  Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-removeNA(data)
      data<-standard(data)
      data<-removeNA(data)
      data
}
holderpct<-function(y,stocklist){
      data<-w.wsd(stocklist,"holder_avgpct",startdatefunwithhash(y),
                  Sys.Date(),"shareType=1;Period=M;Fill=Previous;PriceAdj=F")$Data
      data<-standard(data)
      data<-removeNA(data)
      data
}
}
#Read in Data#
#Performance#
{
gain<-gainf(y,stocklist)
}
#Beta#
{
beta<-beta(y,stocklist)
}
#Momentum#
{
momentum<-momentum(y,stocklist)
}
#Size#
{
size<-size(y,stocklist)
}
#Earnings Yield#
{
#Read in sub factors#
{
eroe<-eroe(y,stocklist)
eeps<-eeps(y,stocklist)
pe<-pe(y,stocklist)
}
#Optimization#
{
calgain<-function(variables,eroe,eeps,pe){
  gain=eroe
  gain[,-1]=variables[1]*(eroe[,-1])+variables[2]*(eeps[,-1])+variables[3]*(pe[,-1])
  gain<-removeNA(gain)
  gain
}

difference<-function(variables,eroe,eeps,pe,real){
      diff<-sum(((real[,-1])-calgain(variables,eroe,eeps,pe)[,-1])^2)
      diff<-removeNA(diff)
      diff
}
variables<-c(1,1,1)
optimresult<-optim(variables,difference,eroe=eroe[-nrow(eroe),],
                   eeps=eeps[-nrow(eeps),],pe=pe[-nrow(pe),],real=gain[-1,])
subweight<-optimresult$par
}
#Generate Earnings Yield#
earningsyield<-calgain(subweight,eroe,eeps,pe)
earningsyield<-standard(earningsyield)
}
#Volatility#
{
#Read in sub factors#
{
vol1<-vol1(y,stocklist)
vol2<-vol2(y,stocklist)
turn<-turn(y,stocklist)
}
#Optimization#
{
      variables<-c(1,1,1)
      optimresult<-optim(variables,difference,eroe=vol1[-nrow(vol1),],
                         eeps=vol2[-nrow(vol2),],pe=turn[-nrow(turn),],real=gain[-1,])
      subweight<-optimresult$par
}
#Generate volatility#
volatility<-calgain(subweight,vol1,vol2,turn)
volatility<-standard(volatility)
}
#Growth#
{
#Read in sub factors#
{
      ep<-eprofit(y,stocklist)
      es<-esales(y,stocklist)
      ge<-growthroe(y,stocklist)
      gt1<-growthgr(y,stocklist)
}
#Optimization#
{
      calgain<-function(variables,ep,es,ge,gt1){
            gain=ep
            gain[,-1]=variables[1]*(ep[,-1])+variables[2]*(es[,-1])+variables[3]*(ge[,-1])+variables[4]*(gt1[,-1])
            gain<-removeNA(gain)
            gain
      }
      
      difference<-function(variables,ep,es,ge,gt1,real){
            diff<-sum(((real[,-1])-calgain(variables,ep,es,ge,gt1)[,-1])^2,na.rm=TRUE)
            diff
      }
      variables<-c(1,1,1,1)
      optimresult<-optim(variables,difference,ep=ep[-nrow(ep),],gr=NULL,
                         es=es[-nrow(es),],gt1=gt1[-nrow(gt1),],ge=ge[-nrow(ge),],real=gain[-1,])
      subweight<-optimresult$par
}
#Generate growth#
growth<-calgain(subweight,ep,es,ge,gt1)
growth<-standard(growth)
}
#Value#
{
      value<-value(y,stocklist)
}
#Leverage#
{
      #Read in sub factors#
{
      longdebt<-longdebt(y,stocklist)
      netdebt<-netdebt(y,stocklist)
      lldebt<-lldebt(y,stocklist)
      assetdebt<-assetdebt(y,stocklist)
}
#Optimization#
{
      variables<-c(1,1,1,1)
      optimresult<-optim(variables,difference,ep=longdebt[-nrow(longdebt),],
                         es=netdebt[-nrow(netdebt),],ge=lldebt[-nrow(lldebt),],gt1=assetdebt[-nrow(assetdebt),],real=gain[-1,])
      subweight<-optimresult$par
}
#Generate leverage#
leverage<-calgain(subweight,longdebt,netdebt,lldebt,assetdebt)
leverage<-standard(leverage)
}
#News&Others#
{
      #Read in sub factors#
{
      focus<-focus(y,stocklist)
      comments<-comments(y,stocklist)
      shares<-shares(y,stocklist)
      holderpct<-holderpct(y,stocklist)
}
#Optimization#
{
      variables<-c(1,1,1,1)
      optimresult<-optim(variables,difference,ep=focus[-nrow(focus),],
                         es=comments[-nrow(comments),],ge=shares[-nrow(shares),],gt1=holderpct[-nrow(holderpct),],real=gain[-1,])
      subweight<-optimresult$par
}
#Generate news#
news<-calgain(subweight,focus,comments,shares,holderpct)
news<-standard(news)
}
#=============================================================================#
#Regression to determine the strength of each factor

#Prepare the public variables#
#!Change the base index here!#
{
      zcf1_CLOSE_p<-w.wsd("000300.SH","pct_chg",beta[1,1],
                          Sys.Date(),"Period=M;Fill=Previous;PriceAdj=F")$Data$PCT_CHG
}

#1.Regression#
#function to use#
{
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
regression_i<-function(factor,t_threshold){
      t<-size
      t[1,-1]<-as.numeric(factor)-1
      for(i in 2:nrow(t)){
            t[i,-1]<-t[1,-1]
      }
      factor<-t
      #================#
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
}
#output! trait factors#
{
library(manipulate)
manipulate(
      regression(get(factor),t_threshold),
      factor=picker("beta","momentum","size"
                    ,"turn"),
      t_threshold=slider(1,4)
)
}
#output! industry factors#
{
library(manipulate)
manipulate(
      regression_i(get(factor),t_threshold),
      factor=picker(factorname[1],factorname[9],factorname[24],factorname[25]
                    ,factorname[2],factorname[10],factorname[23],factorname[26]
                    ,factorname[3],factorname[11],factorname[22],factorname[27]
                    ,factorname[4],factorname[12],factorname[21],factorname[28]
                    ,factorname[5],factorname[13],factorname[20],factorname[29]
                    ,factorname[6],factorname[14],factorname[19],factorname[17]
                    ,factorname[7],factorname[15],factorname[18],factorname[16]
                    ,factorname[8]),
      t_threshold=slider(1,4)
)
}
#-----------------------------------------------------------------------------#
#2.Group & Plot#
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
            adjustgain[j]<-sqrt(gain[num_periods,j]/var(gain[,j]))
      }
      output<-rbind(gain[num_periods,],adjustgain)
      #       jpeg(file=paste(count,".jpeg"))
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
plotfactor_i<-function(factor){
      t<-size
      t[1,-1]<-as.numeric(factor)
      for(i in 2:nrow(t)){
            t[i,-1]<-t[1,-1]
      }
      factor<-t
      groupnum=2
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
            group<-factor[1,-1]
            groupdata<-rbind(close[i+1,-1],plotdata[i,])
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
      par(mfrow=c(groupnum,1),mar=c(2,2,1,1))
      for(i in 1:groupnum){
            plot(as.Date(plotdate),gain[,i],type="p",ylim=c(0,max(max(gain_index),max(gain))))
            lines(as.Date(plotdate),gain[,i])
            lines(as.Date(plotdate),gain_index,col="red")
            legend("topleft",legend=as.character(i-1))
      }
      #       dev.off()
      output
}
#output! trait factors#
{
library(manipulate)
manipulate(
      plotfactor_t(get(factor),groupnum),
      factor=picker("leverage","growth","beta","momentum","size"
                    ,"earningsyield","volatility","value","news"),
      groupnum=slider(2,5)
)
}
#output! industry factors#
{
library(manipulate)
manipulate(
      plotfactor_i(get(factor)),
      factor=picker(factorname[1],factorname[9],factorname[24],factorname[25]
                    ,factorname[2],factorname[10],factorname[23],factorname[26]
                    ,factorname[3],factorname[11],factorname[22],factorname[27]
                    ,factorname[4],factorname[12],factorname[21],factorname[28]
                    ,factorname[5],factorname[13],factorname[20],factorname[29]
                    ,factorname[6],factorname[14],factorname[19],factorname[17]
                    ,factorname[7],factorname[15],factorname[18],factorname[16]
                    ,factorname[8])
)
}
#=============================================================================#

#Pure factor portofolio#
library(truncnorm)
library(Rsolnp)
#Read performance#

#Factor model#
gain_extra<-rep(0,(nrow(beta)-2))
factor_actual<-matrix(0,(nrow(beta)-2),9)
plotdate<-as.character(beta[,1])
num.row<-as.numeric(nrow(beta))
for(i in 1:(nrow(beta)-2)){
gainfun<-function(w){
      -(as.numeric(w%*%as.numeric(gain[i,-1]))-zcf1_CLOSE_p[i]/100)
}
equalcon<-function(w){
      sum(w)
}
#need to deal with all zero#
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
# inequalcon<-function(w){
#       a<-(as.numeric((w)%*%as.numeric(beta[i,-1])))
#       b<-(as.numeric((w)%*%as.numeric(momentum[i,-1])))
#       c<-(as.numeric((w)%*%as.numeric(size[i,-1])))
#       d<-(as.numeric((w)%*%as.numeric(earningsyield[i,-1])))
#       e<-(as.numeric((w)%*%as.numeric(volatility[i,-1])))
#       f<-(as.numeric((w)%*%as.numeric(growth[i,-1])))
#       g<-(as.numeric((w)%*%as.numeric(value[i,-1])))
#       h<-(as.numeric((w)%*%as.numeric(leverage[i,-1])))
#       i<-(as.numeric((w)%*%as.numeric(news[i,-1])))
#       return(c(a,b,c,d,e,f,g,h,i))
# }
portofolio<-solnp(pars=rep(1/num.stock,num.stock),fun=gainfun
                  ,eqfun=equalcon,eqB=1,ineqfun=inequalcon
                  ,ineqLB=(c(-1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1,-0.1))
                  ,ineqUB=(c(1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)),
                  LB=rep(0,num.stock),UB=rep(1,num.stock))
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
factor_actual[i,]<-inequalcon_real(portofolio$par)#actual beta
gain_extra[i]<-(-gainfun_real(portofolio$par))#actual gain
}
#--------now it's right!#
gain_extra<-rep(0,(nrow(growth)-4))
factor_actual<-matrix(0,(nrow(growth)-4),9)
for(i in 1:(nrow(beta)-4)){ 
      gainfun_real<-function(w){
            -(as.numeric(w%*%as.numeric(gain[i+3,-1]))-zcf1_CLOSE_p[i+3]/100)
      }
      inequalcon_real<-function(w){
            a<-(as.numeric((w)%*%as.numeric(beta[i+3,-1])))/(sizeweight_l%*%as.numeric(beta[i+3,-1]))-1
            b<-(as.numeric((w)%*%as.numeric(momentum[i+3,-1])))/(sizeweight_l%*%as.numeric(momentum[i+3,-1]))-1
            c<-(as.numeric((w)%*%as.numeric(size[i+3,-1])))/(sizeweight_l%*%as.numeric(size[i+3,-1]))-1
            d<-(as.numeric((w)%*%as.numeric(earningsyield[i+3,-1])))/(sizeweight_l%*%as.numeric(earningsyield[i+3,-1]))-1
            e<-(as.numeric((w)%*%as.numeric(volatility[i+3,-1])))/(sizeweight_l%*%as.numeric(volatility[i+3,-1]))-1
            f<-(as.numeric((w)%*%as.numeric(growth[i+3,-1])))/(sizeweight_l%*%as.numeric(growth[i+3,-1]))-1
            g<-(as.numeric((w)%*%as.numeric(value[i+3,-1])))/(sizeweight_l%*%as.numeric(value[i+3,-1]))-1
            h<-(as.numeric((w)%*%as.numeric(leverage[i+3,-1])))/(sizeweight_l%*%as.numeric(leverage[i+3,-1]))-1
            i<-(as.numeric((w)%*%as.numeric(news[i+3,-1])))/(sizeweight_l%*%as.numeric(news[i+3,-1]))-1
            return(c(a,b,c,d,e,f,g,h,i))
      }
      factor_actual[i,]<-inequalcon_real(testmulti_final[i,])#actual beta
      gain_extra[i]<-(-gainfun_real(testmulti_final[i,]))#actual gain
}
      
factor_avg<-apply(factor_actual,2,mean)
gain_avg<-sum(gain_extra)/(nrow(beta)-4)
#plot
par(mfrow=c(1,1),mar=c(3,2,1,1))
gain_index<-matrix(1,num.row-3,1)
gain_factor<-matrix(1,num.row-3,1)
for(j in 1:(num.row-4)){
      gain_index[j+1]<-gain_index[j]*(1+zcf1_CLOSE_p[j+3]/100)
      gain_factor[j+1]<-gain_factor[j]+gain_extra[j]
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
out<-data.frame(factor_actual=factor_avg,gain_extra=gain_avg,win_ratio=winratio,final=gain_factor[length(gain_factor)])
gain_extra<-c(0.5,gain_extra)
plot(as.Date(plotdate[3:32]),gain_extra,type="p",ylim=c(-0.5,3),pch=45,col="red",xlab="",ylab="",main="beta portofolio")
lines(as.Date(plotdate[3:32]),gain_index,col="red")
lines(as.Date(plotdate[3:32]),gain_factor,col="blue")
legend("topleft",col=c("red","blue"),lty=1,legend=c("index","factor"))
print(out)