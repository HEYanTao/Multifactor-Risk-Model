#Data Processing
#The first step of the whole system#

#No.1 Read in data
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
      getmid<-apply(data[,-1],1,median,na.rm=TRUE)
      data1<-data[,-1]
      for(i in 1:length(getmid)){
            data1[i,data1[i,]>3*abs(getmid[i])]<-3*abs(getmid[i])
            data1[i,data1[i,]<(-3*abs(getmid[i]))]<-(-3*abs(getmid[i]))
            
      }
      data[,-1]<-data1
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