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

#sub to move to the next quarter
year2<-substr(startdate,1,4)
year2<-as.numeric(year2)
month1<-substr(startdate,6,7)
monthend<-month1
day1<-substr(startdate,9,10)
dayend<-day1
switch(month1,
       "01"={month1<-"04";monthend<-"06";dayend<-"30"},
       "04"={month1<-"07";monthend<-"09";dayend<-"30"},
       "07"={month1<-"10";monthend<-"12";dayend<-"31"},
       "10"={month1<-"01";year2<-year2+1;monthend<-"03";dayend<-"31"}
)
year2<-as.character(year2)
startdate<-paste0(year2,"-",month1,"-01")
enddate<-paste0(year2,"-",monthend,"-",dayend)   
#end sub

#Get startdate and end date
year<-substr(Sys.Date(),1,4)
year<-as.numeric(year)
year<-year-y
year<-as.character(year)
startdate<-paste0(year,"-01-01")
#end sub