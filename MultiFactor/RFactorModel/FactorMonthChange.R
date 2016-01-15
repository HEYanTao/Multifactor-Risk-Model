plotfun<-function(factorname){
      factor<-get(factorname)
      plotdata<-sizeweight_l%*%as.numeric(factor[1,-1])
      for(i in 2:nrow(factor)){
            plotdata<-c(plotdata,sizeweight_l%*%as.numeric(factor[i,-1]))
      }
      library(ggplot2)
      qplot(substr(factor[,1],3,7),plotdata,geom="point",main=factorname)
      #plot(factor[,1],plotdata,main=factorname,type="p")
}