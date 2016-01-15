changepositionrate<-function(factorname){
total<-0
factor<-get(factorname)
for(i in 1:(nrow(factor)-1)){
      print(paste0("Period",i,":"))
      print(sum(abs(factor[i+1,-1]-factor[i,-1])))
      ##Calculate both in and out
      ##hypothesis is buying has the same cost as selling
      total<-total+sum(abs(factor[2,-1]-factor[1,-1]))
}
print("Total:")
print(total)
}