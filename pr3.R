gtd_analysis<-function(gtd) {
  
  
  par(mfrow=c(1,2))
  lm1<-lm(gtd$J.D~gtd$Year)
  plot(gtd$Year,gtd$J.D,pch=16,col="blue",xlab="Year",ylab="Average Global Temp Deviation",main="Avg Global Temp Dev by Year")
  lines(gtd$Year,lm1$fitted,col="red",lwd=3)
  
  plot(gtd$Year,lm1$residuals,pch=16,col="blue",xlab="Year",ylab="Residuals",main="Global Temp Dev Residuals")
  abline(c(0,0),col="red",lwd=3)
  
  
  #f(x)=mx+b is the equation of the linear fit line
  m<-lm1$coefficients[2]
  b<-lm1$coefficients[1]
  
  #predictions can be found by setting x equal to the year we wish to predict
  cat("2012 global average temperature prediction: ",(m*2012)+b,"\n")
  cat("2013 global average temperature prediction: ",(m*2013)+b,"\n\n\n")
  cat("95% confidence interval: \n\n")
  return(confint(lm1,level=0.95)) 
}

gtdmonths<-function(gtd) {
  
  #February was warming the fastest, at 1.042604
  #September was warming the slowest, at 0.7465756
  #Output of function also prints this
  
  cat("Slopes of linear models for all 12 months: \n")
  
  lmJan<-lm(gtd$Jan~gtd$Year)
  cat("(1) Jan: ")
  cat(lmJan$coefficients[2],"\n")
  Jan<-lmJan$coefficients[2]
  
  lmFeb<-lm(gtd$Feb~gtd$Year)
  cat("(2) Feb: ")
  cat(lmFeb$coefficients[2],"\n")
  Feb<-lmFeb$coefficients[2]
  
  lmMar<-lm(gtd$Mar~gtd$Year)
  cat("(3) Mar: ")
  cat(lmMar$coefficients[2],"\n")
  Mar<-lmMar$coefficients[2]
  
  lmApr<-lm(gtd$Apr~gtd$Year)
  cat("(4) Apr: ")
  cat(lmApr$coefficients[2],"\n")
  Apr<-lmApr$coefficients[2]
  
  lmMay<-lm(gtd$May~gtd$Year)
  cat("(5) May: ")
  cat(lmMay$coefficients[2],"\n")
  May<-lmMay$coefficients[2]
  
  lmJun<-lm(gtd$Jun~gtd$Year)
  cat("(6) Jun: ")
  cat(lmJun$coefficients[2],"\n")
  Jun<-lmJun$coefficients[2]
  
  lmJul<-lm(gtd$Jul~gtd$Year)
  cat("(7) Jul: ")
  cat(lmJul$coefficients[2],"\n")
  Jul<-lmJul$coefficients[2]
  
  lmAug<-lm(gtd$Aug~gtd$Year)
  cat("(8) Aug: ")
  cat(lmAug$coefficients[2],"\n")
  Aug<-lmAug$coefficients[2]
  
  lmSep<-lm(gtd$Sep~gtd$Year)
  cat("(9) Sep: ")
  cat(lmSep$coefficients[2],"\n")
  Sep<-lmSep$coefficients[2]
  
  lmOct<-lm(gtd$Oct~gtd$Year)
  cat("(10) Oct: ")
  cat(lmOct$coefficients[2],"\n")
  Oct<-lmOct$coefficients[2]
  
  lmNov<-lm(gtd$Nov~gtd$Year)
  cat("(11) Nov: ")
  cat(lmNov$coefficients[2],"\n")
  Nov<-lmNov$coefficients[2]
  
  lmDec<-lm(gtd$Dec~gtd$Year)
  cat("(12) Dec: ")
  cat(lmDec$coefficients[2],"\n")
  Dec<-lmDec$coefficients[2]
  
  allmonths<-c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
  maxi<-which(allmonths==max(allmonths), arr.ind=TRUE)
  mini<-which(allmonths==min(allmonths), arr.ind=TRUE)
  cat("Month warming the fastest: ",maxi,"at a rate of:",max(allmonths),"\n")
  cat("Month warming the slowest: ",mini,"at a rate of:",min(allmonths),"\n")
}