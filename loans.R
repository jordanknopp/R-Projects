pred_from_FICO_Score <- function(linearModel, fico)
{
  prediction <- predict(linearModel, newdata=data.frame(FICO.Score=fico) )
  return(prediction[[1]])
}

pred_from_Loan_Length <- function(linearModel, loan_length)
{
  #loan_length must be either "36 months" or "60 months".  Typed as a string, just like that.
  prediction <- predict(linearModel, newdata=data.frame(Loan.Length=loan_length) )
  return(prediction[[1]])
}

pred_from_State <- function(linearModel, state)
{
  #State typed as an abbreviation, and a string.
  prediction <- predict(linearModel, newdata=data.frame(State=state) )
  return(prediction[[1]])
}

pred_from_Loan_Purpose <- function(linearModel, purpose)
{
  #Possible purposes:
  #car, credit_card, debt_consolidation, educational, home_improvement, house, major_purchase, medical,
  #moving, other, renewable_energy, small_business, vacation, wedding.  Must be typed as a string.
  prediction <- predict(linearModel, newdata=data.frame(Loan.Purpose=purpose) )
  return(prediction[[1]])
}

pred_from_Employment_Length <- function(linearModel, employment_length)
{
  #employment_length must be either n/a or "x years" where x is [1-9], <1, 10+
  prediction <- predict(linearModel, newdata=data.frame(Employment.Length=employment_length) )
  return(prediction[[1]])
}



dual<-function(lm1, lm2, lm3, lm4, lm5, state, employment_length, purpose, fico, loan_length) {
  
  #sample call: dual(ir_vs_st,ir_vs_el,ir_vs_lp,ir_vs_fs,ir_vs_ll,"SC","<1 year","debt_consolidation",735,"36 months")
  
  #***note: this was an old function, that morphed into modular, the function below
  
  prediction_st <- predict(lm1, newdata=data.frame(State=state))
  prediction_el <- predict(lm2, newdata=data.frame(Employment.Length=employment_length))
  prediction_lp <- predict(lm3, newdata=data.frame(Loan.Purpose=purpose))
  prediction_fs <- predict(lm4, newdata=data.frame(FICO.Score=fico) )
  prediction_ll <- predict(lm5, newdata=data.frame(Loan.Length=loan_length) )
  
  cat(prediction_st[[1]],"\n")
  cat(prediction_el[[1]],"\n")
  cat(prediction_lp[[1]],"\n")
  cat(prediction_fs[[1]],"\n")
  cat(prediction_ll[[1]],"\n")
  
  
  #idea here was to give more weight to the FICO score by putting into the mean several times
  #hopefully would have made a better prediction, but tests did not seem to show that
  test<-c(prediction_st[[1]],prediction_el[[1]],prediction_lp[[1]],prediction_fs[[1]],prediction_ll[[1]],
          prediction_fs[[1]],prediction_fs[[1]],prediction_fs[[1]],prediction_fs[[1]],prediction_ll[[1]],
          prediction_ll[[1]],prediction_ll[[1]])
  test<-mean(test)
  cat("mean: ",test)
  #prediction<-mean(prediction_st,prediction_el,prediction_lp)
  #return(prediction)
  
}



modular<-function(loanData, info) {
  #function that uses any linear model for interest rate prediction
  #loanData is the data you pass in, to let the function handle building all linear models so you don't have to
  #info is a list whose format is ([variable],[value],[variable],[value],...)
  
  #sample call: modular(ld,c("Employment.Length","6 years"))
  #sample call 2: modular(ld,c("Loan.Length","60 months","FICO.Score",750))
  #strings have quotes, numerical values do not
  
  
  #lines of code in this function are organized by length because it looked the best.
  
  #linear model creation
  ir_vs_fs<-lm(Interest.Rate ~ FICO.Score, loanData)
  ir_vs_ln<-lm(Interest.Rate ~ Loan.Number, loanData)
  ir_vs_mi<-lm(Interest.Rate ~ Monthly.Income, loanData)
  ir_vs_ar<-lm(Interest.Rate ~ Amount.Requested, loanData)
  ir_vs_st<-lm(Interest.Rate ~ as.factor(State), loanData)
  ir_vs_oc<-lm(Interest.Rate ~ Open.CREDIT.Lines, loanData)
  ir_vs_dr<-lm(Interest.Rate ~ Debt.To.Income.Ratio, loanData)
  ir_vs_ll<-lm(Interest.Rate ~ as.factor(Loan.Length), loanData)
  ir_vs_lp<-lm(Interest.Rate ~ as.factor(Loan.Purpose), loanData)
  ir_vs_rb<-lm(Interest.Rate ~ Revolving.CREDIT.Balance, loanData)
  ir_vs_ho<-lm(Interest.Rate ~ as.factor(Home.Ownership), loanData)
  ir_vs_af<-lm(Interest.Rate ~ Amount.Funded.By.Investors, loanData)
  ir_vs_el<-lm(Interest.Rate ~ as.factor(Employment.Length), loanData)
  ir_vs_iq<-lm(Interest.Rate ~ Inquiries.in.the.Last.6.Months, loanData)
  

  
  i<-1
  while (i<length(info)) 
  {
    
    if (info[i]=="State"){lm<-ir_vs_st; newdata=data.frame(State=toString(info[i+1]))}
    else if (info[i]=="FICO.Score"){lm<-ir_vs_fs; newdata=data.frame(FICO.Score=as.integer(info[i+1]))}
    else if (info[i]=="Loan.Length") {lm<-ir_vs_ll; newdata=data.frame(Loan.Length=toString(info[i+1]))}
    else if (info[i]=="Loan.Purpose"){lm<-ir_vs_lp; newdata=data.frame(Loan.Purpose=toString(info[i+1]))}
    else if (info[i]=="Home.Ownership"){lm<-ir_vs_ho; newdata=data.frame(Home.Ownership=toString(info[i+1]))}
    else if (info[i]=="Monthly.Income"){lm<-ir_vs_mi; newdata=data.frame(Monthly.Income=as.integer(info[i+1]))}
    else if (info[i]=="Amount.Requested"){lm<-ir_vs_ar; newdata=data.frame(Amount.Requested=as.integer(info[i+1]))}
    else if (info[i]=="Employment.Length"){lm<-ir_vs_el; newdata=data.frame(Employment.Length=toString(info[i+1]))}
    else if (info[i]=="Open.CREDIT.Lines"){lm<-ir_vs_oc; newdata=data.frame(Open.CREDIT.Lines=as.integer(info[i+1]))}
    else if (info[i]=="Debt.To.Income.Ratio"){lm<-ir_vs_dr; newdata=data.frame(Debt.To.Income.Ratio=as.integer(info[i+1]))}
    else if (info[i]=="Revolving.CREDIT.Balance"){lm<-ir_vs_rb; newdata=data.frame(Resolving.CREDIT.Balance=as.integer(info[i+1]))}
    else if (info[i]=="Amount.Funded.By.Investors") {lm<-ir_vs_af; newdata=data.frame(Amount.Funded.By.Investors=as.integer(info[i+1]))}
    else if (info[i]=="Inquiries.in.the.Last.6.Months"){lm<-ir_vs_iq; newdata=data.frame(Inquiries.in.the.Last.6.Months=as.integer(info[i+1]))}
    else { cat("You did something wrong.  Exiting.")
           return[0] }
    
    
    
    prediction<-(predict(lm, newdata))
    cat("Prediction for: ",info[i], " using ",info[i+1],": ",prediction[[1]],"\n")
    
    i<-i+2
    
  }
  
#I was going to average all the predictions together, but that doesn't really seem to make much better of a prediction
  #since...the important idea here is that simply looking at one or two variables won't tell you too much about interest
  #rate because loans are such individualized phenomena
}