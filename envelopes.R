ME_once <- function( amount_found=1.0, sors="switch", verbose=FALSE) {
  
  amount_found<-1.0
  switch_multiplier<-sample(1:2,1)
  
  if (verbose) {
    chosen_envelope<-3
    amount_found<-sample(1:100,1)
    
    cat("You are given two indistinguishable envelopes, each of which contains a 
    positive sum of money. One envelope contains twice as much as the other. 
    You may pick one envelope and keep whatever amount it contains.\n")
    cat("Choose an envelope: (0,1) \n")
    chosen_envelope <- scan(n=1)
    switch_envelope<-(chosen_envelope+1)%%2
    cat("You chose envelope ",chosen_envelope,"\n")
    cat("Before opening your envelope, you may switch and take the other envelope.\n")
    cat("Would you like to switch?  (switch/stay)\n")
    sors <- scan(what=" ",n=1)
  } 
  
  switch_amount<-0
  if(switch_multiplier==2) {
    switch_amount<-(amount_found)*2
  }
  else {
    switch_amount<-(amount_found)*0.5
  }
  
  if (sors == "stay" & verbose) {
    cat("You still have envelope", chosen_envelope, ".\n")
    cat("Your envelope contains exactly ",amount_found, " money...units.\n")
  }
  else {                                  # sors == switch 
    amount_found<-switch_amount
    if (verbose) {
      cat("You chose to switch.\n")
      cat("You now have envelope",switch_envelope,".\n")
      cat("Your envelope contains exactly",amount_found, "money...units.\n")
    }
  }      
  
  return(amount_found)
}

ME_ntimes <- function( n=100 ) {
  results<-replicate(n, ME_once(1.0,"switch",FALSE))
  #hist(results,xlab="Amount Found in Envelope",ylab="Frequency",main="Histogram of Envelope-Opening Results")
  return(mean(results))
}

sample_ME <- function(run_me=100) {
  super.results<-replicate(run_me,ME_ntimes(100))
  hist(super.results,xlab="Amount Found in Envelope",ylab="Frequency",main="Histogram of Envelope-Opening Results")
  return(mean(super.results))
  
  #histogram of ME_ntimes function is left commented out to improve the speed of this function
  
  #A paradox stems from the following idea:
  #If the envelope contains the amount A, then the other envelope contains 2A with probability 0.5 and A/2 with probability 0.5
  #Expected value of the amount in the other envelope: 0.5(2A) + 0.5(A/2) = 1.25(A)
  #Since 1.25A < A, it follows that switching is the better choice.
  #Reapplying the above equation after switching yields the absurdity that switching infinitely is the best course of action.
  #This is a fallacy, as A represents different things in the expected value equation.
  #It would be more correct to assume there are ony two possible sums in either of the envelopes, and to call the 
  #lesser amount S.  The expected value then becomes:
  #0.5(S) + 0.5(2S) = 1.5(S)
  #But in this equation, S always represents the same quantity, and tells us that 1.5(S) is the average expected value
  #in EITHER of the two envelopes.  So swapping infinitely does not yield an infinite expected value.
}



