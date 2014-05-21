MH <- function(chosen_curtain=1,sors="switch",verbose=TRUE)
{
  prize <- sample(1:3,1)
  
  if (verbose)  #technically none of this needs to happen when verbose==FALSE, including the while loop
  {
    cat("Aside: [The prize is behind #",prize, "]\n")
    cat("Welcome! Choose a curtain: (1,2,3)\n")
  
    
    if (chosen_curtain == 0)  #in the case that chosen_curtain isn't specified when calling function
    {
      while (chosen_curtain == 0)
      {
        chosen_curtain <- scan(n=1)
        if (chosen_curtain > 3 | chosen_curtain < 0)  #the while loop checks for valid input...e.g. -9 ruins the math
        {
          chosen_curtain <- 0
          cat("Please choose either 1, 2, or 3.  Or try entering a letter if you're intent on ruining the program.\n")
        }
      }
    }
    cat("You chose:",chosen_curtain, "\n")
  }
    
  g = 6-chosen_curtain-prize 
  if (prize == chosen_curtain)
  {
    g = prize + 1
    if (g > 3) {garbage = 1}
  }
  
  switch_curtain = 6-prize-g
  
  if (verbose)  #don't need to request input, as verbose==FALSE implies we will already have it
  {
    cat("It's good you didn't choose curtain ",g,"\n")
    cat("Because that had a consolation prize...\n")
    cat("Do you want to switch? (Type either switch/stay) \n")
  
    if (sors != "switch" & sors != "stay")
    {
      sors <- 0
      while(sors == 0) #loop to check for valid input
      {
        sors <- scan(what=" ",n=1)
        if (sors == "stay")
        {
          break
        }
        if (sors == "switch")
        {
          break
        }
        cat("If you don't type \"switch\" or \"stay\", this contest will never end.\n")
        sors <- 0
      }
    } 
  
    if (sors == "stay")
    {
      cat("You chose to stay.\n")
      cat("Your curtain is still", chosen_curtain, ".\n")
    }
    else # sors == switch
    {
      cat("You chose to switch.\n")
      cat("Your curtain is now",switch_curtain,".\n")
    }
  
    cat("The prize was behind curtain",prize,".\n")
  }
  
  
  
  if((sors == "switch" & chosen_curtain==prize) | (sors == "stay" & chosen_curtain!=prize))
  {
    if (verbose)
    {
      cat("You have lost.  You may hang your head in shame.  The universe has it out for you.\n")
      cat("Consider playing again until you win so that you may feel better.\n")
    }
    return(FALSE)
  }
  else
  {
    if (verbose)
    {
      cat("A winner is you!\n")
    }
    return(TRUE)
  }
}