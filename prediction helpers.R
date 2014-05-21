pr1 <- function(obs) {
  if (obs$sex=="male" &obs$pclass==1 & obs$survive == 1)
  {
    return(1)
  } 
  else 
  {
    return(0)
  }
}

farecheck <- function(obs,currentfare)
{
  if (currentfare<obs$fare & obs$fare<=(currentfare+1) & obs$survive == 1)
  {
    return(1)
  }
  if (currentfare<obs$fare & obs$fare<=(currentfare+1) & obs$survive == 0)
  {
    return(2)
  }
  else
  {
    return(0)
  }
}



sex <- function(obs2)
{
  if (obs2$sex == "male") 
  {
    return(0)
  }
  else
  {
    return(1)
  }
}

agecheck <- function(obs,currentage)
{
  if (!is.na(obs$age) & (obs$age==currentage) & obs$survive == 1)
  {
    return(1)
  }
  if (!is.na(obs$age) & (obs$age==currentage) & obs$survive == 0)
  {
    return(2)
  }
  else
  {
    return(0)
  }
}