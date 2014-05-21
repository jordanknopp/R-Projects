pr2 <- function(t) {
  require(party)
  
  t$survived <- as.factor(t$survived)
  t$pclass <- as.factor(t$pclass)
  t$embarked <- as.factor(t$embarked)
  t$sex <- as.factor(t$sex)
  t$sibsp <- as.factor(t$sibsp)
  t$parch <- as.factor(t$parch)
  attach(t)
  
  ttree <- tree(survived ~ pclass+sex+age+embarked+sibsp+fare)
  
  pt <- ctree(ttree, data=t)
  plot(pt, type="simple")
  survivors <- predict(pt, newdata=t)
  
  length(survivors)
  predictions <- survivors == survived
  print(predictions)
  table(predictions)
}