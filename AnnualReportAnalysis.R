p <- c()

for(i in levels(factor(AnnualReport$`County Name`))){
  db <- select(filter(AnnualReport$`County Name`==i), c(`Commodity Year`,`Liabilities`, `Indemnity`))
  sumLoss <- sum(db$`Indemnity`)
  sumLib <- sum(db$`Liabilities`)
  x <- (sumLib - sumLoss)/(sumLib)
  p <- c(p,x)
}
