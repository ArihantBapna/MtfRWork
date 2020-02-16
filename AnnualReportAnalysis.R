p <- c()

for(i in levels(factor(AnnualReport$`County Name`))){
  db <- select(filter(AnnualReport,str_detect(AnnualReport$`County Name`,i)), c(`Commodity Year`,`Liabilities ($)`, `Indemnity ($)`))
  sumLoss <- sum(db$`Indemnity ($)`)
  sumLib <- sum(db$`Liabilities ($)`)
  x <- (sumLib - sumLoss)/(sumLib)
  p <- c(p,x)
}
df <- data.frame("County" = levels(factor(AnnualReport$`County Name`)), "Percent.Loss.Incurred" = p )
plot <- plot_ly(f,x=~County, y=~Percent.Loss.Incurred, orientation='h')

remove(i,p,db,sumLoss,sumLib,x)
