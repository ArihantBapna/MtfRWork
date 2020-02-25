p <- c()
q <- c()
for(i in levels(factor(AnnualReport$`County Name`))){
  db <- select(filter(AnnualReport,AnnualReport$`County Name` == i), c(`Commodity Year`,`Liabilities ($)`, `Indemnity ($)`))
}


for(j in levels(factor(AnnualReport$`Commodity Year`))){
  db2 <- select(filter(AnnualReport,AnnualReport$`Commodity Year` == j), c(`Commodity Year`, `Indemnity ($)` ))
  sumLoss <- sum(db2$`Indemnity ($)`)
  p <- c(p,sumLoss)
  q <- c(q,j)
}

df <- data.frame("Years" = q, "Indemnity" = p )
plot <- plot_ly(df,x=~Years, y=~Indemnity, orientation='v', type='scatter')
plot


remove(i,p,db,sumLoss,sumLib,x)

