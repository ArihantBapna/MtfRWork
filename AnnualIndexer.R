remove(AnnualLosses)
AnnualSummary <- cbind(AnnualSummary,AnnualTemp)
remove(AnnualLvT)
remove(AnnualClimate)
remove(AnnualTemp)

pLoss <- ((AnnualSummary$Indemnity / AnnualSummary$Liabilities)*100) / AnnualSummary$Quantity
pLoss <- pLoss * 10e8
AnnualSummary <- cbind(AnnualSummary,"PLoss"=pLoss)
AnnualSummary <- cbind(AnnualSummary, AnnualPrecip)
AnnualSummary <- select(AnnualSummary,-c(PLoss))
remove(AnnualPrecip)


p <- ggplot(AnnualSummary,aes(x=AnnualSummary$Value, y=AnnualSummary$Precip))+
  geom_point()+
  geom_smooth()
p <- ggplotly(p)
p
cor(AnnualSummary$PLoss,AnnualSummary$Precip)
cor.test(AnnualSummary$Precip,AnnualSummary$PLoss)
