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


p <- ggplot(AnnualSummary,aes(x=AnnualSummary$Year,y=AnnualSummary$Pindem))+
  geom_boxplot()

p <- ggplotly(p)
p
cor(AnnualSummary$PLoss,AnnualSummary$Precip)
cor.test(AnnualSummary$Precip,AnnualSummary$PLoss)

fit <- lm(AnnualSummary$PLoss~AnnualSummary$Precip)
summary(fit)
anova(fit)
chisq.test(AnnualSummary$Year, AnnualSummary$Indemnity)
``