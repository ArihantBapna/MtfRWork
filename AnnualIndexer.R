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
remove(pLoss)
write.csv(AnnualSummary,'AnnSum.csv')

p <- ggplot(AnnualSummary,aes(y=AnnualSummary$Pindem))+
  geom_boxplot()

p <- ggplotly(p)
p
cor(AnnualSummary$Pindem,AnnualSummary$Value)
cor.test(AnnualSummary$Pindem,AnnualSummary$Value)
cor.test(AnnualSummary$Indemnity,AnnualSummary$Value)

cor(AnnualSummary$Panom,AnnualSummary$Indemnity)

fit1 <- lm(AnnualSummary$Indemnity~AnnualSummary$Value + AnnualSummary$Precip)
fit2 <- lm(AnnualSummary$Indemnity~AnnualSummary$Value)
summary(fit)
anova(fit1,fit2)
chisq.test(AnnualSummary$Year, AnnualSummary$Indemnity)
step <- stepAIC(fit1,direction="both")
step$anova

