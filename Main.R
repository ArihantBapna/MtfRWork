months <- CauseOfLoss$MonthOfLoss

desc <- factor(CauseOfLoss$CauseOfLossDescription)
remove(desc)


summer <- select(filter(CauseOfLoss, MonthOfLoss >= 6, MonthOfLoss <= 8),c(Year,MonthOfLoss, MonthOfLossName, CauseOfLossDescription, NetPlantedAcres,PoliciesIndemnified,Liability, Indemnity))

remove(months)

p<- ggplot(data=summer, aes(x=factor(summer$CauseOfLossDescription), y=sum(summer$Indemnity), fill=(summer$NetPlantedAcres))) +
  geom_bar(stat="identity",
           size=.3) +                        # Thinner lines
  xlab("Cause of loss") + ylab("Indemnity") + # Set axis labels
  ggtitle("Liabilities for policiies") +     # Set title
  theme_bw()+
  coord_flip()
p

for(i in levels(factor(summer$CauseOfLossDescription))){
  varSumDb <- select(filter(summer, CauseOfLossDescription == i), c(Indemnity))
  varSum <- sum(varSumDb$Indemnity)
  print(i) 
  print(varSum)
  print(" ")
}

