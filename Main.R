months <- CauseOfLoss$MonthOfLoss

desc <- factor(CauseOfLoss$CauseOfLossDescription)
remove(desc)



remove(months)

p<- ggplot(data=summer, aes(x=factor(summer$CauseOfLossDescription), y=sum(summer$Indemnity), fill=(summer$NetPlantedAcres))) +
  geom_bar(stat="identity",
           size=.3) +                        # Thinner lines
  xlab("Cause of loss") + ylab("Indemnity") + # Set axis labels
  ggtitle("Liabilities for policiies") +     # Set title
  theme_bw()+
  coord_flip()
p



