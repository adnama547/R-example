library(plyr)
library(ggplot2)
library(dplyr)
library(reshape2)

ConditionedMedia<-read.csv("/Users/paulson/Desktop/Desktop/ConditionedMedia_props.csv", header = TRUE)
#substrate<-cbind(ConditionedMedia, "sen"=c(0,0,0,0,0,0,0,0,0), "other"=c(0,0,0,0,0,0,0,0,0), "one"=c(0,0,0,0,0,0,0,0,0), "two"=c(0,0,0,0,0,0,0,0,0))

data<-melt(ConditionedMedia)
#data$Substrate<-factor(data$Substrate,levels(data$Substrate)[c(1,4,3,2,5,6)])
head(data)
stats<-ddply(data, c("CM_type", "variable"), summarise,
      Mean = mean(value), SD = sd(value),
      SEM = sd(value)/sqrt(length(value)),
      CI95 = qt(0.975,df=length(value)-1)*SEM)
ggplot(stats, aes(x=CM_type,y=Mean, ymin=Mean-SEM, ymax=Mean+SEM, fill=variable))+
  #facet_grid(~variable) +      
  geom_bar(position=position_dodge(0.9), stat="identity")+
  geom_errorbar(position=position_dodge(0.9), width=0.2)+
  theme(text=element_text(size=14), legend.justification=c(0,0), legend.position="bottom", legend.background = element_rect(fill=alpha(.0001)))+
  xlab("Sorted Into")+
  ylab("Phenotype Frequency")+
  ggtitle("Global feedback from conditioned media")+
  guides(fill=guide_legend(title=NULL))


stats
