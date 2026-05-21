source("paths.R")
library(ggplot2)

ecm.label<- read.csv(ecm.label.string)

#anova is good for normally distributed residuals of data
#aov function

ecm.label.m <- ecm.label[ecm.label$Identifier.2=="Mykorrhiza",]
ecm.label.m <- ecm.label.m[!ecm.label.m$Identifier.1=="Control",]
 ###RUN ANOVA on delta-15N###
##emanating hyphae
d.aov.EH <- aov(d.15N.14N~EH, data = ecm.label.m)

##looks prety okay, qqnorm should be line
qqnorm(d.aov.EH$residuals)
hist(d.aov.EH$residuals)

anova(d.aov.EH)

##EH + RM
d.aov.EHRM <- aov(d.15N.14N~(EH+RM)^2, data = ecm.label.m)

##looks prety okay, qqnorm should be line
qqnorm(d.aov.EHRM$residuals)
hist(d.aov.EHRM$residuals)

anova(d.aov.EHRM)

###boxplot to show difference in uptake of N regarding EM and RM
ggplot(ecm.label.m, aes(x = as.factor(EH), y = d.15N.14N, fill = as.factor(RM)))+ 
  geom_boxplot()+
    theme_classic()
         
##ANOVA for colorl diff

d.aov.color <- aov(d.15N.14N ~ Colour, data = ecm.label.m)
qqnorm(d.aov.color$residuals)
hist(d.aov.color$residuals)

anova(d.aov.color)

###boxplot to show difference in uptake of N regarding colour
ggplot(ecm.label.m, aes(x = as.factor(Colour), y = d.15N.14N))+ 
  geom_boxplot()+
  theme_classic()

TukeyHSD(d.aov.color)
#white and black tan

obs.ecm<- read.csv(obs.ecm.string)
obs.ecm <- obs.ecm[10:19]

##ANOVA for COlonization levels comparing the two sites

s.aov.col <- aov(Total.EcM.col ~ Site, data = obs.ecm)
qqnorm(s.aov.col$residuals)
hist(s.aov.col$residuals)

anova(s.aov.col)


###boxplot 
ggplot(obs.ecm, aes(x = as.factor(Site), y = Total.EcM.col))+ 
  geom_boxplot()+
  theme_classic()

##ANOVA for COlonization levels comparing the two sites

s.aov.col <- aov(Site ~ Total.EcM.col, data = obs.ecm)
qqnorm(s.aov.col$residuals)
hist(s.aov.col$residuals)

anova(s.aov.col)


###boxplot 
ggplot(obs.ecm, aes(x = as.factor(Site), y = Total.EcM.col))+ 
  geom_boxplot()+
  theme_classic()+
  geom_jitter() 


