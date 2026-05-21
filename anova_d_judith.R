library(ggplot2)

source("paths.R")
ecm.label<-read.csv(ecm.label.string)

#### trying anova ####

ecm.label.m<-ecm.label[ecm.label$Identifier.2=="Mykorrhiza",]
ecm.label.m<-ecm.label.m[!ecm.label.m$Identifier.1=="Control",]

# run anova on d15N
d.aov.EH<-aov(d.15N.14N~EH,data=ecm.label.m)

# test normal distribution of residuals
qqnorm(d.aov.EH$residuals)
hist(d.aov.EH$residuals)

# good enough
anova(d.aov.EH)

# anova of EH + RM and if they interact
d.aov.EH.RM<-aov(d.15N.14N~(EH+RM)^2,data=ecm.label.m)

# test normal distribution of residuals
qqnorm(d.aov.EH.RM$residuals)
hist(d.aov.EH.RM$residuals)

# good enough
anova(d.aov.EH.RM)

ggplot(ecm.label.m, aes(x= as.factor(EH), y=d.15N.14N, fill = as.factor(RM)))+
  geom_boxplot()+
  theme_classic()+
  labs(x = "EH", fill = "RM")

# anova on color
d.aov.col<-aov(d.15N.14N~Colour,data=ecm.label.m)

# test normal distribution of residuals
qqnorm(d.aov.col$residuals)
hist(d.aov.col$residuals)

# good enough
anova(d.aov.col)

TukeyHSD (d.aov.col)

ggplot(ecm.label.m, aes(x= as.factor(Colour), y=d.15N.14N))+
  geom_boxplot()+
  theme_classic()



