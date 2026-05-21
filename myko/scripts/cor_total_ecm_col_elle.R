#Myko master linear regression

source("paths.R")
library(ggplot2)

myko.master <- read.csv(myko.master.string)

#### pearson correlations ####

# cor of Ecm col and root diameter
cor.test(myko.master$Total.EcM.col, myko.master$root.diameter.mm)

# cor of Ecm col and Soil nitrogen
cor.test(myko.master$Total.EcM.col, myko.master$soil.N.in..)

# cor of Ecm col and d15N
cor.test(myko.master$Total.EcM.col, myko.master$soil.d15N.unitless.)

# cor of Ecm col and DBH Carbon
cor.test(myko.master$Total.EcM.col, myko.master$DBH.kg.Carbon.)

# cor of Ecm col and pH
cor.test(myko.master$Total.EcM.col, myko.master$pH)
 

#ROOT DIAMETER
## Scatterplots ##

ggplot(myko.master, aes(x=root.diameter.mm., y=Total.EcM.col, fill = Site))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "coral")+
  theme_classic()


##linear model of Ecm and root diameter, look if lines(slopes) of plot are different

ecm.rootD.lm <- lm(Total.EcM.col ~ myko.master$root.diameter.mm, data = myko.master)
ecm.rootD.lm2 <- lm(Total.EcM.col ~ myko.master$root.diameter.mm/Site, data = myko.master)

anova(ecm.rootD.lm, ecm.rootD.lm2)
 

##we can also look if individual site slope is significant

cor.test(myko.master[myko.master$Site == "W",]$Total.EcM.col, myko.master[myko.master$Site == "W",]$root.diameter.mm.)
cor.test(myko.master[myko.master$Site == "K",]$Total.EcM.col, myko.master[myko.master$Site == "K",]$root.diameter.mm.)

##report cor value and p value

#SOIL NITROGEN
## Scatterplots ##

ggplot(myko.master, aes(x=soil.N.in.., y=Total.EcM.col, fill = Site))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "coral")+
  theme_classic()

## linear model of Ecm and soil nitrogen, look if lines(slopes) of plot are different

soilN.lm <- lm(Total.EcM.col ~ myko.master$soil.N.in.., data = myko.master)
soilN.lm2 <- lm(Total.EcM.col ~ myko.master$soil.N.in../Site, data = myko.master)

anova(soilN.lm, soilN.lm2)


## we can also look if individual site slope is significant

cor.test(myko.master[myko.master$Site == "W",]$Total.EcM.col, myko.master[myko.master$Site == "W",]$soil.N.in..)
cor.test(myko.master[myko.master$Site == "K",]$Total.EcM.col, myko.master[myko.master$Site == "K",]$soil.N.in..)

## report cor value and p value

#DELTA 15N
## Scatterplots ##

ggplot(myko.master, aes(x=soil.d15N.unitless., y=Total.EcM.col, fill = Site))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "coral")+
  theme_classic()


# linear model of Ecm and root diameter, look if lines(slopes) of plot are different

soild15N.lm <- lm(Total.EcM.col ~ myko.master$soil.d15N.unitless., data = myko.master)
soild15N.lm2 <- lm(Total.EcM.col ~ myko.master$soil.d15N.unitless./Site, data = myko.master)

anova(soild15N.lm, soild15N.lm2)


# we can also look if individual site slope is significant

cor.test(myko.master[myko.master$Site == "W",]$Total.EcM.col, myko.master[myko.master$Site == "W",]$soil.d15N.unitless.)
cor.test(myko.master[myko.master$Site == "K",]$Total.EcM.col, myko.master[myko.master$Site == "K",]$soil.d15N.unitless.)

## report cor value and p value

#pH
## Scatterplots ##

ggplot(myko.master, aes(x=pH, y=Total.EcM.col, fill = Site))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "coral")+
  theme_classic()

##linear model of Ecm and pH, look if lines(slopes) of plot are different

pH.lm <- lm(Total.EcM.col ~ myko.master$pH, data = myko.master)
pH.lm2 <- lm(Total.EcM.col ~ myko.master$pH/Site, data = myko.master)

anova(pH.lm, pH.lm2)


##we can also look if individual site slope is significant

cor.test(myko.master[myko.master$Site == "W",]$Total.EcM.col, myko.master[myko.master$Site == "W",]$pH)
cor.test(myko.master[myko.master$Site == "K",]$Total.EcM.col, myko.master[myko.master$Site == "K",]$pH)

## report cor value and p value

#DBH
## Scatterplots

ggplot(myko.master, aes(x=DBH.kg.Carbon., y=Total.EcM.col, fill = Site))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "coral")+
  theme_classic()


##linear model of Ecm and BDH, look if lines(slopes) of plot are different

DBH.lm <- lm(Total.EcM.col ~ myko.master$DBH.kg.Carbon., data = myko.master)
DBH.lm2 <- lm(Total.EcM.col ~ myko.master$DBH.kg.Carbon./Site, data = myko.master)

anova(DBH.lm, DBH.lm2)


##we can also look if individual site slope is significant

cor.test(myko.master[myko.master$Site == "W",]$Total.EcM.col, myko.master[myko.master$Site == "W",]$DBH.kg.Carbon.)
cor.test(myko.master[myko.master$Site == "K",]$Total.EcM.col, myko.master[myko.master$Site == "K",]$DBH.kg.Carbon.)

## report cor value and p value

#SOIL CARBON
## Scatterplots

ggplot(myko.master, aes(x=Soil.Carbon..., y=Total.EcM.col, fill = Site))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "coral")+
  theme_classic()


# linear model of Ecm and root diameter, look if lines(slopes) of plot are different

soilC.lm <- lm(Total.EcM.col ~ myko.master$Soil.Carbon..., data = myko.master)
soilC.lm2 <- lm(Total.EcM.col ~ myko.master$Soil.Carbon.../Site, data = myko.master)

anova(soilC.lm, soilC.lm2)


# we can also look if individual site slope is significant

cor.test(myko.master[myko.master$Site == "W",]$Total.EcM.col, myko.master[myko.master$Site == "W",]$Soil.Carbon...)
cor.test(myko.master[myko.master$Site == "K",]$Total.EcM.col, myko.master[myko.master$Site == "K",]$Soil.Carbon...)

## report cor value and p value