#Clear environment
rm(list = ls(all.names = TRUE))
require(ggplot2)
require(ape)
require(vegan)

source("paths.R")

ecm.label <- read.csv(ecm.label.string)
ecm.label.m <- ecm.label[ecm.label$Identifier.2 == "Mykorrhiza",]
ecm.label.m <- ecm.label.m[!ecm.label.m$Identifier.1 == "Control",]

#Run an ANOVA on delta 15 N
d.aov.eh <- aov(d.15N.14N ~ EH, data = ecm.label.m)
qqnorm(d.aov.eh$residuals)
hist(d.aov.eh$residuals)
anova(d.aov.eh)


#Run an ANOVA on delta 15 N
d.aov.eh.r <- aov(d.15N.14N ~ (EH + RM)^2, data = ecm.label.m)
qqnorm(d.aov.eh.r$residuals)
hist(d.aov.eh.r$residuals)
anova(d.aov.eh.r)

ggplot(ecm.label.m, aes(x = as.factor(EH), y = d.15N.14N, fill = as.factor(RM)))+
  geom_boxplot()+
  theme_classic()


#Let's make a PCA of the colonization attributes and see how they vary among each other and then in relation to d15N
traits <- data.frame(
  EH = ecm.label.m$EH,
  RM = ecm.label.m$RM,
  Sclerotia = ecm.label.m$Sclerotia,
  Cystidia = ecm.label.m$Cystidia,
  Color = as.numeric(as.factor(ecm.label.m$Colour)))

traits2 <- data.frame(
  EH = ecm.label.m$EH,
  RM = ecm.label.m$RM,
  Sclerotia = ecm.label.m$Sclerotia,
  Cystidia = ecm.label.m$Cystidia,
  Color = as.numeric(as.factor(ecm.label.m$Colour)),
  Delta15N = ecm.label.m$d.15N.14N,
  Nitrogen = ecm.label.m$Amt..N)

# Euclidean distance matrix
dist_mat <- vegdist(scale(traits2), method = "euclidean")

# PCoA
pcoa_res <- pcoa(sqrt(dist_mat))
scores <- data.frame(pcoa_res$vectors[,1:2])

#Now fit the individual columns
fit <- envfit(scores, scale(traits2), permutations = 999)
vec <- as.data.frame(scores(fit, display = "vectors"))
vec$trait <- rownames(vec)

#Now make a visualization
ggplot(scores, aes(Axis.1, Axis.2)) +
  geom_point(size = 3, color = "grey") +
  geom_segment(
    data = vec,
    aes(
      x = 0,
      y = 0,
      xend = Axis.1,
      yend = Axis.2),
    color = "pink", size = 2,
    arrow = arrow(length = unit(0.25, "cm"))) +
  geom_text(
    data = vec,
    aes(x = 1.2*Axis.1, y = 1.2*Axis.2, label = trait, color = "pink"), size = 5,
    vjust = -0.5, show.legend = FALSE) +
  theme_classic()+
  labs(y = "PCoA1 (23%)", x = "PCoA2 (19%)")
