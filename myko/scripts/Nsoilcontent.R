source("paths.R")
library(ggplot2)

N.soil <- read.csv(N.soil.string)

#remove DW_ and _ from the column
N.soil$Identifier.1 <- gsub("DW_", "", N.soil$Identifier.1)
N.soil$Identifier.1 <- gsub("_", "", N.soil$Identifier.1)
