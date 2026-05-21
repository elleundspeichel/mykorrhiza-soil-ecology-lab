# root diameter averages
library(dplyr)
source("paths.R")
roots.diam <- read.csv(roots.diam.string)

roots.diam.mean <- roots.diam %>%
  group_by(Sample) %>%
  summarize(mean_diam = mean(Diameter))

write.csv(roots.diam.mean, roots.diam.string)
