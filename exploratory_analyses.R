
### Arabidopsis and VOCs; exploratory analyses


# Packages needed

library(readxl)
library(dplyr)
library(ggpubr)
library(tidyverse)



# Load data

herbivory <- read_excel("data/arabidopsis_herbivory.xlsx")[,1:8]
herbivory$genopop <- paste0(herbivory$population, herbivory$genotype) #add a combination of population and genotype
herbivory$treatment <- factor(herbivory$treatment, levels = c("o", "h"))

voc <- read_excel("data/arabidopsis_volatiles.xlsx")



# Normality tests

ggdensity(herbivory$herbivory)

ggqqplot(herbivory$herbivory)

shapiro.test(herbivory$herbivory)



# POPULATIONS

kruskal.test(herbivory ~ population, data = herbivory)

mean(herbivory$herbivory[which(herbivory$population == "B")])
mean(herbivory$herbivory[which(herbivory$population == "C")])

ggplot(herbivory, aes(x = population, y = herbivory)) +
  geom_boxplot() + theme_classic()



# TREATMENTS

kruskal.test(herbivory ~ treatment, data = herbivory)

ggplot(herbivory, aes(x = treatment, y = herbivory)) +
  geom_boxplot() + theme_classic()

mean(herbivory$herbivory[which(herbivory$treatment == "o")])
mean(herbivory$herbivory[which(herbivory$treatment == "h")])



# POPULATION AND TREATMENT

ggplot(herbivory, aes(x = population, y = herbivory)) +
  geom_boxplot(aes(fill = treatment)) + theme_classic()

b <- subset(herbivory, population == "B")
c <- subset(herbivory, population == "C")

mean(b$herbivory[which(b$treatment == "o")])
mean(b$herbivory[which(b$treatment == "h")])

mean(c$herbivory[which(c$treatment == "o")])
mean(c$herbivory[which(c$treatment == "h")])



# GENOTYPES

ggplot(herbivory, aes(x = genopop, y = herbivory)) +
  geom_boxplot() + theme_classic()



unique(herbivory$genopop)




