#Preliminar Analysis

library(readxl)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(fitdistrplus)

df <- read_excel("data/arabidopsis.xlsx")
df$genotype <- as.factor(df$genotype)


ggplot(df, aes(x = total, y = herbivory_receiver)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 100)) +
  geom_smooth(method = "gam", se = FALSE) +
  theme_classic()


descdist(df$herbivory_receiver)
descdist(df[which(!is.na(df$total)),]$total)



# Install and load the lme4 package
library(lme4)

# Assuming df is your data frame
# Fit a mixed-effects beta regression model
summary(glmer(voc9 + 1 ~ herbivory_emitter + (1 | larva_emitter), family = Gamma(link = log), data = df))

# Summarize the model
summary(model)






library(lmerTest)
library(vegan)
library(lsmeans)

lmerTest::lm()


lm(voc1 ~ treatment, data = df)

anova(lm(voc1 ~ treatment * population, data = df))
anova(lm(voc2 ~ treatment * population, data = df))
anova(lm(voc3 ~ treatment * population, data = df))
anova(lm(voc4 ~ treatment * population, data = df))
anova(lm(voc5 ~ treatment * population, data = df))
anova(lm(voc6 ~ treatment * population, data = df))
anova(lm(voc7 ~ treatment * population, data = df))
anova(lm(voc8 ~ treatment * population, data = df))
anova(lm(voc9 ~ treatment * population, data = df))
anova(lm(voc10 ~ treatment * population, data = df))
anova(lm(voc11 ~ treatment * population, data = df))
anova(lm(voc12 ~ treatment * population, data = df))
anova(lm(voc13 ~ treatment * population, data = df))
anova(lm(voc14 ~ treatment * population, data = df))
anova(lm(voc15 ~ treatment * population, data = df))
anova(lm(voc16 ~ treatment * population, data = df))
anova(lm(voc17 ~ treatment * population, data = df))

lsmeans(lm(voc14 ~ treatment * population, data = df), c("treatment", "population"))


kruskal.test(lm(voc14 ~ treatment * population * genotype, data = df[complete.cases(df),]))

kruskal.test(voc14 ~ treatment, data = df)
anova(lm(voc14 ~ treatment, data = df))



lmer(herbivory_receiver)




ggplot(df, aes(x = treatment, y = herbivory_receiver, color = treatment)) +
  geom_boxplot(alpha = 0.15) + theme_classic() +
  geom_point(position = position_jitter(width = 0.15, seed = 122), alpha = 0.45)

mean(df$herbivory_receiver[which(df$treatment == "o")])
mean(df$herbivory_receiver[which(df$treatment == "h")])

df$


ggplot(df, aes(x = genotype, y = total, fill = treatment)) +
  geom_boxplot() +
  theme_classic()

library(vegan)

vegan::adonis2(data = df[complete.cases(df),],
               formula = total ~ treatment, permutations = 999, method = "bray")





soil <- read_excel("data/soil_samples.xlsx")

length(which(df[, 12:28] == 0)) / prod(dim(df[, 12:28])) * 100



sm <- apply(soil[,-1], 2, mean)

df_soil <- df
for (i in 1:nrow(df_soil)){
  df_soil[i, 12:28] <- df_soil[i, 12:28] - sm
}

df_soil[, 12:28] <- apply(df_soil[, 12:28], 2, function(x) ifelse(x < 0, 0, x))

length(which(df_soil[, 12:28] == 0)) / prod(dim(df_soil[, 12:28])) * 100

df <- df_soil


0.8 * (1 - (0.5 / 0.8))

0









