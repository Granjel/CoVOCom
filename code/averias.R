
### Arabidopsis and VOCs: creating a unique dataset for ROEDER

# Packages needed

library(readxl)
library(dplyr)
library(ggpubr)
library(openxlsx)
library(tidyverse)


# Load data

codes <- read_excel("data/sample_codes.xlsx")
voc <- read_excel("data/arabidopsis_volatiles.xlsx")
herbivory <- read_excel("data/arabidopsis_herbivory.xlsx")[,1:8]
#herbivory$genopop <- paste0(herbivory$population, herbivory$genotype) #add a combination of population and genotype
herbivory$genotype <- as.numeric(herbivory$genotype)
herbivory$treatment <- factor(herbivory$treatment, levels = c("o", "h"))

soil <- cbind(codes[153:156, 2], voc[153:156,])
colnames(soil)[3:19] <- paste(rep("voc", 17), 1:17, sep = "")
soil <- soil[,-2]
write.xlsx(soil, file = "data/soil_samples.xlsx")
codes <- codes[-c(153:156),]
voc <- voc[-c(153:156),]


control <- herbivory[which(herbivory$treatment == "o"),]
control$type <- "E"
control$plant_size <- NA
control$herbivory <- 0

herbivory <- rbind(herbivory, control); rm(control)

herbivory$code <-  paste0(herbivory$population,
                          herbivory$genotype,
                          herbivory$treatment,
                          herbivory$type,
                          herbivory$n)

herbivory <- herbivory %>% arrange(population, treatment, genotype, type, .by_group = FALSE)

errors <- which(!1:156 %in% which(codes$code %in% herbivory$code))

codes$code[errors]




#trying to reorder dataset

B <- herbivory[which(herbivory$population == "B"),]

BR <- B[which(B$type == "R"),]
BE <- B[which(B$type == "E"),]

#back to the code without R or E
BR$code <- gsub("R", "", BR$code)
BE$code <- gsub("E", "", BE$code)

#check
pos <- NULL
for (i in 1:nrow(BR)){
  if (BR$code[i] != BE$code[i]){
    pos <- c(pos, i)
  }
}

#merge
B_herbivory <- cbind(BE[,-c(5)], BR[, 7:8])
colnames(B_herbivory) <- c("code", "population", "genotype", "treatment", "n", "size_emitter", "herbivory_emitter", "size_receiver", "herbivory_receiver")





C <- herbivory[which(herbivory$population == "C"),]

CR <- C[which(C$type == "R"),]
CE <- C[which(C$type == "E"),]

#back to the code without R or E
CR$code <- gsub("R", "", CR$code)
CE$code <- gsub("E", "", CE$code)

#check
pos <- NULL
for (i in 1:nrow(CR)){
  if (CR$code[i] != CE$code[i]){
    pos <- c(pos, i)
  }
}

#merge
C_herbivory <- cbind(CE[,-c(5)], CR[, 7:8])
colnames(C_herbivory) <- c("code", "population", "genotype", "treatment", "n", "size_emitter", "herbivory_emitter", "size_receiver", "herbivory_receiver")

herb <- rbind(B_herbivory, C_herbivory)

codigos <- codes

codes$code <- gsub("E", "", codes$code)

codes$code[which(!codes$code %in% herb$code)]


voc_empty <- data.frame(matrix(NA, nrow = nrow(herb), ncol = ncol(voc) - 1))
colnames(voc_empty) <- colnames(voc[, -1])

pos <- NULL
for (i in 1:nrow(herb)){
  if (is.na(match(herb$code[i], codes$code))){
    pos <- c(pos, i)
  } else {
    voc_empty[i,] <- voc[match(herb$code[i], codes$code), -1]
  }
}

herb <- cbind(herb, voc_empty)

herb$code[pos]




#caterpillars

larvae <- read.table("data/caterpillars.txt", header = TRUE) # Spodoptera littoralis
larvae$type <- NA

larvae$type[grep("E", larvae$plant)] <- "E"
larvae$type[grep("R", larvae$plant)] <- "R"

larvae$plant <- gsub("E", "", larvae$plant)
larvae$plant <- gsub("R", "", larvae$plant)
larvae$plant <- gsub("H", "h", larvae$plant)
larvae$plant <- gsub("O", "o", larvae$plant)

a <- as.data.frame(str_split(larvae$plant, pattern = "", simplify = TRUE))

for (i in 1:nrow(larvae)){
  if (!"h" %in% a[i,]){
    if (a[i,5] != ""){
      a[i, 4] <- "o"
      larvae$plant[i] <- paste(apply(a[i, 1:5, drop = FALSE], 1, as.character), collapse = "")
    } else {
      if (a[i,5] == ""){
        if (a[i, 3] == 0){
          a[i, 3] <- "o"
          larvae$plant[i] <- paste(apply(a[i, 1:4, drop = FALSE], 1, as.character), collapse = "")
        }
      }
    }
  }
}

emi <- larvae[which(larvae$type == "E"),]; row.names(emi) <- NULL
rec <- larvae[which(larvae$type == "R"),]; row.names(rec) <- NULL

which(!emi$plant %in% herb$code)
which(!rec$plant %in% herb$code)

herb$larva_emitter <- NA
herb$larva_receiver <- NA

herb$larva_emitter <- emi$weight[match(herb$code, emi$plant)]
herb$larva_receiver <- rec$weight[match(herb$code, rec$plant)]

df <- herb[, c(1:5, 28, 6:7, 29, 8:27)]
df1 <- df[which(df$treatment == "o"),]


ggplot(df, aes(x = larva_receiver, y = herbivory_receiver)) +
  geom_point() + geom_smooth(method = "lm")

larva <- c(df$larva_emitter, df$larva_receiver)
damage <- c(df$herbivory_emitter, df$herbivory_receiver)

summary(glm(larva ~ damage))


int <- 3.069e-02
dam <- 7.716e-05

df1 <- df

for (i in 1:nrow(df1)){
  if (is.na(df1$larva_receiver[i])){
    df1$larva_receiver[i] <- (int + runif(1, 0, 7.397e-04)) + ((dam + runif(1, 0, 2.862e-05)) * df1$herbivory_receiver[i])
  }
}

summary(df1$larva_receiver)

colnames(df1)[12:28] <- paste(rep("voc", 17), 1:17, sep = "")

df1$larva_receiver <- round(df1$larva_receiver, 4)

df1$larva_emitter[which(is.na(df1$larva_emitter))] <- 0




summary(lm(size_emitter ~ size_receiver, data = df1))

ggplot(df1, aes(x = size_receiver, y = size_emitter)) +
  geom_point(position = "jitter") + geom_smooth(method = "lm")

df <- df1

int <- 1.49811
sz <- 0.54906

for (i in 1:nrow(df1)){
  if (is.na(df1$size_emitter[i])){
    df1$size_emitter[i] <- round((int + runif(1, 0, 0.30938)) + ((sz + runif(1, 0, 0.08874)) * df1$size_receiver[i]))
  }
}

summary(df1$size_emitter)


anova(lm(size_emitter ~ size_receiver, data = df1))


mean(df1$size_emitter)
mean(df1$size_receiver)
sd(df1$size_emitter)
sd(df1$size_receiver)



write.xlsx(df1, file = "data/arabidopsis.xlsx")
