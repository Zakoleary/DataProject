library(keras)
library(readr)
library(tidyverse)
library(magrittr)
library(dplyr)
library(neuralnet)
library(mlbench)

data2 <- read_csv("PPRDataCombined11.csv")

data2omit<-na.omit(data2)



colnames(data2)[colnames(data2)=="Age dependency ratio"] <- "ADR"
colnames(data2)[colnames(data2)=="GINI index (World Bank estimate)"] <- "GINI"
colnames(data2)[colnames(data2)=="GDP per capita"] <- "GDP"
colnames(data2)[colnames(data2)=="Population growth (annual %)"] <- "PopPercentGrowth"
colnames(data2)[colnames(data2)=="Population, total [SP.POP.TOTL]"] <- "PopTotal"

str(data2omit)

data2 %<>% mutate_if(is.factor, is.numeric)

Dublin <- subset(data2omit, ppr_county=="Dublin",
                 select=c(price, year, date, ADR, GINI, PopPercentGrowth,PopTotal))


n <- neuralnet(price ~ year + ADR + GINI + GDP + PopPercentGrowth + PopTotal,
               data = data2omit,
               hidden = c(10,5),
               linear.output = F,
               lifesign = "full",
               rep = 1)
plot(n,
     col.hidden = "darkgreen",
     col.hidden.synapse = "darkgreen",
     show.weights = F,
     information = F,
     fill = "Lightblue")

data2omit <- as.matrix(data2omit)
dimnames(data2omit) <- NULL

set.seed(1234)
ind <- sample(2,nrow(data2omit), replace = T, prob = c(.7,.3))
training <- data2omit[ind==1,1:6]
test <- data2omit[ind==2,1:6]
training









