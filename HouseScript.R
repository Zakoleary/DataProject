#Read in 2010 dataset for cleaning
House2010 <- read.csv('C:/Users/Admin/Desktop/KDD/CSV/HousePrice/PPR-2010.csv', header = TRUE)
#Check
str(House2010)

nrow(House2010)

dim(House2010)

summary(House2010)

House2010[3:5]

str(House2010)
complete.cases(House2010)

x <- House2010[complete.cases(House2010), ]
str(x)

x <- na.omit(House2010)
str(House2010)

House2010[3]

#Noticing there are a huge amount of postal codes that don't have data assigned
#Load dplyr to assign values as NA -> na.omit to find Dublin Postal codes
library(dplyr)
House2010%>% head(100)

House2010 <- read.csv('C:/Users/Admin/Desktop/KDD/CSV/HousePrice/PPR-2010.csv', na.strings = c("","NA"))
House2010 = House2010 %>% na.omit()
House2010 %>% head(100)
#Working correctly
House2010$Not.Full.Market.Price <- NULL
House2010$VAT.Exclusive <-NULL
head(House2010)
str(House2010)

NewData <-House2010[c(T,F,F,F,T,F,F,F,F)]
names(NewData)
rdate<-as.Date(NewData$Date.of.Sale..dd.mm.yyyy.,"%y")


