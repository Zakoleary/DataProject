###########################################################
#Import dataset as HouseData

HouseData <- read.csv("PPRDataCombined.csv", header = TRUE, sep = ",")


###########################################################
#Import Libraries used

library(data.table)
library(gridExtra)
library(corrplot)
library(GGally)
library(ggplot2)  
library(e1071)
library(dplyr)
library(plotly)
library(Hmisc)
library(psych)
library(car)
library(bigmemory)
library(biganalytics)
options(bigmemory.typecast.warning=FALSE)

memory.size()
memory.limit(size = 16000)
###########################################################
#Split HouseData into train and test for prediction purposes

split <- sample(seq_len(nrow(HouseData)), size = floor(0.75 * nrow(HouseData)))
train <- HouseData[split, ]
test <- HouseData[split, ]

SubHouseLm <- SubHouse[sample(1:nrow(SubHouse), 30000,
                              replace=FALSE),]

dim(train)
glimpse(train)


colSums(sapply(train, is.na))


#Visualize the missing data <- use if else (ifelse)
options(scipen = 999)
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) +
    scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}
options(scipen = 999)
plot_Missing(train[,colSums(is.na(train)) > 0])

#Check For Duplications
cat("The number of duplicated rows are", nrow(train) - nrow(unique(train)))

#Check the correlation between House price and Counties, Normal data.
options(scipen = 999)
train %>% select(ppr_county, price) %>% ggplot(aes(factor(ppr_county),
                                                             price)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('County')

#Easier visulisation to see House Price per Area
theme_set(theme_bw())
# Draw plot
ggplot(train, aes(x=ppr_county, y=price)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="House Price per county", 
       subtitle="", 
       caption="") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

############################################################
#Create Subset of House Data 
SubHouse <- subset(HouseData, select = c(address, price,ppr_county, year, region, geo_county, latitude, longitude))
head(SubHouse)
sapply(SubHouse, function(x) sum(is.na(x)))
#Hurts Computer... pairs(SubHouse, col="red")
############################################################
fit <- lm(price ~ address + ppr_county + year + region + geo_county + latitude + longitude, data = SubHouseLm)
#Using a sample size of 1000 rows with the 8 columns we can see an Adjusted R-Squared value of 0.9477
#Using a sample size of 2000 rows with the 8 columns we can see an Adjusted R-Squared value of 0.9514
#Using a sample size of 3000 rows with the 8 columns we can see an Adjusted R-Squared value of 0.9922
#Using a sample size of 4000 rows with the 8 columns we can see an Adjusted R-Squared value of 0.9681
#Using a sample size of 5000 rows with the 8 columns we can see an Adjusted R-Squared value of 0.8715
#Using a sample size of 7000 rows with the 8 columns we can see an Adjusted R-Squared value of 0.9556
#Using a sample size of 9000 rows with the 8 columns we can see an Adjusted R-Squared value of 0.9957
#Using a sample size of 10000 rows with the 8 columns we can see an Adjusted R-Squared value of 0.9926
#Using a sample size of 15000 rows with the 8 columns we can see an Adjusted R-Squared value of 0.9241
summary(fit)
 
