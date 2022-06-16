library(shiny)
library(shinyjs)
library(DT)
library(summarytools)
library(vcd)
library(GGally)
library(corrgram)

library(devtools)
install_github("mtennekes/tabplot")
library(car)
library(RColorBrewer)
library(visdat)
library(caret)
library(rpart)
library(rpart.plot)

#library(modeldata)
#library(matrixStats)
library(shinycssloaders)
library(recipes)
library(glmnet)
library(ggplot2)
library(ggrepel)
library(olsrr)

## Read the data from Ass1Data.csv
dat <- read.csv("data.csv", header = TRUE, 
                na.strings=c("NA","-99","--"),
                stringsAsFactors = TRUE)

## Replace numeric missing values to NA
dat[dat==-1] <-NA   

## Create a new level for categorical variable "Not Applicable" missing values
unique(dat$GOVERN_TYPE)
dat$GOVERN_TYPE <- as.character(dat$GOVERN_TYPE)
dat$GOVERN_TYPE[is.na(dat$GOVERN_TYPE)] <- "none"
dat$GOVERN_TYPE <- as.factor(dat$GOVERN_TYPE)

## Assign 0 to numeric variable "Not Applicable" missing values
dat$num_shadow <- as.numeric(is.na(dat$HEALTHCARE_COST)) #create a shadow variable
dat$HEALTHCARE_COST[is.na(dat$HEALTHCARE_COST)] <-0 #assign missing to 0

## EDA Visualisations
# categorical variables
catsdata <- subset(dat, select = c(2,12,15))
choicesA <- colnames(as.data.frame(catsdata))
choicesA_default <- colnames(as.data.frame(catsdata[2:3]))

# numeric variables
numsdata <- subset(dat, select=c(14, 3:11, 13, 16))
choicesB <- colnames(as.data.frame(numsdata))
choicesB_default <- colnames(as.data.frame(numsdata[1:5]))

## Remove excessively missing variables & observations, done by getCleanData in serve
pMiss <- function(x){ sum(is.na(x)) / length(x) * 100 } #calculate missing ratio

## convert HEALTHCARE_COST to categorical, check its range first
range(dat[dat$HEALTHCARE_BASIS =="FREE",]$HEALTHCARE_COST)
range(dat[dat$HEALTHCARE_BASIS =="INSURANCE",]$HEALTHCARE_COST)
range(dat[dat$HEALTHCARE_BASIS =="PRIVATE",]$HEALTHCARE_COST)

## calculate Cook's distance to estimate influential obs 
mod <- lm(DEATH_RATE ~ ., data=numsdata)
cooksd <- cooks.distance(mod)

influ_obs <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))]) 



