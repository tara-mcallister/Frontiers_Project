rm(list=ls())

library(plotrix)
library(dplyr)
library(fields)
library(car)
library(reshape2)
library(plyr)

#Read in data
data <- read.csv("clean_data.csv")
#Check that variables are of correct types
str(data)
data$subject_number <- as.factor(data$subject_number)
#Need to re-set ordered factors
data$session = ordered(data$session, levels = c("BL1","BL2","BL3","BL4","BL5","TX1","TX2","TX3","TX4","TX5","TX6","TX7","TX8","TX9","TX10",
                                                "MP1","MP2","MP3","TX11","TX12",
                                                "TX13","TX14","TX15","TX16","TX17","TX18","TX19","TX20","MN1","MN2","MN3"))   


#This code creates an option to plot only the primary words and exclude generalization words
#Note how these categories are defined: primary means probed in BLMN and PREPOST,
#"generalization" means probed in BLMN only. 
#All are untreated (i.e. not words used as targets during treatment)
#You can ignore it if you are including all words

#Determine which words are BLMN only and which are shared BLMN/PREPOST
length(levels(data$word))
tab2 = as.data.frame(plyr::count(data,"word"))
#  plot(tab2$freq)
#items occurring over 300 times are shared BLMN/PREPOST
#and the items occurring under 100 times are BLMN only.
both <- droplevels(tab2[which(tab2$freq>300),])
primary <- levels(both$word)
only <- droplevels(tab2[which(tab2$freq<150),])
generalization <- levels(only$word)

#Option to reduce to only primary words
primarywords <- droplevels(data[which(data$word%in%primary),])
head(primarywords)
levels(primarywords$word)
primarywords$word_type <- "primary"
generalizationwords <- droplevels(data[which(data$word%in%generalization),])
head(generalizationwords)
levels(generalizationwords$word)
generalizationwords$word_type <- "generalization"
#Only uncomment this if you want to exclude generalization words
#data <- primarywords

data$phat <- 100*data$correct/data$total

str(data)
library(lme4)
library(lmerTest)

#Compare phase 1 vs phase 2
dataTX <- droplevels(data[data$condition!="PROBE",])

mymod <- lmer(phat ~ condition*tx_order + (condition|subject) + (1|word), data=dataTX)
mymodnorandslope <- lmer(phat ~ condition*tx_order + (1|subject) + (1|word), data=dataTX)
anova(mymod, mymodnorandslope)
#Significant, use model w/ random slope
mymodnoint<- lmer(phat ~ condition + tx_order + (condition|subject) + (1|word), data=dataTX)
anova(mymodnorandslope, mymodnoint)
#Significant, include both effects and interaction

summary(mymod)
#Main effect: Trad > BF
#Marginal interaction between order and condition

p1 <- qplot(x = tx_order, y = phat, fill = condition, data=dataTX, geom="boxplot")
p1

#NOTE: This is an estimate--a linear model with phat calculated at the word level.
#This can be found in regression_raw_data.R
#The correct model would be a logistic regression with all individual data points + random effect of rater