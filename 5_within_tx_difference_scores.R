rm(list=ls())

library(plotrix)
library(dplyr)
library(fields)
library(car)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

#Read in data
data <- read.csv("clean_data.csv")
#Check that variables are of correct types
str(data)
data$subject_number <- as.factor(data$subject_number)

#Consider only treatment session data
TX <- droplevels(subset(data, sessiontype=="TX"))
ntx <- length(levels(TX$session))

#Need to re-set ordered factors
TX$session = ordered(TX$session, levels = c("TX1","TX2","TX3","TX4","TX5","TX6","TX7","TX8","TX9","TX10",
                                            "TX11","TX12","TX13","TX14","TX15","TX16","TX17","TX18","TX19","TX20"))   
TX$prepost = ordered(TX$prepost, levels= c("Pre","Post"))

#Summarize by subject, session, prepost
#Include condition (BF vs TRAD) and tx_order as grouping factors
#because you want to preserve this info
TX <- TX %>%
  dplyr::group_by(subject, session, prepost, condition, tx_order) %>%
    dplyr::summarise(correct = sum(correct), total = sum(total))
TX$percent <- 100*TX$correct/TX$total
TX <- as.data.frame(TX)

#Calculate within-session differences by subject
#First use spread() to put pre and post side by side
#Get rid of other numeric columns or they will create NA values
TX$correct <-NULL
TX$total <- NULL
TX <- spread(TX, prepost, percent)

#Create a difference column
#Subtract pre from post on assumption that post will be higher
TX$diff <- TX$Post - TX$Pre
head(TX)

#Visualize the values
TX$tx_order[1]
#Loop through participants
Children <- levels(TX$subject)
numerical <- 1:length(levels(TX$session))
for (i in seq_along(Children)){
  child <- droplevels(TX[TX$subject==Children[i],])
  condition <- as.character(child$tx_order[1])
  plot(child$session, child$diff, type="o",
       xaxt="n", xlab = "Session", ylab = "Mean rating", main = c(Children[i],condition), ylim = c(-50, 50))
  axis(1, at=numerical,  cex.axis = 1.25)
  xline(10.5, lty=2)
  yline(0, lty=2)
}

#Calculate means for each phase
#Temp option to strike Hannah data due to missing session
TX <- droplevels(TX[TX$subject!="Hannah",])
str(TX)

#Code by treatment phase (chronological order; 1 or 2)
TX$phase <- 0
TX[which(TX$session<="TX10"),]$phase <- "1"
TX[which(TX$session>"TX10"),]$phase <- "2"
TX$unique <- paste0(TX$subject,"_",TX$phase)


#Summarize by phase
TX <- TX %>%
  dplyr::group_by(subject, phase, condition, unique) %>%
    dplyr::summarise(mean=mean(diff))

TX <- as.data.frame(TX)

#Compare mean within-session change for first vs second phase (chronologically)
phase1 <- droplevels(TX[TX$phase==1,])
phase2 <- droplevels(TX[TX$phase==2,])
mean(phase1$mean)
sd(phase1$mean)
mean(phase2$mean)
sd(phase2$mean)
t.test(phase1$mean, phase2$mean)
#NS


#Compare mean within-session change for Trad phase vs BF phase 
Trad <- droplevels(TX[TX$condition=="Trad",])
BF <- droplevels(TX[TX$condition=="BF",])
mean(Trad$mean)
sd(Trad$mean)
mean(BF$mean)
sd(BF$mean)
#Mean is larger for BF than Trad, but highly variable; nonsignificant
t.test(BF$mean, Trad$mean)
#NS
#I don't know why this boxplot isn't working
#qplot(TX$mean, fill = TX$condition, geom="boxplot")

#Compare Trad vs BF means in a within-subject fashion
TX$phase <- NULL
TX <- TX %>% 
  spread(condition, mean)
t.test(TX$BF, TX$Trad, paired=TRUE)
#Still definitely NS









