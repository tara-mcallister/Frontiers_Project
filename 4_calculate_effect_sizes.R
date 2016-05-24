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
#You can ignore it if you are including all words

#Determine which words are BLMN only and which are shared BLMN/PREPOST
length(levels(data$word))
tab2 = as.data.frame(plyr::count(data,"word"))
plot(tab2$freq)
#items occurring over 300 times are shared BLMN/PREPOST
#and the items occurring under 100 times are BLMN only.
both <- droplevels(tab2[which(tab2$freq>300),])
primary <- levels(both$word)
only <- droplevels(tab2[which(tab2$freq<100),])
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

#Calculate average n of tokens per session
str(data)
levels(data$session)

tab = plyr::count(data, c("subject","session","prepost"))
tab <- as.data.frame(tab)
BLMN <- tab[which(tab$prepost=="NA"),]
PREPOST <- tab[which(tab$prepost!="NA"),]
mBLMN <- mean(BLMN$freq)
sdBLMN <- sd(BLMN$freq)
mPREPOST <- mean(PREPOST$freq)
sdPREPOST <- sd(PREPOST$freq)

#Determine which words are BLMN only and which are shared BLMN/PREPOST
length(levels(data$word))
tab2 = as.data.frame(plyr::count(data,"word"))
plot(tab2$freq)
#items occurring over 300 times are shared BLMN/PREPOST
#and the items occurring under 100 times are BLMN only.
both <- droplevels(tab2[which(tab2$freq>300),])
primary <- levels(both$word)
only <- droplevels(tab2[which(tab2$freq<100),])
generalization <- levels(only$word)

#Option to plot only the primary words and exclude generalization words
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

tx1 <- data
str(tx1)
Children <- levels(tx1$subject)

#True baseline points
#For each child, calculate the percent of "yes" votes out of total votes in each session
#And average across sessions
bl1 <- droplevels(subset(tx1, sessiontype=="BL"))
nbl <- c()
bl1_sum <- c()
bl1_total <- c()
bl1_perc <- c()
bl1_m <- c()
bl1_sd <-c()
for (i in seq_along(Children)){
  child <- droplevels(subset(bl1, subject==Children[i],))
  nbl[i] <- length(levels(child$session))
  bl1_sum <- tapply(child$correct, child$session, sum, na.rm=TRUE)
  bl1_total <- tapply(child$total, child$session, sum, na.rm=TRUE)
  bl1_perc <- bl1_sum*100/bl1_total
  bl1_m[i] <- mean(bl1_perc)
  bl1_sd[i] <- sd(bl1_perc)
}

#Midpoints
#For each child, calculate the percent of "yes" votes out of total votes in each session
#And average across sessions
mp1 <- droplevels(subset(tx1, sessiontype=="MP"))
nmp <- c()
mp1_sum <- c()
mp1_total <- c()
mp1_perc <- c()
mp1_m <- c()
mp1_sd <-c()
for (i in seq_along(Children)){
  child <- droplevels(subset(mp1, subject==Children[i],))
  nmp[i] <- length(levels(child$session))
  mp1_sum <- tapply(child$correct, child$session, sum, na.rm=TRUE)
  mp1_total <- tapply(child$total, child$session, sum, na.rm=TRUE)
  mp1_perc <- mp1_sum*100/mp1_total
  mp1_m[i] <- mean(mp1_perc)
  mp1_sd[i] <- sd(mp1_perc)
}

#True maintenance points
mn1 <- subset(tx1, sessiontype=="MN")
nmn <- c()
mn1_sum <- c()
mn1_total <- c()
mn1_perc <- c()
mn1_m <- c()
mn1_sd <-c()
for (i in seq_along(Children)){
  child <- droplevels(subset(mn1, subject==Children[i],))
  nmn[i] <- length(levels(child$session))
  mn1_sum <- tapply(child$correct, child$session, sum, na.rm=TRUE)
  mn1_total <- tapply(child$total, child$session, sum, na.rm=TRUE)
  mn1_perc <- mn1_sum*100/mn1_total
  mn1_m[i] <- mean(mn1_perc)
  mn1_sd[i] <- sd(mn1_perc)
}

#Effect size 1: BL to MP
pooled1a <- sqrt(((nbl-1)*(bl1_sd^2) + (nmp-1)*(mp1_sd^2))/(nbl+nmp-2))

pooled1 <- ((nbl-1)*(bl1_sd^2)) %>%
  add((nmp-1)*(mp1_sd^2)) %>%
  divide_by(nbl+nmp-2) %>%
  raise_to_power(1/2)

pooled1a==pooled1

ES1 <- (mp1_m - bl1_m)/pooled1

#Effect size 2: MP to MN
pooled2a <- sqrt(((nmp-1)*(mp1_sd^2) + (nmn-1)*(mn1_sd^2))/(nmp+nmn-2))

pooled2 <- ((nmp-1)*(mp1_sd^2)) %>%
  add((nmn-1)*(mn1_sd^2)) %>%
  divide_by(nmp+nmn-2) %>%
  raise_to_power(1/2)

pooled2a==pooled2

ES2 <- (mn1_m - mp1_m)/pooled2

#Effect size 3: BL to MN
pooled3a <- sqrt(((nbl-1)*(bl1_sd^2) + (nmn-1)*(mn1_sd^2))/(nbl+nmn-2))

pooled3 <- ((nbl-1)*(bl1_sd^2)) %>%
  add((nmn-1)*(mn1_sd^2)) %>%
  divide_by(nbl+nmn-2) %>%
  raise_to_power(1/2)

pooled3a==pooled3

ES3 <- (mn1_m - bl1_m)/pooled2


bl1_m <- round(bl1_m, digits=2)
bl1_sd <- round(bl1_sd, digits=2)
mp1_m <- round(mp1_m, digits=2)
mp1_sd <- round(mp1_sd, digits=2)
mn1_m <- round(mn1_m, digits=2)
mn1_sd <- round(mn1_sd, digits=2)
pooled1 <- round(pooled1, digits=2)
ES1 <- round(ES1, digits=2)
pooled2 <- round(pooled2, digits=2)
ES2 <- round(ES2, digits=2)
pooled3 <- round(pooled3, digits=2)
ES3 <- round(ES3, digits=2)

data1 <- as.data.frame(cbind(Children, bl1_m, bl1_sd,mp1_m, mp1_sd, mn1_m, mn1_sd, pooled1, ES1, pooled2, ES2, pooled3, ES3))
colnames(data1)[1] <- "subject" 

#Add demographic info
demog  = read.csv("BFS2_demog.csv", header=T)
demog$subject <- as.factor(demog$subject)
data2 <- left_join(data1, demog, by="subject")
data2$subject <- as.factor(data2$subject)
str(data2)

#Code by treatment type
TRAD1 <- droplevels(subset(data2, tx_order=="TRAD_BF"))
TRAD1$ES_TRAD <- TRAD1$ES1
TRAD1$ES_BF <- TRAD1$ES2

BF1 <- droplevels(subset(data2, tx_order=="BF_TRAD"))
BF1$ES_TRAD <- BF1$ES2
BF1$ES_BF <- BF1$ES1

data3 <- rbind(TRAD1, BF1) 
data3$ES_TRAD <- as.numeric(as.character(data3$ES_TRAD))
data3$ES_BF <- as.numeric(as.character(data3$ES_BF))
str(data3)

#Diff in effect size
data3$BF_advantage <- data3$ES_BF - data3$ES_TRAD
data3


write.csv(data3, "cohort2_EF_data.csv")
