rm(list=ls())

library(plotrix)
library(dplyr)
library(fields)
library(car)
library(reshape2)
library(plyr)
library(magrittr)

#Read in data
data <- read.csv("clean_data.csv")
#Check that variables are of correct types
str(data)
data$subject_number <- as.factor(data$subject_number)
#Need to re-set ordered factors
data$session = ordered(data$session, levels = c("BL1","BL2","BL3","BL4","BL5","TX1","TX2","TX3","TX4","TX5","TX6","TX7","TX8","TX9","TX10",
                                                "MP1","MP2","MP3","TX11","TX12",
                                                "TX13","TX14","TX15","TX16","TX17","TX18","TX19","TX20","MN1","MN2","MN3"))   


#This code creates an option to examine only the primary probe words 
#and exclude less frequently occurring generalization items
#Note how these categories are defined: primary means probed in BLMN and PREPOST,
#"generalization" means probed in BLMN only. 
#All are untreated (i.e. not words used as targets during treatment)
#Determine which words are BLMN only and which are shared BLMN/PREPOST
    length(levels(data$word))
    tab2 = as.data.frame(plyr::count(data,"word"))
    #plot(tab2$freq)
    #items occurring over 300 times are shared BLMN/PREPOST
    #and the items occurring under 200 times are BLMN only
    both <- droplevels(tab2[which(tab2$freq>300),])
    primary <- levels(both$word)
    only <- droplevels(tab2[which(tab2$freq<200),])
    generalization <- levels(only$word)

#Option to reduce to only primary words
#You can ignore it if you are including all words
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

ESPhase1 <- (mp1_m - bl1_m)/pooled1

#Effect size 2: MP to MN
pooled2a <- sqrt(((nmp-1)*(mp1_sd^2) + (nmn-1)*(mn1_sd^2))/(nmp+nmn-2))

pooled2 <- ((nmp-1)*(mp1_sd^2)) %>%
  add((nmn-1)*(mn1_sd^2)) %>%
  divide_by(nmp+nmn-2) %>%
  raise_to_power(1/2)

pooled2a==pooled2

ESPhase2 <- (mn1_m - mp1_m)/pooled2

#Effect size 3: BL to MN
pooled3a <- sqrt(((nbl-1)*(bl1_sd^2) + (nmn-1)*(mn1_sd^2))/(nbl+nmn-2))

pooled3 <- ((nbl-1)*(bl1_sd^2)) %>%
  add((nmn-1)*(mn1_sd^2)) %>%
  divide_by(nbl+nmn-2) %>%
  raise_to_power(1/2)

pooled3a==pooled3

ESall <- (mn1_m - bl1_m)/pooled2


bl1_m <- round(bl1_m, digits=2)
bl1_sd <- round(bl1_sd, digits=2)
mp1_m <- round(mp1_m, digits=2)
mp1_sd <- round(mp1_sd, digits=2)
mn1_m <- round(mn1_m, digits=2)
mn1_sd <- round(mn1_sd, digits=2)
pooled1 <- round(pooled1, digits=2)
ESPhase1 <- round(ESPhase1, digits=2)
pooled2 <- round(pooled2, digits=2)
ESPhase2 <- round(ESPhase2, digits=2)
pooled3 <- round(pooled3, digits=2)
ESall <- round(ESall, digits=2)

data1 <- data.frame(Children, bl1_m, bl1_sd,mp1_m, mp1_sd, mn1_m, mn1_sd, pooled1, ESPhase1, pooled2, ESPhase2, pooled3, ESall)
colnames(data1)[1] <- "subject" 
str(data1)

#Add demographic info
demog  = read.csv("BFS2_demog.csv", header=T)
demog$subject <- as.factor(demog$subject)
data2 <- left_join(data1, demog, by="subject")
data2$subject <- as.factor(data2$subject)
str(data2)

#Code by treatment type
TRAD1 <- droplevels(subset(data2, tx_order=="TRAD_BF"))
TRAD1$ES_TRAD <- TRAD1$ESPhase1
TRAD1$ES_BF <- TRAD1$ESPhase2

BF1 <- droplevels(subset(data2, tx_order=="BF_TRAD"))
BF1$ES_TRAD <- BF1$ESPhase2
BF1$ES_BF <- BF1$ESPhase1

data3 <- rbind(TRAD1, BF1) 

#Diff in effect size between BF and TRAD conditions
#Putting BF first because hypothesized to have larger effect
data3$BF_advantage <- data3$ES_BF - data3$ES_TRAD

#Diff in effect size between first and second phases
#Putting Phase 2 first because hypothesized to show cumulative effect
data3$order_effect <- data3$ESPhase2 - data3$ESPhase1

######################################################
######################################################
#Write complete data table to file
#write.csv(data3, "cohort2_EF_data.csv")
#######################################################
######################################################

#Mean effect size across all participants for trad phase and BF phase
tradmeanES <- mean(data3$ES_TRAD)
BFmeanES <- mean(data3$ES_BF)

#Mean difference between BF and TRAD
#Small BF advantage, but lots of variability (large SD)
meanBFadvantage <- mean(data3$BF_advantage) 
sdBFadvantage <- sd(data3$BF_advantage) 

#Mean diff between phase 1 and phase 2 
#Generally a larger effect in phase 1 than phase 2, unexpectedly
#Note large SD
meanorder <- mean(data3$order_effect)
sdorder <- sd(data3$order_effect)

#Does the comparison of phase 1 vs phase 2 look different in BF-first vs TRAD-first?
TRAD1 <- droplevels(subset(data3, tx_order=="TRAD_BF"))
meanTRADorder <- mean(TRAD1$order_effect)
sdTRADorder <- sd(TRAD1$order_effect)

BF1 <- droplevels(subset(data3, tx_order=="BF_TRAD"))
meanBForder <- mean(BF1$order_effect)
sdBForder <- sd(BF1$order_effect)

#Both groups show larger generalization gains in phase 1 than phase 2
#But the difference is greater (advantage for phase 1 is greater) for BF-first group
#Note that this is not in keeping with the prediction that BF is better for acquisition 
#and TRAD is better for generalization

#Does overal effect size look different in BF-first vs TRAD-first?
meanTRADfirst <- mean(TRAD1$ESall)
sdTRADfirst <- sd(TRAD1$ESall)

meanBFfirst <- mean(BF1$ESall)
sdBFfirst <-sd(BF1$ESall)
#Large absolute difference, but extremely variable across participants; not significant
t.test(TRAD1$ESall, BF1$ESall)
