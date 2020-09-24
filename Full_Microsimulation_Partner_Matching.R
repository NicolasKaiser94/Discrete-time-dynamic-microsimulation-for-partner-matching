# install.packages("foreign")
# install.packages("lmtest")
# install.packages("viridis")

library(foreign)
library(dplyr)
library(MASS)
library(nnet)
library(RColorBrewer)
library(viridis)


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#---------------------------------------- DATA PREPARATION -------------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

setwd("C:/Users/Joel Rivas/Documents/Documentos académicos/Data Science Master/3-Tercer semestre/Microsimulation/Poster/Data")

MZPanel <- read.spss("CF-MZP9699.sav", add.undeclared.levels = "no", to.data.frame = TRUE)           #German Microcensus Panel 1996-1999
MZ2010 <- read.spss("mz2010_cf.sav", add.undeclared.levels = "no", to.data.frame = TRUE)             #German Microcensus Panel 2010


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#------------- Data 96-99 -----------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

MZPanel <- MZPanel[MZPanel$GebJ != -1, ]                                                    #Exclude cases

#Create data frame and add PID

MZPanel$PID <- c(1:length(MZPanel$FID)) 
dataPanel <- data.frame(PID = MZPanel$PID )

#Add HID
HID <- unique(data.frame(AWB = MZPanel$AWB_NR, HHNr = MZPanel$HHNr))
MZPanel$HID <- sapply(c(1:nrow(MZPanel)), function(x) which(HID$AWB == MZPanel$AWB_NR[x] & HID$HHNr == MZPanel$HHNr[x]))
dataPanel$HID <- MZPanel$HID
rm(HID)

# Age
dataPanel$age_96  <- 1996 - MZPanel$GebJ                                                      #Add the age in 1996
dataPanel$age_97  <- 1997 - MZPanel$GebJ                                                      #Add the age in 1997
dataPanel$age_98  <- 1998 - MZPanel$GebJ                                                      #Add the age in 1998
dataPanel$age_99  <- 1999 - MZPanel$GebJ                                                      #Add the age in 1999

# Sex
  # -> 1 = male
  # -> 2 = female
dataPanel$sex <- ifelse(as.numeric(MZPanel$Sex) == 3, NA,
                      ifelse(as.numeric(MZPanel$Sex) == 4, 1, 2)) 

# # Employment status
# # -> 1 = employed
# # -> 2 = not employed ("Erwerbslos" + "Sonstig Erwerbslos" + "Nichterwerbsperson")
dataPanel$empl_96 <- ifelse(as.numeric(MZPanel$af504) == 3, NA,
                          ifelse(as.numeric(MZPanel$af504) == 4, 1, 2))

dataPanel$empl_97 <- ifelse(as.numeric(MZPanel$bf504) == 3, NA,
                          ifelse(as.numeric(MZPanel$bf504) == 4, 1, 2))

dataPanel$empl_98 <- ifelse(as.numeric(MZPanel$cf504) == 3, NA,
                          ifelse(as.numeric(MZPanel$cf504) == 4, 1, 2))

dataPanel$empl_99 <- ifelse(as.numeric(MZPanel$df504) == 3, NA,
                          ifelse(as.numeric(MZPanel$df504) == 4, 1, 2))

# Education level
  # 1: no degree
  # 2: secondary school (Volks-/Hauptschulabschluss)
  # 3: Intermediate secondary school (Abschluss POS + Realschul-/gleichwertiger Abschluss)
  # 4: high school (Fachhochschulreife + Allgemeine/fachgebundene Hochschulreife)
dataPanel$edu_96 <- ifelse(as.numeric(MZPanel$af287) %in% c(2, 3), NA,
                         ifelse(as.numeric(MZPanel$af287)== 4, 2,
                                ifelse(as.numeric(MZPanel$af287) %in% c(5, 6), 3,
                                       ifelse(as.numeric(MZPanel$af287) == 1, 1, 4))))


dataPanel$edu_97 <- ifelse(as.numeric(MZPanel$bf287) %in% c(2, 3), NA,
                         ifelse(as.numeric(MZPanel$bf287)== 4, 2,
                                ifelse(as.numeric(MZPanel$bf287) %in% c(5, 6), 3,
                                       ifelse(as.numeric(MZPanel$bf287) == 1, 1, 4))))


dataPanel$edu_98 <- ifelse(as.numeric(MZPanel$cf287) %in% c(2, 3), NA,
                         ifelse(as.numeric(MZPanel$cf287) == 4, 2,
                                ifelse(as.numeric(MZPanel$cf287) %in% c(5, 6), 3,
                                       ifelse(as.numeric(MZPanel$cf287) == 1, 1, 4))))



dataPanel$edu_99 <- ifelse(as.numeric(MZPanel$df287) %in% c(2, 3), NA,
                         ifelse(as.numeric(MZPanel$df287) == 4, 2,
                                ifelse(as.numeric(MZPanel$df287) %in% c(5, 6), 3,
                                       ifelse(as.numeric(MZPanel$df287) == 1, 1, 4))))

# Nationality
  # 1: German
  # 2: Foreigner
recode_nationality <- function(x) {
  x <- as.numeric(x)
  result <- 0
  if (x == 4 | x == 5) { result <- 1 } 
  if (x == 6) { result <- 2} 
  ifelse(result > 0,result,NA)
}

dataPanel$nat_96 <- sapply(MZPanel$af43, recode_nationality)
dataPanel$nat_97 <- sapply(MZPanel$bf43, recode_nationality)
dataPanel$nat_98 <- sapply(MZPanel$cf43, recode_nationality)
dataPanel$nat_99 <- sapply(MZPanel$df43, recode_nationality)

# Number of persons in household

dataPanel$hhmembers_96 <- ifelse(as.numeric(MZPanel$af521) < 4, NA,  as.numeric(MZPanel$af521) - 3)
dataPanel$hhmembers_97 <- ifelse(as.numeric(MZPanel$bf521) < 4, NA,  as.numeric(MZPanel$bf521) - 3)
dataPanel$hhmembers_98 <- ifelse(as.numeric(MZPanel$cf521) < 4, NA,  as.numeric(MZPanel$cf521) - 3)
dataPanel$hhmembers_99 <- ifelse(as.numeric(MZPanel$df521) < 4, NA,  as.numeric(MZPanel$df521) - 3)


# Relationship to the household head
  # 1: household head, 
  # 2: partner of household head,  
  # 0: neither household head nor partner
dataPanel$relhead_96 <-  ifelse(as.numeric(MZPanel$af39) == 4, 1,
                              ifelse(as.numeric(MZPanel$af39) == 5 | as.numeric(MZPanel$af38) == 5, 2,
                                     ifelse(as.numeric(MZPanel$af39) %in% c(1, 6, 7), 0, NA)))

dataPanel$relhead_97 <-  ifelse(as.numeric(MZPanel$bf39) == 4, 1,
                              ifelse(as.numeric(MZPanel$bf39) == 5 | as.numeric(MZPanel$bf38) == 5, 2,
                                     ifelse(as.numeric(MZPanel$bf39) %in% c(1, 6, 7), 0, NA)))

dataPanel$relhead_98 <-  ifelse(as.numeric(MZPanel$cf39) == 4, 1,
                              ifelse(as.numeric(MZPanel$cf39) == 5 | as.numeric(MZPanel$cf38) == 5, 2,
                                     ifelse(as.numeric(MZPanel$cf39) %in% c(1, 6, 7), 0, NA)))

dataPanel$relhead_99 <-  ifelse(as.numeric(MZPanel$df39) == 4, 1,
                              ifelse(as.numeric(MZPanel$df39) == 5 | as.numeric(MZPanel$df38) == 5, 2,
                                     ifelse(as.numeric(MZPanel$df39) %in% c(1, 6, 7), 0, NA)))

# Birth 
# (Simplifying assumption: If a child was born in the respective year, the female household head is considered to be the mother)
dataPanel$birth_96 <- numeric(nrow(dataPanel))
dataPanel$birth_96[which(dataPanel$relhead_96 > 0)] <- sapply(c(1:nrow(dataPanel[which(dataPanel$relhead_96 > 0), ])), 
                                                          function(x) ifelse(dataPanel$sex[which(dataPanel$relhead_96 > 0)][x] == 2 &
                                                                               is.element(1996, MZPanel$GebJ[dataPanel$HID == dataPanel$HID[which(dataPanel$relhead_96 > 0)][x]]) == TRUE, 1, 0))
dataPanel$birth_97 <- numeric(nrow(dataPanel))
dataPanel$birth_97[which(dataPanel$relhead_97 > 0)] <- sapply(c(1:nrow(dataPanel[which(dataPanel$relhead_97 > 0), ])), 
                                                          function(x) ifelse(dataPanel$sex[which(dataPanel$relhead_97 > 0)][x] == 2 &
                                                                               is.element(1997, MZPanel$GebJ[dataPanel$HID == dataPanel$HID[which(dataPanel$relhead_97 > 0)][x]]) == TRUE, 1, 0))
dataPanel$birth_98 <- numeric(nrow(dataPanel))
dataPanel$birth_98[which(dataPanel$relhead_98 > 0)] <- sapply(c(1:nrow(dataPanel[which(dataPanel$relhead_98 > 0), ])), 
                                                          function(x) ifelse(dataPanel$sex[which(dataPanel$relhead_98 > 0)][x] == 2 &
                                                                               is.element(1998, MZPanel$GebJ[dataPanel$HID == dataPanel$HID[which(dataPanel$relhead_98 > 0)][x]]) == TRUE, 1, 0))

dataPanel$birth_99 <- numeric(nrow(dataPanel))
dataPanel$birth_99[which(dataPanel$relhead_99 > 0)] <- sapply(c(1:nrow(dataPanel[which(dataPanel$relhead_99 > 0), ])), 
                                                          function(x) ifelse(dataPanel$sex[which(dataPanel$relhead_99 > 0)][x] == 2 &
                                                                               is.element(1999, MZPanel$GebJ[dataPanel$HID == dataPanel$HID[which(dataPanel$relhead_99 > 0)][x]]) == TRUE, 1, 0))


##Excluding cases where existing two or more household head or two or more partner of household in the same household 


r961 <- dataPanel$HID[which(dataPanel$relhead_96 == 1)]
r962 <- dataPanel$HID[which(dataPanel$relhead_96 == 2)]
dup96 <- c(r962[duplicated(dataPanel$HID[which(dataPanel$relhead_96 == 2)])], 
           r961[duplicated(dataPanel$HID[which(dataPanel$relhead_96 == 1)])])

r971 <- dataPanel$HID[which(dataPanel$relhead_97 == 1)]
r972 <- dataPanel$HID[which(dataPanel$relhead_97 == 2)]
dup97 <- c(r971[duplicated(dataPanel$HID[which(dataPanel$relhead_97 == 1)])], 
           r972[duplicated(dataPanel$HID[which(dataPanel$relhead_97 == 2)])])

r981 <- dataPanel$HID[which(dataPanel$relhead_98 == 1)]
r982 <- dataPanel$HID[which(dataPanel$relhead_98 == 2)]
dup98 <- c(r982[duplicated(dataPanel$HID[which(dataPanel$relhead_98 == 2)])],
           r981[duplicated(dataPanel$HID[which(dataPanel$relhead_98 == 1)])])

r991 <- dataPanel$HID[which(dataPanel$relhead_99 == 1)]
r992 <- dataPanel$HID[which(dataPanel$relhead_99 == 2)]
dup99 <- c(r991[duplicated(dataPanel$HID[which(dataPanel$relhead_99 == 1)])],
         r992[duplicated(dataPanel$HID[which(dataPanel$relhead_99 == 2)])])

tBasePop <- dataPanel[which(!dataPanel$HID %in% c(dup96,dup97,dup98,dup99)),]
rm(r961,r962,r971,r972,r981,r982,r991,r992,dup96,dup97,dup98,dup99)

# Partner
# Determine the partner of the person
# It will be consider a person has a partner when is the household head with a partner or the same household's partner
# if the person has not partner, will be indicated by -1

#96
#-

identify_partner_96 <- function (x){
  result <- -1                                                                        #-1 indicates the person has not partner
  if(is.na(tBasePop$relhead_96[x])) { result <- NA }                                        #-2 indicates missing value
  
  else
  {                                                                                       
    if(tBasePop$relhead_96[x] == 2) {                                                       
      result = tBasePop$PID[which(tBasePop$HID == tBasePop$HID[x] & tBasePop$relhead_96 == 1)]      
    }
    if(tBasePop$relhead_96[x] == 1) {
      result = tBasePop$PID[which(tBasePop$HID == tBasePop$HID[x] & tBasePop$relhead_96 == 2)]
    }
    if(length(result) == 0L){result <- -1}
  }
  result
}

tBasePop$IDpartner_96 <- sapply(c(1:nrow(tBasePop)), identify_partner_96) #Apply the function to every person


#_97
identify_partner_97 <- function (x){
  result <- -1                                                        #-1 indicates the person has not partner
  if(is.na(tBasePop$relhead_97[x])) { result <- NA }                  # missing value
  
  else
  {                                                                                       
    if(tBasePop$relhead_97[x] == 2) {                                                       
      result = tBasePop$PID[which(tBasePop$HID == tBasePop$HID[x] & tBasePop$relhead_97 == 1)]      
    }
    if(tBasePop$relhead_97[x] == 1) {
      result = tBasePop$PID[which(tBasePop$HID == tBasePop$HID[x] & tBasePop$relhead_97 == 2)]
    }
    if(length(result) == 0L){result <- -1}
  }
  result
}

tBasePop$IDpartner_97 <- sapply(c(1:nrow(tBasePop)), identify_partner_97)         #Apply the function to every person

#_98
identify_partner_98 <- function (x){
  result <- -1                                                                        #-1 indicates the person has not partner
  if(is.na(tBasePop$relhead_98[x])) { result <- NA }                                 # missing value
  
  else
  {                                                                                       
    if(tBasePop$relhead_98[x] == 2) {                                                       
      result = tBasePop$PID[which(tBasePop$HID == tBasePop$HID[x] & tBasePop$relhead_98 == 1)]      
    }
    if(tBasePop$relhead_98[x] == 1) {
      result = tBasePop$PID[which(tBasePop$HID == tBasePop$HID[x] & tBasePop$relhead_98 == 2)]
    }
    if(length(result) == 0L){result <- -1}
  }
  result
}

tBasePop$IDpartner_98 <- sapply(c(1:nrow(tBasePop)), identify_partner_98)                           #Apply the function to every person

#_99
identify_partner_99 <- function (x){
  result <- -1                                                                        #-1 indicates the person has not partner
  if(is.na(tBasePop$relhead_99[x])) { result <- NA }                                 # missing value
  
  else
  {                                                                                       
    if(tBasePop$relhead_99[x] == 2) {                                                       
      result = tBasePop$PID[which(tBasePop$HID == tBasePop$HID[x] & tBasePop$relhead_99 == 1)]      
    }
    if(tBasePop$relhead_99[x] == 1) {
      result = tBasePop$PID[which(tBasePop$HID == tBasePop$HID[x] & tBasePop$relhead_99 == 2)]
    }
    if(length(result) == 0L){result <- -1}
  }
  result
}

tBasePop$IDpartner_99 <- sapply(c(1:nrow(tBasePop)), identify_partner_99)                           #Apply the function to every person

#--- has a partner or not

tBasePop$partnership_96 <- sapply(c(1:nrow(tBasePop)), 
                               function(x) ifelse(tBasePop$IDpartner_96[x] > 0,1,0))

tBasePop$partnership_97 <- sapply(c(1:nrow(tBasePop)), 
                                  function(x) ifelse(tBasePop$IDpartner_97[x] > 0,1,0))

tBasePop$partnership_98 <- sapply(c(1:nrow(tBasePop)), 
                                  function(x) ifelse(tBasePop$IDpartner_98[x] > 0,1,0))

tBasePop$partnership_99 <- sapply(c(1:nrow(tBasePop)), 
                                  function(x) ifelse(tBasePop$IDpartner_99[x] > 0,1,0))

#-----
#### Merging the periods
#----
tb96 <- data.frame(PID = tBasePop$PID, 
                   HID = tBasePop$HID,
                   syear = 1996,
                   age = tBasePop$age_96,
                   sex = tBasePop$sex,
                   empl = tBasePop$empl_96,
                   edu = tBasePop$edu_96,
                   nat = tBasePop$nat_96,
                   relhead = tBasePop$relhead_96,
                   IDpartner = tBasePop$IDpartner_96,
                   partnership = tBasePop$partnership_96,
                   hhmembers = tBasePop$hhmembers_96,
                   birth = tBasePop$birth_96)[!is.na(tBasePop$hhmembers_96),]


tb97 <- data.frame(PID = tBasePop$PID, 
                   HID = tBasePop$HID,
                   syear = 1997,
                   age = tBasePop$age_97,
                   sex = tBasePop$sex,
                   empl = tBasePop$empl_97,
                   edu = tBasePop$edu_97,
                   nat = tBasePop$nat_97,
                   relhead = tBasePop$relhead_97,
                   IDpartner = tBasePop$IDpartner_97,
                   partnership = tBasePop$partnership_97,
                   hhmembers = tBasePop$hhmembers_97,
                   birth = tBasePop$birth_97)[!is.na(tBasePop$hhmembers_97),]

tb98 <- data.frame(PID = tBasePop$PID, 
                   HID = tBasePop$HID,
                   syear = 1998,
                   age = tBasePop$age_98,
                   sex = tBasePop$sex,
                   empl = tBasePop$empl_98,
                   edu = tBasePop$edu_98,
                   nat = tBasePop$nat_98,
                   relhead = tBasePop$relhead_98,
                   IDpartner = tBasePop$IDpartner_98,
                   partnership = tBasePop$partnership_98,
                   hhmembers = tBasePop$hhmembers_98,
                   birth = tBasePop$birth_98)[!is.na(tBasePop$hhmembers_98),]

tb99 <- data.frame(PID = tBasePop$PID, 
                   HID = tBasePop$HID,
                   syear = 1999,
                   age = tBasePop$age_99,
                   sex = tBasePop$sex,
                   empl = tBasePop$empl_99,
                   edu = tBasePop$edu_99,
                   nat = tBasePop$nat_99,
                   relhead = tBasePop$relhead_99,
                   IDpartner = tBasePop$IDpartner_99,
                   partnership = tBasePop$partnership_99,
                   hhmembers = tBasePop$hhmembers_99,
                   birth = tBasePop$birth_99)[!is.na(tBasePop$hhmembers_99),]

tBasePop9697 <- merge(tb96, tb97, by = c("PID"))
tBasePop9798 <- merge(tb97, tb98, by = c("PID"))
tBasePop9899 <- merge(tb98, tb99, by = c("PID"))



tBasePop9699 <- rbind(tBasePop9697,
                      tBasePop9798,
                      tBasePop9899)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#------------- Data 96-99 -----------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#Create data frame

dataW <- data.frame(PID = c(1:nrow(MZ2010)) )

# Add HID

HID <- unique(data.frame(State = MZ2010$EF1, AWB = MZ2010$EF3s,HHNr = MZ2010$EF4s))
dataW$HID <- sapply(c(1:nrow(MZ2010)), function(x) which(HID$State == MZ2010$EF1[x] & HID$AWB == MZ2010$EF3s[x] & HID$HHNr == MZ2010$EF4s[x]))
rm(HID)

# Age
dataW$age <- as.numeric(MZ2010$EF44) 

# Sex
# -> 1 = male
# -> 2 = female
dataW$sex <- as.numeric(MZ2010$EF46)

# Employment status
# -> 1 = employed
# -> 2 = not employed ("Erwerbslos" + "Sonstig Erwerbslos" + "Nichterwerbsperson")
dataW$empl <- ifelse(as.numeric(MZ2010$EF29) == 1, 1, 2)

# Education level
# 1: no degree
# 2: secondary school (Volks-/Hauptschulabschluss)
# 3: Intermediate secondary school (Abschluss POS + Realschul-/gleichwertiger Abschluss)
# 4: high school (Fachhochschulreife + Allgemeine/fachgebundene Hochschulreife)
dataW$edu <- ifelse(as.numeric(MZ2010$EF310) %in% c(1, 2, 6), 2,
                    ifelse(as.numeric(MZ2010$EF310) %in% c(3, 7), 3,
                           ifelse(as.numeric(MZ2010$EF310) %in% c(4, 5), 4,
                                  ifelse(is.na(MZ2010$EF310) == TRUE, 1, NA))))

# Nationality
# 1: German
# 2: Foreigner
dataW$nat <- as.numeric(MZ2010$EF756)

# #Income
# income_median <- function (x) {
#   x <- as.numeric(x)
#   case_when(
#     #x == 90 ~ 0,
#     x == 1 ~ 75,
#     x == 2 ~ 225,
#     x == 3 ~ 400,
#     x == 4 ~ 600,
#     x == 5 ~ 800,
#     x == 6 ~ 1000,
#     x == 7 ~ 1200,
#     x == 8 ~ 1400,
#     x == 9 ~ 1600,
#     x == 10 ~ 1850,
#     x == 11 ~ 2150,
#     x == 12 ~ 2450,
#     x == 13 ~ 2750,
#     x == 14 ~ 3050,
#     x == 15 ~ 3400,
#     x == 16 ~ 3800,
#     x == 17 ~ 4250,
#     x == 18 ~ 4750,
#     x == 19 ~ 5250,
#     x == 20 ~ 5750,
#     x == 21 ~ 6750,
#     x == 22 ~ 8750,
#     x == 23 ~ 14000,
#     x == 24 ~ 18000
#   )
# }
# 
# dataW$income <- sapply(MZ2010$EF436, income_median)


# Number of persons in household
dataW$hhmembers <- MZ2010$EF20 

# Relationship to the household head
# 1: household head, 
# 2: partner of household head,  
# 0: neither household head nor partner
dataW$relhead <-  ifelse(as.numeric(MZ2010$EF710) == 1, 1,
                         ifelse(as.numeric(MZ2010$EF710) == 2, 2, 0))

# Partner
# Determine the partner of the person
# It will be consider a person has a partner when is the household head with a partner or the same household's partner

identify_partner <- function (x){
  result <- -1                                                                        #-1 indicates the person has not partner
  if(is.na(dataW$relhead[x])) { result <- NA }                                        #-2 indicates missing value
  
  else
  {                                                                                       
    if(dataW$relhead[x] == 2) {                                                       
      result = dataW$PID[which(dataW$HID == dataW$HID[x] & dataW$relhead == 1)]      
    }
    if(dataW$relhead[x] == 1) {
      result = dataW$PID[which(dataW$HID == dataW$HID[x] & dataW$relhead == 2)]
    }
    if(length(result) == 0L){result <- -1}
  }
  result
}

dataW$IDpartner <- sapply(c(1:nrow(dataW)), identify_partner)                           #Apply the function to every person

#--- has a partner or not

dataW$partnership <- sapply(c(1:nrow(dataW)), 
                            function(x) ifelse(dataW$IDpartner[x] > 0,1,0))
#------Exclude missing values

MissHID_edu <- unique(dataW$HID[which(is.na(dataW$edu))])
MissHID_empl <- unique(dataW$HID[which(is.na(dataW$empl))])
MissHID_relhead <- unique(dataW$HID[which(is.na(dataW$relhead))])

MissHID <- unique(c(MissHID_edu, MissHID_empl, MissHID_relhead))

basePop <- dataW[which(!(dataW$HID %in% MissHID)),]


#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#---------------------------------- MODULES IMPLEMENTATION -------------------------------------
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#------------ Ageing module ---------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#---------------------------------------------
#---- Implementation of the birth module -----
#---------------------------------------------

Module_ageing <- function(Data){
  Data$age <- Data$age + 1
  return(Data)
}

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#------------ Birth module ---------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

Data_birth <- tBasePop9699[tBasePop9699$age.y %in% c(15:49) & tBasePop9699$sex.y == 2 & tBasePop9699$relhead.x > 0, ]

mod_birth <- stepAIC(
                  glm(birth.x ~ 
                   age.y + 
                   I(age.y^2) + 
                   #as.factor(edu.x) + 
                   as.factor(empl.x) +
                   partnership.x,
                   #as.factor(nat.x),
                   data = Data_birth, 
                 family = binomial(link = "logit"))
                )

summary(mod_birth)
    #significant: age, age^2, empl, partnership

# Probability function
ProbFun_birth <- function(Data, Mod){
  
  prob <- predict(Mod, 
                  newdata = data.frame(age.y = Data$age,
                                       edu.x = Data$edu,
                                       empl.x = Data$empl,
                                       partnership.x = Data$partnership)[Data$age %in% c(15:49) & Data$sex == 2, ],
                  type = "response")
  
  prob_birth <- numeric(nrow(Data))
  
  prob_birth[Data$age %in% c(15:49) & Data$sex == 2] <- prob
  
  return(prob_birth)
  
}

#---------------------------------------------
#--- Implementation of the birth module ------
#---------------------------------------------

Module_birth <- function(Data, ProbFun, Mod){
  
  WS_birth <- ProbFun(Data, Mod = Mod)
  
  # Sample uniformly distributed random number
  R <- runif(nrow(Data))
  
  birth <- ifelse(R < WS_birth, 1, 0)
  
  if(sum(birth) > 0){
    
    DatGeb <- data.frame(PID = NA, HID = NA, age = 0, sex = NA, empl = 2, edu = 1,  nat = 1,  
                         relhead = 0, IDpartner = -1, partnership = 0, hhmembers = NA)[rep(1, sum(birth)), ]
    
    # add new PIDs
    DatGeb$PID <- c((max(Data$PID) + 1):(max(Data$PID) + sum(birth)))
    
    # add HID of the mother
    DatGeb$HID <- Data$HID[birth == 1]
    
    # add random sex
    DatGeb$sex <- sample(c(1, 2), size = sum(birth), replace = T)
    
    Data <- rbind(Data, DatGeb)  
    
    # update the number of household members
    Data$hhmembers <- sapply(c(1:nrow(Data)), function(x) sum(Data$HID == Data$HID[x]))
    
  }
  return(Data)
}


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#--------- Mortality module ---------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

load("LifeTable.RData")                                              #Load the life table

ProbFun_mort <- function(Data, ProbMat){
  
  Prob_male <- ProbMat$prob_male
  Prob_female <- ProbMat$prob_female
  
  MortProb <- numeric(nrow(Data))
  
  AgeInd <- Data$age
  AgeInd[AgeInd >= 100] <- 100
  AgeInd <- AgeInd + 1
  
  
  MortProb[Data$sex == 1] <- Prob_male[AgeInd[which(Data$sex == 1)]]
  
  MortProb[Data$sex == 2] <- Prob_female[AgeInd[which(Data$sex == 2)]]
  
  return(MortProb)
}

#---------------------------------------------
#-- Implementation of the mortality module ---
#---------------------------------------------
  
Module_mortality <- function(Data, ProbFun, ProbMat){
    
    WS <- ProbFun(Data, ProbMat)
    R <- runif(nrow(Data))
    I_Mort <- ifelse(R <= WS, 1, 0)
    Data2 <- Data[which(I_Mort == 0),] 
    
    #table(tapply(Data2$relhead,Data2$HID,sum))
    
    I_Mort_HH <- Data$HID[I_Mort==1]
    DatMort <- Data2[which(Data2$HID %in% I_Mort_HH),]
    
    # Update the head of household variable 
    DatMort$relhead <- sapply(c(1:nrow(DatMort)), function(x) ifelse(sum(DatMort$relhead[DatMort$HID == DatMort$HID[x]]) %in% c(1,3),DatMort$relhead[x],
                                                                     ifelse(sum(DatMort$relhead[DatMort$HID == DatMort$HID[x]]) == 2 & DatMort$relhead[x] == 2,1,
                                                                            ifelse(sum(DatMort$relhead[DatMort$HID == DatMort$HID[x]]) == 0 & 
                                                                                     DatMort$PID[x] ==  sort(DatMort$PID[DatMort$HID == DatMort$HID[x]],decreasing = T)[1],1,0))))
    
    
    # Update the household size variable
    DatMort$hhmembers <- sapply(c(1:nrow(DatMort)), function(x) sum(DatMort$HID == DatMort$HID[x]))
    
    # Update the partnership variable
    DatMort$partnership <- sapply(c(1:nrow(DatMort)), 
                              function(x) ifelse(sum(DatMort$relhead[DatMort$HID == DatMort$HID[x]]) == 3 & DatMort$relhead[x]  >0,1,0))
    
    
    Data2[which(Data2$HID %in% I_Mort_HH),] <- DatMort
    return(Data2)
}  
    

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#---------- Employment module -------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

mod_empl <- glm(as.factor(ifelse(empl.y == 1, 1, 0)) ~ 
                  age.y + 
                  I(age.y^2) + 
                  as.factor(sex.y) + 
                  as.factor(edu.x) + 
                  as.factor(nat.y) +
                  partnership.y +            #Not significant
                  as.factor(empl.x), 
                data = tBasePop9699[tBasePop9699$age.y >= 15, ], family = binomial(link = "logit"))

mod_empl <- stepAIC(mod_empl)

# Funtion for computing the probability of having employ to the next year

ProbFun_empl <- function(Data, Mod){
  
  EmplProb <- predict(Mod, newdata = data.frame(age.y = Data$age,
                                                sex.y = Data$sex,
                                                edu.x = Data$edu,
                                                nat.y = Data$nat,
                                                partnership.y = Data$partnership,
                                                empl.x = Data$empl), type = "response")
  
  EmplProb[Data$age < 15] <- 0
  return(EmplProb)
}

#---------------------------------------------
#-- Implementation of the employment module --
#---------------------------------------------

Module_employ <- function(Data, ProbFun, Mod){
  
  ProbEmp <- ProbFun(Data,Mod)
  R <- runif(length(ProbEmp))
  Data$empl <- ifelse(R  <= ProbEmp, 1, 2)
  
  return(Data)
  
}

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#----------- Education module -------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

tBasePop9699$edu.y <- ifelse(tBasePop9699$edu.y < tBasePop9699$edu.x,
                             tBasePop9699$edu.x, tBasePop9699$edu.y)

#--------- Models

  # Model for persons with edu == 1
mod_edu_1 <- multinom(as.factor(edu.y) ~ 
                      age.x +
                      I(age.x^2) +
                      as.factor(sex.x) +
                      as.factor(nat.x) +
                      as.factor(partnership.x) +
                      as.factor(empl.x), 
                      data = tBasePop9699[tBasePop9699$edu.x == 1, ],family=binomial(link="logit"))

  # Model for persons with edu == 2
mod_edu_2 <- multinom(as.factor(edu.y) ~ 
                        age.x +
                        I(age.x^2) +
                        as.factor(sex.x) +
                        as.factor(nat.x) +
                        as.factor(partnership.x) +
                        as.factor(empl.x), 
                      data = tBasePop9699[tBasePop9699$edu.x == 2, ],family=binomial(link="logit"))

  # Model for persons with edu == 3
mod_edu_3 <- multinom(as.factor(edu.y) ~ 
                        age.x +
                        I(age.x^2) +
                        as.factor(sex.x) +
                        as.factor(nat.x) +
                        as.factor(partnership.x) +
                        as.factor(empl.x), 
                      data = tBasePop9699[tBasePop9699$edu.x == 3, ],family=binomial(link="logit"))

#--------- Function to compute the probability

ProbFun_edu <- function(Data, Mod1, Mod2, Mod3){
  
  ProbMat <- matrix(rep(0, 4*nrow(Data)), ncol = 4)
  # Persons with edu == 1
  Dat_edu1  <- Data[Data$edu == 1, ]
  
  if(nrow(Dat_edu1) > 0){
    PredMat_edu1 <- predict(mod_edu_1, 
                            newdata = data.frame(age.x = Dat_edu1$age,
                                                 sex.x = Dat_edu1$sex,
                                                 edu.x = Dat_edu1$edu,
                                                 nat.x = Dat_edu1$nat,
                                                 partnership.x = Dat_edu1$partnership,
                                                 empl.x = Dat_edu1$empl),
                            type = "probs"
    )
    ProbMat[Data$edu == 1,] <- PredMat_edu1
    
  }
  
  # Persons with edu == 2
  Dat_edu2  <- Data[Data$edu == 2, ]
  if(nrow(Dat_edu2) > 0){
    PredMat_edu2 <- predict(mod_edu_2, 
                            newdata = data.frame(age.x = Dat_edu2$age,
                                                 sex.x = Dat_edu2$sex,
                                                 edu.x = Dat_edu2$edu,
                                                 nat.x = Dat_edu2$nat,
                                                 partnership.x = Dat_edu2$partnership,
                                                 empl.x = Dat_edu2$empl),
                            type = "probs"
    )
    ProbMat[Data$edu == 2, c(2:4)] <- PredMat_edu2
  }
  
  # Persons with edu == 3
  Dat_edu3  <- Data[Data$edu == 3, ]
  
  if(nrow(Dat_edu3 > 0)){
    PredMat_edu3 <- predict(mod_edu_3, 
                            newdata = data.frame(age.x = Dat_edu3$age,
                                                 sex.x = Dat_edu3$sex,
                                                 edu.x = Dat_edu3$edu,
                                                 nat.x = Dat_edu3$nat,
                                                 partnership.x = Dat_edu3$partnership,
                                                 empl.x = Dat_edu3$empl),
                            type="probs"
    )
    ProbMat[Data$edu == 3, 3] <- 1 - PredMat_edu3
  }
  
  ProbMat[Data$edu == 4, 4] <- 1
  ProbMat[Data$age <= 14, 1] <- 1 
  ProbMat[Data$age <= 14, c(2:4)] <- 0
  
  return(ProbMat)
  
}

#---------------------------------------------
#--- Implementation of the education module --
#---------------------------------------------

Module_education <- function(Data, ProbFun, Mod1, Mod2, Mod3){
  
  WS_edu <- ProbFun(Data)
  ProbMatCum <- t(apply(WS_edu, 1, cumsum))
  R<- runif(nrow(Data))
  
  Data$edu <- ifelse(R <= ProbMatCum[, 1], 1,
                     ifelse(R > ProbMatCum[, 1] & R <= ProbMatCum[, 2], 2,
                            ifelse(R > ProbMatCum[, 2] & R <= ProbMatCum[, 3], 3, 4)))
  return(Data)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#------------------ Marriage module ------------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#-------------Training the model------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

names_col <- function (x) { 
  setNames(x, colnames <- c("PID",  "HID", "age.x", "age.y", "sex", "empl.x", "empl.y", "edu.x", "edu.y", "nat.x", "nat.y",
                            "hhmembers.x", "hhmembers.y", "relhead.x", "relhead.y", "IDpartner.x", "IDpartner.y")
  )
}

exclude_cols_m <- function(x,type = 1) {
  names_col( subset(tBasePop[which(tBasePop[,paste0("IDpartner_",x)] == -1 
                                   &  tBasePop[,paste0("IDpartner_",x + 1)] > -2),],
                    select =  c(PID, HID,
                                get(paste0("age_",x)), get(paste0("age_",x +1)), sex,
                                get(paste0("empl_",x)), get(paste0("empl_",x +1)), 
                                get(paste0("edu_",x)), get(paste0("edu_",x +1)),
                                get(paste0("nat_",x)), get(paste0("nat_",x +1)), 
                                get(paste0("hhmembers_",x)), get(paste0("hhmembers_",x +1)),      
                                get(paste0("relhead_",x)), get(paste0("relhead_",x +1)),
                                get(paste0("IDpartner_",x)), get(paste0("IDpartner_",x +1)) 
                                )  
                    )
             )
}

marry_data9697 <- exclude_cols_m(96)
  marry_data9697$year <- rep(96,nrow(marry_data9697))
  
marry_data9798 <- exclude_cols_m(97)
  marry_data9798$year <- rep(97,nrow(marry_data9798))
  
marry_data9899 <- exclude_cols_m(98)
  marry_data9899$year <- rep(98,nrow(marry_data9899))


marry_data <- rbind(marry_data9697, marry_data9798, marry_data9899)                                                       #Combine 3 tables. These are the single people in the previous year.
marry_data <- marry_data[which(marry_data$age.x > 17),]
  
  marry_data$married <- sapply(c(1:nrow(marry_data)),                                                                     #Determine if the person in the next year became married (1)
                             function(x) ifelse(marry_data$IDpartner.y[x] > -1, 1, 0))                                      #Or continue single (0)

#------------
## Prob. getting into the market
#-----

mod_market_m <- glm(married ~ 
                      age.x + 
                      I(age.x^2) + 
                      as.factor(edu.x) +
                      as.factor(nat.x) +
                      as.factor(empl.x) +   
                      hhmembers.x,
                 data = marry_data[which(marry_data$sex == 1),], 
                 family = binomial(link = "logit"))

mod_market_m <- stepAIC(mod_market_m)
summary(mod_market_m)

#Probability for women

mod_market_f <- glm(married ~ 
                      age.x + 
                      I(age.x^2) + 
                      as.factor(edu.x) +
                      as.factor(nat.x) +       #Not sig
                      as.factor(empl.x) +      #Not sig
                      hhmembers.x,          
                  data = marry_data[which(marry_data$sex == 2),], 
                  family = binomial(link = "logit"))

mod_market_f <- stepAIC(mod_market_f)
summary(mod_market_f)


#------------
## Compatibility
#---------------

#----Actual couples

columns_compat <- c("CID", "year", "PID1", "PID2", "diff_age", "diff_abs_age", "diff_p_age", 
                    "diff_empl", "diff_abs_empl",  "diff_edu", "diff_edu2", "diff_nat")
actual_couples <- data.frame(matrix(ncol = length(columns_compat), nrow = 0))

for(i in 1:nrow(marry_data[which(marry_data$married == 1),])){
 
  par1 <- marry_data[which(marry_data$married == 1),][i,]                         #get the row of partner 1
  par2 <- tBasePop[which( tBasePop$PID == par1$IDpartner.y ),]                    #get the row of partner 2
  
  CID <- (par1$PID + par2$PID) + (par1$PID * par2$PID)                            #create an ID per couple
  if(CID %in% actual_couples[,1]) { next }                                           #avoid to repeat the same couple using the ID and skeeping the following steps of the loop  
  if(par1$sex == par2$sex) { next }                                               #exlude cases of couples of the same sex
  #*Assign values to the variables
  year <- par1$year
  PID1 <- par1$PID
  PID2 <- par2$PID
  diff_age <- if(par1$sex == 1) { par1$age.y - par2[,paste0("age_",par1$year + 1)] }       #The male person will be the minuend of the rest
                else{ par2[,paste0("age_",par1$year + 1)] - par1$age.y }
  
  diff_abs_age <- abs( par1$age.y - par2[,paste0("age_",par1$year + 1)] )
  
  diff_p_age <- if(par1$sex == 1) { ( par1$age.y - par2[,paste0("age_",par1$year + 1)] ) /  #The male person will be the minuend of the rest
                       min(par1$age.y, par2[,paste0("age_",par1$year + 1)]) }
                  else{ ( par2[,paste0("age_",par1$year + 1)] - par1$age.y ) / 
                      min(par1$age.y, par2[,paste0("age_",par1$year + 1)]) }
  
  diff_empl <- if(par1$sex == 1) { par1$empl.y - par2[,paste0("empl_",par1$year + 1)] }       #The male person will be the minuend of the rest
                  else{ par2[,paste0("empl_",par1$year + 1)] - par1$empl.y }
  
  diff_abs_empl <- ifelse(par1$empl.y != par2[,paste0("empl_",par1$year + 1)], 1, 0)
  diff_edu <- abs(par1$edu.y - par2[,paste0("edu_",par1$year + 1)])
  diff_edu2 <- ifelse(par1$edu.y != par2[,paste0("edu_",par1$year + 1)], 1, 0) 
  diff_nat <- ifelse(par1$nat.y != par2[,paste0("nat_",par1$year + 1)], 1, 0)
  #*  
  
  
  actual_couples <- rbind(actual_couples,c(CID, year, PID1,PID2,diff_age ,diff_abs_age, diff_p_age, 
                                  diff_empl, diff_abs_empl, diff_edu, diff_edu2,diff_nat))
                                           
}
actual_couples <- setNames(actual_couples, columns_compat)                                      #Set names of columns
actual_couples$couple <- rep(1,nrow(actual_couples))                                            #All these rows are actual couples

#----potential couples

potential_couples <- data.frame(matrix(ncol = length(columns_compat), nrow = 0))

for(i in 1:nrow(marry_data[which(marry_data$married == 1 & marry_data$sex == 1),])){
  for(j in 1:nrow(marry_data[which(marry_data$married == 1 & marry_data$sex == 2),])){
    
    par1 <- marry_data[which(marry_data$married == 1 & marry_data$sex == 1),][i,]            #get the row of the man
    par2 <- marry_data[which(marry_data$married == 1 & marry_data$sex == 2),][j,]            #get the row of the woman
    
    CID <- (par1$PID + par2$PID) + (par1$PID * par2$PID)                                   #create an ID per couple
    if(CID %in% potential_couples[,1]) { next }                                            #avoid to repeat the same couple using the ID and skeeping the following steps of the loop  
    if(CID %in% actual_couples[,1]) { next }                                               #exlude cases from the actual couples
                                                                                           
    #*Assign values to the variables
    year <- par1$year
    PID1 <- par1$PID
    PID2 <- par2$PID
    diff_age <- par1$age.y - par2$age.y                                                   #The male person will be the minuend of the rest
    diff_abs_age <- abs(par1$age.y - par2$age.y) 
    diff_p_age <- (par1$age.y - par2$age.y) / min (par1$age.y, par2$age.y)                #The male person will be the minuend of the rest
    diff_empl <- par1$empl.y - par2$empl.y                                                #The male person will be the minuend of the rest
    diff_abs_empl <- ifelse(par1$empl.y != par2$empl.y, 1, 0)
    diff_edu <- abs(par1$edu.y - par2$edu.y)
    diff_edu2 <- ifelse(par1$edu.y != par2$edu.y, 1, 0) 
    diff_nat <- ifelse(par1$edu.y != par2$edu.y, 1, 0)
    #*  
    
    potential_couples <- rbind(potential_couples,c(CID, year, PID1,PID2,diff_age, diff_abs_age,diff_p_age, 
                                              diff_empl, diff_abs_empl, diff_edu, diff_edu2,diff_nat))
  }
}
potential_couples <- setNames(potential_couples, columns_compat)                           #Set names
potential_couples$couple <- rep(0,nrow(potential_couples))                                 #All these rows are NOT actual couples

#----Compatibility

compatibility_data <- rbind(actual_couples, potential_couples)

mod_comp <- glm(couple ~ 
                 #diff_age +
                 #diff_age^2 +
                 diff_abs_age +
                 diff_abs_age^2 +
                 #diff_p_age +
                 #diff_p_age^2 + 
                 diff_empl +
                 as.factor(diff_abs_empl) +
                 #diff_edu +
                 diff_edu2 +
                 diff_nat,
               data = compatibility_data, 
               family = binomial(link = "logit"))

mod_comp <- stepAIC(mod_comp)

summary(mod_comp)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#-------- Marriage Implementation ---------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

###-----Probability to enter to the market
#----------------------------------------

prob_market <- function(Data){
  
  prob <- rep(0,nrow(Data))
  #Persons sex = male
    Data_m <- Data[Data$sex == 1,]
    if(nrow(Data_m) > 0){
    pred_m <-
    predict(mod_market_m,
            newdata = data.frame(
                                 age.x = Data_m$age,
                                 edu.x = Data_m$edu,
                                 nat.x = Data_m$nat,       
                                 empl.x = Data_m$empl,
                                 hhmembers.x = Data_m$hhmembers),     
            type = "response"
            ) 
    prob[Data$sex == 1] <- pred_m
    }
    
  #Persons sex = female  
    
    Data_f <- Data[Data$sex == 2,]
    if(nrow(Data_f) > 0){
      
    pred_f <-
      predict(mod_market_f,
              newdata = data.frame(
                                  age.x = Data_f$age,
                                  edu.x = Data_f$edu,
                                  nat.x = Data_f$nat,       
                                  empl.x = Data_f$empl,
                                  hhmembers.x = Data_f$hhmembers),     
                                  type = "response"
      )
    prob[Data$sex == 2] <- pred_f
    }
    ## Only single persons can enter to the market and equal or more than 18 years
    prob[Data$partnership == 1] <- 0
    prob[Data$age < 18] <- 0
    
    return(prob)
} 

###-----Probability of a couple get together
#-----------------------------------------

ProbFun_coupl <- function(Data, Mod){
  
  coupl_Prob <- predict(mod_comp, newdata = data.frame(
                                            diff_age = Data$diff_age,
                                            diff_abs_age = Data$diff_abs_age,
                                            diff_p_age = Data$diff_p_age,
                                            diff_empl = Data$diff_empl,
                                            diff_abs_empl = Data$diff_abs_empl,
                                            diff_edu = Data$diff_edu,
                                            diff_edu2 = Data$diff_edu2,
                                            diff_nat = Data$diff_nat),
                        type = "response"
                       )
  
  #If the two persons are in the same household, the probability becomes 0, to avoid incest
  coupl_Prob[Data$family == 1] <- 0                
  
  return(coupl_Prob)
  
}


###----Function for creating potential couples
#---------------------------------------------

pot_coupl <- function(Data){
  Data_coupl <- data.frame()

  for(i in 1:nrow(Data[which(Data$sex == 1),])){
    for(j in 1:nrow(Data[which(Data$sex == 2),])){
      
      par1 <- Data[which(Data$sex == 1),][i,]                                            #get the row of the man
      par2 <- Data[which(Data$sex == 2),][j,]                                            #get the row of the woman
      
      CID <- (par1$PID + par2$PID) + (par1$PID * par2$PID)                                   #create an ID per couple
      
      #*Assign values to the variables
      PID1 <- par1$PID
      PID2 <- par2$PID
      diff_age <- par1$age - par2$age                                                   #The male person will be the minuend of the rest
      diff_abs_age <- abs(par1$age - par2$age) 
      diff_p_age <- (par1$age - par2$age) / min (par1$age, par2$age)                    #The male person will be the minuend of the rest
      diff_empl <- par1$empl - par2$empl                                                #The male person will be the minuend of the rest
      diff_abs_empl <- ifelse(par1$empl != par2$empl, 1, 0)
      diff_edu <- abs(par1$edu - par2$edu)
      diff_edu2 <- ifelse(par1$edu != par2$edu, 1, 0) 
      diff_nat <- ifelse(par1$nat != par2$nat, 1, 0)
      #*  
      family <- ifelse(par1$HID == par2$HID, 1, 0)
      
      Data_coupl <- rbind( Data_coupl,c(CID, PID1,PID2,diff_age, diff_abs_age,diff_p_age, 
                                        diff_empl, diff_abs_empl, diff_edu, diff_edu2,diff_nat, family) )
    }
  }
  Data_coupl <- setNames(Data_coupl, colnames <- c("CID", "PID1","PID2","diff_age", "diff_abs_age","diff_p_age", 
                                                   "diff_empl", "diff_abs_empl", "diff_edu", "diff_edu2","diff_nat", "family") )
  return(Data_coupl)
}


#------------------------------------------
#---------------- Marriage Module ---------
#------------------------------------------

Module_marriage <- function(Data, prob_market, prob_couple){
  
  prob_mar <- prob_market(basePop)
  #Create the market
  R <- runif(length(prob_mar))
  ent_mark <- ifelse( R < prob_mar, 1, 0)
  market <- basePop[ent_mark == 1,]
  
  #Make all the potential couples
  potential_data <- pot_coupl(market)
  
  #Compute the probability of matching
  prob_match <- prob_couple(potential_data) 
  
  #-Forming the couples
  #---------------------------------------
  match_pool <- potential_data                                                           #Create the table of possible matches pool
  matched_couples <- potential_data[0,c(1,2,3)]                                            #Initialize the table of matches
  
  match_pool <- cbind(match_pool, prob_match)
   #Start the algorithm for creating the match
  cs <- sum(match_pool$prob_match)
    while( cs > 0 ){
        # 1) create the ranges of probabilities
      match_pool$lim.sup <- cumsum(match_pool$prob_match)
      match_pool$lim.inf <- match_pool$lim.sup - match_pool$prob_match
        # 2) Draw a random number
      R <- runif(1, min=0, max = cs)           
        # 3) Select the couple that corresponds to the couple-interval that r falls into
      match <- match_pool[which( R <= match_pool$lim.sup & R >= match_pool$lim.inf ),c(1,2,3)]
      matched_couples <- rbind(matched_couples, match)
        # 4) Remove the people from the pool
      match_pool <- match_pool[which( match_pool$PID1 != match$PID1 & match_pool$PID1 != match$PID2 &
                                        match_pool$PID2 != match$PID1 & match_pool$PID2 != match$PID2),]
        #5) Recompute cs: the algorithm ends when cs = 0
      cs <- sum(match_pool$prob_match)
    }
  #----------------------------------------
  
  #Change the partnership status
  Data$partnership[Data$PID %in% c(matched_couples$PID1, matched_couples$PID2)] <- 1 
  
  #Create new HIDs
  new_HIDs <- c( (max(Data$HID) + 1):(max(Data$HID) + nrow(matched_couples)) )
  
  #Add the current partner's ID
  for(i in 1:nrow(matched_couples)){
    
    Data$IDpartner[Data$PID == matched_couples$PID1[i]] <- matched_couples$PID2[i]
    Data$IDpartner[Data$PID == matched_couples$PID2[i]] <- matched_couples$PID1[i]
    
  #Modify HID
    Data$HID[Data$PID == matched_couples$PID1[i]] <- new_HIDs[i]
    Data$HID[Data$PID == matched_couples$PID2[i]] <- new_HIDs[i]
  
  #Update current relationship to the household head
    Data$relhead[Data$PID == matched_couples$PID1[i]] <- 1
    Data$relhead[Data$PID == matched_couples$PID2[i]] <- 2
    
  #Update members of the household 
    Data$hhmembers[Data$PID == matched_couples$PID1[i]] = 2
    Data$hhmembers[Data$PID == matched_couples$PID2[i]] = 2
  }
  
  return(Data)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#------------------ DIVORCE MODULE -------------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#-----------Training the model ------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


exclude_cols_d <- function(x,type = 1) {
  names_col( subset(tBasePop[which(tBasePop[,paste0("IDpartner_",x)] > -1 
                                   &  tBasePop[,paste0("IDpartner_",x + 1)] > -2),],
                    select =  c(PID, HID,
                                get(paste0("age_",x)), get(paste0("age_",x +1)), sex,
                                get(paste0("empl_",x)), get(paste0("empl_",x +1)), 
                                get(paste0("edu_",x)), get(paste0("edu_",x +1)),
                                get(paste0("nat_",x)), get(paste0("nat_",x +1)), 
                                get(paste0("hhmembers_",x)), get(paste0("hhmembers_",x +1)),      
                                get(paste0("relhead_",x)), get(paste0("relhead_",x +1)),
                                get(paste0("IDpartner_",x)), get(paste0("IDpartner_",x +1)) 
                    )  
  )
  )
}

divor_data <- rbind( exclude_cols_d(96),exclude_cols_d(97),exclude_cols_d(98) )

divor_data$divorce <- sapply(c(1:nrow(divor_data)),                                                     #Create variable of divorce 
                  function(x) ifelse(divor_data$IDpartner.x[x] != divor_data$IDpartner.y[x], 1, 0))        # 1: yes, 0: no 


#----------
## logit model of divorces

mod_div <- glm(divorce ~ 
                 age.x + 
                 I(age.x^2) + 
                 as.factor(edu.x) +    #not signicant
                 as.factor(nat.x) +    #not significant
                 as.factor(empl.x) +   #not significant
                 hhmembers.x,
             data = divor_data, 
             family = binomial(link = "logit"))

mod_div <- stepAIC(mod_div)

summary(mod_div)

# Probability function

ProbFun_divor <- function(Data, Mod){
  
  prob <- predict(Mod, 
                  newdata = data.frame(age.x = Data$age, 
                                       edu.x = Data$edu, 
                                       nat.x = Data$nat,
                                       empl.x = Data$empl,
                                       hhmembers.x = Data$hhmembers),
                  type = "response")
  
  prob[Data$partnership != 1] <- 0
  
  return(prob)
  
}

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#------------ Divorce Module --------------
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

Module_divorce <- function(Data, ProbFun, Mod){ 
  
  Probdivorce <- ProbFun(Data, Mod) 
  
  #Determine the people who will divorce
  R <- runif(length(Probdivorce))
  divorced <- ifelse( R <= Probdivorce, 1, 0)
  IDdivorced <- c(Data$PID[divorced == 1], Data$IDpartner[divorced == 1])
    IDdivorced2 <- Data$PID %in% IDdivorced                                     #It is a boolean variable, true if it is divorced, 0 if not
  
  #Updating the partnership status and the partner's ID
  Data$IDpartner[IDdivorced2] <- -1
  Data$partnership[IDdivorced2] <- 0
  
  # add new HIDs to the partner who was not the head
  new_HIDs <- c((max(Data$HID) + 1):(max(Data$HID) + length(Data$HID[IDdivorced2 & Data$relhead == 2 ]) ))
  Data$HID[IDdivorced2 & Data$relhead == 2 ] <- new_HIDs
  
  #Update the relation of the household head. The previous partner becomes head
  Data$relhead[IDdivorced2 & Data$relhead == 2 ] <- 1
  
  # Update members of the household
  Data$hhmembers[IDdivorced2 & Data$relhead == 2] <- 1
  Data$hhmembers[IDdivorced2 & Data$relhead == 2] <- Data$hhmembers[IDdivorced2 & Data$relhead == 2] - 1
  
  return(Data)
}

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#                                    SIMULATION
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

SimPop <- basePop

set.seed(892674)
for(i in 1:20){
  
  SimPop <- Module_ageing(SimPop)  
  
  SimPop <- Module_birth(SimPop,ProbFun = ProbFun_birth, Mod = mod_birth)
  
  SimPop <- Module_mortality(SimPop,ProbFun = ProbFun_mort, ProbMat = LifeTable)
  
  SimPop <- Module_employ(SimPop,ProbFun = ProbFun_empl, Mod = mod_empl)
  
  SimPop <- Module_education(SimPop,ProbFun = ProbFun_edu,Mod1 = mod_edu_1, Mod2 = mod_edu_2, Mod3 = mod_edu_3)
  
  SimPop <- Module_marriage(SimPop, prob_market = prob_market, prob_couple = ProbFun_coupl)
  
  SimPop <- Module_divorce(SimPop,ProbFun = ProbFun_divor,Mod = mod_div)
  
  print(i)
  
}

nrow(SimPop)
nrow(basePop)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#                                    RESULTS
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#---------------------------------------------
###------ Probabilities distribution
#---------------------------------------------
#- Mean probability of entering to the market

ProbMark <- prob_market(Data = basePop[basePop$sex == 1,])
barplot(tapply(ProbMark, basePop$age[basePop$sex == 1], mean), xlab = "age", ylab = "probability",
            col = "blue", main = "Mean probability of entering \n to the match (marriage) market \n males")

ProbMark <- prob_market(Data = basePop[basePop$sex == 2,])
barplot(tapply(ProbMark, basePop$age[basePop$sex == 2], mean), xlab = "age", ylab = "probability",
        col = "red", main = "Mean probability of entering \n to the match (marriage) market \n females")

#- Mean probability of getting divorce

ProbDivor <- ProbFun_divor(Mod = mod_div, Data = basePop)
barplot(tapply(ProbDivor, basePop$age, mean), xlab = "age", ylab = "probability",
        col = "purple", main = "Mean probability of getting divorce by age")


#---------------------------------------------
###------ Estimation of population in 10 years
#---------------------------------------------

#--Draw a sample of random simulations to estimate population in 10 years


#Without marriage and divorce

population_w1 <- c()
SimPop <- basePop

for(i in 1:15){
  set.seed(892674 + round(i%%72)*(i^2) )
  for(j in 1:10){
    
    SimPop <- Module_ageing(SimPop)  
    
    SimPop <- Module_birth(SimPop,ProbFun = ProbFun_birth, Mod = mod_birth)
    
    SimPop <- Module_mortality(SimPop,ProbFun = ProbFun_mort, ProbMat = LifeTable)
    
    SimPop <- Module_employ(SimPop,ProbFun = ProbFun_empl, Mod = mod_empl)
    
    SimPop <- Module_education(SimPop,ProbFun = ProbFun_edu,Mod1 = mod_edu_1, Mod2 = mod_edu_2, Mod3 = mod_edu_3)
    
    print(paste(i,j))
    
  }
  population_w1 <- c(population_w1, nrow(SimPop) )
  SimPop <- basePop
}

# With marriage, but not divorce

population_w2 <- c()
SimPop <- basePop

for(i in 1:15){
  set.seed(892674 + round(i%%72)*(i^2) )
  for(j in 1:10){
    
    SimPop <- Module_ageing(SimPop)  
    
    SimPop <- Module_birth(SimPop,ProbFun = ProbFun_birth, Mod = mod_birth)
    
    SimPop <- Module_mortality(SimPop,ProbFun = ProbFun_mort, ProbMat = LifeTable)
    
    SimPop <- Module_employ(SimPop,ProbFun = ProbFun_empl, Mod = mod_empl)
    
    SimPop <- Module_education(SimPop,ProbFun = ProbFun_edu,Mod1 = mod_edu_1, Mod2 = mod_edu_2, Mod3 = mod_edu_3)
    
    SimPop <- Module_marriage(SimPop, prob_market = prob_market, prob_couple = ProbFun_coupl)
    
    print(paste(i,j))
    
  }
  population_w2 <- c(population_w2, nrow(SimPop) )
  SimPop <- basePop
}


# With marriage and divorce

population_w3 <- c()
SimPop <- basePop

for(i in 1:15){
  set.seed(892674 + round(i%%72)*(i^2) )
  for(j in 1:10){
    
    SimPop <- Module_ageing(SimPop)  
    
    SimPop <- Module_birth(SimPop,ProbFun = ProbFun_birth, Mod = mod_birth)
    
    SimPop <- Module_mortality(SimPop,ProbFun = ProbFun_mort, ProbMat = LifeTable)
    
    SimPop <- Module_employ(SimPop,ProbFun = ProbFun_empl, Mod = mod_empl)
    
    SimPop <- Module_education(SimPop,ProbFun = ProbFun_edu,Mod1 = mod_edu_1, Mod2 = mod_edu_2, Mod3 = mod_edu_3)
    
    SimPop <- Module_marriage(SimPop, prob_market = prob_market, prob_couple = ProbFun_coupl)
    
    SimPop <- Module_divorce(SimPop,ProbFun = ProbFun_divor,Mod = mod_div)
    
    print(paste(i,j))
    
  }
  population_w3 <- c(population_w3, nrow(SimPop) )
  SimPop <- basePop
}


#Values after the simulation (This is for no having to run the model again)

population_w1 <- c(20786, 20837, 20800, 20787, 20814, 20782, 20879, 20808,
                     20831, 20846, 20864, 20854, 20829, 20765, 20892)

population_w2 <- c(20789, 20809, 20940, 20920, 20853, 20899, 20955, 20881, 
                      20919, 20840, 20808, 20890, 20926, 20864, 20907)

population_w3 <- c(20786, 20837, 20800, 20787, 20814, 20782, 20879, 20808,
                      20831, 20846, 20864, 20854, 20829, 20765, 20892)


#According to the Statistisches Bundesamt the population of germany was 81.5 million of people
#The office estimates the population in Germany of 83.4 million of people
#Now it is estimated the population growth rate
actual_rate <- (83.4 - 81.5) / 81.5
actual_estimation <- nrow(basePop) * (1 + actual_rate )

#Boxplots
data_box <- cbind(population_w1, population_w2, population_w3)

{ boxplot(data_box , main ="Boxplot - Estimation of the population in 10 years",
          names=c("womd","wm","Wmd"), col = brewer.pal(n = 3, name = "RdBu")) 
  text(1, mean(population_w1) + 15, paste("Mean =", round(mean(population_w1),2)), col = "black", cex = 0.7, font = 2)
  text(2, mean(population_w2) + 20, paste("Mean =", round(mean(population_w2),2)), col = "black", cex = 0.7, font = 2)
  text(3, mean(population_w3) + 10, paste("Mean =", round(mean(population_w3),2)), col = "black", cex = 0.7, font = 2) 
  mtext("womd = without marriage and divorce", side = 1, line = 2, cex = 0.6, adj = 0)
  mtext("wm = with marriage, but no divorce", side = 1, line = 3, cex = 0.6, adj = 0)
  mtext("wmd = with marriage and divorce", side = 1, line = 4, cex = 0.6, adj = 0) }



#---------------------------------------------
###------ Status of partnership --------------
#---------------------------------------------

# With marriage and divorce
SimPop <- basePop

partner_stat_w3 <- table(SimPop$partnership)

set.seed(892674)
for(i in 1:20){
  
  SimPop <- Module_ageing(SimPop)  
  
  SimPop <- Module_birth(SimPop,ProbFun = ProbFun_birth, Mod = mod_birth)
  
  SimPop <- Module_mortality(SimPop,ProbFun = ProbFun_mort, ProbMat = LifeTable)
  
  SimPop <- Module_employ(SimPop,ProbFun = ProbFun_empl, Mod = mod_empl)
  
  SimPop <- Module_education(SimPop,ProbFun = ProbFun_edu,Mod1 = mod_edu_1, Mod2 = mod_edu_2, Mod3 = mod_edu_3)
  
  SimPop <- Module_marriage(SimPop, prob_market = prob_market, prob_couple = ProbFun_coupl)
  
  SimPop <- Module_divorce(SimPop,ProbFun = ProbFun_divor,Mod = mod_div)
  
  partner_stat_w3 <- rbind(partner_stat_w3, table(SimPop$partnership))
  
  print(i)
  
}

rownames(partner_stat_w3 ) <- c(2010,	2011,	2012,	2013,	2014,	2015,	2016,	2017,	2018,	2019,	2020,
                  2021,	2022,	2023,	2024,	2025,	2026,	2027,	2028,	2029,	2030)

colnames(partner_stat_w3) <- c("Single", "With partner")

total = as.numeric(partner_stat_w3[,1]) + as.numeric(partner_stat_w3[,2])
partner_stat_w3 <- cbind(partner_stat_w3,total)


#graph
plot(c(1:21), partner_stat_w3[,1]/1000, type = "o", pch= 18, ylim = c(0,24), col = "blue",
      xlab = "Year", ylab = "Number of People (thousands)", xaxt='n', yaxt='n'  ) 
lines(c(1:21), partner_stat_w3[,2]/1000, type = "o", pch= 18, col = "red") 
lines(c(1:21), partner_stat_w3[,3]/1000, type = "o", pch= 18, col = "purple") 
title(main = "Population estimated by partnertship status \n with divorce, 2010 - 2030")
axis(1, at=seq(1,21,by=2), labels=seq(2010,2030,by=2))
axis(2, at=seq(1,25,by=2), labels=seq(0,24,by=2))
legend(1,5, legend=c("Single", "With partner", "Total"),  col=c("blue", "red", "purple"), lty=1:1, cex=0.8, pch= 18)



#-- With marriage, but no divorce
#-------------------------------

SimPop <- basePop

partner_stat_w2 <- table(SimPop$partnership)

set.seed(892674)
for(i in 1:20){
  
  SimPop <- Module_ageing(SimPop)  
  
  SimPop <- Module_birth(SimPop,ProbFun = ProbFun_birth, Mod = mod_birth)
  
  SimPop <- Module_mortality(SimPop,ProbFun = ProbFun_mort, ProbMat = LifeTable)
  
  SimPop <- Module_employ(SimPop,ProbFun = ProbFun_empl, Mod = mod_empl)
  
  SimPop <- Module_education(SimPop,ProbFun = ProbFun_edu,Mod1 = mod_edu_1, Mod2 = mod_edu_2, Mod3 = mod_edu_3)
  
  SimPop <- Module_marriage(SimPop, prob_market = prob_market, prob_couple = ProbFun_coupl)
  
  partner_stat_w2 <- rbind(partner_stat_w2, table(SimPop$partnership))
  
  print(i)
  
}

rownames(partner_stat_w2 ) <- c(2010,	2011,	2012,	2013,	2014,	2015,	2016,	2017,	2018,	2019,	2020,
                                2021,	2022,	2023,	2024,	2025,	2026,	2027,	2028,	2029,	2030)

colnames(partner_stat_w2) <- c("Single", "With partner")

total = as.numeric(partner_stat_w2[,1]) + as.numeric(partner_stat_w2[,2])
partner_stat_w2 <- cbind(partner_stat_w2,total)

partner_stat_w2

#graph
plot(c(1:21), partner_stat_w2[,1]/1000, type = "o", pch= 18, ylim = c(0,24), col = "blue",
     xlab = "Year", ylab = "Number of People (thousands)", xaxt='n', yaxt='n'  ) 
lines(c(1:21), partner_stat_w2[,2]/1000, type = "o", pch= 18, col = "red") 
lines(c(1:21), partner_stat_w2[,3]/1000, type = "o", pch= 18, col = "purple") 
title(main = "Population estimated by partnertship status \n without divorce, 2010 - 2030")
axis(1, at=seq(1,21,by=2), labels=seq(2010,2030,by=2))
axis(2, at=seq(1,25,by=2), labels=seq(0,24,by=2))
legend(1,5, legend=c("Single", "With partner", "Total"),  col=c("blue", "red", "purple"), lty=1:1, cex=0.8, pch= 18)