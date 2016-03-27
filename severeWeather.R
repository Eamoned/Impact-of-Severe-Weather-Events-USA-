####Downloading and Reading Data into R

# Data download
if(!file.exists("./data")) {dir.create("./data")}      #create a data dir if it doesn't exist
fileUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
download.file(fileUrl, destfile='./data/stormData.csv.bz2')  # Create file name and download datase

# R Packages
library(dplyr)
library(knitr)
library(markdown)



#read data to R
dataSet <- read.csv('./data/stormData.csv.bz2', sep=',', header=TRUE)

#check the dimensions
dim(dataSet)
str(dataSet)
head(dataSet,2)

#First reformat the date column
dataSet$BGN_DATE <- strptime(dataSet$BGN_DATE, '%m/%d/%Y %H:%M:%S')
dataSet$BGN_DATE <- as.Date(dataSet$BGN_DATE)


#Filter for post 1996
data96 <- subset(dataSet, BGN_DATE >= '1996-1-1') 


library(dplyr)
# Select relevant variables
stormData <- select(data96, BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
dim(stormData)
head(stormData,3)

####Event Type and Fatalities

summary(stormData$FATALITIES)
#Check for missing values
any(is.na(stormData$FATALITIES))

####Event Type and Injuries
summary(stormData$INJURIES) #Max is 1150 injuries
any(is.na(stormData$INJURIES))

subset(data96, INJURIES > 1000)

####Event Type and Property Damage
summary(data96$PROPDMG) #Max = 5000
any(is.na(data96$PROPDMG))

unique(data96$PROPDMGEXP)
#Check for entries that have significant PROPDMG values but no Multiplier 
a <- subset(data96, PROPDMGEXP == '')
summary(a$PROPDMG) #no values

subset(data96, PROPDMG == 5000 & EVTYPE == 'FLASH FLOOD')
subset(data96, REFNUM == '567221' | REFNUM == '605943')

####Event Type and Crop Damage
summary(data96$CROPDMG) #Max = 5000
any(is.na(data96$CROPDMG))
unique(data96$CROPDMGEXP)

#Check for entries that have significant PROPDMG values but no Multiplier 
a <- subset(data96, CROPDMGEXP == '')
summary(a$CROPDMG)

####Processing datasets for property and crop damage

#Select relevant data
damageData <- select(data96, BGN_DATE, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, REFNUM)

#Filter dataset for Property Damage only.
propDMG <- select(damageData, EVTYPE, PROPDMG, PROPDMGEXP, REFNUM)
propDMG <- filter(propDMG, REFNUM != '605943') #remove invalid double entry

##Remove any data without K, M or B and calculate damage estimates
kprop <- propDMG[(propDMG$PROPDMGEXP == 'K'),]
Mprop <- propDMG[(propDMG$PROPDMGEXP == 'M'),]
Bprop <- propDMG[(propDMG$PROPDMGEXP == 'B'),]

dataDMG <- bind_rows(kprop, Mprop)
dataDMG <- bind_rows(dataDMG, Bprop)

dataDMG$EXP[dataDMG$PROPDMGEXP == 'K'] <- 1000
dataDMG$EXP[dataDMG$PROPDMGEXP == 'M'] <- 1e+06
dataDMG$EXP[dataDMG$PROPDMGEXP == 'B'] <- 1e+09

#Filter datset for crop damage only
cropDMG <- select(damageData, EVTYPE, CROPDMG, CROPDMGEXP)

##Remove any data without K, M or B and calculate damage estimates
kcrop <- cropDMG[(cropDMG$CROPDMGEXP == 'K'),]
Mcrop <- cropDMG[(cropDMG$CROPDMGEXP == 'M'),]
Bcrop <- cropDMG[(cropDMG$CROPDMGEXP == 'B'),]

dataCropDMG <- bind_rows(kcrop, Mcrop)
dataCropDMG <- bind_rows(dataCropDMG, Bcrop)

#Caculate new values using multiplers
dataCropDMG$EXP[dataCropDMG$CROPDMGEXP == 'K'] <- 1000
dataCropDMG$EXP[dataCropDMG$CROPDMGEXP == 'M'] <- 1e+06
dataCropDMG$EXP[dataCropDMG$CROPDMGEXP == 'B'] <- 1e+09

###Results

#####The top weather events most harmful with respect to population health aross the United States

#Identify Fatalities caused by Events 
sumFatalities <- group_by(stormData, EVTYPE) %>%
      summarise(sum=sum(FATALITIES)) %>%
      arrange(desc(sum)) %>%
      print

sumFatalities <- head(sumFatalities,10) #top ten events
sumFatalities$EVTYPE <- as.character(sumFatalities$EVTYPE)

par(mar=c(6,4,4,4))
barplot(sumFatalities$sum, names.arg=sumFatalities$EVTYPE, cex.names=0.55,font.lab=1.8,las=2,main='Highest Fatalities By Event Type',
        ylab='Number of Fatalities', xlab='', cex.lab = 1)
mtext(side = 1, text = "Event Type", line = 4.7)

#Identify Injuries caused by Events
sumInjuries <- group_by(stormData, EVTYPE) %>%
      summarise(sum=sum(INJURIES)) %>%
      arrange(desc(sum)) %>%
      print

#Identify top 10 Events
sumInjuries <- head(sumInjuries,10)
sumInjuries$EVTYPE <- as.character(sumInjuries$EVTYPE)

par(mar=c(6,4,4,4))
barplot(sumInjuries$sum, names.arg=sumInjuries$EVTYPE, cex.names=0.53, font.lab =1.9, las=2,main='Highest Injuries By Event Type',
        ylab='Number of Injuries', xlab='', cex.lab=0.8)
mtext(side=1, text= 'Event Type', line= 4.8)

topPropDmg <- mutate(dataDMG, Est = PROPDMG * EXP) %>%
      group_by(EVTYPE) %>%
      summarise(EstDamage = sum(Est)) %>%
      arrange(desc(EstDamage)) %>%
      print

#Establish Events with most Damage
topCropDmg <- mutate(dataCropDMG, Est = CROPDMG * EXP) %>%
      group_by(EVTYPE) %>%
      summarise(EstDamage = sum(Est)) %>%
      arrange(desc(EstDamage)) %>%
      print

mergeData <- merge(topPropDmg, topCropDmg, by.x='EVTYPE', by.y='EVTYPE',all=TRUE)
mergeData[is.na(mergeData)] <- 0 # change NAs to zero.
economicDmg <- rename(mergeData, 'PropDamage'=EstDamage.x, 'CropDamage'=EstDamage.y)
economicDmg <- mutate(economicDmg, TotalDMG = PropDamage + CropDamage)

economicDmg <- arrange(economicDmg, desc(TotalDMG))
totalEconomicDmg <- head(economicDmg,10)

x <- rbind(totalEconomicDmg$PropDamage, totalEconomicDmg$CropDamage)
rownames(x) <- c("Property Damage", "Crop Damage")
colnames(x) <- totalEconomicDmg$EVTYPE

par(mar=c(6,5,3,3))
barplot(x, legend=rownames(x), ylab='', xlab='',  las=2, cex.names=0.53,font.lab=1.8,cex.lab=0.5, main='Economic Damage by Event Type')
mtext(side=1, text= 'Event Type', line= 4.8)
mtext(side=2, text= 'Total Damage in $US', line= 4.1)
