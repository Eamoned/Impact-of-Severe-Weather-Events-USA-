---
title: "Impact of Severe Weather Events (USA)"
date: "Thursday, December 26, 2015"
output: html_document
---

###Severe weather events impacting the economic and population health across the United States from 1950 to 2011. 

###Synopis
The aim of this report is to explore and identify which major weather events across the United States are most harmful with respect to population health, i.e. fatalities and injuries; and which types of events have the greatest economic consequences with respect to property damage and crop damage.
To identify these events, I explored and processed raw data from the U.S. National Oceanic and Atmospheric Administrations (NOAA) storm database. This database tracks and records characteristics of major weather events including the date, location and estimates of injuries, fatalities and damage incurred.
Using data specifically from 1996 (the most complete and reliable records) I was able to identify the top weather events impacting popultion health with excessive heat responsible for most fatalities, and tornado events responsible for most injuries. With respect to economic impact, hurricane/typhoon events contributed most to the cost of damage.

###Loading and Data Processing
Data was obtained from the [Storm Events Database](https://www.ncdc.noaa.gov/stormevents/).

For this report we obtained files for the years [1950 to November 2011](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).

Documentation from [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) was used to decipher variable names.

Supporting Weather event infomation was obtained from [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf).

Storm Events Database Record updates and changes can be found [here]( http://www.ncdc.noaa.gov/stormevents/details.jsp). This is of particular relevance to the data subset (from 1996 only) chosen for final analysis. 

####Downloading and Reading Data into R
The data for this report comes in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. Note, for data processing and analysis it's assumed relevant packages are installed and loaded. 


```r
# Data download
#if(!file.exists("./data")) {dir.create("./data")}      #create a data dir if it doesn't exist
#fileUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
#download.file(fileUrl, destfile='./data/stormData.csv.bz2')  # Create file name and download dataset
```


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(knitr)
library(markdown)
```


```r
#read data to R
dataSet <- read.csv('./data/stormData.csv.bz2', sep=',', header=TRUE)
```

```
## Warning in file(file, "rt"): cannot open file './data/stormData.csv.bz2':
## No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```
After reading the data in we check the dimensions, the first few rows and the variable types.

```r
dim(dataSet)
```

```
## Error in eval(expr, envir, enclos): object 'dataSet' not found
```

```r
#str(dataSet)
head(dataSet,2)
```

```
## Error in head(dataSet, 2): object 'dataSet' not found
```
Looking at the documentation in the NOAA web site, specifically [here]( http://www.ncdc.noaa.gov/stormevents/details.jsp), it highlights changes in the data collection and processing procedures over time. We can see that all event types are most complete (and defined) from 1996 to the present (48 from Directive 10-1605). Therefore, for this report we will explore data post 1996.    

Before We  can do this we will change the date format to allow subsetting of specific dates.

```r
#First reformat the date column
dataSet$BGN_DATE <- strptime(dataSet$BGN_DATE, '%m/%d/%Y %H:%M:%S')
```

```
## Error in strptime(dataSet$BGN_DATE, "%m/%d/%Y %H:%M:%S"): object 'dataSet' not found
```

```r
dataSet$BGN_DATE <- as.Date(dataSet$BGN_DATE)
```

```
## Error in as.Date(dataSet$BGN_DATE): object 'dataSet' not found
```

And now we subset for events from 1996 onwards

```r
#Filter for post 1996
data96 <- subset(dataSet, BGN_DATE >= '1996-1-1') 
```

```
## Error in subset(dataSet, BGN_DATE >= "1996-1-1"): object 'dataSet' not found
```
To assist exploratory analysis of the data we can filter for only variables of importance. This reduces the dataset from 902297 to 653530 entries. 

```r
#library(dplyr)
# Select relevant variables
stormData <- select(data96, BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
```

```
## Error in select_(.data, .dots = lazyeval::lazy_dots(...)): object 'data96' not found
```

```r
dim(stormData)
```

```
## Error in eval(expr, envir, enclos): object 'stormData' not found
```

```r
head(stormData,3)
```

```
## Error in head(stormData, 3): object 'stormData' not found
```
####Event Type and Fatalities

For Fatalities we're interested in the 'FATALITIES' column and here is a summary. Highest single fatality event is 158 fatalities.

```r
summary(stormData$FATALITIES)
```

```
## Error in summary(stormData$FATALITIES): object 'stormData' not found
```

```r
#Check for missing values
any(is.na(stormData$FATALITIES))
```

```
## Error in eval(expr, envir, enclos): object 'stormData' not found
```
####Event Type and Injuries

For Injuries we're interested in the 'INJURIES' column and here is a summary. Higest single Injury event is 1150 and there is no NAs.

```r
summary(stormData$INJURIES) #Max is 1150 injuries
```

```
## Error in summary(stormData$INJURIES): object 'stormData' not found
```

```r
any(is.na(stormData$INJURIES))
```

```
## Error in eval(expr, envir, enclos): object 'stormData' not found
```
Checking the validity of the higher values we can see the largest single event, a tornado, caused 1150 injuries and also accounted for the largest Fatality event (identified in the Fatalities dataset above) of 158 occurring in Missouri, county of Jasper, on the 22-05-2011.

```r
subset(data96, INJURIES > 1000)
```

```
## Error in subset(data96, INJURIES > 1000): object 'data96' not found
```

####Event Type and Property Damage
For property damage we're interested in the 'PROPDMG' column.  Here is a summary of property damage. There is also a multiplier column 'PROPDMGEXP' which may or may not increase these values.

```r
summary(data96$PROPDMG) #Max = 5000
```

```
## Error in summary(data96$PROPDMG): object 'data96' not found
```

```r
any(is.na(data96$PROPDMG))
```

```
## Error in eval(expr, envir, enclos): object 'data96' not found
```
Looking at the multipliers we can see there are quite a few levels and for this particular column there are K, M, B, 0, and entries with no multiplier. We can also see that there are no PROPDMG entries where there's no multiplier so we don't have to worry about missing some costs (entries that do not have a multiplier) when we filter for valid multipliers later. 

```r
unique(data96$PROPDMGEXP)
```

```
## Error in unique(data96$PROPDMGEXP): object 'data96' not found
```

```r
#Check for entries that have significant PROPDMG values but no Multiplier 
a <- subset(data96, PROPDMGEXP == '')
```

```
## Error in subset(data96, PROPDMGEXP == ""): object 'data96' not found
```

```r
summary(a$PROPDMG) #no values
```

```
## Error in summary(a$PROPDMG): object 'a' not found
```
Looking closely at the observations with property damage greater equal to 5000 there appears to be double entry for "FLASH FLOOD". Damage, State & Date are the same but they do have different County names (Mercer & Henry). The remarks in the 'REMARKS' column are almost identical except for the county names. The 'REFNUM' column refers to 808182 and 808183 consecutively.
However, the two counties are close to each other so it's possible each sustained similar damage in this event. 

```r
subset(data96, PROPDMG == 5000 & EVTYPE == 'FLASH FLOOD')
```

```
## Error in subset(data96, PROPDMG == 5000 & EVTYPE == "FLASH FLOOD"): object 'data96' not found
```
Similarly, there's a double entry for flood damage in Napa Dec 2005/ Jan 2006. One entry shows damage of 115M and a second entry has for 115B damage (REFNUM 567221 & 605943). A quick query on-line indicates that a more accurate value for this entry is 115 Million. As a consequence we will remove 'REFNUM' = 605943.

```r
subset(data96, REFNUM == '567221' | REFNUM == '605943')
```

```
## Error in subset(data96, REFNUM == "567221" | REFNUM == "605943"): object 'data96' not found
```

####Event Type and Crop Damage
For property damage we're interested in the 'CROPDMG' column.  Here is a summary of crop damage. And similar to property, there is also a multiplier column 'CROPDMGEXP' which may or may not increase these values.


```r
summary(data96$CROPDMG) #Max = 5000
```

```
## Error in summary(data96$CROPDMG): object 'data96' not found
```

```r
any(is.na(data96$CROPDMG))
```

```
## Error in eval(expr, envir, enclos): object 'data96' not found
```
Again taking into account the multipliers as we did in the property damage data, we have K, M, B and entries with no multiplier. And like the PROPDMG there are no CROPDMG entries where there's no 
multiplier so we don't have to worry about missing some costs when we filter for valid multipliers later.


```r
unique(data96$CROPDMGEXP)
```

```
## Error in unique(data96$CROPDMGEXP): object 'data96' not found
```

```r
#Check for entries that have significant PROPDMG values but no Multiplier 
a <- subset(data96, CROPDMGEXP == '')
```

```
## Error in subset(data96, CROPDMGEXP == ""): object 'data96' not found
```

```r
summary(a$CROPDMG)
```

```
## Error in summary(a$CROPDMG): object 'a' not found
```

####Dealing with multipliers

For this report we are only concerned with valid entries with letters K, M, and B entered in the PROPDMGEXP & CROPDMGEXP columns.

Section 2.7 page 12 of the [NATIONAL WEATHER SERVICE INSTRUCTION](https://d396qusza40orc.cloudfront.net/repdata/peer2_doc/pd01016005curr.pdf) published by NOAA states that for damage "Estimates should be rounded to three significant digits, followed by an alphabetical character signifying the magnitude of the number, i.e., 1.55B for $1,550,000,000. Alphabetical characters used to signify magnitude include "K" for thousands, "M" for millions, and "B" for billions." 

Also an earlier [Memorandum](http://www.nws.noaa.gov/wsom/manual/archives/NF429405.HTML#4.2.1) refers to estimations and multipliers in 2.2.4 (casualities) 2.2.5 and 4.2.1 specifically for entries and damaged estimates. 

Therefore, estimating multipliers other than those stated, may result in unreliable and most likely guessed estimates would may impact the reports findings.
We will also filter out any entries with no multipliers as discussed earlier.

####Processing datasets for property and crop damage

Select relevant columns for property and crop damage

```r
#Select relevant data
damageData <- select(data96, BGN_DATE, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, REFNUM)
```

```
## Error in select_(.data, .dots = lazyeval::lazy_dots(...)): object 'data96' not found
```
Here we will process the property damage selecting relevant columns and removing the double entry as discussed above.

```r
#Filter dataset for Property Damage only.
propDMG <- select(damageData, EVTYPE, PROPDMG, PROPDMGEXP, REFNUM)
```

```
## Error in select_(.data, .dots = lazyeval::lazy_dots(...)): object 'damageData' not found
```

```r
propDMG <- filter(propDMG, REFNUM != '605943') #remove invalid double entry
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'propDMG' not found
```
Now subset dataset with valid multipliers only (K, M or B).

```r
##Remove any data without K, M or B and calculate damage estimates
kprop <- propDMG[(propDMG$PROPDMGEXP == 'K'),]
```

```
## Error in eval(expr, envir, enclos): object 'propDMG' not found
```

```r
Mprop <- propDMG[(propDMG$PROPDMGEXP == 'M'),]
```

```
## Error in eval(expr, envir, enclos): object 'propDMG' not found
```

```r
Bprop <- propDMG[(propDMG$PROPDMGEXP == 'B'),]
```

```
## Error in eval(expr, envir, enclos): object 'propDMG' not found
```

```r
dataDMG <- bind_rows(kprop, Mprop)
```

```
## Error in bind_rows(kprop, Mprop): object 'kprop' not found
```

```r
dataDMG <- bind_rows(dataDMG, Bprop)
```

```
## Error in bind_rows(dataDMG, Bprop): object 'dataDMG' not found
```

Adding multiplier values to specific multipler letters.

```r
dataDMG$EXP[dataDMG$PROPDMGEXP == 'K'] <- 1000
```

```
## Error in dataDMG$EXP[dataDMG$PROPDMGEXP == "K"] <- 1000: object 'dataDMG' not found
```

```r
dataDMG$EXP[dataDMG$PROPDMGEXP == 'M'] <- 1e+06
```

```
## Error in dataDMG$EXP[dataDMG$PROPDMGEXP == "M"] <- 1e+06: object 'dataDMG' not found
```

```r
dataDMG$EXP[dataDMG$PROPDMGEXP == 'B'] <- 1e+09
```

```
## Error in dataDMG$EXP[dataDMG$PROPDMGEXP == "B"] <- 1e+09: object 'dataDMG' not found
```
Process the crop damage selecting relevant columns.


```r
#Filter datset for crop damage only
cropDMG <- select(damageData, EVTYPE, CROPDMG, CROPDMGEXP)
```

```
## Error in select_(.data, .dots = lazyeval::lazy_dots(...)): object 'damageData' not found
```
Subset the dataset with valid multipliers only (K, M or B).

```r
##Remove any data without K, M or B and calculate damage estimates
kcrop <- cropDMG[(cropDMG$CROPDMGEXP == 'K'),]
```

```
## Error in eval(expr, envir, enclos): object 'cropDMG' not found
```

```r
Mcrop <- cropDMG[(cropDMG$CROPDMGEXP == 'M'),]
```

```
## Error in eval(expr, envir, enclos): object 'cropDMG' not found
```

```r
Bcrop <- cropDMG[(cropDMG$CROPDMGEXP == 'B'),]
```

```
## Error in eval(expr, envir, enclos): object 'cropDMG' not found
```

```r
dataCropDMG <- bind_rows(kcrop, Mcrop)
```

```
## Error in bind_rows(kcrop, Mcrop): object 'kcrop' not found
```

```r
dataCropDMG <- bind_rows(dataCropDMG, Bcrop)
```

```
## Error in bind_rows(dataCropDMG, Bcrop): object 'dataCropDMG' not found
```
Add the multiplier value to specific multipler letters.

```r
#Caculate new values using multiplers
dataCropDMG$EXP[dataCropDMG$CROPDMGEXP == 'K'] <- 1000
```

```
## Error in dataCropDMG$EXP[dataCropDMG$CROPDMGEXP == "K"] <- 1000: object 'dataCropDMG' not found
```

```r
dataCropDMG$EXP[dataCropDMG$CROPDMGEXP == 'M'] <- 1e+06
```

```
## Error in dataCropDMG$EXP[dataCropDMG$CROPDMGEXP == "M"] <- 1e+06: object 'dataCropDMG' not found
```

```r
dataCropDMG$EXP[dataCropDMG$CROPDMGEXP == 'B'] <- 1e+09
```

```
## Error in dataCropDMG$EXP[dataCropDMG$CROPDMGEXP == "B"] <- 1e+09: object 'dataCropDMG' not found
```

###Results

#####The top weather events most harmful with respect to population health aross the United States

Estimate and identify events with the most fatalities.

```r
#Identify Fatalities caused by Events 
sumFatalities <- group_by(stormData, EVTYPE) %>%
      summarise(sum=sum(FATALITIES)) %>%
      arrange(desc(sum)) %>%
      print
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'stormData' not found
```

```r
sumFatalities <- head(sumFatalities,10) #top ten events
```

```
## Error in head(sumFatalities, 10): object 'sumFatalities' not found
```
Create a Plot for Fatalities.

```r
sumFatalities$EVTYPE <- as.character(sumFatalities$EVTYPE)
```

```
## Error in eval(expr, envir, enclos): object 'sumFatalities' not found
```

```r
par(mar=c(6,4,4,4))
barplot(sumFatalities$sum, names.arg=sumFatalities$EVTYPE, cex.names=0.55,font.lab=1.8,las=2,main='Highest Fatalities By Event Type',
        ylab='Number of Fatalities', xlab='', cex.lab = 1)
```

```
## Error in barplot(sumFatalities$sum, names.arg = sumFatalities$EVTYPE, : object 'sumFatalities' not found
```

```r
mtext(side = 1, text = "Event Type", line = 4.7)
```

```
## Error in mtext(side = 1, text = "Event Type", line = 4.7): plot.new has not been called yet
```

Estimate and identify events with most injuries.


```r
#Identify Injuries caused by Events
sumInjuries <- group_by(stormData, EVTYPE) %>%
      summarise(sum=sum(INJURIES)) %>%
      arrange(desc(sum)) %>%
      print
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'stormData' not found
```
Plot Injuries caused by Weather Events.

```r
#Identify top 10 Events
sumInjuries <- head(sumInjuries,10)
```

```
## Error in head(sumInjuries, 10): object 'sumInjuries' not found
```

```r
sumInjuries$EVTYPE <- as.character(sumInjuries$EVTYPE)
```

```
## Error in eval(expr, envir, enclos): object 'sumInjuries' not found
```

```r
par(mar=c(6,4,4,4))
barplot(sumInjuries$sum, names.arg=sumInjuries$EVTYPE, cex.names=0.53, font.lab =1.9, las=2,main='Highest Injuries By Event Type',
        ylab='Number of Injuries', xlab='', cex.lab=0.8)
```

```
## Error in barplot(sumInjuries$sum, names.arg = sumInjuries$EVTYPE, cex.names = 0.53, : object 'sumInjuries' not found
```

```r
mtext(side=1, text= 'Event Type', line= 4.8)
```

```
## Error in mtext(side = 1, text = "Event Type", line = 4.8): plot.new has not been called yet
```

Estimate and identify events with most property damage.

```r
topPropDmg <- mutate(dataDMG, Est = PROPDMG * EXP) %>%
      group_by(EVTYPE) %>%
      summarise(EstDamage = sum(Est)) %>%
      arrange(desc(EstDamage)) %>%
      print
```

```
## Error in mutate_(.data, .dots = lazyeval::lazy_dots(...)): object 'dataDMG' not found
```

Estimate and identify events with the most crop damage.


```r
#Establish Events with most Damage
topCropDmg <- mutate(dataCropDMG, Est = CROPDMG * EXP) %>%
      group_by(EVTYPE) %>%
      summarise(EstDamage = sum(Est)) %>%
      arrange(desc(EstDamage)) %>%
      print
```

```
## Error in mutate_(.data, .dots = lazyeval::lazy_dots(...)): object 'dataCropDMG' not found
```


To calculate overall economic damage we will merge crop and property damage.


```r
mergeData <- merge(topPropDmg, topCropDmg, by.x='EVTYPE', by.y='EVTYPE',all=TRUE)
```

```
## Error in merge(topPropDmg, topCropDmg, by.x = "EVTYPE", by.y = "EVTYPE", : object 'topPropDmg' not found
```

```r
mergeData[is.na(mergeData)] <- 0 # change NAs to zero.
```

```
## Error in mergeData[is.na(mergeData)] <- 0: object 'mergeData' not found
```

```r
economicDmg <- rename(mergeData, 'PropDamage'=EstDamage.x, 'CropDamage'=EstDamage.y)
```

```
## Error in rename_(.data, .dots = lazyeval::lazy_dots(...)): object 'mergeData' not found
```

```r
economicDmg <- mutate(economicDmg, TotalDMG = PropDamage + CropDamage)
```

```
## Error in mutate_(.data, .dots = lazyeval::lazy_dots(...)): object 'economicDmg' not found
```

```r
economicDmg <- arrange(economicDmg, desc(TotalDMG))
```

```
## Error in arrange_(.data, .dots = lazyeval::lazy_dots(...)): object 'economicDmg' not found
```

```r
totalEconomicDmg <- head(economicDmg,10)
```

```
## Error in head(economicDmg, 10): object 'economicDmg' not found
```
Now plot the the top weather events causing the most economic damage.

```r
x <- rbind(totalEconomicDmg$PropDamage, totalEconomicDmg$CropDamage)
```

```
## Error in rbind(totalEconomicDmg$PropDamage, totalEconomicDmg$CropDamage): object 'totalEconomicDmg' not found
```

```r
rownames(x) <- c("Property Damage", "Crop Damage")
```

```
## Error in rownames(x) <- c("Property Damage", "Crop Damage"): object 'x' not found
```

```r
colnames(x) <- totalEconomicDmg$EVTYPE
```

```
## Error in eval(expr, envir, enclos): object 'totalEconomicDmg' not found
```

```r
par(mar=c(6,5,3,3))
barplot(x, legend=rownames(x), ylab='', xlab='',  las=2, cex.names=0.53,font.lab=1.8,cex.lab=0.5, main='Economic Damage by Event Type')
```

```
## Error in barplot(x, legend = rownames(x), ylab = "", xlab = "", las = 2, : object 'x' not found
```

```r
mtext(side=1, text= 'Event Type', line= 4.8)
```

```
## Error in mtext(side = 1, text = "Event Type", line = 4.8): plot.new has not been called yet
```

```r
mtext(side=2, text= 'Total Damage in $US', line= 4.1)
```

```
## Error in mtext(side = 2, text = "Total Damage in $US", line = 4.1): plot.new has not been called yet
```



