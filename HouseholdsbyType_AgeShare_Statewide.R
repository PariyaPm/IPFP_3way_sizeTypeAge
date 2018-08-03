#####################################
# Created by Sarah Philbrick and Pariya Pourmohammadi
# Creation Date 062618
# This file is used to Group PUMs data based on HH type
# by age by TAZ, Agent, and Household size 
# PUMS 08_12
#####################################

#####################################
library(readxl)
library(GDAtools)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape)
#####################################
#Pull in 2008-2012 PUMS Data
Per12 <- read.table(file="K:/DataServices/Projects/Current_Projects/Projections_2050/Data/Tabular/Raw/ss12pma.csv", header=TRUE, sep=",") 
HH12 <- read.table(file="K:/DataServices/Projects/Current_Projects/Projections_2050/Data/Tabular/Raw/ss12hma.csv", header=TRUE, sep=",")
PHa12 <- join(HH12,Per12, by = c("SERIALNO"), type = "left", match = "all")
rm(Per12, HH12)

##############################################################################################
#####################Categorize Data#########################################################
#############################################################################################
# Consolidated Age Category
# NOTE: Levels are (1) 15-34; (2) 35 to 44; (3) 45 to 64; (4) 65+
PHa12$PAGEC2 <- cut(PHa12$AGEP, breaks=c(-Inf,14,34,44,64,Inf), labels=c("0","1","2","3","4"))
PHa12$PAGEC2[PHa12$AGEP==999] <- NA

#counts as child
PHa12$child[PHa12$AGEP<18 & PHa12$SPORDER!=1] = 1
PHa12$child[PHa12$AGEP>=18] = 0
PHa12$child[PHa12$SPORDER==1 & PHa12$AGEP>16] = 0
#Aggregate children in HHds
PHchld12 <- PHa12[c("SERIALNO", "child")]
#PHwrk12$WRKTOT <- ddply(PHwrk12, .(SERIALNO), .fun=summarize, .WRKTOT=sum(worker))
temp <- aggregate(PHa12$child, by=list(PHa12$SERIALNO), FUN=sum)
colnames(temp) <- c("SERIALNO", "CHILDTOT")
#regoin hh attribute (CHILDTOT) to person dataset
PHa12 <- join(temp, PHa12, by = c("SERIALNO"), type = "left", match = "all")
rm(PHchld12, temp)

#counts as person
PHa12$person[PHa12$SPORDER>0] = 1
#Aggregate people in HHds
PHperson12 <- PHa12[c("SERIALNO", "person")]
temp <- aggregate(PHa12$person, by=list(PHa12$SERIALNO), FUN=sum)
colnames(temp) <- c("SERIALNO", "PERSONTOT")
#regoin hh attribute (PERSONTOT) to person dataset
PHa12 <- join(temp, PHa12, by = c("SERIALNO"), type = "left", match = "all")
rm(PHperson12, temp)

#Create person categories-- 1 person, 2 person, 3 person, 4 plus person
PHa12$HHSize[PHa12$PERSONTOT==1] = 1
PHa12$HHSize[PHa12$PERSONTOT==2] = 2 
PHa12$HHSize[PHa12$PERSONTOT==3] = 3 
PHa12$HHSize[PHa12$PERSONTOT>=4] = 4 

#Adjust HH Income into constant dollars (2012)
PHa12$HHINC <- round(as.numeric(PHa12$ADJINC) * as.numeric(PHa12$HINCP)/1000000,0)
#Break household income into wage groups
PHa12$HHINCC <- cut(PHa12$HHINC, breaks=c(-Inf, 14999, 34999, 74999, 149999, Inf), labels=c("1","2","3","4", "5"))
PHa12$HHINCC2 <- cut(PHa12$HHINC, breaks=c(-Inf, 34999, 74999, 124999, Inf), labels=c("1","2","3","4"))

#counts as worker
PHa12$worker[is.na(PHa12$ESR)]= 0 #under 16
PHa12$worker[PHa12$ESR==0] = 0 #under 16
PHa12$worker[PHa12$ESR==1] = 1 #lf
PHa12$worker[PHa12$ESR==2] = 1 #lf
PHa12$worker[PHa12$ESR==3] = 1 #lf
PHa12$worker[PHa12$ESR==4] = 1 #non civilian
PHa12$worker[PHa12$ESR==5] = 1 #non civilian
PHa12$worker[PHa12$ESR==6] = 0 #not in lf

#Aggregate workers in HHds
PHwrk12 <- PHa12[c("SERIALNO", "worker")]
#PHwrk12$WRKTOT <- ddply(PHwrk12, .(SERIALNO), .fun=summarize, .WRKTOT=sum(worker))
temp <- aggregate(PHa12$worker, by=list(PHa12$SERIALNO), FUN=sum)
colnames(temp) <- c("SERIALNO", "WRKTOT")
#regoin hh attribute (WRKTOT) to person dataset
PHa12 <- join(temp, PHa12, by = c("SERIALNO"), type = "left", match = "all")
rm(PHwrk12, temp)

#Create worker categories-- 1 wrkr, 2 wrkr, 3p wrkr
PHa12$WRKHH[PHa12$WRKTOT==0] = 0
PHa12$WRKHH[PHa12$WRKTOT==1] = 1 
PHa12$WRKHH[PHa12$WRKTOT==2] = 2 
PHa12$WRKHH[PHa12$WRKTOT>=3] = 3 


PHa12$HHtype <- 0

HH_type_allocator <- function (PHa12 ){
  PHa12$HHtype[which(PHa12$HHSize == 1 & PHa12$CHILDTOT == 0) ]<- 1
  ##### type 1 => single person HH type
  
  PHa12$HHtype[which(PHa12$HHSize > 1 & PHa12$CHILDTOT == 0) ] <- 2
  ##### type 2 => multiple people without childeren in HH
  
  
  PHa12$HHtype[which(PHa12$HHSize > 1 & PHa12$CHILDTOT > 0) ] <- 3
  ##### type 3 => multiple people with childeren (age <18)
  return(PHa12)
}

###################################################GEOGRAPHIES#################################################################################
puma2010_geog <- read.csv("K:/DataServices/Projects/Current_Projects/Projections_2050/Data/Tabular/Analysis/PUMA_2010Geog.csv", as.is = T)
puma2000_geog <- read.csv("K:/DataServices/Projects/Current_Projects/Projections_2050/Data/Tabular/Analysis/PUMA_2000Geog.csv", as.is = T)
#Join Geographies to joined files
PHa12$MAPC <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$MAPC)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$MAPC)))
PHa12$OCPC <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$OCPC)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$OCPC)))
PHa12$SRPEDD <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$SRPEDD)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$SRPEDD)))
PHa12$MVC <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$MVC)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$MVC)))
PHa12$NPEDC <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$NPEDC)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$NPEDC)))
PHa12$NMCOG <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$NMCOG)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$NMCOG)))
PHa12$MVPC <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$MVPC)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$MVPC)))
PHa12$MRPC <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$MRPC)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$MRPC)))
PHa12$CMRPC <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$CMRPC)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$CMRPC)))
PHa12$FRCOG <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$FRCOG)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$FRCOG)))
PHa12$PVPC <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$PVPC)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$PVPC)))
PHa12$BRPC <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$BRPC)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$BRPC)))
PHa12$CCC <- ifelse(PHa12$PUMA00!=-9,(plyr::mapvalues(PHa12$PUMA00, from = puma2000_geog$name, to = puma2000_geog$CCC)),(plyr::mapvalues(PHa12$PUMA10, from = puma2010_geog$name, to = puma2010_geog$CCC)))
##############################################################################################

PHa12 <- HH_type_allocator(PHa12)


NoHHtype <- PHa12[which(PHa12$HHtype == 0),]
wtHHtype <- PHa12[which(PHa12$HHtype >0),]
a <- wtHHtype

#add puma attribute with puma value and the year
a$puma <- ifelse(a$PUMA00>0, paste0(a$PUMA00,"_00"), paste0(a$PUMA10,"_10"))
a$year <- ifelse(a$PUMA00>0, 10, 00)

#create a new df including "SERIALNO", "PUMA00","PUMA10","puma","year","AgntID","HHSize","PAGEC2", "WGTP", "AGEP" data
summarized_dat <- data.frame(cbind(a$SERIALNO,a$PUMA00,a$PUMA10, a$puma ,a$year , a$HHtype, a$HHSize, a$PAGEC2, a$PWGTP, a$AGEP))
names(summarized_dat) <- c("SERIALNO", "PUMA00","PUMA10","puma","year","HHtype","HHSize","PAGEC2", "PWGTP", "AGEP")

#set new age category of -Inf,4,14,18, Inf for the data
summarized_dat$TAZ_ageC <- cut(as.numeric(as.character(summarized_dat$AGEP)), breaks=c(-Inf,4,14,18, Inf), labels=c("1","2","3","4"))
summarized_dat$TAZ_ageC[summarized_dat$AGEP==999] <- NA

#Join the pums data to each TAZ geography. There is a PUMA for 2000 and a PUMA for 2010 PHa12$PUMA00 and PHa12$PUMA10
#K:\DataServices\Projects\Current_Projects\Projections_2050\Data\Tabular\ReferenceData\TAZ_lookup.xlsx
TAZ_lookup <- read_xlsx('K:\\DataServices\\Projects\\Current_Projects\\Projections_2050\\Data\\Tabular\\ReferenceData\\TAZ_lookup.xlsx', col_names = TRUE)

##>> generate unique ID by concatinating the puma/agntID/HHsize/TAZageC
#summarized_dat$uniqueID <- paste(summarized_dat$SERIALNO,summarized_dat$puma, summarized_dat$HHtype, summarized_dat$HHSize, summarized_dat$TAZ_ageC, sep = "_" )
summarized_dat$uniqueID <- paste(summarized_dat$puma , summarized_dat$HHtype, summarized_dat$HHSize, summarized_dat$TAZ_ageC, sep = "_" )

#Unique_id <- unique(summarized_dat$uniqueID)

##> add weights per unique ID
by_uniqueID <- summarized_dat %>% group_by(uniqueID)
sumhh <- by_uniqueID %>% summarise(wgts = sum(as.numeric(PWGTP)))

## > tokenize unique ID and recreate the data columns of puma/pumayear/agntID/HHsize/TAZageC
temp <- str_split(sumhh$uniqueID,pattern = "_", n = 5)

temp_tokenized <-as.data.frame(matrix(unlist(temp),nrow=length(temp), byrow=T), stringsAsFactors = TRUE)
names(temp_tokenized) <- c("puma","year", "HHtype", "HHSize", "TAZ_ageC")

#set the sumWgt val for each record
Sum_dat <- cbind(sumhh, temp_tokenized )

###>> parse data in 2000 and 2010 data to do TAZlookup
Sum_dat00 <- Sum_dat[which(Sum_dat$year=="00"),]
Sum_dat10 <- Sum_dat[which(Sum_dat$year == "10"),]

#split the data for puma2000 and data for puma 2010
a <- data.frame(table(TAZ_lookup$puma2000))
b <- data.frame(table(TAZ_lookup$puma2010))


###puma data aggregation
#create a new field to keep the number of taz values per puma
Sum_dat00$countTAZ <- a$Freq[match(Sum_dat00$puma, a$Var1)]
Sum_dat10$countTAZ <- b$Freq[match(Sum_dat10$puma, b$Var1)]

#expand the table based on TAZ frequencies
Sum_dat00_exp <-untable(Sum_dat00, num=Sum_dat00$countTAZ)
Sum_dat10_exp <-untable(Sum_dat10, num=Sum_dat10$countTAZ)

#find the TAZ_ID of each record
Sum_dat00_exp $TAZID <- -1
Sum_dat10_exp $TAZID <- -1
for (i in 1:length(Sum_dat00[[1]])){
  Sum_dat00_exp$TAZID[which(Sum_dat00_exp$uniqueID == Sum_dat00$uniqueID[i])] <- TAZ_lookup$TAZ_ID[which(TAZ_lookup$puma2000==Sum_dat00$puma[i])]
}

for (i in 1:length(Sum_dat10[[1]])){
  Sum_dat10_exp$TAZID[which(Sum_dat10_exp$uniqueID == Sum_dat10$uniqueID[i])] <- TAZ_lookup$TAZ_ID[which(TAZ_lookup$puma2010==Sum_dat10$puma[i])]
}

####> combine two datasets 
Total_Data <- rbind(Sum_dat00_exp,Sum_dat10_exp)

Total_Data00 <- Total_Data[which(Total_Data$year == "00"),]
Total_Data00$tmpID <- 1:length(Total_Data00[[1]])

Total_Data10 <- Total_Data[which(Total_Data$year == "10"),]
Total_Data10$tmpID <- 1:length(Total_Data10[[1]])

merged_tab <- merge.data.frame(Total_Data00,Total_Data10, by.x =c("TAZID","HHtype","HHSize","TAZ_ageC"), by.y = c("TAZID","HHtype","HHSize","TAZ_ageC"))

lefOutX <- Total_Data00[which(!Total_Data00$tmpID %in% merged_tab$tmpID.x),]
lefOutX1 <- data.frame(matrix(ncol = length(names(merged_tab)), nrow = length(lefOutX$TAZID)))

names(lefOutX1) = c("TAZID"   ,   "HHtype"    , "HHSize"  ,   "TAZ_ageC"  , "uniqueID.x", "wgts.x" ,"puma.x" ,"year.x" , "countTAZ.x", "tmpID.x","uniqueID.y",
                    "wgts.y","puma.y", "year.y","countTAZ.y","tmpID.y" )

lefOutX1$TAZID = lefOutX$TAZID
lefOutX1$HHtype = lefOutX$HHtype 
lefOutX1$HHSize = lefOutX$HHSize 
lefOutX1$TAZ_ageC = lefOutX$TAZ_ageC
lefOutX1$uniqueID.x = lefOutX$uniqueID 
lefOutX1$wgts.x= lefOutX$wgts
lefOutX1$puma.x = lefOutX$puma 
lefOutX1$year.x = lefOutX$year
lefOutX1$countTAZ.x= lefOutX$countTAZ 
lefOutX1$tmpID.x = lefOutX$tmpID
lefOutX1$uniqueID.y = 0 
lefOutX1$wgts.y = 0 
lefOutX1$puma.y = NA 
lefOutX1$year.y ="10" 
lefOutX1$countTAZ.y = NA  
lefOutX1$tmpID.y = NA


lefOutY <- Total_Data10[which(!Total_Data10$tmpID %in% merged_tab$tmpID.y),]
lefOutY1 <- data.frame(matrix(ncol = length(names(merged_tab)), nrow = length(lefOutY$TAZID)))

names(lefOutY1) = c("TAZID"   ,   "HHtype"    , "HHSize"  ,   "TAZ_ageC"  , "uniqueID.x", "wgts.x" ,"puma.x" ,"year.x" , "countTAZ.x", "tmpID.x","uniqueID.y",
                    "wgts.y","puma.y", "year.y","countTAZ.y","tmpID.y" )

lefOutY1$TAZID = lefOutY$TAZID
lefOutY1$HHtype = lefOutY$HHtype 
lefOutY1$HHSize = lefOutY$HHSize 
lefOutY1$TAZ_ageC = lefOutY$TAZ_ageC
lefOutY1$uniqueID.x = 0 
lefOutY1$wgts.x= 0 
lefOutY1$puma.x = NA
lefOutY1$year.x = "00"
lefOutY1$countTAZ.x= NA
lefOutY1$tmpID.x = NA
lefOutY1$uniqueID.y = lefOutY$uniqueID 
lefOutY1$wgts.y = lefOutY$wgts
lefOutY1$puma.y =  lefOutY$puma 
lefOutY1$year.y =lefOutY$year 
lefOutY1$countTAZ.y = lefOutY$countTAZ 
lefOutY1$tmpID.y = lefOutY$tmpID

merged_table <- rbind(merged_tab,lefOutX1,lefOutY1)

merged_table$compWgt <- 0.8*merged_table$wgts.x + 0.2*merged_table$wgts.y
cleanedData <- merged_table
#colnames(cleanedData)
cleanedData <- cleanedData[, -c(5,8, 9,10,11,14,15,16)]

names(cleanedData) <- c("TAZID"   ,   "HHtype"    , "HHSize"  ,   "TAZ_ageC"  ,"wgts.2000" ,"puma.2000" ,
                        "wgts.2010", "puma.2010", "compWgt" )

#Find the ratio of weights per age category in grouped data based on AZID, HHtype, HHSize
Grouped_dat <- cleanedData %>% group_by(TAZID, HHtype, HHSize) %>% summarise(sum(compWgt))
cleanedData$weightRatio <- 0.0

for(i in 1:length(cleanedData[[1]])) {
  index <- which(cleanedData$TAZID[i] == Grouped_dat$TAZID & cleanedData$HHtype[i] == Grouped_dat$HHtype  & cleanedData$HHSize[i] == Grouped_dat$HHSize)
  cleanedData$weightRatio[i] <- cleanedData$compWgt[i]/Grouped_dat$`sum(compWgt)`[index]
  # print(i)
}

#####> write to :K:\DataServices\Projects\Current_Projects\Projections_2050\Data\Tabular\TAZ_assigned_dat.csv file
getwd()
setwd("K:/DataServices/Projects/Current_Projects/Projections_2050/Data/Tabular")
write.csv(cleanedData, file = 'TAZ_assigned_dat_HHtype.csv')

#######################################################################################################################
####################data preprocessing done!