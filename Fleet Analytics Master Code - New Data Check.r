
###################################################################################################################
#
#                                         Author: Tomas Meca & Yanika Borg
#                                         Date: 21Oct-2016
#                                         Project: Fleet Analysis
#
###################################################################################################################
#THIS CODE IS USED WHENEVER A NEW DATASET IS DELIVERED IN ORDER TO CHECK THE QUALITY OF EACH VARIABLE.FOLLOWING LOADING
#OF THE DATA, EACH COLUMN IS CHECKED INVIDIVIDUALLY IN THE SUBSEQUENT SECTION.

#==================================================================================================================
# CODE INDEX
#==================================================================================================================

# ENVIRONMENT SET-UP
# ADDRESSING DATA QUALITY ISSUES
# GENERATE SUMMARY

#==================================================================================================================
# ENVIRONMENT SET-UP
#==================================================================================================================


# PACKAGES NEEDED
library(xts) 
library(zoo) 
library(PerformanceAnalytics) 
library(magrittr)
library(broom)
library(ggplot2)
library(survsim)
library(OIsurv)
library(rms)
library(plyr)
library(data.table)


# CLEAN WORKSPACE AND CONSOLE
rm(list=ls(all=TRUE))
cat("\014") 

# NAMES OF FILES
Transactions <- "FleetAnalyticsData_Prepped_v6.csv"


# SET WORKING INPUT AND OUPUT ENVIRONMENTS FOR BOTH FOR TOMAS MECA AND YANIKA BORG  
TMi <- file.path("/Users",
                 "tmecafigueras",
                 "Documents",
                 "5. Projects",
                 "28. Client Wings",
                 "01 Fleet Analytics",
                 "20. Statistical Analysis",
                 "10 Input"
)

TMo <- file.path("/Users",
                 "tmecafigueras",
                 "Documents",
                 "5. Projects",
                 "28. Client Wings",
                 "01 Fleet Analytics",
                 "20. Statistical Analysis",
                 "30. Output"
)

TMe <- file.path("/Users",
                 "tmecafigueras",
                 "Documents",
                 "5. Projects",
                 "28. Client Wings",
                 "01 Fleet Analytics",
                 "20. Statistical Analysis"
)

YBi <- file.path("/Users",
                 "yborg",
                 "Desktop",
                 "Red Bull",
                 "01. Projects",
                 "130. Project - Fleet Analytics",
                 "20. Statistical Analysis",
                 "10. Input"
)

YBo <- file.path("/Users",
                 "yborg",
                 "Desktop",
                 "Red Bull",
                 "01. Projects",
                 "130. Project - Fleet Analytics",
                 "20. Statistical Analysis",
                 "30. Output"
)

YBe <- file.path("/Users",
                 "yborg",
                 "Desktop",
                 "Red Bull",
                 "01. Projects",
                 "130. Project - Fleet Analytics",
                 "20. Statistical Analysis",
                 "20. Script"
)

# LOAD FUNCTIONS AND DATA
try(setwd(YBe), silent = TRUE)
try(setwd(TMe), silent = TRUE)
source("R_Functions_TM.r") 
try(setwd(YBi), silent = TRUE)
try(setwd(TMi), silent = TRUE)
master  <- read.csv(Transactions,
                    header=TRUE,
                    sep=","
)

# SET OUTPUT DIRECTORY 
try(setwd(TMo), silent = TRUE)
try(setwd(YBo), silent = TRUE)

# INFORM USER WHERE THE OUTPUT IS
paste("THE PROJECT OUTPUT RESIDES IN:",
      getwd(),
      sep = ""
)


#==================================================================================================================
# ADDRESSING DATA QUALITY ISSUES - THIS SECTION IS TO BE USED WHEN NEW DATA IS RECEIVED IN ORDER TO CHECK WHETHER
# IT IS FIT FOR PURPOSE AND FLAG UP ISSUES
#==================================================================================================================


x <- 2


names(master)[x]
#x <- which(names(master) == "Asset.Status")
str(master[x])

unique(master[,x])

length(unique(master[,x]))
#sort(unique(master[,x]))

sum(is.na(master[,x]))
sum(is.na(master[,x])) / dim(master)[1]
#sum(master[,x]=="WINDSHIELD / GLASS")
#length(which(master[,x]==0))


# FOR NUMERICAL DATA
x11()
plot(master[,x])

max(master[,x],na.rm=TRUE)
min(master[,x],na.rm=TRUE)


x <- 51

names(master)[x]


# GROUPS FOR CATEGORICAL DATA
df <- data.frame(table(master[x]))
names(df)[1] <- names(master)[x]
df <-df[with(df, order(-Freq)), ]
write.csv(df,
          file = paste(names(master)[x],".csv",sep="")
) 
head(df)
tail(df)


# MISCELLANEOUS STUDY OF CATEGORIES
sum(df[,2])
sum(df[,2])+sum(is.na(master[,x])) #sum should come to 127176
sum(df[,2]<11)



#==================================================================================================================
# GENERATE SUMMARY
#==================================================================================================================
#FOLLOWING A COMPLETE CHECK OF THE VARIABLES, AN OVERALL SUMMARY OF THE DATA IS GENERATED. 

Data.Availability(data.tm = master, name.file= "MasterSummary")




#==================================================================================================================
# CHECKING TRANSACTIONALITY OF DATA
#==================================================================================================================
#THE PURPOSE OF THIS SECTION IS TO CHECK WHETHER EACH VARIABLE/COLUMN CONTAINS DATA WHICH IS INTRINSIC FOR THE VEHICLE 
#OR TRANSACTIONAL. FOR EXAMPLE, THE MAKE OF THE VEHICLE IS INTRINSIC AND DOES NOT CHANGE WHEN CONSIDERING ONE SPECIFIC 
#VEHICLE. ON THE OTHER HAND, THE MAINTENANCE AMOUNT FOR ONE VEHICLES CHANGES FOR EACH TRANSACTION IT UNDERGOES. 

for (i in 1:dim(master)[1]){
  
  # SELECT ALL UNIQUE COMBINATIONS OF GE.UNIT AND THE VARIABLE UNDER CONSIDERATION
  uni <- unique(master[,c(names(master)[i], "GE.Unit")])
  
  # CHECK WHETHER ANY OF THE GE.UNITS ARE DUPLICATED. IF YES, THEN THE VARIABLE UNDER CONSIDERATION IS TRANSACTIONAL
  dup <- uni[duplicated(uni[,2])==TRUE,]
  
  # PRINT RESULT
  out <- cbind(names(master)[i],ifelse(dim(dup)[1] > 0 ,"TRANSACTIONAL", "INTRINSIC"))
  print(out)
}

