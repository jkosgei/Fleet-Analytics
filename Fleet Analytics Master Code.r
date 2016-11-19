  ###################################################################################################################
  #
  #                                         Author: Tomas Meca & Yanika Borg
  #                                         Date: 19-Sep-2016
  #                                         Project: Fleet Analysis
  #                                         Purpose: Working Document
  #
  ###################################################################################################################
  #THIS IS THE MAIN CODE TO USE. ALL OTHER CODES SHOULD BE REFERRED TO AS REQUIRED.

  #==================================================================================================================
  # CODE INDEX
  #==================================================================================================================
  
  # ENVIRONMENT SET-UP
  # ADDRESSING DATA QUALITY ISSUES
  # DQ ISSUES AND ADDING VARIABLES TO THE MASTER DATA AND TIME PERIOD SELECTION
  # EXPLANATORY DATA ANALYSIS
  # CREATION OF VEHICLE LEVEL DATA SET
  # CREATION OF TIME LEVEL DATA SET 
  # SURVIVAL ANALYSIS

  # TRANSFERRED - ADDRESSING DATA QUALITY ISSUES - GENERATE SUMMARY
  # TRANSFERRED - ADDRESSING DATA QUALITY ISSUES - WORKING AREA (USED ONLY WHEN PREPPRED DATA V4 WAS DELIVERED DURING QA)
  # TRANSFERRED - SAVING EDITED AND PREPARED DATA
  # TRANSFERRED - CHECKING TRANSACTIONALITY OF DATA
  # TRANSFERRED - DEDUCING LAST MONTH WHEN WE HAVE FULL TRANSACTION DATA
  # TRANSFERRED - EXPLORATORY PLOT OF VEHICLES, TRANSACTIONS AND COSTS
  # TRANSFERRED - BQ 1.1	VISUALISATION OF TOTAL FLEET COST BREAKDOWN
  # TRANSFERRED - BQ3 - PREVENTATIVE MAINTENACE STUDY
  # TRANSFERRED - NUMERICAL VARIABLES ANALSYS FOR POTENTIAL DQ ISSUES
  # TRANSFERRED - APPENDIX - PREVIOUS ANALYSIS OF V3 OF PREPPED DATA
  # TRANSFERRED - APPENDIX - FUNCTIONS TO REFER TO IF NEEDED IN EXPLORATORY AND TIME SERIES ANALYSIS
  #  

  
#==================================================================================================================
# VERSIONING CONTROL
#==================================================================================================================
# V1: CODE START. DATA QUALITY ISSUES. INITIAL EXPLANATORY ANALYSIS
# v2: NEWER VERSION BECAUSE INPUT FILE HAS BEEN UPDATED SO THAT ISUES WITH NAs AND IDS ARE ADDRESSED IN SECTION '
#     ADRESSING DATA QUALITY ISSUES', 'ADDRESSING DATA QUALITY ISSUES - GENERATE SUMMARY' AND 'ADDRESSING DATA 
#     QUALITY ISSUES - WORKING AREA'
# V3: ADDITION OF FURTHER CODE LINES TO ADDRESS ISSUES WITH NA'S AND ADDITION OF CODE LINES FOR FURTHER TESTING 
#     (FOR EXAMPLE, COMMAND LINE "TAIL(DF)"). THESE WERE DEVELOPED TO TEST QUALITY OF V4 OF PREPPED DATA AFTER MAJOR
#     QUALITY ISSUES WERE DISCOVERED IN PREVIOUS VERSIONS.
#     IN SECTION "# ADDRESSING DATA QUALITY ISSUES", CODE LINES WHICH HAVE "#USED" AT THE END INDICATE THAT THEY WERE
#     USED IN THE DATA QUALITY CHECKS OF V4 OF PREPPED DATA.
# v4: ADDITION OF SECTION 'SAVING EDITED AND PREPARED DATA' AND 'BQ3 - PREVENTATIVE MAINTENACE STUDY' FOR INITIAL 
#     ANALYSE OF BQ3 REGARDING PREVENTATIVE MAINTENANCE
#     RE-STRUCTURING OF FILE - MOVING UNUSED SECTIONS TO 'APPENDIX - PREVIOUS ANALYSIS OF V3 OF PREPPED DATA' AND
#     MOVING FUNCTIONS USED IN EXPLORATORY ANALYSIS TO 'APPENDIX - FUNCTIONS TO REFER TO IF NEEDED IN EXPLORATORY AND 
#     TIME SERIES ANALYSIS'
# v5: ADDITION OF
# V6: ADDITION OF CODE BY TOMAS WHICH INCLUDES THE FOLLOWING SECTIONS:
#     - SET-UP FOR DEVELOPMENT OF TEMPORAL AND VEHICLE FRAMEWORKS PLUS DEEP DIVE EXPLORATORY AND SURVIVAL ANALYSIS
#     - QUALITY ASSURANCE FOR DATES - DEFINING THE TIME PERIOD OF THIS STUDY
#     - DEDUCING LAST MONTH WHEN WE HAVE FULL TRANSACTION DATA
#     - CREATION OF VEHICLE LEVEL DATA SET
#     - CREATION OF TIME LEVEL DATA SET 
#     - EXPLORATORY PLOT OF VEHICLES, TRANSACTIONS AND COSTS
#     - SURVIVAL ANALYSIS
#     - BUSINESS QUESTION: 1.1	VISUALISATION OF TOTAL FLEET COST BREAKDOWN
#     -NUMERICAL VARIABLES ANALSYS FOR POTENTIAL DQ ISSUES
#     MOVEROVER ADDITION OF FUNCTIONS TO SECTION 'APPENDIX - FUNCTIONS TO REFER TO IF NEEDED IN EXPLORATORY AND TIME SERIES ANALYSIS'
# v7: TRANSFER OF UNECESSARY AND UNUSED SECTIONS TO THE MASTER DOCUMENT. THIS IS RECORDED WITHIN THE CODE INDEX. THE 
#     RESULTING DOCUMENT HAS ONLY THE SECTIONS WE ARE CURRENTLY WORKING ON. 
# v8: CODE REARRENGEMENT DRAWING ON THE DIFFERENT SEC
# v9: NEW DATA ADDTION FleetAnalyticsData_Prepped_v5.csv
# V10:COMPLETE RESUTRUCTURING OF CODE
# V11:EDITING OF SURVIVAL ANALYSIS
# V12:ADDITION OF CODE FOR EDA FUNCTIONS
# V13 - V20 - TOMAS
# v21 - REPLACEMENT OF DATASET 5 WITH DATASET 6. EDITING OF 'ADDRESSING DATA QUALITY ISSUES' SECTION TO ADDRESS NEW COLUMNS IN DATA

  
  
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
  library(dplyr)
  library(Hmisc)
  
  
# CLEAN WORKSPACE AND CONSOLE
  rm(list=ls(all=TRUE))
  cat("\014") 

# SET WORKING DIRECTORY
#  setwd("C:/Users/jkosgei/Egnyte/Shared/Departments US/ANALYTICS/100 Projects/4180 Fleet Analytics/03 PREPARE")
#  master <- read.csv("FleetAnalyticsData_Prepped_v5.csv", header=TRUE, sep=",")
    
# NAMES OF FILES
  Transactions <- "FleetAnalyticsData_Prepped_v5.csv"

  
# SET WORKING INPUT AND OUPUT ENVIRONMENTS FOR BOTH FOR TOMAS MECA AND YANIKA BORG  
  JKi <- file.path("/Users",
                   "jkosgei",
                   "Egnyte",
                   "Shared",
                   "Departments US",
                   "ANALYTICS",
                   "100 Projects",
                   "4180 Fleet Analytics",
                   "03 PREPARE"
  )
  JKo <- file.path("/Users",
                   "jkosgei",
                   "Egnyte",
                   "Shared",
                   "Departments US",
                   "ANALYTICS",
                   "100 Projects",
                   "4180 Fleet Analytics",
                   "04 ANALYZE",
                   "Analysis Output"
  )
  JKe <- file.path("/Users",
                   "jkosgei",
                   "Egnyte",
                   "Shared",
                   "Departments US",
                   "ANALYTICS",
                   "100 Projects",
                   "4180 Fleet Analytics",
                   "04 ANALYZE"
  )
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
  try(setwd(JKe), silent = TRUE)
  source("Fleet Analytics Master Code - Functions For Analysis.r") 
  source("Fleet Analytics Master Code - Temporal Analysis.r")
  source("Fleet Analytics Master Code - Histogram Static.r")
  try(setwd(YBi), silent = TRUE)
  try(setwd(TMi), silent = TRUE)
  try(setwd(JKi), silent = TRUE)
  master  <- read.csv(Transactions,
                      header=TRUE,
                      sep=","
  )

# SET OUTPUT DIRECTORY 
  try(setwd(JKo), silent = TRUE)
  try(setwd(TMo), silent = TRUE)
  try(setwd(YBo), silent = TRUE)
  
# INFORM USER WHERE THE OUTPUT IS
  paste("THE PROJECT OUTPUT RESIDES IN:",
        getwd(),
        sep = ""
  )
  
# ADD A UNIQUE ID TO THE DATA
  master$ID <- 1:dim(master)[1]
  
  names(master)[3] <- "Trans.Odometer.Clean"
  
#==================================================================================================================
# ADDRESSING DATA QUALITY ISSUES
#==================================================================================================================
  # BASIC DATA CHARACTERISTICS
  head(master)  
  tail(master)
  str(master)
  summary(master)
  describe(master)
  
  # BEFORE CHANGING THE DATAFRAME WE CHANGE FROM FACTORS TO CHARACTER VECTORS
  indx <- sapply(master, is.factor)
  master[indx] <- lapply(master[indx], function(x) as.character(x))
  
  # DATA TRANSFORMATION DUE TO DATA QUALITY ISSUES
  
  master$After.the.Fact.Switch[master$After.the.Fact.Switch == ""] <- NA    #used
  
  #master$Current.Odom.Edited[master$Current.Odom.Edited == ""] <- NA        #
  #master$Current.Odom.Edited[master$Current.Odom.Edited == "Yes"] <- "Y"    #
  #master$Current.Odom.Edited[master$Current.Odom.Edited == "No"] <- "N"     #
  #master$Current.Odometer.Reading.Date <- as.Date(master$Current.Odometer.Reading.Date, format = "%Y-%m-%d")   #
  
  master$Factory.Order.Date <- as.Date(master$Factory.Order.Date, format = "%Y-%m-%d")   #used
  master$Fuel.Type.Required[master$Fuel.Type.Required == ""] <- NA                       #used
  
  master$Garaging.Address[master$Garaging.Address == " "] <- NA              #used
  master$Garaging.Address.Line1[master$Garaging.Address.Line1 == ""] <- NA   #used
  master$Garaging.Address.Line2[master$Garaging.Address.Line2 == ""] <- NA   #used
  master$Garaging.City[master$Garaging.City == ""] <- NA                     #used
  master$Garaging.State.Province[master$Garaging.State.Province == ""] <- NA #used
  master$GE.On.Road.Date <- as.Date(master$GE.On.Road.Date, format = "%Y-%m-%d")   #used
  
  master$Location[master$Location == ""] <- NA                                #used
  #master$License.Plate.Number[master$License.Plate.Number == "."] <- NA       #
  
  master$Off.Road.Date <- as.Date(master$Off.Road.Date, format = "%Y-%m-%d")  #used
  master$Original.On.Road.Date <- as.Date(master$Original.On.Road.Date, format = "%Y-%m-%d")  #used
  master$Organization[master$Organization == ""] <- NA                        #used
  
  master$Post.Warranty.Recovery[master$Post.Warranty.Recovery == "Yes"] <- "Y"    #used
  master$Post.Warranty.Recovery[master$Post.Warranty.Recovery == "No"] <- "N"     #used
  master$Purchase.Transaction.Date <- as.Date(master$Purchase.Transaction.Date, format = "%Y-%m-%d")  #used
  
  master$Service.Detail.Description[master$Service.Detail.Description == ""] <- NA  #used
  master$Sold.Date <- as.Date(master$Sold.Date, format = "%Y-%m-%d")                #used
  master$Supplier[master$Supplier == ""] <- NA                                      #used
  
  master$Transaction.Type.Group[master$Transaction.Type.Group == ""] <- NA          #used
  
  master$Upfit[master$Upfit == "."] <- NA      #used
  
  master$Vehicle.Mailing.City[master$Vehicle.Mailing.City == ""] <- NA                        #used
  master$Vehicle.Mailing.State.Province[master$Vehicle.Mailing.State.Province == ""] <- NA    #used
  #master$Vehicle.Mailing.State.Province[master$Vehicle.Mailing.State.Province == "   "] <- NA #
  
  master$Region.Name[master$Region.Name == ""] <- NA    #used
 
  
  #==================================================================================================================
  # DQ ISSUES AND ADDING VARIABLES TO THE MASTER DATA AND TIME PERIOD SELECTION
  #==================================================================================================================
                    
  # FIELDS SUMMARY ADDITION:
  
  # DQ_Issue: 1 yes, 0 for no
  
  # Abs.Maintenance.Amount: Absolute value of Maintenance.Amount
  # Cat.Maintenance.Amount: NEGATIVE, POSITIVE, ZERO depending on the value of Maintenance.Amount
  
  # UsedVehicle:   If Original.On.Road.Date < GE.On.Road.Date then 1 (i.e. used car) else 0
  # Note that we demonstrated that there are 7 vehicles for which both Original.On.Road.Date and GE.On.Road.Date are null 
  # Death: Alive or Dead on the basis of Off.Road.Date
  # Note that we are basically assuming that all cars that dont have it are alive...
  # End.Obs: usually this is the Off.Road.Date unless it is null for which we put EndOfStudyPeriod
  
  # Age: Age of the vehicle if still alive. If dead then the time the vehicle lived
  # TransAge: Age of the vehicle at the time of the transaction (Purchase.Transaction.Date - Original.On.Road.Date)
  
  
  # COPY OF THE DATA IN CASE IS NEDED - WE REMOVE RECORDS DUE TO DQ ISSUES ---------------------------------------- 
  Orig.Master <- master
  
  # STUDY PERIOD END DATE -----------------------------------------------------------------------------------------
  # This data has been derived by Yanika as the latest completed month with transactions
  # This is used to derive other variables
  EndOfStudyPeriod <- as.Date("2016-05-31")
  

  # DEFINE STUDY PERIOD -------------------------------------------------------------------------------------------
  # DATES SELECTION
  selection <- c(
    #"Current.Odometer.Reading.Date", # RUBBISH -- DQ ISSUES
    "Factory.Order.Date",            # ?
    "Original.On.Road.Date",         # IMPORTANT
    "GE.On.Road.Date",               # IMPORTANT
    "Purchase.Transaction.Date",     # IMPORTANT
    "Off.Road.Date",                 # IMPORTANT
    "Sold.Date"                      # ?
  )
  # SELECT DATA FRAME
  df <- master[,selection]
  # CALCULATIONS 
  MAX <- apply(df, 2, function(x)max(x[!is.na(x)]))
  MIN <- apply(df, 2, function(x)min(x[!is.na(x)]))
  NAs <- apply(df, 2, function(x)sum(is.na(x)))
  # CALULATE TIME PERIOD OF DATA 
  t.min <- as.Date(min(MIN))
  t.max <- as.Date(max(MAX))    
  
  # DEFINE DQ ISSUES VECTORS ---------------------------------------------------------------------------------------  
  
  # DQ ISSUES FOR DATES
  master$DQ_OrOnroadDate              <- ifelse(is.na(master$Original.On.Road.Date),1,0)
  master$DQ_GEOrOnroadDate            <- ifelse(is.na(master$GE.On.Road.Date),1,0)
  master$DQ_PurchaseTransDate         <- ifelse(is.na(master$Purchase.Transaction.Date),1,0)
  master$DQ_TransOdomClean            <- ifelse(is.na(master$cleaned_miles),1,0)  
  
  # LATEST PURCHASE TRANSACTION > OFF ROAD DATE? (THIS WOULD BE EQUIVALENT TO MISSING TRANSACTIONS)
  df <- master[!is.na(master$Off.Road.Date),]
  TDimensions <- paste(as.character(c("ID",
                                      "GE.Unit",
                                      "Off.Road.Date")
                                    ,collapse=","))       
  df <- setDT(df)[,list(
    TransaDate= max(Purchase.Transaction.Date),
    Cost      = sum(abs(Maintenance.Amount))
  ),by= TDimensions]
  
  df <- data.frame(df)
  df$Diff <- df$Off.Road.Date - df$TransaDate
  df <- df[df$Diff < 0,]
  df <- data.frame(df)
  
  # OFF ROAD DATE BEYOND STUDY PERIOD LIMIT DATE?
  df <- master[!is.na(master$Off.Road.Date),]
  df <- data.frame(df)
  df$Diff <- EndOfStudyPeriod - df$Off.Road.Date
  df <- df[df$Diff < 0,]
  df <- data.frame(df)
  length(unique(df$GE.Unit)) # 58
  
  # write.csv(table(df$Diff[!duplicated(df$GE.Unit)])  , "Vehicles_OffRoad_GT_Study period.csv")
  
  # FLAG RECORDS WITH ISSUES
  master$DQ_Transc_GT_OffRoad <- ifelse(master$ID %in%  df$ID, 1,0)
  
  # NOW AGGREGATE TO SUMMARISE ISSUES
  TDimensions <- paste(as.character("Diff",collapse=","))       
  Out <- setDT(df)[,list(
    Cost      = sum(abs(Maintenance.Amount)), 
    Vehicles  = length(unique(GE.Unit)), 
    Transact  = length(ID) 
  ),by= TDimensions] 
  Out <- data.frame(Out)
  Out <- Out[with(Out, order(Diff)), ]
  
  write.csv(Out,"DQ_Purchase_VS_OffRoad.csv")
  
  # CREATE VARIABLE OF TRANSACTIONS ASSOCIATED WITH CARS WITH ISSUES
  master$DQ_TransacsIssues <- ifelse(master$GE.Unit %in% (unique(master$GE.Unit[
    master$DQ_OrOnroadDate == 1 |
      master$DQ_GEOrOnroadDate == 1 |
      master$DQ_PurchaseTransDate == 1 |
      master$DQ_TransOdomClean == 1 |
      master$DQ_Transc_GT_OffRoad == 1
    ])),1,0)
  
  # SUMMARY OF DQ ISSUES   
  TDimensions <- paste(as.character(c("DQ_OrOnroadDate",
                                      "DQ_GEOrOnroadDate",
                                      "DQ_PurchaseTransDate",
                                      "DQ_TransOdomClean",
                                      "DQ_Transc_GT_OffRoad",
                                      "DQ_TransacsIssues"
  ),
  collapse=",") 
  )       
  Out <- setDT(master)[,list(
    Cost      = sum(abs(Maintenance.Amount)), 
    Vehicles  = length(unique(GE.Unit)), 
    Transact  = length(ID)
  ),by= TDimensions] 
  Out <- data.frame(Out)
  Out <- Out[with(Out, order(-Transact)), ]
  
  write.csv(Out,"DQ_SummaryIssues.csv")      
  
  
  # SOLVING DQ ISSUES 
  master <- data.frame(master)
  dq <- data.frame(matrix(ncol = 3, nrow = 4))
  names(dq) <- c("Vehicles","Transactions","Costs")
  
  row.names(dq)[1] <- "ORIGINAL DATA"
  dq[1,1] <- length(unique(master$GE.Unit))
  dq[1,2] <- dim(master)[1]
  dq[1,3] <- sum(abs(master$Maintenance.Amount)) 
  
  
  # REMOVE TRANSACTIONS/VEHICLES ASSOCIATED WITH ISSUES IN ORIGINAL/GE ON ROAD DATE
  master <- master[
    !(master$GE.Unit %in% unique(master$GE.Unit[
      master$DQ_OrOnroadDate == 1 |
        master$DQ_GEOrOnroadDate == 1 
      ])),
    ]
  row.names(dq)[2] <- "REMOVE TRANSACTIONS/VEHICLES ASSOCIATED WITH ISSUES IN ORIGINAL/GE ON ROAD DATE"
  dq[2,1] <- length(unique(master$GE.Unit))
  dq[2,2] <- dim(master)[1]
  dq[2,3] <- sum(abs(master$Maintenance.Amount)) 
  
  # REMOVE TRANSACTION WITH MILLEAGE OR TRANSACTION DATE ISSUE 
  master <- master[!(master$DQ_PurchaseTransDate == 1 |	master$DQ_TransOdomClean ==1 ),]
  row.names(dq)[3] <- "REMOVE TRANSACTION WITH MILLEAGE OR TRANSACTION DATE ISSUE"
  dq[3,1] <- length(unique(master$GE.Unit))
  dq[3,2] <- dim(master)[1]
  dq[3,3] <- sum(abs(master$Maintenance.Amount)) 
  
  
  # REMOVE TRANSACTIONS WITH TRANSACTION DATE LARGER THAN OFF ROAD DATE 
  # MAKE SURE THAT YOU SELECT THE TRANSACTIONS FOR WHICH WE HAVE OFF ROAD DATE (I.E. DEAD)
  df <- master[!is.na(master$Off.Road.Date),]
  df <- master[df$Off.Road.Date -	df$Purchase.Transaction.Date < 0,]
  
  master <- master[which(!(master$ID %in% df$ID)),]
  row.names(dq)[4] <- "REMOVE TRANSACTIONS WITH TRANSACTION DATE LARGER THAN OFF ROAD DATE"
  dq[4,1] <- length(unique(master$GE.Unit))
  dq[4,2] <- dim(master)[1] 
  dq[4,3] <- sum(abs(master$Maintenance.Amount)) 
  
  write.csv(dq,"dq.csv")
  
  # INFORM USER WHERE OF THE RECORD REMOVAL
  paste("AFTER THE REMOVAL OF DQ ISSUES, THE NEW DATA FRAME COMPRISES ",
        dim(master)[1]," RECORDS",
        sep = ""
  )       
  
  # CONVERT DATES INTO MONTHLY VALUES -------------------------------------------------------------------------------
  # master$Od.Read.Month <- as.Date(cut(master$Current.Odometer.Reading.Date,breaks = "month"))
  master$GE.On.Road.Date.Month <- as.Date(cut(master$GE.On.Road.Date,breaks = "month"))
  master$Off.Road.Date.Month <- as.Date(cut(master$Off.Road.Date,breaks = "month"))  
  master$Purchase.Transaction.Date.Month <- as.Date(cut(master$Purchase.Transaction.Date,breaks = "month"))    
  master$Original.On.Road.Date.Month <- as.Date(cut(master$Original.On.Road.Date,breaks = "month"))    
  
  # CATEGORICAL VARIABLE FOR MAINTENANCE AMOUNT ---------------------------------------------------------------------
  master$Cat.Maintenance.Amount <- ifelse(master$Maintenance.Amount < 0 ,"NEGATIVE",
                                          ifelse(master$Maintenance.Amount > 0, "POSITIVE",
                                                 "ZERO")
  )
  master$Abs.Maintenance.Amount <- abs(master$Maintenance.Amount) 
  
  
  # CREATION OF NEW FIELD  ------------------------------------------------------------------------------------------
  master$UsedVehicle <- ifelse(master$Original.On.Road.Date - master$GE.On.Road.Date < 0, 1, 
                               ifelse(master$Original.On.Road.Date == master$GE.On.Road.Date ,0,NA)
  )
  
  # sum(master$UsedVehicle)   # 5738
  # sum(master$UsedVehicle[!duplicated(master$GE.Unit)]) # 64
  
  # NEW VARIABLE Death: IS THE VEHICLE ALIVE?------------------------------------------------------------------------  
  master$Death <- ifelse(is.na(master$Off.Road.Date.Month),
                         0,
                         1
  )
  
  # sum(master$Death)   # 27620
  # sum(master$Death[!duplicated(master$GE.Unit)]) # 486
  
  # NEW VARIABLE End.Obs: END OF THE OBSERVATION PERIOD--------------------------------------------------------------               
  master$End.Obs <- as.Date(ifelse(is.na(master$Off.Road.Date),
                                   EndOfStudyPeriod,
                                   as.Date(master$Off.Road.Date)
  )
  )
  
  # NEW VARIABLE Time: Age ------------------------------------------------------------------------------------------
  master$Age  <- as.numeric(difftime(master$End.Obs, 
                                     master$Original.On.Road.Date,
                                     units="days"
  )
  ) 
  master$Age <- master$Age / 30.41667 # NOTE THAT WE TRANSAFORM ON A MONTHLY BASIS
  
  # NEW VARIABLE: TransactionAge-------------------------------------------------------------------------------------
  master$TransAge  <- as.numeric(difftime(master$Purchase.Transaction.Date, 
                                          master$Original.On.Road.Date,
                                          units="days"
  )
  )  
  master$TransAge <- master$TransAge / 30.41667 # NOTE THAT WE TRANSAFORM ON A MONTHLY BASIS
  
  # NEW VARIABLE Time: Mileage --------------------------------------------------------------------------------------
  # OBTAIN LARGEST MILEAGE FOR EACH VEHICLE
  TDimensions <- paste(as.character(c("GE.Unit"
  ),
  collapse=",") 
  )       
  df <- setDT(master)[,list(
    Mileage   = max(Trans.Odometer.Clean)
  ),by= TDimensions] 
  master <- data.frame(master)
  
  master <- merge(master,
                  df,
                  by.x = "GE.Unit",
                  by.y = "GE.Unit",
                  all.x= TRUE
  )
  
  # SOME GENERAL INFORMATION  
  max(master$Off.Road.Date,na.rm = TRUE)  # "2016-10-13"
  min(master$Off.Road.Date,na.rm = TRUE)  # "2014-04-03"
  max(master$Purchase.Transaction.Date,na.rm = TRUE)  # "2016-06-16"
  
  
  
#==================================================================================================================
# EXPLANATORY DATA ANALYSIS
#==================================================================================================================
# THE PURPOSE OF THIS SECTION IS TO CARRY OUT EDA ON THE DATA. THIS SECTION REFERS TO FUNCTIONS IN OTHER FILES.
# PLEASE FIND AN EXAMPLE RUNNING THROUGH THE CODE BELOW. 
  
  # GENERATE FREQUENCY MATRIX FOR VARIABLE OF INTEREST
  # EXAMPLE: GENERATE FREQ TABLE FOR NUMBER OF VEHICLES SPLIT BY USAGE.TYPE AND MAKE. THE OUTPUT WILL GIVE AN
  # IDEA ABOUT THE NUMBERS BEING DEALTH WITH.
  
  var <- c("Usage.Type","MAKE")
  df <- master[!duplicated(master$GE.Unit),]
  df <- table(df[,var])
  write.csv(df,paste("Matrix - ",var,".csv"))
  
  
  # USAGE ANALYSIS. GENERATE PLOTS OF THE DATA ANALYSED FROM THE POINT OF VIEW OF MILEAGE AND AGE.
  # EXAMPLE: GENERATE PLOTS FOR DATA SPLIT BY USAGE.TYPE AND MAKE. THE DATA USED ONLY CONSIDERS 
  # VEHICLES WHICH HAVE CATEGORY USAGE.TYPE = LIGHT TRUCK AND MAKE = CHVRL OR MRCBN
  
  SummaryUsage(
    CarIdentf = c("GE.Unit"),
    Variables = c("Usage.Type","MAKE"),
    BaseData  = master[master[,"Usage.Type"] %in% c("Light Truck") & master[,"MAKE"] %in% c("CHVRL","MRCBN"),],
    LimitsAge = c(200,500),
    FileName  = "MyNewFile"
  )  
  
  
  vehicle[,"Usage.Type"] %in% c("Light Truck") & vehicle[,"MAKE"] %in% c("CHVRL","MRCBN")
  
  
  # TEMPORAL ANALYSIS. GENERATE PLOTS OF THE DATA ANALYSED FROM A TEMPORAL POINT OF VIEW. 
  # EXAMPLE: GENERATE PLOTS FOR DATA SPLIT BY USAGE.TYPE.
  
  #FOR var.interest, CHOOSE BETWEEN "Transactions", "Vehicles", "Death", "Abs.Maintenance.Amount", AND "Original.On.Road.Date.Month"
  Temporal.Plots(var.interest = c("Transactions"),
                 cat.interest = c("Usage.Type"),
                 database.var = master,
                 limit.time = 20,
                 limit.cat = 200
  )
  
  
  # TODAY ANALSYSIS. GENERATE HISTOGRAM PLOTS OF THE DATA AS IT CURRENTLY STANDS I.E. ONLY CONSIDER A SLICE OF TIME. 
  
  #FOR var.interest, CHOOSE BETWEEN "Age" AND "Mileage"
  Histogram.Generation(var.interest = c("Age"),
                       database = master)
  
  
  
  Frequency.Table(var.interest = c("Usage.Type"),
                  database = master)
  
  

  

#==================================================================================================================
# CREATION OF VEHICLE LEVEL DATA SET
#==================================================================================================================
# DICTIONARY 
# GE.On.Road.Date.Month: GE.On.Road.Date at a monthly scale
# Off.Road.Date.Month::: Off.Road.Date at a monthly scale
# Death::::::::::::::::: Is the vehicle dead? (1 = dead, 0 = alive) i.e. there is an available Off.Road.Date
# End.Obs::::::::::::::: Date end of observation period. If the vehicle is dead then it is the Off.Road.Date else it is ("2016-05-31")
# Time:::::::::::::::::: Time that a vehicle has been observed. Difference between GE.On.Road.Date and End.Obs, in weeks
# Trans.Odometer.Clean:: Milleage data
# purchase.transaction.date::::: Date in which the transaction was taken (to be used in conjunction with Trans.Odometer.Clean)
# Current.Odometer.Reading.Date: DONT USE THIS!!!!!!!!!!!!!!!!!!!


# CREATE TABLE OF UNIQUE CARS SELECTING FOR INTRISIC CAR PROPERTIES. 
# INTRINSIC PROPERTIES WERE DEDUCED IN THE SECTION "CHECKING TRANSACTIONALITY OF DATA"
  selection <- c("GE.Unit",
                 "Company",
                 "Capital.Cost",
                 #"Current.Odom.Edited",
                 #"Current.Odometer.Reading.Date",
                 "Contract.Type",
                 "Fuel.Type.Required",
                 "Factory.Order.Date",
                 "Garaging.Address",
                 "Garaging.Address.Line1",
                 "Garaging.Address.Line2",
                 "Garaging.City",
                 "Garaging.State.Province",
                 "Garaging.Zip.Postal.Code",
                 #"License.Plate.Number",
                 #"License.State..Province",
                 "Location",
                 "MAKE",
                 "Model",
                 "Model.Description",
                 "Model.Year",
                 "Organization",
                 "PM.Threshold.Miles",
                 "Payload.SUT",
                 "Sold.Date",
                 "Upfit",
                 "Usage.Type",
                 "Unit.Status.Group",
                 "Vehicle.Mailing.City",
                 "Vehicle.Mailing.Zip.Code",
                 "Vehicle.Mailing.State.Province",
                 
                 "Original.On.Road.Date",
                 "GE.On.Road.Date",
                 "Off.Road.Date",   
                 "Original.On.Road.Date.Month",      # NEW CREATED VARIABLE
                 "GE.On.Road.Date.Month",            # NEW CREATED VARIABLE
                 "Off.Road.Date.Month",              # NEW CREATED VARIABLE
                 
                 "UsedVehicle", # FROM PREVIOUS CALCULATIONS
                 "Death", # FROM PREVIOUS CALCULATIONS
                 "End.Obs", # FROM PREVIOUS CALCULATIONS
                 "Age",  # FROM PREVIOUS CALCULATIONS - THIS DRAWS ON THE ON ROAD VS OFF ROAD DIF/STUDY PERIOD DATE
                 "Mileage"
                 
  )
  

  vehicle <- master[!duplicated(master$GE.Unit),selection]
 
 
  #NEW VARIABLE TO REPRESENT TOTAL SUM OF COSTS FOR EACH GE.UNIT
  
  vehicle$Total.Cost <- apply(as.data.frame(vehicle$GE.Unit), 1, function(x) sum(master$Maintenance.Amount[master[,"GE.Unit"] == x]))
  
  head(vehicle$Total.Cost)
  
  #NEW VARIABLE TO REPRESENT TOTAL SUM OF COSTS PER ACQUISITION FOR EACH GE.UNIT
  
  #vehicle$Cost.Over.Acquisition <-  vehicle$Total.Cost / vehicle$Capital.Cost
  #vehicle$Cost.Over.Acquisition[vehicle$Cost.Over.Acquisition == Inf] <- NA 
  
  #NEW VARIABLE TO REPRESENT TOTAL SUM OF COSTS PER ACQUISITION, NORMALIZED BY WEEKS ACTIVE FOR EACH GE.UNIT
  #vehicle$Cost.Over.Acquisition.Normal <-  vehicle$Cost.Over.Acquisition / vehicle$Time
  
  #head(vehicle$Cost.Over.Acquisition)
  

#==================================================================================================================
# CREATION OF TIME LEVEL DATA SET 
#==================================================================================================================
  
  # CREATE STRUCTURE ON THE BASIS OF THE MIN GE.On.Road.Date.Month AND TODAY
  temporal <- data.frame(seq(t.min,t.max,by ="month"))
  names(temporal) <- "Oc.Date"
  
  # ADD NUMBER OF VEHICLES LIVE AT A GIVEN TIME STEP
  # CREATE VARIABLES IN temporal DATAFRAME
  temporal$Vehicles <- 0
  
  # LOOP THROUGH ALL DATES IN temporal
  for (i in 1:dim(temporal)[1]) {
    # SELECT THE DATE 
    d <- temporal$Oc.Date[i]
    selection <-  d >= vehicle$GE.On.Road.Date.Month &   
      (d <= vehicle$Off.Road.Date.Month | is.na(vehicle$Off.Road.Date.Month))
    # ADD DATA TO DATAFRAME
    temporal$Vehicles[i] <- dim(vehicle[selection,])[1]     
  } # End loop for
  
  # ADD THE NUMBER OF VEHICLES PUT ON THE ROAD AT A GIVEN MONTH (OnRoad)
  df <- aggregate(GE.Unit ~  GE.On.Road.Date.Month, FUN = length,data = vehicle)
  nc <- dim(temporal)[2]
  temporal<- merge(temporal,
                   df,
                   all.x = TRUE,
                   by.x = "Oc.Date",
                   by.y = "GE.On.Road.Date.Month"
  )
  names(temporal)[nc + 1] <- "OnRoad" 
  temporal[is.na(temporal$OnRoad),"OnRoad"] <- 0
  
  # ADD THE NUMBER OF VEHICLES THAT DIE ON A GIVEN MONTH (OffRoad)
  df <- aggregate(GE.Unit ~  Off.Road.Date.Month, FUN = length,data = vehicle)
  nc <- dim(temporal)[2]
  temporal<- merge(temporal,
                   df,
                   all.x = TRUE,
                   by.x = "Oc.Date",
                   by.y = "Off.Road.Date.Month"
  )
  names(temporal)[nc + 1] <- "OffRoad" 
  temporal[is.na(temporal$OffRoad),"OffRoad"] <- 0   
  

#==================================================================================================================
# SURVIVAL ANALYSIS
#==================================================================================================================
# KEY PLAYERS
# vehicle:: Dataset at vehicle level    
# Age:::::: Time to death or on-study time, weeks
# Mileage:: Mileage to death or on-study mileage
# Death:::: Death indicator (0=alive, 1=dead) 


# CREATE SURVIVAL OBJECT AND OVERALL PERFORMANCE ===================================================================  
# Converting into a Surv object 
  SvAge <- Surv(vehicle$Age, vehicle$Death)
  SvMil <- Surv(vehicle$Mileage, vehicle$Death)

# GET THE OVERAL KM ESTIMATOR
  SvAge = npsurv(SvAge ~ 1)
  SvMil = npsurv(SvMil ~ 1)

  x11(13,5)
  par(oma=c(0,0,0,0),mar=c(4,4,1,1) + 0.1,cex.main=0.85,cex.axis=0.8)
  par(mfrow=c(1,2))
  
  survplot(SvAge,
           #conf = c("none","bands","bars")[2],
           xlab = "", ylab = "Survival for AGE",
           ## xlim(0,100),
           label.curves = TRUE,                     # label curves directly
           ## label.curves = list(keys = "lines"),  # legend instead of direct label
           levels.only  = FALSE,                    # show only levels, no label
           abbrev.label = FALSE,                    # if label used, abbreviate
           ## fun = function(x) {1 - x},            # Cumulative probability plot         
           loglog   = FALSE,                        # log(-log Survival) plot
           logt     = FALSE,                        # log time
           time.inc = 100,                          # time increment
           dots     = FALSE,                        # dot grid
           n.risk   = TRUE,                         # show number at risk
           ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
           y.n.risk = -0.2, 
           cex.n.risk = 0.7
           #col = p.col
  )
  
  survplot(SvMil,
           #conf = c("none","bands","bars")[2],
           xlab = "", ylab = "Survival for MILEAGE",
           ## xlim(0,100),
           label.curves = TRUE,                     # label curves directly
           ## label.curves = list(keys = "lines"),  # legend instead of direct label
           levels.only  = FALSE,                    # show only levels, no label
           abbrev.label = FALSE,                    # if label used, abbreviate
           ## fun = function(x) {1 - x},            # Cumulative probability plot         
           loglog   = FALSE,                        # log(-log Survival) plot
           logt     = FALSE,                        # log time
           time.inc = 40000,                          # time increment
           dots     = FALSE,                        # dot grid
           n.risk   = TRUE,                         # show number at risk
           #srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
           y.n.risk = -0.2, 
           cex.n.risk = 0.7
           # col = p.col
  )
  
  glance(SvAge)  
  glance(SvMil)  

# HAZARD FUNCTION ==================================================================================================
  #NOT FUNCTIONING
  H.hat.age <- -log(SvAge$surv); 
  H.hat.age <- c(H.hat.age, H.hat.age[length(H.hat.age)])
  
  
  H.hat.mile <- -log(SvMil$surv); 
  H.hat.mile <- c(H.hat.mile, H.hat.mile[length(H.hat.mile)])
  
  x11(13,5)
  par(oma=c(0,0,0,0),mar=c(4,4,1,1) + 0.1,cex.main=0.85,cex.axis=0.8)
  par(mfrow=c(1,2))
  
  
  plot(SvAge$time, 
       H.hat.age[-length(H.hat.age)],  
       xlab="time (months)", 
       ylab="cumulative hazard for AGE",
       main="comparing cumulative hazards", 
   #    ylim=range(c(H.hat, H.tilde)), 
       type="s"
  )
  
  
  plot(SvMil$time, 
       H.hat.mile[-length(H.hat.mile)], 
       xlab="mileage", 
       ylab="cumulative hazard for MILEAGE",
       main="comparing cumulative hazards", 
       #    ylim=range(c(H.hat, H.tilde)), 
       type="s"
  )


  
  
  #########################################################################
  #########################################################################
  # CREATE SURVIVAL OBJECT AND OVERALL PERFORMANCE BY CATEGORY===================================================================

  #var <- c("Usage.Type","MAKE")
  #df <- vehicle
  
  #data.frame(table(master[,var]))
  
  
  df <- vehicle[vehicle[,"Usage.Type"] %in% c("Light Truck") & vehicle[,"Company"] %in% c("  RBDC","  RBNA") & 
                  vehicle[,"MAKE"] %in% c("CHVRL","MRCBN"),]
  #limit <- 40

# DATA STUFF  
#  df2 <- data.frame(table(vehicle[c("Usage.Type","MAKE")]))
#  df2$Var1 <- as.character(df2$Var1)
#  leg.label <- df2[df2$Freq>limit,1]
  
# DATA STUFF  
  df2 <- data.frame(table(df[c("Usage.Type","Company","MAKE")]))
  df2$Var1 <- do.call(paste, c(df2[,c("Usage.Type","Company","MAKE")], sep = "_")) 
  leg.label <- df2[,"Var1"]
  
  #df <- df[df$Dis %in% df[,1], ]
  
  
# Converting into a Surv object 
  SvAge <- Surv(df$Age, df$Death)
  SvMil <- Surv(df$Mileage, df$Death)

# GET THE OVERAL KM ESTIMATOR
  SvAge = npsurv(Surv(time = Age, event = Death) ~ Usage.Type + Company + MAKE, 
         data = df
  )
  SvMil = npsurv(Surv(time = Mileage, event = Death) ~ Usage.Type + Company + MAKE, 
                 data = df
  )

  summary(SvAge)
  summary(SvMil)
  
# colours  
  p.lab <- names(SvAge$strata)
  nc <-length(p.lab)
  p.col <- rainbow(length(p.lab),alpha=0.65)
  
  x11(10,6)
  par(oma=c(0,0,0,0),mar=c(4,4,2,1) + nc/10,cex.main=0.85,cex.axis=0.8)
  par(mfrow = c(1,2))
  
  survplot(SvAge,
          #conf = c("none","bands","bars")[1],
          xlab = "", ylab = "Survival for AGE",
          ## xlim(0,100),
          label.curves = TRUE,                     # label curves directly
          #label.curves = list(unique(df$Usage.Type)),  # legend instead of direct label
          levels.only  = FALSE,                    # show only levels, no label
          abbrev.label = FALSE,                    # if label used, abbreviate
          ## fun = function(x) {1 - x},            # Cumulative probability plot         
          loglog   = FALSE,                        # log(-log Survival) plot
          logt     = FALSE,                        # log time
          time.inc = 100,                          # time i
          increment
          dots     = FALSE,                        # dot grid
          n.risk   = TRUE,                         # show number at risk
          ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
          y.n.risk = -0.2, 
          cex.n.risk = 0.7,
          sep.n.risk = 0.025,
          col = p.col
   )
  
 # x11(5,5)
  #par(oma=c(0,0,0,0),mar=c(4,4,2,1),cex.main=0.85,cex.axis=0.8) 
  survplot(SvMil,
           #conf = c("none","bands","bars")[1],
           xlab = "", ylab = "Survival for MILEAGE",
           ## xlim(0,100),
           label.curves = TRUE,                     # label curves directly
           ##label.curves = list(keys = "lines"),  # legend instead of direct label
           levels.only  = FALSE,                    # show only levels, no label
           abbrev.label = FALSE,                    # if label used, abbreviate
           ## fun = function(x) {1 - x},            # Cumulative probability plot         
           loglog   = FALSE,                        # log(-log Survival) plot
           logt     = FALSE,                        # log time
           time.inc = 40000,                        # time increment
           dots     = FALSE,                        # dot grid
           n.risk   = TRUE,                         # show number at risk
           ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
           y.n.risk = -0.2, 
           cex.n.risk = 0.7,
           sep.n.risk = 0.025,
           col = p.col
  )  
 
  #survdiff(Surv(time = Age, event = Death) ~ MAKE,data=df)
  # 1 - pchisq(SvMil$chisq, 1)
  
# Summary
  x11(10,6)
  br = seq(0,700,50)
  death <- df[df$Death==1,]
  alive <- df[df$Death==0,]
  barplot(t(table(cut(death$Age, breaks = br),death$MAKE)))
  barplot(t(table(cut(alive$Age, breaks = br),alive$MAKE)))
 
  
# PRODUCE SEMIPARAMETRIC COX ======================================================================================
# NOTE THAT THE CONFIDENCE INTERVAL DOES NOT CONTAIN THE 0

  Vehicle_coxph = coxph(Surv(time = Age, event = Death) ~ Usage.Type + Company + MAKE, 
                        data = df
  )
  summary(Vehicle_coxph)
  Vehicle_coxph_tidy = tidy(Vehicle_coxph)
  print(Vehicle_coxph_tidy) 
  glance(Vehicle_coxph) 
  
  write.csv(Vehicle_coxph_tidy, "Vehicle_coxph_tidy.csv")

# VALIDATING COX PH ASSUMPTION
# The proportional hazards assumption essentially means the coefficient remains constant over time. 
# So, if we can find correlation between time and the coefficient and test if that's significant or not, 
# we'll able to test the proportional hazards assumption.
# To test this assumption we can use cox.zph function. It tests correlation between Schoenfeld residuals 
# and (transformed) time using a chi-square test. For this test, p-value less than certain threshold (say 0.5) 
# would imply the correlation between the residuals and (transformed) time is significant and proportional hazards 
# assumption does not hold. (More info on schoenfeld residual
  validate_coxph = cox.zph(Vehicle_coxph, 
                           transform = "km" # KAPLAN MEIER TRANSFORMATION
  )
  validate_coxph
  

# the very low p value does not look encouraging....

# Schoenfeld residuals. Ideally for PH assumption to hold, this should be a flat straight line. 
# (Like any other residuals, these residuals should exhibit random pattern. )  
  x11()
  par(mfrow=c(2, 2))
  plot(validate_coxph)
  abline(h=0)

# INFLENTIAL OBSERVATION
   dfbeta <- residuals(Vehicle_coxph, type="dfbeta")
   #par(mfrow=c(2, 2))
   for (j in 1:3) {
   plot(dfbeta[, j], ylab=names(coef(validate_coxph))[j])
   abline(h=0, lty=2)
   }
  
# NON-LINEARITY ASSUMPTION
   x11()
   par(mfrow=c(2, 2))
   
   res <- residuals(Vehicle_coxph, type="martingale")
   X <- as.matrix( df[, c("MAKE")]) # matrix of covariates
   par(mfrow=c(2, 2))
   for (j in 1:2) { # residual plots
     plot(X[, j], res, xlab=c("MAKE")[j], ylab="residuals")
     abline(h=0, lty=2)
     lines(lowess(X[, j], res, iter=0))
     }
   b <- coef(mod.allison.4)[c(2,3)] # regression coefficients
   for (j in 1:2) { # component-plus-residual plots
     plot(X[, j], b[j]*X[, j] + res, xlab=c("age", "prio")[j],
            ylab="component+residual")
        abline(lm(b[j]*X[, j] + res ~ X[, j]), lty=2)
      lines(lowess(X[, j], b[j]*X[, j] + res, iter=0))
      }
  
   
#======   
# OTHER MODELS - FUTURE WORK
  #exponential <- survreg(Surv(Age,Death) ~ MAKE,data = df, dist = "exponential")
  #summary(exponential)
  #weibull <- survreg(Surv(Age,Death) ~ MAKE,data = df, dist = "weibull")
  #summary(weibull)
  #loglogistic <- survreg(Surv(Age,Death) ~ MAKE,data = df, dist = "loglogistic")
  #summary(weibull)
  
  
