
###################################################################################################################
#
#                                         Author: Yanika Borg
#                                         Date: 17-Oct-2016
#                                         Project: Fleet Analysis
#
###################################################################################################################

#==================================================================================================================
# CODE INDEX
#==================================================================================================================

# FREQUENCY.TABLE FUNCTION
# HISTOGRAM.GENERATION FUNCTION







#==================================================================================================================
# FREQUENCY.TABLE FUNCTION
#==================================================================================================================
#THIS FUNCTION SERVES TO CALCULATE THE NUMBER AND COST OF TRANSACTIONS BY VARIABLE OF INTEREST. IN ADDITION, IT
#CALCULATES THE PERCENTAGE OF THE TOTAL FOR EACH LEVEL OF INTEREST

#INPUTS:
#-------var.interest: THE VARIABLE OF INTEREST (EG. "MAKE", "Company")
#-------database::::: THE DATASET FROM WHICH TO EXTRACT THE DATA (EG. master, master[!is.na(master$Purchase.Transaction.Date),])

#Frequency.Table(var.interest = c("MAKE"),
#                database = master)

Frequency.Table <- function(var.interest = NULL,
                           database = NULL
){
      #GENERATE TABLE WITH TRANSACTION & COST BREAKDOWN
  
      #DATA PREP  
      var_interest <- var.interest
      database <- database  
          
      
      #BUILD DF WITH INFO ON NUMBER & PERCENTAGE OF TRANSACTIONS
      df <- data.frame(table(database[var.interest]))
      a <- sum(df[,2])
      df$Perc <- apply(as.data.frame(df[,2]),1,function(x) x/a)
      
      
      #BUILD DF WITH INFO ON SUM & PERCENTAGE OF TRANSACTION COSTS
      df.2 <- aggregate(Maintenance.Amount ~ eval(parse(text=var.interest)),
                        FUN =sum,
                        data = database)
      a2 <- sum(df.2[,2])
      df.2$Perc <- apply(as.data.frame(df.2[,2]),1,function(x) x/a2)
      #print(df.2)
      
      #JOINING AND STORING THE DATA
      df <- merge(df,df.2, by.x="Var1", by.y=names(df.2)[1],all.x = TRUE)
      write.csv(df,
                paste("Frequency and Cost by ",var_interest,".csv"))
      
      print(df)
}



#==================================================================================================================
# HISTOGRAM.GENERATION FUNCTION
#==================================================================================================================
#THIS FUNCTION SERVES TO GENERATE THE HISTOGRAM OF THE VARIABLE OF INTEREST. IT IS A STATIC PLOT, IN THAT IT GIVES 
#US A SNAPSHOT IN MAY 2016 BASED ON ACTIVE AND DE-COMMISIONED VEHICLES. THE HISTOGRAM CAN BE GENERATED FOR THE AGE
#OR MILEAGE OF THE VEHICLE.

#INPUTS:
#-------var.interest: THE VARIABLE OF INTEREST. CAN BE EITHER "Mileage" or "Age"
#-------database::::: THE DATASET FROM WHICH TO EXTRACT THE DATA (EG. master, master[!is.na(master$Purchase.Transaction.Date),])


#Histogram.Generation(var.interest = c("Mileage"),
#                    database = master)


Histogram.Generation <- function(var.interest = NULL,
                                 database = NULL
){
      
    #PREP DATA FOR ANALYSIS
    var.interest <- var.interest
    database <- database
    
    
    #DEVELOP VEHICLE DATASET      
    vehicle <- database[!duplicated(database$GE.Unit),]
          
    #PREP DATA FOR HISTOGRAM
    cond <- vehicle$Death==1
    xmax <- max(vehicle[,var.interest],na.rm=TRUE)
    
    #PLOT
    x11()
    par(mfrow=c(2,2))
    
    hist(vehicle[,var.interest],
         col="dark blue",
         main=paste("Histogram of ",var.interest),
         xlab=var.interest,
         xlim=c(0,xmax),
         breaks=14)
    
    
    hist(vehicle[!(cond),var.interest],
         col="dark blue",
         main=paste("Histogram of ",var.interest,"on condition \n that Death=0"),
         xlab=var.interest,
         xlim=c(0,xmax),
         breaks=14)
    
    
    hist(vehicle[cond,var.interest],
         col="dark blue",
         main=paste("Histogram of ",var.interest,"on condition \n that Death=1"),
         xlab=var.interest,
         xlim=c(0,xmax),
         breaks=14)


}



