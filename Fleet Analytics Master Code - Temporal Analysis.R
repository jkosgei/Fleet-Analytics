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

# TEMPORAL.PLOTS FUNCTION







#==================================================================================================================
# TEMPORAL.PLOTS FUNCTION
#==================================================================================================================
#THIS FUNCTION DEVELOPS A STACKED BARPLOT, CUMULATIVE BAR PLOT AND NORMALIZED BAR PLOT SHOWING 1) NUMBER OF TRANSACTIONS, 
#OR 2) COST OF TRANSACTIONS, OR 3) NUMBER OF DEATHS, OR 4) NUMBER OF BOUGHT-IN VEHICLES, OR 5) NUMBER OF ACTIVE VECHILES
#OVER TIME.
#THE PLOT CAN BE SPLIT BY A NUMBER OF CATEGORIES SUCH AS MAKE, MODEL OR USAGE.TYPE. 
#
#
#
#
#
# VARIABLES TO DEFINE WHEN CALLING FUNCTION::::
# var.interest - THIS IS THE VARIABLE YOU WANT TO ANALYZE OVER TIME. OPTIONS TO SELECT FROM ARE
#                1) Abs.Maintenance.Amount 
#                2) Transactions
#                3) Original.On.Road.Date.Month
#                4) Death
#                5) Vehicles 
# cat.interest - THIS IS THE CATEGORY YOU WANT TO SPLIT YOUR VARIABLE BY DURING ANALYSIS
# data.base.var - THE DATAFRAM IN WHICH THE VAR DATA IS LOCATED. EITHER MASTER OR VEHICLE DF. IF var.interest = Abs.Maintenance.Amount
#                 OR Transactions, THEN master. Else, use Vehicles df.
# limit.time --- IN ORDER TO TIDY UP THE PLOTS, YOU NEED TO DEFINE A MINIMUM NUMBER THIS WILL BE 
#                USED WHEN DECIDING FORM WHICH DATE TO START INCLUDING EVENTS BASED ON OCCURENCE 
#                OF AT LEAST limit EVENTS
# limit.cat ---- IN ORDER TO TIDY UP THE PLOTS, YOU NEED TO DEFINE A MINIMUM NUMBER OF EVENTS REQUIRED FOR WHICH TO INCLUDE THE LEVEL
#                FOR THE CATEGORY UNDER CONSIDERATION


#EXAMPLE
#Temporal.Plots(var.interest <- c("Original.On.Road.Date.Month"),
#               cat.interest <- c("Usage.Type","Company"),
#               database.var <- master,
#               limit.time <- 20,
#               limit.cat <- 200
#)



#var.interest <- "Transactions"
#                              cat.interest <- "Product.Category"
#                              database.var <- analyse
#                              limit.time <- 0
#                              limit.cat <- 0
               

Temporal.Plots <- function(var.interest = NULL, 
                           cat.interest = NULL,
                           database.var = NULL,
                           limit.time = NULL,
                           limit.cat = NULL
                           ){

  
  ############################################################################################################
  ############################################################################################################
    #CONSTRUCTING THE TEMPORAL.CAT DATASET  
  
    #DATA PREP
    var_interest <- var.interest
    cat_interest <- c(cat.interest)
    database_var <- database.var
    limit.time <- limit.time
    limit.cat <- limit.cat
      
    
    if (length(cat_interest) > 1){
      
      
      
    ##############################################################################################################
    ##############################################################################################################
    ##############################################################################################################
    ##############################################################################################################
    ##############################################################################################################
    #FIRST IF LOOP
      
    #REMOVE ANY NA ENTRIES WRT CAT.INTEREST
    j <- which(!is.na(database_var[,cat_interest[1]]) & !is.na(database_var[,cat_interest[2]]))
    database_var <- database_var[j,]
    
    
    #REMOVE CAT ENTRIES WHICH HAVE LESS THAN limit.cat ENTRIES
    database_var$Dis <- do.call(paste, c(database_var[,cat_interest], sep = "_")) 
    df <- aggregate(ID ~ Dis, 
                    data = database_var, 
                    length)
    df <- df[df[,2] > limit.cat,]
    database_var <- database_var[database_var$Dis %in% df[,1], ]
    
    
    #DEDUCE MAX AND MIN DATE FOR ANALYSIS & PLOT
    selection <- c(
      "Factory.Order.Date",            # ?
      "GE.On.Road.Date",               # IMPORTANT
      "Off.Road.Date",                 # IMPORTANT
      "Original.On.Road.Date",         # ?
      "Purchase.Transaction.Date",     # IMPORTANT
      "Sold.Date"                      # ?
    )
    # SELECT DATA FRAME
    df <- master[,selection]
    # CALCULATIONS 
    MAX <- apply(df, 2, function(x)max(x[!is.na(x)]))
    MIN <- apply(df, 2, function(x)min(x[!is.na(x)]))
    # CALULATE TIME PERIOD FOR THE ANALYSIS  
    t.min <- as.Date(min(MIN))
    t.max <- as.Date(max(MAX))
    
    
    
    #ATTENTION CHRISTIAN
    #CALCULATE THE DIMENSIONS OF THE DF.THIS WILL HELP US DEDUCE HOW MANY LEVELS THERE ARE
    #IN OUR VARIABLE OF INTEREST AND THUS HOW MANY TIMES THE 'TIME' COMPONENT NEEDS TO BE
    #REPEATED
    if (length(cat_interest) > 1){
      repetition <- dim(unique(database_var[,cat_interest],na.rm=TRUE))[1]
    } else {
      repetition <- length(unique(database_var[,cat_interest],na.rm=TRUE))
    }
    
    #BUILD DF USING t.min, t.max AND repetition
    temporal.cat <- data.frame(rep(seq(t.min,t.max,by ="month"),each=repetition))
    names(temporal.cat) <- "Oc.Date"
    
    
    #ADD INFORMATION ABOUT CATEGORY OF INTEREST. THIS GIVES YOU THE LABELS
    temporal.cat[,cat_interest] <- unique(database_var[,cat_interest],na.rm=TRUE)
    
    #FOLLOWING THIS YOU WILL NEED TO ADD INFORMATION ABOUT THE SUM, NUMBER OF TRANSACTIONS, ETC
    #END OF ATTENTION CHRISTIAN!
    
    
    ############################################################################################################
    ############################################################################################################
    #COMPLETE temporal.cat BY FILLING IN DATA AS PER var.interest
    # LOOP THROUGH ALL DATES IN temporal.cat
    
    if (var_interest == "Abs.Maintenance.Amount" ){
        
        #CALCULATE DIM, TO BE USED IN PLOT
        dim.veh <- length(unique(database_var$GE.Unit))
        dim.trans <- dim(database_var)[1]
        
        #FILL IN temporal.cat
        for (i in 1:dim(temporal.cat)[1]) {
              # SELECT THE DATE 
              d <- temporal.cat$Oc.Date[i]
              selection <-  d == database_var$Purchase.Transaction.Date.Month & (database_var[,cat_interest[1]] == temporal.cat[i,cat_interest[1]]) & 
                          (database_var[,cat_interest[2]] == temporal.cat[i,cat_interest[2]])
              # ADD DATA TO DATAFRAME
              a <- database_var[selection, var_interest]
              a <- as.data.frame(a)
              temporal.cat[i,var_interest] <- ifelse(dim(a)[1]==0,
                                                     0,
                                                     sum(a, na.rm=TRUE))} # End loop for
    
          
    } else if (var_interest == "Transactions"){
    
        #CALCULATE DIM, TO BE USED IN PLOT
        dim.veh <- length(unique(database_var$GE.Unit))
        dim.trans <- dim(database_var)[1]
        
        #FILL IN temporal.cat
        for (i in 1:dim(temporal.cat)[1]) {
              # SELECT THE DATE 
              d <- temporal.cat$Oc.Date[i]
              selection <-  d == database_var$Purchase.Transaction.Date.Month & (database_var[,cat_interest[1]] == temporal.cat[i,cat_interest[1]]) & 
                (database_var[,cat_interest[2]] == temporal.cat[i,cat_interest[2]])
              # ADD DATA TO DATAFRAME
              temporal.cat[i,var_interest] <- dim(database_var[selection,])[1]} # End loop for
        
      
    } else if (var_interest == "Death"){
      
        #CALCULATE DIM, TO BE USED IN PLOT AND REMOVE AND NA ENTRIES WRT var.interest
        j <- which(!is.na(database_var[,"Off.Road.Date.Month"]))
        database_var <- database_var[j,]
        
        dim.veh <- length(unique(database_var$GE.Unit))
        dim.trans <- dim(database_var)[1]
        
        #GENERATE DF TO BE USED WITHIN ANALYSIS
        database_var <- database_var[!duplicated(database_var$GE.Unit),]
        
        #FILL IN temporal.cat
        for (i in 1:dim(temporal.cat)[1]) {
          # SELECT THE DATE 
          d <- temporal.cat$Oc.Date[i]
          selection <-  d == database_var$Off.Road.Date.Month & (database_var[,cat_interest[1]] == temporal.cat[i,cat_interest[1]]) & 
            (database_var[,cat_interest[2]] == temporal.cat[i,cat_interest[2]])
          # ADD DATA TO DATAFRAME
          a <- database_var[selection, var_interest]
          a <- as.data.frame(a)
          temporal.cat[i,var_interest] <- ifelse(dim(a)[1]==0,
                                                 0,
                                                 sum(a, na.rm=TRUE))} # End loop for
        
        
    } else if (var_interest == "Original.On.Road.Date.Month"){
      
    
        #CALCULATE DIM, TO BE USED IN PLOT AND REMOVE AND NA ENTRIES WRT var.interest
        j <- which(!is.na(database_var[,"Original.On.Road.Date.Month"]))
        database_var <- database_var[j,] 
        
        dim.veh <- length(unique(database_var$GE.Unit))
        dim.trans <- dim(database_var)[1]
        
        #GENERATE DF TO BE USED WITHIN ANALYSIS
        database_var <- database_var[!duplicated(database_var$GE.Unit),]
        
        
        #FILL IN temporal.cat
        for (i in 1:dim(temporal.cat)[1]) {
          # SELECT THE DATE 
          d <- temporal.cat$Oc.Date[i]
          selection <-  d == database_var$Original.On.Road.Date.Month & (database_var[,cat_interest[1]] == temporal.cat[i,cat_interest[1]]) & 
            (database_var[,cat_interest[2]] == temporal.cat[i,cat_interest[2]])
          # ADD DATA TO DATAFRAME
          a <- database_var[selection, var_interest]
          a <- as.data.frame(a)
          temporal.cat[i,var_interest] <- ifelse(dim(a)[1]==0,
                                                 0,
                                                 dim(a)[1])} # End loop for
    
      } else {
        
        
        #CALCULATE DIM, TO BE USED IN PLOT
        dim.veh <- length(unique(database_var$GE.Unit))
        dim.trans <- dim(database_var)[1]
        
        #GENERATE DF TO BE USED WITHIN ANALYSIS
        database_var <- database_var[!duplicated(database_var$GE.Unit),]
        
        #FILL IN temporal.cat
        for (i in 1:dim(temporal.cat)[1]) {
            # SELECT THE DATE 
            d <- temporal.cat$Oc.Date[i]
            selection <-  d >= database_var$GE.On.Road.Date.Month &   
              (d <= database_var$Off.Road.Date.Month | is.na(database_var$Off.Road.Date.Month)) & (database_var[,cat_interest[1]] == temporal.cat[i,cat_interest[1]]) & 
              (database_var[,cat_interest[2]] == temporal.cat[i,cat_interest[2]])
            # ADD DATA TO DATAFRAME
            temporal.cat[i,var_interest] <- dim(database_var[selection,])[1]} # End loop for
      
    }  
      
    # CALULCATE CUMULATIVE SUM
    
    for (i in 1:dim(temporal.cat)[1]) {
      cond.1 <- (temporal.cat[,cat_interest[1]] == temporal.cat[i,cat_interest[1]]) & (temporal.cat[,cat_interest[2]] == temporal.cat[i,cat_interest[2]])
      cond.2 <- temporal.cat[,"Oc.Date"] <= temporal.cat[i,"Oc.Date"]
      
      temporal.cat[i,paste(var_interest,".Cumul")] <- sum(temporal.cat[cond.1 & cond.2,var_interest])
      
    }
    
    ############################################################################################################
    ############################################################################################################
    
    #START STRUCTURING THE DATA IN PREP FOR BAR PLOT
    #SELECT THE MIN DATE TO BE CONSIDERED FOR PLOT BASED ON MIN OCCURENCE OF EVENTS IN SAID DATE
    min.date <- min(temporal.cat$Oc.Date[temporal.cat[,var_interest] > limit.time])
    min.date <- as.Date(trunc.POSIXt(min.date, units = "years"))
    
    
    
    #DATA NEEDS TO BE RE-SHAPED PRIOR TO PLOTTING
    col.no <- which( colnames(temporal.cat)==var_interest )
    bla <- temporal.cat[,c(1:3,col.no)]
    bla <- bla[bla$Oc.Date>=min.date,]
    
    
    wide <- reshape(bla,
                    v.names = var_interest,
                    timevar = "Oc.Date",
                    idvar = cat_interest,
                    direction = "wide")
    
    #RENAME ROW NAMES
    concat <-  paste(wide[,1], wide[,2])
    rownames(wide) <- concat
    
    #RENAME COLUMN NAMES
    xlim <- temporal.cat$Oc.Date[!(duplicated(temporal.cat$Oc.Date))]
    xlim <- xlim[xlim >= min.date]
    
    fin <- length(cat_interest)
    wide <- wide[,-(1:fin)]
    names(wide) <- as.Date(xlim)
    
    
    #DEVELOP THE SAME PLOT BUT RESCALING EVERY BARPLOT TO 100%
    wide.full <- wide
    wide.full.sum <- apply(wide.full,2,sum)
    wide.full.sum <- as.data.frame(wide.full.sum)
    
    wide.full.perc <- wide.full
    
    for (i in 1:dim(wide.full)[2]){
      wide.full.perc[,i] <- wide.full[,i]/wide.full.sum[i,]
    }
   
    wide.full.perc <- apply(wide.full.perc,2, function(x) ifelse(is.na(x),0,x))
    
    
    #CUMUL DATA ALSO NEEDS TO BE RE-SHAPED PRIOR TO PLOTTING
    col.no.cumul <- which( colnames(temporal.cat)==paste(var_interest,".Cumul") )
    bla.cumul <- temporal.cat[,c(1:3,col.no.cumul)]
    bla.cumul <- bla.cumul[bla.cumul$Oc.Date>=min.date,]
    
    
    wide.cumul <- reshape(bla.cumul,
                          v.names = paste(var_interest,".Cumul"),
                          timevar = "Oc.Date",
                          idvar = cat_interest,
                          direction = "wide")
    rownames(wide.cumul) <- concat
    
    
    wide.cumul <- wide.cumul[,-1]
    names(wide.cumul) <- as.Date(xlim)
    
    
    ############################################################################################################
    ############################################################################################################
    #PREP FOR PLOTS
    
    #PREP COLOUR FOR LEGEND
    library(RColorBrewer)
    sequential <- rainbow(repetition, s=0.5)
    
    
    #PREP LABELS FOR PLOTS
    labels.x <- seq(min(xlim),max(xlim),by="12 months")
    labels.x <- format(labels.x, "%Y") 
    
    
    #PLOT ALL INFORMATION WITHIN ONE CHART
    #PLOT RESULTS FOR BARPLOT
    
    x11(50,30)
    par(mfrow=c(2,2), 
        oma=c(0,0,2,0))
    
    #PLOT 1
    mp <- barplot(as.matrix(wide),
                  xaxt="n",
                  col = sequential, # colors
                  ylab = "#",
                  main = paste("Bar plot of ", var_interest, "over Time \n split by ",cat_interest[1],", ",cat_interest[2]),
                  lwd=0.8
    )
    axis(1,at = seq(min(mp),max(mp),1.2*12),
         labels = labels.x,
         las=2,cex.axis=0.75
    )
    mtext("Date", side=1, line=3, cex.lab=1)
    
    abline(v = seq(min(mp),max(mp),1.2*12),
           lty = 3,
           lwd = 0.7,
           col = "grey") 
    
    
    #PLOT RESULTS FOR % BAR PLOT
    mp <- barplot(as.matrix(wide.full.perc),
                  xaxt="n",
                  yaxt="n",
                  col = sequential, # colors
                  ylab = "%",
                  main = paste("% Bar plot of ", var_interest, "over Time \n split by ",cat_interest[1],", ",cat_interest[2]),
                  lwd=0.8
    )
    axis(1,at = seq(min(mp),max(mp),1.2*12),
         labels = labels.x,
         las=2,cex.axis=0.75
    )
    axis(2,at = seq(0, 1, 0.2),
         labels = seq(0,1,0.2)*100,
         las=2
    )
    mtext("Date", side=1, line=3, cex.lab=1)
    
    
    
    #PLOT THE CUMULATIVE BARPLOT
    par(new, mar = c(5,5,5,0), xpd=FALSE)
    mp <- barplot(as.matrix(wide.cumul),
                  xaxt="n",
                  col = sequential, # colors
                  ylab = "#",
                  #yaxt = "m"
                  main = paste("Bar plot of Cumulative # of ", var_interest, "over Time \n split by ",cat_interest[1],", ",cat_interest[2]),
                  lwd=0.8
    )
   axis(1,at = seq(min(mp),max(mp),1.2*12),
         labels = labels.x,
         las=2,cex.axis=0.75
    )
    mtext("Date", side=1, line=3, cex.lab=1)
    
    abline(v = seq(min(mp),max(mp),1.2*12),
           lty = 3,
           lwd = 0.7,
           col = "grey") 
    
    #PLOT 4: IN THIS SPACE, INSERT LEGEND AND INFO ABOUT DIMENSIONS
    par(new)
    
    #EMPTY PLOT
    plot(1, type="n", axes=F, xlab="", ylab="")
    
    #PLOT LEGEND
    leg.prep <- unique(temporal.cat[,cat_interest])
    leg.prep <- paste(leg.prep[,1],leg.prep[,2])
    legend("topleft", # Find suitable coordinates by trial and error
           legend = rev(leg.prep),
           fill = rev(sequential), # 6:1 reorders so legend order matches graph
           cex=1.0,
           ncol=3
    )
    
    
    #INPUT INFORMATION ABOUT NO OF VEHICLES AND TRANSACTIONS WE WORKED WITH      
    mtext(paste("No. of vehicles: ",prettyNum(dim.veh, big.mark=",")), side=1, line=-2, cex.lab=1,adj=0)
    mtext(paste("No. of transactions: ",prettyNum(dim.trans, big.mark=",")), side=1, line=-4, cex.lab=1,adj=0)
         
    
    
    
    } else if (length(cat_interest) == 1){
    
    
      ##############################################################################################################
      ##############################################################################################################
      ##############################################################################################################
      ##############################################################################################################
      ##############################################################################################################
      #SECOND PART OF IF LOOP
      
      
      
      #var_interest <- var.interest
      #cat_interest <- c(cat.interest)
      #database_var <- database.var
      #limit.time <- limit.time
      #limit.cat <- limit.cat
      

      
      #REMOVE ANY NA ENTRIES WRT CAT.INTEREST
      j <- which(!is.na(database_var[,cat_interest]))
      database_var <- database_var[j,]
      
      
      #REMOVE CAT ENTRIES WHICH HAVE LESS THAN limit.cat ENTRIES
      
      database_var$Dis <- database_var[,cat_interest]
      df <- aggregate(ID ~ Dis, 
                      data = database_var, 
                      length)
      df <- df[df[,2] > limit.cat,]
      database_var <- database_var[database_var$Dis %in% df[,1], ]
      
      
      #DEDUCE MAX AND MIN DATE FOR ANALYSIS & PLOT
      selection <- c(
        "Factory.Order.Date",            # ?
        "GE.On.Road.Date",               # IMPORTANT
        "Off.Road.Date",                 # IMPORTANT
        "Original.On.Road.Date",         # ?
        "Purchase.Transaction.Date",     # IMPORTANT
        "Sold.Date"                      # ?
      )
      # SELECT DATA FRAME
      df <- master[,selection]
      # CALCULATIONS 
      MAX <- apply(df, 2, function(x)max(x[!is.na(x)]))
      MIN <- apply(df, 2, function(x)min(x[!is.na(x)]))
      # CALULATE TIME PERIOD FOR THE ANALYSIS  
      t.min <- as.Date(min(MIN))
      t.max <- as.Date(max(MAX))
      
      
      #CALCULATE THE DIMENSIONS OF THE DF
      repetition <- length(unique(database_var[,cat_interest],na.rm=TRUE))
      
      
      #BUILD DF USING t.min, t.max AND repetition
      temporal.cat <- data.frame(rep(seq(t.min,t.max,by ="month"),each=repetition))
      names(temporal.cat) <- "Oc.Date"
      
      
      #ADD INFORMATION ABOUT CATEGORY OF INTEREST
      temporal.cat[,cat_interest] <- unique(database_var[,cat_interest],na.rm=TRUE)
      
      
      ############################################################################################################
      ############################################################################################################
      #COMPLETE temporal.cat BY FILLING IN DATA AS PER var.interest
      # LOOP THROUGH ALL DATES IN temporal.cat
      
      if (var_interest == "Abs.Maintenance.Amount" ){
        
        #CALCULATE DIM, TO BE USED IN PLOT
        dim.veh <- length(unique(database_var$GE.Unit))
        dim.trans <- dim(database_var)[1]
        
        #FILL IN temporal.cat
        for (i in 1:dim(temporal.cat)[1]) {
          # SELECT THE DATE 
          d <- temporal.cat$Oc.Date[i]
          selection <-  d == database_var$Purchase.Transaction.Date.Month & (database_var[,cat_interest] == temporal.cat[i,cat_interest])
          # ADD DATA TO DATAFRAME
          a <- database_var[selection, var_interest]
          a <- as.data.frame(a)
          temporal.cat[i,var_interest] <- ifelse(dim(a)[1]==0,
                                                 0,
                                                 sum(a, na.rm=TRUE))} # End loop for
        
        
      } else if (var_interest == "Transactions"){
        
        #CALCULATE DIM, TO BE USED IN PLOT
        dim.veh <- length(unique(database_var$GE.Unit))
        dim.trans <- dim(database_var)[1]
        
        #FILL IN temporal.cat
        for (i in 1:dim(temporal.cat)[1]) {
          # SELECT THE DATE 
          d <- temporal.cat$Oc.Date[i]
          selection <-  d == database_var$Purchase.Transaction.Date.Month & (database_var[,cat_interest] == temporal.cat[i,cat_interest])
          # ADD DATA TO DATAFRAME
          temporal.cat[i,var_interest] <- dim(database_var[selection,])[1]} # End loop for
        
        
      } else if (var_interest == "Death"){
        
        #CALCULATE DIM, TO BE USED IN PLOT AND REMOVE AND NA ENTRIES WRT var.interest
        j <- which(!is.na(database_var[,"Off.Road.Date.Month"]))
        database_var <- database_var[j,]
        
        dim.veh <- length(unique(database_var$GE.Unit))
        dim.trans <- dim(database_var)[1]
        
        #GENERATE DF TO BE USED WITHIN ANALYSIS
        database_var <- database_var[!duplicated(database_var$GE.Unit),]
        
        #FILL IN temporal.cat
        for (i in 1:dim(temporal.cat)[1]) {
          # SELECT THE DATE 
          d <- temporal.cat$Oc.Date[i]
          selection <-  d == database_var$Off.Road.Date.Month & (database_var[,cat_interest] == temporal.cat[i,cat_interest])
          # ADD DATA TO DATAFRAME
          a <- database_var[selection, var_interest]
          a <- as.data.frame(a)
          temporal.cat[i,var_interest] <- ifelse(dim(a)[1]==0,
                                                 0,
                                                 sum(a, na.rm=TRUE))} # End loop for
        
        
      } else if (var_interest == "Original.On.Road.Date.Month"){
        
        
        #CALCULATE DIM, TO BE USED IN PLOT AND REMOVE AND NA ENTRIES WRT var.interest
        j <- which(!is.na(database_var[,"Original.On.Road.Date.Month"]))
        database_var <- database_var[j,] 
        
        dim.veh <- length(unique(database_var$GE.Unit))
        dim.trans <- dim(database_var)[1]
        
        #GENERATE DF TO BE USED WITHIN ANALYSIS
        database_var <- database_var[!duplicated(database_var$GE.Unit),]
        
        
        #FILL IN temporal.cat
        for (i in 1:dim(temporal.cat)[1]) {
          # SELECT THE DATE 
          d <- temporal.cat$Oc.Date[i]
          selection <-  d == database_var$Original.On.Road.Date.Month & (database_var[,cat_interest] == temporal.cat[i,cat_interest])
          # ADD DATA TO DATAFRAME
          a <- database_var[selection, var_interest]
          a <- as.data.frame(a)
          temporal.cat[i,var_interest] <- ifelse(dim(a)[1]==0,
                                                 0,
                                                 dim(a)[1])} # End loop for
        
      } else {
        
        
        #CALCULATE DIM, TO BE USED IN PLOT
        dim.veh <- length(unique(database_var$GE.Unit))
        dim.trans <- dim(database_var)[1]
        
        #GENERATE DF TO BE USED WITHIN ANALYSIS
        database_var <- database_var[!duplicated(database_var$GE.Unit),]
        
        #FILL IN temporal.cat
        for (i in 1:dim(temporal.cat)[1]) {
          # SELECT THE DATE 
          d <- temporal.cat$Oc.Date[i]
          selection <-  d >= database_var$GE.On.Road.Date.Month &   
            (d <= database_var$Off.Road.Date.Month | is.na(database_var$Off.Road.Date.Month)) & (database_var[,cat_interest] == temporal.cat[i,cat_interest])
          # ADD DATA TO DATAFRAME
          temporal.cat[i,var_interest] <- dim(database_var[selection,])[1]} # End loop for
        
      }  
      
      # CALULCATE CUMULATIVE SUM
      
      for (i in 1:dim(temporal.cat)[1]) {
        cond.1 <- (temporal.cat[,cat_interest] == temporal.cat[i,cat_interest])
        cond.2 <- temporal.cat[,"Oc.Date"] <= temporal.cat[i,"Oc.Date"]
        
        temporal.cat[i,paste(var_interest,".Cumul")] <- sum(temporal.cat[cond.1 & cond.2,var_interest])
        
      }
      
      ############################################################################################################
      ############################################################################################################
      
      #START STRUCTURING THE DATA IN PREP FOR BAR PLOT
      #SELECT THE MIN DATE TO BE CONSIDERED FOR PLOT BASED ON MIN OCCURENCE OF EVENTS IN SAID DATE
      min.date <- min(temporal.cat$Oc.Date[temporal.cat[,var_interest] > limit.time])
      min.date <- as.Date(trunc.POSIXt(min.date, units = "years"))
      
      
      
      #DATA NEEDS TO BE RE-SHAPED PRIOR TO PLOTTING
      col.no <- which( colnames(temporal.cat)==var_interest )
      bla <- temporal.cat[,c(1:2,col.no)]
      bla <- bla[bla$Oc.Date>=min.date,]
      
      
      wide <- reshape(bla,
                      v.names = var_interest,
                      timevar = "Oc.Date",
                      idvar = cat_interest,
                      direction = "wide")
      
      #RENAME ROW NAMES
      concat <- wide[,1]
      rownames(wide) <- concat
      
      #RENAME COLUMN NAMES
      xlim <- temporal.cat$Oc.Date[!(duplicated(temporal.cat$Oc.Date))]
      xlim <- xlim[xlim >= min.date]
      
      fin <- length(cat_interest)
      wide <- wide[,-(1:fin)]
      names(wide) <- as.Date(xlim)
      
      
      #DEVELOP THE SAME PLOT BUT RESCALING EVERY BARPLOT TO 100%
      wide.full <- wide
      wide.full.sum <- apply(wide.full,2,sum)
      wide.full.sum <- as.data.frame(wide.full.sum)
      
      wide.full.perc <- wide.full
      
      for (i in 1:dim(wide.full)[2]){
        wide.full.perc[,i] <- wide.full[,i]/wide.full.sum[i,]
      }
      
      wide.full.perc <- apply(wide.full.perc,2, function(x) ifelse(is.na(x),0,x))
      
      
      #CUMUL DATA ALSO NEEDS TO BE RE-SHAPED PRIOR TO PLOTTING
      col.no.cumul <- which( colnames(temporal.cat)==paste(var_interest,".Cumul") )
      bla.cumul <- temporal.cat[,c(1:2,col.no.cumul)]
      bla.cumul <- bla.cumul[bla.cumul$Oc.Date>=min.date,]
      
      
      wide.cumul <- reshape(bla.cumul,
                            v.names = paste(var_interest,".Cumul"),
                            timevar = "Oc.Date",
                            idvar = cat_interest,
                            direction = "wide")
      rownames(wide.cumul) <- concat
      
      
      wide.cumul <- wide.cumul[,-1]
      names(wide.cumul) <- as.Date(xlim)
      
      
      ############################################################################################################
      ############################################################################################################
      #PREP FOR PLOTS
      
      #PREP COLOUR FOR LEGEND
      library(RColorBrewer)
      sequential <- rainbow(repetition, s=0.5)
      
      
      #PREP LABELS FOR PLOTS
      labels.x <- seq(min(xlim),max(xlim),by="12 months")
      labels.x <- format(labels.x, "%Y") 
      
      
      #PLOT ALL INFORMATION WITHIN ONE CHART
      #PLOT RESULTS FOR BARPLOT
      
      x11(50,30)
      par(mfrow=c(2,2), 
          oma=c(0,0,2,0))
      
      #PLOT 1
      mp <- barplot(as.matrix(wide),
                    xaxt="n",
                    col = sequential, # colors
                    ylab = "#",
                    main = paste("Bar plot of ", var_interest, "over Time \n split by ",cat_interest),
                    lwd=0.8
      )
      axis(1,at = seq(min(mp),max(mp),1.2*12),
           labels = labels.x,
           las=2,cex.axis=0.75
      )
      mtext("Date", side=1, line=3, cex.lab=1)
      
      abline(v = seq(min(mp),max(mp),1.2*12),
             lty = 3,
             lwd = 0.7,
             col = "grey") 
      
      
      #PLOT RESULTS FOR % BAR PLOT
      mp <- barplot(as.matrix(wide.full.perc),
                    xaxt="n",
                    yaxt="n",
                    col = sequential, # colors
                    ylab = "%",
                    main = paste("% Bar plot of ", var_interest, "over Time \n split by ",cat_interest),
                    lwd=0.8
      )
      axis(1,at = seq(min(mp),max(mp),1.2*12),
           labels = labels.x,
           las=2,cex.axis=0.75
      )
      axis(2,at = seq(0, 1, 0.2),
           labels = seq(0,1,0.2)*100,
           las=2
      )
      mtext("Date", side=1, line=3, cex.lab=1)
      
      
      
      #PLOT THE CUMULATIVE BARPLOT
      par(new, mar = c(5,5,5,0), xpd=FALSE)
      mp <- barplot(as.matrix(wide.cumul),
                    xaxt="n",
                    col = sequential, # colors
                    ylab = "#",
                    #yaxt = "m"
                    main = paste("Bar plot of Cumulative # of ", var_interest, "over Time \n split by ",cat_interest),
                    lwd=0.8
      )
      axis(1,at = seq(min(mp),max(mp),1.2*12),
           labels = labels.x,
           las=2,cex.axis=0.75
      )
      mtext("Date", side=1, line=3, cex.lab=1)
      
      abline(v = seq(min(mp),max(mp),1.2*12),
             lty = 3,
             lwd = 0.7,
             col = "grey") 
      
      #PLOT 4: IN THIS SPACE, INSERT LEGEND AND INFO ABOUT DIMENSIONS
      par(new)
      
      #EMPTY PLOT
      plot(1, type="n", axes=F, xlab="", ylab="")
      
      #PLOT LEGEND
      leg.prep <- unique(temporal.cat[,cat_interest])
      legend("topleft", # Find suitable coordinates by trial and error
             legend = rev(leg.prep),
             fill = rev(sequential), # 6:1 reorders so legend order matches graph
             cex=1.0,
             ncol=3
      )
      
      
      #INPUT INFORMATION ABOUT NO OF VEHICLES AND TRANSACTIONS WE WORKED WITH      
      mtext(paste("No. of vehicles: ",prettyNum(dim.veh, big.mark=",")), side=1, line=-2, cex.lab=1,adj=0)
      mtext(paste("No. of transactions: ",prettyNum(dim.trans, big.mark=",")), side=1, line=-4, cex.lab=1,adj=0)
    }
      
       
}