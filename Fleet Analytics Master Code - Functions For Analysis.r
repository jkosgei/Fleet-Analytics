#==================================================================================================================
# FUNTIONS TO USE
#==================================================================================================================
#THE FUNCTIONS DEFINED BELOW MAY BE CALLED IN THE MAIN CODE 'FLEET ANALYTICS MASTER CODE V21'.
#THE FUNCTIONS AVAILABLE ARE: 
#1) "FUNCTION FOR TIME SERIES PLOTS"
#2) "Numerical.vs.Categorical"
#3) "Initial.Numerical.Variable.Analysis"
#4) "FUNCTION FOR COSTING "


# FUNCTION FOR TIME SERIES PLOTS ==================================================================================   
# INPUT:
# tdata MUST BE A ZOO OBJECT
# tmain: CHARACTER STRING FOR TITTLE
# tylab: CHARACTER STRING FOR YLAB
# CREATE FUNCTION
TS <- function(tdata = NULL, 
               tmain = NULL,
               tylab = NULL,
               txlab = NULL
)
{
  # ACTUAL PLOT
  x11(15,8)
  par(oma=c(0,0,0,0),mar=c(4,4,2,1),cex.main=0.85,cex.axis=0.8)
  tsRainbow <- rainbow(ncol(tdata))
  plot(x = tdata, 
       ylab = tylab, 
       xlab = txlab,
       main = tmain,
       plot.type ="single",
       col = tsRainbow, 
       screens = 1,
       cex.axis = 0.8,
       las = 1,
       xaxt ="n"
  ) 
  # LEGEND
  legend(x = "topleft", legend = names(tdata),  lty = 1, col = tsRainbow)
  # X-AXIS LABELS
  ix    <- seq(1, dim(tdata)[1], by=1)  
  fmt   <- "%b-%y"  
  labs  <- format(index(tdata)[ix], fmt)
  axis(side = 1, at = index(tdata)[ix], labels = labs,  cex.axis = 0.7)
} # END FUNCTION


# Numerical.vs.Categorical =============================================================================

#-------------------------------------------------------------------------------
# The purpose of this function is to help on outliers identification by means
# of providing the user with some statistics about  numerical variables in
# respect of a given categorical variable.

# For instance, we might be interested in knowing the statistics between 
# pipe diameter and material. We know that from an engineering standpoint, 
# no PVC can be built that is larger (in diameter) than 900mm (or so...).

# Such statistics are provided for the numerical variable as a whole as well
# as for the subgroups comprising the diffent levels of the categorical variable
# introduced by the user.
#-------------------------------------------------------------------------------

# INPUT DENITION:
# master: data.frame with data
# name.categorical.variable
# name.numerical.variable
# levels.categorical: Optional character vector. This should be a vector of 
# the the categorical variable levels that you are interested in
# Threshold: if a max value of the numerical variable is already known, the
# user can introduced such a value in order to remove data above
# such a value.

# OUPUT:
# Plots: 
# PLOT (1): For each level of the categofical variable the following subplots
# are printed out:
# Sorted index plot
# Histogram
# Emperical Cumulative Distribution Function
# Basic Stats plot
# PLOT (2): A comparison of all emprirical distribution functions for each 
# level of the categorical variable

# PLOT (3): Plot of the general statistics for the numerical variable itself  
# and in respect of the levels of the categorical variable

# PLOT (4): Barplot of the record number for each level of the categorical
# variable

# summary_stats.csv: a .csv file comprsing the main stats of the numerical 
# variable in respect of the categorical variable

# Records_table.txt: Summary file of the distribution of records in respect 
# of the relation numerical variable-categorical variable

# EXAMPLE:
# Numerical.vs.Categorical(name.numerical.variable = c("DIAMETER_MWH"),
#                          name.categorical.variable = c("SOIL_FRACTURE"), 
#                          master=master, 
#                          threshold=NULL,
#                          levels.categorical=NULL)

# CODE: ----------------------------------------------------------------------

Numerical.vs.Categorical <- function(name.categorical.variable = NULL, 
                                     name.numerical.variable = NULL,
                                     master=master, 
                                     threshold=NULL,
                                     levels.categorical=NULL) {
  
  cat("PROCESSING...\n")
  numerical <-  as.numeric(master[,
                                  which(names(master)==name.numerical.variable)])
  categorical <- as.character(master[,
                                     which(names(master)==name.categorical.variable)])
  
  if(!is.null(threshold) & sum(!is.na(numerical) & numerical>threshold,
                               na.rm=TRUE)>0){
    reject<- -(which(!is.na(numerical) & numerical>threshold))
    numerical <- numerical[reject]
    categorical <- as.factor(as.character(categorical[reject]))
    sink("position_values_above thershold.txt")
    print(reject)
    sink()
    cat("position_values_above thershold.txt DONE \n")
  } # End if statment 
  
  # Stats for all numerical records
  all <- c(max(numerical,na.rm=TRUE),mean(numerical,na.rm=TRUE),
           median(numerical,na.rm=TRUE),min(numerical,na.rm=TRUE),
           length(numerical),sum(is.na(numerical)))  
  
  stats <- data.frame(all)
  row.names(stats) <- c("max","mean","median","min","records","NAs")
  
  # Remove NAs in either of the variables
  i <- which(!is.na(numerical) & !is.na(categorical))
  Analysis <- data.frame(numerical,categorical)
  Analysis <- Analysis[i,]
  
  # If there are not specified levels of the categorical variable, then 
  # study all levels
  if (is.null(levels.categorical)){
    levels.categorical <- levels(as.factor(as.character(categorical)))                                 
  } # End if statment
  
  # Make sure we have the levels we really want in the categorical variable
  Analysis <- Analysis[which(Analysis[,2]%in%levels.categorical),]
  Analysis$categorical <- as.factor(as.character(Analysis$categorical))
  
  # Plot each level
  for (i in 1:length(levels.categorical)) {
    TMP <-Analysis[which(as.character(Analysis$categorical)  %in% 
                           levels(Analysis$categorical)[i]),]  
    tmp <- TMP$numerical
    
    stats <- cbind(stats,c(max(tmp,na.rm=TRUE),mean(tmp,na.rm=TRUE),
                           median(tmp,na.rm=TRUE),min(tmp,na.rm=TRUE),length(tmp),
                           sum(is.na(tmp))))
    names(stats)[i+1]<-levels(Analysis$categorical)[i]
    
    # Plot each categorical levels
    x11()
    par(cex.main=0.85)
    par(mfrow=c(2,2))
    
    plot(tmp[order(tmp)],pch=16,main="Sorted Index plot", 
         xlab="Datapoint",ylab=paste(name.numerical.variable," for ",
                                     levels(Analysis$categorical)[i],sep="")) 
    hist(tmp,col="red",main="Histogram",xlab=paste(name.numerical.variable,
                                                   " for ",levels(Analysis$categorical)[i],sep=""))  
    plot(ecdf(tmp),main='Empirical Cumulative Distribution Function',
         ylab='cdf',xlab=paste(name.numerical.variable," for ",
                               levels(Analysis$categorical)[i],sep=""))
    #mtext(levels(Analysis$categorical)[i], side = 3)
    
    
    dotchart(as.matrix(stats[1:4,c(1,i+1)]),col="red",pch=16,cex=0.7,
             main=paste(name.numerical.variable))
    
    savePlot(filename=paste(levels(Analysis$categorical)[i],"_",
                            name.numerical.variable,".jpeg",sep=""), type = c("jpeg"),
             device = dev.cur(), restoreConsole = TRUE)
    
    cat(paste(levels.categorical)[i],"PLOTED \n")
    
  } # End loop for
  
  #-----------------------------------------------------------------------
  # Plot ecdf
  x11()
  plot(1,type="n",ylim=c(0,1),ylab="ecdf",main=paste("ecdf for",
                                                     name.categorical.variable),
       xlim=c(0,max(numerical,na.rm=TRUE)),xlab=name.numerical.variable)
  
  for (i in 1:length(levels.categorical)) {
    par(new=TRUE)
    TMP <-Analysis[which(as.character(Analysis$categorical)  %in% 
                           levels(Analysis$categorical)[i]),]  
    tmp <- TMP$numerical
    
    plot(ecdf(tmp),lwd=2,verticals= TRUE,col=i+1,ylim=c(0,1),main="",
         xlim=c(0,max(numerical,na.rm=TRUE)),xlab="",ylab="")
  } # End loop for
  legend("bottomright", legend = levels.categorical,pch=16, 
         col=2:(length(levels.categorical)+1),cex=1)
  
  
  savePlot(filename=paste("ECDF_",name.numerical.variable,".jpeg",sep=""), 
           type = c("jpeg"),device = dev.cur(), restoreConsole = TRUE)
  
  cat("ECDF for all levels PLOTED \n")
  
  #-----------------------------------------------------------------------
  # PLOT all
  x11(w=20,h=25)
  layout(matrix(c(1,2,1,3,1,4), 3, 2, byrow = TRUE))
  par(oma=c(0,0,0,0),mar=c(4,4,2,1),cex.main=0.85,cex.axis=0.8)
  # Numerical variable comparison
  dotchart(as.matrix(stats[1:4,]),col="red",pch=16,cex=0.7,
           main=paste(name.numerical.variable))
  grid(lwd=2)
  hist(numerical,col="red",main="Histogram",xlab=paste(name.numerical.variable))
  plot(ecdf(numerical),main='Empirical Cumulative Distribution Function',
       ylab='cdf',xlab=paste(name.numerical.variable))
  stripchart(Analysis$numerical ~ Analysis$categorical,main = "stripchart",las=2, 
             vertical = TRUE,ylab=paste(name.numerical.variable))
  
  savePlot(filename=paste("All","_",name.numerical.variable,".jpeg",sep=""), 
           type = c("jpeg"),device = dev.cur(), restoreConsole = TRUE)
  
  
  
  # Add number of unused records 
  Unused_Records <- stats[5,1]-sum(stats[5,2:dim(stats)[2]])
  stats$Unused_Records <- c(rep(NA,4),Unused_Records,NA)
  
  write.csv(stats,"summary_stats.csv")
  cat("summary_stats.csv: Done \n")
  #-----------------------------------------------------------------------
  # Barplot
  
  # NUMBER OF RECORDS
  # Summary records
  Records.table <- t(table(Analysis))
  sink("Records_table.txt")
  print(Records.table)
  sink()
  
  cat("Records_table.txt: Done \n")
  
  # Begin plot 
  x11(w=10,h=10) 
  par(mfrow=c(2,1),oma=c(3,3,3,3),mar=c(4,4,2,1),cex.main=0.85,cex.axis=0.8)
  # Boxplot
  boxplot(Analysis$numerical ~ Analysis$categorical,           
          data = master, col = "lightgray",varwidth=TRUE,
          ylab=paste(name.numerical.variable), main="Boxplot",las=2)
  
  # barplot
  total <- sum(apply(Records.table,1,sum))
  max.val <- max(apply(Records.table,1,max))
  labels <- round((apply(Records.table,1,sum)/total*100),2)
  
  tmp <- barplot(apply(Records.table,1,sum),las=2,
                 ylim=c(0,max(apply(Records.table,1,sum))*1.2),
                 xlab="Number of Records",
                 main=paste("Total records for each",name.categorical.variable))
  text(tmp, apply(Records.table,1,sum), labels = labels, pos = 3, cex = 0.7)
  
  
  savePlot(filename=paste("All_2","_",name.numerical.variable,
                          ".jpeg",sep=""), type = c("jpeg"),device = dev.cur(), restoreConsole = TRUE)
  
  cat(paste("FINISH \n Have a look at the output files in:",getwd()))
  
  # Plot of all the cdf 
  
} # End function

# Initial.Numerical.Variable.Analysis =============================================================================

#-------------------------------------------------------------------------------
# This function is aimed at the very first step of the cleaning up process. 

# This function provides a single plot for each numerical variable provided. 
# This plot includes de following subplots:
# 1. Index plot of the variable in question (like a scatterplot)
# 2. A boxplot
# 3. Histogram
# 4. Optionally if the response variable is provided, then it produces
# a plot of the numerical variable vs the response variable
#-------------------------------------------------------------------------------

# INPUT DENITION:
# master= dataframe of data comprising the numerical variables
# Numerical.Var=character string of header of the numerical variables
# Response=character string of header of the response variable else set this to 
# NULL

# OUPUT:
# PLOT (as menioned in the introduction of this function) saved on your 
# working directory

# EXAMPLE(1):
#   Initial.Numerical.Variable.Analysis(master=master, 
# Numerical.Var="DIAMETER_MWH",
# Response="FAILURE_COUNT")

# EXAMPLE(2):
# Initial.Numerical.Variable.Analysis(master=master, 
#     Numerical.Var=c("DIAMETER_MWH","WATER_PRESSURE_MIN"),
#     Response=NULL)

# CODE: ----------------------------------------------------------------------
Initial.Numerical.Variable.Analysis <- function(master=NULL, 
                                                Numerical.Var=NULL,
                                                Response=NULL){
  cat("PROCESSING...\n")  
  # PLOT Numerical variables Individual plots
  # Loop with plots
  for(i in 1:length(Numerical.Var)) { 
    cat(paste("Plotting",Numerical.Var[i],"\n"))
    x11(w=15,h=12)
    par(mfrow=c(2,2))      
    # Four figures per plot: Index plot, box-and-whisker plot, 
    # frequency plot, and index plot with Response variable  
    # Index plot 
    # PLOT:  Index plot
    plot(master[,which(names(master)==Numerical.Var[i])], 
         main=Numerical.Var[i],pch=16,ylab=Numerical.Var[i],col="gray20")
    # PLOT: box-and-whisker plot
    boxplot(master[,which(names(master)==Numerical.Var[i])],
            main=Numerical.Var[i],col="green",ylab=Numerical.Var[i])
    # PLOT: frequency plot
    hist(master[,which(names(master)==Numerical.Var[i])],
         main=Numerical.Var[i],col="red",ylab="Frequency",
         xlab=Numerical.Var[i])
    # PLOT: index plot with Response variable if it is not null
    if (!is.null(Response)) {
      plot(master[,which(names(master)==Numerical.Var[i])], 
           master[,which(names(master)==Response)],col="lightgrey" ,
           main=paste(Response,"vs",Numerical.Var[i]),
           pch=16,xlab=Numerical.Var[i],ylab=Response)
    } else { plot(1,axes=F,type="n",xlab="",ylab="")
      
    }# End if statment
    
    savePlot(filename=paste("Initial.Numerical.Variable.Analysis.",
                            i,".",Numerical.Var[i],".jpeg",sep=""), 
             type = c("jpeg"),device = dev.cur(), restoreConsole = TRUE)
    
  } # End loop for  
  cat(paste("Output plot saved in",getwd(),"\n"))
  
} # End funtion 


########################################################################################################
# FUNCTION FOR COSTING  
########################################################################################################
#This function serves to generate scatter plots of the transactions carried out for age vs mileage, 
#split by a category of interest.
#In addition, the function serves to generate a series of plots which look at the avg cost, no of vehicles,
#and no. of transactions over age or mileage, split by a category of interest.
#Thirdly, a set of plot are generate which break down the avg.cost, no of vehicles and no of transactions by
#whether the vehicle under consideration was old or yound. 

#Inputs required are:
#CarIdentf = the variable to use to distinguish vehicles between each other
#Variables = category of interest
#BaseData = database to use
#LimitsAge = what to define as 'middle age'. If c(200,500), then a vehicle is considered of middle age if its
#age is between 200 and 500 weeks.
#Filenames = File name to use when saving data

SummaryUsage <- function (CarIdentf = NULL,
                          Variables = NULL,
                          BaseData  = NULL,
                          LimitsAge = NULL,
                          FileName  = NULL
) {
  
  # PACKAGE
  require(data.table)
  require(ggplot2)
  
  # GROUP BY
  Cart <- CarIdentf 
  Grby <- c(CarIdentf,Variables)
  
  # DATA DEFINITION
  df <- BaseData
  
  # FILLING NAS
  df$Trans.Odometer.Clean[is.na(df$Trans.Odometer.Clean)] <- 0 # DOING THIS BECAUSE OTHERWISE PACKAGE DOES NOT WORK
  
  # DIMENTSION DEFINITION  
  TDimensions <- paste(as.character(Grby
                                    ,collapse=",")
  )    
  # AGGREGATATION
  df <- setDT(df)[,list(
    CostSum   = sum(Abs.Maintenance.Amount), 
    Count     = length(Abs.Maintenance.Amount), 
    Age       = max(Age),
    Milleage  = max(Trans.Odometer.Clean),
    Death     = max(Death), 
    Used      = max(UsedVehicle)
  ),by= TDimensions] 
  df <- data.frame(df)
  
  # COLLAPSE ALL LEVEL COMBINATIONS INTO ONE VARIABLE (Dis)  
  df$Dis <- do.call(paste, c(df[Variables], sep = "_"))
  
  # COLOUR ADDITION TO DATAFRAME
  var <- unique(df$Dis)
  np <- length(unique(df$Dis))
  myColour <- rainbow(np)
  MyNewColour <- data.frame(var,myColour)
  df <- merge(df,
              MyNewColour,
              by.x = "Dis",
              by.y = "var"
  )
  df$myColour <- as.character(df$myColour)
  
  # SHAPE ADDITION TO DATAFRAME BASED ON df$Expense.Type.Group
  shapes <- 15:19         # Shapes I like... we will repeat if necessary
  myShape <- rep(shapes,length.out = np)  
  MyNewShape <- data.frame(var,myShape)
  df <- merge(df,
              MyNewShape,
              by.x = "Dis",
              by.y = "var"
  ) 
  
  # AXIS SET-UP
  x.min <- min(df$Age,na.rm = TRUE)
  x.max <- max(df$Age,na.rm = TRUE)
  y.min <- min(df$Milleage)
  y.max <- max(df$Milleage) 
  
  # TITLE
  b <- c(apply(df[,c("CostSum","Count","Death","Used")], 2, sum), length(unique(df$GE.Unit)))
  tlt <- paste("Vehicles: ", b[5], "-",
               "Transact: ", b[2], "-",
               "Costs   :" , b[1], 
               sep = " "
  )
  
  # ACTUAL PLOT OF COSTS
  X11()
  un <- unique(df$GE.Unit)
  np <- length(un)
  
  plot(df$Age,
       df$Milleage,
       xlim = c(0,x.max),
       ylim = c(0,y.max),
       col = df$myColour,
       cex = 2 *sqrt(df$CostSum)/sqrt(max(df$CostSum)),
       main = paste("Costs \n",tlt,sep=""),
       xlab = "Time (Months)",  
       ylab = "Milleage",
       pch = df$myShape 
  )
  
  # LEGEND
  selection <- c("Dis","myColour","myShape")
  leg <- unique(df[,selection], 
                by = selection)
  indx <- sapply(leg, is.factor)
  leg[indx] <- lapply(leg[indx], function(x) as.character(x))
  legend(x = "topleft", 
         legend = leg[,1],  
         pch = leg[,3], 
         col = leg[,2]
  )    
  
  # ACTUAL PLOT OF TRANSACTIONS
  X11()
  un <- unique(df$GE.Unit)
  np <- length(un)
  
  plot(df$Age,
       df$Milleage,
       xlim = c(0,x.max),
       ylim = c(0,y.max),
       col = df$myColour,
       cex = 2 *sqrt(df$Count)/sqrt(max(df$Count)),
       main = paste("Transactions \n",tlt,sep=""),
       xlab = "Time (Months)",  
       ylab = "Milleage",
       pch = df$myShape 
  )
  
  # LEGEND
  selection <- c("Dis","myColour","myShape")
  leg <- unique(df[,selection], 
                by = selection)
  indx <- sapply(leg, is.factor)
  leg[indx] <- lapply(leg[indx], function(x) as.character(x))
  legend(x = "topleft", 
         legend = leg[,1],  
         pch = leg[,3], 
         col = leg[,2]
  )    
  
  # SECOND PART OF THE FUNCTION AGGREGATED VIEW OF AGE AND MILEAGE --------------------------------------------
  # DATA DEFINITION
  df <- BaseData
  
  # ADD MILLEAGE AND AGE CATEGORICAL
  br = seq(0,220,24)
  br = c(-Inf,br[-1],Inf)
  df$Cat.Age <- cut(df$TransAge, breaks = br) 
  br = seq(0,320,30)
  br = c(-Inf,br[-1],Inf)
  df$Cat.Mil <- cut(df$Mileage/1000, breaks = br) 
  
  
  # MILEAGE AND AGE AGGREGATED COSTS
  # AGE===================================================== 
  
  # SET UP OF DIMENTIONS  
  TDimensions <- paste(as.character(c(Grby[-1],"Cat.Age")
                                    ,collapse=",")
  )      
  
  # CREATE BASE CATEGORIES WITH ALL TRANSACTIONS
  out <- setDT(df)[,list(
    Avg.Cost  = mean(Abs.Maintenance.Amount), 
    Trans     = length(Abs.Maintenance.Amount),
    Vehicles  = length(unique(GE.Unit))
  ),by= TDimensions]  
  out <- data.frame(out)
  out$Dis <- do.call(paste, c(out[Variables], sep = "_"))
  nc <- length(unique(out$Dis))
  
  #                       x11(25,10)
  #                       ggplot(out, 
  #                              aes(x = Cat.Age, y = Avg.Cost, color = MAKE, group = MAKE)) +  
  #                              geom_point() + geom_line() + 
  #                              facet_wrap(~ MAKE, ncol = nc) +
  #                                           theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # COSTS
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Age", y = "Avg.Cost", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    facet_wrap(as.formula(paste("~", "Dis")), ncol = nc) +
    labs(title="Age vs Avg Cost") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))    
  print(p)
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Age", y = "Avg.Cost", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    labs(title="Age vs Avg Cost") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(p)
  # TRANSACTIONS
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Age", y = "Trans", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    facet_wrap(as.formula(paste("~", "Dis")), ncol = nc) +
    labs(title="Age vs Transactions") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))    
  print(p)
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Age", y = "Trans", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    labs(title="Age vs Transactions") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  print(p)
  # VEHICLES
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Age", y = "Vehicles", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    facet_wrap(as.formula(paste("~", "Dis")), ncol = nc) +
    labs(title="Age vs Vehicles") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))   
  print(p)
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Age", y = "Vehicles", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    labs(title="Age vs Vehicles") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  print(p)
  
  
  # MILEAGE ===================================================== 
  # ADD CATEGORICAL  
  
  # SET UP OF DIMENTIONS  
  TDimensions <- paste(as.character(c(Grby[-1],"Cat.Mil")
                                    ,collapse=",")
  )      
  
  # CREATE BASE CATEGORIES WITH ALL TRANSACTIONS
  out <- setDT(df)[,list(
    Avg.Cost  = mean(Abs.Maintenance.Amount), 
    Trans     = length(Abs.Maintenance.Amount),
    Vehicles  = length(unique(GE.Unit))
  ),by= TDimensions]  
  out <- data.frame(out)
  out$Dis <- do.call(paste, c(out[Variables], sep = "_"))
  nc <- length(unique(out$Dis))
  print(p)
  
  #                       x11(25,10)
  #                       ggplot(out, 
  #                              aes(x = Cat.Age, y = Avg.Cost, color = MAKE, group = MAKE)) +  
  #                              geom_point() + geom_line() + 
  #                              facet_wrap(~ MAKE, ncol = nc) +
  #                                           theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # COSTS
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Mil", y = "Avg.Cost", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    facet_wrap(as.formula(paste("~", "Dis")), ncol = nc) +
    labs(title="Mileage ('000) vs Avg Cost") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))     
  print(p)
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Mil", y = "Avg.Cost", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    labs(title="Mileage ('000) vs Avg Cost") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(p)
  # TRANSACTIONS
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Mil", y = "Trans", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    facet_wrap(as.formula(paste("~", "Dis")), ncol = nc) +
    labs(title="Mileage ('000) vs Transactions") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))    
  print(p)
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Mil", y = "Trans", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    labs(title="Mileage  ('000) vs Transactions") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(p)
  # VEHICLES
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Mil", y = "Vehicles", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    facet_wrap(as.formula(paste("~", "Dis")), ncol = nc) +
    labs(title="Mileage ('000) vs Vehicles") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  print(p)
  x11(25,10)
  p<-ggplot(out, 
            aes_string(x = "Cat.Mil", y = "Vehicles", color = "Dis", group = "Dis")) +  
    geom_point() + geom_line() + 
    labs(title="Mileage ('000) vs Vehicles") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  
  print(p)
  
  
  # THIRD PART OF THE FUNCTION AGGREGATED VIEW OF AGE AND MILEAGE --------------------------------------------                      
  # SET UP OF DIMENTIONS  
  TDimensions <- paste(as.character(Grby[-1]
                                    ,collapse=",")
  )      
  
  # CREATE BASE CATEGORIES WITH ALL TRANSACTIONS
  AllAge <- setDT(df)[,list(
    Tot.Cost  = sum(Abs.Maintenance.Amount), 
    Max.Cost  = max(Abs.Maintenance.Amount), 
    Avg.Cost  = mean(Abs.Maintenance.Amount), 
    Max.Age   = max(Age),
    Avg.Age   = mean(Age),
    Max.Mil   = max(Trans.Odometer.Clean),
    Avg.Mil   = max(Trans.Odometer.Clean),
    Tot.Death = length(unique(GE.Unit[Death == 1])), 
    Tot.Used  = length(unique(GE.Unit[UsedVehicle == 1])),
    Count     = length(Abs.Maintenance.Amount), 
    Vehicles  = length(unique(GE.Unit))
  ),by= TDimensions] 
  AllAge <- data.frame(AllAge)
  AllAge$Dis <- do.call(paste, c(AllAge[Variables], sep = "_"))
  AllAge$UsagePeriod <- "All"  
  AllAge <- AllAge[!(AllAge$Dis == "NA"),]
  AllAge$Tot.Not.Used <- AllAge$Vehicles - AllAge$Tot.Used
  AllAge$Tot.Not.Dead <- AllAge$Vehicles - AllAge$Tot.Death
  AllAge$Per.Used <- round((AllAge$Tot.Used / AllAge$Vehicles) *100,1)
  AllAge$Per.Dead <- round((AllAge$Tot.Death / AllAge$Vehicles) *100,1)
  
  # BASE FOR JOINS  
  myBase <-  data.frame(AllAge[,c("Dis")])
  names(myBase) <- c("Dis")
  
  # AGGREGATATION FOR LOW AGE
  LowAge <- setDT(df[df$Age < LimitsAge[1],])[,list(
    Tot.Cost  = sum(Abs.Maintenance.Amount), 
    Max.Cost  = max(Abs.Maintenance.Amount), 
    Avg.Cost  = mean(Abs.Maintenance.Amount), 
    Max.Age   = max(Age),
    Avg.Age   = mean(Age),
    Max.Mil   = max(Trans.Odometer.Clean),
    Avg.Mil   = max(Trans.Odometer.Clean),
    Tot.Death = length(unique(GE.Unit[Death == 1])), 
    Tot.Used  = length(unique(GE.Unit[UsedVehicle == 1])),
    Count     = length(Abs.Maintenance.Amount), 
    Vehicles  = length(unique(GE.Unit)),
    Transact  = length(GE.Unit)
  ),by= TDimensions] 
  LowAge <- data.frame(LowAge)
  LowAge$Dis <- do.call(paste, c(LowAge[Variables], sep = "_"))
  LowAge <- LowAge[!(LowAge$Dis == "NA"),]
  LowAge <- merge(myBase,
                  LowAge,
                  by.x = "Dis",
                  by.y = "Dis",
                  all.x = TRUE
  ) 
  LowAge$UsagePeriod <- "Low" 
  LowAge <-  LowAge[with(LowAge, order(Dis)), ]
  LowAge$Tot.Not.Used <- LowAge$Vehicles - LowAge$Tot.Used
  LowAge$Tot.Not.Dead <- LowAge$Vehicles - LowAge$Tot.Death
  LowAge$Per.Used <- round((LowAge$Tot.Used / LowAge$Vehicles) *100,1)
  LowAge$Per.Dead <- round((LowAge$Tot.Death / LowAge$Vehicles) *100,1)
  
  # AGGREGATATION FOR MID AGE
  MidAge <- setDT(df[df$Age >= LimitsAge[1] & df$Age < LimitsAge[2] ,])[,list(
    Tot.Cost  = sum(Abs.Maintenance.Amount), 
    Max.Cost  = max(Abs.Maintenance.Amount), 
    Avg.Cost  = mean(Abs.Maintenance.Amount), 
    Max.Age   = max(Age),
    Avg.Age   = mean(Age),
    Max.Mil   = max(Trans.Odometer.Clean),
    Avg.Mil   = max(Trans.Odometer.Clean),
    Tot.Death = length(unique(GE.Unit[Death == 1])),  
    Tot.Used  = length(unique(GE.Unit[UsedVehicle == 1])),
    Count     = length(Abs.Maintenance.Amount), 
    Vehicles  = length(unique(GE.Unit)),
    Transact  = length(GE.Unit)
  ),by= TDimensions] 
  MidAge <- data.frame(MidAge)
  MidAge$Dis <- do.call(paste, c(MidAge[Variables], sep = "_"))
  MidAge <- MidAge[!(MidAge$Dis == "NA"),]
  MidAge <- merge(myBase,
                  MidAge,
                  by.x = "Dis",
                  by.y = "Dis",
                  all.x = TRUE
  ) 
  MidAge$UsagePeriod <- "Mid"  
  MidAge <-  MidAge[with(MidAge, order(Dis)), ]
  MidAge$Tot.Not.Used <- MidAge$Vehicles - MidAge$Tot.Used
  MidAge$Tot.Not.Dead <- MidAge$Vehicles - MidAge$Tot.Death
  MidAge$Per.Used <- round((MidAge$Tot.Used / MidAge$Vehicles) *100,1)
  MidAge$Per.Dead <- round((MidAge$Tot.Death / MidAge$Vehicles) *100,1)
  
  # AGGREGATATION FOR HIGH AGE
  HighAge <- setDT(df[df$Age >= LimitsAge[2],])[,list(
    Tot.Cost  = sum(Abs.Maintenance.Amount), 
    Max.Cost  = max(Abs.Maintenance.Amount), 
    Avg.Cost  = mean(Abs.Maintenance.Amount), 
    Max.Age   = max(Age),
    Avg.Age   = mean(Age),
    Max.Mil   = max(Trans.Odometer.Clean),
    Avg.Mil   = max(Trans.Odometer.Clean),
    Tot.Death = length(unique(GE.Unit[Death == 1])), 
    Tot.Used  = length(unique(GE.Unit[UsedVehicle == 1])),
    Count     = length(Abs.Maintenance.Amount), 
    Vehicles  = length(unique(GE.Unit)),
    Transact  = length(GE.Unit)
  ),by= TDimensions] 
  HighAge <- data.frame(HighAge)
  HighAge$Dis <- do.call(paste, c(HighAge[Variables], sep = "_"))
  HighAge$UsagePeriod <- "High"     
  HighAge <- data.frame(HighAge)
  HighAge$Dis <- do.call(paste, c(HighAge[Variables], sep = "_"))
  HighAge <- HighAge[!(HighAge$Dis == "NA"),]
  HighAge <- merge(myBase,
                   HighAge,
                   by.x = "Dis",
                   by.y = "Dis",
                   all.x = TRUE
  ) 
  HighAge$UsagePeriod <- "High"    
  HighAge <-  HighAge[with(HighAge, order(Dis)), ] 
  HighAge$Tot.Not.Used <- HighAge$Vehicles - HighAge$Tot.Used
  HighAge$Tot.Not.Dead <- HighAge$Vehicles - HighAge$Tot.Death
  HighAge$Per.Used <- round((HighAge$Tot.Used / HighAge$Vehicles) *100,1)
  HighAge$Per.Dead <- round((HighAge$Tot.Death / HighAge$Vehicles) *100,1)
  
  SumAll <- rbind(LowAge,MidAge,HighAge)
  
  # COST SUMMARY (x)  
  x <- matrix(0,
              nrow = 3, 
              ncol = length(HighAge$Dis)
  )
  x[1,] <- t(LowAge[,c("Avg.Cost")])
  x[2,] <- t(MidAge[,c("Avg.Cost")])
  x[3,] <- t(HighAge[,c("Avg.Cost")])
  colnames(x) <- HighAge$Dis 
  rownames(x) <- c("Low","Mid","High")
  
  # COST SUMMARY (x)    
  p <- matrix(0,
              nrow = 3, 
              ncol = length(HighAge$Dis)
  )
  p[1,] <- t(LowAge[,c("Transact")])
  p[2,] <- t(MidAge[,c("Transact")])
  p[3,] <- t(HighAge[,c("Transact")])
  colnames(p) <- HighAge$Dis 
  rownames(p) <- c("Low","Mid","High")  
  
  
  
  # PLOT OF COSTS AND TRANSACTIONS 
  
  x11()
  par(oma=c(2,1,1,1),mar=c(4,4,1.5,1.5))
  par(mfrow = c(2,1))
  barplot(x,
          ylab = "Average Cost vehicle",
          col=c("gray87","gray48","gray15"),
          main = "Average Cost",
          cex.axis = 0.7,
          cex.names = 0.7,
          las = 2,
          legend = rownames(x), 
          beside = FALSE 
  )
  
  
  barplot(p,
          ylab = "# Transactions",
          col=c("gray87","gray48","gray15"),
          main = "Total Transactions",
          cex.axis = 0.7,
          cex.names = 0.7,
          las = 2,
          legend = rownames(p), 
          beside = FALSE 
  )  
  
  # USED CARS SUMMARY (y)  
  y <- matrix(0,
              nrow = 3, 
              ncol = length(HighAge$Dis)
  )
  y[1,] <- t(LowAge[,c("Per.Used")])
  y[2,] <- t(MidAge[,c("Per.Used")])
  y[3,] <- t(HighAge[,c("Per.Used")])
  colnames(y) <- HighAge$Dis 
  rownames(y) <- c("Low","Mid","High") 
  
  
  # TOTAL VEHICLES (Z)
  z <- matrix(0,
              nrow = 3, 
              ncol = length(HighAge$Dis)
  )
  z[1,] <- t(LowAge[,c("Vehicles")])
  z[2,] <- t(MidAge[,c("Vehicles")])
  z[3,] <- t(HighAge[,c("Vehicles")])
  colnames(z) <- HighAge$Dis 
  rownames(z) <- c("Low","Mid","High") 
  
  # TOTAL VEHICLES (Z)
  w <- matrix(0,
              nrow = 3, 
              ncol = length(HighAge$Dis)
  )
  w[1,] <- t(LowAge[,c("Per.Dead")])
  w[2,] <- t(MidAge[,c("Per.Dead")])
  w[3,] <- t(HighAge[,c("Per.Dead")])
  colnames(w) <- HighAge$Dis 
  rownames(w) <- c("Low","Mid","High") 
  
  # PLOT SUMMARY (off cost and # vehicles)
  X11()
  par(oma=c(2,1,1,1),mar=c(4,4,1.5,1.5))
  par(mfrow = c(2,1))
  barplot(x,
          ylab = "Average Cost",
          col=c("gray87","gray48","gray15"),
          main = "Average Cost",
          cex.axis = 0.7,
          cex.names = 0.7,
          las = 2,
          legend = rownames(x), 
          beside=TRUE 
  )
  
  #X11()
  barplot(z,
          ylab = "# Vehicles",
          col=c("gray87","gray48","gray15"),
          main = "#  Total Vehicles",
          cex.axis = 0.7,
          cex.names = 0.7,
          las = 2,
          legend = rownames(x), 
          beside = TRUE 
  ) 
  
  # PLOT SUMMARY (of death and used)
  X11()
  par(oma=c(2,1,1,1),mar=c(4,4,1.5,1.5))
  par(mfrow = c(2,1))
  barplot(y,
          ylab = "% Vehicles",
          col=c("gray87","gray48","gray15"),
          main = "% Vehicles Used",
          cex.axis = 0.7,
          cex.names = 0.7,
          las = 2,
          legend = rownames(x), 
          beside = TRUE 
  )
  
  #X11()
  barplot(w,
          ylab = "% Vehicles",
          col=c("gray87","gray48","gray15"),
          main = "% Vehicles Death",
          cex.axis = 0.7,
          cex.names = 0.7,
          las = 2,
          legend = rownames(w), 
          beside = TRUE 
  )
  
  write.csv(AllAge,paste(FileName, "_AllAge.csv" ,sep = "")) 
  write.csv(SumAll,paste(FileName, "_SumAll.csv" ,sep = "")) 
  
} # END FUNCTION  
