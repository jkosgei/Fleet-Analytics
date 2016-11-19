
###################################################################################################################
#
#                                         Author: Yanika Borg & Tomas Meca
#                                         Date: 17-Oct-2016
#                                         Project: Fleet Analysis
#
###################################################################################################################
#THE CODE IN THIS DATA SERVES TO GENERAT SURIVAL MODELS WHERE THE DATA IS SPLIT BY VARIABLES OF INTEREST.
#MOREOVER, THE SIGNIFICANCE AND VALIDITY OF THE DATA IS TESTED. 


#==================================================================================================================
# CODE INDEX
#==================================================================================================================

# SURVIVAL.PLOT:
#     CREATE SURVIVAL OBJECT AND OVERALL PERFORMANCE BY CATEGORY
#     PRODUCE SEMIPARAMETRIC COX


#==================================================================================================================
# SURVIVAL.PLOT
#==================================================================================================================

# CREATE SURVIVAL OBJECT AND OVERALL PERFORMANCE BY CATEGORY===================================================================



#EXAMPLE
#Survival.Plot(var.interest = c("Company","MAKE","Model"),
#              df = vehicle[vehicle[,"Usage.Type"] %in% c("Light Truck") & vehicle[,"Company"] %in% c("  RBDC","  RBNA"),],
#              limit = 0,
#              conf.bands = c("none") 
#)

#Inputs required:
#var.interest = category of interest to split data by
#df = database to use
#limit = if number of vehicles available in a specific level of the category of interest are below this limit, 
#they are excluded from the analysis



Survival.Plot <- function(var.interest = NULL,
                          df = NULL,
                          limit = NULL,
                          conf.bands = NULL
                          ){

  
var.interest <- var.interest  
df <- df
limit <- limit
conf.bands <- conf.bands


# DATA STUFF  
#  df2 <- data.frame(table(vehicle[c("Usage.Type","MAKE")]))
#  df2$Var1 <- as.character(df2$Var1)
#  leg.label <- df2[df2$Freq>limit,1]

#DATA PREP
#df$Dis <- do.call(paste, c(df[,var.interest], sep = "_")) 
#df2 <- aggregate(GE.Unit ~ Dis, 
#               data = df, 
#               length)
#df2 <- df2[df2[,2] > limit,]
#
#df <- df[df$Dis %in% df2[,1], ]


#LABEL PREP
if (length(var.interest)>1){
  df$Dis <- do.call(paste, c(df[,var.interest], sep = "_")) 
  leg.label <- unique(df$Dis)
} else {
  df$Dis <- df[,var.interest]
  leg.label <- unique(df[,var.interest])
}

leg.label <- as.character(leg.label)



# Converting into a Surv object 
SvAge <- Surv(df$Age, df$Death)
SvMil <- Surv(df$Mileage, df$Death)

k <- "Surv(time = Age, event = Death) ~"
a <- length(var.interest)
for (i in 1:a)
{
  if (i==1){
    k <- paste(k,as.character(var.interest[1]))
  } else {
    k <- paste(k,"+", as.character(var.interest[i]))
  }
}
kk <- k
k<-list(k)


# GET THE OVERAL KM ESTIMATOR

SvAge = npsurv(eval(parse(text = k)), 
               data = df
)
SvMil = npsurv(eval(parse(text = k)), 
               data = df
)

#SvAge = npsurv(Surv(time = Age, event = Death) ~ Usage.Type + Company, 
#               data = df
#)
#SvMil = npsurv(Surv(time = Mileage, event = Death) ~ Usage.Type + Company, 
#               data = df
#)



print(summary(SvAge))
print(summary(SvMil))


# colours  
p.lab <- names(SvAge$strata)
nc <-length(p.lab)
p.col <- rainbow(length(p.lab),alpha=0.65)

x11(10,6)
par(oma=c(0,0,0,0),mar=c(4,4,2,1) + nc/10,cex.main=0.85,cex.axis=0.8)
par(mfrow = c(1,2))

survplot(SvAge,
         conf = conf.bands,    # to include or exclude conf bands
         xlab = "", ylab = "Survival for AGE",
         ## xlim(0,100),
         #label.curves = TRUE,                     # label curves directly
         label.curves = list(keys = "lines"),
         levels.only  = FALSE,                    # show only levels, no label
         abbrev.label = FALSE,                    # if label used, abbreviate
         ## fun = function(x) {1 - x},            # Cumulative probability plot         
         loglog   = FALSE,                        # log(-log Survival) plot
         logt     = FALSE,                        # log time
         time.inc = 12,                          # time increment
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
         conf = conf.bands,    # to include or exclude conf bands
         xlab = "", ylab = "Survival for MILEAGE",
         ## xlim(0,100),
         #label.curves = TRUE,                     # label curves directly
         label.curves = list(keys = "lines"),
         #label.curves = leg.label,  # legend instead of direct label
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


legend(
  "topright",
  legend=leg.label,
  col=p.col,
  lty=1,
  horiz=FALSE,
  bty='n')


# Summary
x11(10,6)
par(mfrow = c(1,2))
br = seq(0,700,50)
death <- data.frame(df[df$Death==1,])
alive <- data.frame(df[df$Death==0,])
barplot(t(table(cut(death$Age, breaks = br),death$Dis)),
        legend.text = rev(leg.label))
barplot(t(table(cut(alive$Age, breaks = br),alive$Dis)),
        legend.text = rev(leg.label))


# PRODUCE SEMIPARAMETRIC COX ======================================================================================
# NOTE THAT THE CONFIDENCE INTERVAL DOES NOT CONTAIN THE 0




Vehicle_coxph = coxph(Surv(time = Age, event = Death) ~ Dis, 
                      data = df
)
summary(Vehicle_coxph)
Vehicle_coxph_tidy = tidy(Vehicle_coxph)
print(Vehicle_coxph_tidy) 
glance(Vehicle_coxph) 
write.csv(Vehicle_coxph_tidy, "Vehicle_coxph_tidy_AGE.csv")


Vehicle_coxph = coxph(Surv(time = Mileage, event = Death) ~ Dis, 
                      data = df
)
summary(Vehicle_coxph)
Vehicle_coxph_tidy = tidy(Vehicle_coxph)
print(Vehicle_coxph_tidy) 
glance(Vehicle_coxph) 
write.csv(Vehicle_coxph_tidy, "Vehicle_coxph_tidy_MILEAGE.csv")
}