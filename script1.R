install.packages("lmtest")
# Clear the workspace
rm(list=ls())
# Set WD
setwd("~/Desktop/Portugal")
getwd()

# Load libraries
library(foreign);
library(lmtest);
library(car);
library(sandwich)


### DOWNLOAD DATA ##
dataOG <-read.csv("OG.csv")  #output gap (2000-2021)
dataIR <-read.csv("IR.csv") #interest rate (2000-2021)
dataI <-read.csv("I.csv") #inflation (2000-2021)
dataUR <-read.csv("UR.csv") #unemployment rate (2000-2021)

# we generate the data-frame
DF = data.frame(cbind(dataOG$Value, dataIR$Value - 2, dataI$Value)) ## 2 is the target inflation rate
colnames(DF) <- c('OG', 'IR', 'I')

## PLOTS
#Interest rate over time
plot(dataIR$Value ~ dataIR$TIME, data = DF, xlab = "Time", ylab = "Interest rate", pch = 20, col = 'darkgreen')
abline(h=0, col="black")

#Interest rate and output gap
plot(IR ~ OG, data = DF, pch = 20)

#Interest rate and inflation
plot(IR ~ I, data = DF, pch = 20)

#Linear regression with the database (2000-2021)
lr_1 = lm(IR ~ OG + I, data=DF)
summary(lr_1)
plot(lr_1)

#Partial regression plots
avPlot(lr_1, variable = 'OG', col.lines = "darkgreen")
avPlot(lr_1, variable = 'I', col.lines = "darkred")


#### Testing OLS assumptions 
#BP test
bptest(lr_1)

#HAC-robust standard errors
coeftest(lr_1, vcov = vcovHAC)


#Breusch-Godfrey test.
bgtest(lr_1, order=3) 

#Reset test
resettest(lr_1, power=2, type="regressor")

#DW test
dwtest(lr_1)


                                    ### INTERPOLATION ####
# We generate monthly data from the yearly data (2000-2021)

(N <- sum(!is.na(dataOG$Value)))
(min <- min(dataOG$Time))
(max <- max(dataOG$Time))

month <- rep(1:12, times=N, each=1)
year <- rep(min:max, each=12, times=1)
(month_df <- data.frame(month, year))

##OUTPUT GAP INTERPOLATION
new_OG <- merge(x = month_df, y = dataOG, by.x = "year", 
                by.y = "Time", all = TRUE)

# Adding a column for the date (year/month), useful when plotting
new_OG$Date <- as.yearmon(paste(new_OG$year, new_OG$month), "%Y %m")

new_OG <- new_OG[1:(N*12-11),]

# Define the function
interpolation_v1 <- function(df) {
  df$val_interp[df$month == 1] <- df$Value[df$month == 1] # Update with your 
  # variable name
  (acs <- rep(NA, 12))
  df$interp_lag <- c(df$val_interp[13:(length(df$val_interp))],acs);
  df$diff <- c(df$val_interp - df$interp_lag)
  
  for (i in min:(max-1)) {
    for (j in 2:13) {
      init = df$val_interp[df$month==1 & df$year == i]
      end=df$interp_lag[df$month==1 & df$year == i]
      record=which(df$month==j & df$year==i)
      record1=which(df$month==1 & df$year==i)
      if (init  < end) {
        df$val_interp[record]<- df$val_interp[record1] + 
          abs(df$diff[record1])/12*(j-1)
      }  
      else {
        df$val_interp[record] <-
          df$val_interp[record1] - 
          abs(df$diff[record1])/12*(j-1)
      }
    }
  }
  
  return(df)
}

# Create the new dataset
new_OG <- interpolation_v1(new_OG)


## INTEREST RATE INTERPOLATION
new_IR <- merge(x = month_df, y = dataIR, by.x = "year", 
                by.y = "Time", all = TRUE)
new_IR$Date <- as.yearmon(paste(new_IR$year, new_IR$month), "%Y %m")

new_IR <- new_IR[1:(N*12-11),]

interpolation_v1 <- function(df) {
  df$val_interp[df$month == 1] <- df$Value[df$month == 1] # Update with your 
  # variable name
  (acs <- rep(NA, 12))
  df$interp_lag <- c(df$val_interp[13:(length(df$val_interp))],acs);
  df$diff <- c(df$val_interp - df$interp_lag)
  
  for (i in min:(max-1)) {
    for (j in 2:13) {
      init = df$val_interp[df$month==1 & df$year == i]
      end=df$interp_lag[df$month==1 & df$year == i]
      record=which(df$month==j & df$year==i)
      record1=which(df$month==1 & df$year==i)
      if (init  < end) {
        df$val_interp[record]<- df$val_interp[record1] + 
          abs(df$diff[record1])/12*(j-1)
      }  
      else {
        df$val_interp[record] <-
          df$val_interp[record1] - 
          abs(df$diff[record1])/12*(j-1)
      }
    }
  }
  
  return(df)
}

new_IR <- interpolation_v1(new_IR)

## INFLATION INTERPOLATION
new_I <- merge(x = month_df, y = dataI, by.x = "year", 
                by.y = "Time", all = TRUE)

new_I$Date <- as.yearmon(paste(new_I$year, new_I$month), "%Y %m")

new_I <- new_I[1:(N*12-11),]


interpolation_v1 <- function(df) {
  df$val_interp[df$month == 1] <- df$Value[df$month == 1] # Update with your 
  # variable name
  (acs <- rep(NA, 12))
  df$interp_lag <- c(df$val_interp[13:(length(df$val_interp))],acs);
  df$diff <- c(df$val_interp - df$interp_lag)
  
  for (i in min:(max-1)) {
    for (j in 2:13) {
      init = df$val_interp[df$month==1 & df$year == i]
      end=df$interp_lag[df$month==1 & df$year == i]
      record=which(df$month==j & df$year==i)
      record1=which(df$month==1 & df$year==i)
      if (init  < end) {
        df$val_interp[record]<- df$val_interp[record1] + 
          abs(df$diff[record1])/12*(j-1)
      }  
      else {
        df$val_interp[record] <-
          df$val_interp[record1] - 
          abs(df$diff[record1])/12*(j-1)
      }
    }
  }
  
  return(df)
}

new_I <- interpolation_v1(new_I)

C = data.frame(cbind(new_OG$val_interp, new_IR$val_interp - 2, new_I$val_interp))
colnames(C) <- c('OG', 'IR', 'I')

# PLOTS
#Monthly interest rate over time
plot(new_IR$val_interp ~ new_IR$Date, data = C, xlab = "Time", ylab = "Interest rate", pch = 20, cex = 0.3, col = 'darkgreen')
abline(h=0, col="black")

#Monthly interest rate and monthly output gap
plot(IR ~ OG, data = C, pch = 20, cex = 0.3, xlab = 'Output Gap', ylab = 'Interest rate')

#Monthly interest rate and monthly inflation
plot(IR ~ I, data = C, pch = 20, cex = 0.3, xlab = 'Inflation', ylab = 'Interest rate')

#Linear regression with the new database (2000-2021, monthly)
lr_2 = lm(IR ~ OG + I, data=C)
summary(lr_2)
plot(lr_2)

#### Testing OLS assumptions
#BP test
bptest(lr2)
#HAC-robust standard errors
coeftest(lr_2, vcov = vcovHAC)

#Breusch-Godfrey test.
bgtest(lr_2, order=3) 

#Reset test
resettest(lr_2, power=2, type="regressor")

#DW test
dwtest(lr_2)


                              ### NEW MODEL (w/ unemployment rate) ####

DF2 = data.frame(cbind(dataUR$Value - 4.5, dataIR$Value - 2, dataI$Value)) #4.5 natural rate of unemployment
colnames(DF2) <- c('UR', 'IR', 'I')

#PLOT 
plot(IR ~ UR, data = DF2, pch = 20)

lr_3 = lm(IR ~ UR + I, data=DF2)
summary(lr_3)
plot(lr_3)
avPlot(lr_3, variable = 'UR', col.lines = "darkgoldenrod2")

#### Testing OLS assumptions
#BP test
bptest(lr_3) 
#HAC-robust standard errors
coeftest(lr_3, vcov = vcovHAC)

#Breusch-Godfrey test.
bgtest(lr_3, order=3) 

#Reset test
resettest(lr_3, power=2, type="regressor")

#DW test
dwtest(lr_3)


