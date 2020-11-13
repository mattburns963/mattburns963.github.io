library(readxl)
library(caret)
library(Rmisc)
library(ggplot2)
library(forecastML)
library(effects)
library(DataCombine)
require("forecast")
require("expsmooth") # required for the data
library(smooth)


# pull in monthly economic data from Jan. 2002 thru Nov. 2019
econ <- read_excel("C:/Users/burns/OneDrive/Desktop/Matt/Grad School/DSC 630/Project/Economic Data.xlsx")
econ <- econ[-c(216), ]
typeof(econ)
class(econ)


# pull in monthly retail data from Jan. 2002 thru Nov. 2019
retail <- read_excel("C:/Users/burns/OneDrive/Desktop/Matt/Grad School/DSC 630/Project/Retail.xlsx")
typeof(retail)
class(retail)


# check that dates align and merge dataframes
dates <- data.frame(e1=econ$Date,e3=econ[3],e5=econ[5],r1=retail$Date)
df<-merge(x=econ,y=retail,by="Date",all=TRUE)

# knock out unnecessary date columns
df[3] <- NULL
df[4] <- NULL
df[5] <- NULL


# fix variable names
names(df)[2]<-"GasSales"
names(df)[3]<-"Payrolls"
names(df)[5]<-"CPI"
names(df)[6]<-"CPIfoodbev"
names(df)[8]<-"Brent"
names(df)[13]<-"MortPayments"

df[!complete.cases(df),]


# Plot Canidate Explanatory Variables
ggplot(df, aes(x=Date, y=GasSales, color="Red")) + ggtitle("Gasoline Sales") + 
  geom_point() + theme(legend.position="none")
ggplot(df, aes(x=Date, y=Brent, color="#E69F00")) + ggtitle("Brent Prices") + 
  geom_point() + theme(legend.position="none")
ggplot(df, aes(x=Date, y=Payrolls, color="#E69F00")) + ggtitle("Payrolls") + 
   geom_point() + theme(legend.position="none")
ggplot(df, aes(x=Date, y=CPI, color="#E69F00")) + ggtitle("CPI") + 
   geom_point() + theme(legend.position="none")
ggplot(df, aes(x=Date, y=MortPayments, color="#E69F00")) + ggtitle("Mortgage Payment") + 
   geom_point() + theme(legend.position="none")



# Plot retail sales
ggplot(retail, aes(Date, y = value, color = variable)) + 
  geom_point(aes(y = Retail, col = "Retail")) + 
  geom_point(aes(y = Retail_and_Food, col = "Retail & Food")) +
  geom_point(aes(y = Retail_and_Food_no_gas, col = "Retail & Food (no gas)")) +
  theme(legend.position="bottom")

# Break apart the componets of retail sales
retailtimeseries <- ts(retail$Retail_and_Food, frequency=12, start=c(2002,1))
retailcomponents <- decompose(retailtimeseries)
plot(retailcomponents)

# Test 3 candidate models
multi.fit1 = lm(Retail_and_Food~Payrolls+Brent+MortPayments+GasSales+CPI, data=df)
summary(multi.fit1)

multi.fit2 = lm(Retail_and_Food~Payrolls+Brent+MortPayments+CPI, data=df)
summary(multi.fit2)

multi.fit3 = lm(Retail_and_Food~Payrolls+Brent+MortPayments+CPIfoodbev, data=df)
summary(multi.fit3)

eff.fit1 <- allEffects(multi.fit1, xlevels=50)
for(i in 1:5) {plot(eff.fit1[i])}

plot(resid(multi.fit1), type="l")  

# Exponetial Smooth Predictors
pes <- es(df$Payrolls, "ZZZ", h=36)
plot(pes)
p <- predict(pes)

bes <- es(df$Brent, "ZZZ", h=18)
plot(bes)
b <- predict(bes)

mpes <- es(df$MortPayments, "ZZZ", h=18)
plot(mpes)
m <- predict(mpes)

gses <- es(df$GasSales, "ZZZ", h=18)
plot(gses)
gs <- predict(gses)

cpies <- es(df$CPI, "ZZZ", h=18)
plot(cpies)
c <- predict(cpies)

drivers <- data.frame(cbind(p,b,m,gs,c))

df_lag1 <- as.data.frame(df)
df_lag1 <- slide(df_lag1, "Brent", NewVar = "BrentLag1", slideBy = -1)  # create lag1 variable
multi.fit1.lag1 = lm(Retail_and_Food~Payrolls+BrentLag1+MortPayments+GasSales+CPI, data=df_lag1)
summary(multi.fit1.lag1)

df_lag2 <- as.data.frame(df)
df_lag2 <- slide(df_lag2, "Brent", NewVar = "BrentLag2", slideBy = -2)  # create lag2 variable
multi.fit1.lag2 = lm(Retail_and_Food~Payrolls+BrentLag2+MortPayments+GasSales+CPI, data=df_lag2)
summary(multi.fit1.lag2)

df_lag3 <- as.data.frame(df)
df_lag3 <- slide(df_lag3, "Brent", NewVar = "BrentLag3", slideBy = -3)  # create lag3 variable
multi.fit1.lag3 = lm(Retail_and_Food~Payrolls+BrentLag3+MortPayments+GasSales+CPI, data=df_lag3)
summary(multi.fit1.lag3)

eff.fitlag <- allEffects(multi.fit1.lag1, xlevels=50)
for(i in 1:5) {plot(eff.fitlag[i])}

plot(resid(multi.fit1.lag1), type="l")  
plot(resid(multi.fit1), type="l")  

plot(resid(multi.fit1),type="l",col="red")
lines(resid(multi.fit1.lag1),col="purple")

rm(df_lag2)
rm(df_lag3)


fit2 <- predict(multi.fit1)
df <- cbind(df, fit2)
ggplot(df, aes(Date, y = value, color = variable)) + 
  geom_point(aes(y = Retail_and_Food, col = "Retail & Food Actual")) +
  geom_line(aes(y = fit2, col = "Retail & Food Fitted")) +
  theme(legend.position="bottom")

fcst <- read_excel("C:/Users/burns/OneDrive/Desktop/Matt/Grad School/DSC 630/Project/Fcst.xlsx")
ggplot(fcst, aes(Date, y = value, color = variable)) + 
  geom_point(aes(y = Actual, col = "Retail & Food Actual")) +
  geom_line(aes(y = Fcst, col = "Retail & Food Fitted & Forecast")) +
  theme(legend.position="bottom")
