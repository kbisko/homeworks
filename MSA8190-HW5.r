rm(list=ls())
setwd("H:/Desktop/School/MSA8190")

install.packages("rmarkdown")
install.packages("xlsx")
library("xlsx")
install.packages("plyr")
library("plyr")

### Problem 1
The percentage of cotton in material used to manufacture mens shirts follows.
#df <- read.xlsx("HW5_data",sheetIndex = 1) 
df<-c(34.2,33.1,34.5,35.6,36.3,35.1,34.7,33.6,37.8,36.6,35.4,34.6,33.8,37.1,34,34.1,33.6,34.7,35,35.4,36.2,36.8,35.1,35.3,32.6,33.1,34.6,35.9,34.7,33.6,32.9,33.5,33.8,34.2,33.4,34.7,34.6,35.2,35,34.9,35.8,37.6,37.3,34.6,35.5,32.8,32.1,34.5,34.7,33.6,32.5,34.1,35.1,36.8,37.9,36.4,34.6,33.6,34.1,34.7,35.7,36.8,34.3,32.7)

(a) Compute the sample mean, variance and median.
mu<-mean(df)
var<-var(df)
med<-median(df)
mu
var
med
(b) Construct a stem-and-leaf display for the data.
stem(df)
(c) Construct a frequency distribution and histogram for the cotton content.
factordf<-factor(cut(df, breaks=c(32, 33, 34, 35, 36, 37, 38)))
dffd<-as.data.frame(table(factordf))
dffd
hist(df)
(d) Construct a box plot of the data and comment on the information in this display.
boxplot(df)
There are no outliers in the data set. 
It seems to be fairly evenly spread with the second quantile being midway between the first and third quantiles. 
There is a slightly larger range in the upper whisker than the lower.  

### Problem 2
The following data are the viscosity measurements for a chemical product observed hourly
(read down, then left to right).
#df2 <- read.xlsx("HW5_data",sheetIndex = 2) 
df2<-c(47.9,47.9,48.6,48,48.4,48.1,48,48.6,48.8,48.1,48.3,47.2,48.9,48.6,48,47.5,48.6,48,47.9,48.3,48.5,48.1,48,48.3,43.2,43,43.5,43.1,43,42.9,43.6,43.3,43,42.8,43.1,43.2,43.6,43.2,43.5,43)

(a) Construct and interpret either a digidot plot or a separate stem-and-leaf and time series plot of these data.
stem(df2)
plot(ts(df2))
The viscosity remains remotely steady around 48 until the 25 hour when it plummets drastically to an average of 43.

(b) Specifications on product viscosity are at 48 ¡Ó 2. What conclusions can you make about process performance?
Until the 25th hour, the process falls within the product specifications. After the 25th hour the product is no longer acceptable. 

### Problem 8
Regression methods were used to analyze the data from a study investigating the relationship
between roadway surface temperature (x) and pavement deflection (y). Summary quantities were:
n<-20
yi<-12.7
yi2<-8.8
xi<-1487
xi2<-143215
xiyi<-1083

(a) Calculate the least squares estimates of the slope and intercept. Graph the regression line.
A<-matrix(c(xi2, xi, xi, 1), 2, 2)
A
B<-matrix(c(xiyi, yi),2,1)
B
c<-solve(A, B)
slope<-c[1]
intercept<-c[2]
fx8<- function(x) slope*x+intercept
plot(fx8)
(b) Use the equation of the fitted line to predict what pavement deflection would be observed when the surface temperature is 85 ???F.
fx8(85)
(c) What is the mean pavement deflection when the surface temperature is 90???F?
fx8(90)
(d) What change in mean pavement deflection would be expected for a 1???F change in surface temperature?
an increase of 
slope
in the deflection

### Problem 9
A rocket motor is manufactured by bonding together two types of propellants, an igniter
and a sustainer. The shear strength of the bond y is thought to be a linear function of the
age of the propellant x when the motor is cast. Twenty observations are shown in the table
below (Excel file also uploaded in D2L).
(a) Draw a scatter diagram of the data. Does the straight-line regression model seem to be plausible? Yes, with a negative slope
#strength <- read.xlsx("HW5_data",sheetIndex = 3, col=1) 
#age <- read.xlsx("HW5_data",sheetIndex = 3, col=2) 
strength<-c(2158.7,1678.15,2316,2061.3,2207.5,1708.3,1784.7,2575,2357.9,2277.7,2165.2,2399.55,1779.8,2336.75,1765.3,2053.5,2414.4,2200.5,2654.2,1753.7)
age<-c(15.5,23.75,8,17,5,19,24,2.5,7.5,11,13,3.75,25,9.75,22,18,6,12.5,2,21.5)
plot(age,strength)
cor(age, strength)
(b) Find the least squares estimates of the slope and intercept in the simple linear regression model. Find an estimate of £m2.
lmsum<-summary(lm(strength~age))
lmsum
sigmasq<-sum(resid(lmsum)^2)/18
sigmasq
fx<- function(x) -36.962*x+2625.385
abline(lm(strength~age))
(c) Estimate the mean shear strength of a motor made from propellant that is 20 weeks old.
fx(20)
