require('PerformanceAnalytics')
require('quantmod')
require('RColorBrewer')


#AA backtesting data
aa_data <- read.csv(file="../Downloads/AAEquityBacktesting_v2.csv",head = TRUE, sep=',')
simple_returns <- aa_data$"StrategyReturn"
prices1 <- buy_and_hold2(1, simple_returns)
prices <- buy_and_hold2(1, log_ret)
log_ret <- log(1 + simple_returns)
ann_retn <- 252*(sum(log_ret)/length(log_ret))

#Calculation of expected return.
hist_data2<- read.csv(file="../Downloads/CNX NIFTY07-04-1990-05-04-2000.csv",head = TRUE, sep=',')
close_prices2 <- hist_data2$"Close"
rethist<- ret(close_prices2)
Rn <- (1/length(rethist))*(sum(rethist))


hist_data <- read.csv(file="../Downloads/CNX NIFTY07-04-2000-07-04-2014.csv", head = TRUE, sep=',')
close_prices <- hist_data$"Close"
date <- hist_data$"Date"
#close_with_date <- c(close_prices, date)

#Calculation of the drawdown
drawdown <- function(vector)
{
dd <- rep(NA, length(vector))
M <- 0
for(i in 1:length(vector))
{
if (M < vector[i])
{
M <- vector[i]
}
dd[i] = (1 - vector[i]/M)*100
}
return(dd)
}

#Generating the price series from the simple returns. Returns are given in percentages.
sim_ret <- function(vector)
{
prices <- rep(100,(length(vector)+1))
vec <- c(0, vector)
for(i in 1:length(vec))
{
if(i ==1) {prices[i] <- 100}
else
{
prices[i] <- prices[i-1] + vec[i] 
}
}
return(prices)
} 

#Positive part function
ispositive <- function(value) 
{
if (value <= 0) {value <- 0}
return(value)
}

#Positive part on a vector
isposvec <- function(vector)
{
result <- rep(NA, length(vector))
for(i in 1:length(vector)) {result[i] <- ispositive(vector[i])}
return(result)
}

#Nonzero components of a vector

#Finding the largest component of a vector
maxval <- function(vector) 
{
M <- 0
pos <- 0
for(i in 1:length(vector))
{
if (M < vector[i]) 
{
M <- vector[i]
pos <- i
}
}
return(c(M, pos))
}

#Finding the smallest component of a vector
minval <- function(vector) 
{
M <- vector[1]
pos <- 1
for(i in 1:length(vector))
{
if (M > vector[i]) 
{
M <- vector[i]
pos <- i
}
}
return(c(M, pos))
}

#Calculates daily log return
ret <- function(vector)
{
vector1 <- vector[2:length(vector)]
vector2 <- vector[1:length(vector)-1]
result <- log((vector1/vector2))
return(result)
}


#Estimating volatility and drift.
#Returns (volatility, drift).
volat <- function(vector)
{
vector1 <- vector[2:length(vector)]
vector2 <- vector[1:length(vector)-1]
trend <- (1/(length(vector)-1))*sum(ret(vector))
vec <- ret(vector) - trend
sigma <- (1/(length(vector1)-1)*sum(vec*vec))
return(c(sigma, trend))
}

#Passive management using buy and hold strategy.
buy_and_hold <- function(amount, vector)
{
evol<- rep(amount, length(vector))
retn <- c(0,ret(vector))
for(i in 2:length(vector))
{
evol[i] <- evol[i-1]*exp(retn[i])
}
return(evol)
} 

#Passive management using buy and hold strategy when the log returns are given. 
buy_and_hold2 <- function(amount, ret_vector)
{
evol<- rep(amount, (length(ret_vector)+1))
retn <- c(0,ret_vector)
for(i in 2:length(retn))
{
evol[i] <- evol[i-1]*exp(retn[i])
}
return(evol)
} 


#Calculation of the R/sigma^2 ratio
sharpef <- function(vector, step)
{
sharpe <- rep(0, length(vector))
#for(i in seq(0,length(vector),step))
#{
#if (i>0) 
#{
#parameters <- volat(vector[(i-step+1):i])

#for(j in 1:step)
#{
#if(i+j <= length(vector))
#{
#sharpe[i+j] <- parameters[2]/parameters[1]
#}
#else {break}
#}
#}
#}
 
for(i in 1:length(vector))
{
if (i<=step) {sharpe[i] <- 0}
else 
{
parameters <- volat(vector[(i-step+1):i])
sharpe[i] <- parameters[2]/parameters[1]
}
}
return(sharpe)
}





#Calculation of the proportion alloted to the risky asset
prop <- function(level, vector)    
#level is the allowed drawdown limit, vector is the vector of stock prices
{
result <- rep(NA, length(vector))
ddmod <- drawdown(vector)/100
dd <- rep(0, length(vector))
retn <- c(0,ret(vector))
w <- rep(NA, length(vector))
sharpe <- sharpef(vector,250)
M <- 0
for(i in 1:length(vector))
{
if(i <= 250) { 
result[i] <- 0
w[i] <- 1
result[i] <- 0
dd[i] <- 0
}
else {
if(i == 251) {
dd[i] <- 0
result[i] <- ispositive((ispositive((sharpe[i]+0.5)/(1 - (level)^2)))*level)
w[i] <- 1
M <- 1 
}
else{
w[i] <- result[i-1]*w[i-1]*exp(retn[i]) + (1-result[i-1])*w[i-1]
if(M<=w[i]){M<-w[i]}
dd[i] = 1 - w[i]/M 
result[i] <- ispositive((ispositive((sharpe[i]+0.5)/(1 - (level)^2)))*((level - dd[i])/(1 - dd[i])))
}
}
}
return(result)
}



#for(i in seq(0, length(vector), 250))
#{
#if(i>0)
#{
#sharpe <- sharpef(vector[(i-250+1):i])
#}
#for(j in 1:250) 
#{
#if(i+j <= length(vector))
#{
#if(i==0) {result[i+j] <-  0}
#
#else
#{
#result[i+j] <- ispositive(((sharpe[i%/%250] + 0.5)/(1-(level)^2))*((level - ddmod[i+j])/(1 - ddmod[i+j])))
#}
#}
#else {break}
#}
#return(result)
#}
#}

#Calculation of the wealth evolution using REDD-COPS.

redd <- function(vector)
{
wealth <- rep(NA, length(vector))
retn <- c(0, ret(vector))
props<-prop(0.2, vector)
wealth[1]<-1
for(i in 1:(length(vector)-1))
{
wealth[i+1] <-(props[i]*wealth[i]*exp(retn[i+1]) + (1-props[i])*wealth[i]) 
}
return(wealth)
}


#Calculation of the drawdown, the proportion allotted and the wealth in a REDD-COPS active asset allocation process.

reddcops <- function(vector, delta, init_amt, step)
{
wealth <- rep(init_amt, length(vector))
prop <- rep(0, length(vector))
dd <- rep(0, length(vector))
sharpe <- sharpef(vector, step)
retn <- c(0,ret(vector))
M <- init_amt
result <- rep(NA, length(vector))

for (i in 1:length(vector))
{
if (i > step)
{
prop[i] <- ispositive(((sharpe[i] + 0.5)/(1 - delta^2))*((delta - dd[i])/(1 - dd[i])))

if (i < length(vector))
{
wealth[i+1] <- wealth[i]*prop[i]*exp(retn[i+1]) + (1-prop[i])*wealth[i]
if (M < wealth[i+1])
{
M <- wealth[i+1]
dd[i+1] <- 0
}
else {dd[i+1] <- 1 - wealth[i+1]/M}
} 
}
}
result <- as.matrix(cbind(wealth, dd, prop))
return(result)
}

#Calculation of the drawdown, the proportion allotted and the wealth in a REDD-COPS active asset allocation process. The proportion alloted to the risky asset is capped from below. 

c_reddcops <- function(vector, delta, init_amt, step, cap_level)
{
wealth <- rep(init_amt, length(vector))
prop <- rep(0, length(vector))
dd <- rep(0, length(vector))
sharpe <- sharpef(vector, step)
retn <- c(0,ret(vector))
M <- init_amt
result <- rep(NA, length(vector))

for (i in 1:length(vector))
{
if (i > step)
{
prop[i] <- ispositive((((sharpe[i] + 0.5)/(1 - delta^2))*((delta - dd[i])/(1 - dd[i]))) - cap_level) + cap_level

if (i < length(vector))
{
wealth[i+1] <- wealth[i]*prop[i]*exp(retn[i+1]) + (1-prop[i])*wealth[i]
if (M < wealth[i+1])
{
M <- wealth[i+1]
dd[i+1] <- 0
}
else {dd[i+1] <- 1 - wealth[i+1]/M}
} 
}
}
result <- as.matrix(cbind(wealth, dd, prop))
print(c(delta*100, cap_level*100, maxval(dd)[1]*100, volat(wealth)[2]*25200,(volat(wealth)[2]*25200)/(maxval(dd)[1]*100)))
plot(vector, type='o',pch='.',col='blue',axes=FALSE, ann=FALSE, ylim= c(0,3))
lines(wealth,type='o',pch='.',col='red')
lines(prop,type='o',pch='.',col='green')
axis(1,0:length(vector))
axis(2,0:ceiling(max(wealth)))
axis(4)
grid(22,9,lty=1)
#legend("topleft",c(AAbacktesting, paste("REDDCOPS_",delta*100),"x_t"), cex=1, col=c("blue","red","green"))
#title(main=paste("REDDCOPS_",delta*100,"%"),font.main=1)
return(result)
}

















