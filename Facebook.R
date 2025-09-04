#define pre and post period dates
start <- "2016-01-01"
treatment <- "2018-03-17"
end <- "2018-07-17"

#retrieve data
install.packages("tseries")
library(tseries)
Facebook <- get.hist.quote(instrument = "META", 
                           start = start, end = end, 
                           quote = "Close", 
                           compression = "w")  ##w = weekly data

Facebook

#taking stocks of other companies which are not technologically related to facebook (for control variable)
Amazon <- get.hist.quote(instrument = "AMZN", 
                           start = start, end = end, 
                           quote = "Close", 
                           compression = "w")

Disney <- get.hist.quote(instrument = "DIS", 
                           start = start, end = end, 
                           quote = "Close", 
                           compression = "w")

GeneralMotors <- get.hist.quote(instrument = "GM", 
                         start = start, end = end, 
                         quote = "Close", 
                         compression = "w")

Novartis <- get.hist.quote(instrument = "NVS", 
                         start = start, end = end, 
                         quote = "Close", 
                         compression = "w")
GoldmanSachs <- get.hist.quote(instrument = "GS", 
                           start = start, end = end, 
                           quote = "Close", 
                           compression = "w")
GeneralElectric <- get.hist.quote(instrument = "GE", 
                           start = start, end = end, 
                           quote = "Close", 
                           compression = "w")
Heinz <- get.hist.quote(instrument = "KHC", 
                           start = start, end = end, 
                           quote = "Close", 
                           compression = "w")
PepsiCo <- get.hist.quote(instrument = "PEP", 
                           start = start, end = end, 
                           quote = "Close", 
                           compression = "w")
McDonalds <- get.hist.quote(instrument = "MCD", 
                           start = start, end = end, 
                           quote = "Close", 
                           compression = "w")


#plotting data

##combining all data wrt date
series <- cbind(Facebook, Amazon, Disney, GeneralMotors, Novartis, 
                GoldmanSachs, GeneralElectric, Heinz, PepsiCo, McDonalds)
series <- na.omit(series) #omits null values

library(ggplot2)
autoplot(series, facet = NULL) + xlab("time") + ylab("price close")
##sudden dip in 2018 in the facebook graph indicates the time of the scandal, post which stock prices recovered almost immediately


#correlation check
dataset_cor <- window(series, start = start, end = treatment)
dataset_cor <- as.data.frame(dataset_cor) 
cor(dataset_cor)


#selecting final dataset using only variables with high correlation
Final_series <- cbind(Facebook, Amazon, GeneralMotors, GoldmanSachs, GeneralElectric, PepsiCo, McDonalds)
Final_series <- na.omit(Final_series)

#create pre and post periods to check for impact
pre.period <- as.Date(c(start, treatment))
post.period <- as.Date(c(treatment, end))

#check for causal impact
install.packages("CausalImpact")
library(CausalImpact)
impact <- CausalImpact(data = Final_series, pre.period = pre.period, post.period = post.period,
                       model.args = list(niter = 2000,
                                         nseasons = 52))
plot(impact)
##In the cummulative chart we can see a small dip followed by a sharp increase indicating a negative initial impact followed by a positive impact. the the scabdal had a positive impact on Meta's stock prices

summary(impact)
##absolute impact (average) = 5.3 dollars, relative impact = 3.2%

summary(impact, "report")


