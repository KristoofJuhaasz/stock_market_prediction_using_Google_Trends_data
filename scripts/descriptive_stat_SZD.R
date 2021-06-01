
library(curl)
library(gtrendsR)
library(magrittr) 
library(dplyr)
library(htmlwidgets)
library(scales)
library(reshape)
library(tidyverse)
library(RSQLite)
library(lattice)
library(corrplot)
library(ggthemes)
library(ggrepel)
library(quantmod)
library(PerformanceAnalytics)
library(ISLR)
library(ggplot2)
library(lmtest)
library(aTSA)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(here)
library(EnvStats)
library(rugarch)
library(GGally)
library(gtrendsR)
library(plyr)
library(lubridate)
library(dplyr)
library(gtools)
library(neuralnet)

results_df <- data.frame(matrix(NA, nrow = 12, ncol = 13))
names(results_df) <- c("ticker", "n", "start date", "end date", "mean", "std dev", "min", "1st Q", "median",  "3rd Q", "max",  "ACF(1)", "NAs")

fund_num <- 1

funds <- c('DJI',
            'VNQ', 'XLK', 'XLB', 'XLV', 'XLI', 'XLP', 'XLE', 'XLY', 'VOX', 'XLU', 'XLF')


for (fund_num in 1:12) {
  
  symbol <- funds[fund_num]  
  print(symbol)
  
  returns <- getSymbols(symbol, src = 'yahoo', auto.assign = FALSE, from = "2004-01-01", to = "2020-08-28")
  returns <- Return.calculate(returns, method = "log")
  returns <- returns[-1, ]              #remove first row
  returns <- data.frame(time = as.Date(index(returns)), coredata(returns))
  returns <- select(returns, 1,7)
  names(returns) <- c("time","y_lead")
  #returns <- mutate( returns, time = time-3 )
    
    results_df[fund_num,1] <- funds[fund_num] #ticker             
    results_df[fund_num,2] <- length(returns$y_lead) #n
    results_df[fund_num,3] <- as.character(returns$time[1]) #start date
    results_df[fund_num,4] <- as.character(returns$time[length(returns$y_lead)]) #end date
    results_df[fund_num,5] <- summary(returns$y_lead)[4] #mean
    results_df[fund_num,6] <- sd(returns$y_lead, na.rm=T) #std dev
    results_df[fund_num,7] <- summary(returns$y_lead)[1] #min
    results_df[fund_num,8] <- summary(returns$y_lead)[2] #1st Q
    results_df[fund_num,9] <- summary(returns$y_lead)[3] #median
    results_df[fund_num,10] <- summary(returns$y_lead)[5] #3rd Q
    results_df[fund_num,11] <- summary(returns$y_lead)[6] #Max
    results_df[fund_num,12] <- unlist(acf(returns$y_lead, lag.max=1, 
                                          plot=FALSE, na.action = na.pass))[2] #ACF 1
    results_df[fund_num,13] <- sum(is.na(returns$y_lead)) #NAs

}


write.csv(results_df, file = "des_stats_DJI_and_sectorial.csv", row.names=FALSE)




######################################################################################################



words <- c(
  "reforms", "boost", "consolidate", "construction", "outperform", 
  "savings", "progress", "booming", "accrue", "surpass",
  
  "debt", "crisis", "decline", "recession", "unemployment",
  "bankrupt", "deficit", "collapse", "market crash", "downturn",
  
  "banana", "door", "weather", "chopstick", "phone",
  "chicago", "christmas", "swim", "bycicle", "programming"
)

keyword_results_df <- data.frame(matrix(NA, nrow = 30, ncol = 13))
names(keyword_results_df) <- c("ticker", "n", "start date", "end date", "mean", "std dev", "min", "1st Q", "median",  "3rd Q", "max",  "ACF(1)", "NAs")

kw=5

for (kw in 1:30) {
  
  print(kw)
  
  kw_file_name <- paste( words[kw], "_stdfinaldf.csv", sep="")
  keywords <- read.csv(file= kw_file_name, header=TRUE, sep=",") %>% select(1:2)
  names(keywords) <- c("time", "data")
  keywords <- mutate(keywords, time = as.Date(time, format="%Y-%m-%d"))
  keywords <- filter(keywords, time<=as.Date("2020-08-28"))

  keyword_results_df[kw,1] <- words[kw] #ticker             
  keyword_results_df[kw,2] <- length(keywords$data) #n
  keyword_results_df[kw,3] <- as.character(keywords$time[1]) #start date
  keyword_results_df[kw,4] <- as.character(keywords$time[length(keywords$data)]) #end date
  keyword_results_df[kw,5] <- summary(keywords$data)[4] #mean
  keyword_results_df[kw,6] <- sd(keywords$data, na.rm=T) #std dev
  keyword_results_df[kw,7] <- summary(keywords$data)[1] #min
  keyword_results_df[kw,8] <- summary(keywords$data)[2] #1st Q
  keyword_results_df[kw,9] <- summary(keywords$data)[3] #median
  keyword_results_df[kw,10] <- summary(keywords$data)[5] #3rd Q
  keyword_results_df[kw,11] <- summary(keywords$data)[6] #Max
  keyword_results_df[kw,12] <- unlist(acf(keywords$data, lag.max=1, 
                                          plot=FALSE, na.action = na.pass))[2] #ACF 1
  keyword_results_df[kw,13] <- sum(is.na(keywords$data)) #NAs

}

keyword_results_df
write.csv(keyword_results_df, file = "des_stats_keywords.csv", row.names=FALSE)
