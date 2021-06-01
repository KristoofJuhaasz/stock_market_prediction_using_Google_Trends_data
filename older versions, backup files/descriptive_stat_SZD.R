
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
  
    sec_file_name <- paste( funds[fund_num], ".csv", sep="")
    security <- read.csv(file= sec_file_name, header=TRUE, sep=",") %>% select(Date, Adj.Close)
    rownames(security) = security[[1]]
    security <- select(security, Adj.Close)
    security <- as.xts(security)
    returns <- Return.calculate(security, method = "log")
    returns <- returns[-1, ]              #remove first row
    returns <- data.frame(time = as.Date(index(returns)), coredata(returns))  %>% select(time, Adj.Close)
    returns <- mutate(returns, Adj.Close_lead = lead(Adj.Close, 3)) #3-as lead
    returns <- returns[-( nrow(returns):(nrow(returns)-2 )) ,]   #remove last 3 rows
    returns <- returns %>% select(time, Adj.Close_lead)  
    names(returns) <- c("time","y_lead")
    
    results_df[fund_num,1] <- funds[fund_num] #ticker             
    results_df[fund_num,2] <- length(returns$y_lead) #n
    results_df[fund_num,3] <- as.character(returns$time[1]) #start date
    results_df[fund_num,4] <- as.character(returns$time[length(returns$y_lead)]) #end date
    results_df[fund_num,5] <- summary(returns$y_lead)[4] #mean
    results_df[fund_num,6] <- sd(returns$y_lead) #std dev
    results_df[fund_num,7] <- summary(returns$y_lead)[1] #min
    results_df[fund_num,8] <- summary(returns$y_lead)[2] #1st Q
    results_df[fund_num,9] <- summary(returns$y_lead)[3] #median
    results_df[fund_num,10] <- summary(returns$y_lead)[5] #3rd Q
    results_df[fund_num,11] <- summary(returns$y_lead)[6] #Max
    results_df[fund_num,12] <- unlist(acf(returns$y_lead, lag.max=1, plot=FALSE))[2] #ACF 1
    results_df[fund_num,13] <- sum(is.na(returns$y_lead)) #NAs

}


write.csv(results_df, file = "des_stats_DJI_and_sectorial.csv", row.names=FALSE)




######################################################################################################



words <- c(
  "reforms", "boost", "consolidate", "construction", "outperform",
  "savings", "progress", "booming", "accrue", "surpassed",
  
  
  "debt", "crisis", "decline", "recession", "unemployment",
  "bankrupt", "deficit", "collapse", "market crash", "downturn",

   "banana", "door", "weather", "chopstick", "phone",
   "chicago", "christmas", "swim", "bycicle", "programming"
)

keyword_results_df <- data.frame(matrix(NA, nrow = 30, ncol = 13))
names(keyword_results_df) <- c("ticker", "n", "start date", "end date", "mean", "std dev", "min", "1st Q", "median",  "3rd Q", "max",  "ACF(1)", "NAs")


for (kw in 1:30) {

  kw_file_name <- paste( words[kw], "_final.csv", sep="")
  keywords <- read.csv(file= kw_file_name, header=TRUE, sep=",") %>% select(1:2)
  names(keywords) <- c("time", "data")

  keyword_results_df[kw,1] <- words[kw] #ticker             
  keyword_results_df[kw,2] <- length(keywords$data) #n
  keyword_results_df[kw,3] <- as.character(keywords$time[1]) #start date
  keyword_results_df[kw,4] <- as.character(keywords$time[length(keywords$data)]) #end date
  keyword_results_df[kw,5] <- summary(keywords$data)[4] #mean
  keyword_results_df[kw,6] <- sd(keywords$data) #std dev
  keyword_results_df[kw,7] <- summary(keywords$data)[1] #min
  keyword_results_df[kw,8] <- summary(keywords$data)[2] #1st Q
  keyword_results_df[kw,9] <- summary(keywords$data)[3] #median
  keyword_results_df[kw,10] <- summary(keywords$data)[5] #3rd Q
  keyword_results_df[kw,11] <- summary(keywords$data)[6] #Max
  keyword_results_df[kw,12] <- unlist(acf(keywords$data, lag.max=1, plot=FALSE))[2] #ACF 1
  keyword_results_df[kw,13] <- sum(is.na(keywords$data)) #NAs

}

keyword_results_df
write.csv(keyword_results_df, file = "des_stats_keywords.csv", row.names=FALSE)
