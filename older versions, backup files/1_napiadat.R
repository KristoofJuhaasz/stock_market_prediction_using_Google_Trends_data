
#install.packages("tidyverse")
#install.packages("lattice")
#install.packages("tseries")
#install.packages("gtrendsR")
#install.packages("dplyr")
#install.packages("lubridate")

if (!require("devtools")) install.packages("devtools")
devtools::install_github('PMassicotte/gtrendsR')

library(lubridate)
library(tidyverse)
library(lattice)
library(tseries)
library(gtrendsR)
library(dplyr)
library(zoo)
library(plyr)




key_words <- c(
  "reforms", "boost", "consolidate", "construction", "outperform",
  "savings", "progress", "booming", "accrue", "surpassed",
  
  
  "debt", "crisis", "decline", "recession", "unemployment",
  "bankrupt", "deficit", "collapse", "market crash", "downturn",
  
  "banana", "door", "weather", "chopstick", "phone",
  "chicago", "christmas", "swim", "bycicle", "programming"
)

kw <- key_words


global_start_date <- dmy("1/1/2004")
global_end_date <- dmy("19/8/2020")
span <- 90

time_diff <-difftime(global_end_date, global_start_date, units = c("days")) %>% as.numeric()



for (w in 1:length(kw)) {
  for (i in 0:time_diff) {
    date_l <- global_start_date + days(i)
    #date_l <- global_start_date + days(i* (span/2) )
    date_u <- date_l + days(span-1)
    date_string <- paste(date_l, date_u, sep=" ")
    
    temp_df <- gtrendsR::gtrends(keyword = kw[w], geo = "US", time = date_string, 
                                 low_search_volume = FALSE, onlyInterest = TRUE)[1] %>% 
                data.frame(stringsAsFactors=TRUE)
    temp_df <- temp_df %>% select(1:2)
    names(temp_df) <- c("date", "hits")
    
    if (i==0) {
      gt_daily_df <- temp_df #elsõ df
      names(gt_daily_df) <- c("date", "hits_1")
    } else {
      gt_daily_df <- merge(x = temp_df, y = gt_daily_df, by = "date", all = TRUE) #hozzátoldás
    }
    
  }
  storage[w] <- gt_daily_df
  print(kw[w])
  print("done")
}







