
install.packages("tidyverse")
install.packages("lattice")
install.packages("tseries")
install.packages("gtrendsR")
install.packages("dplyr")

library(tidyverse)
library(lattice)
library(tseries)
library(gtrendsR)
library(dplyr)


test_dates = c(
  "2004-01-01 2019-12-31",
  "2004-01-01 2018-12-31",
  "2004-01-01 2017-12-31",
  "2004-01-01 2016-12-31",
  "2004-01-01 2015-12-31",
  "2004-01-01 2014-12-31",
  "2004-01-01 2013-12-31",
  "2004-01-01 2012-12-31",
  "2004-01-01 2011-12-31",
  "2004-01-01 2010-12-31",
  "2004-01-01 2009-12-31"
)

key_words <- c("debt", "crisis", "decline", "recession", "unemployment", 
               "bankrupt", "deficit", "collapse", "market crash", "downturn",
               
               "banana", "door", "weather", "chopstick", "phone", 
               "chicago", "christmas", "swim", "bycicle", "programming",
               
               "reforms", "boost", "consolidate", "construction", "stabilized",
               "liberalize", "modernization", "outperform", "savings", "progress",
               "booming", "accrue", "surpassed"
               )
    
for (j in 1:33) {    
    
    kw = key_words[j]
    print(kw)
    
    t = test_dates  
    
    first_month <- t[1] %>% substr(1, 10)
    last_month <- t[1] %>% substr(12, 21)
    year <- t[1] %>% substr(12, 15)
    gt_monthly <- gtrendsR::gtrends(keyword = kw, geo = "US", time = paste(first_month, last_month, sep=" "), 
                                    low_search_volume = FALSE)[1] %>% data.frame(stringsAsFactors=TRUE)
    gt_monthly <- gt_monthly %>% select(1:2)
    names(gt_monthly) <- c("time", as.character(year) )
    print(year)
    
    for (i in 2:11) {
      first_month <- t[i] %>% substr(1, 10)
      last_month <- t[i] %>% substr(12, 21)
      year <- t[i] %>% substr(12, 15)
      df <- gtrendsR::gtrends(keyword = kw, geo = "US", time = paste(first_month, last_month, sep=" "), 
                                      low_search_volume = FALSE)[1] %>% data.frame(stringsAsFactors=TRUE)
      df <- df %>% select(1:2)
      names(df) <- c("time", as.character(year) )
      gt_monthly <- merge(gt_monthly, df, by="time", all.x = TRUE)
      print(year)
    }
    
    
    file_name <- paste(as.character(kw), "monthly.csv", sep="_")
    write.csv(gt_monthly, file = file_name, row.names=FALSE)
    
    print("file is ready")
}
    
    #warnings()



