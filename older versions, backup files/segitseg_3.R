
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
                    "2004-01-01 2019-12-31"
)

key_words <- c("reforms", 
               "boost", 
               "consolidate", 
               "construction", 
               "stabilized",
               
               "liberalize", 
               "modernization", 
               "outperform", 
               "savings", 
               "progress"
)

kw <- key_words

t = test_dates  


for ( i in 1:20 ) {
  
  print(i)
  print(kw[1])
  gt_monthly <- gtrendsR::gtrends(keyword = kw[1], geo = "US", time = t[1], low_search_volume = FALSE)[1] %>% 
    data.frame(stringsAsFactors=TRUE)
  gt_monthly <- gt_monthly %>% select(1:2)
  names(gt_monthly) <- c("time","debt")
  for ( w in 2:length(kw) ) {
    print(kw[w])
    df <- gtrendsR::gtrends(keyword = kw[w], geo = "US", time = t[1], low_search_volume = FALSE)[1] %>% 
      data.frame(stringsAsFactors=TRUE)
    df <- df %>% select(1:2)
    names(df) <- c("time", as.character(kw[w]) )
    gt_monthly <- merge(gt_monthly, df, by="time", all.x = TRUE)
  }
  
  
  file_name <- paste(as.character(i), "_monthly_positive.csv", sep="")
  write.csv(gt_monthly, file = file_name, row.names=FALSE)
  
  print( paste(file_name, "  is ready", sep="") )
  
  Sys.sleep(3800)
}


