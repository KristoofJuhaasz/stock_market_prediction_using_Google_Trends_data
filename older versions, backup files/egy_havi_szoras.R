
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
                    "2004-01-01 2009-06-30"
)

kw="debt"

t = test_dates  

gt_daily <- gtrendsR::gtrends(keyword = kw, geo = "US", time = t[1], low_search_volume = FALSE)[1] %>% 
  data.frame(stringsAsFactors=TRUE)
gt_daily <- gt_daily %>% select(1:2)
names(gt_daily) <- c("time","1")
print(1)
for (i in 2:3) {
  print(i)
  df <- gtrendsR::gtrends(keyword = kw, geo = "US", time = t[1], low_search_volume = FALSE)[1] %>% 
    data.frame(stringsAsFactors=TRUE)
  df <- df %>% select(1:2)
  names(df) <- c("time", as.character(i) )
  gt_daily <- merge(gt_daily, df, by="time", all.x = TRUE)
}


file_name <- "egy_havi_szoras.csv"
write.csv(gt_daily, file = file_name, row.names=FALSE)

print("file is ready")




