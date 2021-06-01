
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
library(lubridate)
library(stats)
library(DescTools)

install.packages("DescTools")



############## returns ##############

#returns <- read.csv(file= "VIX.csv", header=TRUE, sep=",") %>% select(Date, Adj.Close)
returns <- read.csv(file= "DJI.csv", header=TRUE, sep=",") %>% select(Date, Adj.Close)
rownames(returns) = returns[[1]]
returns <- select(returns, Adj.Close)
returns <- as.xts(returns)
returns <- Return.calculate(returns, method = "log")
returns <- returns[-1, ]              #remove first row
returns <- data.frame(time = as.Date(index(returns)), coredata(returns))  %>% select(time, Adj.Close)
names(returns) <- c("date","y_lead")
returns <- mutate( returns, date = date-3 )


print(sum(returns$y_lead, na.rm=TRUE))




for (w in 1:length(key_word)) {
  
  dates_vector <- seq(as.Date('2004-01-01'), as.Date('2020-08-28'), by = 'days')
  dates_vector <- data.frame(dates_vector)
  names(dates_vector) <- "date"
  
  file_name <- paste(key_word[w], "_stdfinaldf.csv", sep="")
  std_final_df <- read.csv2(file_name, header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE)
  std_final_df <- mutate(std_final_df, date = as.Date(date))
  
  dates_vector <- merge(dates_vector, std_final_df, by="date", all.x=TRUE, sort=TRUE)
  dates_vector <- merge(dates_vector, returns, by="date", all.x=TRUE, sort=TRUE)
  
  dates_vector <- mutate(dates_vector, hits = (hits - mean(hits, na.rm=T)) / sd(hits, na.rm=T) )
  
  print(key_word[w])
  
  for (i in 3:30) {
    temp_df <- dates_vector
    temp_df <- mutate( temp_df, y_lead = lead(y_lead,i) )
    correlation <- cor(temp_df[,2], temp_df[,3], use="complete.obs")
    
    if (abs(correlation) > 0.02) {
      print(i)
      print(correlation)
    }

  }
}





############## raw data manipulation ##############
key_word <- c(
   "reforms", "boost", "consolidate", "construction", "outperform", 
   "savings", "progress", "booming", "accrue", "surpass",

  "debt", "crisis", "decline", "recession", "unemployment",
  "bankrupt", "deficit", "collapse", "market crash", "downturn",
  
  "banana", "door", "weather", "chopstick", "phone",
  "chicago", "christmas", "swim", "bycicle", "programming"
)

############## raw data manipulation ##############
#key_word <- c("booming")
#days <- c(6,13,20,27,34,41,48,55,62,69,76,83)


result_df <- data.frame(matrix(NA, ncol = 8, nrow = 30))

for (w in 18:length(key_word)) {

  file_name <- paste(key_word[w], "_raw.csv", sep="")
  raw_df <- read.csv2(file_name, header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE)
  
  #filter date until 2020-08-31
  raw_df <- mutate(raw_df, select_ind = if_else(date == "2020-08-31", 1, 0) )
  raw_df <- mutate(raw_df, cums_sum_select_ind = cumsum(select_ind) )
  raw_df <- mutate(raw_df, cums_sum_select_ind = lag(cums_sum_select_ind) )
  raw_df$cums_sum_select_ind[1] <- 0
  raw_df <- filter(raw_df, cums_sum_select_ind == 0)
  raw_df <- select(raw_df, 1:2)
  raw_df <- mutate(raw_df, date =as.Date(date))
  
  #break up into parts
  raw_df <- mutate(raw_df, break_ind = if_else( date < lag(date), 1, 0) )
  raw_df$break_ind[1] <- 0
  raw_df <- mutate(raw_df, break_ind = cumsum(break_ind) )
  timelength <- max(raw_df$break_ind) #for end of cycle
  
  #fix missing values
  start_date <- as.Date('2004-01-01')
  
  

  
  for (i in 0:timelength) {
    
    temp_dates_df <- filter(raw_df, break_ind == i)
    temp_dates_df <- select(temp_dates_df, 1:2)
    
    if ( nrow(temp_dates_df) < 90 ) {
      
      #print(nrow(temp_dates_df))
      #print(i)
      
      l_date <- start_date + i
      u_date <- l_date + 90-1
      
      static_dates <- seq(l_date, u_date, by = "days")
      static_dates <- data.frame(static_dates)
      names(static_dates) <- c("date")
      
      temp_dates_df <- merge(static_dates, temp_dates_df, by="date", all.x=TRUE, sort=TRUE)
      
      temp_dates_df <- na.locf(temp_dates_df, na.rm=FALSE, fromLast=FALSE) #fill forward
      temp_dates_df <- na.locf(temp_dates_df, na.rm=FALSE, fromLast=FALSE) #fill backward the first
      
    }
    
    temp_dates_df <- mutate(temp_dates_df, break_ind = i)
    
    if (i==0) {
      final_df <- temp_dates_df
    } else {
      final_df <- rbind(final_df, temp_dates_df, stringsAsFactors=FALSE)
    }
  
  }
  
  
  print(w)
  print(key_word[w])
  new_file_name <- paste(key_word[w], "_finaldf.csv", sep="")
  write.csv(final_df, file = new_file_name, row.names=FALSE)
  print("done")
} #ezt majd törölni  
  


############## standardized time series ##############

for (w in 1:length(key_word)) {
  
  file_name <- paste(key_word[w], "_finaldf.csv", sep="")
  final_df <- read.csv2(file_name, header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE)
  final_df <- mutate(final_df, hits = as.numeric(hits))
  final_df[final_df=="<1"] <- 1

  std_final_df <- filter(final_df, break_ind == 0)
  std_final_df <- select(std_final_df, 1:2)
  
  for (i in 0:timelength) {
    
    temp_df_1 <- filter(final_df, break_ind == i)
    temp_df_1 <- select(temp_df_1, 1:2)
    
    temp_df_2 <- filter(final_df, break_ind == i+1)
    temp_df_2 <- select(temp_df_2, 1:2)
    
    temp_final_df <- merge(temp_df_1, temp_df_2, by="date", all =TRUE, sort=TRUE)
    temp_final_df <- mutate(temp_final_df, ratio = ifelse( is.na(hits.x) | hits.x==0 | is.na(hits.y) | hits.y==0, 
                                                           NA, hits.x/hits.y) )
    ratio <- mean(temp_final_df$ratio, na.rm=TRUE)
    temp_final_df[nrow(temp_final_df),2] <- ratio * temp_final_df[nrow(temp_final_df),3]
    temp_final_df <- temp_final_df[nrow(temp_final_df),(1:2)]
    names(temp_final_df) <- c("date", "hits")
    
    std_final_df <- rbind(std_final_df, temp_final_df, stringsAsFactors=FALSE)
    
    #print(i)
    
  }
  
  print(w)
  print(key_word[w])
  new_file_name <- paste(key_word[w], "_stdfinaldf.csv", sep="")
  write.csv(std_final_df, file = new_file_name, row.names=FALSE)
  print("done")
  print(head(std_final_df))
  print(tail(std_final_df))
  
}



