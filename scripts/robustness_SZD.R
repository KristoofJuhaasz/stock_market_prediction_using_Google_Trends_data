
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
library(quantmod)
library(dplyr)




############## raw data manipulation ##############
key_word <- c(
  "reforms", "boost", "consolidate", "construction", "outperform", 
  "savings", "progress", "booming", "accrue", "surpass",
  
  "debt", "crisis", "decline", "recession", "unemployment",
  "bankrupt", "deficit", "collapse", "market crash", "downturn",
  
  "banana", "door", "weather", "chopstick", "phone",
  "chicago", "christmas", "swim", "bycicle", "programming"
)


symbols <- c(
            "VNQ",
            "XLK",
            "XLB",
            "XLV",
            "XLI",
            "XLP",
            "XLE",
            "XLY",
            "VOX",
            "XLU",
            "XLF"
            )




for (s in 4:length(symbols)) {
  
  
############## returns ##############

  
symbol <- symbols[s]  
print(symbol)


returns <- getSymbols(symbol, src = 'yahoo', auto.assign = FALSE, from = "2004-01-01", to = "2020-08-28")
returns <- Return.calculate(returns, method = "log")
returns <- returns[-1, ]              #remove first row
returns <- data.frame(time = as.Date(index(returns)), coredata(returns))
returns <- select(returns, 1,7)
names(returns) <- c("date","y_lead")
returns <- mutate( returns, date = date-3 )


print(sum(returns$y_lead, na.rm=TRUE))




############## STANDARDIZED DATA MANIPULATION ############## 

ultimate_result_df <- returns

days <- 181

for (w in 1:length(key_word)) {
  
  file_name <- paste(key_word[w], "_stdfinaldf.csv", sep="")
  std_final_df <- read.csv2(file_name, header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE)
  
  std_final_df <- mutate(std_final_df, date = as.Date(date))
  std_final_df <- mutate(std_final_df, hits = as.numeric(hits))
  std_final_df <- merge(std_final_df, returns, by="date", all=FALSE, sort=TRUE) #inner join
  std_final_df$y_lead[is.na(std_final_df$y_lead)] <- 0
  
  for (y in 3:3) { #heteket rögzítjük 3-ban
    
    hetek <- y
    
    final_test_df <- std_final_df
    final_test_df <- mutate(final_test_df, position_ind = NA)
    cycle_length <- nrow(final_test_df) - days - 1
    
    
    for (x in 0:cycle_length) {
      
      
      start_day <- 1
      end_day <- x+1+days
      
      temp_df <- std_final_df[(start_day:end_day),]
      #temp_df[temp_df == 0] <- NA
      
      #position <- strat_PREIS(temp_df, hetek, elojel_1[w]) #step 1 - preis
      #position <- strat_PREIS(temp_df, hetek, elojel_2[w]) #step 2 - preis + sentiment
      
      
      t_stat <- irany_check(temp_df)[1]
      
      
      if (is.na(t_stat)) {elojel <- 0} else {
        if ( t_stat > 0) {
          elojel <- 1
        } else { 
          elojel <- -1
        }
      }
      
      position <- strat_PREIS_linreg(temp_df, hetek, elojel) * abs(t_stat)
      
      if(position==0 | is.na(position)) {position <- NA}
      
      final_test_df[end_day,ncol(final_test_df)] <- position
      #print(x)
      
    }
    
    
    #final_test_df$position_ind <- na.locf(final_test_df$position_ind, na.rm=FALSE)
    
    final_test_df <- mutate(final_test_df, strat_return = position_ind * y_lead )
    cum_return <- sum(final_test_df$strat_return, na.rm=TRUE)
    
    #result_df2[w,y+1] <- cum_return
    print(y)
  }  
  
  
  final_test_df <- dplyr::select(final_test_df, 1, 4)
  ultimate_result_df <- merge(ultimate_result_df, final_test_df, by="date", all.x=TRUE, sort=TRUE)
  names(ultimate_result_df)[length(names(ultimate_result_df))] <- c(key_word[w])
  
  
  print(w)
  print(key_word[w])
  #result_df2[w,1] <- key_word[w]
  
  
}


output_file_name <- paste(symbol, ".csv", sep="")
write.csv(ultimate_result_df, file = output_file_name, row.names=FALSE)

}



date_to <- c(
  "2020-08-24",
  "2020-08-24",
  "2020-08-24",
  "2020-08-24",
  "2020-08-24",
  "2020-08-24",
  "2020-08-24",
  "2020-08-24",
  "2020-08-24",
  "2020-08-24",
  "2020-08-24"
)

date_from <- c(
  "2005-06-14",
  "2004-09-19",
  "2004-09-19",
  "2004-09-19",
  "2004-09-19",
  "2004-09-19",
  "2004-09-19",
  "2004-09-19",
  "2005-06-14",
  "2004-09-19",
  "2004-09-19"
)




for (s in 1:length(symbols)) {
  
  
  ############## returns ##############
  
  
  symbol <- symbols[s]  
  print(symbol)
  
  date_start <- as.Date(date_from[s])
  data_final <- as.Date(date_to[s])
  
  
  returns <- getSymbols(symbol, src = 'yahoo', auto.assign = FALSE, from = date_start, to = data_final)
  returns <- returns[,6]
  returns <- data.frame(time = as.Date(index(returns)), coredata(returns))
  last_row <- nrow(returns)
  cum_yield <- log( returns[last_row,2] / returns[1,2] )
  print(cum_yield)


}


