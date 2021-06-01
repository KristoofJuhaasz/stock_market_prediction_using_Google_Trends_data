
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




############## raw data manipulation ##############
key_word <- c(
   "reforms", "boost", "consolidate", "construction", "outperform", 
   "savings", "progress", "booming", "accrue", "surpass",

  "debt", "crisis", "decline", "recession", "unemployment",
  "bankrupt", "deficit", "collapse", "market crash", "downturn",
  
  "banana", "door", "weather", "chopstick", "phone",
  "chicago", "christmas", "swim", "bycicle", "programming"
)


key_word <- c(

  "debt", "crisis", "unemployment"

)





############## STANDARDIZED DATA MANIPULATION ############## 

elojel_1 <- c(-1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,   
              -1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,
              -1,-1,-1,-1,-1,   -1,-1,-1,-1,-1)
elojel_2 <- c(1,1,1,1,1,   1,1,1,1,1,   
            -1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,
            1,1,1,1,1,   1,1,1,1,1)

ultimate_result_df <- returns
result_df2 <- data.frame(matrix(NA, nrow = 30, ncol = 7))
names(result_df2) <- c("kw", "1", "2", "3", "4", "5", "6")

days <- 30

for (w in 1:length(key_word)) {
  
  file_name <- paste(key_word[w], "_stdfinaldf.csv", sep="")
  std_final_df <- read.csv2(file_name, header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE)
  
  std_final_df <- mutate(std_final_df, date = as.Date(date))
  std_final_df <- mutate(std_final_df, hits = as.numeric(hits))
  std_final_df <- merge(std_final_df, returns, by="date", all=FALSE, sort=TRUE) #inner join
  std_final_df$y_lead[is.na(std_final_df$y_lead)] <- 0

for (y in 1:6) { #heteket rögzítjük 3-ban

  hetek <- y
  
  final_test_df <- std_final_df
  final_test_df <- mutate(final_test_df, position_ind = NA)
  cycle_length <- nrow(final_test_df) - days - 1
  
 
  for (x in 0:cycle_length) {
    
    
    start_day <- 1
    end_day <- x+1+days
      
    temp_df <- std_final_df[(start_day:end_day),]
    #temp_df[temp_df == 0] <- NA

      position <- strat_PREIS(temp_df, hetek, elojel_1[w]) #step 1 - preis
      #position <- strat_PREIS(temp_df, hetek, elojel_2[w]) #step 2 - preis + sentiment

      
      #t_stat <- irany_check(temp_df)[1]
      
      
      #if (is.na(t_stat)) {elojel <- 0} else {
       # if ( t_stat > 0) {
       #   elojel <- 1
       # } else { 
       #   elojel <- -1
       # }
      #}
  
      #position <- strat_PREIS_linreg(temp_df, hetek, elojel) * abs(t_stat)
    
    if(position==0 | is.na(position)) {position <- NA}
    
    final_test_df[end_day,ncol(final_test_df)] <- position
    #print(x)
    
  }
  
  
  #final_test_df$position_ind <- na.locf(final_test_df$position_ind, na.rm=FALSE)
  
  final_test_df <- mutate(final_test_df, strat_return = position_ind * y_lead )
  final_test_df <- filter(final_test_df, date <= as.Date("2011-02-22"))
  cum_return <- sum(final_test_df$strat_return, na.rm=TRUE)

  result_df2[w,y+1] <- cum_return
  print(y)
}  

  
  final_test_df <- dplyr::select(final_test_df, 1, 4)
  ultimate_result_df <- merge(ultimate_result_df, final_test_df, by="date", all.x=TRUE, sort=TRUE)
  names(ultimate_result_df)[length(names(ultimate_result_df))] <- c(key_word[w])
  
  
  print(w)
  print(key_word[w])
  result_df2[w,1] <- key_word[w]

  
}

#write.csv(result_df2, file = "temp_temp.csv", row.names=FALSE)
#write.csv(ultimate_result_df, file = "temp_temp.csv", row.names=FALSE)





############ strat PREIS ##############

strat_PREIS <- function(temporary_df, het_multiplier, elojel) {
  
  test_df <- temporary_df[nrow(temporary_df),]
  utolso_datum <- test_df$date[1]
  kezdo_datum <- utolso_datum - het_multiplier*7
  
  train_df <- temporary_df[-nrow(temporary_df),]
  train_df <- filter(train_df, date >= kezdo_datum)
  
  atlag <- mean(train_df$hits, na.rm=TRUE)
  utolso_ertek <- test_df$hits[1]
  
  position <- 
    if ( is.na(utolso_ertek) | is.na(atlag) ) {
      0
    } else {
      if ( utolso_ertek - atlag > 0 )  {
        elojel
      } else if ( utolso_ertek - atlag < 0 ) { 
        -elojel
      } else {
        0
      }
    }
  
  return(position)
  
}





############ strat PREIS + linreg ##############

irany_check <- function(temporary_df) {
  
  temporary_df <- mutate(temporary_df, hits = ifelse(hits==0 | lag(hits)==0, 0, log( hits/lag(hits) ) ) )
  temporary_df <- temporary_df[-1,]
  
  test_df <- temporary_df[nrow(temporary_df),]
  utolso_datum <- test_df$date[1]

  
  train_df <- temporary_df[-nrow(temporary_df),]
  #kezdo_datum <- utolso_datum - 180
  #kezdo_datum <- utolso_datum - 21
  #train_df <- filter(train_df, date >= kezdo_datum)
  
  
      #train_df <- mutate(train_df, hits = ifelse(hits==0, 0.1, hits))
      #train_df <- mutate(train_df, hits = log(hits))
      #train_df <- mutate(train_df, hits = (hits - mean(hits, na.rm=T)) / sd(hits, na.rm=T) )
  
    if (sum(train_df$hits, na.rm=T)==0) {
      t_stat <- 0
      beta <- 0
    } else {
      lin_reg <- lm(y_lead~hits, data=train_df)
      t_stat <- summary(lin_reg)$coefficients[2,3]
      beta <- summary(lin_reg)$coefficients[2,1]
    }
  
  result <- c(t_stat,beta)
  
  return(result)
  
}



strat_PREIS_linreg <- function(temporary_df, het_multiplier, elojel) {
  
  utolso_datum <- temporary_df$date[nrow(temporary_df)]
  kezdo_datum <- utolso_datum - het_multiplier*7
  temporary_df <- filter(temporary_df, date >= kezdo_datum)
  
  #temporary_df <- mutate(temporary_df, hits = ifelse(hits==0, 1, hits))
  #temporary_df <- mutate(temporary_df, hits = log(hits))
  #temporary_df <- mutate(temporary_df, hits = (hits - mean(hits, na.rm=T)) / sd(hits, na.rm=T) )
  
  test_df <- temporary_df[nrow(temporary_df),]
  #utolso_datum <- test_df$date[1]
  #kezdo_datum <- utolso_datum - het_multiplier*7
  
  train_df <- temporary_df[-nrow(temporary_df),]
  #train_df <- filter(train_df, date >= kezdo_datum)
  
  atlag <- mean(train_df$hits, na.rm=TRUE)
  utolso_ertek <- test_df$hits[1]
  
  position <- 
    if ( is.na(utolso_ertek) | is.na(atlag) ) {
      0
    } else {
      if ( utolso_ertek - atlag > 0 )  {
        elojel
      } else if ( utolso_ertek - atlag < 0 ) { 
        -elojel
      } else {
        0
      }
    }
  
  return(position)
  
}





ultimate_result_df <- returns

days <- 181

for (w in 1:length(key_word)) {
  
  file_name <- paste(key_word[w], "_stdfinaldf.csv", sep="")
  std_final_df <- read.csv2(file_name, header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE)
  
  std_final_df <- mutate(std_final_df, date = as.Date(date))
  std_final_df <- mutate(std_final_df, hits = as.numeric(hits))
  std_final_df <- merge(std_final_df, returns, by="date", all=FALSE, sort=TRUE) #inner join
  std_final_df$y_lead[is.na(std_final_df$y_lead)] <- 0
  
  
  hetek <- 3
  
  final_test_df <- std_final_df
  final_test_df <- mutate(final_test_df, position_ind = NA)
  cycle_length <- nrow(final_test_df) - days - 1
  
  
  for (x in 0:cycle_length) {
    
    start_day <- 1
    end_day <- x+1+days
    
    temp_df <- std_final_df[(start_day:end_day),]
    
    t_and_beta <- irany_check(temp_df)
    t_stat <- t_and_beta[1]
    beta <- t_and_beta[2]
    
    
    final_test_df[end_day,ncol(final_test_df)] <- t_stat

    
  }
  
  final_test_df <- dplyr::select(final_test_df, 1, 4)
  
  ultimate_result_df <- merge(ultimate_result_df, final_test_df, by="date", all.x=TRUE, sort=TRUE)
  names(ultimate_result_df)[length(names(ultimate_result_df))] <- c(key_word[w])
  
  print(w)
  print(key_word[w])
}


write.csv(ultimate_result_df, file = "ultimate_result_df3.csv", row.names=FALSE)








key_word <- "decline"


ultimate_result_df <- returns

days <- 181

for (w in 1:length(key_word)) {
  
  file_name <- paste(key_word[w], "_stdfinaldf.csv", sep="")
  std_final_df <- read.csv2(file_name, header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE)
  
  std_final_df <- mutate(std_final_df, date = as.Date(date))
  std_final_df <- mutate(std_final_df, hits = as.numeric(hits))
  std_final_df <- merge(std_final_df, returns, by="date", all=FALSE, sort=TRUE) #inner join
  std_final_df$y_lead[is.na(std_final_df$y_lead)] <- 0
  
  
  hetek <- 3
  
  final_test_df <- std_final_df
  final_test_df <- mutate(final_test_df, position_ind = NA)
  cycle_length <- nrow(final_test_df) - days - 1
  
  
  for (x in 0:cycle_length) {
    
    start_day <- 1
    end_day <- x+1+days
    
    temp_df <- std_final_df[(start_day:end_day),]
    
    t_stat <- irany_check(temp_df)[1]
    if (is.na(t_stat)) {elojel <- 0} else {
      if ( t_stat > 0) {
        elojel <- 1
      } else { 
        elojel <- -1
      }
    }
    
    #if ( abs(t_stat) > 1.282 ) {
    position <- strat_PREIS_linreg(temp_df, hetek, elojel) * abs(t_stat)
    #position <- strat_PREIS_linreg_szoras(temp_df, hetek, elojel,0) * abs(t_stat)
    #} else { position <- NA}
    
    #position <- strat_PREIS(temp_df, hetek, elojel_2[w]) #step 2 - preis + sentiment
    
    final_test_df[end_day,ncol(final_test_df)] <- position
    
    #print(x)
    
  }
  
  final_test_df <- dplyr::select(final_test_df, 1, 4)
  
  ultimate_result_df <- merge(ultimate_result_df, final_test_df, by="date", all.x=TRUE, sort=TRUE)
  names(ultimate_result_df)[length(names(ultimate_result_df))] <- c(key_word[w])
  
  print(w)
  print(key_word[w])
}


ultimate_result_df <- mutate(ultimate_result_df, atlag = rowMeans( select(ultimate_result_df, 3:32), na.rm=TRUE ) )
ultimate_result_df <- mutate(ultimate_result_df, ind = ifelse(atlag>0,1,ifelse(atlag<0,-1,0) ) )
ultimate_result_df <- mutate(ultimate_result_df, strat_return = ind*y_lead )

ultimate_result_df <- mutate(ultimate_result_df, ind_continous = ifelse(is.na(ind), NA, ind) )
ultimate_result_df$ind_continous <- na.locf(ultimate_result_df$ind_continous, na.rm=FALSE)
ultimate_result_df <- mutate(ultimate_result_df, strat_return_continous = ind_continous*y_lead )

print(sum(ultimate_result_df$strat_return, na.rm=TRUE))
print(sum(ultimate_result_df$strat_return_continous, na.rm=TRUE))

write.csv(ultimate_result_df, file = "ultimate_result_df3.csv", row.names=FALSE)
write.csv(ultimate_result_df, file = "temp_temp.csv", row.names=FALSE)






















strat_PREIS_linreg_szoras <- function(temporary_df, het_multiplier, elojel, szoras_multiplier) {
  
  temporary_df <- mutate(temporary_df, hits = ifelse(hits==0, 0.1, hits))
  temporary_df <- mutate(temporary_df, hits = log(hits))
  temporary_df <- mutate(temporary_df, hits = (hits - mean(hits, na.rm=T)) / sd(hits, na.rm=T) )
  
  test_df <- temporary_df[nrow(temporary_df),]
  utolso_datum <- test_df$date[1]
  kezdo_datum <- utolso_datum - het_multiplier*7
  
  train_df <- temporary_df[-nrow(temporary_df),]
  train_df <- filter(train_df, date >= kezdo_datum)
  
  atlag <- mean(train_df$hits, na.rm=TRUE)
  utolso_ertek <- test_df$hits[1]
  
  utolso_elotti_ertek <- train_df$hits[nrow(train_df)]
  szoras <- sd(train_df$hits, na.rm=TRUE)
  abs_kulonbseg <- abs(utolso_ertek - atlag)
  
  position <- 
    #if ( is.na(utolso_ertek) | is.na(atlag) | is.na(utolso_elotti_ertek) | is.na(abs_kulonbseg) ) {
    if ( is.na(utolso_ertek) | is.na(atlag) | is.na(abs_kulonbseg) | is.na(szoras) ) {
      NA
    } else {
      if ( utolso_ertek - atlag > 0 & abs_kulonbseg > szoras*szoras_multiplier )  {
        elojel
      } else if ( utolso_ertek - atlag < 0 & abs_kulonbseg > szoras*szoras_multiplier ) { 
        -elojel
      } else {
        NA
      }
    }
  #print(szoras*szoras_multiplier)
  return(position)
  
}







