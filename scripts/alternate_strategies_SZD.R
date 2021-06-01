
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




############## raw data manipulation ##############
key_word <- c(
   "reforms", "boost", "consolidate", "construction", "outperform", 
   "savings", "progress", "booming", "accrue", "surpass",

  "debt", "crisis", "decline", "recession", "unemployment",
  "bankrupt", "deficit", "collapse", "market crash", "downturn",
  
  "banana", "door", "weather", "chopstick", "phone",
  "chicago", "christmas", "swim", "bycicle", "programming"
)






############## STANDARDIZED DATA MANIPULATION ############## 

elojel <- c(1,1,1,1,1,   1,1,1,1,1,   
            -1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,
            1,1,1,1,1,   1,1,1,1,1)
elojel <- c(-1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,   
            -1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,
            -1,-1,-1,-1,-1,   -1,-1,-1,-1,-1)

result_df2 <- data.frame(matrix(NA, nrow = 30, ncol = 7))
names(result_df2) <- c("kw", "1", "2", "3", "4", "5", "6")

days=c(181)
d=1

for (w in 1:length(key_word)) {
  
  file_name <- paste(key_word[w], "_stdfinaldf.csv", sep="")
  std_final_df <- read.csv2(file_name, header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE)
  
  std_final_df <- mutate(std_final_df, date = as.Date(date))
  std_final_df <- mutate(std_final_df, hits = as.numeric(hits))
  std_final_df <- merge(std_final_df, returns, by="date", all=FALSE, sort=TRUE) #inner join
  std_final_df$y_lead[is.na(std_final_df$y_lead)] <- 0

#for (y in 1:6) {
y=3
  
  final_test_df <- std_final_df
  final_test_df <- mutate(final_test_df, position_ind = NA)
  cycle_length <- nrow(final_test_df) - days[d] - 1
  
 
  for (x in 0:cycle_length) {
    
    
    #start_day <- 1
    start_day <- x+1
    end_day <- x+1+days[d]
      
    temp_df <- std_final_df[(start_day:end_day),]
    
    #MÛKÖDNI LÁTSZÓ
    #position <- strat_fixedlimitpair(temp_df,0) #nem kitöltéssel, 90=day, elsõ naptól végig, RSI minta (0.3 , 0.7)
    #position <- strat_valtogatos(temp_df,0.33) #egész jo
    
    
      #position <- strat_PREIS(temp_df, y) #step 1-2
      position <- strat_PREIS_linreg(temp_df, y) #step 3
    
    #position <- strat_linreg(temp_df,0)
    #position <- strat_correl(temp_df,0.2)
    #position <- strat_atlag(temp_df,1)
    #position <- strat_limitpair(temp_df,0)
    
    if(position==0 | is.na(position)) {position <- NA}
    
    final_test_df[end_day,ncol(final_test_df)] <- position
    #print(x)
    
  }
  
  
  #final_test_df$position_ind <- na.locf(final_test_df$position_ind, na.rm=FALSE)
  
  final_test_df <- mutate(final_test_df, strat_return = position_ind * y_lead )
  cum_return <- sum(final_test_df$strat_return, na.rm=TRUE)

  result_df2[w,y+1] <- cum_return
  print(y)
#}  

  print(w)
  print(key_word[w])
  result_df2[w,1] <- key_word[w]

  
}








############ strat PREIS ##############

strat_PREIS <- function(temporary_df, het_multiplier) {
  
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
        -1 * -elojel[w]
      } else if ( utolso_ertek - atlag < 0 ) { 
        1 * -elojel[w]
      } else {
        0
      }
    }
  
  return(position)
  
}



############ strat PREIS + linreg ##############

strat_PREIS_linreg <- function(temporary_df, het_multiplier) {
  
  test_df <- temporary_df[nrow(temporary_df),]
  utolso_datum <- test_df$date[1]
  kezdo_datum <- utolso_datum - het_multiplier*7
  
  train_df <- temporary_df[-nrow(temporary_df),]
  train_df <- filter(train_df, date >= kezdo_datum)
  
  linear_reg <- lm(y_lead ~ hits, data=train_df)
  alfa <- linear_reg$coefficients[1]
  beta <- linear_reg$coefficients[2]
  
  if (is.na(beta)) {
    elojel_linreg <- 0
  } else if (beta > 0) {
    elojel_linreg <- 1
  } else {
    elojel_linreg <- -1
  }
  
  atlag <- mean(train_df$hits, na.rm=TRUE)
  utolso_ertek <- test_df$hits[1]
  
  position <- 
    if ( is.na(utolso_ertek) | is.na(atlag) ) {
      0
    } else {
      if ( utolso_ertek - atlag > 0 )  {
        -1 * elojel_linreg
      } else if ( utolso_ertek - atlag < 0 ) { 
        1 * elojel_linreg
      } else {
        0
      }
    }
  
  return(position)
  
}


############ irány checklinreg ##############

irany_check <- function(temporary_df) {
  
  test_df <- temporary_df[nrow(temporary_df),]
  utolso_datum <- test_df$date[1]
  kezdo_datum <- utolso_datum - 180
  
  train_df <- temporary_df[-nrow(temporary_df),]
  train_df <- filter(train_df, date >= kezdo_datum)
  
  linear_reg <- lm(y_lead ~ hits, data=train_df)
  
  if ( is.na( summary(linear_reg)$coefficients[2,3] ) ) {
    t_stat <- 0
  } else {
    t_stat <- summary(linear_reg)$coefficients[2,3]
  }
  
  position <- t_stat
  
  return(position)
  
}







































############ strat próba 0 - linear regression ##############
temporary_df <- temp_df
szoras_multiplier <- 0

strat_linreg <- function(temporary_df, szoras_multiplier) {
  
  temporary_df <- mutate(temporary_df, hits = (hits - mean(hits, na.rm=T)) / sd(hits, na.rm=T) )
  
  train_df <- temporary_df[-nrow(temporary_df),]
  test_df <- temporary_df[nrow(temporary_df),]
if (sum(train_df$hits,na.rm=T) > 0 ) {
  linear_reg <- lm(y_lead ~ hits, data=train_df)
  alfa <- linear_reg$coefficients[1]
  beta <- linear_reg$coefficients[2]
  #p_value <- summary(linear_reg)$coefficients[2,4]
  
  atlag <- mean(train_df$hits, na.rm=TRUE)
  szoras <- sd(train_df$hits, na.rm=TRUE)
  utolso_ertek <- test_df$hits[1]
  
  varhato_hozam <- alfa + beta * utolso_ertek
  atlag_kul <- abs(utolso_ertek-atlag)
  
  position <- 
    #if ( is.na(atlag_kul) | is.na(szoras) | p_value > 0.5 )  {
    if ( is.na(atlag_kul) | is.na(szoras) ) {
      0
    } else {
      #if(1==0){
      if ( atlag_kul < szoras * szoras_multiplier )  {
        0
      } else { 
        if (is.na(varhato_hozam)) {
          0
        } else {
          if ( varhato_hozam > 0) { 
            1
          } else if ( varhato_hozam < 0 ) {
            -1
          } else {
            0
          }
        } 
      }
    }

} else { postition <- 0}
  
  return(position)
  
}






############## stratégia 2 - limit pair ############## STANDARDIZED


strategy <- function(data_frame, limit_a, limit_c) 
{ 
  temp_df <- data_frame
  temp_df <- mutate( temp_df , bb_ind = ifelse( hits <= limit_a, 1 , ifelse( hits > limit_c, -1 , NA ) ) )
  
  if (is.na( temp_df[ 1 , "bb_ind" ] )) { temp_df[ 1 , "bb_ind" ] <- 0  }
  
  temp_df$bb_ind <- na.locf( temp_df$bb_ind )
  temp_df <- mutate( temp_df , y_sen = ifelse( hits == 0, 0, y_lead * bb_ind ) ) #if any 0s remain which should not
  
  sum_y_sen <- sum( temp_df$y_sen )
  results <- c(sum_y_sen)
  return( results )
} 

limit_pair <- function(result_data_frame) {
  
  result_m <- mutate(result_data_frame, limit_a = rownames(result_data_frame))
  result_m <- melt(result_m, na.rm = FALSE)
  names(result_m) <- c("limit_a","limit_c", "value")
  #result_m[] <- lapply(result_m, gsub, pattern='X', replacement='')
  
  result_m$limit_c <- as.numeric(result_m$limit_c)
  result_m$limit_a <- as.numeric(result_m$limit_a)
  limit_a <- min( filter(result_m, value == max(value, na.rm=TRUE))[1] ) %>% as.numeric()
  limit_c <- max( filter(result_m, value == max(value, na.rm=TRUE))[2] ) %>% as.numeric()
  
  limit_pair <- c(limit_a, limit_c)
  return( limit_pair )
  
}


temp_df <- temporary_df
strat_limitpair <- function(temporary_df, szoras_multiplier) {
  
  result_df_1 <- data.frame(matrix(NA, nrow = 100, ncol = 100))
  
  train_df <- temporary_df[-nrow(temporary_df),]
  test_df <- temporary_df[nrow(temporary_df),]
  
  min_for <- max( floor( min(train_df$hits, na.rm=TRUE) ), 1)
  max_for <- ceiling( max(train_df$hits, na.rm=TRUE) )
  
  
  for ( i in min_for:max_for )
  {
    for ( j in min_for:max_for )
    {
      if ( i > j ) 
      { result_df_1[ i , j ] <- NA }
      else
      { 
        result_vector <- strategy( train_df , i , j )
        result_df_1[ i , j ] <- result_vector
      }
      
    }
  }
  
  best_limit_pair <- limit_pair(result_df_1)
  limit_a_1 <- best_limit_pair[1]
  limit_c_1 <- best_limit_pair[2]
  
  position <- ifelse( test_df$hits[1] <= limit_a_1, -elojel[w] , ifelse( test_df$hits[1] > limit_c_1, elojel[w] , NA ) )
  
  return(position)
}







############ strat próba 2 - atlag ##############

elojel <- c(1,1,1,1,1,   1,1,1,1,1,   
           -1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,
           1,1,1,1,1,   1,1,1,1,1)
strat_atlag <- function(temporary_df, szoras_multiplier) {
  
  temporary_df <- mutate(temporary_df, hits = log(hits) )
  temporary_df <- mutate(temporary_df, hits = (hits - mean(hits, na.rm=T)) / sd(hits, na.rm=T) )
  
  train_df <- temporary_df[-nrow(temporary_df),]
  test_df <- temporary_df[nrow(temporary_df),]
  
  atlag <- mean(train_df$hits, na.rm=TRUE)
  szoras <- sd(train_df$hits, na.rm=TRUE)
  utolso_ertek <- test_df$hits[1]
  
  position <- 
    if ( is.na(utolso_ertek) | is.na(szoras) | is.na(atlag) ) {
      0
    } else {
      if ( utolso_ertek > atlag & abs(utolso_ertek-atlag) > szoras * szoras_multiplier )  {
        1*elojel[w]
      } else if (utolso_ertek < atlag & abs(utolso_ertek-atlag) > szoras * szoras_multiplier) { 
        -1*elojel[w]
      } else {
        0
      }
    }
  
  return(position)
  
}


############ strat váltogatós  ##############

elojel <- c(1,1,1,1,1,   1,1,1,1,1,   
            -1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,
            1,1,1,1,1,   1,1,1,1,1)
strat_valtogatos <- function(temporary_df, szoras_multiplier) {
  
  train_df <- temporary_df[-nrow(temporary_df),]
  test_df <- temporary_df[nrow(temporary_df),]
  
  atlag <- mean(train_df$hits, na.rm=TRUE)
  szoras <- sd(train_df$hits, na.rm=TRUE)
  
  utolso_elotti_ertek <- train_df$hits[nrow(train_df)]
  utolso_ertek <- test_df$hits[1]
  
  position <- 
    if ( is.na(utolso_ertek) | is.na(szoras) | is.na(utolso_elotti_ertek) ) {
      0
    } else {
      if ( utolso_ertek > utolso_elotti_ertek & abs(utolso_ertek-utolso_elotti_ertek) > szoras * szoras_multiplier )  {
        1*elojel[w]
      } else if (utolso_ertek < utolso_elotti_ertek & abs(utolso_ertek-utolso_elotti_ertek) > szoras * szoras_multiplier) { 
        -1*elojel[w]
      } else {
        0
      }
    }
  
  return(position)
  
}


#### strat fixed limit pair ####

elojel <- c(1,1,1,1,1,   1,1,1,1,1,   
            -1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,
            1,1,1,1,1,   1,1,1,1,1)

temporary_df <- temp_df
strat_fixedlimitpair <- function(temporary_df, szoras_multiplier) {
  
  
  train_df <- temporary_df[-nrow(temporary_df),]
  test_df <- temporary_df[nrow(temporary_df),]
  
  min_for <- max( floor( min(train_df$hits, na.rm=TRUE) ), 1)
  max_for <- ceiling( max(train_df$hits, na.rm=TRUE) )
  
  
  limit_a_1 <- (max_for-min_for)*0.3
  limit_c_1 <- (max_for-min_for)*0.7
  
  position <- ifelse( test_df$hits[1] <= limit_a_1, -1*elojel[w] , ifelse( test_df$hits[1] > limit_c_1, elojel[w] , NA ) )
  
  return(position)
}





###CORRELATION

elojel <- c(1,1,1,1,1,   1,1,1,1,1,   
            -1,-1,-1,-1,-1,   -1,-1,-1,-1,-1,
            1,1,1,1,1,   1,1,1,1,1)

temporary_df<-temp_df
strat_correl <- function(temporary_df, correlation_strength) {
  
  
  temporary_df <- mutate(temporary_df, hits = (hits - mean(hits, na.rm=T)) / sd(hits, na.rm=T) )
  
  train_df <- temporary_df[-nrow(temporary_df),]
  test_df <- temporary_df[nrow(temporary_df),]
  
  correlation <- cor(train_df$hits, train_df$y_lead)
  
  utolso_elotti_ertek <- train_df$hits[nrow(train_df)]
  utolso_ertek <- test_df$hits[1]
  
  position <- 
    if ( is.na(utolso_ertek) | is.na(correlation) | is.na(utolso_elotti_ertek) ) {
      0
    } else {
      if ( utolso_ertek > utolso_elotti_ertek & correlation > correlation_strength )  {
        1*elojel[w]
      } else if (utolso_ertek < utolso_elotti_ertek & correlation < -1 * correlation_strength ) { 
        -1*elojel[w]
      } else {
        0
      }
    }
  
  return(position)
  
}























  
  ############## stratégia 1 - Milán ##############
  
  result_df <- data.frame(matrix(NA, ncol = 12, nrow = 6))
  names(result_df) <- days
  
  strat1_df <- final_df
      
  for (x in 1:6) {
    
    szoras_multiplier <- x/3
    
    for (y in 1:12) {
      
      print(y)
  
      for (i in 0:timelength) {
        
        temp_df <- filter(strat1_df, break_ind == i)
        temp_df <- select(temp_df, 1:2)
        
        temp_df <- temp_df[days[y]:90,]
        temp_df <- mutate(temp_df, hits = as.numeric(hits))
        
        atlag <- mean(temp_df$hits[-nrow(temp_df)], na.rm=TRUE)
        median <- median(temp_df$hits[-nrow(temp_df)], na.rm=TRUE)
        #print(atlag)
        #print(median)
        szoras <- sd(temp_df$hits[-nrow(temp_df)], na.rm=TRUE)
        utolso_ertek <- temp_df$hits[nrow(temp_df)]
        position <- if ( (utolso_ertek-atlag) > szoras_multiplier * szoras) { #pozitiv novekedes
          -neg[w]
        } else if ( (utolso_ertek-atlag) < -szoras_multiplier * szoras ) {
          neg[w]
        } else {
          0
        }
        position_df <- temp_df[nrow(temp_df),]
        position_df[1,3] <- position
        names(position_df) <- c("date", "hits", "position_ind")
        
        if (i==0) {
          final_position_df <- position_df
        } else {
          final_position_df <- rbind(final_position_df, position_df, stringsAsFactors=FALSE)
        }
        
        
      }
      
      summary(final_position_df)
      
      test_df <- merge(final_position_df, returns, by="date", all=FALSE, sort=TRUE)
      test_df <- mutate(test_df, return_ind = position_ind * y_lead )
      
      print("szoras_multiplier:")
      print(x/3)
      result_df[x,y] <- sum(test_df$return_ind, na.rm=TRUE)
      
    }
  
  }

print(key_word[w])
new_file_name <- paste(key_word[w], "_results.csv", sep="")
write.csv(result_df, file = new_file_name, row.names=FALSE)
}




############## stratégia 2 - limit pair ##############


strat2_df <- final_df


for (x in 0:timelength) {

result_df_1 <- data.frame(matrix(NA, nrow = 100, ncol = 100))
result_df_2 <- data.frame(matrix(NA, nrow = 100, ncol = 100))

temp_df <- filter(strat2_df, break_ind == x)
temp_df <- select(temp_df, 1:2)
temp_df <- merge(temp_df, returns, by="date", all.x=TRUE, sort=TRUE)
temp_df$y_lead[is.na(temp_df$y_lead)] <- 0

train_df <- temp_df[-nrow(temp_df),]
test_df <- temp_df[nrow(temp_df),]

min_for <- max( floor( min(train_df$hits, na.rm=TRUE) ), 1)
max_for <- ceiling( max(train_df$hits, na.rm=TRUE) )
  
  
for ( i in min_for:max_for )
{
  for ( j in min_for:max_for )
  {
    if ( i > j ) 
    { result_df_1[ i , j ] <- NA }
    else
    { 
      result_vector <- strategy( train_df , i , j )
      result_df_1[ i , j ] <- result_vector
      }
    
  }
}

max(result_df_1, na.rm=T)

best_limit_pair <- limit_pair(result_df_1)
limit_a_1 <- best_limit_pair[1]
limit_c_1 <- best_limit_pair[2]


test_df <- mutate( test_df , bb_ind = NA )
test_df[1,4] <- ifelse( test_df$hits[1] <= limit_a, 1 , ifelse( test_df$hits[1] > limit_c, -1 , NA ) )

if (x==0) {
  final_test_df <- test_df
} else {
  final_test_df <- rbind(final_test_df, test_df, stringsAsFactors=FALSE)
}

print(x)
}


final_test_df <- mutate(final_test_df, strat_return = bb_ind * y_lead )
sum(final_test_df$y_lead, na.rm=TRUE)
sum(final_test_df$strat_return, na.rm=TRUE)








############## stratégia 3 - fixed limit pair ############## STANDARDIZED

strat2_df <- std_final_df
strat2_df <- mutate(strat2_df, hits = as.numeric(hits))
#strat2_df <- mutate(strat2_df, hits_lead1 = lead(hits,1))
#strat2_df <- mutate(strat2_df, hits_lead2 = lead(hits,2))
#strat2_df <- mutate(strat2_df, day = wday(date))
#strat2_df <- mutate(strat2_df, hits = ifelse( day==3, hits_lead2, ifelse(day==4, hits_lead1, hits) ) )
#strat2_df <- select(strat2_df, 1:2)

strat2_df <- merge(strat2_df, returns, by="date", all.x=TRUE, sort=TRUE)
strat2_df$y_lead[is.na(strat2_df$y_lead)] <- 0


for (x in 0:timelength) {
  
  result_df_1 <- data.frame(matrix(NA, nrow = 100, ncol = 100))
  
  temp_df <- strat2_df[(1:(x+91)),]
  
  train_df <- temp_df[-nrow(temp_df),]
  test_df <- temp_df[nrow(temp_df),]
  
  test_df <- mutate( test_df , bb_ind = NA )
  test_df[1,4] <- ifelse( test_df$hits[1] <= 30, 1 , ifelse( test_df$hits[1] >= 70, -1 , NA ) )
  
  if (x==0) {
    final_test_df <- test_df
  } else {
    final_test_df <- rbind(final_test_df, test_df, stringsAsFactors=FALSE)
  }
  
  #print(x)
}


final_test_df <- mutate(final_test_df, strat_return = bb_ind * y_lead )
sum(final_test_df$y_lead, na.rm=TRUE)
sum(final_test_df$strat_return, na.rm=TRUE)
sum(final_test_df$strat_return[1:5500], na.rm=TRUE)


















install.packages('vars')
library(vars)


print(Canada)
VARselect(Canada, lag.max = 5, type="const")
VARselect(train_df$hits, lag.max = 20)

test <- data.frame(train_df$hits)
names(test) <- "hits"
test <- mutate(test, lag1=lag(hits,1))
test <- mutate(test, lag2=lag(hits,2))
test <- test[-(1:2),]

VAR(test, p = 2)


VAR(Canada, p = 2, type = "none")


var_selection <- function(temporary_df) {
  
  temporary_df <- mutate(temporary_df, hits = ifelse(hits==0 | lag(hits)==0, 0, log( hits/lag(hits) ) ) )
  temporary_df <- temporary_df[-1,]
  
  test_df <- temporary_df[nrow(temporary_df),]
  utolso_datum <- test_df$date[1]
  kezdo_datum <- utolso_datum - 180
  
  train_df <- temporary_df[-nrow(temporary_df),]
  
  var_select <- VARselect(train_df)
  
  return(t_stat, lag.max=3)
  
}


temporary_df <- temp_df
gr_casuality <- function(temporary_df, het_multiplier) {
  
  temporary_df <- mutate(temporary_df, hits = ifelse(hits==0 | lag(hits)==0, 0, log( hits/lag(hits) ) ) )
  temporary_df <- temporary_df[-1,]
  
  test_df <- temporary_df[nrow(temporary_df),]
  utolso_datum <- test_df$date[1]
  kezdo_datum <- utolso_datum - 180
  
  train_df <- temporary_df[-nrow(temporary_df),]
  train_df <- filter(train_df, date >= kezdo_datum)
  
  var_select <- VARselect(train_df$hits, lag.max = 12)
  lag_order <- var_select$selection[3]
  
  granger_test <- grangertest(train_df$y_lead ~ train_df$hits, order=lag_order)
  granger_test <- granger_test[2,4]
  
  return(granger_test)
  
}


tryCatch(1, finally = print("Hello"))

days <- 181

for (w in 1:length(key_word)) {
  
  file_name <- paste(key_word[w], "_stdfinaldf.csv", sep="")
  std_final_df <- read.csv2(file_name, header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE)
  
  std_final_df <- mutate(std_final_df, date = as.Date(date))
  std_final_df <- mutate(std_final_df, hits = as.numeric(hits))
  std_final_df <- merge(std_final_df, returns, by="date", all=FALSE, sort=TRUE) #inner join
  std_final_df$y_lead[is.na(std_final_df$y_lead)] <- 0
  
  
  final_test_df <- std_final_df
  final_test_df <- mutate(final_test_df, position_ind = NA)
  cycle_length <- nrow(final_test_df) - days - 1
  
  
  for (x in 0:cycle_length) {
    
    
    start_day <- 1
    end_day <- x+1+days
    
    temp_df <- std_final_df[(start_day:end_day),]
    
    result <- var_selection(temp_df)
    print(result)
    
  }
  
  print(w)
  print(key_word[w])
  result_df2[w,1] <- key_word[w]
  
  
}




