library(tidyverse)
library(lubridate)
library(corrr)
library(ggplot2)
library(knitr)
options(scipen=999)

# Data are upto "2021-08-24"
#all analysis are manly done for interval 2020-8-24  ---- 2021-8-24

#-----------------------------------------------------------------------------------------------------------------------------------------------
#GLOBAL VARIABLES
max_date <- "2021-08-24"
max_date_minus1_year <- ymd(max_date) - years(1)
#-----------------------------------------------------------------------------------------------------------------------------------------------

#df of top 50 crypto coins
df_top_50 <- read.csv("top50.csv")

# MAIN DF ---- entries of all top 10 coins
df <- df_top_50 |>
  filter(Currency_Name %in% top_10_coins_list )

top_10_coins_list <- c("Bitcoin", "Ethereum", "Polkadot", "Avalanche", 
                       "Binance_Coin", "Cardano", "Dogecoin", "Solana",
                       "SHIBA_INU", "XPR", "USD_Coin")
#-----------------------------------------------------------------------------------------------------------------------------------------------
#NOT valid
#average price of each coin in 2021 1.1 - 24-8
avg_price__only_2021 <- df |> 
  filter(ymd(Date) >= ymd("2021-01-01") & ymd(Date) <= ymd("2021-12-31")) |> 
  group_by(Currency_Name) |> 
  summarize(Avg_2021_Price =  format(mean(Open), scientific=FALSE ) )

#-----------------------------------------------------------------------------------------------------------------------------------------------
#trend of absolute price for btc and eth in last year and in all years

#get btc and eth prices, pivot it
btc_eth_prices_2021 <- df |> 
  filter(ymd(Date) >= max_date_minus1_year & ymd(Date) <= max_date & Currency_Name %in% c("Ethereum", "Bitcoin")) |> 
  select(Currency_Name, Date, Open) |> 
  pivot_wider(names_from = Currency_Name, values_from = Open) |> 
  mutate(Date = ymd(Date))

btc_eth_prices_2021_not_pivot <- df |> 
  filter(ymd(Date) >= max_date_minus1_year & ymd(Date) <= max_date & Currency_Name %in% c("Ethereum", "Bitcoin")) |> 
  select(Currency_Name, Date, Open) |> 
  mutate(Date = ymd(Date))

btc_eth_prices_2021

plot(btc_eth_prices_2021$Date,btc_eth_prices_2021$Bitcoin, type="l")

#btc price 2020-2021 graph
ggplot(btc_eth_prices_2021, aes(x = Date, y = Bitcoin, colour = "Bitcoin")) + 
  geom_line() +
  labs(title = "Bitcoin price from 8.2020 - 8.2021", x = "Date", y = "Price($)")

#eth price 2020-2021 graph
ggplot(btc_eth_prices_2021, aes(x = Date, y = Ethereum, colour = "Etehreum")) + 
  geom_line() +
  labs(title = "Ethereum price from 8.2020 - 8.2021", x = "Date", y = "Price($)")

#combined btc and eth 2020-2021
ggplot(btc_eth_prices_2021_not_pivot, aes(x = Date, y = Open, colour = Currency_Name)) + 
  geom_line() +
  labs(title = "Bitcoin and Ethereum prices from 8.2020 - 8.2021", x = "Date", y = "Price($)")

#btc price for last 11 years
min_btc_date <- ymd("2010-07-18")
btc_prices_2010_2021 <- df |> 
  filter(ymd(Date) >= min_btc_date & ymd(Date) <= max_date & Currency_Name %in% c("Bitcoin")) |> 
  mutate(Date=ymd(Date)) |> 
  select(Currency_Name, Date, Open)

btc_prices_2010_2021

ggplot(btc_prices_2010_2021) + 
  geom_line(mapping=aes(x = Date, y = Open, group=1, colour = "Bitcoin")) +
  labs(title = "Bitcoin prices from 2010-2020", x = "Date", y = "Price($)")
  

#-----------------------------------------------------------------------------------------------------------------------------------------------
#average price for coins in 2021
#table and graph(not representative)
#done

#average price of each coin in 2020-8-24  ---- 2021-8-24
avg_price_2021 <- df |> 
  filter(ymd(Date) >= max_date_minus1_year & ymd(Date) <= ymd("2021-12-31")) |> 
  group_by(Currency_Name) |> 
  summarize(Avg_2021_Price =  format(mean(Open), scientific=FALSE ) ) |> 
  mutate(Avg_2021_Price = as.double(Avg_2021_Price))

avg_price_2021
#table
kable(avg_price_2021) 

#include graph optionally
ggplot(data=avg_price_2021, aes(x= Currency_Name, y=Avg_2021_Price)) +
  geom_bar(stat="identity", color="blue", fill="white")

#-----------------------------------------------------------------------------------------------------------------------------------------------
#volatility
#daily and annual volatility
#including graphs
# done

#daily standard deviation, 8.2020 -8.2021
daily_stdev_2021 <- df |> 
  filter(ymd(Date) >= max_date_minus1_year & ymd(Date) <= max_date) |> 
  group_by(Currency_Name) |> 
  summarize(Daily_stdev_2021 =  format(sd(Change..), scientific=FALSE ) ) |> 
  mutate(Daily_stdev_2021 = as.double(Daily_stdev_2021))

#get data of doge
# df |> 
#   filter(ymd(Date) >= max_date_minus1_year & ymd(Date) <= max_date & Currency_Name == "Dogecoin" & Change.. >= 7.0)  

#daily plot
ggplot(data=daily_stdev_2021, aes(x= Currency_Name, y=Daily_stdev_2021)) +
  geom_bar(stat="identity", color="blue", fill="white")


#yearly(annual) standard deviation of coins8.2020 -8.2021
yearly_stdev_2021 <- daily_stdev_2021 |> 
  mutate(Annual_stdev_2021 = sqrt(252)*as.double(Daily_stdev_2021))

#yearly plot
ggplot(data=yearly_stdev_2021, aes(x=Currency_Name, y=Annual_stdev_2021)) +
  geom_bar(stat="identity", color="blue", fill="white")


#-----------------------------------------------------------------------------------------------------------------------------------------------
#NOT valid
#monthly standard deviation for coins
monthly_stdev_2021 <- df |> 
  filter(ymd(Date) >= max_date_minus1_year & ymd(Date) <= ymd("2021-12-31")) |> 
  mutate(Month = month(ymd(Date))) |> 
  group_by(Currency_Name, Month) |> 
  summarize(Monthly_stdev_2021 = format(sd(Open), scientific=FALSE), Percent_Stdv_AvgMonthlyPrice = format(sd(Open)/mean(Open), scientific=FALSE))

monthly_stdev_2021

#-----------------------------------------------------------------------------------------------------------------------------------------------
#relative price change for coins in last year of data (24.8.2020 - 24.8.2021)
#show table for all
#show graph for btc eth
#done

#relative price change in past year
# only name, date and open price
relative_price_changes_last_year_only_prices <- df |> 
  filter( (ymd(Date) == max_date_minus1_year | ymd(Date) == max_date) & Currency_Name != "Avalanche" ) |> 
  select(Currency_Name, Date, Open)


#relative price changes in last available year 
#   pivot table to get both start and end price in each col
relative_price_changes_last_year <- relative_price_changes_last_year_only_prices |> 
  spread(key=Date, value=Open) |> 
  mutate(Relative_change_percent = `2021-08-24`/`2020-08-24`*100) |> 
  filter(!is.na(Relative_change_percent))
  
relative_price_changes_last_year

ggplot(data=relative_price_changes_last_year, aes(x=Currency_Name, y=Relative_change_percent)) +
  geom_bar(stat="identity", color="blue", fill="white")

#relative price changes for BTC, ETH
relative_price_changes_last_year_main_coins <- relative_price_changes_last_year_only_prices |> 
  spread(key=Date, value=Open) |> 
  mutate(Relative_change_percent = `2021-08-24`/`2020-08-24`*100) |> 
  filter(!is.na(Relative_change_percent) & Currency_Name %in% c("Bitcoin", "Ethereum"))

relative_price_changes_last_year_main_coins

ggplot(data=relative_price_changes_last_year_main_coins, aes(x=Currency_Name, y=Relative_change_percent)) +
  geom_bar(stat="identity", color="blue", fill="white")


#-----------------------------------------------------------------------------------------------------------------------------------------------
#----- start btc sp500 correlation
#correlation between btc and s&p500 in last year and in last 10years
#tables and graphs
#done 

#sp500 data
sp_raw <- read_csv("data/sp500_index.csv")
sp_raw |> head()

sp <- sp_raw |> 
  filter(ymd(Date) >= max_date_minus1_year & ymd(Date) <= max_date) 

#btc last year
btc <- df |> 
  filter(Currency_Name == "Bitcoin" & ymd(Date) >= max_date_minus1_year & ymd(Date) <= max_date) |> 
  mutate(Date = as.Date(Date, "%Y-%m-%d")) |> 
  select(Currency_Name, Date, Open)
  
#normal join
sp |> 
  inner_join(btc)
  
#pivot table style date, sp, btc
sp_btc <- sp |> 
  inner_join(btc) |> 
  rename(Bitcoin=Open) |> 
  select(Date, `S&P500`, Bitcoin)
  
sp_btc
str(sp_btc)

sp_btc_correlation_ommit_na <- sp_btc |> 
  select(`S&P500`, Bitcoin) |> 
  correlate() |> 
  replace_na(list( `S&P500` = 1, Bitcoin = 1))

sp_btc_correlation <- sp_btc |> 
  select(`S&P500`, Bitcoin) |> 
  correlate()

sp_btc_correlation

rplot(
  sp_btc_correlation,
  legend = TRUE,
  shape = 16,
  colours = c("blue", "red"),
  print_cor = TRUE,
)

#--correlation btc - sp500 over last 10 years
  #interval of analysis time 7.5.2012 - 24.8.2021
sp_min_date <-ymd("2012-05-07 ")
btc_max_date <- ymd(max_date)

#sp values from 2012
sp_2012_2021 <- sp_raw |> 
  filter(ymd(Date) >= sp_min_date & ymd(Date) <= btc_max_date) 

#btc last year
btc_2012_2021 <- df |> 
  filter(Currency_Name == "Bitcoin" & ymd(Date) >= sp_min_date & ymd(Date) <= btc_max_date) |> 
  mutate(Date = as.Date(Date, "%Y-%m-%d")) |> 
  select(Currency_Name, Date, Open)

#join and pivot btc and sp500 for last 10 years
sp_btc_2012_2021 <- sp_2012_2021 |> 
  inner_join(btc_2012_2021) |> 
  rename(Bitcoin=Open) |> 
  select(Date, `S&P500`, Bitcoin)

sp_btc_correlation_2012_2021 <- sp_btc_2012_2021 |> 
  select(`S&P500`, Bitcoin) |> 
  correlate()

sp_btc_correlation_2012_2021

rplot(
  sp_btc_correlation_2012_2021,
  legend = TRUE,
  shape = 16,
  colours = c("white", "black"),
  print_cor = TRUE,
)

#----- end btc sp500 correlation

#-----------------------------------------------------------------------------------------------------------------------------------------------

#correlation between various coins

#------ start btc, eth, binance, cardano correlation
correlation_coins <- c("Bitcoin", "Ethereum", "Cardano", "Binance_Coin")

corr_coins_pivot <- df |> 
  filter(Currency_Name %in% correlation_coins & ymd(Date) >= max_date_minus1_year & ymd(Date) <= max_date) |> 
  mutate(Date = as.Date(Date, "%Y-%m-%d")) |> 
  select(Currency_Name, Date, Open) |>
  pivot_wider(names_from = Currency_Name , values_from =  Open)

coins_correlations <- corr_coins_pivot |> 
  select(Binance_Coin, Bitcoin, Ethereum, Cardano) |> 
  correlate()

coins_correlations

rplot(
  coins_correlations,
  legend = TRUE,
  shape = 16,
  colours = c("white", "black"),
  print_cor = FALSE,
)


#------ end btc, eth, binance, cardano correlation
#-----------------------------------------------------------------------------------------------------------------------------------------------
  
  
  
  







