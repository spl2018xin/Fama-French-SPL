
library(quantmod)
library(BatchGetSymbols) 
library(rvest)
library(xml2)


# No need to run the following codes, which are for downloading sp500 stocks and 
# already store in the folder "Data" in GitHub

# !!!! RUNNUNG THE LOOP TAKES ONE HOUR !!!!!

#≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤
# to download data from "Yahoo finance" the package (quantmod) is needed     #
#≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤≥≤



companies = GetSP500Stocks() # get S&P 500 stocks with sectors information 

# useful information for the future analysis:
# • tickers/company
# • GICS.Sector/GICS.Sub.Industry: Industry analysis
# CIK: Central Index Key

head(companies, 3)
str(companies)


# step 2: Batch download data from Yahoo Finance, need tickers and OHLC info.
# • require internet connection
# • The downloaded list contains 2 dataframes: df.control, df.tickers

stocks = BatchGetSymbols(tickers    = companies$tickers,
                         first.date = "2017-01-01",
                         last.date  = "2017-12-31")


head(stocks[["df.tickers"]])


# select stocks can be used in regression / "KEEP" for 2010 - 2017 
good.tickers = stocks$df.control$ticker[stocks$df.control$threshold.decision == "KEEP"]


# Fill dates as the first stock "MMM" as it happens to have complete dates (column name = "date")
SP500.data = data.frame(date = stocks$df.tickers[stocks$df.tickers$ticker == "MMM", "ref.date"]) 


# loop over the good data: read one ticker at a time and merge into SP500.data


for(i in 1:length(good.tickers)){
  X = data.frame(
    stocks$df.tickers[stocks$df.tickers$ticker == good.tickers[i] ,
                       c("ref.date", "price.adjusted")])
  
  # we need as.character() to convert the factor level to ticker names
  colnames(X) = c("date", as.character(good.tickers[i]))
  
  # merge X as a new column into SP500.data by matching date
  # missing dates will have NA by default
  SP500.data = merge.data.frame(SP500.data, X, by = "date", all.x = TRUE)
}

write.csv(SP500.data, "SP500_price.adjusted_2010-2017.csv")


# ======================= Automatic Download ===============================

# loop over above codes to download data from 1980 - 2015, group every 5 yrs.

library(lubridate)

List.of.start.date = seq(as.Date("1980/1/1"), as.Date("2016/1/1"), "years")
List.of.start.date = List.of.start.date[year(List.of.start.date) %% 5 == 0]

Download.Stat = data.frame(Data = List.of.start.date)
temp          = vector()

for(i in 1:(length(List.of.start.date) - 1)){
  start.date = as.Date(List.of.start.date[i])
  end.date   = as.Date(List.of.start.date[i + 1]) - 1
  
  # Download
  stocks= BatchGetSymbols(tickers    = companies$tickers,
                          first.date = start.date,
                          last.date  = end.date)
  
  # Fill Date
  SP500.data = data.frame(date = stocks$df.tickers$ref.date[1:max(stocks$df.control$total.obs)])

  good.tickers = stocks$df.control$ticker[stocks$df.control$threshold.decision == "KEEP"]
  temp = cbind(temp, length(good.tickers))
  
  for(j in 1:length(good.tickers)) {
    X = data.frame(stocks$df.tickers[stocks$df.tickers$ticker == good.tickers[j],
                                     c("ref.date", "price.adjusted")])
    
    # change the column name of X to be the ticker of the stock
    colnames(X) = c("date", good.tickers[j])
    
    # merge X as a new column into SP500.data by matching date
    # missing dates will have NA by default
    SP500.data = merge.data.frame(SP500.data, X, by = "date", all.x = TRUE)
  }
  
  file.name = paste("SP500_price.adjusted_", 
                    paste(year(start.date), year(end.date), sep="-"), ".csv", sep="")
  write.csv(SP500.data, file.name)
}

# how many good tickers in every 5 years.(1980-2015)
Download.Stat = Download.Stat[1:7, ]
Download.Stat = cbind(Download.Stat, Num.Tickers = data.frame(t(temp)))

