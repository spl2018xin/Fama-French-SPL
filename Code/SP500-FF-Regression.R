library(quantmod)
library(xts)
library(ggplot2)
library(corrplot)
library(reshape2)
library(lubridate)


# Structure

# 1.  S&P 500 2010 - 2017 / Sector Analysis

# 2.  S&P 500 1980 - 2015 / 5Y / Trend Analysis


# 0.  Data Preparation ----------------------------------------------------

# Fama French factors, till 201803
FF3       = read.csv("Data/FF3.CSV")
FF3[,-1]  = FF3[ , -1] / 100.00

# SP500 company information
Mapping   = read.csv("Data/constituents.csv")
colnames(Mapping)[1] = "Ticker"


# 1.  S&P 500 2010 - 2017 / Sector Analysis -------------------------------

# 1.1 Data Preparation ----------------------------------------------------

FF = FF3[ FF3$X >= 201001 & 
          FF3$X <= 201712, ]

SP500.data          = read.csv("Data/SP500_price.adjusted_2010-2017-2.csv")
SP500.data$date     = as.Date(SP500.data$date)

Stock.Prices.Daily  = SP500.data[ SP500.data$date >= "2010-01-01" & 
                                  SP500.data$date <= "2017-12-31" , -1 ]

# Number of stocks to start with
ncol(Stock.Prices.Daily)

# Drop outlier MNST and BHF
Stock.Drop          = c("MNST", "BHF")
Stock.Prices.Daily  = Stock.Prices.Daily[ , !names(Stock.Prices.Daily) %in% Stock.Drop]

# Calculate period returns
Period.Returns = tail(Stock.Prices.Daily[ ,-1], n = 1) / head(Stock.Prices.Daily[ ,-1], n = 1) - 1
rownames(Period.Returns) = "Return.2010.2017"
Period.Returns = t(Period.Returns)
Period.Returns = data.frame(Ticker = rownames(Period.Returns), 
                     Period.Return = Period.Returns)
Period.Returns = merge(Period.Returns, Mapping, by = "Ticker", all.x = T)

# Convert to XTS for quantmod::monthlyReturn
Stock.Prices.Daily = xts( Stock.Prices.Daily[ , -1], 
                          order.by = as.POSIXct(Stock.Prices.Daily$date))

# Remove stocks with NAs in the series, otherwise monthly Return will not work properly
Stock.Prices.Daily = Stock.Prices.Daily[,colSums(is.na(Stock.Prices.Daily)) == 0]


# Convert daily price to Monthly Return
Stock.Returns.Monthly           = do.call(cbind, lapply(Stock.Prices.Daily, monthlyReturn))
colnames(Stock.Returns.Monthly) = colnames(Stock.Prices.Daily)

# Number of stocks left
ncol(Stock.Returns.Monthly)


# 1.2 Regression -----------------------------------------------------------
temp.result = vector()

for(i in 1: ncol(Stock.Returns.Monthly)) {
          RiRF  = Stock.Returns.Monthly[ ,i] - FF$RF
    Regression  = lm(RiRF ~ FF$Mkt.RF + FF$SMB + FF$HML)
    
    temp.result = rbind(temp.result, 
                       cbind(data.frame(t(summary(Regression)$coefficients[ ,1])),
                             data.frame(t(summary(Regression)$coefficients[ ,4])),
                             data.frame(t(summary(Regression)$r.squared))))
    rm(RiRF)
}

Regression.results = data.frame(Ticker = colnames(Stock.Returns.Monthly), temp.result)

colnames(Regression.results) = c("Ticker", 
                                 "Intercept", "Mkt-Rf", "SMB", "HML", 
                                 "P(Intercept)", "P(Mkt-Rf)", "P(SMB)", "P(HML)",
                                 "R.squared")

# contains all stocks with some regression results have NAs
Regression.results2 = merge(x = Period.Returns, 
                            y = Regression.results, 
                           by = "Ticker", 
                        all.x = TRUE)

# Regression result + Mapping
Regression.results  = merge(x = Regression.results, 
                            y = Mapping, 
                           by = "Ticker",
                        all.x = TRUE)

# 1.3 Analysis ------------------------------------------------------------

# display stocks with highest r.squared 

head( 
  Regression.results[order(Regression.results$R.squared,
                           decreasing = TRUE), 
                     c("Ticker","Name","Sector","R.squared")])

# boxplot 

myboxplot = function(plot.melt, xlab = "Factors", ylab = "Value") {
  
  colnames(plot.melt)[1] = "Factors"
  
  ggplot(data = plot.melt, aes(x = Factors, y = value)) + 
  geom_boxplot(color = "#1e314f") + 
  facet_wrap(~ variable, scales = 'free') + 
  labs(x = xlab, y = ylab)
  
}

num.stocks = dim(Regression.results)[1]

plot.data  = data.frame(Factors      = rep(c("Intercept", "Mkt-Rf", "SMB", "HML"),
                                           rep(num.stocks, 4)),
                        Coefficients = melt(Regression.results[ ,2:5])[ ,2],
                        P.Values     = melt(Regression.results[ ,6:9])[ ,2])

# plot betas / p-value by factors -----
plot.melt         = melt(plot.data, "Factors")
plot.melt$Factors = factor(plot.melt$Factors, levels = c("Intercept", "Mkt-Rf", "SMB", "HML"))
myboxplot(plot.melt)

# plot betas by sectors -----
plot.melt  = melt(Regression.results[ , c(2:5, 13)] , "Sec")
myboxplot(plot.melt, xlab = "Sectors", ylab = "Regressed Coefficients")

# plot returns by sectors -----
plot.melt  = melt(Regression.results2[ , c(2, 5)]   , "Sec")
ggplot(plot.melt, aes(x=Sec, y=value)) + 
  geom_boxplot(color = "#1e314f") + 
  
  labs(x = "Sectors", y = "Price Returns") + 
  
  annotate("text", x = "CD", y = 12,   label = "ULTA",           size = 3, color = "#1e314f") +
  annotate("text", x = "CS", y = 14.5, label = "STZ",            size = 3, color = "#1e314f") +
  annotate("text", x = "E",  y = 8.7,  label = "ANDV",           size = 3, color = "#1e314f") +
  annotate("text", x = "H",  y = 21.5, label = "ABMD",           size = 3, color = "#1e314f") +
  annotate("text", x = "H",  y = 22.9, label = "ABIOMED, Inc.",  size = 3, color = "#738cb5") +
  annotate("text", x = "H",  y = 15.3, label = "REGN",           size = 3, color = "#1e314f") +
  annotate("text", x = "I",  y = 17.1, label = "URI",            size = 3, color = "#1e314f") +
  annotate("text", x = "IT", y = 26.5, label = "Netflix Inc.",   size = 3, color = "#738cb5") +
  annotate("text", x = "IT", y = 25.1, label = "NFLX",           size = 3, color = "#1e314f") +
  annotate("text", x = "IT", y = 15.4, label = "AVGO",           size = 3, color = "#1e314f") +
  annotate("text", x = "M",  y = 7.4,  label = "SHW",            size = 3, color = "#1e314f") +
  annotate("text", x = "M",  y = -1.5, label = "MOS",            size = 3, color = "#87382f") +
  annotate("text", x = "RE", y = 9.6,  label = "EXR",            size = 3, color = "#1e314f") +
  annotate("text", x = "RE", y = -2,   label = "WELL",           size = 3, color = "#87382f") +
  annotate("text", x = "RE", y = -3.4, label = "Welltower Inc.", size = 3, color = "#af5a50") +
  annotate("text", x = "U", y = 7.9,   label = "NI",             size = 3, color = "#1e314f")


# 1.4 Stock selection -----------------------------------------------------

# period returns are calculated above in line 45

# correlation plots -----
Regression.results2 = Regression.results2[ !is.na(Regression.results2$R.squared) , ]

Top.20      = head(Regression.results2[order(Regression.results2$Return.2010.2017,
                                        decreasing = TRUE) , ], n = 20 )
Bottom.20   = head(Regression.results2[order(Regression.results2$Return.2010.2017) , ], n = 20)

mycorrplot  = function(plot.data) {
  colnames(plot.data)[1]  = "Return"
  M                       = cor(plot.data)
  corrplot.mixed(M, upper = "ellipse", lower.col = "#1e314f", tl.col = "black")
}

mycorrplot(Regression.results2[ , c(2,6:9)])
mycorrplot(Top.20[ , c(2,6:9)])
mycorrplot(Bottom.20[ , c(2,6:9)])

# compare factor significance and return -----

ecdf_percentile = ecdf(Regression.results2$Return.2010.2017)

mydataprep = function(reg.result, i, decrease = TRUE, significance = 1.00) {
  temp = reg.result[ , c(1:5, i, i + 4)]
  temp = temp[temp[ , 7] < significance, ]
  temp = head(temp[order(temp[ , 6], decreasing = decrease), ], n = 20)
  
  temp = cbind(temp, rep(colnames(reg.result)[i],20), ecdf_percentile(temp$'Return.2010.2017'))
  colnames(temp) = c("Ticker","Return.2010.2017","Name","Sector","Sec","Factor.Value", "Factor.P", "Factor", "Return.Percentile")
  
  return(temp)
}


Top            = data.frame()
Bottom         = data.frame()
Top.5pc.Sig    = data.frame() # 5% significance
Bottom.5pc.Sig = data.frame() # 5% significance

for(i in 6:9){
  # Top stocks of each beta
  Top            = rbind(Top,            mydataprep(Regression.results2, i, TRUE,  1.00))
  Top.5pc.Sig    = rbind(Top.5pc.Sig,    mydataprep(Regression.results2, i, TRUE,  0.05))
  Bottom         = rbind(Bottom,         mydataprep(Regression.results2, i, FALSE, 1.00))
  Bottom.5pc.Sig = rbind(Bottom.5pc.Sig, mydataprep(Regression.results2, i, FALSE, 0.05))
}

# Without filtering significance
df.melt = rbind(Top[ , c("Factor", "Return.Percentile")],
                Bottom[ , c("Factor", "Return.Percentile")] )

df.melt = cbind(c(rep("Large", 80), rep("Small", 80)), df.melt)
colnames(df.melt) = c("Factors","variable", "value")

myboxplot(df.melt, ylab = "Return Percentile")

# Filter significance at 5%
df.melt = rbind( Top.5pc.Sig[ , c("Factor", "Return.Percentile")], 
                 Bottom.5pc.Sig[ , c("Factor", "Return.Percentile")] )
df.melt = cbind(c(rep("Large", 80), rep("Small", 80)), df.melt)
colnames(df.melt) = c("Factors","variable", "value")

myboxplot(df.melt, ylab = "Return Percentile", xlab = "Factors 5% significance")



# 2.  S&P 500 1980 - 2015 / 5Y / Trend Analysis ----------------------------

# batch regression

List.of.start.date = seq(as.Date("1980/1/1"), as.Date("2016/1/1"), "years")
List.of.start.date = List.of.start.date[ year(List.of.start.date) %% 5 == 0 ]

# Each batch stores results for a 5yr group
Descriptions  = list()
Beta.batch    = list()

for(i in 1 : ( length(List.of.start.date) - 1 )){
  start.date = as.Date( List.of.start.date[i])
  end.date   = as.Date( List.of.start.date[i + 1]) - 1
  print(paste(start.date, end.date , sep = " - "))
  
  # read data
  file.name       = paste("Data/SP500_price.adjusted_", 
                          paste( year(start.date), year(end.date), sep = "-"),
                          ".csv", sep = "")
  
  SP500.data      = read.csv(file.name)
  SP500.data$date = as.Date(SP500.data$date)
  
  # remove first column "X" created due to importing  
  Stock.Prices.Daily = SP500.data[SP500.data$date >= start.date & 
                                  SP500.data$date <= end.date , -1]
  
  # Convert series to XTS for using quantmod's monthlyReturn function
  Stock.Prices.Daily = xts(Stock.Prices.Daily[ ,-1], 
                           order.by = as.POSIXct(Stock.Prices.Daily$date))
  
  
  # try a diff approach: loop over stocks and convert to monthly for each stock
  
  # initialize
  Description = data.frame()  # 1 Description for each period, and they stored in Descriptions
  betas       = data.frame()
  
  # loop through stocks
  for(j in 1 : ncol(Stock.Prices.Daily)){
    # The j-th stock
    Rj = Stock.Prices.Daily[ ,j]
    
    cat(colnames(Stock.Prices.Daily[ ,j]), " ")
    
    # remove MNST from 1985 - 1989
    if(colnames(Stock.Prices.Daily[ ,j]) == "MNST" && year(start.date) == 1985) next
    
    # non-NA entries
    Rj = Rj[!is.na(Rj), ]
    Rj = monthlyReturn(Rj)
    
    # matching FF data
    FF = FF3[FF3$X >= format(index(head(Rj, n = 1)), "%Y%m") & 
             FF3$X <= format(index(tail(Rj, n = 1)), "%Y%m"), ]
    
    # Rj is now RjRF
    Rj = Rj - FF$RF
    Regression  = lm(Rj ~ FF$Mkt.RF + FF$SMB + FF$HML)
    Description = rbind(Description, 
                        data.frame(colnames(Stock.Prices.Daily[ ,j]), 
                                   format(index(head(Rj, n = 1)), "%Y%m"), 
                                   format(index(tail(Rj, n = 1)), "%Y%m"), 
                                   length(Rj)))
    
    # read-out results at regression time
    # betas, p-values, r-squareds
    betas = rbind(betas, cbind(data.frame(t(summary(Regression)$coefficients[ ,1])),
                               data.frame(t(summary(Regression)$coefficients[ ,4])),
                               data.frame(t(summary(Regression)$r.squared))))
  }
  
  print("")
  
  
  # Save the ticker / dates for ease of tracking the regression summary
  colnames(Description) = c("Ticker", "Start.Month", "End.Month", "Number.of.Months")
  Descriptions[[i]]     = Description
  
  # Save the regression results for plotting
  colnames(betas) = c("Intercept", "Mkt-Rf", "SMB", "HML", 
                      "P(Intercept)", "P(Mkt-Rf)", "P(SMB)", "P(HML)",
                      "R-squared")
  Beta.batch[[i]] = betas
  
  # remove temp variables
  rm(Description, Regression, Rj, betas)
}

# Boxplot
# Combine all batches together into one large dataframe
df      = data.frame()
Num.Obs = data.frame()

for(i in 1:(length(List.of.start.date) - 1)){
  start.date = as.Date(List.of.start.date[i])
  end.date   = as.Date(List.of.start.date[i + 1]) - 1
  label      = paste(year(start.date), year(end.date),sep = "-")
  
  df         = rbind(df,
                     cbind(rep(label, dim(Beta.batch[[i]])[1]), 
                        Beta.batch[[i]]))
  Num.Obs    = rbind(Num.Obs,
                     cbind(paste(year(start.date), year(end.date),sep="-"),
                           dim(Beta.batch[[i]])[1]))
}

colnames(df) = c("Year",
                 "Intercept", "Mkt-Rf", "SMB", "HML", 
                 "P(Intercept)", "P(Mkt-Rf)", "P(SMB)", "P(HML)",
                 "R-squared")

df.melt = melt(df[ ,1:5], "Year")
myboxplot(df.melt, xlab = "Years", ylab = "Regressed Coefficients")

df.melt = melt(df[ ,c(1,6:9)], "Year")
myboxplot(df.melt, xlab = "Years", ylab = "P Values")

colnames(Num.Obs) = c("Time Period", "Number of Stocks")
# write.csv(Num.Obs, "Data/SP500_Number_of_Stocks_1980_2015.csv")


# Run batch regression again from line 256 but this time with 168 surviving stocks

# Find surviving stocks in all periods

Fix.Tickers   = Descriptions[[1]]$Ticker

for(i in 2:7) {
  Fix.Tickers = Reduce(intersect, list(Fix.Tickers, Descriptions[[i]]$Ticker))
}
  
# 168 stock run -----------------------------------------------------------

Beta.batch    = list()

for(i in 1 : ( length(List.of.start.date) - 1 ) ){
  start.date = as.Date( List.of.start.date[i] )
  end.date   = as.Date( List.of.start.date[i + 1] ) - 1
  print(paste(start.date, end.date , sep = " - "))
  
  # read data
  file.name       = paste("Data/SP500_price.adjusted_", 
                          paste( year(start.date), year(end.date), sep = "-" ), 
                          ".csv", sep = "")
  SP500.data      = read.csv(file.name)
  SP500.data$date = as.Date(SP500.data$date)
  
  Stock.Prices.Daily = SP500.data[SP500.data$date >= start.date & 
                                  SP500.data$date <= end.date,  -1]
  Stock.Prices.Daily = xts(Stock.Prices.Daily[ ,-1], 
                           order.by = as.POSIXct(Stock.Prices.Daily$date))
  
  # select only tickers surviving all periods
  Stock.Prices.Daily = Stock.Prices.Daily[ , Fix.Tickers]
  
  # initialize
  betas       = data.frame()
  
  # loop through stocks
  for(j in 1 : ncol(Stock.Prices.Daily)){
    Rj = Stock.Prices.Daily[,j]
   
    # remove MNST from 1985 - 1989
    if(colnames(Stock.Prices.Daily[,j]) == "MNST" && year(start.date) == 1985) next
    
    # non-NA entries
    Rj = Rj[!is.na(Rj), ]
    Rj = monthlyReturn(Rj)
    
    # matching FF data
    FF = FF3[FF3$X >= format(index(head(Rj, n=1)), "%Y%m") & 
             FF3$X <= format(index(tail(Rj, n=1)), "%Y%m"), ]
    
    Rj = Rj - FF$RF
    Regression  = lm(Rj ~ FF$Mkt.RF + FF$SMB + FF$HML)
    # betas, p-values, r-squareds
    betas = rbind(betas, cbind(data.frame(t(summary(Regression)$coefficients[ ,1])),
                               data.frame(t(summary(Regression)$coefficients[ ,4])),
                               data.frame(t(summary(Regression)$r.squared))))
  }
  
  # Save the regression results for plotting
  colnames(betas) = c("Intercept", "Mkt-Rf", "SMB", "HML", 
                      "P(Intercept)", "P(Mkt-Rf)", "P(SMB)", "P(HML)",
                      "R-squared")
  Beta.batch[[i]] = betas
  
  # remove temp variables
  rm(Regression, Rj, betas)
}

# Boxplot
# Combine all batches together into one large dataframe
df      = data.frame()

for(i in 1:(length(List.of.start.date) - 1)){
  start.date = as.Date(List.of.start.date[i])
  end.date   = as.Date(List.of.start.date[i + 1]) - 1
  label      = paste(year(start.date), year(end.date),sep = "-")
  
  df = rbind(df, 
             cbind(rep(label, dim(Beta.batch[[i]])[1]), 
             Beta.batch[[i]]))
}

colnames(df) = c("Year",
                 "Intercept", "Mkt-Rf", "SMB", "HML", 
                 "P(Intercept)", "P(Mkt-Rf)", "P(SMB)", "P(HML)",
                 "R-squared")

df.melt = melt(df[ , 1:5], "Year")
myboxplot(df.melt, xlab = "Years", ylab = "Regressed Coefficients (168 Stocks)")

df.melt = melt(df[ , c(1,6:9)], "Year")
myboxplot(df.melt, xlab = "Years", ylab = "P Values (168 Stocks)")


rm(SP500.data, FF, FF3, temp.result, file.name, 
   start.date, end.date, List.of.start.date,
   label, i, j, num.stocks, 
   plot.data, plot.melt, df, df.melt)

