##################################################################
#                                                                #
#     Replication of the Fama French Three Factor Model          #
#                                                                #
##################################################################

#1# Data preparation

library(quantmod)
library(xts)
library(ggplot2)
library (readxl)

# FF3 is the monthly data collection of the three factors from 1963/07 to 1991/12
FF3 = read_excel ("Data/FF3_196307-199112.xlsx")

# P25 is value-weighted 5*5 portfolios from 1963/07 to 1991/12
P25 = read_excel ("Data/FF3_25_ValueWeighted.xlsx")


#2# Read regression factors

# Unlist: convert the data into vector format
rmrf = unlist (FF3[, 2])
smb  = unlist (FF3[, 3])
hml  = unlist (FF3[, 4])
rf   = unlist (FF3[, 5])
ri   = unlist (P25[, 2])
rirf = unlist (ri-rf) 
rirf 


#3# Start batch regressing 25 portfolios 

results = list ()

# Data starts from the 2nd col of P25
for (i in 1: (ncol (P25) - 1)) { 
  rirf = unlist (P25[, i + 1]) - rf   
     y = lm (rirf ~ rmrf + smb + hml)
  results [[i]] = summary (y)
}

# Set up empty vectors

betas      = vector ()
std.errors = vector ()
t.values   = vector ()
R.squareds = vector ()

# Save all betas' regression results

for (i in 1: (ncol (P25) - 1)) {
       betas = cbind (betas, results[[i]]$coefficients[, 1])
  std.errors = cbind (std.errors,results[[i]]$sigma)
    t.values = cbind (t.values, results[[i]]$coefficients[, 3])
  R.squareds = cbind (R.squareds, results[[i]]$adj.r.squared)
}

# Give names
colnames (betas) = colnames (P25)[-1]


#4# Resize the output to 5x5 format like Fama French paper
resize = function (x) {
  return (matrix (x, nrow=5, byrow = TRUE, 
                  dimnames = list (c ("SMALL", "2", "3", "4", "BIG"), c ("LOW", "2", "3", "4", "HIGH"))))
}

# Resize alpha (intercept)
alpha = resize (betas[1, ])
alpha

# Resize beta
market.beta = resize (betas[2, ])
SMB.beta    = resize (betas[3, ])
HML.beta    = resize (betas[4, ])

# Display betas
market.beta
SMB.beta
HML.beta

# Resize t-stats
market.t = resize (t.values[2,])
SMB.t    = resize (t.values[3,])
HML.t    = resize (t.values[4,])
market.t

# Resize R-squareds
resize (R.squareds)
resize (std.errors)

# Create a table with all coefficients and t statistics
coef.t = cbind (rbind (alpha, market.beta, SMB.beta, HML.beta), 
                 rbind (resize (t.values[1,]), market.t, SMB.t, HML.t))
View (coef.t)


# heat map
heat.plot = function(df, legend.label = "Return"){
  # prepare data into 25 rows vector
  plot.data        = expand.grid(HML = c("LOW", "2", "3", "4", "HIGH"),
                            SMB = c("SMALL", "2", "3", "4", "BIG"))
  plot.data$Return = matrix(df, nrow = 25)

  # plot
  ggplot(data = plot.data, aes(x = HML, y = SMB, fill = Return)) + 
    geom_tile() + 
    geom_text(aes(label=round(Return, digits = 2))) + 
    labs(fill = legend.label)
}

# mean excess return of the 25 FF portfolios
heat.plot(colMeans(P25[,-1] - FF3$RF))

heat.plot(alpha,       "Alpha")
heat.plot(market.beta, "Market Beta")
heat.plot(SMB.beta,    "SMB Beta")
heat.plot(HML.beta,    "HML Beta")

