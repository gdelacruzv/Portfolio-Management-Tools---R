#############
# LIBRARIES #
#############
library(quantmod)
library(PerformanceAnalytics)
library(plyr)
library(quadprog)
library(Matrix)
library(matrixcalc)
library(ggplot2)
library(reshape2)
library(lubridate)

#################
# GLOBAL INPUTS #
#################

# How far back do we download data?
HISTPERIOD <- 11 # Look back 11 years
maxDate <- Sys.Date()-years(HISTPERIOD) # Look back 11 years

# Remove most recent date unless today is the last day of the month
REMOVE.MOST.RECENT.DATE <- T

# Lookback period for optimization input estimation
LOOKBACK <- 12 # How many MONTHS do we look back when performing the optimization?

# Which benchmark do we use?
Benchmark.Ticker <- "SPY" # Benchmark Index

# Which assets?
tickers <- c("XLF", "XLP", "VTI", "VTV", "XLY",'QQQ')

# How many periods in a year? E.g. 12 for months
periodScale <- 12

################################
# Downloading and merging data #
################################

# Download prices for each stock
StockData <- new.env()
l_ply(tickers, function(ticker) getSymbols(ticker,env = StockData,src = "yahoo",from = maxDate, to = Sys.Date()), .progress = "tk") 
prices <- to.monthly(do.call(merge, eapply(StockData, Ad)[tickers]),OHLC=F)
colnames(prices) <- tickers # Rename for charting
Benchmark.prices <- to.monthly(Ad(getSymbols(Benchmark.Ticker, from = maxDate, to = Sys.Date(), src="yahoo", auto.assign = F)), OHLC=F)

# Calculate returns for each stock
returns <- ROC(na.omit(prices), type="discrete")[-1,]
Benchmark.returns <- ROC(na.omit(Benchmark.prices), type="discrete")[-1,]+.005 
colnames(Benchmark.returns) <- "Benchmark"

# Combine Returns into One DataFrame including Benchmark
CombinedRets <- na.omit(merge(Benchmark.returns, returns))
paste("Your combined data including the Benchmark goes back to",index(CombinedRets)[1])

if(REMOVE.MOST.RECENT.DATE){
  # TBD: Check if today is the last day of the month
  paste("We are removing the most recent data point since today is not the last day of the month")
  CombinedRets <- head(CombinedRets,-1)
}
paste("Your combined data leads up until",index(tail(CombinedRets,1)))

########################################
# Begin Portfolio Optimization Process #
########################################

minSim <- LOOKBACK + 1
maxSim <- (dim(CombinedRets)[1]-1)
BackTestResults <- NA
BackTestWeights <- NA

# Quadratic portfolio optimization function, outputs optimal weights
quadraticFit <- function(trainData.BM, trainData, testData, testData.BM, ...){
  
  # Extract dimensions
  trainrows = dim(trainData)[1]
  traincols = dim(trainData)[2]
  
  # Calculate D matrix and dvec
  Dmat = cov(trainData)
  dvec = cov(trainData.BM, trainData)
  
  # Catch non-positive semi-definite matrices before passing it in
  # Squeeze to positive definite if needed
  if(!is.positive.definite(Dmat)) Dmat <- as.matrix(nearPD(Dmat)$mat)
  
  # Goal: A' b >= b_0 
  
  # Weight constraints: Fully invested
  # SUM[w_i] == 1
  a1 = rep(1, traincols)
  
  # In b0, which represents the right side of the equation, this vector is 
  # paired with the value '1'.
  
  # The second constraint sets the lower bound of the weights to zero.
  
  # First, we set up an identity matrix.  
  a2 = matrix(0, traincols, traincols)
  diag(a2) = 1
  
  # It's paired in b0 with a vector of lower bounds set to zeros:
  w.min = rep(0, traincols) # Change this for minimum weights
  
  # Construct A from the two components a1 and a2
  Amat = t(rbind(a1, a2))
  
  # Construct constraints vector b_0
  b0 = c(1.0, w.min) # 100% target allocation
  
  # Solve quadratic problem
  optimal <- solve.QP(Dmat, dvec, Amat, bvec=b0, meq=1)
  
  ### Testing Period ###
  
  # Extract solution and names
  weights = as.data.frame(optimal$solution)
  rownames(weights) = colnames(testData)
  colnames(weights) = index(testData.BM)
  
  # Now Evaluate Results with test Data
  TestReturns <- testData*t(weights) # Weighted returns
  TestReturns$Portfolio <- sum(TestReturns) # Portfolio return
  TestReturns$Benchmark <- testData.BM # Benchmark return
  
  # Return results
  result <- list(weights = weights,TestReturns=TestReturns)
  return(result)
}

# Go through each simulation period on a rolling basis to avoid lookahead bias
for( simTick in minSim:maxSim){
  
  # Split data according to indexes for training periods
  trainData <- CombinedRets[(simTick - LOOKBACK):simTick,-1]
  trainData.BM <- CombinedRets[(simTick - LOOKBACK):simTick,1]
  
  # Split data for testing periods
  testData <- CombinedRets[(simTick+1):(simTick+1),-1]
  testData.BM <- CombinedRets[(simTick+1):(simTick+1),1]
  
  # Run quadratic optimization function defined above
  FitPrev <- quadraticFit(trainData.BM, trainData, testData, testData.BM)
  
  #########################
  # Now Aggregate Results #
  #########################
  
  if(simTick == minSim){ # First simulation, set variables
    BackTestResults <- FitPrev$TestReturns
    BackTestWeights <- as.xts(as.zoo(t(FitPrev$weights), order.by=index(FitPrev$TestReturns)))
  } else {
    BackTestResults <- rbind(BackTestResults, FitPrev$TestReturns)
    BackTestWeights <- rbind(BackTestWeights, as.xts(as.zoo(t(FitPrev$weights), order.by=index(FitPrev$TestReturns))))
  }
  
  # Output progress
  print(paste(round((simTick- minSim)/(maxSim - minSim) * 100, 2), "% done..."))
}

################################
# Analyze Backtest Performance #
################################

# Custom Colors
myColors <- c("#000000","#1f78b4","#33a02c","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a")

# Transaction cost
TXNCost <- 0.0020 # 20 basis point transaction cost
PortfolioTurnover <- rowSums(abs(rbind(0,apply(BackTestWeights[,1:length(tickers)], 2, diff))))
plot(PortfolioTurnover, type = "l")

BackTestResults$PortfolioTXN20bps <- BackTestResults$Portfolio - PortfolioTurnover*TXNCost

# Rolling Performance vs the Benchmark
charts.RollingPerformance(BackTestResults[,c("Benchmark", "Portfolio", "PortfolioTXN20bps")], scale=periodScale, main=paste0("Rolling Quarterly Portfolio Performance vs. Benchmark (",Benchmark.Ticker,")"), colorset=myColors, width=3, legend.loc="topleft")
charts.RollingPerformance(BackTestResults[,c("Benchmark", "Portfolio", "PortfolioTXN20bps")], scale=periodScale, main=paste0("Rolling Semi-Annual Portfolio Performance vs. Benchmark (",Benchmark.Ticker,")"), colorset=myColors, width=6, legend.loc="topleft")
charts.RollingPerformance(BackTestResults[,c("Benchmark", "Portfolio", "PortfolioTXN20bps")], scale=periodScale, main=paste0("Rolling Annual Portfolio Performance vs. Benchmark (",Benchmark.Ticker,")"), colorset=myColors, width=12, legend.loc="topleft")

# Tracking Error Over Time
TE.rolling <- na.omit(rollapply(BackTestResults, 3, FUN = function(x) TrackingError(x[,"Portfolio"],x[,"Benchmark"], scale=periodScale),by.column = FALSE, align = "right"))
plot(TE.rolling, main=paste0("Rolling Quarterly Tracking Error vs. Benchmark (",Benchmark.Ticker,")"))

TE.rolling <- na.omit(rollapply(BackTestResults, 6, FUN = function(x) TrackingError(x[,"Portfolio"],x[,"Benchmark"], scale=periodScale),by.column = FALSE, align = "right"))
plot(TE.rolling, main=paste0("Rolling Semi-Annual Tracking Error vs. Benchmark (",Benchmark.Ticker,")"))

TE.rolling <- na.omit(rollapply(BackTestResults, 12, FUN = function(x) TrackingError(x[,"Portfolio"],x[,"Benchmark"], scale=periodScale),by.column = FALSE, align = "right"))
plot(TE.rolling, main=paste0("Rolling Annual Tracking Error vs. Benchmark (",Benchmark.Ticker,")"))

# Histogram of results
par(mfrow=c(1,1))
hist(BackTestResults[,c("Benchmark")], breaks="fd", border = "tomato", main="Benchmark vs Portfolio\nDistribution of Returns", xlab="Monthly Return", ylim=c(0,30))
hist(BackTestResults[,c("Portfolio")], breaks="fd", border = "steelblue", add=T)
legend("topleft", c(Benchmark.Ticker, "Portfolio"), col = c("tomato", "steelblue"), bty="n", lty=c(1,1))

plot(as.numeric(BackTestResults[,c("Benchmark")]), as.numeric(BackTestResults[,c("Portfolio")]), xlab=paste0("Benchmark (",Benchmark.Ticker,") Return"), ylab="Portfolio Return")
abline(0, 1)
abline(coef(lm(BackTestResults[,c("Benchmark")] ~ BackTestResults[,c("Portfolio")])), col="red")
legend("topleft", c("Perfect Tracking", "Regression Line"), col = c("black", "red"), lty=c(1,1))

# Plot Portfolio Weights Over Time
meltedWeights <- melt(cbind(Date=index(BackTestWeights),as.data.frame(BackTestWeights)), id=1, value.name="Weight", variable.name="Asset")
ggplot(meltedWeights, aes(x = Date, y=Weight, fill=Asset)) + geom_bar(stat = "identity") + scale_x_yearqtr(format="%YQ%q", n=nrow(BackTestWeights)/4)

# Render Charts
numAssets <- length(tickers)
charts.PerformanceSummary(BackTestResults[,c("Benchmark", "Portfolio","PortfolioTXN20bps")], main="Portfolio Performance vs. Benchmark", colorset=myColors)
charts.PerformanceSummary(BackTestResults[,c(1:numAssets)], main="Cumulative Performance Contribution of All Assets", colorset=myColors[-1])

# Output Selected Portfolio Stats
table.AnnualizedReturns(BackTestResults[,c("Benchmark", "Portfolio","PortfolioTXN20bps")], scale=periodScale)

# Most recent weights
tail(round(BackTestWeights,2))

