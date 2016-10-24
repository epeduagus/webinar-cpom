#############################################################################
###
###  Webinar - Contemporary Portfolio Optimization Modeling with R
###  -------------------------------------------------------------
###  Ronald Hochreiter - http://www.finance-r.com/cpom/
###
#############################################################################

#############################################################################
##### 1. Setup & Data

library(webinar.cpom)
library(quantmod)
library(PerformanceAnalytics)

### 1.1. Use Hedge Fund Data

data(hedgefund)
scenario <- hedgefund

assets <- ncol(scenario)
scenarios <- nrow(scenario)

cat("----- Hedge Fund Strategies -----\n", paste0(names(scenario), "\n"))
chart.Bar(scenario$Event.Driven)
charts.PerformanceSummary(scenario$Event.Driven)

#############################################################################
##### 2. Package - tseries

library(tseries)

### 2.1. Easy Markowitz Portfolio Optimization

timeframe <- "2003/2007" # Examples: "2003/2007" | "2007/2010"
portfolio <- round(portfolio.optim(scenario[timeframe])$pw, 2)

pie_labels <- names(scenario); pie_labels[which(portfolio == 0)] <- NA
pie(portfolio, labels=pie_labels, main=paste("Portfolio-Composition ", timeframe))

### 2.2. Easy Backtesting

window <- 5 * 12 # 5 years rolling window (5 times 12 months, monthly data)

# portfolio optimization backtesting in one line/loop!
ret <- vector()
for (pos in (window+1):scenarios) 
  ret <- c(ret, sum(round(portfolio.optim(scenario[(pos-window):(pos-1)])$pw, 2) * scenario[pos]))

# create a time series object and plot the performance of backtesting
backtesting <- as.xts(ret, order.by=index(scenario[(window+1):scenarios]))
charts.PerformanceSummary(backtesting)

#############################################################################
##### 3. Package - fPortfolio

### 3.1. fPortfolio with scenario Strategies

library(fPortfolio)

# convert time series format
scenario.ts <- as.timeSeries(scenario)

# specification: solver and efficient fronier 
spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
setNFrontierPoints(spec) <- 20

# constraints
constraints <- c('LongOnly')
portfolioConstraints(scenario.ts, spec, constraints)

# optimization
frontier <- portfolioFrontier(scenario.ts, spec, constraints)
print(frontier)

# plotting
tailoredFrontierPlot(frontier)
weightsPlot(frontier) 
weightsPlot(frontier, col=rainbow(ncol(scenario))) 

# adding and changing constraints
constraints <- c('minW[1:assets]=0', 'maxW[1:assets]=0.5')
portfolioConstraints(scenario.ts, spec, constraints)
frontier <- portfolioFrontier(scenario.ts, spec, constraints)
weightsPlot(frontier) 

#############################################################################
##### 4. Plain Matrix-Based Manual Optimization

library(modopt.matlab)

### 4.1. Manual Minimum Variance Portfolio (MVP) computation

H <- cov(scenario)
f <- rep(0, assets)
Aeq <- rep(1, assets)
beq <- 1
lb <- rep(0, assets)
ub <- rep(1, assets)

solution <- quadprog(H, f, NULL, NULL, Aeq, beq, lb, ub)
portfolio <- round(solution$x, 2)

### 4.2. Manual Markowitz efficient frontier computation

markowitz <- function(data, mu) {
  H <- cov(data)
  f <- rep(0, assets)
  Aeq <- rep(1, assets)
  beq <- 1
  A <- -as.numeric(colMeans(data))
  b <- -mu
  lb <- rep(0, assets)
  ub <- rep(1, assets)
  solution <- quadprog(H, f, A, b, Aeq, beq, lb, ub)
  return(round(solution$x, 2))
}

data <- scenario["2011/2015"]

portfolio.m <- c()
portfolio.sd <- c()
for(m in seq(mean(colMeans(data)), max(colMeans(data))*0.99, length.out=10)) {
  portfolio <- markowitz(data, m)
  portfolio.m <- c(portfolio.m, mean(portfolio %*% t(data)))
  portfolio.sd <- c(portfolio.sd, sd(portfolio %*% t(data)))
}
plot(portfolio.sd, portfolio.m, xlab="Standard Deviation", ylab="Return", main="Efficient Frontier")
lines(portfolio.sd, portfolio.m)

### 4.3. Manual CVaR/ES modeling

#############################################################################
##### 5. PortfolioAnalytics

#############################################################################
##### 6. scenportopt

library(scenportopt)

### 6.1. Comparison

markowitz <- model <- optimal.portfolio(scenario)

# Expected Shortfall/CVaR with alpha=95% and alpha=90%
cvar95 <- optimal.portfolio(objective(model, "expected.shortfall"))
cvar90 <- optimal.portfolio(alpha(model, 0.1))

# Mean Absolute Deviation (MAD)
mad <- optimal.portfolio(objective(model, "mad"))

# Plot Comparison
compare <- matrix(c(x(markowitz), x(mad), x(cvar95), x(cvar90)), nrow=nasset(model), byrow=FALSE)
barplot(t(compare), beside=TRUE, col=rainbow(4), las=3, names.arg=names(scenario), legend=c("Markowitz", "MAD", "CVaR (95%)", "CVaR (90%)"))

# Add upper bounds (0.15) and repeat optimizations
markowitz <- model <- optimal.portfolio(upper.bound(portfolio.model(scenario), 0.15))
cvar95 <- optimal.portfolio(objective(model, "expected.shortfall"))
cvar90 <- optimal.portfolio(alpha(model, 0.1))
mad <- optimal.portfolio(objective(model, "mad"))
compare <- matrix(c(x(markowitz), x(mad), x(cvar95), x(cvar90)), nrow=nasset(model), byrow=FALSE)
barplot(t(compare), beside=TRUE, col=rainbow(4), las=3, names.arg=names(scenario), legend=c("Markowitz", "MAD", "CVaR (95%)", "CVaR (90%)"))

### 6.2. Natural fit into contemporary R coding styles (piping)

x(optimal.portfolio(scenario))

library(magrittr)
scenario %>% optimal.portfolio %>% x
scenario %>% portfolio.model %>% objective("expected.shortfall") %>% alpha(0.1) %>% optimal.portfolio %>% x

### 6.3. Active-extension portfolios ("130/30")

model <- optimal.portfolio(data)
model <- active.extension(model, 130, 30)
cvar13030 <- optimal.portfolio(objective(model, "expected.shortfall"))
mad13030 <- optimal.portfolio(objective(model, "mad"))
barplot(matrix(c(x(cvar13030), x(mad13030)), nrow=2, byrow=TRUE), las=3, names.arg=names(data), beside=TRUE, col=topo.colors(2))

#############################################################################
##### 7. Contemporary Portfolio Modeling

library("ROI")
library("ROML")
library("ROML.portfolio")

# 7.1. An AML (Algebraic Modeling Language) for R (ROML)

m <- model()
m$variable(x, length=1L)
m$variable(y, length=1L)
m$maximize( 2*x + y )
m$subject_to ( x <= 3 )
m$subject_to ( y <= 5 )
opt <- optimize(m, solver="glpk")
solution(opt)

# 7.2. Full flexibility

m <- model()
m$variable(portfolio, lb = 0) 
m$maximize ( reward(portfolio) )
m$subject_to ( budget_norm(portfolio) )
m$subject_to ( portfolio[1] + portfolio[3] + portfolio[5] == 0.5 )

opt <- optimize(m, solver="glpk", data=list(returns = as.matrix(scenario))) 
as.numeric(opt$solution)
colMeans(scenario)
