#############################################################################
###
### Webinar - Contemporary Portfolio Optimization Modeling with R
### -------------------------------------------------------------
### Ronald Hochreiter - http://www.finance-r.com/cpom/
###
### Webinar Version 1 - October 25th, 2016 (Interactive Brokers)
###
#############################################################################

#############################################################################
##### 1. Setup & Data

library(webinar.cpom)
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

### 1.1. Use Hedge Fund Data

data(hedgefund)
scenario.set <- hedgefund

assets <- ncol(scenario.set)
scenarios <- nrow(scenario.set)

cat("\n", paste0(names(scenario.set), "\n"))

chart.Bar(scenario.set$Event.Driven)
charts.PerformanceSummary(scenario.set$Event.Driven)

#############################################################################
##### 2. Package - tseries

library(tseries)

### 2.1. Easy Markowitz Portfolio Optimization

timeframe <- "2003/2007" # Examples: "2003/2007" | "2007/2010"
portfolio <- round(portfolio.optim(scenario.set[timeframe])$pw, 2)
portfolio.pie(portfolio, scenario.set)

portfolio <- scenario.set[timeframe] %>% portfolio.optim() %>% .$pw %>% round(2)

### 2.2. Easy Backtesting

window <- 5 * 12 # 5 years rolling window (5 times 12 months, monthly data)

# portfolio optimization backtesting in one line/loop!
ret <- vector()
for (pos in (window+1):scenarios) 
  ret <- c(ret, sum(round(portfolio.optim(scenario.set[(pos-window):(pos-1)])$pw, 2) * scenario.set[pos]))

# create a time series object and plot the performance of backtesting
backtesting <- as.xts(ret, order.by=index(scenario.set[(window+1):scenarios]))
charts.PerformanceSummary(backtesting)

#############################################################################
##### 3. Package - fPortfolio

library(fPortfolio)

# convert time series format
scenario.ts <- as.timeSeries(scenario.set)

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
weightsPlot(frontier, col=rainbow(ncol(scenario.set))) 

# adding and changing constraints
constraints <- c('minW[1:assets]=0', 'maxW[1:assets]=0.5')
portfolioConstraints(scenario.ts, spec, constraints)
frontier <- portfolioFrontier(scenario.ts, spec, constraints)
weightsPlot(frontier) 

#############################################################################
##### 4. Plain Matrix-Based Manual Optimization

library(modopt.matlab)

### 4.1. Manual Minimum Variance Portfolio (MVP) computation

H <- cov(scenario.set)
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

data <- scenario.set["2011/2015"]

portfolio.m <- c()
portfolio.sd <- c()
for(m in seq(mean(colMeans(data)), max(colMeans(data))*0.99, length.out=10)) {
  portfolio <- markowitz(data, m)
  portfolio.m <- c(portfolio.m, mean(portfolio %*% t(data)))
  portfolio.sd <- c(portfolio.sd, sd(portfolio %*% t(data)))
}
plot(portfolio.sd, portfolio.m, xlab="Standard Deviation", ylab="Return", main="Efficient Frontier")
lines(portfolio.sd, portfolio.m)

#############################################################################
##### 5. PortfolioAnalytics

library(PortfolioAnalytics)
library(ROI)

# initialize portfolio
init.portfolio <- portfolio.spec(assets = colnames(scenario.set))
print.default(init.portfolio)

# adding constraints
init.portfolio <- add.constraint(portfolio = init.portfolio, type = "full_investment")
init.portfolio <- add.constraint(portfolio = init.portfolio, type = "long_only")

# objeective: different risk measures
minSD.portfolio <- add.objective(portfolio=init.portfolio, type="risk", name="StdDev")
meanES.portfolio <- add.objective(portfolio=init.portfolio, type="risk", name="ES")

# optimization
minSD.opt <- optimize.portfolio(R = scenario.set, portfolio = minSD.portfolio, optimize_method = "ROI", trace = TRUE)
meanES.opt <- optimize.portfolio(R = scenario.set, portfolio = meanES.portfolio, optimize_method = "ROI", trace = TRUE)

# check and plot results
print(minSD.opt)
print(meanES.opt)
portfolio.pie(round(minSD.opt$weights,2), scenario.set)
portfolio.pie(round(meanES.opt$weights,2), scenario.set)

#############################################################################
##### 6. scenportopt

library(scenportopt)

### 6.1. Comparison of risk measures

markowitz <- model <- optimal.portfolio(scenario.set)

# Expected Shortfall/CVaR with alpha=95% and alpha=90%
cvar95 <- optimal.portfolio(objective(model, "expected.shortfall"))
cvar90 <- optimal.portfolio(alpha(model, 0.1))

# Mean Absolute Deviation (MAD)
mad <- optimal.portfolio(objective(model, "mad"))

# Plot Comparison
compare <- matrix(c(x(markowitz), x(mad), x(cvar95), x(cvar90)), nrow=nasset(model), byrow=FALSE)
barplot(t(compare), beside=TRUE, col=rainbow(4), las=3, names.arg=names(scenario.set), legend=c("Markowitz", "MAD", "CVaR (95%)", "CVaR (90%)"))

# Add upper bounds (0.15) and repeat optimizations
markowitz <- model <- optimal.portfolio(upper.bound(portfolio.model(scenario.set), 0.15))
cvar95 <- optimal.portfolio(objective(model, "expected.shortfall"))
cvar90 <- optimal.portfolio(alpha(model, 0.1))
mad <- optimal.portfolio(objective(model, "mad"))
compare <- matrix(c(x(markowitz), x(mad), x(cvar95), x(cvar90)), nrow=nasset(model), byrow=FALSE)
barplot(t(compare), beside=TRUE, col=rainbow(4), las=3, names.arg=names(scenario.set), legend=c("Markowitz", "MAD", "CVaR (95%)", "CVaR (90%)"))

### 6.2. Natural fit into contemporary R coding styles (piping)

x(optimal.portfolio(scenario.set))

scenario.set %>% optimal.portfolio %>% x
scenario.set %>% portfolio.model %>% objective("expected.shortfall") %>% alpha(0.1) %>% optimal.portfolio %>% x

### 6.3. Active-extension portfolios ("130/30")

model <- optimal.portfolio(scenario.set)
model <- active.extension(model, 130, 30)
cvar13030 <- optimal.portfolio(objective(model, "expected.shortfall"))
mad13030 <- optimal.portfolio(objective(model, "mad"))
barplot(matrix(c(x(cvar13030), x(mad13030)), nrow=2, byrow=TRUE), las=3, names.arg=names(data), beside=TRUE, col=topo.colors(2))

#############################################################################
##### 7. Contemporary Portfolio Modeling

library("ROI")
library("ROML")
library("ROML.portfolio")

### 7.1. An AML (Algebraic Modeling Language) for R (ROML)

# maximize   2x1 + x2
# subject to x1 + x2 <= 10
#            x1 <= 7

m <- model()
m$variable(x1, length=1L)
m$variable(x2, length=1L)

m$maximize   ( 2*x1 + x2 )
m$subject_to ( x1 + x2 <= 10 )
m$subject_to ( x1 <= 7 )

opt <- optimize(m, solver="glpk")
solution(opt)

### 7.2. ROML.portfolio - full flexibility for learning & prototyping

# 7.2.1. Maximize expectation, use any additional constraint

m <- model(); m$variable(portfolio, lb=0) 

m$maximize   ( reward(portfolio) )
m$subject_to ( budget_norm(portfolio) )
m$subject_to ( portfolio[1] + portfolio[3] + portfolio[5] == 0.5 )

opt <- optimize(m, solver="glpk", data=list(returns = as.matrix(scenario.set))) 
x <- round(as.numeric(opt$solution), 2)
colMeans(scenario.set)

# 7.2.2. Minimize Markowitz risk

m <- model(); m$variable(portfolio, lb=0)

m$minimize   ( markowitz(portfolio) )
m$subject_to ( budget_norm(portfolio) )

opt <- optimize(m, solver="quadprog", data=list(returns = as.matrix(scenario.set))) 
x <- round(as.numeric(opt$solution), 2)

portfolio.pie(x, scenario.set)

# 7.2.3. Changing the risk measure using building blocks (objective)

# reward        | Portfolio return/reward
# markowitz     | Variance
# mad           | Mean Absolute Deviation
# downside_var  | Lower semi-variance
# downside_mad  | Lower semi-mean absolute deviation 
# cvar          | Conditional Value at Risk (CVaR) / Expected Shortfall
# minimax_young | Minimax portfolio
# sharpe        | Sharpe ratio
# omega         | Omega measure

m <- model(); m$variable(portfolio, lb=0)

m$minimize   ( cvar(portfolio, 0.05) ) # cvar(portfolio, 0.05) | mad(portfolio)
m$subject_to ( budget_norm(portfolio) )

opt <- optimize(m, solver="glpk", data=list(returns = as.matrix(scenario.set))) 
x <- round(as.numeric(opt$solution[grep("portfolio", names(opt$solution))]), 2)

portfolio.pie(x, scenario.set)

# 7.2.4. Combination Example 1, using building blocks in the constraints

# reward       | Target return
# budget_norm  | Budget normalization (Sum of x=1)
# budget       | Budget constraint
# turnover     | Maximum turnover
# cardinality  | Cardinality
# markowitz    | Variance
# cvar         | Conditional Value at Risk (CVaR) / Expected Shortfall

m <- model(); m$variable(portfolio, lb=0)

m$maximize   ( reward(portfolio) )
m$subject_to ( cvar(portfolio, 0.95) <= 0.02 )
m$subject_to ( cvar(portfolio, 0.99) <= 0.03 ) # 0.03 | 0.04

opt <- optimize(m, solver="glpk", data=list(returns = as.matrix(scenario.set)))  
x <- round(opt$solution[grep("portfolio", names(opt$solution))], 3)

portfolio.pie(x, scenario.set)

# 7.2.5. Combination Example 2

m <- model(); m$variable(portfolio, lb=0)

m$maximize   ( omega(portfolio) )
m$subject_to ( cardinality(portfolio) <= 7 )
m$subject_to ( cvar(portfolio, 0.95) <= 0.02 ) # 0.02 | 0.01

opt <- optimize(m, solver="glpk", data=list(returns = as.matrix(scenario.set)))  
x <- as.numeric(round(opt$solution[grep("portfolio", names(opt$solution))]/ opt$solution[grep("z", names(opt$solution))], 3))

portfolio.pie(x, scenario.set)
