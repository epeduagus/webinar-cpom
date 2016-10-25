#############################################################################
###
###  Webinar - Contemporary Portfolio Optimization Modeling with R
###  -------------------------------------------------------------
###  Ronald Hochreiter - http://www.finance-r.com/cpom/
###
#############################################################################

### Packages from CRAN

package_list <- c("tseries", "quantmod", "PerformanceAnalytics", "PortfolioAnalytics", "fPortfolio", 
                  "magrittr", "FRAPO",
                  "slam", "registry", "ROI", "Rglpk", "ROI.plugin.glpk", "ROI.plugin.quadprog", "R6")

for(x in package_list) if(!(x %in% rownames(installed.packages()))) { install.packages(x) }

### Packages from R/Forge

if(!("ROML" %in% rownames(installed.packages()))) { install.packages("ROML", repos="http://R-Forge.R-project.org") }
if(!("ROML.portfolio" %in% rownames(installed.packages()))) { install.packages("ROML.portfolio", repos="http://R-Forge.R-project.org") }

### Packages from GitHub

if(!("modopt.matlab" %in% rownames(installed.packages()))) { install_github("rhochreiter/modopt.matlab") }
if(!("scenportopt" %in% rownames(installed.packages()))) { install_github("rhochreiter/scenportopt") }
