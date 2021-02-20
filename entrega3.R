#### Packages ####
# install.packages("readxl")
# install.packages("matrixStats")
# install.packages("xtable")
# install.packages("tidyverse")
# install.packages("fastDummies")

library(readxl) 
library(matrixStats)
library(xtable)
library(tidyverse)
library(fastDummies)

#### Data ####
data_raw <- read_excel("eurostox50.xlsx")  
dates <- as.Date(data_raw$Fecha,format="%Y-%m-%d")

T <- nrow(data_raw)-250  # Matrix length (in sample), excl. one year (250 days)

# In sample
prices <- data_raw[1:T, c(-1)] # Excluding dates
returns <- colDiffs(as.matrix(log(prices)))*100 # Returns (%)
N <- ncol(prices) # Number of companies

# Out of sample
oos_prices <- data_raw[(T+1):nrow(data_raw), c(-1)]
oos_returns <- colDiffs(as.matrix(log(oos_prices)))*100 # Returns (%)
colnames(oos_returns) <- colnames(oos_prices) # Names

#### Description data ####
description <- read_excel("eurostoxx_description.xlsx") # (Alphabetical order)
colnames(returns) <- description$NAME # Names

description$`SECTOR NAME` <- description$`ICB INDUSTRY NAME`

# Regrouping by sector
# Consumer:
description$`SECTOR NAME` <- description$`SECTOR NAME` %>% 
  str_replace("Consumer Goods", "Consumer")
description$`SECTOR NAME` <- description$`SECTOR NAME` %>% 
  str_replace("Consumer Services", "Consumer")

# BM and Industrials:
description$`SECTOR NAME` <- description$`SECTOR NAME` %>% 
  str_replace("Industrials", "BM and Industrials")
description$`SECTOR NAME` <- description$`SECTOR NAME` %>% 
  str_replace("Basic Materials", "BM and Industrials")


# Oil, gas and utilities:
description$`SECTOR NAME` <- description$`SECTOR NAME` %>% 
  str_replace("Utilities", "Oil, gas and utilities")
description$`SECTOR NAME` <- description$`SECTOR NAME` %>% 
  str_replace("Oil & Gas", "Oil, gas and utilities")

# Tech and teleco
description$`SECTOR NAME` <- description$`SECTOR NAME` %>% 
  str_replace("Telecommunications", "Tech and teleco")
description$`SECTOR NAME` <- description$`SECTOR NAME` %>% 
  str_replace("Technology", "Tech and teleco")

# Financials and Health Care remain the same

dummy_sector <- dummy_cols(description$`SECTOR NAME`)
dummy_industry <- dummy_cols(description$`ICB INDUSTRY NAME`)
dummy_core <- cbind(description$CORE, abs(description$CORE - 1)) 
# 1 for EU core (all but Spain, Italy & Finland)



#### Excercise 1 ####


#### Section a) ####

# PCA
pca_returns <- princomp(returns)
pca_scores <- pca_returns$scores
summary(pca_returns)
eigenvectors <- loadings(pca_returns)
pca_main <- eigenvectors[,1:6] # +70% variance explained components
 

plot(pca_returns, main = "PCA")

cumpro <- cumsum(pca_returns$sdev^2 / sum(pca_returns$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", 
     main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.70042015, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)

k <- 3 # First 3 components
pca_k <- eigenvectors[,1:k] 

# Analysis 
PC1_industrials <- dummy_sector$`.data_BM and Industrials` %*%  pca_k[,1]
PC1_consumer <- dummy_sector$.data_Consumer %*%  pca_k[,1]
PC1_financials <- dummy_sector$.data_Financials %*%  pca_k[,1]
PC1_health <- dummy_sector$`.data_Health Care` %*%  pca_k[,1]
PC1_oil <- dummy_sector$`.data_Oil, gas and utilities` %*%  pca_k[,1]
PC1_tech <- dummy_sector$`.data_Tech and teleco` %*%  pca_k[,1]


PC2_industrials <- dummy_sector$`.data_BM and Industrials` %*%  pca_k[,2]
PC2_consumer <- dummy_sector$.data_Consumer %*%  pca_k[,2]
PC2_financials <- dummy_sector$.data_Financials %*%  pca_k[,2]
PC2_health <- dummy_sector$`.data_Health Care` %*%  pca_k[,2]
PC2_oil <- dummy_sector$`.data_Oil, gas and utilities` %*%  pca_k[,2]
PC2_tech <- dummy_sector$`.data_Tech and teleco` %*%  pca_k[,2]

PC3_industrials <- dummy_sector$`.data_BM and Industrials` %*%  pca_k[,3]
PC3_consumer <- dummy_sector$.data_Consumer %*%  pca_k[,3]
PC3_financials <- dummy_sector$.data_Financials %*%  pca_k[,3]
PC3_health <- dummy_sector$`.data_Health Care` %*%  pca_k[,3]
PC3_oil <- dummy_sector$`.data_Oil, gas and utilities` %*%  pca_k[,3]
PC3_tech <- dummy_sector$`.data_Tech and teleco` %*%  pca_k[,3]

PC_sector <- rbind(c(PC1_industrials, PC2_industrials, PC3_industrials),
                   c(PC1_consumer, PC2_consumer, PC3_consumer),
                   c(PC1_financials, PC2_financials, PC3_financials),
                   c(PC1_health, PC2_health, PC3_health),
                   c(PC1_oil, PC2_oil, PC3_oil),
                   c(PC1_tech, PC2_tech, PC3_tech))
colnames(PC_sector) <- c("PC1", "PC2", "PC3")
rownames(PC_sector) <- c("BM and Industrial", "Consumer", "Financials",
                         "Health Care", "Oil, gas and utilities",
                         "Tech and teleco")
# Results in LaTeX:
#print(xtable(PC_sector, type = "latex"), file = "PC_sector.tex")
print(PC_sector)

PC1_core <- dummy_core[,1] %*%  pca_k[,1]
PC1_nocore <- dummy_core[,2] %*%  pca_k[,1]

PC2_core <- dummy_core[,1] %*%  pca_k[,2]
PC2_nocore <- dummy_core[,2] %*%  pca_k[,2]

PC3_core <- dummy_core[,1] %*%  pca_k[,3]
PC3_nocore <- dummy_core[,2] %*%  pca_k[,3]

PC_country <-  cbind(c(PC1_core, PC1_nocore),  c(PC2_core, PC2_nocore),
                     c(PC3_core, PC3_nocore))
colnames(PC_country) <- c("PC1", "PC2", "PC3")
rownames(PC_country) <- c("Core countries", "No core countries")
# Results in LaTeX:
#print(xtable(PC_country, type = "latex"), file = "PC_country.tex")
print(PC_country)

returns_corr <- cor(returns)


#### Section b) ####

# Weights matrix
PC_w <- matrix(0, nrow = nrow(pca_main), ncol = ncol(pca_main))
beta_PC <- rep(0, ncol(pca_main))
for (i in 1:ncol(pca_main)) {
  PC_w[,i] <- pca_main[,i] / sum(pca_main[,i])
  beta_PC[i] <- description$BETA %*% PC_w[,i]
}

# PC portfolios analysis
PC_portfolio <- oos_returns %*% PC_w 

mean_PC <- colMeans(PC_portfolio)
sd_PC <- colSds(PC_portfolio)


#### Section c) ####

# Sector portfolio
sector_portfolio <- matrix(0, nrow = nrow(oos_returns), 
                           ncol = (ncol(dummy_sector) - 1))

beta_sector <- rep(0, (ncol(dummy_sector) - 1))
for (i in 1:(ncol(dummy_sector) - 1)) {
  sector_portfolio[,i] <- oos_returns %*% dummy_sector[,1+i] 
  beta_sector[i] <- (description$BETA %*% dummy_sector[,1+i]) / 
  colSums(dummy_sector[1+i])
}

mean_sector <- colMeans(sector_portfolio)
sd_sector <- colSds(sector_portfolio)


# Equally weighted portfolio
eqw_port <- rep(0, nrow = nrow(oos_returns), ncol = N)
for (i in 1:nrow(oos_returns)) {
  eqw_port[i] <- sum(oos_returns[i,])/N
}

mean_eqw <- mean(eqw_port)
sd_eqw <- sd(eqw_port)
beta_eqw <- mean(description$BETA)

# Sharpe ratio

r = 0 # Risk-free rate

sharpe_pc <- rep(0, ncol(pca_main))
for (i in 1:length(sharpe_pc)) {
  sharpe_pc[i] <- (mean_PC[i] - r) / sd_PC[i]
}

sharpe_sector <- rep(0, (ncol(dummy_sector) - 1))
for (i in 1:length(sharpe_sector)) {
  sharpe_sector[i] <- (mean_sector[i] - r) / sd_sector[i]
}

sharpe_eqw <- (mean_eqw - r) / sd_eqw


# Treynor ratio

treynor_pc <- rep(0, k)
for (i in 1:length(treynor_pc)) {
  treynor_pc[i] <- (mean_PC[i] - r) / beta_PC[i]
}

treynor_sector <- rep(0, (ncol(dummy_sector) - 1))
for (i in 1:length(sharpe_sector)) {
  treynor_sector[i] <- (mean_sector[i] - r) / beta_sector[i]
}

treynor_eqw <- (mean_eqw - r) / beta_eqw


## Results

results_1c <- rbind(c(mean_PC, mean_sector, mean_eqw),
                   c(sd_PC, sd_sector, sd_eqw),
                   c(sharpe_pc, sharpe_sector, sharpe_eqw),
                   c(treynor_pc, treynor_sector, treynor_eqw))
rownames(results_1c) <- c("Mean returns", "Sdev", "Sharpe", "Treynor")
colnames(results_1c) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6",
                          "BM and Industrial", "Consumer", "Financials", 
                          "Health Care", "Oil, gas and utilities",
                         "Tech and teleco", "Equally weighted")
# Results in LaTeX:
#print(xtable(results_1c[,1:6], type = "latex"), file = "results_1c_1.tex")
#print(xtable(results_1c[,7:13], type = "latex"), file = "results_1c_2.tex")
print(results_1c)


#### Excercise 2 ####


# Section a) #
k <- 4 # +65% variance explained components (Comp. 4: 66.269812%)
pca_f65 <- pca_scores[,1:k] 


#### Section b) ####
# Estimation

X <-  cbind(rep(1,T), pca_f65) # X
XX <-  crossprod(X) # X'X

G <-  solve(XX) %*% crossprod(X, returns) # (X'X)^{-1} X'R. Parameters
B <-  G[2:(k+1),] # Betas
E <-  returns - X %*% G # Errors = R - X\Beta

# Error variance
diagD <-  diag(crossprod(E)/(T-(k+1))) # \sigma^2 = e'e/(T-(k+1))

# R2
sumSquares = apply(returns, 2, function(x) {sum( (x - mean(x))^2 )})
R_square = 1 - (T-(k+1))*diagD/sumSquares

# Variance decomposition
var_f <- var(pca_f65) # Factors (\Omega)
var_sys <-  t(B) %*% var_f %*% B # Systematic (B'\Omega B)
var_idio <- diag(diagD) # Idiosyncratic (\Psi)
cov <-  var_sys + var_idio # Var-Cov matrix
cor <-  cov2cor(cov) # Correlation matrix

# Weights matrix
industry_w <- matrix(0, nrow = N, ncol = (ncol(dummy_industry) - 1)) 
for (i in 1:ncol(industry_w)) {
  industry_w[,i] <- dummy_industry[,1+i]/colSums(dummy_industry[1+i]) 
}

country_w <- matrix(0, nrow = N, ncol = ncol(dummy_core)) 
for (i in 1:ncol(dummy_core)) {
  country_w[,i] <- dummy_core[,i]/sum(dummy_core[,i]) 
}

# First 3 sectors: BM and Industrials, Consumer and Financials
W <- cbind(country_w, industry_w[,c(2, 4, 6)], rep(1/N, N)) 
colnames(W) <- c("Core", "Non core", "Consumer Goods", "Financials", 
                 "Industrials", "Equally weighted")
rownames(W) <- description$NAME


B_all <-  G[2:(k+1),] %*% W
rownames(B_all) <- c("Beta 1", "Beta 2", "Beta 3", "Beta 4")
var_sys_all <-  t(diag(t(B_all) %*% var_f %*% B_all))
rownames(var_sys_all) <- "Sys var"
var_idio_all <- t(diag(t(W ) %*% diag(diagD) %*% W))
rownames(var_idio_all) <- "Idio var"
total_var <- var_sys_all + var_idio_all
rownames(total_var) <- "Total var"

var_sys_all_perc <- (var_sys_all/total_var)*100
var_idio_all_perc <- (var_idio_all/total_var)*100

results_2 <- rbind(B_all, total_var, var_sys_all_perc, var_idio_all_perc)
# Results in LaTeX:
#print(xtable(results_2, type = "latex"), file = "results_2.tex")
print(results_2)


risk_sys_all <- sqrt(var_sys_all/T)
rownames(risk_sys_all) <- "Systematic risk"
risk_idio_all <- sqrt(var_idio_all/T)
rownames(risk_idio_all) <- "Idiosyncratic risk"
total_risk <- risk_sys_all + risk_idio_all
rownames(total_risk) <- "Total risk"

results_2risk <- rbind(total_risk, risk_sys_all, risk_idio_all)
# Results in LaTeX:
#print(xtable(results_2risk, type = "latex"), file = "results_2risk.tex")
print(results_2risk)

#### Section c) ####
I <- 1000 # Investment in each portfolio
W2 <- W * I 

B_all2 <-  G[2:(k+1),] %*% W2
rownames(B_all2) <- c("Beta 1", "Beta 2", "Beta 3", "Beta 4")
var_sys_all2 <-  t(diag(t(B_all2) %*% var_f %*% B_all2))
rownames(var_sys_all2) <- "Sys var"
var_idio_all2 <- t(diag(t(W2 ) %*% diag(diagD) %*% W2))
rownames(var_idio_all2) <- "Idio var"
total_var2 <- var_sys_all2 + var_idio_all2
rownames(total_var2) <- "Total var"

var_sys_all2_perc <- (var_sys_all2/total_var2)*100
var_idio_al2l_perc <- (var_idio_all2/total_var2)*100

results_2c <- rbind(B_all2, total_var2, var_sys_all2_perc, var_idio_al2l_perc)
# Results in LaTeX:
#print(xtable(results_2c, type = "latex"), file = "results_2c.tex")
print(results_2c)

risk_sys_all2 <- sqrt(var_sys_all2/T)
rownames(risk_sys_all2) <- "Systematic risk"
risk_idio_all2 <- sqrt(var_idio_all2/T)
rownames(risk_idio_all2) <- "Idiosyncratic risk"
total_risk2 <- risk_sys_all2 + risk_idio_all2
rownames(total_risk2) <- "Total risk"

results_2crisk <- rbind(total_risk2, risk_sys_all2, risk_idio_all2)
# Results in LaTeX:
#print(xtable(results_2crisk, type = "latex"), file = "results_2crisk.tex")
print(results_2crisk)

#### Excercise 3 ####
# Section a)
# BARRA Model

# Dummies for firm size, dividend rates and betas
dummy_size <- as.numeric(median(description$`MARKET VAL BY CO.`) < 
                          description$`MARKET VAL BY CO.`) # Greater firms
dummy_div <- as.numeric(median(description$`DIV RATE UNADJUST`) < 
                      description$`DIV RATE UNADJUST`) # Greater dividends
dummy_beta <- as.numeric(description$BETA > 1) # Greater betas (More agressive)

B_barra <- as.matrix(cbind(dummy_industry[, -1], dummy_core[,-2], dummy_size,
                           dummy_div, dummy_beta))

                     
nfactors <- length(B_barra[1,])
BB_barra <- crossprod(B_barra)

# Parameters
G_barra <- solve(BB_barra) %*% crossprod(B_barra, t(returns))
factors_barra <- G_barra
E_barra <- t(returns) - B_barra %*% factors_barra

# Error variance
diagD_barra <-  diag(crossprod(t(E_barra))/(T-(nfactors+1))) 
# \sigma^2 = ee'/(T-(k+1))
# diagD_barra[21] <- mean(diagD_barra) # To fix multicollinearity. Not used

# R2
sumSquares_barra = apply(returns, 2, function(x) {sum( (x - mean(x))^2 )})
R_square_barra = 1 - (T-(nfactors+1))*diagD_barra/sumSquares_barra

# Variance decomposition
var_f_barra <- var(t(factors_barra)) # Var-Cov matrix
var_sys_barra <-  B_barra %*% var_f_barra %*% t(B_barra) # Systematic(B\Omega B')
var_idio_barra <- diag(diagD_barra) # Idiosyncratic (\Psi)
cov_barra <- var_sys_barra + var_idio_barra # Covariance matrix
cor_barra <-  cov2cor(var_f_barra) # Correlation matrix


# Cross-section model results
B_barra_all <-  t(B_barra) %*% W
rownames(B_barra_all) <- c("BM", "Goods", "Services", "Financials", "Health",
                           "Industrials", "Oil", "Tech", "Teleco", "Utilities",
                           "Core", "Size", "Dividend", "Beta") 
var_sys_all_barra <-  t(diag(t(B_barra_all) %*% var_f_barra %*% B_barra_all))
rownames(var_sys_all_barra) <- "Sys var"
var_idio_all_barra <- t(diag(t(W) %*% diag(diagD_barra) %*% W))
rownames(var_idio_all_barra) <- "Idio var"
total_var_barra <- var_sys_all_barra + var_idio_all_barra
rownames(total_var_barra) <- "Total var"

var_sys_all_barra_perc <- (var_sys_all_barra/total_var_barra)*100
var_idio_all_barra_perc <- (var_idio_all_barra/total_var_barra)*100


results_3a <- rbind(total_var_barra, var_sys_all_barra_perc, 
                    var_idio_all_barra_perc, B_barra_all)

# Results in LaTeX:
#print(xtable(results_3a, type = "latex"), file = "results_3a.tex")
print(results_3a)


risk_sys_all_barra <- sqrt(var_sys_all_barra/T)
rownames(risk_sys_all_barra) <- "Systematic risk"
risk_idio_all_barra <- sqrt(var_idio_all_barra/T)
rownames(risk_idio_all_barra) <- "Idiosyncratic risk"
total_risk_barra <- risk_sys_all_barra + risk_idio_all_barra
rownames(total_risk_barra) <- "Total risk"

results_3arisk <- rbind(total_risk_barra, risk_sys_all_barra, risk_idio_all_barra)
# Results in LaTeX:
#print(xtable(results_3arisk, type = "latex"), file = "results_3arisk.tex")
print(results_3arisk)


#### GLS DOESN'T WORK ####

# var_idio_barra_inv <- solve(var_idio_barra) # Heteroskedasticity correction
# 
# #GLS estimation
# G_gls <-  solve(t(B_barra) %*% var_idio_barra_inv %*% B_barra)  %*% t(B_barra) %*% 
#   var_idio_barra_inv %*% t(returns)
# factors_gls <-  G_gls
# E_gls <-  t(returns) - B_barra %*% G_gls
# 
# # Error variance
# diagD_gls <-  diag(crossprod(t(E_gls)/(N-(nfactors+1))))
# 
# # Var-cov matrices
# var_f_gls <- var(t(factors_gls))  # Factors
# var_sys_gls <-  t(factors_gls) %*% var_f_gls %*% (factors_gls) # Systematic 
# var_idio_gls <- diag(diagD_gls) # Idiosyncratic
# 
# cov_gls <-  var_sys_gls + diag(var_idio_gls) # Covariance
# cor_gls <-  cov2cor(cov_gls) # Correlation
# 
# 
# # GLS results
# B_barra_all <-  t(B_barra) %*% W
# rownames(B_barra_all) <- c("BM", "Goods", "Services", "Financials", "Health",
#                            "Industrials", "Oil", "Tech", "Teleco", "Utilities",
#                            "Core", "Size", "Dividend", "Beta")
# var_sys_all_gls <-  t(diag(t(B_barra_all) %*% var_f_gls %*% B_barra_all))
# rownames(var_sys_all_gls) <- "Sys var"
# var_idio_all_gls <- t(diag(t(W) %*% diag(diagD_gls) %*% W))
# rownames(var_idio_all_gls) <- "Idio var"
# total_var_gls <- var_sys_all_gls + var_idio_all_gls
# rownames(total_var_gls) <- "Total var"
# 
# var_sys_all_gls_perc <- (var_sys_all_gls/total_var_gls)*100
# var_idio_all_gls_perc <- (var_idio_all_gls/total_var_gls)*100
# 
# results_3a_gls <- rbind(total_var_gls, var_sys_all_gls_perc, 
#                         var_idio_all_gls_perc, B_barra_all)
# # Results in LaTeX:
# #print(xtable(results_3a_gls, type = "latex"), file = "results_3a_gls.tex")
# print(results_3a_gls)
# 
# 
# risk_sys_all_gls <- sqrt(var_sys_all_gls/T)
# rownames(risk_sys_all_gls) <- "Systematic risk"
# risk_idio_all_gls <- sqrt(var_idio_all_gls/T)
# rownames(risk_idio_all_gls) <- "Idiosyncratic risk"
# total_risk_gls <- risk_sys_all_gls + risk_idio_all_gls
# rownames(total_risk_gls) <- "Total risk"
# 
# results_3a_glsrisk <- rbind(total_risk_gls, risk_sys_all_gls, risk_idio_all_gls)
# # Results in LaTeX:
# #print(xtable(results_3a_glsrisk, type = "latex"), file = "results_3a_glsrisk.tex")
# print(results_3a_glsrisk)


#### Section b) ####
# Cross-section model results
B_barra_all2 <-  t(B_barra) %*% W2
rownames(B_barra_all2) <- c("BM", "Goods", "Services", "Financials", "Health",
                            "Industrials", "Oil", "Tech", "Teleco", "Utilities",
                            "Core", "Size", "Dividend", "Beta")
var_sys_all_barra2 <-  t(diag(t(B_barra_all2) %*% var_f_barra %*% B_barra_all2))
rownames(var_sys_all_barra2) <- "Sys var"
var_idio_all_barra2 <- t(diag(t(W2) %*% diag(diagD_barra) %*% W2))
rownames(var_idio_all_barra2) <- "Idio var"
total_var_barra2 <- var_sys_all_barra2 + var_idio_all_barra2
rownames(total_var_barra2) <- "Total var"

var_sys_all_barra2_perc <- (var_sys_all_barra2/total_var_barra2)*100
var_idio_all_barra2_perc <- (var_idio_all_barra2/total_var_barra2)*100

results_3b <- rbind(total_var_barra2, var_sys_all_barra2_perc, 
                    var_idio_all_barra2, B_barra_all2)
# Results in LaTeX:
print(xtable(results_3b, type = "latex"), file = "results_3b.tex")
print(results_3b)


risk_sys_all_barra2 <- sqrt(var_sys_all_barra2/T)
rownames(risk_sys_all_barra2) <- "Systematic risk"
risk_idio_all_barra2 <- sqrt(var_idio_all_barra2/T)
rownames(risk_idio_all_barra2) <- "Idiosyncratic risk"
total_risk_barra2 <- risk_sys_all_barra2 + risk_idio_all_barra2
rownames(total_risk_barra2) <- "Total risk"

results_3brisk <- rbind(total_risk_barra2, risk_sys_all_barra2, 
                        risk_idio_all_barra2)
# Results in LaTeX:
#print(xtable(results_3brisk, type = "latex"), file = "results_3brisk.tex")
print(results_3brisk)

# # GLS results
# B_barra_all2 <-  t(B_barra) %*% W2
# rownames(B_barra_all2) <- c("BM", "Goods", "Services", "Financials", "Health",
#                             "Industrials", "Oil", "Tech", "Teleco", "Utilities",
#                             "Core", "Size", "Dividend", "Beta")
# var_sys_all_gls2 <-  t(diag(t(B_barra_all2) %*% var_f_gls %*% B_barra_all2))
# rownames(var_sys_all_gls2) <- "Systematic risk"
# var_idio_all_gls2 <- t(diag(t(W2) %*% diag(diagD_gls) %*% W2))
# rownames(var_idio_all_gls2) <- "Idiosyncratic risk"
# total_var_gls2 <- var_sys_all_gls2 + var_idio_all_gls2
# rownames(total_var_gls2) <- "Total risk"
# 
# results_3b_gls <- rbind(total_var_gls2, var_sys_all_gls2, var_idio_all_gls2,
#                     B_barra_all2)
# # Results in LaTeX:
# #print(xtable(results_3b_gls, type = "latex"), file = "results_3b_gls.tex")
# print(results_3b_gls)
