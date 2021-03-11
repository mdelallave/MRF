## Paquetes necesarios

#install.packages("readxl")
#install.packages("rugarch")

library(readxl) 
library(rugarch)


#### Importing data ####
data <- read_excel("data.xlsx");
dates <-as.Date(data$Date,format="%Y-%m-%d");

sp500 = data$`S&P 500 COMPOSITE - PRICE INDEX` # SP500 index at close
gold = data$`Gold Bullion LBM $/t oz DELAY`    # Gold - London Bullion Market


# Logarithmic returns (percentages)
r_sp <-  100*diff(log(sp500))
r_gold <- 100*diff(log(gold))
nobs <- length(r_sp)


#### GJR model ####
model.spec = ugarchspec(variance.model = list(model='gjrGARCH',
                                              garchOrder = c(1 , 1)),
                        mean.model = list(armaOrder = c(1 , 0))) 
# A GJR and AR(1) for the conditional mean. Norm distr by default

# SP500

gjr_sp = ugarchfit(spec=model.spec, data=r_sp, solver='solnp') # Estimation
cond_sigma_sp = gjr_sp@fit$sigma # Conditonal variance
cond_mu_sp = gjr_sp@fit$coef[1]  # Conditonal mean

# Gold

gjr_gold = ugarchfit(spec=model.spec, data=r_gold, solver='solnp') # Estimation
cond_sigma_gold = gjr_gold@fit$sigma # Conditonal variance
cond_mu_gold = gjr_gold@fit$coef[1]  # Conditonal mean


#### DCC Models ####

# Standarized returns
Z_sp <- gjr_sp@fit$z
Z_gold <- gjr_gold@fit$z
cross <- Z_sp*Z_gold

# DCC-GARCH

q11_1 <- matrix(0,nobs)
q12_1 <- matrix(0,nobs)
q22_1 <- matrix(0,nobs)

Q1 <- function(param){
  alpha <- param[1]
  beta <- param[2]
  q11_1[1] = 1
  q12_1[1] = mean(cross)
  q22_1[1] = 1
  for (i in 2:nobs){
    q11_1[i] <- q11_1[1]*(1-alpha-beta)+alpha*Z_sp[i-1]^2+beta*q11_1[i-1]
    q12_1[i] <- q12_1[1]*(1-alpha-beta)+alpha*cross[i-1]+beta*q12_1[i-1]
    q22_1[i] <- q22_1[1]*(1-alpha-beta)+alpha*Z_gold[i-1]^2+beta*q22_1[i-1]
  }
  ccorr1 <- q12_1/sqrt(q11_1*q22_1)
  lnL_1 <- -0.5*log(1-ccorr1^2)-0.5*(Z_sp^2+Z_gold^2-2*ccorr1*cross)/(1-ccorr1^2)
  Q1 <- -sum(lnL_1)
}

optim1 <- nlminb(c(0.1,0.7), Q1)
print("Estimación de los parametros alfa y beta")
optim1$par
print("valor función objetivo")
optim1$objective

q11_1[1] = 1
q12_1[1] = mean(cross)
q22_1[1] = 1
alpha <- optim1$par[1]
beta <- optim1$par[2]
for (i in 2:nobs){
  q11_1[i] <- q11_1[1]*(1-alpha-beta)+alpha*Z_sp[i-1]^2+beta*q11_1[i-1]
  q12_1[i] <- q12_1[1]*(1-alpha-beta)+alpha*cross[i-1]+beta*q12_1[i-1]
  q22_1[i] <- q22_1[1]*(1-alpha-beta)+alpha*Z_gold[i-1]^2+beta*q22_1[i-1]
}
ccorr1 <- q12_1/sqrt(q11_1*q22_1)


# DCC-EWMA

q11_2 <- matrix(0,nobs)
q12_2 <- matrix(0,nobs)
q22_2 <- matrix(0,nobs)

Q2 <- function(param){
  lambda <- param[1]
  q11_2[1] = 1
  q12_2[1] = mean(cross)
  q22_2[1]=  1
  for (i in 2:nobs){
    q11_2[i] <- (1-lambda)*Z_sp[i-1]^2+lambda*q11_2[i-1]
    q12_2[i] <- (1-lambda)*cross[i-1]+lambda*q12_2[i-1]
    q22_2[i] <- (1-lambda)*Z_gold[i-1]^2+lambda*q22_2[i-1]
  }
  ccorr2 <- q12_2/sqrt(q11_2*q22_2)
  lnL_2 <- -0.5*log(1-ccorr2^2)-0.5*(Z_sp^2+Z_gold^2-2*ccorr2*cross)/(1-ccorr2^2)
  penalty <- (1+(max(lambda,1.0)-1))^50
  Q2 <- -sum(lnL_2)*penalty
}

optim2 <- optim(c(0.9), Q2)
print("Estimación del parametro lambda")
optim2$par

q11_2[1] = 1
q12_2[1] = mean(cross)
q22_2[1] = 1
lambda <- optim2$par
for (i in 2:nobs){
  q11_2[i] <- (1-lambda)*Z_sp[i-1]^2+lambda*q11_2[i-1]
  q12_2[i] <- (1-lambda)*cross[i-1]+lambda*q12_2[i-1]
  q22_2[i] <- (1-lambda)*Z_gold[i-1]^2+lambda*q22_2[i-1]
}
ccorr2 <- q12_2/sqrt(q11_2*q22_2)

# Dinamic Correlation Plot

plot(ccorr1 ~ dates[-1], type='l',col = "blue", 
     main = "Dinamic Conditional Correlation",
     xlab="Year", ylab="Conditional Correlation",) # GARCH
lines(ccorr2 ~ dates[-1], col = "red") # EWMA
legend(dates[3800], 0.3,legend= c("CGARCH", "EWMA"), col=c("blue","red"), lty=1)

#### VaR ####

r_portfolio <- 0.5*r_sp + 0.5*r_gold
mu_portfolio <- 0.5*cond_mu_sp + 0.5*cond_mu_gold
taus <- c(.005, .01, .025)

# GARCH
sd_portfolio_garch <- sqrt(0.5^2*cond_sigma_sp^2 + 0.5^2*cond_sigma_gold^2 + 
                             2*0.5*0.5*ccorr1*cond_sigma_sp*cond_sigma_gold)
VaR_GARCH <- matrix(0,nrow=nobs,ncol=length(taus))
b=1
for(a in taus){
  VaR_GARCH[,b] <- mu_portfolio + qnorm(a)*sd_portfolio_garch  
  b=b+1}

# EMWA
sd_portfolio_EWMA <- sqrt(0.5^2*cond_sigma_sp^2 + 0.5^2*cond_sigma_gold^2 + 
                            2*0.5*0.5*ccorr2*cond_sigma_sp*cond_sigma_gold)
VaR_EWMA <- matrix(0,nrow=nobs,ncol=length(taus))
b=1
for(a in taus){
  VaR_EWMA[,b] <- mu_portfolio + qnorm(a)*sd_portfolio_EWMA  
  b=b+1}


# Plotting
par(mfrow=c(2,1),cex.lab=1, cex.axis=1, cex.main=1, mar = c(2, 4, 4, 4))
plot(r_portfolio ~ dates[-1],main="VaR - GARCH",
     xlab="", ylab="",type="l",col=1,lwd=1,
     ylim = c(min(VaR_GARCH),max(r_portfolio)))
for (i in 1:length(taus)){
  lines(VaR_GARCH[,i] ~ dates[-1],col=i+1,lwd=1)
}
legend("bottomleft",c(expression("VaR"[0.5~'%']), expression("VaR"[1~'%']),
                      expression("VaR"[2.5~'%'])), 
       ncol=1,merge=TRUE, col=2:(length(taus)+1),lty=1, cex=0.8,
       horiz=TRUE, bty = "n")

plot(r_portfolio ~ dates[-1],main="VaR - EWMA",
     xlab="",ylab="",type="l",col=1,lwd=1,
     ylim = c(min(VaR_GARCH),max(r_portfolio)))
par(new=T)
for (i in 1:length(taus)){
  lines(VaR_EWMA[,i] ~ dates[-1], col=i+1,lwd=1)
}
legend("bottomleft",c(expression("VaR"[0.5~'%']), expression("VaR"[1~'%']),
                      expression("VaR"[2.5~'%'])), 
       ncol=1,merge=TRUE, col=2:(length(taus)+1),lty=1, cex=0.8, 
       horiz=TRUE,bty = "n")

#### Kratz et al. Tests ####

# Pearson - GARCH

O_GARCH <- rep(0, length(taus)+1)
O_GARCH[1] <- sum(r_portfolio > VaR_GARCH[,3]) 
O_GARCH[2] <- sum(r_portfolio > VaR_GARCH[,2]) - O_GARCH[1] 
O_GARCH[3] <- sum(r_portfolio > VaR_GARCH[,1]) - O_GARCH[2] - O_GARCH[1] 
O_GARCH[4] <- sum(r_portfolio < VaR_GARCH[,1]) 

alpha <- rev(c(1 , (1-taus), 0))
Pearson_GARCH <- 0
for (i in length(taus)+1) {
  Sn = (O_GARCH[i] - nobs*(alpha[i+1] - alpha[i]))^2/(nobs*(alpha[i+1] - alpha[i]))
  Pearson_GARCH = Pearson_GARCH + Sn
}

if(Pearson_GARCH > qchisq(.95, df=length(O_GARCH))){
  print("Rechazamos H0 para Pearson - GARCH")
} else {
  print("No rechazamos H0 para Pearson - GARCH")
}


# Pearson - EWMA
O_EWMA <- rep(0, length(taus)+1)
O_EWMA[1] <- sum(r_portfolio > VaR_EWMA[,3]) 
O_EWMA[2] <- sum(r_portfolio > VaR_EWMA[,2]) - O_EWMA[1] 
O_EWMA[3] <- sum(r_portfolio > VaR_EWMA[,1]) - O_EWMA[2] - O_EWMA[1] 
O_EWMA[4] <- sum(r_portfolio < VaR_EWMA[,1]) 

Pearson_EWMA<- 0
for (i in length(taus)+1) {
  Sn = (O_EWMA[i] - nobs*(alpha[i+1] - alpha[i]))^2/(nobs*(alpha[i+1] - alpha[i]))
  Pearson_EWMA = Pearson_EWMA + Sn
}

if(Pearson_EWMA > qchisq(.95, df=length(O_EWMA))){
  print("Rechazamos H0 para Pearson - EWMA")
} else {
  print("No rechazamos H0 para Pearson - EWMA")
}

# Nass

N <- length(O_GARCH)

aux <- 0
for (i in N) {
  aux = aux + 1/(alpha[i+1] - alpha[i])
}
c <- 2*N / (2*N - (N^2 + 4*N + 1 + aux)/nobs) 
df <- c*N


Nass_GARCH <- c*Pearson_GARCH
Nass_EWMA <- c*Pearson_EWMA

if(Nass_GARCH > qchisq(.95, df=df)){
  print("Rechazamos H0 para Nass - GARCH")
} else {
  print("No rechazamos H0 para Nass - GARCH")
}

if(Nass_EWMA > qchisq(.95, df=df)){
  print("Rechazamos H0 para Nass - EWMA")
} else {
  print("No rechazamos H0 para Nass - EWMA")
}

# LR - GARCH

LR_GARCH <- 0
for (i in length(O_GARCH)){
  if (O_GARCH[i] == 0){
    next
  } 
  else{
    Sn_tilde = 2*O_GARCH[i]*log(O_GARCH[i]/(nobs*(alpha[i+1]-alpha[i])))
    LR_GARCH = LR_GARCH + Sn_tilde
  }
}

if(LR_GARCH > qchisq(.95, df=N)){
  print("Rechazamos H0 para LR - GARCH")
} else {
  print("No rechazamos H0 para LR - GARCH")
}

# LR - EWMA

LR_EWMA <- 0
for (i in length(O_EWMA)){
  if (O_EWMA[i] == 0){
    next
  } 
  else{
  Sn_tilde = 2*O_EWMA[i]*log(O_EWMA[i]/(nobs*(alpha[i+1]-alpha[i])))
  LR_EWMA = LR_EWMA + Sn_tilde
  }
}
if(LR_EWMA > qchisq(.95, df=N)){
  print("Rechazamos H0 para LR - EWMA")
} else {
  print("No rechazamos H0 para LR - EWMA")
}