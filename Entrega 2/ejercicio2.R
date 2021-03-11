## Paquetes necesarios

#install.packages("readxl")
#install.packages("rugarch")

library(readxl) 
library(rugarch)


#### Importing data ####
data <- read_excel("datastoxx.xlsx");
dates <-as.Date(data$Date,format="%Y-%m-%d");

set.seed(1585) # For replication (should not differ a lot)
ES50 = data$`Stoxx 50` # EURO STOXX 50 index at close
r_ES50 <-  100*diff(log(ES50)) # Logarithmic returns (percentages)
T <- length(r_ES50)
h <- 10   # Days ahead
W <- 1000 # Window length
N <- 5000 # Simulations
tau <- 0.025 # Significance level

## GJR model specifications
model.spec = ugarchspec(variance.model = list(model = 'gjrGARCH' , 
                                              garchOrder = c(1 , 1)), 
                        mean.model=list(armaOrder=c(0,0), include.mean=FALSE), 
                        distribution.model = "std") 
# A GJR and 0 for the conditional mean. t-Student distribution


#### Initializing loop ####
boot_resid_est <- matrix(0, nrow=N,ncol=h)
r_sim <- matrix(0,nrow=N,ncol = h+1)
sigma_sim <- matrix(0,nrow=N,ncol=h+1)
resid_sim <- matrix(0,nrow=N,ncol=h+1)
r_h <- matrix(0,nrow=N,ncol=(T-W)) # Simulated returns h days ahead
VaR_h <- rep(0, (T-W))
ES_h <- rep(0, (T-W))

for (k in 1:(T-W)) {
  # GJR estimation
  model <- ugarchfit(spec=model.spec,data=r_ES50[k:(W+k-1)], solver = 'solnp')
  cond_sigma <-  model@fit$sigma # Conditonal sd
  resid <- model@fit$residuals # Residuals
  resid_est <- model@fit$z # Standardized innovations
  omega <- model@fit$coef[1] # GARCH(1,1) parameters
  alpha <- model@fit$coef[2]
  beta <- model@fit$coef[3]
  gamma <- model@fit$coef[4]
  
  # Bootstrapping
  uni <- matrix(floor(W*runif(N*h) + 1),N,h)
  for (j in 1:h) {
    for (i in 1:N) {
      boot_resid_est[i,j] <- resid_est[uni[i,j]]
    }
  }
  
  ## Simulation
  # Initial values
  r_sim[,1] <- r_ES50[W+k-1]
  sigma_sim[,1] <- cond_sigma[W]
  resid_sim[,1] <- resid[W]
  # Loop
  for (i in 1:N) {
    for (j in 1:h) {
        if (resid_sim[i,j] <= 0){
          sigma_sim[i,j+1] = sqrt(omega + (alpha + gamma)*resid_sim[i,j]^2 + 
                                    beta*sigma_sim[i,j]^2)
        } else {
          sigma_sim[i,j+1] = sqrt(omega + alpha*resid_sim[i,j]^2 + 
                                    beta*sigma_sim[i,j]^2)
        }
        resid_sim[i,j+1] <- sigma_sim[i,j]*boot_resid_est[i,j]
      }
  }
  r_sim <- resid_sim[,-1]
  r_h[,k] <- rowSums(r_sim)
  
  ## VaR and ES
  VaR_h[k] <- quantile(r_h[,k], probs=tau)
  ES_h[k] <- sum(r_h[r_h[,k]<VaR_h[k],k])/(N*tau)
}

## Returns in h days
r_ES50_h <- rep(0,length(VaR_h))
for (i in 1:(length(VaR_h)-9)) {
  r_ES50_h[i] <- sum(r_ES50[(W-1+i):(W+h+i)])
}


#### Plotting ####

plot(r_ES50_h ~ dates[(W+2):length(dates)], 
     xlab="", ylab="",type='l',col = "black", 
     ylim = c(min(ES_h),max(r_ES50_h)),
     main = "Rentabilidad y VaR/ES a 10 días")
lines(VaR_h ~ dates[(W+2):length(dates)], col = "blue")
lines(ES_h ~ dates[(W+2):length(dates)], col = "red")
legend("bottomleft",c(expression("Euro Stoxx 50"),expression("VaR"[2.5~'%']),
                      expression("ES"[2.5~'%'])), ncol=1,merge=TRUE, 
       col=c("black","blue","red"),lty=1, cex=0.8, bty = "n")

#### Backtesting ####
# Unconditional Coverage Test (Kupiec) for VaR
T1 <- sum(r_ES50_h < VaR_h) # Number of violations
T0 <- length(r_ES50_h) - T1 
FR <- T1/length(r_ES50_h) # Failure rate

Kupiec <- -2*log(((1-tau)^T0*tau^T1)/((1-FR)^T0*FR^T1))
crit_val_kupiec <- qchisq(1 - tau, df=1)

if (Kupiec > crit_val_kupiec){
print("Rechazamos hipótesis nula en el test de Kupiec")      # tau != FR
} else {
  print("No rechazamos hipótesis nula en el test de Kupiec") # tau = FR
}

#### Acerbi & Szekely Test for ES (2 statistics) ####


sum <- sum(ifelse(r_ES50_h<VaR_h,1,0)*r_ES50_h/ES_h)
Z1 <- sum/T1 - 1
Z2 <- 1 - (1-Z1)*T1/(length(r_ES50_h)*tau)

 
sum_sim <- rep(0,N)
Z1_sim <- sum_sim
Z2_sim <- sum_sim
for (i in 1:N) {
  sum_sim[i] <- sum(ifelse(r_h[i,]<VaR_h,1,0)*r_h[i,]/ES_h)
  Z1_sim[i] <- sum_sim[i]/T1 - 1
  Z2_sim[i] <- 1 - (1-Z1_sim[i])*T1/(length(r_h[1,])*tau)
}

pvalue_Z1 <- sum(Z1_sim< Z1)/N
pvalue_Z2 <- sum(Z2_sim< Z2)/N

if (pvalue_Z1 < tau){
  print("Rechazamos hipótesis nula para Z1")     # ES_sim = ES
} else {
  print("No rechazamos hipótesis nula para Z1")  # ES_sim \leq  ES
}

if (pvalue_Z2 < tau){
  print("Rechazamos hipótesis nula para Z2")     # ES_sim = ES
} else {
  print("No rechazamos hipótesis nula para Z2")  # ES_sim \leq  ES
}