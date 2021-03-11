## Paquetes necesarios

#install.packages("moments")

library(moments) 

set.seed(1585) # For replication
T <- 1.0e6 # Number of simulations
p_ini <- 0.288 # Mix parameter.
mean1 <-  0.0
mean2 <-  0.0
sd1_ini <-  2
sd2_ini <-  5
n <- 50 # Number of variances/parameters

#### Moving var ratio ####
sd1 <- rep(sd1_ini, n)
sd2 <- seq(from=sd2_ini, by=0.25, length.out=n)
sd_ratio <-  sd2/sd1
y1 <- matrix(0,nrow=length(sd1), ncol=T)
y2 <- y1
for (i in 1:length(sd1)) {
  y1[i,] <- rnorm(T, mean=mean1, sd=sd1[i]) 
  y2[i,] <- rnorm(T, mean=mean2, sd=sd2[i])
}

M1 <- matrix(0,nrow=length(sd1), ncol=T) # Mixture 
skew1 <- rep(0,length(sd1))
kurt1 <- skew1
for (i in 1:length(sd1)) {
  U <- runif(T)  # Uniform for mixing
  for (j in 1:T) {
    if (U[j] <= p_ini){
      M1[i,j] <- y1[i,j] 
    } else {
      M1[i,j] <- y2[i,j] 
    }
  }
  skew1[i] <- skewness(M1[i,])
  kurt1[i] <- kurtosis(M1[i,])
}

## Ploting
var_ratio <- sd_ratio^2
par(mfrow=c(2,1),cex.lab=1, cex.axis=1, cex.main=1, mar = c(2, 4, 4, 4))
plot(skew1 ~ var_ratio, type='l',col = "blue",
     main="Momentos en función del ratio varianza", xlab="", ylab="Skewness")
plot(kurt1 ~ var_ratio, type='l',col = "blue", 
     main = "", xlab = "", ylab = "Kurtosis")


#### Moving mix parameter ####
p <- seq(from=p_ini, to=0.49, length.out=n)
x1 <- rnorm(T, mean=mean1, sd=sd1_ini) 
x2 <- rnorm(T, mean=mean2, sd=sd2_ini)
M2 <- matrix(0,nrow=length(p), ncol=T) # Mixture 
skew2 <- rep(0,length(p))
kurt2 <- skew2
for (i in 1:length(p)) {
  U <- runif(T)  # Uniform for mixing
  for (j in 1:T) {
    if (U[j] <= p[i]){
      M2[i,j] <- x1[j] 
    } else {
      M2[i,j] <- x2[j] 
    }
  }
  skew2[i] <- skewness(M2[i,])
  kurt2[i] <- kurtosis(M2[i,])
}

## Ploting

par(mfrow=c(2,1),cex.lab=1, cex.axis=1, cex.main=1, mar = c(2, 4, 4, 4))
plot(skew2 ~ p, type='l',col = "blue", 
     main = "Momentos en función del parámetro de mixtura", 
     xlab="", ylab="Skewness")
plot(kurt2 ~ p, type='l',col = "blue", main = "", xlab = "", ylab = "Kurtosis")


## Same, with different means

mean1_2 <-  1.0
mean2_2 <-  -1.0

#### Moving var ratio (different means) ####
y1_2 <- matrix(0,nrow=length(sd1), ncol=T)
y2_2 <- y1_2
for (i in 1:length(sd1)) {
  y1_2[i,] <- rnorm(T, mean=mean1_2, sd=sd1[i]) 
  y2_2[i,] <- rnorm(T, mean=mean2_2, sd=sd2[i])
}

M1_2 <- matrix(0,nrow=length(sd1), ncol=T) # Mixture 
skew1_2 <- rep(0,length(sd1))
kurt1_2 <- skew1_2
for (i in 1:length(sd1)) {
  U <- runif(T)  # Uniform for mixing
  for (j in 1:T) {
    if (U[j] <= p_ini){
      M1_2[i,j] <- y1_2[i,j] 
    } else {
      M1_2[i,j] <- y2_2[i,j] 
    }
  }
  skew1_2[i] <- skewness(M1_2[i,])
  kurt1_2[i] <- kurtosis(M1_2[i,])
}

# Ploting
par(mfrow=c(2,1),cex.lab=1, cex.axis=1, cex.main=1, mar = c(2, 4, 4, 4))
plot(skew1_2 ~ var_ratio, type='l',col = "blue", 
     main = "Momentos en función del ratio varianza (diferente media)", 
     xlab = "", ylab = "Skewness")
plot(kurt1_2 ~ var_ratio, type='l',col="blue",main="",xlab="", ylab="Kurtosis")


#### Moving mix parameter (different means) ####
x1_2 <- rnorm(T, mean=mean1_2, sd=sd1_ini) 
x2_2 <- rnorm(T, mean=mean2_2, sd=sd2_ini)
M2_2 <- matrix(0,nrow=length(p), ncol=T) # Mixture 
skew2_2 <- rep(0,length(p))
kurt2_2 <- skew2_2
for (i in 1:length(p)) {
  U <- runif(T)  # Uniform for mixing
  for (j in 1:T) {
    if (U[j] <= p[i]){
      M2_2[i,j] <- x1_2[j] 
    } else {
      M2_2[i,j] <- x2_2[j] 
    }
  }
  skew2_2[i] <- skewness(M2_2[i,])
  kurt2_2[i] <- kurtosis(M2_2[i,])
}

# Ploting
par(mfrow=c(2,1),cex.lab=1, cex.axis=1, cex.main=1, mar = c(2, 4, 4, 4))
plot(skew2_2 ~ p, type='l',col = "blue", 
     main = "Momentos en función del parámetro de mixtura (diferente media)", 
     xlab = "", ylab = "Skewness")
plot(kurt2_2 ~ p, type='l',col="blue", main="", xlab="", ylab="Kurtosis")


#### VaR ####

tau <- 0.05 # Significance level
h <- c(1,5,20) # Days ahead

## Moving var ratio (different means)

var_ratio_index <- c(1,round(n/2),n)
sd_M_1 <- rep(0,length(var_ratio_index))
mean_M_1 <- rep(0,length(var_ratio_index))
VaR_M_1 <- matrix(0,nrow = length(h),ncol = length(var_ratio_index))
VaR_N_1 <- matrix(0,nrow = length(h),ncol = length(var_ratio_index))

for (i in 1:length(var_ratio_index)) {
  sd_M_1[i] <- sd(M1_2[var_ratio_index[i],])
  mean_M_1[i] <- mean(M1_2[var_ratio_index[i],])
  for (j in 1:length(h)) {
    VaR_M_1[i,j] <- -sqrt(h[j])*quantile(M2_2[var_ratio_index[i],],tau)
    VaR_N_1[i,j] <- -sd_M_1[i]*sqrt(h[j])*qnorm(tau) - h[j]*mean_M_1[i]
  }
}

# Ploting
VaR1_1 <- matrix(c(VaR_M_1[1,],VaR_N_1[1,]),ncol=3,byrow=TRUE)
colnames(VaR1_1) <- c("1 día", "5 días", "20 días")
rownames(VaR1_1) <- c("Mixtura", "Normal")
VaR1_1 <- as.table(VaR1_1)

VaR2_1 <- matrix(c(VaR_M_1[2,],VaR_N_1[2,]),ncol=3,byrow=TRUE)
colnames(VaR2_1) <- c("1 día", "5 días", "20 días")
rownames(VaR2_1) <- c("Mixtura", "Normal")
VaR2_1 <- as.table(VaR2_1)

VaR3_1 <- matrix(c(VaR_M_1[3,],VaR_N_1[3,]),ncol=3,byrow=TRUE)
colnames(VaR3_1) <- c("1 día", "5 días", "20 días")
rownames(VaR3_1) <- c("Mixtura", "Normal")
VaR3_1 <- as.table(VaR3_1)


par(mfrow=c(1,1),cex.lab=1, cex.axis=1, cex.main=1, mar = c(2, 4, 4, 4))
barplot(VaR1_1,
        main = expression("sd ratio = 2.5"),
        xlab = "Días",
        col = c("blue","darkgrey"),
        beside = TRUE
)
legend("topleft",
       c("Mixtura","Normal"),
       fill = c("blue","darkgrey")
)
barplot(VaR2_1,
        main = expression("sd ratio = 5.5"),
        xlab = "Días",
        col = c("blue","darkgrey"),
        beside = TRUE
)
legend("topleft",
       c("Mixtura","Normal"),
       fill = c("blue","darkgrey")
)
barplot(VaR3_1,
        main = expression("sd ratio = 8.625"),
        xlab = "Días",
        col = c("blue","darkgrey"),
        beside = TRUE
)
legend("topleft",
       c("Mixtura","Normal"),
       fill = c("blue","darkgrey")
)

## Moving mix parameter (different means)

p_index <- c(1,round(n/2),n)
sd_M_2 <- rep(0,length(p_index))
mean_M_2 <- rep(0,length(p_index))
VaR_M_2 <- matrix(0,nrow = length(h),ncol = length(p_index))
VaR_N_2 <- matrix(0,nrow = length(h),ncol = length(p_index))

for (i in 1:length(p_index)) {
  sd_M_2[i] <- sd(M2_2[p_index[i],])
  mean_M_2[i] <- mean(M2_2[p_index[i],])
  for (j in 1:length(h)) {
    VaR_M_2[i,j] <- -sqrt(h[j])*quantile(M2_2[p_index[i],],tau)
    VaR_N_2[i,j] <- -sd_M_2[i]*sqrt(h[j])*qnorm(tau) - h[j]*mean_M_2[i]
  }
}

# Ploting
VaR1_2 <- matrix(c(VaR_M_2[1,],VaR_N_2[1,]),ncol=3,byrow=TRUE)
colnames(VaR1_2) <- c("1 día", "5 días", "20 días")
rownames(VaR1_2) <- c("Mixtura", "Normal")
VaR1_2 <- as.table(VaR1_2)

VaR2_2 <- matrix(c(VaR_M_2[2,],VaR_N_2[2,]),ncol=3,byrow=TRUE)
colnames(VaR2_2) <- c("1 día", "5 días", "20 días")
rownames(VaR2_2) <- c("Mixtura", "Normal")
VaR2_2 <- as.table(VaR2_2)

VaR3_2 <- matrix(c(VaR_M_2[3,],VaR_N_2[3,]),ncol=3,byrow=TRUE)
colnames(VaR3_2) <- c("1 día", "5 días", "20 días")
rownames(VaR3_2) <- c("Mixtura", "Normal")
VaR3_2 <- as.table(VaR3_2)


barplot(VaR1_2,
        main = expression("p = 0.288"),
        xlab = "Días",
        col = c("blue","darkgrey"),
        beside = TRUE
)
legend("topleft",
       c("Mixtura","Normal"),
       fill = c("blue","darkgrey")
)
barplot(VaR2_2,
        main = expression("p = 0.3869388"),
        xlab = "Días",
        col = c("blue","darkgrey"),
        beside = TRUE
)
legend("topleft",
       c("Mixtura","Normal"),
       fill = c("blue","darkgrey")
)
barplot(VaR3_2,
        main = expression("p = 0.49"),
        xlab = "Días",
        col = c("blue","darkgrey"),
        beside = TRUE
)
legend("topleft",
       c("Mixtura","Normal"),
       fill = c("blue","darkgrey")
)
