
#  Detrended home price data

CS <- read.table(
  file = "D:\\5 курс\\Багатовимірний статистичний аналіз\\Data for R_ Zelterman\\HomePrice.txt",
  header = TRUE, row.names  = 1)
colnames(CS) <- 2001:2011       # columns are years
CS <- log(CS/100)               # log of data values

dc <- dim(CS)
detr <- CS - t(matrix(colMeans(CS), dc[2], dc[1]))

year <- as.integer(colnames(CS))
year <- c(2000, year)
ncity <- dim(CS)[1]
yrange <- range(detr)
### pdf(file = "CSdetrended.pdf")   #    Figure 7.5
plot(year , c(0, detr[1,]), type = "l", ylim = yrange, col = "blue",
     xlab = "Year", ylab = "Detrended C-S Index", cex.lab = 1.5)
text(2009, -.6, labels = "Detroit")
for (i in 2:ncity)
  lines(year , c(0, detr[i, ]), type = "l", ylim = yrange, col = "blue")
lines(c(1990, 2012), c(0,0), type = "l", col = "red")
### dev.off()

# Exchangeable correlation matrix
exchange.mat <- function(size = 5, rho = 0.5, k = 2, p = 3) {
  mat <- diag(size)
  for (i in 1:size) {
    for (j in 1:size) {
      if (i != j) {
        if (abs(i - j) <= k) {
          mat[i, j] <- rho
        } else {
          mat[i, j] <- 0
        }
      }
    }
  }
  
  return(mat)
}
exchange.mat(size = 5, rho = 0.5, k = 2, p = 3)
# Autocorrelation matrix
autocorr.mat <- function(size = 5, rho = 0.5, k = 2, p = 3) {

  mat <- diag(size)
  for (i in 1:size) {
    for (j in 1:size) {
      if (i != j) {
        if (abs(i - j) <= k) {
          mat[i, j] <- rho ^ abs(i - j)
        } else {
          mat[i, j] <- 0
        }
      }
    }
  }
  
  return(mat)
}


cov.model <- function(size, parms)
  # shell to fit patterned covariance matrix 
  #  mv.vars models variances,  mv.cors models correlations
{
  rho <- parms[1]                             # rho is first parameter
  rho <- max(.0001, min(rho, .9999))          # must be between zero and one
  sigma <- sqrt(mv.vars(size, parms[-1]))     # model for marginal st dev's
  sigma %*% t(sigma) * mv.cors(size, rho)     # covariance matrix
}


library(mvtnorm)                # library with -dmvnorm-

fit.cov <- function(parms) # Shell called by nlm
  # computes log-likelihood of detrended home price data (detr)
  # checks for positive definite covariance matrix
{
  logl <- 0
  size <- dim(detr)[2]          # size of the detrended data
  cov <- cov.model(size, parms) # the trial built  covariance matrix
  print(det(cov))               # check on trial covariance matrix 
  if(det(cov) <= 1.0e-30)       # is the covariance positive definite?
  {
    cov <- cov + diag(size)  # adjust the covariance... 
    logl <- -1000            #  ... and penalize the likelihood 
  }                             # log multivariate normal density
  logl <- logl + dmvnorm(detr, mean = rep(0, size), 
                         sigma = cov, log = TRUE)
  logl <- sum(logl)             # log likelihood of detrended data
  print(c(parms, logl))         # trace the progress of nlm
  -logl                         # return negative log likelihood
}


format.nlm <- function(nlm.out)  # format output from nlm
{
  est <- nlm.out$estimate
  p <- length(est)
  estvar <- solve(nlm.out$hessian)
  line <- -nlm.out$minimum
  for (i in 1:p)
    line <- c(line, est[i], sqrt(estvar[i, i]))
  line
}



size <- dim(detr)[2]

mv.vars <- function(size, parms)  rep(exp(parms[1]), size)   #  same std dev for all
mv.cors <- function (size, rho) exchange.mat(size, rho)      #  exchangeable correlation
nlm.out <- nlm(fit.cov, c(0.5, -2), hessian = TRUE)  # 125.8429352
print(format.nlm(nlm.out), 3)
# 125.843   0.650   0.079  -3.211   0.218

# # # #

mv.vars <- function(size, parms)  
  pmin(  exp(parms[1] + parms[2] * (1 : size)), # log-linear vars
         rep(10, size))                         # check for overflow
mv.cors <- function (size, rho) exchange.mat(size, rho)  #  exchangeable correlation

nlm.out <- nlm(fit.cov, c(0.6729004, -4.7436157,  0.2289721), hessian = TRUE)  #   149.8109957
print(format.nlm(nlm.out), 3)
# 149.8110   0.6729   0.0761  -4.7436   0.3027   0.2290   0.0335
# # # #  

mv.vars <- function(size, parms)  
  pmin(  exp(parms[1] + parms[2] * (1 : size) + parms[3] * (1 : size) ^2), # log-quad vars
         rep(10, size))                          # check for overflow
mv.cors <- function (size, rho) exchange.mat(size, rho)  #  exchangeable correlation

nlm.out <- nlm(fit.cov, c(0.7068494, -6.64529169, 0.97739795, -0.06020731),
               hessian = TRUE)  #  180.2007
print(format.nlm(nlm.out), 3)
# 180.2007   0.7068   0.0712  -6.6453   0.3346   0.9774   0.0887  -0.0602   0.0070



######################  autocorrelation models  ################


mv.vars <- function(size, parms)  rep(exp(parms[1]), size)   #  same std dev for all
mv.cors <- function(size, rho) autocorr.mat(size, rho)       #  autocorrelation

nlm.out <- nlm(fit.cov, c(0.9265689, -3.388858), hessian = TRUE)   # 256.174784
print(format.nlm(nlm.out), 3)
# 256.1748   0.9266   0.0172  -3.3889   0.2261
# # # # 

mv.vars <- function(size, parms)  
  pmin(  exp(parms[1] + parms[2] * (1 : size)), # log-linear vars
         rep(10, size))                          # check for overflow
mv.cors <- function (size, rho) autocorr.mat(size, rho)      #  autocorrelation

nlm.out <- nlm(fit.cov, c( 0.90914786,  -4.8670958,  0.1964539), hessian = TRUE)  # 268.7139643
print(format.nlm(nlm.out), 3)
# 268.7140   0.9091   0.0191  -4.8671   0.3317   0.1965   0.0398
# # # #


mv.vars <- function(size, parms)  
  pmin(  exp(parms[1] + parms[2] * (1 : size) + parms[3] * (1 : size) ^2), # log-quad vars
         rep(10, size))                          # check for overflow
mv.cors <- function (size, rho) autocorr.mat(size, rho)  #  exchangeable correlation

nlm.out <- nlm(fit.cov, c(0.93926171, -6.11727682, 0.80739825, -0.04945812),
               hessian = TRUE)  #  292.0728
print(format.nlm(nlm.out), 3)
# 292.07283   0.93931   0.01596  -6.11728   0.33635   0.80739   0.08452  -0.04946   0.00651


library(mvnormtest)
mshapiro.test(t(CS))

# Значення мале, тому ймовірність помилитися, 
# якщо відхилити нульову гіпотезу велика, тому відхиляємо нульову гіпотезу.








