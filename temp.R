linea<-"---------------------------------------------"
L<- function(x,k){
  return(sum(-x+k)-2*(sum(log(1+exp(-(x-k))))))
}

L1<- function(x,k){
  return(sum(-(exp(k)-exp(x))/(exp(k)+exp(x))))
}

L2<- function(x,k){
  return(sum(-2*(exp(k+x)/(exp(k)+exp(x))^2)))
}

count_values_in_quantiles <- function(sample, n) {
  # Calculate quantiles for chi-squared distribution with 1 degree of freedom
  quantiles <- qchisq(seq(0, 1, length.out = n + 1), df = 1)
  
  # Initialize counts vector
  counts <- rep(0, n)
  
  # Count values within each interval
  for (i in 1:n) {
    counts[i] <- sum(sample >= quantiles[i] & sample <= quantiles[i + 1])
  }
  
  # Return counts
  return(counts)
}

n_values <- c(10,20,50,100,200)
m <- 1000

XL <- matrix(NA, nrow = m, ncol = length(n_values))
XW <- matrix(NA, nrow = m, ncol = length(n_values))
XR <- matrix(NA, nrow = m, ncol = length(n_values))


n_values <- c(10,20,50,100,200)
for (j in 1:length(n_values)) {
  k<-n_values[j]
  
  for (i in 1:m) {
    # Generar una muestra de tamaño n utilizando la función inverse_sample
    x <- rlogis(k)
  
    p<-mean(x)
    f  <-function(a){return(L1(x,a))}
    fp <-function(a){return(L2(x,a))}
    
    for(o in range(100000)){
      p<-p -(f(p)/fp(p))
    }
    XL[i, j] <- -(2)*(L(x,0)-L(x,p))
    XW[i, j] <- (sqrt(k*1/3)*p)^2
    XR[i, j] <- (L1(x,0)/sqrt(k/3))^2
  }
}


for (j in 1:length(n_values)) {
  n <- n_values[j]
  print(linea)
  
  sample <- XR[, j]
  observed <- count_values_in_quantiles(sample, n=7)
  expected <- rep(1/7, 7)
  
  cat("Result for n =", n,"with XR statistic", "\n")
  
  # Realizar prueba de bondad de ajuste con estadístico de Pearson K^2_n
  chisq_result <- chisq.test(observed, p = expected)
  print(chisq_result)
  
  print(linea)
  # Aplicar la prueba de bondad de ajuste de Kolmogorov-Smirnov
  ks_result <- ks.test(sample, "pchisq", df = 1)
  print(ks_result)
  
  print(linea)
  # Aplicar la prueba de bondad de ajuste de Cramér-von Mises
  cvm_result <- cvm.test(sample, "pchisq", df = 1)
  print(cvm_result)
}
