# Diego Moisés Rojas Mata

V <- 10
R <- 100
I <- V/R

mediciones <- rnorm(100, mean = I, sd = 0.002)

promedios_acum <- numeric(100) 
suma_actual <- 0     

for (i in 1:100) {
  suma_actual <- suma_actual + mediciones[i]
  promedios_acum[i] <- suma_actual / i
}

plot(1:100, promedios_acum, 
     type = "l", 
     main = "Estabilización del Promedio Acumulativo (I = 0.1)", 
     xlab = "Número de Medición", 
     ylab = "Corriente Promedio (A)")

abline(h = I, col = "red")