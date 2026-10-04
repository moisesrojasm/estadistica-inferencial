library(tidyverse)

penguins <- read.csv("penguins.csv") 

datos <- penguins %>%
  drop_na(body_mass_g)

# Parte 1: Distribución de una variable discreta
datos <- datos %>%
  mutate(
    mass_250 = 250*round(body_mass_g / 250)
  )

# 1. Construir la tabla con el soporte, PMF y CDF
tabla_dist <- datos %>%
  # "soporte" (valores únicos de mass_250)
  count(mass_250) %>% 
  mutate(
    PMF = n / sum(n),
    CDF = cumsum(PMF) 
  ) %>%
  select(-n)

print(tabla_dist)

# 2. Comprobar que la PMF sume 1 y la CDF termine en 1
suma_pmf <- sum(tabla_dist$PMF)
ultimo_cdf <- tail(tabla_dist$CDF, n = 1)

print(paste("Suma de la PMF:", suma_pmf))
print(paste("Último valor de la CDF:", ultimo_cdf))

# 3. Calcule P(3500 <= X <= 4500)
prob_3 <- sum(datos$mass_250 >= 3500 & datos$mass_250 <= 4500) / nrow(datos)
print(paste("Probabilidad de masa entre 3500 y 4500: ", prob_3))

# 4. Obtenga el valor esperado y la varianza a partir de la PMF
valor_esp <- sum(tabla_dist$mass_250 * tabla_dist$PMF)
varianza <- sum(tabla_dist$mass_250^2 * tabla_dist$PMF) - valor_esp^2

print(paste("Valor esperado:", valor_esp))
print(paste("Varianza:", varianza))

# 5. Represente gráficamente la PMF
barplot(height = tabla_dist$PMF,
        names.arg = tabla_dist$mass_250,
        main = "PMF Masa Corporal (Discreta)",
        xlab = "Masa corporal redondeada (gramos)",
        ylab = "Probabilidad P(X = x)")


# Parte 2: Representación de una variable continua
masa <- datos$body_mass_g

# 1. Calcule P(3800 <= X <= 4200)
prob_1 <- mean(masa >= 3800 & masa <= 4200)
print(paste("Probabilidad original:", prob_1))

# 2. Media y varianza poblacionales
media_pob <- mean(masa)

n_pob <- length(masa)
var_pob <- var(masa) * (n_pob - 1) / n_pob

print(paste("Media poblacional:", media_pob))
print(paste("Varianza poblacional:", var_pob))

# 3. Normalizar evento y verificar que la prob sea la misma
masa_01 <- (masa - min(masa)) / (max(masa) - min(masa))
lim_inf_01 <- (3800 - min(masa)) / (max(masa) - min(masa))
lim_sup_01 <- (4200 - min(masa)) / (max(masa) - min(masa))

prob_01 <- mean(masa_01 >= lim_inf_01 & masa_01 <= lim_sup_01)
print(paste("Probabilidad normalizada:", prob_01))

# 4. Histograma normalizado con 12 intérvalos
hist(masa,
     breaks = 12,
     probability = TRUE,
     main = "Histograma de probabilidad (masa corporal)",
     xlab = "Masa Corporal (g)",
     ylab = "Densidad")


# Parte 3: CDF de la poblacióon y ECDF de una muestra
F_pob <- ecdf(masa)

set.seed(123)

muestra_60 <- sample(masa, size = 60, replace = FALSE)

Fn <- ecdf(muestra_60)

# 1. Grafique F_pob y Fn en los mismos ejes
plot(F_pob, 
     main = "Comparación de CDF Poblacional vs ECDF Muestral (n=60)", 
     xlab = "Masa Corporal (g)", 
     ylab = "Probabilidad Acumulada",
     col = "blue",
     lwd = 2)     

plot(Fn, 
     add = TRUE, 
     col = "red",  
     lwd = 2, 
     lty = 2) 

legend("bottomright", 
       legend = c("CDF Población", "ECDF Muestral (n=60)"), 
       col = c("blue", "red"), 
       lty = c(1, 2), 
       lwd = 2)

# 2. Calcule F_poblacion(4000) y Fn(4000).
F_pob_4000 <- F_pob(4000)
Fn_4000 <- Fn(4000)

print(paste("Probabilidad Poblacional F(4000):", F_pob_4000))
print(paste("Probabilidad Muestral Fn(4000):", Fn_4000))


