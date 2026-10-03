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