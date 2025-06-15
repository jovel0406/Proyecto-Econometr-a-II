##### Librerías #####

install.packages(c("tidyverse", "urca", "tseries", "vars", "tsDyn"))
library(tidyverse)
library(urca)
library(tseries)
library(vars)
library(tsDyn)



BD <- read_excel("UNA/BD proyecto econometria.xlsx")

# Asegurar nombres uniformes desde BD
BD <- BD_proyecto_econometria %>%
  rename(
    País = PAIS,
    Año = Año,
    PIBPC = `PIBPERCAPITA(BASE2021)`,
    CO2 = CO2_PerCapita,
    Energia = ConsumoEnergiaPerCápita
  )

# Aplicar logaritmos y limpiar
BD_log <- BD %>%
  mutate(
    ln_PIB = log(PIBPC),
    ln_CO2 = log(CO2),
    ln_Energia = log(Energia)
  ) %>%
  filter(
    !is.na(ln_PIB), !is.infinite(ln_PIB),
    !is.na(ln_CO2), !is.infinite(ln_CO2),
    !is.na(ln_Energia), !is.infinite(ln_Energia)
  )

# 1. Filtrar datos de Costa Rica
CR <- BD_log %>%
  filter(País == "Costa Rica") %>%
  dplyr::select(Año, ln_PIB, ln_CO2, ln_Energia)

# 2. Pruebas ADF
adf.test(CR$ln_PIB); adf.test(diff(CR$ln_PIB))
adf.test(CR$ln_CO2); adf.test(diff(CR$ln_CO2))
adf.test(CR$ln_Energia); adf.test(diff(CR$ln_Energia))

# 3. Crear serie de tiempo multivariada
CR_ts <- ts(CR[, -1], start = min(CR$Año))

# 4. Prueba de cointegración de Johansen
johansen_CR <- ca.jo(CR_ts, type = "trace", ecdet = "const", K = 2)
summary(johansen_CR)

# 5. Estimar modelo VEC (r = 1)
vec_CR <- cajorls(johansen_CR, r = 1)
summary(vec_CR$rlm)

# 6. Extraer datos para ECT
data_CR <- vec_CR$rlm$model %>%
  mutate(Año = seq(min(CR$Año) + 2, max(CR$Año)))

# 7. Gráfico: evolución logarítmica
ggplot(BD_log %>% filter(País == "Costa Rica") %>%
         pivot_longer(cols = starts_with("ln_"),
                      names_to = "Variable", values_to = "Valor"),
       aes(x = Año, y = Valor, color = Variable)) +
  geom_line(size = 1.2) +
  labs(title = "Costa Rica: Evolución de ln(PIB), ln(CO2), ln(Energía)",
       x = "Año", y = "Valor (log)") +
  theme_minimal()

# 8. Gráfico: Término de corrección de errores (ECT)
ggplot(data_CR, aes(x = Año, y = ect1)) +
  geom_line(color = "#A23529", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Término de Corrección de Errores (ECT) – Costa Rica",
       x = "Año", y = "ECT") +
  theme_minimal()

# 9. Modelo VAR en primeras diferencias para impulso-respuesta
CR_diff <- diff(as.matrix(CR[, -1]))
CR_diff_ts <- ts(CR_diff, start = min(CR$Año) + 1)
VAR_CR <- VAR(CR_diff_ts, p = 2, type = "const")

# 10. Función impulso-respuesta (PIB → CO2)
irf_CR <- irf(VAR_CR, impulse = "ln_PIB", response = "ln_CO2", n.ahead = 10, boot = TRUE)

# 11. Gráfico impulso-respuesta
plot(irf_CR, main = "Respuesta de ln(CO₂) a un shock en ln(PIB) – Costa Rica")

# 12. Gráfico: Residuos de las tres ecuaciones
residuos_CR <- as.data.frame(resid(vec_CR$rlm))
residuos_CR$Año <- seq(min(CR$Año) + 2, max(CR$Año))

res_plot <- residuos_CR %>%
  pivot_longer(cols = starts_with("ln_"), names_to = "Variable", values_to = "Residual") %>%
  ggplot(aes(x = Año, y = Residual, color = Variable)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuos del modelo VEC – Costa Rica", y = "Residual") +
  theme_minimal()
print(res_plot)

# 13. Correlogramas (ACF) de residuos
par(mfrow = c(1, 3))
acf(residuos_CR$ln_PIB.d, main = "ACF Residuos ln(PIB)")
acf(residuos_CR$ln_CO2.d, main = "ACF Residuos ln(CO2)")
acf(residuos_CR$ln_Energia.d, main = "ACF Residuos ln(Energía)")
par(mfrow = c(1,1))


# 14. Estabilidad del modelo VAR – Raíces en el plano complejo
roots_CR <- roots(VAR_CR)

# Gráfico con círculo unitario (mejor visualización)
plot(roots_CR, circle = TRUE, main = "Raíces del modelo VAR – Costa Rica")

# ============================
# 1. Filtrar datos de Brasil
# ============================
BR <- BD_log %>%
  filter(País == "Brazil") %>%
  dplyr::select(Año, ln_PIB, ln_CO2, ln_Energia)

# =============================
# 2. Pruebas ADF (Raíz unitaria)
# =============================
adf.test(BR$ln_PIB)
adf.test(diff(BR$ln_PIB))

adf.test(BR$ln_CO2)
adf.test(diff(BR$ln_CO2))

adf.test(BR$ln_Energia)
adf.test(diff(BR$ln_Energia))

# ============================
# 3. Serie de tiempo multivariada
# ============================
BR_ts <- ts(BR[, -1], start = min(BR$Año))

# ============================
# 4. Prueba de Cointegración Johansen
# ============================
johansen_BR <- ca.jo(BR_ts, type = "trace", ecdet = "const", K = 2)
summary(johansen_BR)

# ============================
# 5. Estimar Modelo VEC (r = 1)
# ============================
vec_BR <- cajorls(johansen_BR, r = 1)
summary(vec_BR$rlm)

# ============================
# 6. Datos para gráfico del ECT
# ============================
data_BR <- vec_BR$rlm$model %>%
  mutate(Año = seq(min(BR$Año) + 2, max(BR$Año)))

# ============================
# 7. Gráfico de evolución logarítmica
# ============================
BD_log %>%
  filter(País == "Brazil") %>%
  pivot_longer(cols = starts_with("ln_"), names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Año, y = Valor, color = Variable)) +
  geom_line(size = 1.2) +
  labs(title = "Brasil: Evolución de ln(PIB), ln(CO₂), ln(Energía)",
       x = "Año", y = "Valor (log)") +
  theme_minimal()

# ============================
# 8. Gráfico del término ECT
# ============================
ggplot(data_BR, aes(x = Año, y = ect1)) +
  geom_line(color = "#1C1C1C", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Término de Corrección de Errores (ECT) – Brasil",
       x = "Año", y = "ECT") +
  theme_minimal()

# ============================
# 9. VAR en primeras diferencias
# ============================
BR_diff <- diff(as.matrix(BR[, -1]))
BR_diff_ts <- ts(BR_diff, start = min(BR$Año) + 1)

VAR_BR <- VAR(BR_diff_ts, p = 2, type = "const")

# ============================
# 10. Impulso-respuesta (PIB → CO2)
# ============================
irf_BR <- irf(VAR_BR, impulse = "ln_PIB", response = "ln_CO2", n.ahead = 10, boot = TRUE)

# ============================
# 11. Gráfico Impulso-Respuesta
# ============================
plot(irf_BR, main = "Respuesta de ln(CO₂) a un shock en ln(PIB) – Brasil")

# ============================
# 12. Estabilidad del VAR
# ============================
plot(roots(VAR_BR), circle = TRUE, main = "Raíces del modelo VAR – Brasil")

# =====================================
# 1. Filtrar datos de Estados Unidos
# =====================================
USA <- BD_log %>%
  filter(País == "United States") %>%
  dplyr::select(Año, ln_PIB, ln_CO2, ln_Energia)

# =====================================
# 2. Pruebas de raíz unitaria (ADF)
# =====================================
adf.test(USA$ln_PIB)
adf.test(diff(USA$ln_PIB))

adf.test(USA$ln_CO2)
adf.test(diff(USA$ln_CO2))

adf.test(USA$ln_Energia)
adf.test(diff(USA$ln_Energia))

# =====================================
# 3. Serie de tiempo multivariada
# =====================================
USA_ts <- ts(USA[, -1], start = min(USA$Año))

# =====================================
# 4. Cointegración de Johansen
# =====================================
johansen_USA <- ca.jo(USA_ts, type = "trace", ecdet = "const", K = 2)
summary(johansen_USA)

# =====================================
# 5. Estimar modelo VEC (r = 1)
# =====================================
vec_USA <- cajorls(johansen_USA, r = 1)
summary(vec_USA$rlm)

# =====================================
# 6. Extraer data para gráfico del ECT
# =====================================
data_USA <- vec_USA$rlm$model %>%
  mutate(Año = seq(min(USA$Año) + 2, max(USA$Año)))

# =====================================
# 7. Gráfico de evolución logarítmica
# =====================================
BD_log %>%
  filter(País == "United States") %>%
  pivot_longer(cols = starts_with("ln_"), names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Año, y = Valor, color = Variable)) +
  geom_line(size = 1.2) +
  labs(title = "EE. UU.: Evolución de ln(PIB), ln(CO₂), ln(Energía)",
       x = "Año", y = "Valor (log)") +
  theme_minimal()

# =====================================
# 8. Gráfico del término ECT
# =====================================
ggplot(data_USA, aes(x = Año, y = ect1)) +
  geom_line(color = "#395773", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Término de Corrección de Errores (ECT) – EE. UU.",
       x = "Año", y = "ECT") +
  theme_minimal()

# =====================================
# 9. VAR en primeras diferencias
# =====================================
USA_diff <- diff(as.matrix(USA[, -1]))
USA_diff_ts <- ts(USA_diff, start = min(USA$Año) + 1)

VAR_USA <- VAR(USA_diff_ts, p = 2, type = "const")

# =====================================
# 10. Función impulso-respuesta (PIB → CO₂)
# =====================================
irf_USA <- irf(VAR_USA, impulse = "ln_PIB", response = "ln_CO2", n.ahead = 10, boot = TRUE)

# =====================================
# 11. Gráfico de impulso-respuesta
# =====================================
plot(irf_USA, main = "Respuesta de ln(CO₂) a un shock en ln(PIB) – EE. UU.")

# =====================================
# 12. Estabilidad del modelo VAR
# =====================================
plot(roots(VAR_USA), circle = TRUE, main = "Raíces del modelo VAR – EE. UU.")

