# Proyecto-Econometr-a-II
Script en R del proyecto de cointegración sobre PIB, CO2 y energía
#  Proyecto Econometría II – Relación entre Crecimiento Económico y Presión Ambiental

Este repositorio contiene el código completo en lenguaje **R** del proyecto de investigación de Econometría II, cuyo objetivo es analizar la existencia de una relación de largo plazo entre el **PIB per cápita**, las **emisiones de CO₂ per cápita** y el **consumo energético per cápita** en tres países: **Costa Rica, Brasil y Estados Unidos**, utilizando pruebas de **cointegración de Johansen** y modelos **VEC**.

---

##  Objetivo del estudio

Evaluar si existe una relación estable de largo plazo entre crecimiento económico y presión ambiental, y cómo se ajustan estas variables en el corto plazo, utilizando herramientas econométricas adecuadas para series no estacionarias.

---

##  Metodología

- **Transformación logarítmica** de las variables: ln(PIB), ln(CO₂), ln(Energía).
- **Pruebas ADF** (Augmented Dickey-Fuller) para verificar raíz unitaria.
- **Prueba de cointegración de Johansen** para identificar vectores de cointegración.
- **Modelo VEC (Vector de Corrección de Errores)** para captar la dinámica de corto y largo plazo.
- **Análisis de impulso-respuesta** (IRF) para evaluar el efecto de un shock en ln(PIB) sobre ln(CO₂).
- **ACF de residuos y prueba de estabilidad del VAR** (raíces propias).

---

##  Archivos

| Archivo                            | Descripción                                                       |
|-----------------------------------|-------------------------------------------------------------------|
| `IsaacJovelEconometríaIIProyecto.R` | Script principal en R que contiene todo el análisis econométrico. |
| `README.md`                       | Este archivo. Descripción general del proyecto.                   |

---

##  Datos utilizados

Las variables provienen de la base de datos de **Our World in Data**, y fueron seleccionadas para los países mencionados con cobertura desde 1990 hasta 2021, en su versión per cápita y ajustada a dólares constantes de 2021.

---

## Paquetes requeridos

```r
install.packages(c("tidyverse", "urca", "tseries", "vars", "tsDyn", "readxl"))
