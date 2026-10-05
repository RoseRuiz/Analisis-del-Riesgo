```{r rolling-forecast-function}
build_ma_test_forecasts <- function(r, r2, m, cut) {
  
  # 1. Promedio móvil con exactamente m observaciones
  ma_t <- SMA(r2, n = m)
  
  # 2. El valor calculado en t-1 se usa como forecast para t.
  # Usamos stats::lag para evitar el conflicto con dplyr::lag().
  forecast_all <- stats::lag(ma_t, k = 1)
  
  # 3. Unir rendimiento, proxy realizado y forecast
  combined <- merge(
    r,
    r2,
    forecast_all
  )
  
  # merge.xts() puede conservar los nombres internos de las series,
  # por eso forzamos nombres explícitos DESPUÉS del merge.
  colnames(combined) <- c(
    "Return",
    "Realized",
    "Forecast"
  )
  
  # 4. Eliminar observaciones incompletas
  combined <- na.omit(combined)
  
  # 5. Mantener únicamente el periodo de evaluación común
  test_start <- index(r2)[cut + 1]
  combined <- combined[paste0(test_start, "/")]
  
  return(combined)
}

test_m5 <- build_ma_test_forecasts(
  r = rendimiento,
  r2 = rend_c,
  m = 5,
  cut = cut
)

head(test_m5)
colnames(test_m5)
```