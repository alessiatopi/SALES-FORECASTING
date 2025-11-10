rm(list=ls())
library(rstudioapi)
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tseries)
library(dplyr)
library(forecast)
library(tsibble)
library(zoo)
# Caricamento dati
data <- read_excel("Dataset_def.xlsx")

######## DATA QUALITY ###########
# somma per colonna tutti gli na di data:
colSums(is.na(data))

# Vediamo se ci sono righe duplicate
duplicated(data)
# andiamo a vedere quale riga è duplicata
which(duplicated(data))
# Non ci sono righe duplicate

summary(data)
str(data)
glimpse(data)

# Conversione formato Date
data <- data %>%
  mutate(
    Date = as.yearmon(Date, format = "%b-%Y"),
  ) %>%
  arrange(Date)

# Verifica sincronizzazione delle serie
length(unique(data$Date)) == nrow(data) # Ogni mese è unico? --> si

# Conversione in zoo multivariato
df_zoo <- zoo(data[,-1], order.by = data$Date)  # Esclude la colonna date come variabili

# 5. Visualizzazione della Y e delle covariate
autoplot(df_zoo[, "Auto_Sales"]) + labs(x = "Month-Year", y = "Auto Sales") + ggtitle("Auto Sales USA")  + theme_bw() +
  theme(
    legend.title = element_blank(),        # opzionale: elimina il titolo della legenda
    plot.title = element_text(size = 14, face = "bold"),  # migliora la leggibilità del titolo
    axis.text = element_text(size = 11),   # ingrandisce il testo degli assi
    axis.title = element_text(size = 12)   # ingrandisce le etichette degli assi
  )# si trend, stag boh?
autoplot(df_zoo[, "Auto_stocks"]) + ggtitle("Scorte auto USA")
autoplot(df_zoo[, "Cost_per_Gallon"]) + ggtitle("Costo Carburante")
autoplot(df_zoo[, "CPI"]) + ggtitle("CPI")
autoplot(df_zoo[, "Disposable_income"]) + ggtitle("Reddito")
autoplot(df_zoo[, "Fed_Effective_Rate"]) + ggtitle("Tasso di interesse")
autoplot(df_zoo[, "Industrial_Production"]) + ggtitle("Produzione industriale")
autoplot(df_zoo[, "Consumer_Credit_Owned"]) + ggtitle("Offerta di credito")
autoplot(df_zoo[, "Consumer_Sentiment"]) + ggtitle("Fiducia dei consumatori")
autoplot(df_zoo[, "Unemployment_Rate"]) + ggtitle("Tasso di disoccupazione")


# Costruzione serie temporale ts
start_year <- year(min(data$Date))
start_month <- month(min(data$Date))
ts_auto <- ts(data$Auto_Sales, start = c(start_year, start_month), frequency = 12)


# Supponiamo di avere una serie ts mensile
decomp <- decompose(ts_auto, type = "additive")

# Visualizzazione delle componenti
plot(decomp)

############################### STAZIONARIETA' #######################
# Per la target
adf.test(ts_auto)

ts_diff <- diff(ts_auto, lag = 12)

# Verifica post-differenziazione
plot(ts_diff)
adf.test(ts_diff)  # p-value dovrebbe scendere < 0.05

# Test ADF sulle covariate
adf.test(ts(data$Auto_stocks, start = c(1992,1), frequency = 12))
adf.test(ts(data$Cost_per_Gallon, start = c(1992,1), frequency = 12))
adf.test(ts(data$CPI, start = c(1992,1), frequency = 12))
adf.test(ts(data$Disposable_income, start = c(1992,1), frequency = 12))
adf.test(ts(data$Fed_Effective_Rate, start = c(1992,1), frequency = 12))
adf.test(ts(data$Industrial_Production, start = c(1992,1), frequency = 12))
adf.test(ts(data$Consumer_Credit_Owned, start = c(1992,1), frequency = 12))
adf.test(ts(data$Consumer_Sentiment, start = c(1992,1), frequency = 12))
adf.test(ts(data$Unemployment_Rate, start = c(1992,1), frequency = 12))


library(tibbletime)

# Costruzione oggetto ts con tutte le serie stazionarie
df_ts <- ts(data[,-1], start = c(1992, 1), frequency = 12)

# Differenziamo
df_diff <- diff(df_ts, lag = 12)
# Verifica post-differenziazione
plot(df_diff)

# df_ts è il multivariate ts differenziato (escluse le date)
# Test di ADF su tutte le colonne
adf_results <- apply(df_diff, 2, function(col) {
  test <- adf.test(col)
  return(test$p.value)
})

# Risultato leggibile
adf_results <- as.data.frame(adf_results)
adf_results$Variable <- rownames(adf_results)
colnames(adf_results)[1] <- "ADF_p_value"
adf_results <- adf_results[, c("Variable", "ADF_p_value")]

print(adf_results)

# Calcolo manuale dell'ACF fino a lag 69
acf_manual <- function(x, max_lag = 69) {
  acf_vals <- numeric(max_lag)
  for (lag in 1:max_lag) {
    acf_vals[lag] <- cor(x[1:(length(x) - lag)], x[(lag + 1):length(x)])
  }
  return(c(1, acf_vals))  # lag 0 = 1
}

acf_vals <- acf_manual(as.numeric(data$Auto_Sales), 69)

# Plot con barre e lags stagionali evidenziati
barplot(acf_vals, names.arg = 0:69, col = ifelse((0:69 %% 12 == 0) & (0:69 != 0), "red", "gray"),
        main = "ACF", xlab = "Lag", ylab = "Autocorrelazione")


# Calcolo manuale semplificato della PACF
pac_manual <- function(x, max_lag = 49) {
  pac_vals <- numeric(max_lag)
  residuals <- x
  for (lag in 1:max_lag) {
    fit <- lm(residuals[(lag + 1):length(x)] ~ x[1:(length(x) - lag)])
    pac_vals[lag] <- cor(x[1:(length(x) - lag)], residuals[(lag + 1):length(x)])
    residuals[(lag + 1):length(x)] <- residuals[(lag + 1):length(x)] - fitted(fit)
  }
  return(c(1, pac_vals))  # lag 0 = 1
}

pac_vals <- pac_manual(as.numeric(data$Auto_Sales), 49)

# Plot con barre
barplot(pac_vals, names.arg = 0:49, col = ifelse((0:49 %% 12 == 0) & (0:49 != 0), "red", "gray"),
        main = "PACF", xlab = "Lag", ylab = "Autocorrelazione parziale")


# Differenziazione stagionale
sales_seasonal_diff <- diff(data$Auto_Sales, lag = 12)

# ACF
ggAcf(sales_seasonal_diff, lag.max = 50) + ggtitle("ACF dopo differenziazione stagionale")

# PACF
ggPacf(sales_seasonal_diff, lag.max = 50) + ggtitle("PACF dopo differenziazione stagionale")

library(tidyr)

df_plot <- data.frame(
  Date = time(data$Auto_Sales)[-c(1:12)],
  Original = data$Auto_Sales[-c(1:12)],
  Diff12 = sales_seasonal_diff
)

df_long <- pivot_longer(df_plot, -Date, names_to = "Serie", values_to = "Valore")

ggplot(df_long, aes(x = Date, y = Valore, color = Serie)) +
  geom_line(linewidth = 1) +
  labs(title = "Monthly Sales over Time",
       subtitle = "Originale vs Differenziata (lag 12)",
       x = "Data", y = "Vendite") +
  theme_minimal()

#################################### SCELTA MODELLO ###############################
## Creo dataset per modello
dev.new()
matrcorr=cor(df_diff)
library(ggcorrplot)
library(corrplot)
corrplot(matrcorr, method = "color", type ="lower",
         addCoef.col = "black", # colore dei coefficienti
         number.cex = 0.7)
df_def <- df_diff[,c(1,3,7,10)]
df_def_scale <- scale(df_def)

ts_auto <- as.matrix(df_def_scale[,1])
ts_CPG <- as.matrix(df_def_scale[,2])
ts_IP <- as.matrix(df_def_scale[,3])
ts_UR <- as.matrix(df_def_scale[,4])

# Test di causalità di Granger (employment causa sales)
library(lmtest)
grangertest(data$Auto_Sales ~ data$Unemployment_Rate, order = 12)
grangertest(data$Unemployment_Rate ~ data$Auto_Sales, order = 12)
grangertest(data$Auto_Sales ~ data$Cost_per_Gallon, order = 12)
grangertest(data$Auto_Sales ~ data$Industrial_Production, order = 12)
grangertest(data$Auto_Sales ~ data$CPI, order = 12)
grangertest(data$Auto_Sales ~ data$Disposable_income, order = 12)
grangertest(data$Auto_Sales ~ data$Consumer_Credit_Owned, order = 12)

grangertest(data$Unemployment_Rate ~ data$Cost_per_Gallon, order = 12)
grangertest(data$Cost_per_Gallon ~ data$Unemployment_Rate, order = 12)
grangertest(data$Industrial_Production ~ data$Cost_per_Gallon, order = 12)
grangertest(data$Cost_per_Gallon ~ data$Industrial_Production, order = 12)
grangertest(data$Industrial_Production ~ data$Unemployment_Rate, order = 12)
grangertest(data$Unemployment_Rate ~ data$Industrial_Production, order = 12)

# Modello senza X
fit <- auto.arima(ts_auto)

# Modello con 2 X
fit2 <- auto.arima(ts_auto, xreg = cbind(ts_UR, ts_CPG))

# Modello con 3 X
fit3 <- auto.arima(ts_auto, xreg = cbind(ts_UR, ts_CPG, ts_IP))

# Confronto AIC
AIC(fit2, fit3)

# Check residuals
checkresiduals(fit2)
checkresiduals(fit3)

# Riepilogo
summary(fit2)
summary(fit3)
lrtest(fit,fit3) # --> modello migliore: modello con UR,CPG,IP

# --- Stima modello AR con auto.arima forzando solo parte AR ---
library(forecast)
model_ar <- auto.arima(ts_auto,
                       max.p = 12, max.q = 0, max.d = 0,
                       seasonal = FALSE,
                       stepwise = FALSE, approximation = FALSE)
summary(model_ar)
checkresiduals(model_ar)

residui <- residuals(model_ar)

# numero di lag da testare
numero_lag <- 11

# Eseguo il test di Ljung-Box
test_ljung_box <- Box.test(residui, lag = numero_lag, type = "Ljung-Box")

# Visualizzo i risultati
print(test_ljung_box)

#################################### ARX ####################################

library(zoo)
n <- 384  # lunghezza della serie differenziata
start_date <- as.yearmon("Jan 1993")

date_diff <- start_date + seq(0, by = 1/12, length.out = n)

# Supponiamo che ts_auto_diff e df_def_diff siano le versioni differenziate
ts_auto_zoo <- zoo(ts_auto, order.by = date_diff)
ts_UR_zoo  <- zoo(ts_UR,  order.by = date_diff)
ts_CPG_zoo  <- zoo(ts_CPG,  order.by = date_diff)
ts_IP_zoo  <- zoo(ts_IP,  order.by = date_diff)

ts_auto_train <- window(ts_auto_zoo, end = as.yearmon("Dec 2023"))
ts_auto_test  <- window(ts_auto_zoo, start = as.yearmon("Jan 2024"))

ts_UR_train <- window(ts_UR_zoo, end = as.yearmon("Dec 2023"))
ts_UR_test  <- window(ts_UR_zoo, start = as.yearmon("Jan 2024"))

ts_CPG_train <- window(ts_CPG_zoo, end = as.yearmon("Dec 2023"))
ts_CPG_test  <- window(ts_CPG_zoo, start = as.yearmon("Jan 2024"))

ts_IP_train <- window(ts_IP_zoo, end = as.yearmon("Dec 2023"))
ts_IP_test  <- window(ts_IP_zoo, start = as.yearmon("Jan 2024"))

library(forecast)

# Imposto ordine ARX: (p, d=0, q=0) → solo parte AR
# Uso auto.arima con vincoli
# Supponiamo che tutte le serie abbiano lo stesso numero di osservazioni e siano già allineate
xreg_mat <- cbind(UR = ts_UR_train,
                  CPG = ts_CPG_train,
                  IP = ts_IP_train)

# Ora puoi usarla correttamente in auto.arima
model_arx <- auto.arima(ts_auto_train,
                        xreg = xreg_mat,
                        max.q = 0, max.d = 0,
                        stepwise = FALSE, approximation = FALSE,
                        seasonal = FALSE)

summary(model_arx)

residui_arx <- residuals(model_arx)
checkresiduals(model_arx)
numero_lag <- 10
# test di Ljung-Box
test_ljung_box <- Box.test(residui_arx, lag = numero_lag, type = "Ljung-Box")
print(test_ljung_box)

## Previsione di unrate nelle date di test
model_unrate <- auto.arima(ts_UR_train)
summary(model_unrate)

forecast_unrate <- forecast(model_unrate, h = 12)
autoplot(forecast_unrate)

## Previsione di Cost per Gallon nelle date di test
model_cpg <- auto.arima(ts_CPG_train)
summary(model_cpg)

forecast_cpg <- forecast(model_cpg, h = 12)
autoplot(forecast_cpg)

## Previsione di Industrial Production nelle date di test
model_ip <- auto.arima(ts_IP_train)
summary(model_ip)

forecast_ip <- forecast(model_ip, h = 12)
autoplot(forecast_ip)


library(Metrics)

mae_val_UR  <- mae(ts_UR_test, forecast_unrate$mean)
rmse_val_UR <- rmse(ts_UR_test, forecast_unrate$mean)
mape_val_UR <- mape(ts_UR_test, forecast_unrate$mean)*100

cat("📊 Performance Forecast VARX su periodo 2024:\n")
cat("MAE  =", round(mae_val_UR, 2), "\n")
cat("RMSE =", round(rmse_val_UR, 2), "\n")
cat("MAPE =", round(mape_val_UR, 2), "%\n")


mae_val_CPG  <- mae(ts_CPG_test, forecast_cpg$mean)
rmse_val_CPG <- rmse(ts_CPG_test, forecast_cpg$mean)
mape_val_CPG <- mape(ts_CPG_test, forecast_cpg$mean)*100

cat("📊 Performance Forecast VARX su periodo 2024:\n")
cat("MAE  =", round(mae_val_CPG, 2), "\n")
cat("RMSE =", round(rmse_val_CPG, 2), "\n")
cat("MAPE =", round(mape_val_CPG, 2), "%\n")


mae_val_IP  <- mae(ts_IP_test, forecast_ip$mean)
rmse_val_IP <- rmse(ts_IP_test, forecast_ip$mean)
mape_val_IP <- mape(ts_IP_test, forecast_ip$mean)*100

cat("📊 Performance Forecast VARX su periodo 2024:\n")
cat("MAE  =", round(mae_val_IP, 2), "\n")
cat("RMSE =", round(rmse_val_IP, 2), "\n")
cat("MAPE =", round(mape_val_IP, 2), "%\n")

# Forecast su test set
xreg_mat_test <- cbind(UR = forecast_unrate$mean,
                  CPG = forecast_cpg$mean,
                  IP = forecast_ip$mean)
forecast_armax <- forecast(model_arx, xreg = xreg_mat_test, h = 12)
forecast_values <- forecast_armax$mean

######## TRASFORMAZIONE INVERSA PER VALUTAZIONE MODELLO #############
# --- Inverso della standardizzazione ---
# Calcola media e sd originali PRIMA della scalatura
auto_sales <- df_def[,1]
mu  <- mean(auto_sales, na.rm = TRUE)
sdv <- sd(auto_sales, na.rm = TRUE)

# Riporta le previsioni dalla scala standardizzata alla scala differenziata
forecast_diff <- forecast_values * sdv + mu
# Serie originale (non differenziata)
auto_sales_orig <- zoo(data$Auto_Sales, order.by = as.yearmon(data$Date))

start_test <- as.yearmon("Jan 2024")
n_test <- nrow(ts_auto_test)  # oppure length(forecast_diff)
forecast_dates <- start_test + seq(0, by = 1/12, length.out = n_test)


lagged_dates <- forecast_dates - 1
y_lag <- auto_sales_orig[lagged_dates]
forecast_final <- as.numeric(forecast_diff) + as.numeric(y_lag)
forecast_final_zoo <- zoo(forecast_final, order.by = forecast_dates)


# Intervalli di confidenza
lower_diff <- as.numeric(forecast_armax$lower[,2]) * sdv + mu
upper_diff <- as.numeric(forecast_armax$upper[,2]) * sdv + mu
lower_final <- lower_diff + y_lag
upper_final <- upper_diff + y_lag

# --- Valutazione ---
actual <- window(auto_sales_orig, start = forecast_dates[1], end = forecast_dates[12])

mae  <- mean(abs(actual - forecast_final_zoo))
rmse <- sqrt(mean((actual - forecast_final_zoo)^2))
mape <- mean(abs((actual - forecast_final_zoo) / actual)) * 100

cat("📊 Performance ARMAX (2024):\n")
cat("MAE: ", round(mae, 2), "\n") # 4476.95
cat("RMSE:", round(rmse, 2), "\n") # 5573.94
cat("MAPE:", round(mape, 2), "%\n") # 3.89%



# Serie train e serie test osservate
train_dates <- time(ts_auto_train)
test_dates  <- time(ts_auto_test)

train_obs <- auto_sales_orig[train_dates]
test_obs  <- auto_sales_orig[test_dates]


# Serie test predetta (in scala originale)
pred <- forecast_final_zoo

# Crea data frame unico
df_plot <- rbind(
  data.frame(Date = time(train_obs), Valore = as.numeric(train_obs), Tipo = "Train"),
  data.frame(Date = time(test_obs),  Valore = as.numeric(test_obs),  Tipo = "Test_Osservato"),
  data.frame(Date = time(pred),      Valore = as.numeric(pred),      Tipo = "Test_Predetto")
)

# Estrai ultimo punto del train
punto_finale_train <- df_plot %>%
  filter(Tipo == "Train") %>%
  slice_tail(n = 1)

# Duplica il punto per i test
punto_per_test_obs <- punto_finale_train %>% mutate(Tipo = "Test_Osservato")
punto_per_test_pred <- punto_finale_train %>% mutate(Tipo = "Test_Predetto")

# Aggiungili all’inizio dei test
df_plot <- bind_rows(df_plot, punto_per_test_obs, punto_per_test_pred)

ggplot(df_plot, aes(x = Date, y = Valore, color = Tipo)) +
  geom_line(linewidth = 1.2) +
  ggtitle("Auto Sales: Train, Test osservato, Test predetto (scala originale)") +
  xlab("Anno") + ylab("Auto Sales") +
  theme_minimal()

##### Grafico con intervalli di confidenza

# Prepara dataframe con intervalli solo per la parte predetta
df_pred <- data.frame(
  Date = forecast_dates,
  Valore = as.numeric(forecast_final_zoo),
  Tipo = "Test_Predetto",
  Lower_95 = as.numeric(lower_final),
  Upper_95 = as.numeric(upper_final)
)

# Aggiungi colonne NA per intervallo alle osservazioni
df_obs <- df_plot %>%
  mutate(Lower_95 = NA, Upper_95 = NA)

# Unione predizioni e osservazioni
df_plot_full <- bind_rows(df_obs, df_pred)

# Grafico con intervallo di confidenza
ggplot(df_plot_full, aes(x = Date, y = Valore, color = Tipo)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(
    data = df_pred,
    aes(x = Date, ymin = Lower_95, ymax = Upper_95),
    inherit.aes = FALSE,
    fill = "steelblue", alpha = 0.25
  ) +
  ggtitle("Auto Sales: Train, Test osservato, Test predetto con IC 95%") +
  xlab("Anno") + ylab("Auto Sales") +
  theme_minimal()



############## PREVISIONE VALORI GEN-MAR 2025 ##############

h <- 3  # numero di mesi da prevedere
k <- ncol(df_def)  # numero di esogene

# Valori previsti di UNRATE
model_unrate <- auto.arima(ts_UR)
summary(model_unrate)
forecast_unrate <- forecast(model_unrate, h = 3)
autoplot(forecast_unrate)

# Valori previsti di Cost per Gallon
model_cpg <- auto.arima(ts_CPG)
summary(model_cpg)
forecast_cpg <- forecast(model_cpg, h = 3)
autoplot(forecast_cpg)

# Valori previsti di Industrial Production
model_ip <- auto.arima(ts_IP)
summary(model_ip)
forecast_ip <- forecast(model_ip, h = 3)
autoplot(forecast_ip)

library(Metrics)
xreg_mat_2025 <- data.frame(cbind(UR = ts_UR,
                  CPG = ts_CPG,
                  IP = ts_IP))
names(xreg_mat_2025)<-c("UR","CPG","IP")
xreg_mat_2025 <- as.matrix (xreg_mat_2025)
model_arx <- auto.arima(ts_auto,
                        xreg = xreg_mat_2025,
                        max.q = 0, max.d = 0,
                        stepwise = FALSE, approximation = FALSE,
                        seasonal = FALSE)
summary(model_arx)

# Previsione
xreg_mat_2025_pred <- cbind(UR = as.numeric(forecast_unrate$mean),
                       CPG = as.numeric(forecast_cpg$mean),
                       IP = as.numeric(forecast_ip$mean))
forecast_arx <- forecast(model_arx, xreg = xreg_mat_2025_pred, h = 3, level = c(80, 90, 95))
# Estrai previsioni della prima variabile (es. Auto_Sales)
pred_values <- as.numeric(forecast_arx$mean)

# Torna alla scala originale
forecast_diff <- pred_values * sdv + mu
lower_diff <- forecast_arx$lower[,3] * sdv + mu
upper_diff <- forecast_arx$upper[,3] * sdv + mu


# Date future: gennaio, febbraio, marzo 2025
future_dates <- as.yearmon("Dec 2024") + seq(1/12, 3/12, by = 1/12)

# Date y_{t-12}: gennaio, febbraio, marzo 2024
lagged_dates_2025 <- future_dates - 1

# Estrai i valori reali di y_{t-12}
y_lag_2025 <- auto_sales_orig[lagged_dates_2025]
y_lag_2025 <- zoo(y_lag_2025, order.by = future_dates)
forecast_diff_zoo <- zoo(forecast_diff, order.by = future_dates)
# Inversione differenziazione
forecast_final_2025 <- forecast_diff_zoo + y_lag_2025
lower_diff_zoo <- zoo(lower_diff, order.by = future_dates)
lower_2025 <- lower_diff_zoo + y_lag_2025
upper_diff_zoo <- zoo(upper_diff, order.by = future_dates)
upper_2025 <- upper_diff_zoo + y_lag_2025

# Dataframe finale
df_2025 <- data.frame(
  Date = future_dates,
  Forecast = forecast_final_2025,
  Lower_95 = lower_2025,
  Upper_95 = upper_2025
)

# Grafico valori forecast
ggplot(df_2025, aes(x = Date, y = Forecast)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "lightblue", alpha = 0.4) +
  ggtitle("Previsioni Auto Sales (Gen–Mar 2025) con IC 95%") +
  ylab("Auto Sales") + xlab("Mese") +
  theme_minimal()

################################# VISUALIZZAZIONE SERIE COMPLETA CON PREVISIONE ##################

# Dati storici fino a Dicembre 2024
last_obs_date <- as.yearmon("Dec 2024")
auto_sales_past <- window(auto_sales_orig, end = last_obs_date)

df_past <- data.frame(
  Date = time(auto_sales_past),
  Valore = as.numeric(auto_sales_past),
  Tipo = "Observed"
)

df_2025 <- data.frame(
  Date = future_dates,
  Valore = as.numeric(forecast_final_2025),
  Tipo = "Predicted",
  Lower_95 = as.numeric(lower_2025),
  Upper_95 = as.numeric(upper_2025)
)


# Unione dati
df_all <- bind_rows(
  df_past %>% mutate(Lower_95 = NA, Upper_95 = NA),
  df_2025
)

library(ggplot2)
ggplot(df_all, aes(x = Date, y = Valore, color = Tipo, group = 1)) +
  geom_line(size = 1.2) +
  geom_ribbon(
    data = df_2025,
    aes(x = Date, ymin = Lower_95, ymax = Upper_95),
    inherit.aes = FALSE,
    fill = "blue", alpha = 0.2
  ) +
  ggtitle("Auto Sales USA: Serie Osservata + Previsioni Gen–Mar 2025 con IC 95%") +
  xlab("Data") + ylab("Auto Sales") +
  theme_minimal() +
  scale_color_manual(values = c("Observed" = "black", "Predicted" = "darkblue"))

############################### VISUALIZZAZIONE DA GEN 2020 #############################
# Filtro per visualizzare solo da gennaio 2020
df_all_zoom <- df_all %>%
  filter(Date >= as.yearmon("Jan 2020"))

# Grafico
ggplot(df_all_zoom, aes(x = Date, y = Valore, color = Tipo, group = 1)) +
  geom_line(size = 1.2) +
  geom_ribbon(
    data = df_2025,
    aes(x = Date, ymin = Lower_95, ymax = Upper_95),
    inherit.aes = FALSE,
    fill = "red", alpha = 0.2
  ) +
  ggtitle("Auto Sales USA: Serie Osservata + Previsioni Gen–Mar 2025 con IC 95% (zoom dal 2020)") +
  xlab("Month-Year") + 
  ylab("Auto Sales") +
  scale_color_manual(values = c("Observed" = "black", "Predicted" = "darkred")) +
  theme_bw() +
  theme(
    legend.title = element_blank(),        # opzionale: elimina il titolo della legenda
    plot.title = element_text(size = 14, face = "bold"),  # migliora la leggibilità del titolo
    axis.text = element_text(size = 11),   # ingrandisce il testo degli assi
    axis.title = element_text(size = 12)   # ingrandisce le etichette degli assi
  )
