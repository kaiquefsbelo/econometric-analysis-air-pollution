############################################################
# Air Pollution and Public Health in Sao Paulo
# Econometric Analysis
#
# Dataset: Projeto PASSO (IME-USP)
# Author: Kaique Francisco da Silva Belo
############################################################

##############################
# Setup
##############################

# Required object:
# dados <- readxl::read_excel("data/dados_passos.xlsx")
#
# This script assumes that the data frame `dados` is already
# loaded in the R environment.

##############################
# Helper functions
##############################

summary_stats <- function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE)
  )
}

scatter_with_lm <- function(x, y, main, xlab, ylab, use = "complete.obs") {
  plot(x, y, main = main, xlab = xlab, ylab = ylab)
  abline(lm(y ~ x, na.action = na.exclude), col = "red", lwd = 2)
  cor(x, y, use = use)
}

##############################
# Age and mortality structure
##############################

age_groups <- list(
  "<5" = dados$CAR5,
  "5-12" = dados$CAR5_12,
  "12-20" = dados$CAR12_20,
  "20-35" = dados$CAR20_35,
  "35-50" = dados$CAR35_50,
  "50-65" = dados$CAR50_65,
  "65+" = dados$CAR65
)

boxplot(
  age_groups,
  main = "Graph 1: Cardiac deaths by age group",
  ylab = "Number of deaths"
)

age_summary <- do.call(rbind, lapply(age_groups, summary_stats))

##############################
# Carbon monoxide by monitoring station
##############################

co_stations <- list(
  CO1 = dados$CO1,
  CO3 = dados$CO3,
  CO5 = dados$CO5,
  CO8 = dados$CO8,
  CO9 = dados$CO9,
  CO10 = dados$CO10,
  CO12 = dados$CO12,
  CO16 = dados$CO16
)

boxplot(
  co_stations,
  names = names(co_stations),
  main = "Graph 2: CO concentration by station",
  ylab = "ppm"
)

co_station_summary <- do.call(rbind, lapply(co_stations, summary_stats))

##############################
# Derived variables
##############################

# Mean CO concentration across stations

dados$mean_co <- rowMeans(
  cbind(
    dados$CO1,
    dados$CO3,
    dados$CO5,
    dados$CO8,
    dados$CO9,
    dados$CO10,
    dados$CO12,
    dados$CO16
  ),
  na.rm = TRUE
)

# Mortality ratios

dados$respiratory_ratio <- dados$RESP / dados$TOT
dados$cardiac_ratio <- dados$CARD / dados$TOT

##############################
# CO exceedances by year
##############################

co_exceedances_by_year <- data.frame(
  year = 1994:1997,
  exceedances = sapply(1994:1997, function(y) {
    sum(dados$mean_co > 35 & dados$ANO == y, na.rm = TRUE)
  })
)

##############################
# Monthly mortality patterns
##############################

boxplot(
  dados$RESP ~ dados$MES,
  main = "Graph 3: Respiratory deaths by month (absolute values)",
  xlab = "Month",
  ylab = "Number of deaths"
)

boxplot(
  dados$CARD ~ dados$MES,
  main = "Graph 4: Cardiac deaths by month (absolute values)",
  xlab = "Month",
  ylab = "Number of deaths"
)

boxplot(
  dados$respiratory_ratio ~ dados$MES,
  main = "Graph 5: Respiratory death ratio by month",
  xlab = "Month",
  ylab = "Ratio"
)

boxplot(
  dados$cardiac_ratio ~ dados$MES,
  main = "Graph 6: Cardiac death ratio by month",
  xlab = "Month",
  ylab = "Ratio"
)

##############################
# Respiratory deaths vs weather
##############################

resp_temp_cor <- scatter_with_lm(
  dados$TEMPMIN,
  dados$RESP,
  main = "Graph 7: Respiratory deaths vs minimum temperature",
  xlab = "Minimum temperature (C)",
  ylab = "Respiratory deaths"
)

resp_humidity_cor <- scatter_with_lm(
  dados$HUMIDMED,
  dados$RESP,
  main = "Graph 8: Respiratory deaths vs average humidity",
  xlab = "Average relative humidity (%)",
  ylab = "Respiratory deaths"
)

resp_humidity_range_cor <- scatter_with_lm(
  dados$HUMIDAMP,
  dados$RESP,
  main = "Graph 9: Respiratory deaths vs humidity range",
  xlab = "Humidity range (%)",
  ylab = "Respiratory deaths"
)

##############################
# Mean CO concentration vs weather
##############################

co_temp_cor <- scatter_with_lm(
  dados$TEMPMIN,
  dados$mean_co,
  main = "Graph 10: Mean CO concentration vs minimum temperature",
  xlab = "Minimum temperature (C)",
  ylab = "Mean CO concentration (ppm)"
)

co_humidity_cor <- scatter_with_lm(
  dados$HUMIDMED,
  dados$mean_co,
  main = "Graph 11: Mean CO concentration vs average humidity",
  xlab = "Average relative humidity (%)",
  ylab = "Mean CO concentration (ppm)"
)

co_humidity_range_cor <- scatter_with_lm(
  dados$HUMIDAMP,
  dados$mean_co,
  main = "Graph 12: Mean CO concentration vs humidity range",
  xlab = "Humidity range (%)",
  ylab = "Mean CO concentration (ppm)"
)

##############################
# Respiratory mortality ratio vs weather
##############################

resp_ratio_temp_cor <- scatter_with_lm(
  dados$TEMPMIN,
  dados$respiratory_ratio,
  main = "Graph 13: Respiratory death ratio vs minimum temperature",
  xlab = "Minimum temperature (C)",
  ylab = "Respiratory death ratio"
)

resp_ratio_humidity_cor <- scatter_with_lm(
  dados$HUMIDMED,
  dados$respiratory_ratio,
  main = "Graph 14: Respiratory death ratio vs average humidity",
  xlab = "Average relative humidity (%)",
  ylab = "Respiratory death ratio"
)

resp_ratio_humidity_range_cor <- scatter_with_lm(
  dados$HUMIDAMP,
  dados$respiratory_ratio,
  main = "Graph 15: Respiratory death ratio vs humidity range",
  xlab = "Humidity range (%)",
  ylab = "Respiratory death ratio"
)

##############################
# Mean CO concentration vs causes of death
##############################

co_resp_cor <- scatter_with_lm(
  dados$mean_co,
  dados$RESP,
  main = "Graph 16: Mean CO concentration vs respiratory deaths",
  xlab = "Mean CO concentration (ppm)",
  ylab = "Respiratory deaths"
)

co_card_cor <- scatter_with_lm(
  dados$mean_co,
  dados$CARD,
  main = "Graph 17: Mean CO concentration vs cardiac deaths",
  xlab = "Mean CO concentration (ppm)",
  ylab = "Cardiac deaths"
)

co_other_cor <- scatter_with_lm(
  dados$mean_co,
  dados$OTHER,
  main = "Graph 18: Mean CO concentration vs other causes of death",
  xlab = "Mean CO concentration (ppm)",
  ylab = "Deaths from other causes"
)

co_total_cor <- scatter_with_lm(
  dados$mean_co,
  dados$TOT,
  main = "Graph 19: Mean CO concentration vs total deaths",
  xlab = "Mean CO concentration (ppm)",
  ylab = "Total deaths"
)

##############################
# Simple linear regressions
##############################

model_resp_temp <- lm(dados$RESP ~ dados$TEMPMIN, na.action = na.exclude)
model_resp_ratio_temp <- lm(dados$respiratory_ratio ~ dados$TEMPMIN, na.action = na.exclude)

summary(model_resp_temp)
summary(model_resp_ratio_temp)

##############################
# Missing data analysis for CO stations
##############################

dados$NA_CO1 <- ifelse(is.na(dados$CO1), 1, 0)
dados$NA_CO3 <- ifelse(is.na(dados$CO3), 1, 0)
dados$NA_CO5 <- ifelse(is.na(dados$CO5), 1, 0)
dados$NA_CO8 <- ifelse(is.na(dados$CO8), 1, 0)
dados$NA_CO9 <- ifelse(is.na(dados$CO9), 1, 0)
dados$NA_CO10 <- ifelse(is.na(dados$CO10), 1, 0)
dados$NA_CO12 <- ifelse(is.na(dados$CO12), 1, 0)
dados$NA_CO16 <- ifelse(is.na(dados$CO16), 1, 0)

dados$total_NA_CO <-
  dados$NA_CO1 + dados$NA_CO3 + dados$NA_CO5 + dados$NA_CO8 +
  dados$NA_CO9 + dados$NA_CO10 + dados$NA_CO12 + dados$NA_CO16

missing_by_weekday <- aggregate(total_NA_CO ~ DIASEM, data = dados, FUN = mean, na.rm = TRUE)
missing_by_month <- aggregate(total_NA_CO ~ MES, data = dados, FUN = mean, na.rm = TRUE)
missing_by_year <- aggregate(total_NA_CO ~ ANO, data = dados, FUN = mean, na.rm = TRUE)

##############################
# Pollutant comparison at Congonhas station
##############################

boxplot(
  dados$O38,
  dados$NO28,
  dados$PM108,
  dados$SO28,
  names = c("O3", "NO2", "PM10", "SO2"),
  main = "Graph 20: Pollutant distribution at Congonhas station",
  ylab = "Concentration (ug/m3)"
)

par(mfrow = c(2, 2))

co_o3_cor <- scatter_with_lm(
  dados$O38,
  dados$CO8,
  main = "Graph 21: CO8 vs O3",
  xlab = "O3",
  ylab = "CO"
)

co_no2_cor <- scatter_with_lm(
  dados$NO28,
  dados$CO8,
  main = "Graph 22: CO8 vs NO2",
  xlab = "NO2",
  ylab = "CO"
)

co_pm10_cor <- scatter_with_lm(
  dados$PM108,
  dados$CO8,
  main = "Graph 23: CO8 vs PM10",
  xlab = "PM10",
  ylab = "CO"
)

co_so2_cor <- scatter_with_lm(
  dados$SO28,
  dados$CO8,
  main = "Graph 24: CO8 vs SO2",
  xlab = "SO2",
  ylab = "CO"
)

par(mfrow = c(1, 1))

##############################
# Additional correlations
##############################

cor_resp_temp <- cor(dados$RESP, dados$TEMPMIN, use = "complete.obs")
cor_resp_co8 <- cor(dados$RESP, dados$CO8, use = "complete.obs")
cor_ratio_temp <- cor(dados$respiratory_ratio, dados$TEMPMIN, use = "complete.obs")
cor_ratio_co8 <- cor(dados$respiratory_ratio, dados$CO8, use = "complete.obs")

##############################
# Objects generated by the script
##############################

# age_summary
# co_station_summary
# co_exceedances_by_year
# missing_by_weekday
# missing_by_month
# missing_by_year
# correlation objects created above
