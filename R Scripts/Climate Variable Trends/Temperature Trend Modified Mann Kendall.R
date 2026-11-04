# ============================================================
# ANNUAL TEMPERATURE TREND ANALYSIS
# Study period: 1989–2018
# ============================================================

# Install packages if necessary
install.packages("readr")
install.packages("ggplot2")
install.packages("Kendall")
install.packages("trend")

# Load packages
library(readr)
library(ggplot2)
library(Kendall)
library(trend)


# ------------------------------------------------------------
# 1. IMPORT ANNUAL TEMPERATURE DATA
# ------------------------------------------------------------

temperature_data <- read_csv("Temp_GEE_1989.csv",
                     show_col_types = FALSE)

# View data
print(temperature_data)

# Check column names
names(temperature_data)


# ------------------------------------------------------------
# 2. PREPARE THE DATA
# ------------------------------------------------------------

# Change these names if your CSV uses different headings
year <- temperature_data$year
termperature <- temperature_data$temperature


# Remove missing values
valid <- !is.na(year) & !is.na(termperature)

year <- year[valid]
temperature <- termperature[valid]


# Check number of observations
length(year)


# ------------------------------------------------------------
# 3. CREATE ANNUAL TEMPERATURE TIME SERIES
# ------------------------------------------------------------

temperature_ts <- ts(
  temperature,
  start = min(year),
  frequency = 1
)

print(temperature_ts)


# ------------------------------------------------------------
# 4. PLOT ANNUAL TEMPERATURE TIME SERIES
# ------------------------------------------------------------

plot(
  year,
  temperature,
  type = "l",
  lwd = 2,
  xlab = "Year",
  ylab = "Temperature (°C)",
  main = "Umvumvumvu Annual Temperature (1989–2018)"
)

points(
  year,
  temperature,
  pch = 16
)


# ------------------------------------------------------------
# 5. LINEAR TREND
# ------------------------------------------------------------

trend_model <- lm(
  temperature ~ year
)

summary(trend_model)


# Add linear trendline
abline(
  trend_model,
  lwd = 2,
  col = "red"
)


# ------------------------------------------------------------
# 6. MODIFIED MANN-KENDALL TEST
# ------------------------------------------------------------

install.packages("modifiedmk")

library(modifiedmk)

mmk_result <- mmkh(temperature)

print(mmk_result)


# ------------------------------------------------------------
# 8. SEN'S SLOPE
# ------------------------------------------------------------

library(trend)

sen_result <- sens.slope(temperature)

print(sen_result)

sen_slope <- sen_result$estimates

cat(
  "Sen's temperature slope:",
  sen_slope,
  "°C/year\n"
)