
# ============================================================
# INNOVATIVE TREND ANALYSIS (ITA)
# Annual GLEAM Evapotranspiration: 1989–2018
# 95% confidence interval and significance test
# ============================================================

# 1. Install packages if necessary
install.packages("readr")
install.packages("ggplot2")
install.packages("trend")

# 2. Load packages
library(readr)
library(ggplot2)
library(trend)

# 3. Import data

Evapotranspiration <- read_csv("ET_GLEAM.csv",show_col_types = FALSE
)

# Check column names
names(Evapotranspiration)

# 4. Select the period and remove missing values
data <- Evapotranspiration[
  Evapotranspiration$year >= 1989 &
    Evapotranspiration$year <= 2018,
]

data <- data[
  complete.cases(data$year, data$ET),
]

# Ensure chronological order
data <- data[order(data$year), ]

year <- data$year
evapotranspiration <- data$ET

# Check observations
cat("Number of observations:", length(evapotranspiration), "\n")
cat("First year:", min(year), "\n")
cat("Last year:", max(year), "\n")

# Check that there are 30 annual observations
stopifnot(length(evapotranspiration) == 30)

# 5. Divide the time series into two equal halves
first_half <- evapotranspiration[year <= 2003]
second_half <- evapotranspiration[year >= 2004]

stopifnot(length(first_half) == 15)
stopifnot(length(second_half) == 15)

# 6. Sort the halves for the ITA plot
first_sorted <- sort(first_half)
second_sorted <- sort(second_half)

# 7. Conventional ITA graphical slope
# Descriptive regression slope between sorted values
ita_model <- lm(second_sorted ~ first_sorted)

cat("\nDescriptive regression slope:\n")
print(coef(ita_model)[2])

cat("\nCorrelation between sorted halves:\n")
print(cor(first_sorted, second_sorted))

# 8. ITA confidence interval and significance test
# Uses the ITA implementation in the trend package
ita_test <- trend::itam(evapotranspiration)

cat("\nITA statistical test:\n")
print(ita_test)

cat("\nITA estimated slope:\n")
print(ita_test$estimates)

cat("\nITA confidence interval:\n")
print(ita_test$conf.in)

cat("\nITA p-value:\n")
print(ita_test$p.value)

# 9. Calculate change in mean evapotranspiration
mean_first <- mean(first_half)
mean_second <- mean(second_half)

percentage_change <-
  ((mean_second - mean_first) / mean_first) * 100

cat("\nMean evapotranspiration, 1989–2003:",
    mean_first, "mm\n")

cat("Mean evapotranspiration, 2004–2018:",
    mean_second, "mm\n")

cat("Percentage change:",
    percentage_change, "%\n")

# 11. ITA plot
ita_data <- data.frame(
  First_Half = first_sorted,
  Second_Half = second_sorted
)

ggplot(ita_data, aes(x = First_Half, y = Second_Half)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 1
  ) +
  geom_point(size = 3) +
  labs(
    title = "Umvumvumvu Innovative Trend Analysis of Annual GLEAM Evapotranspiration",
    x = "Sorted evapotranspiration values, 1989–2003 (mm)",
    y = "Sorted evapotranspiration values, 2004–2018 (mm)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(face = "bold", size = 10)
  )

