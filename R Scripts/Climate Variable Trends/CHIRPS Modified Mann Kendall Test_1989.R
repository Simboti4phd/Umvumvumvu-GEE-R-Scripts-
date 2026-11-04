# ============================================================
# MODIFIED MANN-KENDALL TEST
# CHIRPS ANNUAL PRECIPITATION: 1989-2018
# ============================================================

# ------------------------------------------------------------
# 1. Install required packages
# ------------------------------------------------------------

install.packages("modifiedmk")

# Load package
library(modifiedmk)


# ------------------------------------------------------------
# 2. Import the CHIRPS annual precipitation data
# ------------------------------------------------------------

# Set your working directory if necessary
# setwd("C:/Users/YourName/Documents/CHIRPS")

# Read CSV file
data <- read.csv("Chirps_GEE_1989.csv")

# Examine the data
head(data)
str(data)


# ------------------------------------------------------------
# 3. Select Year and Annual Precipitation
# ------------------------------------------------------------

# IMPORTANT:
# Change the column names below if your CSV uses different names.

year <- data$year
precipitation <- data$precipitation


# ------------------------------------------------------------
# 4. Check the study period
# ------------------------------------------------------------

# Keep only 1989-2018
data <- data[data$year >= 1989 & data$year <= 2018,]

# Recreate the vectors after filtering
year <- data$year
precipitation <- data$precipitation


# ------------------------------------------------------------
# 5. Check for missing values
# ------------------------------------------------------------

sum(is.na(precipitation))

# Remove missing observations if present
complete <- complete.cases(year, precipitation)

year <- year[complete]
precipitation <- precipitation[complete]


# ------------------------------------------------------------
# 6. Check the number of observations
# ------------------------------------------------------------

length(precipitation)

# For 1989-2018, there should be 31 observations
# if there is one annual value for every year.


# ------------------------------------------------------------
# 7. Perform Modified Mann-Kendall Test
# ------------------------------------------------------------

mmk_result <- mmkh(precipitation)

# Display results
mmk_result


# ------------------------------------------------------------
# 8. Extract important results
# ------------------------------------------------------------

mmk_result


# ============================================================
# 9. Calculate Sen's slope
# ============================================================

# Install package if necessary
install.packages("trend")

library(trend)

sen_result <- sens.slope(precipitation)

sen_result


# ------------------------------------------------------------
# 10. Extract Sen's slope
# ------------------------------------------------------------

sen_slope <- sen_result$estimates

sen_slope


# ============================================================
# 11. Create a summary of the results
# ============================================================

cat("\n============================================\n")
cat("MODIFIED MANN-KENDALL TREND ANALYSIS\n")
cat("CHIRPS Annual Precipitation\n")
cat("Study Period: 1989-2018\n")
cat("============================================\n")

cat("Number of observations:", length(precipitation), "\n")

cat("\nModified Mann-Kendall Test:\n")
print(mmk_result)

cat("\nSen's Slope:\n")
print(sen_result)

cat("\nSen's slope estimate:",
    round(sen_slope, 3), "\n")


# ============================================================
# 12. Plot the annual precipitation time series
# ============================================================

plot(year,
     precipitation,
     type = "o",
     pch = 16,
     xlab = "Year",
     ylab = "Annual Precipitation (mm)",
     main = "Umvumvumvu CHIRPS Annual Precipitation (1989-2018)",
     lwd = 1.5)


# ------------------------------------------------------------
# Add Sen's slope trend line
# ------------------------------------------------------------

trend_line <- precipitation[1] +
  sen_slope * (year - year[1])

lines(year,
      trend_line,
      lwd = 2,
      col = "red")


# ------------------------------------------------------------
# Add legend
# ------------------------------------------------------------

legend("topright",
       legend = c("Annual precipitation",
                  "Sen's slope"),
       lty = c(1, 1),
       pch = c(16, NA),
       col = c("black", "red"),
       lwd = c(1.5, 2))


# ============================================================
# END OF SCRIPT
# ============================================================