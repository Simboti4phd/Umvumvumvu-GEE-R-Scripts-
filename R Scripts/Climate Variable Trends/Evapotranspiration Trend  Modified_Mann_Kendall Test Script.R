# ============================================================
# MODIFIED MANN-KENDALL TEST
# GLEAM ANNUAL EVAPOTRANSPIRATION: 1989-2018
# ============================================================

# ------------------------------------------------------------
# 1. Install required packages
# ------------------------------------------------------------

install.packages("modifiedmk")

# Load package
library(modifiedmk)


# ------------------------------------------------------------
# 2. Import the GLEAM annual evapotranspiration data
# ------------------------------------------------------------

# Set your working directory 

# Read CSV file
ET_GLEAM <- read.csv("ET_GLEAM.csv")

# Examine the data
head(ET_GLEAM)
str(ET_GLEAM)


# ------------------------------------------------------------
# 3. Select Year and Annual Evapotranspiration
# ------------------------------------------------------------

# IMPORTANT:
# Change the column names below if your CSV uses different names.

year <- ET_GLEAM$year
evapotranspiration <- ET_GLEAM$ET


# ------------------------------------------------------------
# 4. Check the study period
# ------------------------------------------------------------

# Keep only 1989-2018
evapor <- ET_GLEAM[ET_GLEAM$year >= 1989 & ET_GLEAM$year <= 2018,]

# Recreate the vectors after filtering
year <- ET_GLEAM$year
evapotranspiration <- ET_GLEAM$ET


# ------------------------------------------------------------
# 5. Check for missing values
# ------------------------------------------------------------

sum(is.na(ET_GLEAM))


# ------------------------------------------------------------
# 6. Check the number of observations
# ------------------------------------------------------------

length(ET_GLEAM)

# ------------------------------------------------------------
# 7. Perform Modified Mann-Kendall Test
# ------------------------------------------------------------

mmk_result <- mmkh(evapotranspiration)

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

sen_result <- sens.slope(evapotranspiration)

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
cat("GLEAM Annual Evapotranspiration\n")
cat("Study Period: 1989-2018\n")
cat("============================================\n")

cat("Number of observations:", length(evapotranspiration), "\n")

cat("\nModified Mann-Kendall Test:\n")
print(mmk_result)

cat("\nSen's Slope:\n")
print(sen_result)

cat("\nSen's slope estimate:",
    round(sen_slope, 3), "\n")


# ============================================================
# 12. Plot the annual evapotranspiration time series
# ============================================================

plot(year,
     evapotranspiration,
     type = "o",
     pch = 16,
     xlab = "Year",
     ylab = "Annual Evapotranspiration (mm)",
     main = "GLEAM Annual Precipitation (1989-2018)",
     lwd = 1.5)


# ------------------------------------------------------------
# Add Sen's slope trend line
# ------------------------------------------------------------

trend_line <- evapotranspiration[1] +
  sen_slope * (year - year[1])

lines(year,
      trend_line,
      lwd = 2,
      col = "red")


# ------------------------------------------------------------
# Add legend
# ------------------------------------------------------------

legend("topright",
       legend = c("Annual evapotranspiration",
                  "Sen's slope"),
       lty = c(1, 1),
       pch = c(16, NA),
       col = c("black", "red"),
       lwd = c(1.5, 2))


# ============================================================
# END OF SCRIPT
# ============================================================
