# Load the package
library(CoinCalc)

# Check the package version
packageVersion("CoinCalc")  # Should return ‘2.0’

# Read the CSV file
data <- read.csv("Min_Flow_STI.csv")

# Check the structure
str(data)

# Adjust date format (adjust the format string as needed)
data$Period <- as.Date(data$Period, format = "%Y-%m-%d")

#Binarize Events
# Event B: River intermittence onset
data$intermittence_event <- ifelse(data$Min_Flow <= 0.01, 1, 0)

# Event A: Meteorological drought (do this for each STI timescale)
data$sti1_event <- ifelse(data$STI_1 >= 1.0, 1, 0)
data$sti3_event <- ifelse(data$STI_3 >= 1.0, 1, 0)
data$sti6_event <- ifelse(data$STI_6 >= 1.0, 1, 0)
data$sti12_event <- ifelse(data$STI_12 >= 1.0, 1, 0)

eca_sti1_test <- CC.eca.ts(
  seriesA = data$sti1_event,
  seriesB = data$intermittence_event,
  delT = 12,
  tau = 6,
  sigtest = "poisson"
)

eca_sti3_test <- CC.eca.ts(
  seriesA = data$sti3_event,
  seriesB = data$intermittence_event,
  delT = 12,
  tau = 6,
  sigtest = "poisson"
)

eca_sti6_test <- CC.eca.ts(
  seriesA = data$sti6_event,
  seriesB = data$intermittence_event,
  delT = 12,
  tau = 6,
  sigtest = "poisson"
)

eca_sti12_test <- CC.eca.ts(
  seriesA = data$sti12_event,
  seriesB = data$intermittence_event,
  delT = 12,
  tau = 6,
  sigtest = "poisson"
)
eca_sti1_test
