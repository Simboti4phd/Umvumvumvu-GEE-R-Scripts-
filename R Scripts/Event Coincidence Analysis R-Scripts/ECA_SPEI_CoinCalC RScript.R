# Load the package
library(CoinCalc)

# Check the package version
packageVersion("CoinCalc")  # Should return ‘2.0’

# Read the CSV file
data <- read.csv("Min_Flow_SPEII.csv")

# Check the structure
str(data)

# Adjust date format (adjust the format string as needed)
data$Period <- as.Date(data$Period, format = "%Y-%m-%d")

#Binarize Events
# Event B: River intermittence onset
data$intermittence_event <- ifelse(data$Min_Flow <= 0.01, 1, 0)

# Event A: Meteorological drought (do this for each SPEI timescale)
data$spei1_event <- ifelse(data$SPEI_1 <= 1.0, 1, 0)
data$spei3_event <- ifelse(data$SPEI_3 <= 1.0, 1, 0)
data$spei6_event <- ifelse(data$SPEI_6 <= 1.0, 1, 0)
data$spei12_event <- ifelse(data$SPEI_12 >= 1.0, 1, 0)

eca_spei1_test <- CC.eca.ts(
  seriesA = data$spei1_event,
  seriesB = data$intermittence_event,
  delT = 1,
  tau = 1,
  sigtest = "poisson"
)

eca_spei3_test <- CC.eca.ts(
  seriesA = data$spei3_event,
  seriesB = data$intermittence_event,
  delT =12,
  tau = 6,
  sigtest = "poisson"
)

eca_spei6_test <- CC.eca.ts(
  seriesA = data$spei6_event,
  seriesB = data$intermittence_event,
  delT = 12,
  tau = 6,
  sigtest = "poisson"
)

eca_spei12_test <- CC.eca.ts(
  seriesA = data$spei12_event,
  seriesB = data$intermittence_event,
  delT = 12,
  tau = 6,
  sigtest = "poisson"
)
eca_spei1_test
