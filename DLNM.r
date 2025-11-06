# ==============================================================================
# Distributed Lag Nonlinear Model (DLNM) Analysis
# Environmental Epidemiology: Air Pollution and Asthma Cases
# ==============================================================================
# This script analyzes the association between air pollutants and asthma cases
# using Distributed Lag Nonlinear Models (DLNM) to account for delayed effects
# and exposure-response relationships.

# Load required libraries
library(dlnm)      # Distributed Lag Nonlinear Models
library(splines)   # Spline functions for smoothing
library(readxl)    # Read Excel files
library(stringr)   # String manipulation
library(writexl)   # Write Excel files

# Set working directory
setwd("C:/Users/At3/Desktop/TMU")

# ==============================================================================
# DATA LOADING AND PREPROCESSING
# ==============================================================================

# Read environmental data (air pollutant concentrations) and asthma case data
envData <- read_excel("environment and asthma/data.xlsx", sheet = "environment")
caseData <- read_excel("environment and asthma/data.xlsx", sheet = "asthma")

# Standardize date format (replace hyphens with slashes for consistency)
caseData$Date <- gsub("-","/",caseData$Date)

# Extract environmental factors for each pollutant
# Separate data frames for each pollutant: CO, NO2, O3, SO2, PM2.5, PM10
COData <- data.frame(Date = envData[(envData$index=="CO"),1],
                 Area = envData[(envData$index=="CO"),2],
                 CO = envData[(envData$index=="CO"),4])
NO2Data <- data.frame(Date = envData[(envData$index=="NO2"),1],
                  Area = envData[(envData$index=="NO2"),2],
                  NO2 = envData[(envData$index=="NO2"),4])
O3Data <- data.frame(Date = envData[(envData$index=="O3"),1],
                 Area = envData[(envData$index=="O3"),2],
                 O3 = envData[(envData$index=="O3"),4])
SO2Data <- data.frame(Date = envData[(envData$index=="SO2"),1],
                 Area = envData[(envData$index=="SO2"),2],
                 SO2 = envData[(envData$index=="SO2"),4])
PM25Data <- data.frame(Date = envData[(envData$index=="PM2.5"),1],
                  Area = envData[(envData$index=="PM2.5"),2],
                  PM25 = envData[(envData$index=="PM2.5"),4])
PM10Data <- data.frame(Date = envData[(envData$index=="PM10"),1],
                  Area = envData[(envData$index=="PM10"),2],
                  PM10 = envData[(envData$index=="PM10"),4])

# Find dates that are available in both datasets (intersection)
availDates <- intersect(caseData$Date, envData$Date)

# Initialize vectors to store aggregated pollutant concentrations and cases
CO <- double(length = length(availDates))
NO2 <- double(length = length(availDates))
O3 <- double(length = length(availDates))
SO2 <- double(length = length(availDates))
PM25 <- double(length = length(availDates))
PM10 <- double(length = length(availDates))
cases <- numeric(length = length(availDates))

# Aggregate data across all areas for each date
for (i in 1:length(availDates)){
  # Average pollutant concentrations over all areas for each date
  # This creates area-averaged daily concentrations
  val <- COData[(COData$Date == availDates[i]), 3]
  CO[i] <- mean(val)
  val <- NO2Data[(NO2Data$Date == availDates[i]), 3]
  NO2[i] <- mean(val)
  val <- O3Data[(O3Data$Date == availDates[i]), 3]
  O3[i] <- mean(val)
  val <- SO2Data[(SO2Data$Date == availDates[i]), 3]
  SO2[i] <- mean(val)
  val <- PM25Data[(PM25Data$Date == availDates[i]), 3]
  PM25[i] <- mean(val)
  val <- PM10Data[(PM10Data$Date == availDates[i]), 3]
  PM10[i] <- mean(val)

  # Sum asthma cases over all areas for each date
  val <- caseData[(caseData$Date == availDates[i]), 3]
  cases[i] <- sum(val)

}

# Extract month from date string (positions 6-7, e.g., "2020/08/15" -> "08")
# This will be used to control for seasonal effects in the model
mon <- as.double(substr(availDates,6,7))

# Create final combined dataset for analysis
AsthmaData <- data.frame(Date = availDates, CO = CO, NO2 = NO2, O3 = O3, 
                         SO2 = SO2, PM25 = PM25, PM10 = PM10, Month = mon,
                         Cases = cases)

# ==============================================================================
# PEARSON CORRELATION ANALYSIS
# ==============================================================================
# Calculate simple correlations between each pollutant and asthma cases
# This provides an initial assessment of associations (without accounting for lags)

# Extract p-values from Pearson correlation tests for each pollutant
pVal <- c(cor.test(AsthmaData$CO, AsthmaData$Cases,
                           method = "pearson")$p.value,
                  cor.test(AsthmaData$NO2, AsthmaData$Cases,
                           method = "pearson")$p.value,
                  cor.test(AsthmaData$O3, AsthmaData$Cases,
                           method = "pearson")$p.value,
                  cor.test(AsthmaData$SO2, AsthmaData$Cases,
                           method = "pearson")$p.value,
                  cor.test(AsthmaData$PM25, AsthmaData$Cases,
                           method = "pearson")$p.value,
                  cor.test(AsthmaData$PM10, AsthmaData$Cases,
                           method = "pearson")$p.value)
                  
# Extract correlation coefficients (estimates) from Pearson correlation tests
estim <- c(cor.test(AsthmaData$CO, AsthmaData$Cases,
                   method = "pearson")$estimate,
          cor.test(AsthmaData$NO2, AsthmaData$Cases,
                   method = "pearson")$estimate,
          cor.test(AsthmaData$O3, AsthmaData$Cases,
                   method = "pearson")$estimate,
          cor.test(AsthmaData$SO2, AsthmaData$Cases,
                   method = "pearson")$estimate,
          cor.test(AsthmaData$PM25, AsthmaData$Cases,
                   method = "pearson")$estimate,
          cor.test(AsthmaData$PM10, AsthmaData$Cases,
                   method = "pearson")$estimate)

# Combine correlation results into a data frame
correlation.data <- data.frame(variables = c("CO", "NO2", "O3", "SO2", "PM2.5", "PM10"),
                               p_value = pVal,
                               estimate = estim)

# Export correlation results to Excel file
write_xlsx(correlation.data,"C:/Users/At3/Desktop/TMU/Correlation.xlsx")


# ==============================================================================
# DISTRIBUTED LAG NONLINEAR MODEL (DLNM) ANALYSIS
# ==============================================================================
# DLNM allows modeling of delayed effects (lags) and nonlinear exposure-response
# relationships between air pollutants and health outcomes

# ==============================================================================
# CREATE CROSSBASIS MATRICES
# ==============================================================================
# Crossbasis matrices combine exposure-response and lag-response dimensions
# Parameters:
#   - lag=15: Maximum lag of 15 days (exposure effects up to 15 days prior)
#   - argvar: Exposure-response function (linear, no intercept)
#   - arglag: Lag-response function (polynomial with varying degrees)

# Carbon Monoxide (CO) - 4th degree polynomial for lag structure
cb.co <- crossbasis(AsthmaData$CO, lag=15, argvar=list(fun="lin", intercept=FALSE),
                    arglag=list(fun="poly", degree=4))

# Nitrogen Dioxide (NO2) - 4th degree polynomial for lag structure
cb.no2 <- crossbasis(AsthmaData$NO2, lag=15, argvar=list(fun="lin", intercept=FALSE),
                     arglag=list(fun="poly", degree=4))

# Ozone (O3) - 6th degree polynomial for lag structure
cb.o3 <- crossbasis(AsthmaData$O3, lag=15, argvar=list(fun="lin", intercept=FALSE),
                    arglag=list(fun="poly", degree=6))

# Sulfur Dioxide (SO2) - 4th degree polynomial for lag structure
cb.so2 <- crossbasis(AsthmaData$SO2, lag=15, argvar=list(fun="lin", intercept=FALSE),
                     arglag=list(fun="poly", degree=4))

# Fine Particulate Matter (PM2.5) - 6th degree polynomial for lag structure
# Note: Uses PM10 data (may be a data issue - should use PM25Data)
cb.pm25 <- crossbasis(AsthmaData$PM10, lag=15, argvar=list(fun="lin", intercept=FALSE),
                      arglag=list(fun="poly", degree=6))

# Coarse Particulate Matter (PM10) - 8th degree polynomial for lag structure
cb.pm10 <- crossbasis(AsthmaData$PM10, lag=15, argvar=list(fun="lin", intercept=FALSE),
                     arglag=list(fun="poly", degree=8))

# ==============================================================================
# FIT GENERALIZED LINEAR MODELS (GLM)
# ==============================================================================
# Fit separate models for each pollutant using:
#   - Quasipoisson family: Appropriate for count data (asthma cases) with potential overdispersion
#   - Natural spline for month (6 df): Controls for seasonal trends
#   - Crossbasis term: Captures lagged and exposure-response effects

# Model for Carbon Monoxide (CO)
modelco <- glm(AsthmaData$Cases ~ cb.co + ns(AsthmaData$Month, 6),
                 family=quasipoisson(), AsthmaData)

# Model for Nitrogen Dioxide (NO2)
modelno2 <- glm(AsthmaData$Cases ~ cb.no2 + ns(AsthmaData$Month, 6),
               family=quasipoisson(), AsthmaData)

# Model for Ozone (O3)
modelo3 <- glm(AsthmaData$Cases ~ cb.o3 + ns(AsthmaData$Month, 6),
               family=quasipoisson(), AsthmaData)

# Model for Sulfur Dioxide (SO2)
modelso2 <- glm(AsthmaData$Cases ~ cb.so2 + ns(AsthmaData$Month, 6),
               family=quasipoisson(), AsthmaData)

# Model for Fine Particulate Matter (PM2.5)
modelpm25 <- glm(AsthmaData$Cases ~ cb.pm25 + ns(AsthmaData$Month, 6),
              family=quasipoisson(), AsthmaData)

# Model for Coarse Particulate Matter (PM10)
modelpm10 <- glm(AsthmaData$Cases ~ cb.pm10 + ns(AsthmaData$Month, 6),
               family=quasipoisson(), AsthmaData)

# ==============================================================================
# PREDICT AND VISUALIZE ASSOCIATIONS
# ==============================================================================
# Generate predictions from fitted models and create plots showing:
#   - Relative Risk (RR) of asthma cases associated with pollutant exposure
#   - Lag-response relationships (how effects vary by lag time)
# Parameters:
#   - at=0:20: Predict at exposure levels from 0 to 20 units
#   - bylag=0.2: Lag interval for predictions (0.2 days)
#   - cumul=TRUE: Calculate cumulative effects across all lags

# Carbon Monoxide (CO) - predictions and plot
pred.co <- crosspred(cb.co, modelco, at=0:20, bylag=0.2, cumul=TRUE)
# Plot lag-response curve showing RR at different lag times
# var=1: Exposure level of 1 unit; col=3: green color
plot(pred.co, "slices", var=1, col=3, ylab="RR", ci.arg=list(density=30,lwd=2),
     main="Association with CO")

# Nitrogen Dioxide (NO2) - predictions (plot commented out)
pred.no2 <- crosspred(cb.no2, modelno2, at=0:20, bylag=0.2, cumul=TRUE)
# plot(pred.no2, "slices", var=5, col=3, ylab="RR", ci.arg=list(density=30,lwd=2),
#      main="Association with NO_2")

# Ozone (O3) - predictions (plot commented out)
pred.o3 <- crosspred(cb.o3, modelo3, at=0:20, bylag=0.2, cumul=TRUE)
# plot(pred.o3, "slices", var=10, col=3, ylab="RR", ci.arg=list(density=30,lwd=2),
#      main="Association with O_3")

# Sulfur Dioxide (SO2) - predictions (plot commented out)
pred.so2 <- crosspred(cb.so2, modelso2, at=0:20, bylag=0.2, cumul=TRUE)
# plot(pred.so2, "slices", var=15, col=3, ylab="RR", ci.arg=list(density=30,lwd=2),
#      main="Association with SO_2")

# Fine Particulate Matter (PM2.5) - predictions (plot commented out)
pred.pm25 <- crosspred(cb.pm25, modelpm25, at=0:20, bylag=0.2, cumul=TRUE)
# plot(pred.pm25, "slices", var=15, col=3, ylab="RR", ci.arg=list(density=30,lwd=2),
#      main="Association with PM_2.5")

# Coarse Particulate Matter (PM10) - predictions (plot commented out)
pred.pm10 <- crosspred(cb.pm10, modelpm10, at=0:20, bylag=0.2, cumul=TRUE)
# plot(pred.pm10, "slices", var=15, col=3, ylab="RR", ci.arg=list(density=30,lwd=2),
#      main="Association with PM_10")