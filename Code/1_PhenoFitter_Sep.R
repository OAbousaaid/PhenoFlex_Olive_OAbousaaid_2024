# Calibration starting from September 1st ####

# The needed packages for all analysis
library(chillR)
library(tidyverse)
library(readxl)
library(dormancyR)
library(tidyr)
library(tidyverse)
library(dplyr)
library(multcomp)
library(ggplot2)
library(scales)
library(ggpubr)
library(ggtext)
library(patchwork)

# For reproducibility we set the seed to a random value
set.seed(52243)

# Data preparation:
#lets import temperature data first

data <- read_excel("HT1985_2022.xlsx")
data <- as.data.frame(data)

data <- add_YEARMODA(data)

# Import the phenology data from the folder
pheno <- read_excel("Bloom_PM.xlsx")
pheno <- as.data.frame(pheno)

# Here I will analyse the data of Picholine Marocaine (PM) from 1986 to 2022
# Since data are not available from 1992 to 1996 and 2020 I will exclude this rows from the data frame

# Pheno data
pheno_v1 <- na.omit(pheno)

# Calibration season
calibration_seasons <- pheno_v1[1:23,1]

# Define the list of seasons (weather data) the season list from September to June
season_list_Sep <- genSeasonList(data, mrange = c(9, 6), years = calibration_seasons)

# Model run number 1 ####
# Set the initial parameters (as in Fernandez et al. 2022)
#                          yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_sep_r1 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_sep_r1   <- c(          40,          190,          0.5,           25,       3372.8,       9900.3,       6319.5,  5.939917e13,            4,           36,            4,         1.60)
upper_sep_r1 <- c(          80,          500,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_sep_r1 <- phenologyFitter(par.guess = par_sep_r1,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_Sep,
                                   lower = lower_sep_r1,
                                   upper = upper_sep_r1,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_sep_r1[["bloomJDays"]],pheno_fit_sep_r1[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_sep_r1 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_sep_r1

# Parameters of run 1
print(pheno_fit_sep_r1$par)

# Model run number 2 ####
# Take the parameters of the previous run and put it in the next run; the same is applied in all runs
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_sep_r2 <- c(          30,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_sep_r2   <- c(7.906034e+01, 4.990033e+02, 1.010994e-01, 2.131714e+01, 3.018779e+03, 9.574559e+03, 6.001538e+03, 5.939923e+13, 1.932229e+00, 2.226730e+01, 9.637927e+00, 2.351275e+01)
upper_sep_r2 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_sep_r2 <- phenologyFitter(par.guess = par_sep_r2,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_Sep,
                                    lower = lower_sep_r2,
                                    upper = upper_sep_r2,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_sep_r2[["bloomJDays"]],pheno_fit_sep_r2[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_sep_r2 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_sep_r2

# Parameters of run 2
print(pheno_fit_sep_r2$par)

# Model run number 3 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_sep_r3 <- c(          35,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_sep_r3   <- c(7.718768e+01, 9.759396e+02, 9.831368e-01, 1.416408e+01, 3.013095e+03, 9.575190e+03, 6.046601e+03, 5.939921e+13, 3.871756e+00, 3.934322e+01, 9.858530e+00, 5.680486e+00)
upper_sep_r3 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_sep_r3 <- phenologyFitter(par.guess = par_sep_r3,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_Sep,
                                    lower = lower_sep_r3,
                                    upper = upper_sep_r3,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_sep_r3[["bloomJDays"]],pheno_fit_sep_r3[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_sep_r3 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_sep_r3

# Parameters of run 3
print(pheno_fit_sep_r3$par)

# Calibration starting from mid september ####

# Julian day to date and vice-versa
as.POSIXlt(c("15.9"), format = "%d.%m")$yday+1
as.Date(258, format="%d.%m",origin = "01.01")-1

# Define the list of seasons (weather data) the season list from october to june
season_list_mid_sep <- list()

for (i in c(1:23)) {
  num_rows <- nrow(season_list_Sep[[i]])
  season_list_mid_sep[[i]] <- season_list_Sep[[i]][337:num_rows,]
  }

# Model run number 1 ####
# Set the initial parameters (as in Fernandez et al. 2022)
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_sep_r1 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_sep_r1   <- c(          40,          190,          0.5,           25,       3372.8,       9900.3,       6319.5,  5.939917e13,            4,           36,            4,         1.60)
upper_mid_sep_r1 <- c(          80,          500,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_sep_r1 <- phenologyFitter(par.guess = par_mid_sep_r1,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_mid_sep,
                                   lower = lower_mid_sep_r1,
                                   upper = upper_mid_sep_r1,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_sep_r1[["bloomJDays"]],pheno_fit_mid_sep_r1[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_sep_r1 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_sep_r1

# Parameters of run 1
print(pheno_fit_mid_sep_r1$par)

# Model run number 2 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_sep_r2 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_sep_r2   <- c(7.714203e+01, 4.991033e+02, 1.519114e-01, 2.031061e+01, 3.018790e+03, 9.574556e+03, 6.003585e+03, 5.939920e+13, 1.921494e+00, 2.102967e+01, 9.910278e+00, 1.184017e+01)
upper_mid_sep_r2 <- c(          80,          800,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_sep_r2 <- phenologyFitter(par.guess = par_mid_sep_r2,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_mid_sep,
                                   lower = lower_mid_sep_r2,
                                   upper = upper_mid_sep_r2,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_sep_r2[["bloomJDays"]],pheno_fit_mid_sep_r2[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_sep_r2 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_sep_r2

# Parameters of run 2
print(pheno_fit_mid_sep_r2$par)

# Model run number 3 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_sep_r3 <- c(          35,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_sep_r3   <- c(7.770436e+01, 4.982428e+02, 1.808488e-01, 2.032804e+01, 3.018785e+03, 9.574506e+03, 6.077360e+03, 5.939943e+13, 6.449126e-02, 2.048773e+01, 9.910278e+00, 4.479715e+01)
upper_mid_sep_r3 <- c(          80,          800,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_sep_r3 <- phenologyFitter(par.guess = par_mid_sep_r3,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_mid_sep,
                                   lower = lower_mid_sep_r3,
                                   upper = upper_mid_sep_r3,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_sep_r3[["bloomJDays"]],pheno_fit_mid_sep_r3[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_sep_r3 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_sep_r3

# Parameters of run 3
print(pheno_fit_mid_sep_r3$par)

# Model run number 4 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_sep_r4 <- c(          35,          400,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_sep_r4   <- c(7.730929e+01, 4.915878e+02, 2.514717e-01, 2.034761e+01, 3.018749e+03, 9.572582e+03, 6.002775e+03, 5.939944e+13, 2.103182e+00, 2.039819e+01, 9.900203e+00, 7.015653e+00)
upper_mid_sep_r4 <- c(          90,          800,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_sep_r4 <- phenologyFitter(par.guess = par_mid_sep_r4,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_mid_sep,
                                   lower = lower_mid_sep_r4,
                                   upper = upper_mid_sep_r4,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_sep_r4[["bloomJDays"]],pheno_fit_mid_sep_r4[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_sep_r4 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_sep_r4

# Parameters of run 4
print(pheno_fit_mid_sep_r4$par)

# Model run number 5 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_sep_r5 <- c(          35,          400,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_sep_r5   <- c(7.767497e+01, 4.816284e+02, 2.285865e-01, 2.034709e+01, 3.018748e+03, 9.572594e+03, 6.002559e+03, 5.939944e+13, 3.940232e+00, 2.039741e+01, 9.900622e+00, 3.271631e+00)
upper_mid_sep_r5 <- c(          90,          600,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_sep_r5 <- phenologyFitter(par.guess = par_mid_sep_r5,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_mid_sep,
                                   lower = lower_mid_sep_r5,
                                   upper = upper_mid_sep_r5,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_sep_r5[["bloomJDays"]],pheno_fit_mid_sep_r5[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_sep_r5 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_sep_r5

# Parameters of run 5
print(pheno_fit_mid_sep_r5$par)

# Model run number 6 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_sep_r6 <- c(          50,          400,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_sep_r6   <- c(7.773930e+01, 4.835593e+02, 2.519998e-01, 2.034583e+01, 3.018786e+03, 9.572683e+03, 6.000737e+03, 5.939957e+13, 3.373700e+00, 2.041143e+01, 9.782484e+00, 1.049992e+00)
upper_mid_sep_r6 <- c(          80,          600,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_sep_r6 <- phenologyFitter(par.guess = par_mid_sep_r6,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_mid_sep,
                                   lower = lower_mid_sep_r6,
                                   upper = upper_mid_sep_r6,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_sep_r6[["bloomJDays"]],pheno_fit_mid_sep_r6[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_sep_r6 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_sep_r6

# Parameters of run 6
print(pheno_fit_mid_sep_r6$par)

# Model run number 7 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_sep_r7 <- c(          50,          400,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_sep_r7   <- c(7.689597e+01, 4.841338e+02, 2.871453e-01, 2.037987e+01, 3.018845e+03, 9.572697e+03, 6.000234e+03, 5.939933e+13, 3.300207e+00, 2.038608e+01, 9.770268e+00, 9.793320e-01)
upper_mid_sep_r7 <- c(          80,          600,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_sep_r7 <- phenologyFitter(par.guess = par_mid_sep_r7,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_mid_sep,
                                   lower = lower_mid_sep_r7,
                                   upper = upper_mid_sep_r7,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_sep_r7[["bloomJDays"]],pheno_fit_mid_sep_r7[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_sep_r7 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_sep_r7

# Parameters of run 7
print(pheno_fit_mid_sep_r7$par)

# Model run number 8 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_sep_r8 <- c(          60,          400,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_sep_r8   <- c(7.986098e+01, 4.741163e+02, 3.342946e-01, 2.037915e+01, 3.018829e+03, 9.572719e+03, 6.002320e+03, 5.939910e+13, 3.303438e+00, 2.038936e+01, 9.718286e+00, 1.103665e+00)
upper_mid_sep_r8 <- c(          90,          600,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_sep_r8 <- phenologyFitter(par.guess = par_mid_sep_r8,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_mid_sep,
                                   lower = lower_mid_sep_r8,
                                   upper = upper_mid_sep_r8,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_sep_r8[["bloomJDays"]],pheno_fit_mid_sep_r8[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_sep_r8 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_sep_r8

# Parameters of run 8
print(pheno_fit_mid_sep_r8$par)

# Model run number 9 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_sep_r9 <- c(          60,          400,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_sep_r9   <- c(8.382472e+01, 4.786254e+02, 9.972873e-01, 2.037451e+01, 3.018811e+03, 9.572721e+03, 6.001570e+03, 5.939922e+13, 2.745696e+00, 2.038836e+01, 9.528820e+00, 1.441986e+00)
upper_mid_sep_r9 <- c(          90,          600,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_sep_r9 <- phenologyFitter(par.guess = par_mid_sep_r9,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_mid_sep,
                                   lower = lower_mid_sep_r9,
                                   upper = upper_mid_sep_r9,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_sep_r9[["bloomJDays"]],pheno_fit_mid_sep_r9[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_sep_r9 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_sep_r9

# Parameters of run 9
print(pheno_fit_mid_sep_r9$par)

# Model run number 10 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_sep_r10 <- c(          60,          400,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_sep_r10   <- c(8.458243e+01, 4.760414e+02, 9.941974e-01, 2.036024e+01, 3.018809e+03, 9.572723e+03, 6.001935e+03, 5.939946e+13, 2.778296e+00, 2.039518e+01, 9.530142e+00, 1.638449e+00)
upper_mid_sep_r10 <- c(          90,          600,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_sep_r10 <- phenologyFitter(par.guess = par_mid_sep_r10,
                                   modelfn = PhenoFlex_GDHwrapper,
                                   bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                   SeasonList = season_list_mid_sep,
                                   lower = lower_mid_sep_r10,
                                   upper = upper_mid_sep_r10,
                                   control = list(smooth = FALSE,
                                                  verbose = FALSE,
                                                  maxit = 1000,
                                                  nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_sep_r10[["bloomJDays"]],pheno_fit_mid_sep_r10[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_sep_r10 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_sep_r10

# Parameters of run 10
print(pheno_fit_mid_sep_r10$par)
