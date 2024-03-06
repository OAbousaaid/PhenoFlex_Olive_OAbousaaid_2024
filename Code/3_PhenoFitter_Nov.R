# Calibration starting from November 1st ####

# Define the list of seasons (weather data) the season list from November to June
season_list_nov <- genSeasonList(data, mrange = c(11, 6), years = calibration_seasons)

# Model run number 1 ####
# Set the initial parameters (as in Fernandez et al. 2022)
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_nov_r1 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_nov_r1   <- c(          40,          190,          0.5,           25,       3372.8,       9900.3,       6319.5,  5.939917e13,            4,           36,            4,         1.60)
upper_nov_r1 <- c(          80,          500,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_nov_r1 <- phenologyFitter(par.guess = par_nov_r1,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_nov,
                                    lower = lower_nov_r1,
                                    upper = upper_nov_r1,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_nov_r1[["bloomJDays"]],pheno_fit_nov_r1[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_nov_r1 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_nov_r1

# Parameters run 1
print(pheno_fit_nov_r1$par)

# Model run number 2 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_nov_r2 <- c(          30,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_nov_r2   <- c(6.620250e+01, 4.997037e+02, 1.003876e-01, 2.344190e+01, 3.018789e+03, 9.554174e+03, 6.106021e+03, 5.939894e+13, 3.350922e+00, 2.354325e+01, 9.222279e+00, 1.094037e+00)
upper_nov_r2 <- c(          80,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_nov_r2 <- phenologyFitter(par.guess = par_nov_r2,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_nov,
                                    lower = lower_nov_r2,
                                    upper = upper_nov_r2,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_nov_r2[["bloomJDays"]],pheno_fit_nov_r2[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_nov_r2 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_nov_r2

# Parameters run 2
print(pheno_fit_nov_r2$par)

# Model run number 3 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_nov_r3 <- c(          30,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_nov_r3   <- c(6.022382e+01, 6.715036e+02, 9.999381e-01, 1.560144e+01, 3.019010e+03, 9.563409e+03, 6.051584e+03, 5.939886e+13, 5.232317e+00, 2.813518e+01, 1.127393e+01, 1.614413e+00)
upper_nov_r3 <- c(          80,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_nov_r3 <- phenologyFitter(par.guess = par_nov_r3,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_nov,
                                    lower = lower_nov_r3,
                                    upper = upper_nov_r3,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_nov_r3[["bloomJDays"]],pheno_fit_nov_r3[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_nov_r3 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_nov_r3

# Parameters run 3
print(pheno_fit_nov_r3$par)

# Model run number 4 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_nov_r4 <- c(          30,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_nov_r4   <- c(6.235374e+01, 6.715215e+02, 9.717910e-01, 1.531224e+01, 3.018994e+03, 9.563447e+03, 6.041367e+03, 5.939846e+13, 5.227456e+00, 2.820263e+01, 1.124048e+01, 1.824427e+00)
upper_nov_r4 <- c(          80,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_nov_r4 <- phenologyFitter(par.guess = par_nov_r4,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_nov,
                                    lower = lower_nov_r4,
                                    upper = upper_nov_r4,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_nov_r4[["bloomJDays"]],pheno_fit_nov_r4[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_nov_r4 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_nov_r4

# Parameters run 4
print(pheno_fit_nov_r4$par)

# Mid November ####

# Julian day to date and vice-versa
as.POSIXlt(c("15.11"), format = "%d.%m")$yday+1
as.Date(319, format="%d.%m",origin = "01.01")-1

# Select season list

season_list_mid_nov <- list()

for (i in c(1:23)) {
  num_rows <- nrow(season_list_nov[[i]])
  season_list_mid_nov[[i]] <- season_list_nov[[i]][337:num_rows,]
}

# Model run number 1 ####
# Set the initial parameters (as in Fernandez et al. 2022)
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_nov_r1 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_nov_r1   <- c(          40,          190,          0.5,           25,       3372.8,       9900.3,       6319.5,  5.939917e13,            4,           36,            4,         1.60)
upper_mid_nov_r1 <- c(          80,          500,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_nov_r1 <- phenologyFitter(par.guess = par_mid_nov_r1,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_mid_nov,
                                    lower = lower_mid_nov_r1,
                                    upper = upper_mid_nov_r1,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_nov_r1[["bloomJDays"]],pheno_fit_mid_nov_r1[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_nov_r1 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_nov_r1

# Parameters run 1
print(pheno_fit_mid_nov_r1$par)

# Model run number 2 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_nov_r2 <- c(          30,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_nov_r2   <- c(7.866163e+01, 4.909924e+02, 9.908302e-01, 1.932525e+01, 3.018791e+03, 9.569415e+03, 6.049487e+03, 5.939875e+13, 3.113736e+00, 2.164900e+01, 9.583790e+00, 9.204411e-01)
upper_mid_nov_r2 <- c(          90,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_nov_r2 <- phenologyFitter(par.guess = par_mid_nov_r2,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_nov,
                                        lower = lower_mid_nov_r2,
                                        upper = upper_mid_nov_r2,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_nov_r2[["bloomJDays"]],pheno_fit_mid_nov_r2[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_nov_r2 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_nov_r2

# Parameters run 2
print(pheno_fit_mid_nov_r2$par)

