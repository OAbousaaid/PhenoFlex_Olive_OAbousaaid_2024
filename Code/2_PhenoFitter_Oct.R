# Calibration starting from October 1st ####

# Define the list of seasons (weather data) the season list from October to June
season_list_oct <- genSeasonList(data, mrange = c(10, 6), years = calibration_seasons)

# Model run number 1 ####
# Set the initial parameters (as in Fernandez et al. 2022)
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_oct_r1 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_oct_r1   <- c(          40,          190,          0.5,           25,       3372.8,       9900.3,       6319.5,  5.939917e13,            4,           36,            4,         1.60)
upper_oct_r1 <- c(          80,          500,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_oct_r1 <- phenologyFitter(par.guess = par_oct_r1,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_oct,
                                    lower = lower_oct_r1,
                                    upper = upper_oct_r1,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_oct_r1[["bloomJDays"]],pheno_fit_oct_r1[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_oct_r1 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_oct_r1

# Parameters run 1
print(pheno_fit_oct_r1$par)

# Model run number 2 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_oct_r2 <- c(          35,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_oct_r2   <- c(7.714203e+01, 4.991033e+02, 1.519114e-01, 2.031061e+01, 3.018790e+03, 9.574556e+03, 6.003585e+03, 5.939920e+13, 1.921494e+00, 2.102967e+01, 9.910278e+00, 1.184017e+01)
upper_oct_r2 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)


# Run the fitter
pheno_fit_oct_r2 <- phenologyFitter(par.guess = par_oct_r2,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_oct,
                                    lower = lower_oct_r2,
                                    upper = upper_oct_r2,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_oct_r2[["bloomJDays"]],pheno_fit_oct_r2[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_oct_r2 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_oct_r2

# Parameters run 2
print(pheno_fit_oct_r2$par)

# Model run number 3 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_oct_r3 <- c(          35,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_oct_r3   <- c(7.701704e+01, 4.859188e+02, 1.809264e-01, 2.031805e+01, 3.018703e+03, 9.574662e+03, 6.072991e+03, 5.939918e+13, 1.987118e+00, 2.049738e+01, 9.909480e+00, 6.054206e-01)
upper_oct_r3 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)


# Run the fitter
pheno_fit_oct_r3 <- phenologyFitter(par.guess = par_oct_r3,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_oct,
                                    lower = lower_oct_r3,
                                    upper = upper_oct_r3,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_oct_r3[["bloomJDays"]],pheno_fit_oct_r3[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_oct_r3 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_oct_r3

# Parameters run 3
print(pheno_fit_oct_r3$par)

# Model run number 4 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_oct_r4 <- c(          35,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_oct_r4   <- c(7.744436e+01, 8.850144e+02, 9.967620e-01, 1.537963e+01, 3.015069e+03, 9.574451e+03, 6.052368e+03, 5.939901e+13, 6.236894e+00, 3.408449e+01, 9.864402e+00, 6.498654e+00)
upper_oct_r4 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)


# Run the fitter
pheno_fit_oct_r4 <- phenologyFitter(par.guess = par_oct_r4,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_oct,
                                    lower = lower_oct_r4,
                                    upper = upper_oct_r4,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_oct_r4[["bloomJDays"]],pheno_fit_oct_r4[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_oct_r4 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_oct_r4

# Parameters run 4
print(pheno_fit_oct_r4$par)

# Model run number 5 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_oct_r5 <- c(          35,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_oct_r5   <- c(7.658503e+01, 9.308682e+02, 9.994421e-01, 1.418271e+01, 3.015060e+03, 9.575011e+03, 6.167086e+03, 5.939917e+13, 3.899480e+00, 3.444386e+01, 9.847159e+00, 7.435658e+00)
upper_oct_r5 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)


# Run the fitter
pheno_fit_oct_r5 <- phenologyFitter(par.guess = par_oct_r5,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_oct,
                                    lower = lower_oct_r5,
                                    upper = upper_oct_r5,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_oct_r5[["bloomJDays"]],pheno_fit_oct_r5[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_oct_r5 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_oct_r5

# Parameters run 1
print(pheno_fit_oct_r5$par)

# Model run number 6 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_oct_r6 <- c(          35,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_oct_r6   <- c(7.658503e+01, 9.308682e+02,          0.7,           25, 3.015060e+03, 9.575011e+03, 6.167086e+03, 5.939917e+13,            6, 3.444386e+01, 9.847159e+00,          0.5)
upper_oct_r6 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)


# Run the fitter
pheno_fit_oct_r6 <- phenologyFitter(par.guess = par_oct_r6,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_oct,
                                    lower = lower_oct_r6,
                                    upper = upper_oct_r6,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_oct_r6[["bloomJDays"]],pheno_fit_oct_r6[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_oct_r6 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_oct_r6

# Parameters run 6
print(pheno_fit_oct_r6$par)

# Model run number 7 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_oct_r7 <- c(          35,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_oct_r7   <- c(8.981207e+01, 8.653696e+02, 1.477996e-01, 1.451223e+01, 3.014331e+03, 9.577812e+03, 6.251858e+03, 5.939928e+13, 1.040369e+00, 3.730843e+01, 1.031646e+01, 1.341759e+00)
upper_oct_r7 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.1e13,           10,           40,           12,        50.00)


# Run the fitter
pheno_fit_oct_r7 <- phenologyFitter(par.guess = par_oct_r7,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_oct,
                                    lower = lower_oct_r7,
                                    upper = upper_oct_r7,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_oct_r7[["bloomJDays"]],pheno_fit_oct_r7[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_oct_r7 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_oct_r7

# Parameters run 7
print(pheno_fit_oct_r7$par)

# Model run number 8 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_oct_r8 <- c(          35,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_oct_r8   <- c(8.816703e+01, 9.342305e+02, 1.996065e-01, 1.339354e+01, 3.014311e+03, 9.577907e+03, 6.255422e+03, 5.939930e+13, 5.980466e+00, 3.901053e+01, 1.036401e+01, 2.811085e+00)
upper_oct_r8 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,       6.1e13,           10,           40,           12,        50.00)


# Run the fitter
pheno_fit_oct_r8 <- phenologyFitter(par.guess = par_oct_r8,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_oct,
                                    lower = lower_oct_r8,
                                    upper = upper_oct_r8,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_oct_r8[["bloomJDays"]],pheno_fit_oct_r8[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_oct_r8 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_oct_r8

# Parameters run 8
print(pheno_fit_oct_r8$par)

# Mid October ####

# Julian day to date and vice-versa
as.Date(305, format="%d.%m",origin = "01.01")-1
as.POSIXlt(c("15.10"), format = "%d.%m")$yday+1

# Select season list
season_list_mid_oct <- list()

for (i in c(1:23)) {
  num_rows <- nrow(season_list_oct[[i]])
  season_list_mid_oct[[i]] <- season_list_oct[[i]][360:num_rows,]
}

# Model run number 1 ####
# Set the initial parameters (as in Fernandez et al. 2022)
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_oct_r1 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_oct_r1   <- c(          40,          190,          0.5,           25,       3372.8,       9900.3,       6319.5,  5.939917e13,            4,           36,            4,         1.60)
upper_mid_oct_r1 <- c(          80,          500,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_oct_r1 <- phenologyFitter(par.guess = par_mid_oct_r1,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_oct,
                                        lower = lower_mid_oct_r1,
                                        upper = upper_mid_oct_r1,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_oct_r1[["bloomJDays"]],pheno_fit_mid_oct_r1[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_oct_r1 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_oct_r1

# Parameters run 1
print(pheno_fit_mid_oct_r1$par)

# Model run number 2 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_oct_r2 <- c(          30,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_oct_r2   <- c(7.664286e+01, 4.980208e+02, 1.148855e-01, 2.377621e+01, 3.018898e+03, 9.566925e+03, 6.077046e+03, 5.939899e+13, 9.954827e+00, 2.400808e+01, 9.319697e+00, 5.735461e+00)
upper_mid_oct_r2 <- c(          90,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,       6.1e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_oct_r2 <- phenologyFitter(par.guess = par_mid_oct_r2,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_oct,
                                        lower = lower_mid_oct_r2,
                                        upper = upper_mid_oct_r2,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_oct_r2[["bloomJDays"]],pheno_fit_mid_oct_r2[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_oct_r2 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_oct_r2

# Parameters run 2
print(pheno_fit_mid_oct_r2$par)

# Model run number 3 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_oct_r3 <- c(          30,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_oct_r3   <- c(6.919196e+01, 7.168444e+02, 9.189908e-01, 1.533331e+01, 3.015807e+03, 9.566196e+03, 6.050114e+03, 5.939941e+13, 3.583329e+00, 2.826930e+01, 1.071408e+01, 1.945699e+00)
upper_mid_oct_r3 <- c(          90,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,       6.1e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_oct_r3 <- phenologyFitter(par.guess = par_mid_oct_r3,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_oct,
                                        lower = lower_mid_oct_r3,
                                        upper = upper_mid_oct_r3,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_oct_r3[["bloomJDays"]],pheno_fit_mid_oct_r3[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_oct_r3 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_oct_r3

# Parameters run 3
print(pheno_fit_mid_oct_r3$par)

# Model run number 4 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_oct_r4 <- c(          30,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_oct_r4   <- c(6.949869e+01, 7.157134e+02, 9.995442e-01, 1.513692e+01, 3.015706e+03, 9.567521e+03, 6.023963e+03, 5.939955e+13, 3.604094e+00, 2.827690e+01, 1.072596e+01, 1.781976e+00)
upper_mid_oct_r4 <- c(          90,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,       6.1e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_oct_r4 <- phenologyFitter(par.guess = par_mid_oct_r4,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_oct,
                                        lower = lower_mid_oct_r4,
                                        upper = upper_mid_oct_r4,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_oct_r4[["bloomJDays"]],pheno_fit_mid_oct_r4[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_oct_r4 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_oct_r4

# Parameters run 4
print(pheno_fit_mid_oct_r4$par)

# Model run number 5 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_oct_r5 <- c(          30,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_oct_r5   <- c(          60, 7.157134e+02,          0.7,           24, 3.015706e+03, 9.567521e+03, 6.023963e+03, 5.939955e+13,            6,           36, 1.072596e+01,          0.5)
upper_mid_oct_r5 <- c(          90,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,       6.1e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_oct_r5 <- phenologyFitter(par.guess = par_mid_oct_r5,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_oct,
                                        lower = lower_mid_oct_r5,
                                        upper = upper_mid_oct_r5,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_oct_r5[["bloomJDays"]],pheno_fit_mid_oct_r5[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_oct_r5 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_oct_r5

# Parameters run 5
print(pheno_fit_mid_oct_r5$par)

# Model run number 6 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_oct_r6 <- c(          30,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_oct_r6   <- c(8.993634e+01, 8.951789e+02, 1.068370e-01, 1.409525e+01, 3.189437e+03, 9.856664e+03, 6.047307e+03, 5.939981e+13, 9.391879e+00, 3.676072e+01, 9.107698e+00, 3.123263e+00)
upper_mid_oct_r6 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,       6.1e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_oct_r6 <- phenologyFitter(par.guess = par_mid_oct_r6,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_oct,
                                        lower = lower_mid_oct_r6,
                                        upper = upper_mid_oct_r6,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_oct_r6[["bloomJDays"]],pheno_fit_mid_oct_r6[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_oct_r6 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_oct_r6

# Parameters run 6
print(pheno_fit_mid_oct_r6$par)

# Model run number 7 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_oct_r7 <- c(          30,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_oct_r7   <- c(8.993649e+01, 9.423304e+02, 1.205800e-01, 1.384822e+01, 3.188388e+03, 9.856600e+03, 6.067031e+03, 5.939981e+13, 9.357676e+00, 3.637228e+01, 8.985160e+00, 3.649407e+00)
upper_mid_oct_r7 <- c(          90,         1000,          1.0,           30,       4000.0,      10000.0,       7000.0,       6.1e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_oct_r7 <- phenologyFitter(par.guess = par_mid_oct_r7,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_oct,
                                        lower = lower_mid_oct_r7,
                                        upper = upper_mid_oct_r7,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_oct_r7[["bloomJDays"]],pheno_fit_mid_oct_r7[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_oct_r7 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_oct_r7

# Parameters run 7
print(pheno_fit_mid_oct_r7$par)


# Model run number 8 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_oct_r8 <- c(          30,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_oct_r8   <- c(          60, 7.157134e+02,          0.1,           24, 3.015706e+03, 9.567521e+03, 6.023963e+03, 5.939955e+13,            6,           36, 1.072596e+01,          0.5)
upper_mid_oct_r8 <- c(          90,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,       6.1e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_mid_oct_r8 <- phenologyFitter(par.guess = par_mid_oct_r8,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_oct,
                                        lower = lower_mid_oct_r8,
                                        upper = upper_mid_oct_r8,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_oct_r8[["bloomJDays"]],pheno_fit_mid_oct_r8[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_oct_r8 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_oct_r8

# Parameters run 8
print(pheno_fit_mid_oct_r8$par)
