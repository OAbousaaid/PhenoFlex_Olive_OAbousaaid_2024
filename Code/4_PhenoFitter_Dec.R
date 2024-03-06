# Calibration starting from 1st December ####

# Define the list of seasons (weather data) the season list from December to June
season_list_dec <- genSeasonList(data, mrange = c(12, 6), years = calibration_seasons)

# Model run number 1 ####
# Set the initial parameters (as in Fernandez et al. 2022)
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_dec_r1 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_dec_r1   <- c(          40,          190,          0.5,           25,       3372.8,       9900.3,       6319.5,  5.939917e13,            4,           36,            4,         1.60)
upper_dec_r1 <- c(          80,          500,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_dec_r1 <- phenologyFitter(par.guess = par_dec_r1,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_dec,
                                    lower = lower_dec_r1,
                                    upper = upper_dec_r1,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_dec_r1[["bloomJDays"]],pheno_fit_dec_r1[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_dec_r1 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_dec_r1

# Parameters run 1
print(pheno_fit_dec_r1$par)

# Model run number 2 ####
#                           yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_dec_r2 <- c(          30,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_dec_r2   <- c(6.455022e+01, 3.960465e+02, 1.756409e-01, 2.015256e+01, 3.237142e+03, 9.867729e+03, 6.066024e+03, 5.939914e+13, 3.065704e+00, 2.017223e+01, 9.684955e+00, 3.303093e+00)
upper_dec_r2 <- c(          90,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_dec_r2 <- phenologyFitter(par.guess = par_dec_r2,
                                    modelfn = PhenoFlex_GDHwrapper,
                                    bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                    SeasonList = season_list_dec,
                                    lower = lower_dec_r2,
                                    upper = upper_dec_r2,
                                    control = list(smooth = FALSE,
                                                   verbose = FALSE,
                                                   maxit = 1000,
                                                   nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_dec_r2[["bloomJDays"]],pheno_fit_dec_r2[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_dec_r2 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_dec_r2

# Parameters run 2
print(pheno_fit_dec_r2$par)

# Calibration mid December ####

# Julian day to date and vice-versa
as.POSIXlt(c("15.12"), format = "%d.%m")$yday+1
as.Date(349, format="%d.%m",origin = "01.01")-1

season_list_mid_dec <- list()

for (i in c(1:23)) {
  num_rows <- nrow(season_list_dec[[i]])
  season_list_mid_dec[[i]] <- season_list_dec[[i]][337:num_rows,]
}

# Model run number 1 ####
# Set the initial parameters (as in Fernandez et al. 2022)
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_dec_r1 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_dec_r1   <- c(          40,          190,          0.5,           25,       3372.8,       9900.3,       6319.5,  5.939917e13,            4,           36,            4,         1.60)
upper_mid_dec_r1 <- c(          80,          500,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_dec_r1 <- phenologyFitter(par.guess = par_mid_dec_r1,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_dec,
                                        lower = lower_mid_dec_r1,
                                        upper = upper_mid_dec_r1,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_dec_r1[["bloomJDays"]],pheno_fit_mid_dec_r1[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_dec_r1 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_dec_r1

# Parameters run 1
print(pheno_fit_mid_dec_r1$par)

# Model run number 2 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_mid_dec_r2 <- c(          30,          300,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_mid_dec_r2   <- c(7.348479e+01, 3.970298e+02, 1.737400e-01, 2.026085e+01, 3.235121e+03, 9.874580e+03, 6.415730e+03, 5.939894e+13, 3.969746e+00, 2.083792e+01, 8.732164e+00, 1.608930e+01)
upper_mid_dec_r2 <- c(          90,          900,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_mid_dec_r2 <- phenologyFitter(par.guess = par_mid_dec_r2,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_mid_dec,
                                        lower = lower_mid_dec_r2,
                                        upper = upper_mid_dec_r2,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_mid_dec_r2[["bloomJDays"]],pheno_fit_mid_dec_r2[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_mid_dec_r2 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_mid_dec_r2

# Parameters run 2
print(pheno_fit_mid_dec_r2$par)
