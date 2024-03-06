# Calibration starting from the end of December  (pheno_fit_fin_dec) ####

# Julian day to date and vice-versa
as.POSIXlt(c("31.12"), format = "%d.%m")$yday+1
as.Date(365, format="%d.%m",origin = "01.01")-1

# The season list; selected from the season list of December
season_list_fin_dec <- list()

for (i in c(1:23)) {
  num_rows <- nrow(season_list_dec[[i]])
  season_list_fin_dec[[i]] <- season_list_dec[[i]][744:num_rows,]
}

# Model run number 1 ####
# Set the initial parameters (as in Fernandez et al. 2022)
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_fin_dec_r1 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_fin_dec_r1   <- c(          40,          190,          0.5,           25,       3372.8,       9900.3,       6319.5,  5.939917e13,            4,           36,            4,         1.60)
upper_fin_dec_r1 <- c(          80,          500,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_fin_dec_r1 <- phenologyFitter(par.guess = par_fin_dec_r1,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_fin_dec,
                                        lower = lower_fin_dec_r1,
                                        upper = upper_fin_dec_r1,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_fin_dec_r1[["bloomJDays"]],pheno_fit_fin_dec_r1[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_fin_dec_r1 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_fin_dec_r1

# Parameters of run 1
print(pheno_fit_fin_dec_r1$par)

# Model run number 2 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_fin_dec_r2 <- c(          10,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_fin_dec_r2   <- c(6.860910e+01, 3.915166e+02, 1.457394e-01, 2.380333e+01, 3.235607e+03, 9.866137e+03, 6.686837e+03, 5.939889e+13, 7.709898e+00, 2.406118e+01, 7.008824e+00, 9.409489e+00)
upper_fin_dec_r2 <- c(          80,          500,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_fin_dec_r2 <- phenologyFitter(par.guess = par_fin_dec_r2,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_fin_dec,
                                        lower = lower_fin_dec_r2,
                                        upper = upper_fin_dec_r2,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_fin_dec_r2[["bloomJDays"]],pheno_fit_fin_dec_r2[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_fin_dec_r2 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_fin_dec_r2

# Parameters of run 2
print(pheno_fit_fin_dec_r2$par)

# Model run number 3 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_fin_dec_r3 <- c(          30,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_fin_dec_r3   <- c(6.898423e+01, 3.932507e+02, 1.066405e-01, 2.838031e+01, 3.235599e+03, 9.866186e+03, 6.647352e+03, 5.939901e+13, 6.149617e+00, 2.958047e+01, 5.238026e+00, 9.419286e+00)
upper_fin_dec_r3 <- c(          80,          600,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_fin_dec_r3 <- phenologyFitter(par.guess = par_fin_dec_r3,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_fin_dec,
                                        lower = lower_fin_dec_r3,
                                        upper = upper_fin_dec_r3,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_fin_dec_r3[["bloomJDays"]],pheno_fit_fin_dec_r3[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_fin_dec_r3 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_fin_dec_r3

# Parameters of run 3
print(pheno_fit_fin_dec_r3$par)

# Model run number 4 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_fin_dec_r4 <- c(          30,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_fin_dec_r4   <- c(6.200543e+01, 4.424122e+02, 9.977147e-01, 2.585525e+01, 3.295992e+03, 9.948174e+03, 6.650084e+03, 5.939882e+13, 5.730722e+00, 3.017838e+01, 5.239990e+00, 3.989417e+00)
upper_fin_dec_r4 <- c(          80,          600,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           10,        50.00)

# Run the fitter
pheno_fit_fin_dec_r4 <- phenologyFitter(par.guess = par_fin_dec_r4,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_fin_dec,
                                        lower = lower_fin_dec_r4,
                                        upper = upper_fin_dec_r4,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_fin_dec_r4[["bloomJDays"]],pheno_fit_fin_dec_r4[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_fin_dec_r4 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_fin_dec_r4

# Parameters of run 4
print(pheno_fit_fin_dec_r4$par)

# Model run number 5 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_fin_dec_r5 <- c(          30,          100,          0.1,            0,       3000.0,       9000.0,       6000.0,       5.0e13,            0,            0,            0,         0.05)
par_fin_dec_r5   <- c(          50,          500,          0.7, 2.585525e+01, 3.295992e+03, 9.948174e+03, 6.650084e+03, 5.939882e+13, 5.730722e+00, 3.017838e+01, 5.239990e+00,          0.5)
upper_fin_dec_r5 <- c(          80,          800,          1.0,           30,       4000.0,      10000.0,       7000.0,        6.e13,           10,           40,           12,        50.00)

# Run the fitter
pheno_fit_fin_dec_r5 <- phenologyFitter(par.guess = par_fin_dec_r5,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_fin_dec,
                                        lower = lower_fin_dec_r5,
                                        upper = upper_fin_dec_r5,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_fin_dec_r5[["bloomJDays"]],pheno_fit_fin_dec_r5[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_fin_dec_r5 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_fin_dec_r5

# Parameters of run 5
print(pheno_fit_fin_dec_r5$par)

# Model run number 6 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_fin_dec_r6 <- c(          40,          400,          0.1,           20,       3000.0,       9000.0,       6000.0,       5.0e13,            5,           25,            1,         0.05)
par_fin_dec_r6   <- c(7.996566e+01, 5.478293e+02, 2.035730e-01, 2.738509e+01, 3.239017e+03, 9.923085e+03, 6.817478e+03, 5.939880e+13, 7.993613e+00, 3.185238e+01, 1.924340e+00, 1.148130e+01)
upper_fin_dec_r6 <- c(          90,          800,          0.8,           30,       4000.0,      10000.0,       7000.0,        6.e13,            8,           36,           12,        50.00)

# Run the fitter
pheno_fit_fin_dec_r6 <- phenologyFitter(par.guess = par_fin_dec_r6,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_fin_dec,
                                        lower = lower_fin_dec_r6,
                                        upper = upper_fin_dec_r6,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_fin_dec_r6[["bloomJDays"]],pheno_fit_fin_dec_r6[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_fin_dec_r6 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_fin_dec_r6

# Parameters of run 6
print(pheno_fit_fin_dec_r6$par)

# Model run number 7 ####
#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_fin_dec_r7 <- c(          60,          400,          0.2,           20,       3000.0,       9000.0,       6000.0,       5.0e13,            5,           25,          2.5,         0.05)
par_fin_dec_r7   <- c(8.283128e+01, 4.876031e+02, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939905e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
upper_fin_dec_r7 <- c(          90,          800,          0.8,           30,       4000.0,      10000.0,       7000.0,        6.e13,            8,           36,           12,        50.00)

# Run the fitter
pheno_fit_fin_dec_r7 <- phenologyFitter(par.guess = par_fin_dec_r7,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_fin_dec,
                                        lower = lower_fin_dec_r7,
                                        upper = upper_fin_dec_r7,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# Sortie RMSE
bloom_data <- data.frame(calibration_seasons,pheno_fit_fin_dec_r7[["bloomJDays"]],pheno_fit_fin_dec_r7[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_fin_dec_r7 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_fin_dec_r7

# MAE
Observed <- c(bloom_data$pheno)
predicted <- c(bloom_data$Predicted)
mae_calib <- mean(abs(Observed - predicted))

# Parameters of run 7
print(pheno_fit_fin_dec_r7$par)

# Model run number 8 ####

#                               yc,           zc,           s1,           Tu,           E0,           E1,           A0,           A1,           Tf,           Tc,           Tb,        slope
lower_fin_dec_r8 <- c(          50,          480,          0.5,           20,       3000.0,       9000.0,       6000.0,       5.0e13,            5,           25,            2,          0.5)
par_fin_dec_r8   <- c(8.283128e+01, 4.876031e+02, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
upper_fin_dec_r8 <- c(          85,          800,          0.9,           30,       4000.0,      10000.0,       7000.0,        6.e13,            8,           36,            5,            6)

# Run the fitter
pheno_fit_fin_dec_r8 <- phenologyFitter(par.guess = par_fin_dec_r8,
                                        modelfn = PhenoFlex_GDHwrapper,
                                        bloomJDays = pheno_v1[pheno_v1$Year %in% calibration_seasons, "pheno"],
                                        SeasonList = season_list_fin_dec,
                                        lower = lower_fin_dec_r8,
                                        upper = upper_fin_dec_r8,
                                        control = list(smooth = FALSE,
                                                       verbose = FALSE,
                                                       maxit = 1000,
                                                       nb.stop.improvement = 250))

# Some intermediate results
# RMSEP
bloom_data <- data.frame(calibration_seasons,pheno_fit_fin_dec_r8[["bloomJDays"]],pheno_fit_fin_dec_r8[["pbloomJDays"]])
colnames(bloom_data) <- c("Year","pheno","Predicted")


RMSEP_fin_dec_r8 <- RMSEP(bloom_data$Predicted, bloom_data$pheno, na.rm = TRUE)
RMSEP_fin_dec_r8

# Parameters of run 8
print(pheno_fit_fin_dec_r8$par)
