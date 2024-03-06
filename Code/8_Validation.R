# This code will be for the validation, the boot strap, the plots of observed vs predicted 
# and the temperature response plot.

# After calibration with different date I got the best calibration with the model starting from
# the end of December.

# Validation Version 1; from the first level of calibration without forcing
# Set the validation seasons
validation_seasons <- sort(pheno_v1[!(pheno_v1$Year %in% calibration_seasons), "Year"])

# Now i will generate a validation data set with phenology data for six years of validation (2014_2019)
valid_df_v1 <- pheno_v1[pheno_v1$Year %in% validation_seasons[1:6], ]

# Then I generate a list of seasons with weather data for the validation procedure
valid_season_list <- genSeasonList(data, mrange = c(12, 6), years = validation_seasons[1:6])

# Season list selection starting from the end of December
valid_season_list_fin_dec <- list()

for (i in c(1:6)) {
  num_rows <- nrow(valid_season_list[[i]])
  valid_season_list_fin_dec[[i]] <- valid_season_list[[i]][744:num_rows,]
}

# Estimate the bloom dates with PhenoFlexGDHwrapper
for (i in 1 : nrow(valid_df_v1)) {
  
  valid_df_v1[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_fin_dec[[i]], pheno_fit_fin_dec_r7$par)
}

# Compute the error (observed - predicted)
valid_df_v1[["Error"]] <- valid_df_v1$pheno - valid_df_v1$Predicted

# Estimate the RMSEP
RMSEP_valid_v1 <- RMSEP(valid_df_v1$Predicted, valid_df_v1$pheno, na.rm = TRUE)
RMSEP_valid_v1

# Compute the MAE
mae_valid_v1 <- mean(abs(c(valid_df_v1$pheno) - c(valid_df_v1$Predicted)))

# Validation after calibration with forcing tests

pheno_fit_fin_dec_r8$par <- c(58, 785, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
saveRDS(pheno_fit_fin_dec_r8, file = "PhenFlex_olive.rds")

# Validation data set version 2
valid_df_V2 <- pheno_v1[pheno_v1$Year %in% validation_seasons[1:6], ]

# Estimate the bloom dates with PhenoFlexGDHwrapper
for (i in 1 : nrow(valid_df_V2)) {
  
  valid_df_V2[i, "Predicted"] <- PhenoFlex_GDHwrapper(valid_season_list_fin_dec[[i]], pheno_fit_fin_dec_r8$par)
}

# Compute the error (observed - predicted)
valid_df_V2[["Error"]] <- valid_df_V2$pheno - valid_df_V2$Predicted

# Estimate the RMSEP
RMSEP_valid_v2 <- RMSEP(valid_df_V2$Predicted, valid_df_V2$pheno, na.rm = TRUE)
RMSEP_valid_v2

# Compute the MAE
mae_valid_v2 <- mean(abs(c(valid_df_V2$pheno) - c(valid_df_V2$Predicted)))
mae_valid_v2

# After extensive procedure of calibration and validation, We are moving forward to plot the data and
# to Bootstrap to get the error

out_df_fin_dec_r7 <- pheno_v1[pheno_v1$Year %in% calibration_seasons, ]

# Estimate the bloom dates with PhenoFlexGDHwrapper
for (i in 1 : nrow(out_df_fin_dec_r7)) {
  
  out_df_fin_dec_r7[i, "Predicted"] <- PhenoFlex_GDHwrapper(season_list_fin_dec[[i]], pheno_fit_fin_dec_r7$par)
}

# Plot the calibrated and validated 
ggplot(out_df_fin_dec_r7, aes(pheno, Predicted)) +
  geom_point(shape = 1, size = 2) +
  geom_point(data = valid_df_V2, aes(pheno, Predicted, color = "Validation seasons"),
             size = 2.5) +
  scale_color_manual(values = c("firebrick", "cadetblue")) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = "Observed",
       colour = "") +
  theme_bw()

# Bootstrap ####
# The bootstrap of residuals is implemented following the same procedure by Fernandez et al., 2022 and the PhenoFlex vignette. 

#          yc,  zc,  s1, Tu,     E0,      E1,     A0,          A1,   Tf, Tc, Tb, slope
lower <- c(20, 100, 0.1,  0, 3000.0,  9000.0, 6000.0,       5.e13,    0,  0,  0,  0.05)
upper <- c(90, 800, 1.0, 30, 4000.0, 10000.0, 7000.0,       6.e13,   10, 40, 12, 50.00)

# Bootstrap the calibration fitting to obtain boot.R sets of parameters
fit_res_boot <- bootstrap.phenologyFit(pheno_fit_fin_dec_r8,
                                          boot.R = 10,
                                          lower = lower,
                                          upper = upper,
                                          control = list(smooth = FALSE,
                                                         verbose = TRUE,
                                                         maxit = 1000,
                                                         nb.stop.improvement = 10))

# Take some quick look at the outputs
summary(fit_res_boot)

# Plot the bootstrap element
plot(fit_res_boot)

# Implement a for loop to compute the bloom dates based on the different sets of parameters from the bootstrap element.
# This loop will add the date and boot.R column to the validation data frame
for (k in 1 : length(fit_res_boot$res)) {
  
  par_k <- fit_res_boot$res[[k]]$par
  
  name <- paste0("Pred_Boot_", k)
  
  for (i in 1 : nrow(valid_df_V2)) {
    
    valid_df_V2[i, name] <- PhenoFlex_GDHwrapper(valid_season_list_fin_dec[[i]], par_k)
  }
}

# Compute the sd across boot.R bloom dates
valid_df_V2 <- valid_df_V2 %>% 
  pivot_longer(starts_with("Pred_Boot_")) %>% 
  group_by(Year, pheno, Predicted, Error) %>% 
  summarise(SD_boot = sd(value))

valid_df_V2 <- as.data.frame(valid_df_V2)

# Create a data set that computes the RSMEP and MAE
RMSEP_text <- data.frame(pheno = 110,
                         Predicted = c(128, 126, 123, 121,120),
                         Dataset = c("Calibration", "Calibration","Validation", "Validation","Validation"))

# Plot all results including the error for the validation dots
ggplot() +
  geom_abline(intercept = 0, slope = 1, alpha = 0.35) +
  geom_point(data = out_df_fin_dec_r7,
             aes(pheno, Predicted,shape = "Calibration"),color = "deepskyblue") +
  geom_pointrange(data = valid_df_V2,
                  aes(pheno, Predicted, ymin = Predicted - SD_boot, ymax = Predicted + SD_boot, color = "Validation"), 
                  size = 0.3, fatten = 0.1) +
  geom_text(data = RMSEP_text,
            aes(pheno - 5, Predicted + 7), 
            label = c(bquote("RMSE"["calib"]*" = "*.(round(RMSEP_fin_dec_r7, 1))),
                      bquote("MAE"["calib"]*"   = "*.(round(mae_calib, 1))),
                      bquote("RMSE"["valid"]*" = "*.(round(RMSEP_valid_v2, 1))),
                      bquote("MAE"["valid"]*"   = "*.(round(mae_valid_v2, 1))),
                      expression("")),
            hjust = 0, size = 3, fontface = "italic") +
  scale_x_continuous(breaks = seq(105, 135, 5),
                     labels = function (x) format(dormancyR::JDay_to_date(x, 2023), "%b %d"),
                     limits = c(105, 135)) +
  scale_y_continuous(breaks = seq(105, 135, 5),
                     labels = function (x) format(dormancyR::JDay_to_date(x, 2023), "%b %d"),
                     limits = c(105, 135)) +
  scale_color_manual(values = "coral") +
  labs(x = "Observed bloom date",
       y = "Predicted bloom date",
       color = NULL,
       shape = NULL,
       fill = NULL,
       face="bold") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.spacing = unit(-0.1, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = 0.8, vjust = 1))

# Save the final plot to folder
ggsave("figures/Figure_4.png", width = 14, height = 12, units = "cm", dpi = 600)

# Plot Figure S2: With validation data set without calibrating with forcing experiment.
ggplot() +
  geom_abline(intercept = 0, slope = 1, alpha = 0.35) +
  geom_point(data = out_df_fin_dec_r7,
             aes(pheno, Predicted,shape = "Calibration"),color = "deepskyblue") +
  geom_point(data = valid_df_v1,
                  aes(pheno, Predicted, color = "Validation")) +
  geom_text(data = RMSEP_text,
            aes(pheno - 5, Predicted + 7), 
            label = c(bquote("RMSE"["calib"]*" = "*.(round(RMSEP_fin_dec_r7, 1))),
                      bquote("MAE"["calib"]*"   = "*.(round(mae_calib, 1))),
                      bquote("RMSE"["valid"]*" = "*.(round(RMSEP_valid_v1, 1))),
                      bquote("MAE"["valid"]*"   = "*.(round(mae_valid_v1, 1))),
                      expression("")),
            hjust = 0, size = 3, fontface = "italic") +
  scale_x_continuous(breaks = seq(105, 135, 5),
                     labels = function (x) format(dormancyR::JDay_to_date(x, 2023), "%b %d"),
                     limits = c(105, 135)) +
  scale_y_continuous(breaks = seq(105, 135, 5),
                     labels = function (x) format(dormancyR::JDay_to_date(x, 2023), "%b %d"),
                     limits = c(105, 135)) +
  scale_color_manual(values = "red4") +
  labs(x = "Observed bloom date",
       y = "Predicted bloom date",
       color = NULL,
       shape = NULL,
       fill = NULL,
       face="bold") +
  theme_bw() +
  theme(strip.background = element_blank(),
        legend.spacing = unit(-0.1, "cm"),
        legend.position = "top",
        legend.text = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = 0.8, vjust = 1))

# Save the final plot to folder
ggsave("figures/Figure_S4.png", width = 14, height = 12, units = "cm", dpi = 600)

# Estimate chill and heat responses from the calibration fitting
# First, load helper functions to organize the data. (Fernandez et al., 2022)
source("code/00_helper_functions.R")

# Create a data set with theoretical temperatures and heat and chill responses
temp_response <- data.frame(Temp = seq(-5, 60, 0.1),
                               Chill_res = gen_bell(pheno_fit_fin_dec_r8$par, seq(-5, 60, 0.1)),
                               Heat_res = GDH_response(pheno_fit_fin_dec_r8$par, seq(-5, 60, 0.1)))

# Pivot longer to generate a panel plot
Pivot 


# Implement the plot. Generate two plots and then merge them to overcome the issue produced by
# facet_grid (free scales working but not as intended in this particular case)

# Chill plot
chill_response_plot <- ggplot(filter(temp_response, Var == "Chill_res"), aes(Temp, Response)) +
  geom_line(size = 1, color = "blue4") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_x_continuous(limits = c(-5, 25),
                     labels = function (x) paste0(x, " °C")) +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Arbitrary units") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(size = 8),
        strip.text.y = element_blank(),
        strip.background = element_blank())# Heat plot

heat_response_plot <- ggplot(filter(temp_response, Var == "Heat_res"), aes(Temp, Response, color = Var)) +
  geom_line(size = 1, color = "firebrick") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_x_continuous(labels = function (x) paste0(x, " °C")) +
  scale_color_manual(values = c("blue", "red")) +
  labs(y = "Arbitrary units") +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8),
        strip.background = element_blank())

# Use patchwork syntax and functionality to merge the plots
(chill_response_plot + heat_response_plot) + patchwork::plot_annotation(caption = "Temperature") &
  theme(plot.caption = element_text(hjust = 0.5, vjust = 1, size = 11))

# Save the final plot to folder
ggsave("figures/Figure_5.png", width = 12, height = 10, units = "cm", dpi = 600)
