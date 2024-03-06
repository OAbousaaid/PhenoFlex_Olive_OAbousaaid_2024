# Calibration with forcing test
# After the validation of the model and comparing the outputs of the the dynamic of chill and heat
# See script PhenologyFitter_&_forcing_tests.R) we noticed that the amount of chill proposed by PhenoFlex in the first calibration starting from December
# surpasses the date of breaking dormancy by the forcing tests.

# To improve the model with regards to the forcing experiments we are going to perform a second level
# of calibration of the best parameters from the first calibration (See script PhenoFitter_Fin_Dec.R)
# for the two years of forcing experiment. 

# Improving the calibration process with different values of yc, zc
# All possible combinations
# Initialize empty vectors to store the combinations
yc1 <- vector("integer")
zc1 <- vector("integer")

# Generating combinations for vector1 (45 to 53, moving by half unit). yc values according to forcing tets
for (i in seq(53, 58,by = 0.5)) {
  yc1 <- c(yc1, i)
}

# Generating combinations for vector2 (400 to 900, moving by 5 units)
for (j in seq(400, 950, by = 5)) {
  zc1 <- c(zc1, j)
}

# List of values for yc and zc that you want to test
yc_values <- yc1
zc_values <- zc1

# Now i will generate a validation data set with phenology data for 2021 and 2022
pheno_v2 <- pheno_v1[pheno_v1$Year %in% validation_seasons[7:8], ]

# Then I generate a list of seasons with weather data for the validation procedure (2021 and 2022)
season_list_21_22 <- genSeasonList(data, mrange = c(12, 6), years = validation_seasons[7:8])

# Then I generate a list of seasons with weather data for the validation procedure
season_list_forcing <- list()

for (i in c(1:2)) {
  num_rows <- nrow(season_list_21_22[[i]])
  season_list_forcing[[i]] <- season_list_21_22[[i]][744:num_rows,]
}

# Run the loop to get the best combination of yc and zc. The choice of the best combination is based
# smallest mean absolute error between the two years.

lowest_mae <- Inf  # Initialize with a large value
best_combination <- NULL

for (yc in yc_values) {
  for (zc in zc_values) {
    
    # Model parameters here
    pheno_fit_fin_dec_r7_c <- c(yc, zc, 7.992534e-01, 2.681513e+01, 3.240997e+03, 9.921809e+03, 6.867300e+03, 5.939913e+13, 7.492625e+00, 3.427934e+01, 2.959986e+00, 3.555938e+00)
    
    pheno_v2 <- pheno_v1[pheno_v1$Year %in% validation_seasons[7:8], ]
    for (i in 1:nrow(pheno_v2)) {
      num_rows <- nrow(season_list_forcing[[i]])
      pheno_v2[i, "Predicted"] <- PhenoFlex_GDHwrapper(season_list_forcing[[i]], pheno_fit_fin_dec_r7_c)
    }
    
    Observed <- c(pheno_v2[1,]$pheno,pheno_v2[2,]$pheno)
    predicted <- c(pheno_v2[1,]$Predicted, pheno_v2[2,]$Predicted)
    mae <- mean(abs(Observed - predicted))
    
    # Update if the current combination has a lower MAE
    if (mae < lowest_mae) {
      lowest_mae <- mae
      best_combination <- c(yc = yc, zc = zc, mae = lowest_mae)
    }
  }
}
print(best_combination)

