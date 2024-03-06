# The dynamic of chill and heat in comparison with the forcing experiment

# Plot the chill and heat in a combined plot ####
# You can plot with changes in yc and zc; Version 1: yc and zc of the model
#                                         Version 2: yc and zc with the lowest MAE for both years
#                                         Version 3: yc and zc with the closest difference to 0 between the observed and predicted 

# Version 1 ####
yc_v1=8.283128e+01
zc_v1=4.876031e+02 

# Season 2020_2021 ####
iSeason_2021 <- genSeason(data,
                          mrange = c(12, 6),
                          years=c(2021))

length_iSeason_2021 <- length(iSeason_2021[[1]])

season_data_2021<-data[iSeason_2021[[1]][744:length_iSeason_2021],]

res_2021 <- PhenoFlex(temp=season_data_2021$Temp,
                      times=c(1: length(season_data_2021$Temp)),
                      stopatzc=T,basic_output=F,
                      yc=yc_v1, 
                      zc=zc_v1, 
                      s1=7.992534e-01, 
                      Tu=2.681513e+01,
                      E0=3.240997e+03, 
                      E1=9.921809e+03,
                      A0=6.867300e+03, 
                      A1=5.939913e+13,
                      Tf=7.492625e+00, 
                      Tc=3.427934e+01, 
                      Tb=2.959986e+00, 
                      slope=3.555938e+00)

FullBloom_2021_v1 <- res_2021$bloomindex
seasontemps_2021<-data[iSeason_2021[[1]][744:length_iSeason_2021],]
seasontemps_2021[,"x"]<-res_2021$x
seasontemps_2021[,"y"]<-res_2021$y
seasontemps_2021[,"z"]<-res_2021$z
seasontemps_2021<-add_date(seasontemps_2021)

CR_full_2021_v1<-seasontemps_2021$Date[which(seasontemps_2021$y>=yc_v1)[1]]
Bloom_2021_v1<-seasontemps_2021$Date[which(seasontemps_2021$z>=zc_v1)[1]]

# Chill and heat plot
chill_heat_21_v1 <- ggplot(data = seasontemps_2021[1:FullBloom_2021_v1,], aes(x = Date)) +
  geom_line(aes(y = y*3), col = "blue", lwd = 0.5) +
  geom_line(aes(y = z), col = "red", lwd = 0.5) +
  geom_hline(yintercept = yc_v1*3, lty = 2, col = "blue", lwd = 0.5) +
  geom_hline(yintercept = zc_v1, lty = 2, col = "red", lwd = 0.5) +
  geom_vline(xintercept = CR_full_2021_v1, lty = 3, col = "blue", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2021-02-20 12:00:00"), linetype = "dotdash", color = "green4",lwd = 1) +
  geom_vline(xintercept = Bloom_2021_v1, lty = 3, col = "red", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2021-04-20 12:00:00"), linetype = "dotdash", color = "orange2",lwd = 1) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ ./3, 
      name = "Chill accumulation (y)",
      breaks = seq(0, 160, 20)
    ),
    breaks = seq(0, 500, 100)
  ) +
  theme_bw() +
  ylab("Heat accumulation (z)") +
  labs(title = "Chill & Heat 2021") +
  theme(
    plot.title = element_text(color = "black", face = "bold.italic", hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10, color = "red"),
    axis.text.y  = element_text(color = "red"),
    axis.line.y = element_line(color = "red"),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "blue"),
    axis.line.y.right = element_line(color = "blue")
  ) +
  annotate(
    "text", label = "Chill req. (yc)", 
    x = ISOdate(2021, 01, 15),
    y = yc_v1 * 3.5, col = "blue", size = 3
  ) +
  annotate(
    "text", label = "Heat req. (zc)", 
    x = ISOdate(2021, 01, 15),
    y = zc_v1 * 0.95, col = "red", size = 3
  )
chill_heat_21_v1

# Season 2021_2022 ####
iSeason_2022 <- genSeason(data,
                          mrange = c(12, 6),
                          years=c(2022))

length_iSeason_2022 <- length(iSeason_2022[[1]])

season_data_2022<-data[iSeason_2022[[1]][744:length_iSeason_2022],]

res_2022 <- PhenoFlex(temp=season_data_2022$Temp,
                      times=c(1: length(season_data_2022$Temp)),
                      stopatzc=T,basic_output=F,
                      yc=yc_v1, 
                      zc=zc_v1, 
                      s1=7.992534e-01, 
                      Tu=2.681513e+01,
                      E0=3.240997e+03, 
                      E1=9.921809e+03,
                      A0=6.867300e+03, 
                      A1=5.939913e+13,
                      Tf=7.492625e+00, 
                      Tc=3.427934e+01, 
                      Tb=2.959986e+00, 
                      slope=3.555938e+00)

FullBloom_2022_v1 <- res_2022$bloomindex
seasontemps_2022<- data[iSeason_2022[[1]][744:length_iSeason_2022],]
seasontemps_2022[,"x"]<-res_2022$x
seasontemps_2022[,"y"]<-res_2022$y
seasontemps_2022[,"z"]<-res_2022$z
seasontemps_2022 <-add_date(seasontemps_2022)

CR_full_2022_v1<-seasontemps_2022$Date[which(seasontemps_2022$y>=yc_v1)[1]]
Bloom_2022_v1<-seasontemps_2022$Date[which(seasontemps_2022$z>=zc_v1)[1]]

# The chill and heat plot
chill_heat_22_v1 <- ggplot(data = seasontemps_2022[1:FullBloom_2022_v1,], aes(x = Date)) +
  geom_line(aes(y = y*3), col = "blue", lwd = 0.5) +
  geom_line(aes(y = z), col = "red", lwd = 0.5) +
  geom_hline(yintercept = yc_v1*3, lty = 2, col = "blue", lwd = 0.5) +
  geom_hline(yintercept = zc_v1, lty = 2, col = "red", lwd = 0.5) +
  geom_vline(xintercept = CR_full_2022_v1, lty = 3, col = "blue", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2022-03-01 12:00:00"), linetype = "dotdash", color = "green4",lwd = 1) +
  geom_vline(xintercept = Bloom_2022_v1, lty = 3, col = "red", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2022-05-08 12:00:00"), linetype = "dotdash", color = "orange2",lwd = 1) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ ./3, 
      name = "Chill accumulation (y)",
      breaks = seq(0, 160, 20)
    ),
    breaks = seq(0, 500, 100)
  ) +
  theme_bw() +
  ylab("Heat accumulation (z)") +
  labs(title = "Chill & Heat 2022") +
  theme(
    plot.title = element_text(color = "black", face = "bold.italic", hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10, color = "red"),
    axis.text.y  = element_text(color = "red"),
    axis.line.y = element_line(color = "red"),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "blue"),
    axis.line.y.right = element_line(color = "blue")
  ) +
  annotate(
    "text", label = "Chill req. (yc)", 
    x = ISOdate(2022, 01, 15),
    y = yc_v1 * 3.5, col = "blue", size = 3
  ) +
  annotate(
    "text", label = "Heat req. (zc)", 
    x = ISOdate(2022, 01, 15),
    y = zc_v1 * 0.95, col = "red", size = 3
  )+
  xlim(range(seasontemps_2022$Date[1:2940]))
chill_heat_22_v1

# Version 2 ####
# Season 2020_2021 ####
yc_v2= 58
zc_v2= 785

res_2021 <- PhenoFlex(temp=season_data_2021$Temp,
                      times=c(1: length(season_data_2021$Temp)),
                      stopatzc=T,basic_output=F,
                      yc=yc_v2, 
                      zc=zc_v2, 
                      s1=7.992534e-01, 
                      Tu=2.681513e+01,
                      E0=3.240997e+03, 
                      E1=9.921809e+03,
                      A0=6.867300e+03, 
                      A1=5.939913e+13,
                      Tf=7.492625e+00, 
                      Tc=3.427934e+01, 
                      Tb=2.959986e+00, 
                      slope=3.555938e+00)

FullBloom_2021_v2 <- res_2021$bloomindex
seasontemps_2021<-data[iSeason_2021[[1]][744:length_iSeason_2021],]
seasontemps_2021[,"x"]<-res_2021$x
seasontemps_2021[,"y"]<-res_2021$y
seasontemps_2021[,"z"]<-res_2021$z
seasontemps_2021<-add_date(seasontemps_2021)

CR_full_2021_v2<-seasontemps_2021$Date[which(seasontemps_2021$y>=yc_v2)[1]]
Bloom_2021_v2<-seasontemps_2021$Date[which(seasontemps_2021$z>=zc_v2)[1]]

# The chill and heat plot
chill_heat_21_v2 <- ggplot(data = seasontemps_2021[1:FullBloom_2021_v2,], aes(x = Date)) +
  geom_line(aes(y = y*5.3), col = "blue", lwd = 0.5) +
  geom_line(aes(y = z), col = "red", lwd = 0.5) +
  geom_hline(yintercept = yc_v2*5.3, lty = 2, col = "blue", lwd = 0.5) +
  geom_hline(yintercept = zc_v2, lty = 2, col = "red", lwd = 0.5) +
  geom_vline(xintercept = CR_full_2021_v2, lty = 3, col = "blue", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2021-02-20 12:00:00"), linetype = "dotdash", color = "green4",lwd = 1) +
  geom_vline(xintercept = Bloom_2021_v2, lty = 3, col = "red", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2021-04-20 12:00:00"), linetype = "dotdash", color = "orange2",lwd = 1) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ ./5.3, 
      name = "Chill accumulation (y)",
      breaks = seq(0, 160, 20)
    ),
    breaks = seq(0, 900, 100)
  ) +
  theme_bw() +
  ylab("Heat accumulation (z)") +
  labs(title = "Chill & Heat 2021") +
  theme(
    plot.title = element_text(color = "black", face = "bold.italic", hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10, color = "red"),
    axis.text.y  = element_text(color = "red"),
    axis.line.y = element_line(color = "red"),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "blue"),
    axis.line.y.right = element_line(color = "blue")
  ) +
  annotate(
    "text", label = "Chill req. (yc)", 
    x = ISOdate(2021, 01, 15),
    y = yc_v2 * 6.1, col = "blue", size = 3
  ) +
  annotate(
    "text", label = "Heat req. (zc)", 
    x = ISOdate(2021, 01, 15),
    y = zc_v2 * 0.95, col = "red", size = 3
  )
chill_heat_21_v2

# Season 2021_2022 ####
res_2022 <- PhenoFlex(temp=season_data_2022$Temp,
                      times=c(1: length(season_data_2022$Temp)),
                      stopatzc=T,basic_output=F,
                      yc=yc_v2, 
                      zc=zc_v2, 
                      s1=7.992534e-01, 
                      Tu=2.681513e+01,
                      E0=3.240997e+03, 
                      E1=9.921809e+03,
                      A0=6.867300e+03, 
                      A1=5.939913e+13,
                      Tf=7.492625e+00, 
                      Tc=3.427934e+01, 
                      Tb=2.959986e+00, 
                      slope=3.555938e+00)

FullBloom_2022_v2 <- res_2022$bloomindex
seasontemps_2022<- data[iSeason_2022[[1]][744:length_iSeason_2022],]
seasontemps_2022[,"x"]<-res_2022$x
seasontemps_2022[,"y"]<-res_2022$y
seasontemps_2022[,"z"]<-res_2022$z
seasontemps_2022 <-add_date(seasontemps_2022)

CR_full_2022_v2<-seasontemps_2022$Date[which(seasontemps_2022$y>=yc_v2)[1]]
Bloom_2022_v2<-seasontemps_2022$Date[which(seasontemps_2022$z>=zc_v2)[1]]

# The chill and heat plot
chill_heat_22_v2 <- ggplot(data = seasontemps_2022[1:FullBloom_2022_v2,], aes(x = Date)) +
  geom_line(aes(y = y*5.3), col = "blue", lwd = 0.5) +
  geom_line(aes(y = z), col = "red", lwd = 0.5) +
  geom_hline(yintercept = yc_v2*5.3, lty = 2, col = "blue", lwd = 0.5) +
  geom_hline(yintercept = zc_v2, lty = 2, col = "red", lwd = 0.5) +
  geom_vline(xintercept = CR_full_2022_v2, lty = 3, col = "blue", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2022-03-01 12:00:00"), linetype = "dotdash", color = "green4",lwd = 1) +
  geom_vline(xintercept = Bloom_2022_v2, lty = 3, col = "red", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2022-05-08 12:00:00"), linetype = "dotdash", color = "orange2",lwd = 1) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ ./5.3, 
      name = "Chill accumulation (y)",
      breaks = seq(0, 160, 20)
    ),
    breaks = seq(0, 900, 100)
  ) +
  theme_bw() +
  ylab("Heat accumulation (z)") +
  labs(title = "Chill & Heat 2022") +
  theme(
    plot.title = element_text(color = "black", face = "bold.italic", hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10, color = "red"),
    axis.text.y  = element_text(color = "red"),
    axis.line.y = element_line(color = "red"),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "blue"),
    axis.line.y.right = element_line(color = "blue")
  ) +
  annotate(
    "text", label = "Chill req. (yc)", 
    x = ISOdate(2022, 01, 15),
    y = yc_v2 * 6.1, col = "blue", size = 3
  ) +
  annotate(
    "text", label = "Heat req. (zc)", 
    x = ISOdate(2022, 01, 15),
    y = zc_v2 * 0.95, col = "red", size = 3
  )
chill_heat_22_v2

# Combined plot for the two versions and two seasons

PhenoFlex_Forcing <- (chill_heat_21_v1 | chill_heat_21_v2) /
  (chill_heat_22_v1 | chill_heat_22_v2)

# Save the final plot to folder
ggsave("figures/Figure_S3.png", width = 22, height = 16, units = "cm", dpi = 600)

# Version 3 ####
# Season 2020_2021 ####
# The best yc and zc for the year 2021 (7_Second_level_calibration.R)
res_2021 <- PhenoFlex(temp=season_data_2021$Temp,
                      times=c(1: length(season_data_2021$Temp)),
                      stopatzc=T,basic_output=F,
                      yc=53, 
                      zc=670, 
                      s1=7.992534e-01, 
                      Tu=2.681513e+01,
                      E0=3.240997e+03, 
                      E1=9.921809e+03,
                      A0=6.867300e+03, 
                      A1=5.939913e+13,
                      Tf=7.492625e+00, 
                      Tc=3.427934e+01, 
                      Tb=2.959986e+00, 
                      slope=3.555938e+00)

FullBloom_2021_v3 <- res_2021$bloomindex
seasontemps_2021<-data[iSeason_2021[[1]][744:length_iSeason_2021],]
seasontemps_2021[,"x"]<-res_2021$x
seasontemps_2021[,"y"]<-res_2021$y
seasontemps_2021[,"z"]<-res_2021$z
seasontemps_2021<-add_date(seasontemps_2021)

CR_full_2021_v3<-seasontemps_2021$Date[which(seasontemps_2021$y>=53)[1]]
Bloom_2021_v3<-seasontemps_2021$Date[which(seasontemps_2021$z>=670)[1]]

# Plot 2020_2021
chill_heat_21_v3 <- ggplot(data = seasontemps_2021[1:FullBloom_2021_v3,], aes(x = Date)) +
  geom_line(aes(y = y*5.3), col = "blue", lwd = 0.5) +
  geom_line(aes(y = z), col = "red", lwd = 0.5) +
  geom_hline(yintercept = 53*5.3, lty = 2, col = "blue", lwd = 0.5) +
  geom_hline(yintercept = 670, lty = 2, col = "red", lwd = 0.5) +
  geom_vline(xintercept = CR_full_2021_v3, lty = 3, col = "blue", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2021-02-20 12:00:00"), linetype = "dotdash", color = "green4",lwd = 1) +
  geom_vline(xintercept = Bloom_2021_v3, lty = 3, col = "red", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2021-04-20 12:00:00"), linetype = "dotdash", color = "orange2",lwd = 1) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ ./5.3, 
      name = "Chill accumulation (y)",
      breaks = seq(0, 160, 20)
    ),
    breaks = seq(0, 900, 100)
  ) +
  theme_bw() +
  ylab("Heat accumulation (z)") +
  labs(title = "Chill & Heat 2021") +
  theme(
    plot.title = element_text(color = "black", face = "bold.italic", hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10, color = "red"),
    axis.text.y  = element_text(color = "red"),
    axis.line.y = element_line(color = "red"),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "blue"),
    axis.line.y.right = element_line(color = "blue")
  ) +
  annotate(
    "text", label = "Chill req. (yc)", 
    x = ISOdate(2021, 01, 15),
    y = 53 * 6.1, col = "blue", size = 3
  ) +
  annotate(
    "text", label = "Heat req. (zc)", 
    x = ISOdate(2021, 01, 15),
    y = 670 * 0.95, col = "red", size = 3
  )
chill_heat_21_v3

# Season 2021_2022 ####
# The best yc and zc for the year 2022 (7_Second_level_calibration.R)
res_2022 <- PhenoFlex(temp=season_data_2022$Temp,
                      times=c(1: length(season_data_2022$Temp)),
                      stopatzc=T,basic_output=F,
                      yc=58.5, 
                      zc=795, 
                      s1=7.992534e-01, 
                      Tu=2.681513e+01,
                      E0=3.240997e+03, 
                      E1=9.921809e+03,
                      A0=6.867300e+03, 
                      A1=5.939913e+13,
                      Tf=7.492625e+00, 
                      Tc=3.427934e+01, 
                      Tb=2.959986e+00, 
                      slope=3.555938e+00)

FullBloom_2022_v3 <- res_2022$bloomindex
seasontemps_2022<- data[iSeason_2022[[1]][744:length_iSeason_2022],]
seasontemps_2022[,"x"]<-res_2022$x
seasontemps_2022[,"y"]<-res_2022$y
seasontemps_2022[,"z"]<-res_2022$z
seasontemps_2022 <-add_date(seasontemps_2022)

CR_full_2022_v3<-seasontemps_2022$Date[which(seasontemps_2022$y>=58.5)[1]]
Bloom_2022_v3<-seasontemps_2022$Date[which(seasontemps_2022$z>=795)[1]]

chill_heat_22_v3 <- ggplot(data = seasontemps_2022[1:FullBloom_2022_v3,], aes(x = Date)) +
  geom_line(aes(y = y*5.3), col = "blue", lwd = 0.5) +
  geom_line(aes(y = z), col = "red", lwd = 0.5) +
  geom_hline(yintercept = 58.5*5.3, lty = 2, col = "blue", lwd = 0.5) +
  geom_hline(yintercept = 795, lty = 2, col = "red", lwd = 0.5) +
  geom_vline(xintercept = CR_full_2022_v3, lty = 3, col = "blue", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2022-03-01 12:00:00"), linetype = "dotdash", color = "green4",lwd = 1) +
  geom_vline(xintercept = Bloom_2022_v3, lty = 3, col = "red", lwd = 0.5) +
  geom_vline(xintercept = as.POSIXct("2022-05-08 12:00:00"), linetype = "dotdash", color = "orange2",lwd = 1) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ ./5.3, 
      name = "Chill accumulation (y)",
      breaks = seq(0, 160, 20)
    ),
    breaks = seq(0, 900, 100)
  ) +
  theme_bw() +
  ylab("Heat accumulation (z)") +
  labs(title = "Chill & Heat 2022") +
  theme(
    plot.title = element_text(color = "black", face = "bold.italic", hjust = 0.5, vjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10, color = "red"),
    axis.text.y  = element_text(color = "red"),
    axis.line.y = element_line(color = "red"),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "blue"),
    axis.line.y.right = element_line(color = "blue")
  ) +
  annotate(
    "text", label = "Chill req. (yc)", 
    x = ISOdate(2022, 01, 15),
    y = 58.5 * 6.1, col = "blue", size = 3
  ) +
  annotate(
    "text", label = "Heat req. (zc)", 
    x = ISOdate(2022, 01, 15),
    y = 795 * 0.95, col = "red", size = 3
  )
chill_heat_22_v3

# Combined plot for the two versions (V1+V3) and two seasons
PhenoFlex_Forcing_final <- (chill_heat_21_v1 | chill_heat_21_v3) /
  (chill_heat_22_v1 | chill_heat_22_v3)

# Save the final plot to folder
ggsave("figures/Figure_3.png", width = 22, height = 16, units = "cm", dpi = 600)

