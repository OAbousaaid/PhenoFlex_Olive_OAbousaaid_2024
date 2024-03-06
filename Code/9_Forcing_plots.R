# This script is for the plot of the forcing test of Picholine Marocaine (PM)

# Upload data
forcing <- read_excel("Forcing_PM.xlsx",
                        col_types = c("date","text","text","text","numeric","numeric"))
forcing <- as.data.frame(forcing)

forcing <- forcing[,c(1,3:6)]

forcing_PM <- forcing %>%
  rename(Unforced=fresh_weight)%>%
  rename(Forced=fresh_weight_F)%>%
  pivot_longer(cols = c(Unforced, Forced),    
               names_to = "Treatment",        
               values_to = "Weight")

forcing_PM <- na.omit(forcing_PM)
forcing_PM$Date <- as.Date(forcing_PM$Date)

# Visualization

# The Plot
PM <- ggline(forcing_PM, x = "Date", y = "Weight", add = "mean_se",
       color = "Treatment",facet.by = "Year")+
  stat_compare_means(aes(group = Treatment), label = "p.signif",paired = T,
                     method = "t.test",vjust = 0.5)

# X axis start date and break
breaks <- seq(as.Date("2021-01-01"), as.Date("2022-12-31"), by = "17 days")

PM <- facet(PM,facet.by = "Year",scales = "free",panel.labs.font = list(face = "bold"))+
    labs(x="Sampling date", y="Floral bud fresh weights (mg)")+
    scale_color_manual(values=c("cornflowerblue", "coral"),
                       name="",
                       breaks=c("Unforced", "Forced"),
                       labels=c("**Unforced**", "**Forced**"))+
  ggtitle("Picholine Marocaine")+
  theme(plot.title = element_text(color="black", size=14, face="bold.italic",
                                    hjust = 0.5,vjust = 0.5),
        axis.text.x = element_text(angle = 30, hjust = 0.8, vjust = 1))+
  theme(axis.title.x = element_text(face="bold", size=10))+
  theme(axis.title.y = element_text(face="bold", size=10))+
  theme(legend.text = element_markdown())+
  scale_x_date(breaks = breaks, date_labels = "%b %d")

PM <- PM + geom_vline(xintercept = as.Date(c("2021-02-20", "2022-03-01")), 
                linetype = "dotdash", color = "green4",alpha=0.5,lwd=1)
print(PM)


# Temperatures for the two years of forcing tests

Temp_2021_2022 <- read_excel("Max_Min_Temp_2021_2022.xlsx")
Temp_2021_2022 <- as.data.frame(Temp_2021_2022)

Temp_2021_2022 <- add_date(Temp_2021_2022)
Temp_2021_2022$Year <- as.factor(Temp_2021_2022$Year)

Temp_2021_2022 <- Temp_2021_2022 %>%
  pivot_longer(cols = c(Tmax, Tmin, Tmean),
               names_to = "Treatment",
               values_to = "Temperature") %>%
  select(Date, Year, Treatment, Temperature)

# Rolling mean of 11 days
library(zoo)
Sys.setlocale("LC_TIME", "English")

Temp_run_mean <- Temp_2021_2022 %>%
  group_by(Treatment) %>%
  arrange(Date) %>%
  mutate(RunningMean = rollmean(Temperature, k = 11, align = "center", fill = NA))

Temp_run_mean <- na.omit(Temp_run_mean)

rows_to_remove <- 271:285
Temp_run_mean <- Temp_run_mean[-rows_to_remove, ]




# Create the plot using ggplot2
Temp_plot <- ggplot(Temp_run_mean, aes(x = Date, y = RunningMean, color = Treatment)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ Year,scales = "free_x") +
  labs(x = "Date", y = "Temperature", color = "Treatment") +
  theme_bw()+
  theme(legend.position = "top",
        axis.text = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold",size = 10),
        axis.title.y = element_text(face = "bold",size = 10)) +
  guides(color = guide_legend(title = NULL))
    
print(Temp_plot)    

library(patchwork)
Figure_2 <- PM / Temp_plot

print(Figure_2)

ggsave("figures/Figure_2.png", width = 24, height = 22, units = "cm", dpi = 600)







