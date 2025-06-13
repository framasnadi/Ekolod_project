#############################################
# Metrics from Historical and New Data files B1 & H4
#############################################
library(dplyr)
library(purrr)
library(readxl)
library(e1071)
library(ggplot2)
library(stringr)
library(readr)
library(ggpubr)
setwd("C:/Users/frma6502/Desktop/SU/EKOLOD")

##########
# Historical data 1985-1996
# Paths
data_path    <- "C:/Users/frma6502/Desktop/SU/EKOLOD/OLDfiles_Sture/Historical_Data"
meta_path    <- "C:/Users/frma6502/Desktop/SU/EKOLOD/OLDfiles_Sture/Summary_stations.xlsx"
# Read station metadata once
summary_meta <- read_excel(meta_path, sheet = "Subset")

# Function to process one CSV
process_file <- function(csv_file) {
  # extract the “filename” (the numeric ID) from the basename
  file_id   <- tools::file_path_sans_ext(basename(csv_file))
  file_num  <- as.numeric(file_id)
  
  # read the raw .CSV
  raw_data  <- read.table(csv_file,
                          sep = ";",
                          header = FALSE,
                          fill = TRUE,
                          stringsAsFactors = FALSE)
  # TS classes & counts
  TSclas      <- seq(-41, -55, by = -2)   # ALL community
  fish_counts <- round(as.numeric(raw_data[85:92, 3]))  # to match the TSclas selected
  #TSclas      <- seq(-41, -49, by = -2)  # adults clupeid
  #fish_counts <- round(as.numeric(raw_data[85:89, 3]))  # to match juveniles TSclas 
  #TSclas      <- seq(-51, -55, by = -2)  # juveniles
  #fish_counts <- round(as.numeric(raw_data[90:92, 3]))  # to match juveniles TSclas 
  
  fish_nasc <- (as.numeric(raw_data[85:92, 6]))  # to match the TSclas selected
  fish_abun <- (as.numeric(raw_data[85:92, 3]))  # to match the TSclas selected
  fish_biom <- (as.numeric(raw_data[85:92, 10]))  # to match the TSclas selected
  
  # expand for summary stats
  expanded <- rep(TSclas, times = fish_counts)
  # pull metadata for this file
  md <- summary_meta %>% filter(File == file_num)
  # build a one-row tibble of all your metrics + metadata
  tibble(
    File                = file_num,
    Station             = md$Site,
    Echosounder         = md$`Echo-sounder`,
    Software            = md$`Post Process-ing software`,
    Info                = as.Date(as.character(md$Date), format = "%y%m%d"),
    Year                = format(as.Date(md$Date, format = "%y%m%d"), "%Y"),
    Month               = format(as.Date(md$Date, format = "%y%m%d"), "%m"),
    Day                 = format(as.Date(md$Date, format = "%y%m%d"), "%d"),
    Time_Min            = md$Time,
    Depth               = md$`Average depth (m)`,
    Distance            = NA_real_,
    SpeedVes            = NA_real_,
    Type                = "SmallPel",
    NASC                 = sum(fish_nasc, na.rm = TRUE),
    tot_abund_hectar     = sum(fish_abun, na.rm = TRUE),
    tot_biomass_hectar   = sum(fish_biom, na.rm = TRUE),
    mean_TS             = mean(expanded, na.rm = TRUE),
    median_TS           = median(expanded, na.rm = TRUE),
    skewness_TS         = e1071::skewness(expanded, na.rm = TRUE), # <0 left skewed; >0 right skewed
    IQR_TS              = IQR(expanded, na.rm = TRUE)
  )
}

# Get all .CSV files and process them
csv_files <- list.files(data_path, pattern = "\\.CSV$", full.names = TRUE,ignore.case=TRUE)
db_final_historic <- map_dfr(csv_files, process_file)

# db version for plotting - Historical
summary_plot_old <- db_final_historic %>%
  mutate(
    Year   = as.integer(Year),
    Season = case_when(
      Month %in% c("12","01","02") ~ "Winter",
      Month %in% c("03","04","05") ~ "Spring",
      Month %in% c("06","07","08") ~ "Summer",
      Month %in% c("09","10","11") ~ "Autumn"),
    Station = case_when(
      Station %in% c("Station 1","Lacka-Askö") ~ "B1",
      Station %in% c("Station 4","Station 5","Himmerfjärden","Oaxen","Oaxen - Station 4") ~ "H4")
  ) %>%
  group_by(Station, Season, Year) %>%
  summarise(
    mean_skew = mean(skewness_TS, na.rm = TRUE),
    sd_skew   = sd(skewness_TS,   na.rm = TRUE),
    cv_skew   = sd_skew / mean_skew,
    mean_NASC = mean(log10(NASC), na.rm = TRUE),
    sd_NASC   = sd(log10(NASC),   na.rm = TRUE),
    cv_NASC   = sd_NASC / mean_NASC,
    mean_abun = mean(log10(tot_abund_hectar), na.rm = TRUE),
    sd_abun   = sd(log10(tot_abund_hectar),   na.rm = TRUE),
    cv_abun   = sd_abun / mean_abun,
    mean_biom = mean(log10(tot_biomass_hectar), na.rm = TRUE),
    sd_biom   = sd(log10(tot_biomass_hectar),   na.rm = TRUE),
    cv_biom   = sd_biom / mean_biom,
    mean_median_TS = mean(median_TS, na.rm = TRUE),
    sd_median_TS   = sd(median_TS,   na.rm = TRUE),
    cv_median_TS   = sd_median_TS / mean_median_TS,
    # ADD OTHER METRICS!!!
    .groups   = "drop"
  )

##########
# New data 2023 August, 2024 September
data_path_new    <- "C:/Users/frma6502/Desktop/SU/EKOLOD/ESP3output/outputNEW"
new_files <- list.files(data_path_new, pattern = "\\.CSV$", full.names = TRUE,ignore.case=TRUE)
db_final_new <- map_dfr(
  new_files,
  ~ read_csv(.x) %>% filter(Type == "SmallPel") %>% mutate(Day = as.character(Day),Month = as.character(Month)),
  .id = "file"        
) %>%
  mutate(
    file    = basename(new_files[as.integer(file)]),
    Station = str_sub(tools::file_path_sans_ext(file), 1, 2)
  ) %>%
  select(-file) 
# Filter db_new to match the same TS class of the old data (-41;-55)
db_final_new <- db_final_new %>% dplyr:: filter(TSclas <= -41 & TSclas >= -55 )  %>% 
  group_by(Year,Station, Time_Min) %>% 
  mutate(
  NASC = sum(NASCbyTS, na.rm = TRUE),
  mean_abund_hectar = mean(abund_hectar_byTS, na.rm = TRUE),
  tot_abund_hectar = sum(abund_hectar_byTS, na.rm = TRUE), 
  mean_biomass_hectar = mean(biomass_hectar_byTS/1000, na.rm = TRUE), # gr to kg
  tot_biomass_hectar = sum(biomass_hectar_byTS/1000, na.rm = TRUE), # gr to kg
  # proxies of community structure 
  mean_TS = mean(rep(TSclas,count)),
  median_TS = median(rep(TSclas,count)),
  skewness_TS = skewness(rep(TSclas,count)), # <0 left skewed; >0 right skewed
  IQR_TS = IQR(rep(TSclas,count)) #interquartile range
)

# db version for plotting - New
summary_plot_new <- db_final_new %>%
  mutate(
    Year   = as.integer(Year),
    Season = case_when(
      Month %in% c("12","01","02") ~ "Winter",
      Month %in% c("03","04","05") ~ "Spring",
      Month %in% c("06","07","08") ~ "Summer",
      Month %in% c("09","10","11") ~ "Autumn")
  ) %>%
  group_by(Station, Season, Year) %>%
  summarise(
    mean_skew = mean(skewness_TS, na.rm = TRUE),
    sd_skew   = sd(skewness_TS,   na.rm = TRUE),
    cv_skew   = sd_skew / mean_skew,
    mean_NASC = mean(log10(NASC), na.rm = TRUE),
    sd_NASC   = sd(log10(NASC),   na.rm = TRUE),
    cv_NASC   = sd_NASC / mean_NASC,
    mean_abun = mean(log10(tot_abund_hectar), na.rm = TRUE),
    sd_abun   = sd(log10(tot_abund_hectar),   na.rm = TRUE),
    cv_abun   = sd_abun / mean_abun,
    mean_biom = mean(log10(tot_biomass_hectar), na.rm = TRUE),
    sd_biom   = sd(log10(tot_biomass_hectar),   na.rm = TRUE),
    cv_biom   = sd_biom / mean_biom,
    mean_median_TS = mean(median_TS, na.rm = TRUE),
    sd_median_TS   = sd(median_TS,   na.rm = TRUE),
    cv_median_TS   = sd_median_TS / mean_median_TS,
    # ADD OTHER METRICS!!!
    .groups   = "drop"
  )

####################
# Merge new and old
summary_plot <- rbind(summary_plot_old, summary_plot_new)%>% dplyr::filter(Season != "Spring")
# Add values form Svedäng 2021 in H4 summer
summary_plot[nrow(summary_plot) + 1, ] <- NA
summary_plot[34, ]$Station <- "H4"
summary_plot[34, ]$Season <- "Summer"
summary_plot[34, ]$Year <- 2021
summary_plot[34, ]$mean_NASC <- log10(343*3.43)
summary_plot[34, ]$mean_abun <- log10(1.688*10000)
summary_plot[34, ]$mean_median_TS <- -51 # estimated visually from the Figure S6 of the supl mat

################################
# PLOT metric TIME SERIES
################################
# NASC
ggnasc <- ggplot(summary_plot,
       aes(x = Year, y = mean_NASC, color = Season, group = Season)) +
  #geom_line() +
  geom_point(size=1.5) +
  geom_errorbar(aes(
    ymin = mean_NASC - (cv_NASC * mean_NASC),
    ymax = mean_NASC + (cv_NASC * mean_NASC)
  ),
  width = 0.2
  ) + geom_smooth(method="gam",formula = y ~ s(x,bs="cs", k = 4), se=F, linetype= "dashed", size=0.7 )+
  facet_wrap(~ Station, scales = "free_y", ncol=1) +
  labs(
    title = "NASC",
    x     = "",
    y     = "log10(NASC)"
  ) +
  theme_pubr()+ scale_x_continuous(breaks = seq(1985, 2024,  by = 1)) + theme( axis.text.x = element_text(vjust = 0.5, angle=90))

# Abundance
ggabb <- ggplot(summary_plot,
       aes(x = Year, y = mean_abun, color = Season, group = Season)) +
 # geom_line() +
  geom_point(size=1.5) +
  geom_errorbar(aes(
    ymin = mean_abun - (cv_abun * mean_abun),
    ymax = mean_abun + (cv_abun * mean_abun)
  ),
  width = 0.2
  ) + geom_smooth(method="gam",formula = y ~ s(x,bs="cs", k = 4), se=F, linetype= "dashed", size=0.7 )+
  facet_wrap(~ Station, scales = "free_y", ncol=1) +
  labs(
    title = "Abundance",
    x     = "",
    y     = "log10(fish/ha)"
  ) +
  theme_pubr()+ scale_x_continuous(breaks = seq(1985, 2024,  by = 1)) + theme( axis.text.x = element_text(vjust = 0.5, angle=90))

# Biomass
ggbiom <- ggplot(summary_plot,
       aes(x = Year, y = mean_biom, color = Season, group = Season)) +
  # geom_line() +
  geom_point(size=1.5) +
  geom_errorbar(aes(
    ymin = mean_biom - (cv_biom * mean_biom),
    ymax = mean_biom + (cv_biom * mean_biom)
  ),
  width = 0.2
  ) + geom_smooth(method="gam",formula = y ~ s(x,bs="cs", k = 4), se=F, linetype= "dashed", size=0.7 )+
  facet_wrap(~ Station, scales = "free_y", ncol=1) +
  labs(
    title = "Biomass",
    x     = "",
    y     = "log10(gr/ha)"
  ) +
  theme_pubr()+ scale_x_continuous(breaks = seq(1985, 2024,  by = 1)) + theme( axis.text.x = element_text(vjust = 0.5, angle=90))

# Skewness
ggskew <-ggplot(summary_plot,
       aes(x = Year, y = mean_skew, color = Season, group = Season)) +
 # geom_line() +
  geom_point(size=1.5) +
  geom_errorbar(aes(
    ymin = mean_skew - (cv_skew * mean_skew),
    ymax = mean_skew + (cv_skew * mean_skew)
  ),
  width = 0.2
  ) + geom_smooth(method="gam",formula = y ~ s(x,bs="cs", k = 4), se=F, linetype= "dashed", size=0.7 )+
  facet_wrap(~ Station, scales = "free_y", ncol=1) +
  labs(
    title = "Skewness (high skewness = smaller individuals)",
    x     = "",
    y     = "Skewness"
  ) +
  theme_pubr()+ scale_x_continuous(breaks = seq(1985, 2024,  by = 1)) + theme( axis.text.x = element_text(vjust = 0.5, angle=90))

# median TS
ggmedian <- ggplot(summary_plot,
       aes(x = Year, y = mean_median_TS, color = Season, group = Season)) +
  # geom_line() +
  geom_point(size=1.5) +
  geom_errorbar(aes(
    ymin = mean_median_TS - (cv_median_TS * mean_median_TS),
    ymax = mean_median_TS + (cv_median_TS * mean_median_TS)
  ),
  width = 0.2
  ) + geom_smooth(method="gam",formula = y ~ s(x,bs="cs", k = 4), se=F, linetype= "dashed", size=0.7 )+
  facet_wrap(~ Station, scales = "free_y", ncol=1) +
  labs(
    title = "Median TS (Adult clupeids > -50 dB < juveniles/sticklebacks)",
    x     = "",
    y     = "Median TS (dB)"
  ) + geom_hline(yintercept = -50, linetype= "dotted") +
  theme_pubr()+scale_x_continuous(breaks = seq(1985, 2024,  by = 1)) + theme( axis.text.x = element_text( vjust = 0.5, angle=90))


######################
# All plot together
jpeg("Ekolodtimeseries_metrics.jpeg",width = 360, height = 230, units = "mm", res = 600)
ggarrange(ggnasc, ggabb, ggskew, ggmedian  , 
          common.legend = TRUE,
          legend       = "bottom",
          ncol = 2, nrow = 2)
dev.off()

# for different species/life stages
ggarrange(ggnasc, ggabb , 
          common.legend = TRUE,
          legend       = "bottom",
          ncol = 2, nrow = 1)

