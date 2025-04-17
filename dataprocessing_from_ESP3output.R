#############################################################
#                                                           #
#    Processing ESP3 exported data after echo integration   #
#                                                           #
#   Author: Francesco Masnadi (DEEP, Stockholm University)  #
#                                                           #
#############################################################

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(rstudioapi)
library(e1071)

# Function to produce metrics of interest starting from total NASC and TS-echoes histogram (similar to Stox)
process_ESP3_data <- function(db_nasc, db_hist, a, b, b20, TS_min, TS_max) {
  # Process TS data
  db_TS_hist <- db_hist %>% 
    group_by(Track_ID)  %>% # some echoes are in several pings
    summarise(mTS = mean(TS_comp), .groups = "drop") %>%  # mean TS by echoes
    mutate(TSclas = round(mTS)) %>%
    group_by(TSclas) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(
      percentage = count / sum(count),
      sigma_bs = 10^(TSclas / 10)  # Convert echoes TS to backscattering cross-section
    ) %>%
    # Join with total NASC from db_nasc 
    mutate(db_nasc %>% 
                  group_by(Horz_Slice_Idx) %>% 
                  summarise(sumproduct = sum(NASC*Nb_good_pings),# sumproduct by depth layer 
                            goodpings = sum(Nb_good_pings),# good pings 
                            sump_pings = sumproduct/goodpings,# sumproduct/good pings
                            .groups = "drop") %>% 
                  summarise(NASC= sum(sump_pings))) %>% # totNASc
    mutate(
      # here I mimic the procedure from the same function of StoX package: https://stoxproject.github.io/RstoxBase/reference/AcousticDensity.html
      NASCbyTS = (sigma_bs*count / sum(sigma_bs*count)) * NASC # NASC per bin
    ) %>%
    # Calculate Length, Weight, Abundance, and Biomass
    mutate(
      # before starting, add Info from the transect
      Type = "Overall",
      Info = as.POSIXct(min(db_nasc$Time_S), format = "%d/%m/%Y %H:%M:%OS", tz = "UTC"),
      Info = format(Info, "%b_%d_%Y"),
      Year = format(as.POSIXct(min(db_nasc$Time_S), format = "%d/%m/%Y %H:%M:%OS", tz = "UTC"),format = "%Y"),
      Month = format(as.POSIXct(min(db_nasc$Time_S), format = "%d/%m/%Y %H:%M:%OS", tz = "UTC"),format = "%m"),
      Day =  format(as.POSIXct(min(db_nasc$Time_S), format = "%d/%m/%Y %H:%M:%OS", tz = "UTC"),format = "%d"),
      Time_Min =   abs(as.numeric(as.POSIXct(min(db_nasc$Time_S), format = "%d/%m/%Y %H:%M:%OS", tz = "UTC")-as.POSIXct(max(db_nasc$Time_E), format = "%d/%m/%Y %H:%M:%OS", tz = "UTC"))),
      Distance = max(db_nasc$Dist_E), # in meter
      SpeedVes =  (Distance/1852)/(Time_Min/60), # knots
      Depth = max(db_nasc$Depth_max), # in meter
      
      Len = 10^((TSclas - b20) / 20),  # Convert TS to fish length
      W = a * Len^b,  # Compute fish weight
      
      abund_nm2_byTS = NASCbyTS / (4 * pi * sigma_bs),
      abund_hectar_byTS = abund_nm2_byTS / 343,
      abund_m2_byTS = abund_hectar_byTS / 10000,
      biomass_nm2_byTS = W * abund_nm2_byTS,
      biomass_hectar_byTS = biomass_nm2_byTS / 343,
      biomass_m2_byTS = biomass_hectar_byTS / 10000
    ) %>%
    mutate(
      mean_abund_hectar = mean(abund_hectar_byTS, na.rm = TRUE),
      tot_abund_hectar = sum(abund_hectar_byTS, na.rm = TRUE),
      mean_biomass_hectar = mean(biomass_hectar_byTS, na.rm = TRUE),
      tot_biomass_hectar = sum(biomass_hectar_byTS, na.rm = TRUE),
      # proxies of community structure 
      mean_TS = mean(rep(TSclas,count)),
      median_TS = median(rep(TSclas,count)),
      skewness_TS = skewness(rep(TSclas,count)), # <0 left skewed; >0 right skewed
      IQR_TS = IQR(rep(TSclas,count)) #interquartile range
    )
  
  # Apply TS class filter while maintaining the same columns
  db_TS_hist_mask <- db_TS_hist %>%
    filter(TSclas <= TS_min & TSclas >= TS_max ) %>%
    mutate(Type = "Masked") %>%
    mutate(
      NASC = sum(NASCbyTS, na.rm = TRUE),
      mean_abund_hectar = mean(abund_hectar_byTS, na.rm = TRUE),
      tot_abund_hectar = sum(abund_hectar_byTS, na.rm = TRUE),
      mean_biomass_hectar = mean(biomass_hectar_byTS, na.rm = TRUE),
      tot_biomass_hectar = sum(biomass_hectar_byTS, na.rm = TRUE),
      # proxies of community structure 
      mean_TS = mean(rep(TSclas,count)),
      median_TS = median(rep(TSclas,count)),
      skewness_TS = skewness(rep(TSclas,count)),
      IQR_TS = IQR(rep(TSclas,count)) #interquartile range
    )
  
  # Combine total and masked datasets while ensuring same structure
  dbfin <- bind_rows(db_TS_hist, db_TS_hist_mask) %>% select(Info,Year,Month,Day,Time_Min,Depth,Distance,SpeedVes,Type,NASC,tot_abund_hectar,tot_biomass_hectar,mean_TS,median_TS,skewness_TS,IQR_TS,TSclas,Len,W,everything())
  
  # Remove variables from the "Total" part of the db (since we don't have L-W rel)
  dbfin <- dbfin %>%
    mutate(biomass_nm2_byTS = ifelse(Type == "Masked", biomass_nm2_byTS, NA),
           biomass_hectar_byTS = ifelse(Type == "Masked", biomass_hectar_byTS, NA),
           biomass_m2_byTS = ifelse(Type == "Masked", biomass_m2_byTS, NA),
           tot_biomass_hectar = ifelse(Type == "Masked", tot_biomass_hectar, NA),
           mean_biomass_hectar = ifelse(Type == "Masked", mean_biomass_hectar, NA),
           Len = ifelse(Type == "Masked", Len, NA),
           W = ifelse(Type == "Masked", W, NA),
           mean_TS = ifelse(Type == "Masked", mean_TS, NA),
           median_TS = ifelse(Type == "Masked", median_TS, NA),
           skewness_TS = ifelse(Type == "Masked", skewness_TS, NA),
           IQR_TS = ifelse(Type == "Masked", IQR_TS, NA)
           )
   
  return(dbfin)
}

# Set wd automatically in the folder the script is stored
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
# Read in your datasets:
# dbTS from ESP3: see User manual point 2: "Single Target and Echoes detection"
# dbNASC from ESP3: see User manual point 3: "Echo integration of the transect"
dbTS <- read_excel("TStot_Echohist_Aug_09_2022.xlsx", sheet = "Tracked Targets") 
dbNASC <- read_excel("NASCtot_Aug_09_2022.xlsx") 

#dbTS <- read_excel("C:/Users/frma6502/Desktop/SU/EKOLOD/esp3testH4/TShist.xlsx",sheet = "Tracked Targets")
#dbNASC <- read_excel("C:/Users/frma6502/Desktop/SU/EKOLOD/esp3testH4/regionby10x10_badpings.xlsx")

#######################################################################################
# Calculate Total and "by TS" abundance/NASC/biomass for the specie/group of interest #
#######################################################################################

# Define species parameters and TS mask range 
a <- 0.0054 # a from L-W rel for clupeids (W=a*Len^b)      
b <- 3.04 # b from L-W rel for clupeids (W=a*Len^b)          
b20 <- -68.6 # b20 value in the Baltic from Didrikas & Hansson 2004
TS_max <- -40   # assumed TS range for small pelagic fish/clupeids
TS_min <- -60   # assumed TS range for small pelagic fish/clupeids
# Call the function and View first rows of the final db
dbfin <- process_ESP3_data(dbNASC, dbTS, a, b, b20, TS_max, TS_min);head(dbfin)


##################
# various PLOTS  #
##################
# change "x = TSclas" to x=Len for length based plots

# plot TS hist total
ggplot(dbfin, aes(x = TSclas, y = percentage*100, fill = Type, color =Type )) +
  geom_bar(stat = "identity", alpha =1  ,position= "identity") +
  labs(title = paste0("Echos Target Strength Distribution: ", dbfin$Info), x = "mean TS (dB) of echoes", y = "Percentage (%)") + 
  theme_minimal();unique(dbfin$mean_TS);unique(dbfin$median_TS);unique(dbfin$skewness_TS);unique(dbfin$IQR_TS)

# plot NASC hist
ggplot(dbfin, aes(x = TSclas, y = NASCbyTS, fill = Type, color =Type )) +
  geom_bar(stat = "identity", alpha =1  ,position= "identity") +
  labs(title = paste0("NASC Distribution: ", dbfin$Info), x = "mean TS (dB) of echoes", y = "NASC") + 
  theme_minimal(); unique(dbfin$NASC)

# plot Abund hist
ggplot(dbfin, aes(x = TSclas, y = abund_hectar_byTS, fill = Type, color =Type )) +
  geom_bar(stat = "identity", alpha =1  ,position= "identity") +
  labs(title = paste0("Abundance Distribution: ", dbfin$Info) , x = "mean TS (dB) of echoes", y = "Target/ha") + 
  theme_minimal(); unique(dbfin$tot_abund_hectar)

 # plot Biom hist (considered only the masked part)
ggplot(dbfin%>% dplyr::filter(Type=="Masked"), aes(x = TSclas, y = biomass_hectar_byTS, fill = Type, color =Type)) + geom_bar(stat = "identity", alpha =1  ,position= "identity") +
  labs(title = paste0("Biomass Distribution: ", dbfin$Info), x = "mean TS (dB) of echoes", y = "gr/ha") +
  theme_minimal(); unique(dbfin$tot_biomass_hectar)


###############################################
# Save the final db
write.csv(dbfin,paste0("Final_dbs/db_",unique(dbfin$Info),"_",TS_min, TS_max,".csv"), row.names = F)
