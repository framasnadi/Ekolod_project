library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(e1071)
#############################################################
#                                                           #
#    Processing ESP3 exported data after Echo integration   #
#                                                           #
#   Author: Francesco Masnadi (DEEP, Stockholm University)  #
#                                                           #
#############################################################

# Data processing function
process_ESP3_data <- function(db_nasc, db_hist, a, b, b20, TS_min, TS_max,masked_label = "SmallPel") {
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
      Distance = max(db_nasc$Dist_E)-min(db_nasc$Dist_S) , # in meter
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
    mutate(Type = masked_label) %>%
    mutate(
      NASC = sum(NASCbyTS, na.rm = TRUE),
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
  
  # Combine total and masked datasets while ensuring same structure
  dbfin <- bind_rows(db_TS_hist, db_TS_hist_mask) %>% select(Info,Year,Month,Day,Time_Min,Depth,Distance,SpeedVes,Type,NASC,tot_abund_hectar,tot_biomass_hectar,mean_TS,median_TS,skewness_TS,IQR_TS,TSclas,Len,W,everything())
  
  # Remove variables from the "Total" part of the db (e.g. we don't have L-W rel)
  dbfin <- dbfin %>%
    mutate(biomass_nm2_byTS = ifelse(Type == masked_label, biomass_nm2_byTS, NA),
           biomass_hectar_byTS = ifelse(Type == masked_label, biomass_hectar_byTS, NA),
           biomass_m2_byTS = ifelse(Type == masked_label, biomass_m2_byTS, NA),
           tot_biomass_hectar = ifelse(Type == masked_label, tot_biomass_hectar, NA),
           mean_biomass_hectar = ifelse(Type == masked_label, mean_biomass_hectar, NA),
           Len = ifelse(Type == masked_label, Len, NA),
           W = ifelse(Type == masked_label, W, NA),
           mean_TS = ifelse(Type == masked_label, mean_TS, NA),
           median_TS = ifelse(Type == masked_label, median_TS, NA),
           skewness_TS = ifelse(Type == masked_label, skewness_TS, NA),
           IQR_TS = ifelse(Type == masked_label, IQR_TS, NA)
    )
  
  return(dbfin)
}

# UI ########################################################################
ui <- fluidPage(
  titlePanel("ESP3 Hydroacoustic Data Visualization & Processing"),
  helpText(
    tags$span("Need help? ",
              tags$a(
                href = "https://www.su.se/english/profiles/frma6502-1.665005",
                HTML("Francesco Masnadi (DEEP - Stockholm University)"), 
                style = "color: #007BFF; text-decoration: none;"
              )
    )
  ),
  
  downloadButton("download_manual", "Download User Manual"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("dbTS from ESP3: see User manual point 2 Single Target and Echoes detection; dbNASC from ESP3: see User manual point 3: Echo integration of the transect"),
      fileInput("ts_file", "Upload dbTS Excel File (sheet: 'Tracked Targets')",
                accept = c(".xlsx", ".xls")),
      fileInput("nasc_file", "Upload dbNASC Excel File (sheet: '70kHz' or '200kHz')",
                accept = c(".xlsx", ".xls")),
      
      helpText("Maximum TS (larger fish/target). Used to filter target species."),
      numericInput("TS_min", "TS Max (e.g., -40 dB for clupeids)", value = -40),
      helpText("Minimum TS (smaller fish/target). Echoes weaker than this are excluded."),
      numericInput("TS_max", "TS Min (e.g., -60 dB for clupeids)", value = -60),
     #helpText("Rename the filter as:"),
      textInput("customLabel", "Rename the TS mask as:", value = "SmallPel"),
      helpText("Species-specific empirical relationship between TS and length (Length = 10^((TS - b20)/20) e.g.; from Didrikas & Hansson 2004 for clupeids in the Baltic)"),
      numericInput("b20", "b20 (e.g., -67.8 for clupeids)", value = -67.8),
      helpText("Length-Weight relationship (W=a*Len^b)"),
      numericInput("a", "Parameter a (e.g.; 0.0054 for clupeids)", value = 0.0054),
      #helpText("Length-Weight relationship (W=a*Len^b)"),
      numericInput("b", "Parameter b (e.g.; 3.04 for clupeids)", value = 3.04)
    ),
    
    mainPanel(
      # Dropdown placed ABOVE the plot
      fluidRow(
        column(12,
               selectInput("plot_type", "Select Metric to Plot", 
                           choices = c("TS", "NASC", 
                                       "Abundance", "Biomass"),
                           width = "50%")
        )
      ),
      # Bold and larger metric summary
      downloadButton("downloadData", "Download Processed Data"),
      tags$div(style = "margin-top:10px; font-weight:bold; font-size:16px; color:#333;",
               uiOutput("summaryText")),
      plotOutput("mainPlot", height = "450px"),
      uiOutput("vesselSummary")
    )
  )
)

# SERVER ########################################################################
# Set max upload size to 50 MB
options(shiny.maxRequestSize = 150 * 1024^2)
server <- function(input, output) {
  dataset <- reactive({
    req(input$ts_file, input$nasc_file)
    dbTS <- read_excel(input$ts_file$datapath, sheet = "Tracked Targets")
    dbNASC <- read_excel(input$nasc_file$datapath)
    

    process_ESP3_data(dbNASC, dbTS, input$a, input$b, input$b20, input$TS_min, input$TS_max,masked_label = input$customLabel )
  })


  output$download_manual <- downloadHandler(
    filename = function() {
      "Processing_hydroacoustic_User_manual.pdf"
    },
    content = function(file) {
      file.copy("Processing_hydroacoustic_User_manual.pdf", file)
    }
  )
  
  output$summaryText <- renderText({
    df <- dataset()
    masked <- df %>% filter(Type == input$customLabel)
    total <- df %>% filter(Type == "Overall")
    plot_type <- input$plot_type
    
    if (nrow(masked) == 0 || nrow(total) == 0) return("No sufficient data available.")
    
    if (plot_type == "TS") {
      skew_val <- round(max(masked$skewness_TS), 2)
      
      message <- if (skew_val > 0) {
        "The Masked community is dominated by smaller individuals"
      } else if (skew_val < 0) {
        "The Masked community is dominated by larger individuals"
      } else {
        "The Masked community has a symmetric size distribution"
      }
      
      return(HTML(paste0(
        "<b>Skewness of the Masked distribution:</b> ", skew_val, "<br/>", message
      )))
    } else if (plot_type == "NASC") {
      HTML(paste0(
        "<b>Total Overall NASC (Nautical Area Scattering Coefficient): <b>", round(sum(total$NASCbyTS),2), "<br>",
        "<b>Total Masked NASC (Nautical Area Scattering Coefficient): <b>", round(sum(masked$NASCbyTS),2)
      ))
    } else if (plot_type == "Abundance") {
      HTML(paste0(
        "<b>Total Overall Abundance (targets/ha): <b>", round(total$tot_abund_hectar[1],0), "<br>",
        "<b>Total Masked Abundance (targets/ha): <b>", round(masked$tot_abund_hectar[1], 0)
      ))
    } else if (plot_type == "Biomass") {
      paste0("Total Masked Biomass (gr/ha): ", round(masked$tot_biomass_hectar[1], 0))
    }
  })
  
  
  output$mainPlot <- renderPlot({
    df <- dataset()
    plot_type <- input$plot_type
    
    if (plot_type == "TS") {
      avg_TS <- df %>% filter(Type == input$customLabel) %>% summarise(avg = mean(rep(TSclas,count))) 
      Median_TS <- df %>% filter(Type == input$customLabel) %>% summarise(mdn = median(rep(TSclas,count))) 
      Skss_TS <- df %>% filter(Type == input$customLabel) %>% summarise(mdn = skewness(rep(TSclas,count))) 
      
      ggplot(df, aes(x = TSclas, y = percentage * 100, fill = Type, color =Type)) +
        geom_bar(stat = "identity", position = "identity") +
        labs(title = paste0("Echos Target Strength Distribution: ", df$Info), x = "mean TS (dB) of echoes", y = "Percentage (%)")  +
        annotate("text", x = Median_TS$mdn,y=(df %>% filter(Type == input$customLabel) %>% filter(percentage == max(percentage, na.rm = TRUE)) %>% pull(percentage)*100)+0.55 , vjust = -0.5, label = paste0("Median: ", Median_TS, " dB"),  color = "black", fontface = "bold")+
        geom_vline(xintercept = Median_TS$mdn, linetype = "dashed", color = "black") + 
        annotate("text", x = avg_TS$avg,y=(df %>% filter(Type == input$customLabel) %>% filter(percentage == max(percentage, na.rm = TRUE)) %>% pull(percentage)*100)+0.1 , vjust = -0.5,
                 label = paste0("Mean: ", round(avg_TS$avg,1), " dB"),
                 color = "black", fontface = "bold")+
        geom_vline(xintercept = avg_TS$avg, linetype = "dashed", color = "black") + 
        theme_minimal()
      
    } else if (plot_type == "NASC") {
      avg_TS <- df %>% filter(Type == input$customLabel) %>% summarise(avg = mean(rep(TSclas,NASCbyTS))) 
      Median_TS <- df %>% filter(Type == input$customLabel) %>% summarise(mdn = median(rep(TSclas,NASCbyTS))) 
      
      ggplot(df, aes(x = TSclas, y = NASCbyTS, fill = Type, color =Type)) +
        geom_bar(stat = "identity", position = "identity") +
        labs(title = paste0("NASC Distribution: ", df$Info), x = "mean TS (dB) of echoes", y = "NASC (m2/m2nmi-2)")  + 
        annotate("text", x = Median_TS$mdn,y=(df %>% filter(Type == input$customLabel) %>% filter(NASCbyTS == max(NASCbyTS, na.rm = TRUE)) %>% pull(NASCbyTS))+2 , vjust = -0.5,
                 label = paste0("Median: ", Median_TS, " dB"),
                 color = "black", fontface = "bold")+
        geom_vline(xintercept = Median_TS$mdn, linetype = "dashed", color = "black") + 
        annotate("text", x = avg_TS$avg,y=(df %>% filter(Type == input$customLabel) %>% filter(NASCbyTS == max(NASCbyTS, na.rm = TRUE)) %>% pull(NASCbyTS))+0.5 , vjust = -0.5,
                 label = paste0("Mean: ", round(avg_TS$avg,1), " dB"),
                 color = "black", fontface = "bold")+
        geom_vline(xintercept = avg_TS$avg, linetype = "dashed", color = "black") + 
        theme_minimal()
      
    } else if (plot_type == "Abundance") {
      avg_TS <- df %>% filter(Type == input$customLabel) %>% summarise(avg = mean(rep(TSclas,abund_hectar_byTS))) 
      Median_TS <- df %>% filter(Type == input$customLabel) %>% summarise(mdn = median(rep(TSclas,abund_hectar_byTS)))
      
      ggplot(df, aes(x = TSclas, y = abund_hectar_byTS, fill = Type, color= Type)) +
        geom_bar(stat = "identity", position = "identity") +
        labs(title = paste0("Abundance Distribution: ", df$Info), x = "mean TS (dB) of echoes", y = "Targets/ha")  + 
        annotate("text", x = Median_TS$mdn,y=(df %>% filter(Type == input$customLabel) %>% filter(abund_hectar_byTS == max(abund_hectar_byTS, na.rm = TRUE)) %>% pull(abund_hectar_byTS))+250 , vjust = -0.5,
                 label = paste0("Median: ", Median_TS, " dB"),
                 color = "black", fontface = "bold")+
        geom_vline(xintercept = Median_TS$mdn, linetype = "dashed", color = "black") + 
        annotate("text", x = avg_TS$avg,y=(df %>% filter(Type == input$customLabel) %>% filter(abund_hectar_byTS == max(abund_hectar_byTS, na.rm = TRUE)) %>% pull(abund_hectar_byTS))+2 , vjust = -0.5,
                 label = paste0("Mean: ", round(avg_TS$avg,1), " dB"),
                 color = "black", fontface = "bold")+
        geom_vline(xintercept = avg_TS$avg, linetype = "dashed", color = "black") + 
        theme_minimal()
      
    } else if (plot_type == "Biomass") {
      avg_TS <- df %>% filter(Type == input$customLabel) %>% summarise(avg = mean(rep(TSclas,biomass_hectar_byTS))) 
      Median_TS <- df %>% filter(Type == input$customLabel) %>% summarise(mdn = median(rep(TSclas,biomass_hectar_byTS)))
      
      ggplot(df %>% filter(Type == input$customLabel), aes(x = TSclas, y = biomass_hectar_byTS, fill = Type,color =Type)) +
        geom_bar(stat = "identity", position = "identity") +
        labs(title = paste0("Biomass Distribution: ", df$Info), x = "mean TS (dB) of echoes", y = "gr/ha")   + 
        annotate("text", x = Median_TS$mdn,y=(df %>% filter(Type == input$customLabel) %>% filter(biomass_hectar_byTS == max(biomass_hectar_byTS, na.rm = TRUE)) %>% pull(biomass_hectar_byTS))+10 , vjust = -0.5,
                 label = paste0("Median: ", Median_TS, " dB"),
                 color = "black", fontface = "bold")+
        geom_vline(xintercept = Median_TS$mdn, linetype = "dashed", color = "black") + 
        annotate("text", x = avg_TS$avg,y=(df %>% filter(Type == input$customLabel) %>% filter(biomass_hectar_byTS == max(biomass_hectar_byTS, na.rm = TRUE)) %>% pull(biomass_hectar_byTS))+2 , vjust = -0.5,
                 label = paste0("Mean: ", round(avg_TS$avg,1), " dB"),
                 color = "black", fontface = "bold")+
        geom_vline(xintercept = avg_TS$avg, linetype = "dashed", color = "black") + 
        theme_minimal()
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      df <- dataset()
      date_tag <- if (!is.null(df$Info[1])) df$Info[1] else format(Sys.Date(), "%b_%d_%Y")
      paste0("db_", date_tag, "_", input$customLabel , ".csv")
    },
    content = function(file) {
      df <- dataset()
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$vesselSummary <- renderUI({
    df <- dataset()
    masked <- df %>% filter(Type == input$customLabel)
    
    # Use values from first row (they're identical across rows)
    if (nrow(masked) == 0) return(NULL)
    
    time_min <- masked$Time_Min[1]
    dist_m <- masked$Distance[1]
    speed_knots <- round(masked$SpeedVes[1], 2)
    mdepth <- round(masked$Depth[1], 2)
    
    HTML(paste0(
      "<b>Max Depth:</b> ", mdepth, " m<br>",
      "<b>Vessel Speed:</b> ", speed_knots, " knots<br>",
      "<b>Distance:</b> ", round(dist_m,1), " m<br>",
      "<b>Time:</b> ", round(time_min, 1), " minutes"
    ))
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

