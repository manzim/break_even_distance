# ==========================================================
# Pyrolysis Cost & Payable Model — R version
# Tractor-only + KTBL + Moisture
# ==========================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)


# (1) ASCII-safe labels to avoid encoding/font issues on servers
lab_eur_t_asrec <- "EUR/t as-received"
lab_eur_t_dm    <- "EUR/t DM"

# (2) Safe plotting wrapper: show plot errors in the UI instead of blank plot
safePlot <- function(expr) {
  tryCatch(expr, error = function(e) {
    plot.new()
    text(0.5, 0.6, "Plot error on server:", cex = 1.1)
    text(0.5, 0.45, conditionMessage(e), cex = 0.9)
  })
}

# ==========================================================
# UI
# ==========================================================
ui <- fluidPage(
  titlePanel("Pyrolysis Payable Price & Break-even Radius Model"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      h4("Plant (Pyrolysis) Parameters"),
      numericInput("Qin_DM_h", "Feed rate (t DM/h)", 0.299),
      numericInput("Y_char", "Char yield (t char / t DM)", 0.25),
      numericInput("E_elec", "Net electricity (kW)", 130),
      numericInput("E_heat", "Net heat (kW)", 200),
      numericInput("Hop_year", "Operating hours per year", 8000),
      
      h4("Prices & Revenues"),
      numericInput("P_char", "Biochar price (EUR/t)", 600),
      numericInput("P_el", "Electricity price (EUR/kWh)", 0.11),
      numericInput("P_heat", "Heat price (EUR/kWh_th)", 0.06),
      
      h4("Plant Costs"),
      numericInput("n_ops", "Operators per shift", 1),
      numericInput("w_hour", "Wage (EUR/h)", 28),
      numericInput("OM_hour", "Fixed O&M (EUR/h)", 30),
      numericInput("P_buy", "Grid electricity price (EUR/kWh)", 0.28),
      numericInput("E_buy_kWh", "Grid purchase (kWh/h)", 0),
      
      h4("KTBL Upstream (tractor only)"),
      numericInput("Tractor_eur_h", "Tractor cost (EUR/h)", 41.84),
      numericInput("PTO_eur_h", "PTO chipper cost (EUR/h)", 22.63),
      numericInput("Bucket_eur_t", "Bucket cost (EUR/t)", 0.39),
      numericInput("Front_eur_h", "Front loader (EUR/h)", 8.68),
      numericInput("Chipper_m3_h", "Chipper capacity (m3/h)", 25),
      numericInput("BulkDensity", "Bulk density (t/m3)", 0.30),
      numericInput("Handling_tph", "Handling capacity (t/h)", 20),
      numericInput("Tractor_speed", "Tractor speed (km/h)", 40),
      numericInput("PayloadTractor", "Tractor payload (t)", 22 * 0.30),
      numericInput("Body_surcharge", "Surcharge (EUR/t per trip)", 0.82),
      
      h4("Moisture"),
      sliderInput("MC_asrec", "Moisture content (as-received)", 0, 0.5, value = 0.35, step = 0.01),
      
      h4("Distance sweep"),
      sliderInput("max_dist", "Max distance (km)", 10, 300, 200, step = 10),
      
      h4("Export"),
      downloadButton("export_csv", "Export CSV"),
      
      hr(),
      h4("Diagnostics"),
      verbatimTextOutput("diagText")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel(
          "Delivered Cost vs Distance",
          h3("Delivered cost vs distance"),
          plotOutput("distPlot", height = "420px"),
          h4("Key KPIs"),
          tableOutput("kpiTable")
        ),
        
        tabPanel(
          "Payable vs Biochar Price",
          h3("Payable chip price vs Biochar price"),
          plotOutput("biocharPlot", height = "420px")
        )
      )
    )
  )
)

# ==========================================================
# SERVER
# ==========================================================
server <- function(input, output, session) {
  
  # Core calculations
  results <- reactive({
    # guard against invalid values
    validate(
      need(input$Qin_DM_h > 0, "Feed rate (t DM/h) must be > 0"),
      need(input$Chipper_m3_h > 0, "Chipper capacity must be > 0"),
      need(input$BulkDensity > 0, "Bulk density must be > 0"),
      need(input$Handling_tph > 0, "Handling capacity must be > 0"),
      need(input$Tractor_speed > 0, "Tractor speed must be > 0"),
      need(input$PayloadTractor > 0, "Payload must be > 0")
    )
    
    # Plant hourly revenues
    Qchar_h <- input$Y_char * input$Qin_DM_h
    Rchar   <- input$P_char * Qchar_h
    Rel     <- input$P_el * input$E_elec
    Rheat   <- input$P_heat * input$E_heat
    Rev     <- Rchar + Rel + Rheat
    
    # Hourly costs
    Clab <- input$n_ops * input$w_hour
    Com  <- input$OM_hour
    Cbuy <- input$P_buy * input$E_buy_kWh
    
    # Max payable chip price (EUR/t DM)
    P_chip_DM <- (Rev - (Clab + Com + Cbuy)) / input$Qin_DM_h
    
    # Convert to as-received
    P_chip_asrec <- P_chip_DM * (1 - input$MC_asrec)
    
    # Upstream costs
    C_chip_mach <- (input$Tractor_eur_h + input$PTO_eur_h) /
      (input$Chipper_m3_h * input$BulkDensity)
    
    Labor_chip_hpt <- 1 / (input$Chipper_m3_h * input$BulkDensity)
    C_chip <- C_chip_mach + input$w_hour * Labor_chip_hpt
    
    C_hand_mach <- input$Bucket_eur_t + input$Front_eur_h / input$Handling_tph
    Labor_hand  <- 1 / input$Handling_tph
    C_handle <- C_hand_mach + input$w_hour * Labor_hand
    
    C_tkm <- (input$Tractor_eur_h + input$w_hour) /
      (input$Tractor_speed * input$PayloadTractor)
    
    Backhaul <- 2
    
    BE_trac <- max(
      0,
      (P_chip_DM - (C_chip + C_handle + input$Body_surcharge)) / (Backhaul * C_tkm)
    )
    
    CharOutput_yr <- input$Y_char * input$Qin_DM_h * input$Hop_year
    Qin_asrec_yr  <- input$Qin_DM_h * input$Hop_year / (1 - input$MC_asrec)
    
    list(
      P_chip_DM = P_chip_DM,
      P_chip_asrec = P_chip_asrec,
      C_chip = C_chip,
      C_handle = C_handle,
      C_tkm = C_tkm,
      BE_trac = BE_trac,
      CharOutput_yr = CharOutput_yr,
      Qin_asrec_yr = Qin_asrec_yr
    )
  })
  
  # Diagnostics shown in sidebar (useful on shinyapps.io)
  output$diagText <- renderText({
    r <- results()
    paste0(
      "Payable (DM): ", round(r$P_chip_DM, 2), " ", lab_eur_t_dm, "\n",
      "Payable (as-rec): ", round(r$P_chip_asrec, 2), " ", lab_eur_t_asrec, "\n",
      "BE radius (km): ", round(r$BE_trac, 1), "\n",
      "Note: labels are ASCII-safe for cloud rendering."
    )
  })
  
  # Plot 1
  output$distPlot <- renderPlot({
    safePlot({
      r <- results()
      d <- seq(0, input$max_dist, by = 5)
      Backhaul <- 2
      
      cost <- (r$C_chip + r$C_handle + input$Body_surcharge) * (1 - input$MC_asrec) +
        Backhaul * r$C_tkm * d * (1 - input$MC_asrec)
      
      df <- data.frame(km = d, cost = cost, payable = r$P_chip_asrec)
      
      ggplot(df) +
        geom_line(aes(x = km, y = cost, color = "Delivered cost"), linewidth = 1.1) +
        geom_hline(aes(yintercept = payable, color = "Payable"), linetype = "dashed", linewidth = 1.1) +
        scale_color_manual(values = c("Delivered cost" = "red", "Payable" = "blue")) +
        labs(x = "Distance (km)", y = lab_eur_t_asrec, color = "", title = "Delivered Cost vs Distance") +
        theme_minimal(base_size = 15)
    })
  })
  
  # KPI Table
  output$kpiTable <- renderTable({
    r <- results()
    data.frame(
      KPI = c(
        paste0("Payable chip price (", lab_eur_t_dm, ")"),
        paste0("Payable chip price (", lab_eur_t_asrec, ")"),
        "Break-even radius (km, tractor)",
        "Char output (t/year)",
        "Required as-received intake (t/year)"
      ),
      Value = c(
        round(r$P_chip_DM, 2),
        round(r$P_chip_asrec, 2),
        round(r$BE_trac, 1),
        round(r$CharOutput_yr, 1),
        round(r$Qin_asrec_yr, 1)
      )
    )
  })
  
  # Plot 2
  output$biocharPlot <- renderPlot({
    safePlot({
      r <- results()  # cache once
      
      Pchars <- seq(200, 1000, by = 20)
      df <- data.frame(Pchar = Pchars)
      
      df$P_chip_DM <- (
        (df$Pchar * input$Y_char * input$Qin_DM_h) +
          input$P_el * input$E_elec + input$P_heat * input$E_heat -
          (input$n_ops * input$w_hour + input$OM_hour + input$P_buy * input$E_buy_kWh)
      ) / input$Qin_DM_h
      
      df$BE <- pmax(
        0,
        (df$P_chip_DM - (r$C_chip + r$C_handle + input$Body_surcharge)) / (2 * r$C_tkm)
      )
      
      ggplot(df) +
        geom_line(aes(x = Pchar, y = P_chip_DM, color = "Payable chip price"), linewidth = 1.1) +
        geom_line(aes(x = Pchar, y = BE, color = "BE radius (km)"), linewidth = 1.1) +
        scale_color_manual(values = c("Payable chip price" = "darkgreen", "BE radius (km)" = "purple")) +
        labs(x = "Biochar price (EUR/t)", y = "Value", color = "",
             title = "Payable Chip Price & BE Radius vs Biochar Price") +
        theme_minimal(base_size = 15)
    })
  })
  
  # Export CSV
  output$export_csv <- downloadHandler(
    filename = function() { "pyrolysis_results.csv" },
    content = function(file) {
      r <- results()
      df <- data.frame(Parameter = names(r), Value = unlist(r))
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
