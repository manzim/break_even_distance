# ==========================================================
# Pyrolysis Cost & Payable Model — R version of Plantflip2.gms
# Tractor-only + KTBL + Moisture + Monte Carlo
# ==========================================================

# ---- packages ----
need <- c("shiny","ggplot2","dplyr","scales")
have <- need %in% rownames(installed.packages())
if(any(!have)) install.packages(need[!have], repos="https://cloud.r-project.org")
invisible(lapply(need, require, character.only=TRUE))


# ==========================================================
# UI
# ==========================================================
ui <- fluidPage(
  titlePanel("Pyrolysis Payable Price & Break-even Radius Model"),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Plant (Pyrolysis) Parameters"),
                 numericInput("Qin_DM_h", "Feed rate (t DM/h)", 0.299),
                 numericInput("Y_char", "Char yield (t char / t DM)", 0.25),
                 numericInput("E_elec", "Net electricity (kW)", 130),
                 numericInput("E_heat", "Net heat (kW)", 200),
                 numericInput("Hop_year", "Operating hours per year", 8000),
                 
                 h4("Prices & Revenues"),
                 numericInput("P_char", "Biochar price (€/t)", 600),
                 numericInput("P_el", "Electricity price (€/kWh)", 0.11),
                 numericInput("P_heat", "Heat price (€/kWh_th)", 0.06),
                 
                 h4("Plant Costs"),
                 numericInput("n_ops", "Operators per shift", 1),
                 numericInput("w_hour", "Wage (€/h)", 28),
                 numericInput("OM_hour", "Fixed O&M (€/h)", 30),
                 numericInput("P_buy", "Grid electricity price (€/kWh)", 0.28),
                 numericInput("E_buy_kWh", "Grid purchase (kWh/h)", 0),
                 
                 h4("KTBL Upstream (tractor only)"),
                 numericInput("Tractor_eur_h", "Tractor cost (€/h)", 41.84),
                 numericInput("PTO_eur_h", "PTO chipper cost (€/h)", 22.63),
                 numericInput("Bucket_eur_t", "Bucket cost (€/t)", 0.39),
                 numericInput("Front_eur_h", "Front loader (€/h)", 8.68),
                 numericInput("Chipper_m3_h", "Chipper capacity (m³/h)", 25),
                 numericInput("BulkDensity", "Bulk density (t/m³)", 0.30),
                 numericInput("Handling_tph", "Handling capacity (t/h)", 20),
                 numericInput("Tractor_speed", "Tractor speed (km/h)", 40),
                 numericInput("PayloadTractor", "Tractor payload (t)", 22*0.30),
                 numericInput("Body_surcharge", "Surcharge (€/t per trip)", 0.82),
                 
                 h4("Moisture"),
                 sliderInput("MC_asrec", "Moisture content (as-received)", 0, 0.5, value=0.35, step=0.01),
                 
                 h4("Monte Carlo"),
                 checkboxInput("enable_mc", "Enable Monte Carlo", TRUE),
                 numericInput("mc_runs", "Runs", 800),
                 numericInput("MC_min", "Moisture min", 0.22),
                 numericInput("MC_max", "Moisture max", 0.45),
                 
                 h4("Distance sweep"),
                 sliderInput("max_dist", "Max distance (km)", 10, 300, 200, step=10),
                 
                 h4("Export"),
                 downloadButton("export_csv", "Export CSV")
    ),
    
    mainPanel(width=8,
              
              tabsetPanel(
                tabPanel("Delivered Cost vs Distance",
                         
                         h3("Delivered cost (€/t as-received) vs distance"),
                         
                         plotOutput("distPlot", height="420px"),
                         
                         h4("Key KPIs"),
                         tableOutput("kpiTable"),
                         
                         verbatimTextOutput("debug1")
                ),
                
                tabPanel("Payable vs Biochar Price",
                         h3("Payable chip price vs Biochar price"),
                         
                         plotOutput("biocharPlot", height="420px")
                ),
                
                tabPanel("Monte-Carlo diagnostics",
                         h3("Moisture Monte Carlo — Payable chip price spread"),
                         plotOutput("mcPlot", height="420px")
                )
              )
    )
  )
)



# ==========================================================
# SERVER LOGIC
# ==========================================================
server <- function(input, output, session) {
  
  # ========= CORE CALCULATIONS ===================================================
  
  results <- reactive({
    
    # ----- plant hourly revenues -----
    Qchar_h <- input$Y_char * input$Qin_DM_h
    Rchar   <- input$P_char * Qchar_h
    Rel     <- input$P_el   * input$E_elec
    Rheat   <- input$P_heat * input$E_heat
    Rev     <- Rchar + Rel + Rheat
    
    # ----- hourly costs -----
    Clab <- input$n_ops * input$w_hour
    Com  <- input$OM_hour
    Cbuy <- input$P_buy * input$E_buy_kWh
    
    # ======= MAX PAYABLE CHIP PRICE (€/t DM) =======
    P_chip_DM <- (Rev - (Clab + Com + Cbuy)) / input$Qin_DM_h
    
    # ======= Convert to as-received =======
    P_chip_asrec <- P_chip_DM * (1 - input$MC_asrec)
    
    # ==========================================================
    # KTBL UPSTREAM COSTS (tractor only)
    # ==========================================================
    
    # Chipping (€/t)
    C_chip_mach <- (input$Tractor_eur_h + input$PTO_eur_h) /
      (input$Chipper_m3_h * input$BulkDensity)
    
    Labor_chip_hpt <- 1 / (input$Chipper_m3_h * input$BulkDensity)
    C_chip <- C_chip_mach + input$w_hour * Labor_chip_hpt
    
    # Handling (€/t)
    C_hand_mach <- input$Bucket_eur_t + input$Front_eur_h / input$Handling_tph
    Labor_hand <- 1 / input$Handling_tph
    C_handle <- C_hand_mach + input$w_hour * Labor_hand
    
    # Transport (€/t-km)
    C_tkm <- (input$Tractor_eur_h + input$w_hour) /
      (input$Tractor_speed * input$PayloadTractor)
    
    # Radius denominator
    Backhaul <- 2
    
    # ======= Break-even radius (tractor) =======
    BE_trac <- max(
      0,
      (P_chip_DM - (C_chip + C_handle + input$Body_surcharge)) /
        (Backhaul * C_tkm)
    )
    
    # ======= Annual KPIs =======
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
  
  
  
  # ==========================================================
  # TAB 1 — Cost vs Distance
  # ==========================================================
  output$distPlot <- renderPlot({
    r <- results()
    
    d <- seq(0, input$max_dist, by=5)
    
    Backhaul <- 2
    
    cost <- (r$C_chip + r$C_handle + input$Body_surcharge) * (1 - input$MC_asrec) +
      Backhaul * r$C_tkm * d * (1 - input$MC_asrec)
    
    df <- data.frame(
      km = d,
      cost = cost,
      payable = r$P_chip_asrec
    )
    
    ggplot(df) +
      geom_line(aes(km, cost, color="Delivered cost"), size=1.1) +
      geom_hline(aes(yintercept=payable, color="Payable"), linetype="dashed", size=1.1) +
      scale_color_manual(values=c("Delivered cost"="red","Payable"="blue")) +
      labs(x="Distance (km)", y="€/t as-received", color="", title="Delivered Cost vs Distance") +
      theme_minimal(base_size=15)
  })
  
  
  # KPI TABLE
  output$kpiTable <- renderTable({
    r <- results()
    data.frame(
      KPI = c(
        "Payable chip price (€/t DM)",
        "Payable chip price (€/t as-received)",
        "Break-even radius (km, tractor)",
        "Char output (t/year)",
        "Required as-received intake (t/year)"
      ),
      Value = c(
        round(r$P_chip_DM,2),
        round(r$P_chip_asrec,2),
        round(r$BE_trac,1),
        round(r$CharOutput_yr,1),
        round(r$Qin_asrec_yr,1)
      )
    )
  })
  
  
  # ==========================================================
  # TAB 2 — Payable vs Biochar price
  # ==========================================================
  output$biocharPlot <- renderPlot({
    
    Pchars <- seq(200, 1000, by=20)
    
    df <- data.frame(Pchar=Pchars)
    
    df$P_chip_DM <- ( (df$Pchar * input$Y_char * input$Qin_DM_h) +
                        input$P_el*input$E_elec + input$P_heat*input$E_heat -
                        (input$n_ops*input$w_hour + input$OM_hour + input$P_buy*input$E_buy_kWh)
    ) / input$Qin_DM_h
    
    df$BE <- pmax(0, (df$P_chip_DM -
                        (results()$C_chip + results()$C_handle + input$Body_surcharge))
                  / (2 * results()$C_tkm))
    
    ggplot(df) +
      geom_line(aes(Pchar, P_chip_DM, color="Payable chip price"), size=1.1) +
      geom_line(aes(Pchar, BE, color="BE radius (km)"), size=1.1) +
      scale_color_manual(values=c("Payable chip price"="darkgreen","BE radius (km)"="purple")) +
      labs(x="Biochar price (€/t)", y="Value", color="", title="Payable Chip Price & BE Radius vs Biochar Price") +
      theme_minimal(base_size=15)
  })
  
  
  # ==========================================================
  # TAB 3 — Monte-Carlo moisture
  # ==========================================================
  output$mcPlot <- renderPlot({
    if(!input$enable_mc) return(NULL)
    
    mc <- runif(input$mc_runs, input$MC_min, input$MC_max)
    
    Pchip <- results()$P_chip_DM * (1 - mc)
    
    df <- data.frame(Pchip=Pchip)
    
    ggplot(df, aes(Pchip)) +
      geom_histogram(bins=30, fill="skyblue", color="white") +
      labs(x="Payable chip price (€/t as-received)", title="Monte Carlo moisture effect") +
      theme_minimal(base_size=15)
  })
  
  
  # ==========================================================
  # EXPORT CSV
  # ==========================================================
  output$export_csv <- downloadHandler(
    filename = function(){ "pyrolysis_results.csv" },
    content = function(file){
      r <- results()
      df <- data.frame(
        Parameter = names(r),
        Value = unlist(r)
      )
      write.csv(df, file, row.names=FALSE)
    }
  )
}


# ==========================================================
# RUN APP
# ==========================================================
shinyApp(ui, server)
