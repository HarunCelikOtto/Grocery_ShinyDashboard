#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(leaflet)


library(googleway)
library(dplyr)
library(sf)
library(tidycensus)
library(tigris)
library(DSPGGrocery)
library(ggplot2)
library(plotly)
library(scales)
library(tidyr)
library(ggokabeito)
library(forcats)
library(usethis)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "green",

  # Design
  dashboardHeader(title = "DSPG Grocery Stores"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tool Overview", tabName = "instruction_dashboard", icon = icon("map-location-dot")),
      menuItem("Store Map", tabName = "map_dashboard", icon = icon("map-location-dot")),
      menuItem("Profit Estimation", tabName = "user_dashboard", icon = icon("calculator")),
      menuItem("Census Income", tabName = "income_dashboard", icon = icon("chart-area")),
      menuItem("Census Demographics", tabName = "demographic_dashboard", icon = icon("chart-area")),
      menuItem("Rural Store Analysis", tabName = "ruralstore_dashboard", icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    tabItems(
      
      # Instructions Tab
      tabItem(
        tabName = "instruction_dashboard",
        fluidRow(
          
          box(
            width = 12,
            collapsible = TRUE,
            collapsed = F,
            title = "Step One: Using the Map",
            status = "success",
            
            source(file = "R/StepOne.R")[1]
            
            ),
          box(
            width = 12,
            collapsible = TRUE,
            collapsed = F,
            title = "Step Two: Profit Estimation",
            status = "warning",
            
            source(file = "R/StepTwo.R")[1]
            ),
          
          box(
            width = 12,
            collapsible = TRUE,
            title = "Plot Information",
            status = "warning",
            
            source(file = "R/StepThree.R")[1]
            )
          )
        ),

      # First Tab
      tabItem(
        tabName = "map_dashboard",
        fluidRow(
          
          box(
            width = 12,
            collapsible = TRUE,
            title = "Store Locator",
            status = "success",
            
            # UI for map inputs
            box(
              width = 6,
              title = "Find Stores",
              solidHeader = TRUE,

              textInput(
                inputId = "address",
                label = "Enter Store Address",
                placeholder = "St, Lamoni, IA",
                value = "St, Lamoni, IA"
              ),
              textInput(
                inputId = "keyword",
                label = "Enter Type of Store",
                placeholder = "Grocery, Dollar, Chain",
                value = "Grocery"
              ),
              actionButton(
                inputId = "calc_data_button",
                label = "Calculate Location",
                icon = icon("location-dot")
              ),
              actionButton(
                inputId = "map_data_button",
                label = "Map Retrieved Data",
                icon = icon("location")
              )
            ),
            
            # UI for Map Information
            box(
              width = 6,
              title = "Map Data",
              solidHeader = TRUE,
              
              uiOutput("map_information_ui")
              
            )
          ),
          
          box(
            width = 12,
            leafletOutput("leaflet_map", height = 700)
          )
        )
      ),

      # Second Tab
      tabItem(
        tabName = "user_dashboard",
        fluidRow(
          box(
            # Box container containing UI for user inputs
            width = 4,
            title = "Calculating Revenue",
            solidHeader = TRUE,
            status = "primary",
            
            radioButtons(
              inputId = "scenario_button",
              label = "Select Scenario:",
              choices = c(
                "Owned Building" = "scenario_one",
                "Leased Building" = "scenario_two"
              ),
              selected = "scenario_one"
            ),
            
            # Monthly Rent if Scenario One
            uiOutput(outputId = "month_box"),
            
            numericInput(
              inputId = "loan_amt",
              label = "Loan Amount",
              value = 100000,
              step = 100,
              min = 0
            ),
            sliderInput(
              inputId = "int_rate",
              label = "Interest Rate",
              min = 0,
              max = 15,
              value = 5,
              step = 0.05
            ),
            
            # Percentages Adding to Revenue
            HTML("<h4><center> Percentages Added to Revenue </center></h4>"),

            
            sliderInput(
              inputId = "gross_margin_pct",
              label = "Gross Margin Percentage",
              min = 22,
              max = 26.5,
              value = 24,
              step = 0.05
            ),
            sliderInput(
              inputId = "other_inc_pct",
              label = "Other Income Percentage",
              min = 0,
              max = 2,
              value = 0.01,
              step = 0.001
            ),
            sliderInput(
              inputId = "int_inc_pct",
              label = "Interest Income Percentage",
              min = 0,
              max = 0.5,
              value = 0.0012,
              step = 0.001
            ),
            
            
            # Percentages Added to Expenses
            HTML("<h4><center> Percentages Added to Expenses </center></h4>"),
            sliderInput(
              inputId = "employee_pct",
              label = "Employee Wages Percentage",
              min = 6,
              max = 12,
              value = 9,
              step = 0.05
            ),
            sliderInput(
              inputId = "officer_pct",
              label = "Officer Compensation Percentage",
              min = 0,
              max = 5,
              value = 1,
              step = 0.05
            ),
            sliderInput(
              inputId = "other_exp_pct",
              label = "Other Operating Expense Percentage",
              min = 8,
              max = 12,
              value = 11,
              step = 0.05
            )
            
          ),
          box(
            width = 4,
            title = "Expense Estimations",
            solidHeader = TRUE,
            
            #uiOutput for scenario-based inputs
            uiOutput(outputId = "scenario_ui")
          ),
          box(
            width = 4,
            collapsible = T,
            title = "Pre-Tax Profit",
            valueBoxOutput("pretax_vbox", width = 12)
          ),
          box(
            width = 4,
            collapsible = T,
            title = "Total Estimated Revenue",
            valueBoxOutput("estRev_vbox", width = 12)
          ),
          box(
            width = 4,
            collapsible = T,
            title = "Gross Margin Profit",
            valueBoxOutput("gross_margin_vbox", width = 12)
          ),
          box(
            width = 4,
            collapsible = T,
            title = "Secondary Income",
            valueBoxOutput("secondaryInc_vbox", width = 12)
          ),
          box(
            width = 4,
            collapsible = T,
            title = "Depreciation Costs",
            valueBoxOutput("depreciation_vbox", width = 12)
          ),
          box(
            width = 4,
            collapsible = T,
            title = "Expenses",
            valueBoxOutput("expenses_vbox", width = 12)
          )
        )
      ),

      # Third Tab
      tabItem(
        tabName = "income_dashboard",
        fluidPage(
          box(
            title = "Income Plot", 
            width = 12,
            footer = "Source: American Community Survey (5-Year Average, 2021)",
            status = "warning",
            
            plotlyOutput(outputId = "plot_one")
            ),
          box(
            title = "Employment Plot", 
            width = 12,
            footer = "Source: American Community Survey (5-Year Average, 2021)",
            status = "warning",
            
            plotlyOutput(outputId = "plot_two"))
        )
      ),
      
      tabItem(
        tabName = "demographic_dashboard",
        fluidPage(
          box(
            title = "Languages Spoken Plot", 
            width = 12,
            footer = "Source: American Community Survey (5-Year Average, 2021)",
            status = "warning",
            
            plotlyOutput(outputId = "plot_three")
          ),
          box(
            title = "Race & Ethnicity Table", 
            width = 12,
            footer = "Source: US Decennial Census (2020)",
            status = "warning",
            
            tableOutput(outputId = "plot_four"))
        )
      ),
      
      tabItem(
        tabName = "ruralstore_dashboard",
        fluidPage(
          box(
            title = "Distribution of Grocery Stores by Type (Iowa)", 
            width = 12,
            footer = "Source: SalesGenie Business Data, Accessed August 2020",
            status = "warning",
            
            plotlyOutput(outputId = "plot_five")
          ),
          box(
            title = "Sales Volume Distribution for Non-Chain Grocery Stores (Iowa)", 
            width = 12,
            footer = "Source: SalesGenie Business Data, Accessed August 2020",
            status = "warning",
            
            plotlyOutput(outputId = "plot_six"))
        )
      )
    )
  )
)

################################ SERVER SIDE ###################################

server <- function(input, output) {
  
  # Loading Datasets required for automating CPI and State Index values.
  df_state_index <<- read.csv("loadData/State_Index_SubSet.csv")
  df_cpi <<- read.csv("loadData/CPI_Subset.csv")
  df_storeType <<- read.csv("loadData/SalesTypeDistribution.csv")
  df_salesVolume <<- read.csv("loadData/SalesVolumeDistribution.csv")

  
  cpi <<- df_cpi$Apr[9] - df_cpi$HALF2[8]

  
  
  
  #### FIRST PAGE
  # Render the base leaflet map using the following settings.
  output$leaflet_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lat = 42.034534, lng = -93.620369, zoom = 6)
    })
  
  # Trigger Circle Buffer Calculations after hitting the 'calculate location' Button
  observeEvent(input$calc_data_button, {
    
    
    withProgress(message = "Pulling Geographic Information", 
                 value = 0, 
                 max = 30, 
                 detail = "Calculating Stores, Cities, and Counties", {
                   
                   incProgress(amount = 10, message = "Loading Geographies")
                   Sys.sleep(2)
                   incProgress(amoun = 10, message = "Please Wait... Pulling Store Information ")
                   
                   StoreInfo <<- Create_Circle_Buffer(address = input$address,
                                                      keyword = input$keyword,
                                                      api_key = Sys.getenv("PLACES_KEY"))
                   
                   incProgress(amount = 10, "Process Complete")
                   }
                 )
    })
  
  
  # Define a reactive variable to display City, County, and State's intersecting buffer
  # zone to use with the 'Map Data' button
  mapDataReactive <- reactive({
    
    if (exists("grocery_cities_inter")) {
      tagList(
        
        HTML("Cities in area: "),
        renderText(grocery_cities_inter$NAME, sep = ", "),
        
        p(),
        
        HTML("Counties in area: "),
        renderText(grocery_counties_inter$NAME, sep = ", "),
        
        p(),
        
        HTML("States in area: "),
        renderText(unique(grocery_counties_inter$STATEFP), sep = ", ")
      )
    }
  })
  
  # Trigger the render of mapDataReactive() with 'Map Data' button.
  observeEvent(input$map_data_button, {
    output$map_information_ui <- renderUI({
      mapDataReactive()
    })
  })
  
  # Trigger on 'Map Data' button the leaflet proxy with polygons added as well as retrieving the data 
  # frames necessary for the census plots. Additionally calls the market size calculation
  # to be used for Total Estimated Revenue
  observeEvent(input$map_data_button, {
   
    # Load the named list into the global environment
    if (exists("StoreInfo")) {
      list2env(StoreInfo, envir = .GlobalEnv)
    } else {
      print("StoreInfo doesn't exist.")
    }
    
    # !Warning for leaflet plot about geometry crs is suppressed!
    suppressWarnings(leafletProxy("leaflet_map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = all_counties$geometry, 
                  fillOpacity = 0.05, 
                  fillColor = 'darkred', 
                  color = 'black',
                  opacity = .5) %>%
      addPolygons(data = buffer_circle$geometry, fillColor = 'gray', color = 'red') %>%
      addPolygons(data = grocery_cities_inter$geometry, fillColor = 'blue') %>%
      addCircleMarkers(lng = df_grocery_all$lng, 
                       lat = df_grocery_all$lat, 
                       color= 'green', 
                       popup = df_grocery_all$name, 
                       radius = 10))
    
    # Progress bar for the census and market size data retrieval.
    withProgress(value = 0, 
                 max = 40, 
                 message = "Extracting Demographic Information for Area", {
                   
                   # Retrieving census data frames 
                   req(df_census_call)
                   
                   incProgress(amount = 10, "Pulling ACS Information")
                   
                   df_acs <<- Get_Census_Vars_ACS(df_locations = df_census_call)
                   
                   incProgress(amount = 20, "Pulling Census Information")
                   
                   df_decennial <<- Get_Census_Vars_Decennial(df_locations = df_census_call)
                   
                   incProgress(amount = 5, "Loading Additional Parameters")

                   # Filtering for State FIPS code to match state index table value.
                   state_abbrev <- Address_Parser(input$address)[3]
                   
                   state_code <<- fips_codes %>% 
                     filter(state == state_abbrev) %>%
                     select(state_code) %>%
                     unique() %>%
                     as.integer()
                   
                   state_index <<- df_state_index %>%
                     filter(GeoFips == state_code) %>%
                     select(X2021) %>%
                     as.double()
                   
                   # Generate a list of distances with the market size calculator.
                   DistancesList <<-  Calc_Market_Size(address = input$address, 
                                                       df_census_call = df_census_call, 
                                                       df_geocode = df_geocode, 
                                                       df_grocery_only = df_grocery_only,
                                                       state = state_abbrev$state[1])
                   
                   incProgress(amount = 5, "Completing Requests")
                 })
   
  })
  
  #### SECOND PAGE
    
    # Change scenario UI based on radio button selection
    output$scenario_ui <- renderUI({
      
      if (input$scenario_button == "scenario_one") {
        source(file = "R/Scenario_One.R")[1]
      } 
      
      else if (input$scenario_button == "scenario_two") {
        source(file = "R/Scenario_Two.R")[1]
      }
    })
    
    output$month_box <- renderUI({
      
      if (input$scenario_button == "scenario_two") {
        numericInput(inputId = "month_rent", 
                     label = "Monthly Rent", 
                     value = 2500, step = 100)
      } 
      
      else if (input$scenario_button == "scenario_one") {
        NULL
      }
      
    })
    
    DistList <- eventReactive(input$map_data_button, {DistancesList})
    
    # Define Reactive Calculation for Total Estimated Revenue
    EstRevenueReactive <- reactive({
      
      req(DistList())
      Total_Estimate_Revenue(metro_pop = DistList()$metro_population, 
                             town_pop = DistList()$city_population, 
                             rural_pop = DistList()$rural_population, 
                             state_index = state_index, 
                             est_per_price_increase = cpi)
      
      
    })
    
    # Define reactive depreciation values to call based on selected scenario options.
    Depr_1 <- reactive({
      
      Depreciation_1(Building_Remodeling = input$remodel,
                     Parking_Lot_Improvements = input$parklot,
                     Shelving_Check_Out_Counters = input$shelves,
                     Computer_Equipment_POS = input$computers,
                     Vehicles = input$vehicles,
                     Display_Cases = input$disp_case,
                     Refrigeration = input$refrige,
                     Freezers = input$freezer,
                     Meat_Cutting_Equipment = input$meat_eqp,
                     Miscellaneous_Assets_1 = input$misc_one,
                     Miscellaneous_Assets_1_Use_Life = input$misc_one_life,
                     Miscellaneous_Assets_2 = input$misc_two,
                     Miscellaneous_Assets_2_Use_Life = input$misc_two_life,
                     Miscellaneous_Assets_3 = input$misc_three,
                     Miscellaneous_Assets_3_Use_Life = input$misc_three_life)
    })
    
    Depr_2 <- reactive({
      
      Depreciation_2(Leasehold_Improvements = input$leasehold, 
                     Leasehold_Improvements_Use_Life = input$leasehold_life,
                     Shelving_Check_Out_Counters = input$shelves, 
                     Computer_Equipment_POS = input$computers,
                     Vehicles = input$vehicles,
                     Display_Cases = input$disp_case, 
                     Refrigeration = input$refrige,
                     Freezers = input$freezer,
                     Meat_Cutting_Equipment = input$meat_eqp,
                     Miscellaneous_Assets_1 = input$misc_one, 
                     Miscellaneous_Assets_1_Use_Life = input$misc_one_life, 
                     Miscellaneous_Assets_2 = input$misc_two, 
                     Miscellaneous_Assets_2_Use_Life = input$misc_two_life, 
                     Miscellaneous_Assets_3 = input$misc_three, 
                     Miscellaneous_Assets_3_Use_Life = input$misc_three_life)
    })
    
    
    
    # Define a reactive event to update Depreciation calculation values based on
    # selected scenario.
    DepreciationReactive <- reactive({
      
      req(input$shelves)
      
      if (input$scenario_button == "scenario_one") {
        Depr_1()
      }
      else if (input$scenario_button == "scenario_two") {
        Depr_2()
      }
    })
  
    
    ExpenseReactive <- reactive({
      
      req(input$shelves)
      req(input$loan_amt)
      
      Interest_Exp <- Interest_Expense(Loan_Amount = input$loan_amt, 
                                       Interest_Rate = input$int_rate / 100)
      
      Cost_Goods_Sold <- Cost_of_Goods_Sold(Total_Estimated_Revenue = EstRevenueReactive(),
                                            Gross_Margin_Percentage = input$gross_margin_pct/100)
      
      Employee_Wages <- Employee_Wages(Total_Estimated_Revenue = EstRevenueReactive(), 
                                       Percentage = input$employee_pct/100)
      
      Officer_Compensation <- Officer_Compensation(Total_Estimated_Revenue = EstRevenueReactive(), 
                                                   Percentage = input$officer_pct/100)
      
      Other_Op_Exp <- Other_Operating_Expense(Total_Estimated_Revenue = EstRevenueReactive(),
                                              Percentage = input$other_exp_pct/100)
      
      Annual_Rent <- Annual_Rent(Monthly_Rent = input$month_rent)
      
      if (input$scenario_button == "scenario_one") {
        Expenses <- Cost_Goods_Sold + 
          Employee_Wages + 
          Officer_Compensation + 
          Other_Op_Exp + 
          Interest_Exp + 
          Depr_1()
      }
      else if (input$scenario_button == "scenario_two") {
        Expenses <- Cost_Goods_Sold + 
          Employee_Wages + 
          Officer_Compensation + 
          Other_Op_Exp + 
          Interest_Exp + 
          Annual_Rent + 
          Depr_2()
      }
      
      Expenses
      
    })
    
    
    SecondaryIncReactive <- reactive({
       Interest_Inc <- Interest_Income(Total_Estimated_Revenue = EstRevenueReactive(), 
                                       Percentage = input$int_inc_pct/100)
       Other_Inc <- Other_Income(Total_Estimated_Revenue = EstRevenueReactive(), 
                                 Percentage = input$other_inc_pct/100)
       
       Secondary_Income <- Other_Inc + Interest_Inc
    })
    
    PretaxReactive <- reactive({
      EstRevenueReactive() + SecondaryIncReactive() - ExpenseReactive()
    })
    
    
    # Output valueBox(es) to display calculated values.
    ## For Pre-Tax Profit
    output$pretax_vbox <- renderValueBox({
      valueBox(floor(PretaxReactive()), 
               subtitle = "Estimated Pre-Tax Profit", 
               color = 'green',
               icon = icon("dollar-sign")) 
    })
    
    ## For Estimated Revenue
    output$estRev_vbox <- renderValueBox({
      valueBox(floor(EstRevenueReactive()), 
               subtitle = "Estimated Total Revenue", 
               color = 'olive',
               icon = icon("dollar-sign")) 
    })
    
    ## For Gross Margin Revenue
    
    output$gross_margin_vbox <- renderValueBox({

      #test_total_rev = 100000000
      valueBox(floor(Gross_Margin(Total_Estimated_Revenue = EstRevenueReactive(), 
                            Percentage = input$gross_margin_pct/100)), 
               subtitle = "Estimated Gross Margin Profit", 
               color = 'blue',
               icon = icon("dollar-sign")) 
    })
    
    ## For Secondary Income
    output$secondaryInc_vbox <- renderValueBox({
      valueBox(floor(SecondaryIncReactive()),
               subtitle = "Secondary Income",
               color = 'green',
               icon = icon("circle-dollar-to-slot"))
    })
    
    ## For Depreciation Costs
    output$depreciation_vbox <- renderValueBox({
      valueBox(DepreciationReactive(),
               subtitle = "Depreciation Costs",
               color = 'red',
               icon = icon("circle-dollar-to-slot"))
    })
    
    ## For Expenses
    output$expenses_vbox <- renderValueBox({
      valueBox(ceiling(ExpenseReactive()),
               subtitle = "Expenses",
               color = 'red',
               icon = icon("circle-dollar-to-slot"))
    })
    

  
  #### THIRD PAGE
  
  # Create Census Plots
    
  ## Define Reactive Plot One
  plot_one <- reactive({
    req(df_acs)
    
    
    reduced_df_acs1 <- df_acs %>% filter(variable=="B19013_001")
    
    ## Formatting Text for Hover Box
    
    reduced_df_acs1$text <- reduced_df_acs1$text <- paste0("Estimate: ", 
                                                           scales::dollar_format()(reduced_df_acs1$estimate),
                                                           "<br>",
                                                           "Margin of error: ",
                                                           scales::dollar_format()(reduced_df_acs1$moe))
    
    ## For moe bar in plot
    
    ymin <- reduced_df_acs1$estimate - reduced_df_acs1$moe
    ymax <- reduced_df_acs1$estimate + reduced_df_acs1$moe
    
    max_lim1 <- max(ymax)
    
    ## Make plot object
    
    p <- reduced_df_acs1 %>%
      ggplot(aes(x = NAME, y = estimate, fill = NAME, text = text)) +
      geom_bar(stat = 'identity', show.legend = FALSE) +
      geom_errorbar(ymin = ymin, ymax = ymax) +
      ggtitle("Median Household Income (2021)") +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_okabe_ito() +
      labs(x = NULL, y = NULL) +
      coord_flip(ylim = c(0, max_lim1))
    
    
    ## Convert to plotly object
    
    plot1 <- plotly::ggplotly(p, tooltip = "text")
    
    plot1
    
  })
  
   
  ## Render Reactive Plot One
  output$plot_one <- renderPlotly({
    plot_one()
  })
      
    
    
  ## Define Reactive Plot Two
  plot_two <- reactive({
    req(df_acs)
    
    ## Filter vars from acs table
    
    Employment_status_df <- df_acs %>% filter(variable %in% c("B23025_003", 
                                                              "B23025_004", 
                                                              "B23025_005"))
    
    ## Convert to wide format
    
    Emploment_status_df_wide <- Employment_status_df %>% 
      pivot_wider(names_from = "variable", 
                  values_from = c("estimate", "moe"))
    
    ## Rename columns
    
    employment_df <- Emploment_status_df_wide %>% 
      rename("Total" = "estimate_B23025_003", 
             "Employed_in_Civilian_Labor_Force" = "estimate_B23025_004", 
             "Unemployed_in_Civilian_Labor_Force" = "estimate_B23025_005",
             "Total_moe" = "moe_B23025_003",
             "Employed_in_Civilian_Labor_Force_moe" = "moe_B23025_004", 
             "Unemployed_in_Civilian_Labor_Force_moe" = "moe_B23025_005")
    
    ## Calculate percentage and margin of error (mutated into two new columns)
    
    employment_df2 <- employment_df %>% 
      mutate(Employment_rate = 100 * (Employed_in_Civilian_Labor_Force/Total), 
             Employment_rate_moe = 100 * moe_prop(Employed_in_Civilian_Labor_Force, 
                                                  Total, 
                                                  Employed_in_Civilian_Labor_Force_moe, Total_moe))
    
    ## For moe bar in plot
    
    ymin2 <- employment_df2$Employment_rate - employment_df2$Employment_rate_moe
    ymax2 <- employment_df2$Employment_rate + employment_df2$Employment_rate_moe
    
    ## Make plot object
    
    p2 <- employment_df2 %>%
      ggplot(aes(x = NAME, y = Employment_rate, fill = NAME)) +
      geom_bar(stat = 'identity', show.legend = FALSE) +
      geom_errorbar(ymin = ymin2, ymax = ymax2, width = .2) +
      ggtitle("Employment Rate (2021)") +
      scale_fill_okabe_ito() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      xlab(NULL) +
      ylab(NULL) +
      coord_flip(ylim = c(80,100))
    
    ## Convert to plotly object
    
    plot2 <- plotly::ggplotly(p2)
  
    
    ## Show Plot
    
    plot2
    
  })
    
  ## Render Reactive Plot Two  
  output$plot_two <- renderPlotly({
    plot_two()
  })
  
  ## Define Reactive Plot Three
  plot_three <- reactive({
    req(df_acs)
    
    language_df <- df_acs %>% 
      filter(variable %in% c("C16001_001","C16001_003", "C16001_006", "C16001_009",
                             "C16001_012", "C16001_015","C16001_018", "C16001_021",
                             "C16001_024", "C16001_027", "C16001_030", "C16001_033", 
                             "C16001_036"))
    
    ## Renaming Variables
    
    language_df$variable <- fct_recode(language_df$variable,
                                       Total = "C16001_001", 
                                       Spanish = "C16001_003", 
                                       `French, Haitian or Cajun` = "C16001_006", 
                                       `German or other West Germanic languages` = "C16001_009",  
                                       `Russian, Polish or other Slavic languages` = "C16001_012", 
                                       `Other Indo European languages` = "C16001_015", 
                                       Korean = "C16001_018", 
                                       `Chinese (includes Mandarin, Cantonese)` = "C16001_021",
                                       Vietnamese = "C16001_024", 
                                       `Tagalog (includes Filipino)` = "C16001_027", 
                                       `Other Asian and Pacific Island languages` = "C16001_030", 
                                       Arabic = "C16001_033", 
                                       `Other and unspecified languages` = "C16001_036")
    
    language_df <- language_df %>% rename(Language_Spoken = variable)
    
    ## Pre_processing and adding percentages/margins of error
    
    small_df2 <- language_df %>% group_by(NAME) %>% slice_max(estimate, n = 4)
    
    small_df2 <- small_df2 %>% 
      mutate(Total = estimate[Language_Spoken=="Total"],
             Total_moe = moe[Language_Spoken=="Total"]) %>% 
      subset(Language_Spoken != "Total") %>%
      mutate(Percentage = 100 * (estimate/Total),
             Percentage_moe = 100 * moe_prop(estimate, Total,   moe, Total_moe))
    
    ## Formatting text for hover box
    
    small_df2$text <- small_df2$text <- paste0("Estimate: ", small_df2$estimate, "<br>",
                                               "Margin of error: ", small_df2$moe)
    
    
    
    ymin3 <- small_df2$Percentage - small_df2$Percentage_moe
    ymax3 <- small_df2$Percentage + small_df2$Percentage_moe
    
    max_lim3 <- max(ymax3)
    
    
    ### GROUPED BAR APPROACH ###
    
    p3 <- small_df2 %>% 
      ggplot(aes(fill = Language_Spoken, y = Percentage, x = NAME, text = text)) +
      geom_bar(position = "dodge", stat = "identity", show.legend = FALSE) + 
      geom_errorbar(ymin = ymin3, ymax = ymax3, width = .2, position = position_dodge(.9)) + 
      ggtitle("Languages Spoken other than English (2021)") +
      scale_fill_okabe_ito() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      xlab(NULL) +
      ylab(NULL) +
      coord_flip(ylim = c(0, max_lim3))
    
    
    plot3 <- plotly::ggplotly(p3, tooltip = "text")
    
    plot3
    
  })
  
  # Render Reactive Plot Three
  output$plot_three <- renderPlotly({
    plot_three()
  })
  
  ## Define Reactive Plot Four (TABLE)
  plot_four <- reactive({
    req(df_decennial)
    
    decennial_prop_table <- df_decennial %>%
      mutate(White = White/`Total Population`,
             `White (Not Hispanic)` = `White (Not Hispanic)`/`Total Population`,
             Black = Black/`Total Population`,
             `American Indian/Alaskan Native` = `American Indian/Alaskan Native`/`Total Population`,
             Asian = Asian/`Total Population`,
             `Native Hawaiian/Pacific Islander` = `Native Hawaiian/Pacific Islander`/`Total Population`,
             Other = Other/ `Total Population`,
             `Two or More Races` = `Two or More Races`/`Total Population`,
             `Hispanic/Latino` = `Hispanic/Latino`/`Total Population`) %>% 
      rename(County = NAME) %>% 
      mutate_at(4:12, scales::percent) %>% 
      select(-GEOID)
    
  
    decennial_prop_table
  })
  
  ## Render Reactive Plot Four (TABLE)
  output$plot_four <- renderTable({
    plot_four()
  })
  
  ## Define Reactive Plot Five
  plot_five <- reactive({

    req(df_storeType)
    
    ds_ncg_cg<-  ggplot(df_storeType, aes(x =GrpCode, y = Count, fill =StoreType)) +
      geom_bar(position="dodge", stat="identity")+
      #Rename to describe access
      scale_x_discrete(labels=c("PG1" = ">=10,000 Core county of a MSA",
                                "PG2" = ">=10,000 Non-core MSA county",
                                "PG3" = "2,500 to 9,999 Non-metro county",
                                "PG4"="2,500 to 9,999 Metro county",
                                "PG5A"="500 to 2,499
Non-metro 
adjacent 
to a MSA",
                                "PG5N"="500 to 2,499
Non-metro
not adjacent 
to a MSA",
                                "PG6"="500 to 2,499 
Metro county",
                                "PG7"="250 to 499",
                                "Rest_of_the_State"="249 or fewer"))+
      scale_fill_okabe_ito() +
      theme(axis.text.x = element_text(hjust = 0.5),
            plot.title = element_text(vjust = 3,hjust = 0.5))+
      ylab("Number Of Stores")+
      xlab("")
    
    ggplotly(ds_ncg_cg) %>% layout(title = list(text = "Number of Grocery Store Type in Cities in Iowa (under 2,500 population) by Population Group", y = 0.98),font = list(size = 10))
    
    })
  
  # Render Reactive Plot Five
  output$plot_five <- renderPlotly({
    plot_five()
  })
  
  # Define Reactive Plot Six
  plot_six <- reactive({
    
    req(df_salesVolume)
    df_salesVolume$GrpCode <- factor(df_salesVolume$GrpCode, 
                                     levels = c("499 and fewer", 
                                                "500 to 2,499 non-metro county", 
                                                "500 to 2,499 in metro county"))
    
    ncg_sales_plot<- ggplot(df_salesVolume, aes(x =factor(Location_Sales_Volume_Range, 
                                                          level=c('Less Than $500,000',
                                                                  '$500,000-1 Million',
                                                                  '$1-2.5 Million',
                                                                  '$2.5-5 Million',
                                                                  '$5-10 Million',
                                                                  '$10-20 Million',
                                                                  '$20-50 Million',
                                                                  '$50-100 Million',
                                                                  '$100-500 Million')), 
                                                y = Count, fill = GrpCode)) +
      geom_bar(stat = "identity", position = "stack") +
      coord_flip()+
      xlab("") +
      ylab("Number of Stores") +
      scale_fill_okabe_ito(name="")+
      theme(axis.text.x = element_text(hjust = 1))
    ggplotly(ncg_sales_plot) %>% layout(title = list(text = "Number of Non-Chain Grocery Stores by Sales Volume Range for Cities in Iowa (under 2,500 Population)", 
                                                     y = 0.98),font = list(size = 10))
  })
  
  ## Render Reactive Plot Six
  output$plot_six <- renderPlotly({
    plot_six()
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

