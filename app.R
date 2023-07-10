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

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "green",

  # Design
  dashboardHeader(title = "DSPG Grocery Stores"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Store Map", tabName = "map_dashboard", icon = icon("map-location-dot")),
      menuItem("User Inputs", tabName = "user_dashboard", icon = icon("calculator")),
      menuItem("Dashboards", tabName = "plot_dashboard", icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    tabItems(

      # First Tab
      tabItem(
        tabName = "map_dashboard",
        fluidRow(
          
          box(
            width = 12,
            collapsible = TRUE,
            title = "Store Locator",
            
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
            numericInput(
              inputId = "month_rent",
              label = "Monthly Rent",
              value = 2500,
              step = 100,
              min = 0
            ),
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
            radioButtons(
              inputId = "scenario_button",
              label = "Select Scenario:",
              choices = c(
                "Scenario One" = "scenario_one",
                "Scenario Two" = "scenario_two",
                "No Scenario" = "no_scenario"
              ),
              selected = 1
            ),

            # Value Index section
            HTML("<h3> Value Index </h3>
                 
                 <p> The following are a list of inputs for the two possible scenarios 
                 available in the calculation tool. Regardless of the scenario, the tool requires
                 the following values. </p> 
                 
                 <ul> 
                 <li>Monthly Rent - rent cost per month</li>
                 <li>Loan Amount - amount taken for loan</li>
                 <li>Interest Rate - interest on the loan as a percentage</li>
                 </ul>
                 
                 
                 <p> Each scenario requires the following values.</p>
                 
                 <ul> 
                 <li>Parking Lot Improvements - cost for parking lot improvements</li>
                 <li>Shelving Check Out Counters - ???</li>
                 <li>Computer Equipment POS - ??? computer equipment costs</li>
                 <li>Vehicles - vehicle purchase costs</li>
                 <li>Display Cases - cost of display cases</li>
                 <li>Refrigeration - refrigeration setup costs</li>
                 <li>Freezers - freezer setup costs</li>
                 <li>Meat Cutting Equipment - meat cutting equipment costs</li>
                 <li>Miscellaneous Assets - an optional asset to evaluate</li>
                 <li>Miscellaneous Asset Life - use life of asset in years</li>
                 </ul>"),
            HTML("<h4> Scenario One - Building / Remodeling </h4>
                 
                 <p> In scenario One, the user is either building the grocery 
                 store anew or as part of a remodeling effort. Once 'Scenario One'
                 is selected in the 'Select Scenario' menu, a set of cost inputs
                 will open to the right. </p>
                 
                 <p> Scenario One requires the following additional value(s)</p>
                 
                 <ul> 
                 <li>Building Remodel Costs - cost of remodeling purchased grocery store</li>
                 </ul>"),
            HTML(
              "<h4> Scenario Two - Renting </h4>",
              "<p> In scenario Two, the user is renting the grocery store. 
                 Once 'Scenario Two' is selected in the 'Select Scenario' menu, 
                 a set of additional cost inputs will open to the right</p>
                 
                  <p> Scenario Two requires the following additional value(s)</p>
                 
                 <ul> 
                 <li>Leasehold Improvements - cost of improving rented grocery store</li>
                 </ul>"
            )
          ),
          box(
            width = 4,
            title = "Scenario Estimations",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            #uiOutput for scenario-based inputs
            uiOutput(outputId = "scenario_ui")
          ),
          box(
            width = 4,
            title = "Pre-Tax Profit",
            valueBoxOutput("pretax_vbox", width = 12)
          ),
          box(
            width = 4,
            title = "Gross Margin Profit",
            valueBoxOutput("gross_margin_vbox", width = 12)
          ),
          box(
            width = 4,
            title = "Depreciation Costs",
            valueBoxOutput("depreciation_vbox", width = 12)
          )
        )
      ),

      # Third Tab
      tabItem(
        tabName = "plot_dashboard",
        fluidPage(
          box(title = "Plot One", width = 6, background = "blue"),
          box(title = "Plot Three", width = 6, background = "yellow"),
          box(title = "Plot Two", width = 12, background = "green")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  StoreInfo <- NULL
  
  #### FIRST PAGE
  # Render the base leaflet map using the following settings
  output$leaflet_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lat = 42.034534, lng = -93.620369, zoom = 6)
    })
  
  # Trigger this after hitting the calculate location Button
  observeEvent(input$calc_data_button, {
  
    StoreInfo <<- Create_Circle_Buffer(address = input$address,
                                      keyword = input$keyword,
                                      api_key = Sys.getenv("PLACES_KEY"))
    })
  
  
  # REACTIVE TEST
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
  
  # Map Calculate Button Generates list of cities, counties and states
  observeEvent(input$map_data_button, {
    output$map_information_ui <- renderUI({
      mapDataReactive()
    })
  })
  
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
  
  DepreciationReactive <- observeEvent(input$scenario_button, {
    
    if (input$scenario_button == "scenario_one") {
      output$Depr_1()
    }
    else if (input$scenario_button == "scenario_two") {
      print("Scenario Two")
      }
    })
  
  
  observeEvent(input$map_data_button, {
    # Load the named list into the global environment
    if (exists("StoreInfo")) {
      list2env(StoreInfo, envir = .GlobalEnv)
    } else {
      print("StoreInfo doesn't exist.")
    }
  
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
    
  })
  
  #### SECOND PAGE
    
    # Change the UI output based on what radio button is selected
    output$scenario_ui <- renderUI({
      
      if (input$scenario_button == "scenario_one") {
        source(file = "R/Scenario_One.R", local = T)
      } 
      
      else if (input$scenario_button == "scenario_two") {
        source(file = "R/Scenario_Two.R")
      }
      
      else if (input$scenario_button == "no_scenario")  {
        HTML("<h5> No Scenario is Selected </h5> ")
      }
      
    })
    
    # Change Depreciation value based on scenario
    
    
    
    # Output Value box(es)
    output$pretax_vbox <- renderValueBox({
      valueBox(input$remodel, 
               subtitle = "Total Estimated Pre-Tax Profit", 
               color = 'green',
               icon = icon("dollar-sign")) 
    })
    
    test_total_rev <- 120000
    
    output$gross_margin_vbox <- renderValueBox({
      valueBox(Gross_Margin(Total_Estimated_Revenue = test_total_rev, 
                            Percentage = input$int_rate / 100), 
               subtitle = sprintf("Estimated Gross Margin Profit 
                                  (based on %s as total estimated revenue)", 
                                  test_total_rev), 
               color = 'blue',
               icon = icon("dollar-sign")) 
    })
    
    # Should change based on which depreciation calculation is running
    output$depreciation_vbox <- renderValueBox({
      valueBox(output$DepreciationReactive(),
               subtitle = "Depreciation Costs",
               color = 'red',
               icon = icon("circle-dollar-to-slot"))
    })

  
  #### THIRD PAGE
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

