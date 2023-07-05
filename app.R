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
            # Box container containing UI for map inputs
            width = 4,
            title = "Find Stores",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            textInput(
              inputId = "address",
              label = "Enter Store Address",
              placeholder = "23 Main St, Lake View, IA, United States, Iowa",
              value = "23 Main St, Lake View, IA, United States, Iowa"
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
          box(
            width = 12,
            leafletOutput("leaflet_map", height = 750)
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
            uiOutput(outputId = "scenario_ui")
          ),
          box(
            width = 4,
            title = "Pre-Tax Profit",
            valueBoxOutput("pretax_vbox", width = 12)
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
  
  #### FIRST PAGE

  source(file = "../Grocery/Harun/testR/Grocery_Store_Map.R")

  
  # Render the base leaflet map using the following settings
  output$leaflet_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      setView(lat = 42.034534, lng = -93.620369, zoom = 6)
    })
  
  # Trigger this after hitting the calculate location Button
  observeEvent(input$calc_data_button, {
  
    StoreInfo <- Grocery_Store_Map(address = input$address, keyword = input$keyword)
    })

  observeEvent(input$map_data_button, {
    # Load the named list into the global environment
    if (exists("StoreInfo")) {
      list2env(StoreInfo, envir = .GlobalEnv)
    } else {
      print("StoreInfo doesn't exist.")
    }
  
    leafletProxy("leaflet_map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = grocery_counties_inter,
                  fillOpacity = 0.05, 
                  fillColor = 'darkred', 
                  color = 'black',
                  opacity = .5) %>%
      addPolygons(data = grocery_cities_inter,
                  fillColor = "blue",
                  popup = grocery_cities_inter$NAME.x) %>%
      addCircleMarkers(lng = df_grocery$lng, 
                       lat = df_grocery$lat, 
                       popup = df_grocery$name, 
                       color = "green", radius = 10)
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
    
    
    # Output Value box(es)
    output$pretax_vbox <- renderValueBox({
      valueBox(input$remodel, 
               subtitle = "Total Estimated Pre-Tax Profit", 
               color = 'green',
               icon = icon("dollar-sign")) 
    })

  
  #### THIRD PAGE
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

