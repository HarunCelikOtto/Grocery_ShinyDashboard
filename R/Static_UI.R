numericInput(
  inputId = "month_rent", 
  label = "Monthly Rent", 
  value = 2500, 
  step = 100, 
  min = 0)

numericInput(
  inputId = "loan_amt", 
  label = "Loan Amount",
  value = 100000,
  step = 100,
  min = 0)

sliderInput(inputId = "int_rate", 
            label = "Interest Rate", 
            min = 0, 
            max = 15, 
            value = 5, 
            step = 0.05)

radioButtons(inputId = "scenario_button", 
             label = "Select Scenario:", 
             choices = c("Scenario One" = "scenario_one",
                         "Scenario Two" = "scenario_two",
                         "No Scenario" = "no_scenario"), 
             selected = 1)