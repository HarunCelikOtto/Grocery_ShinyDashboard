tagList(
  numericInput(inputId = "leasehold", 
               label = "Leasehold Improvements", 
               value = 50000, step = 100),
  
  numericInput(inputId = "leasehold_life", 
               label = "Leasehold Improvements Use Life", 
               value = 50000, step = 100),
  
  numericInput(inputId = "shelves", 
               label = "Shelving Check Out Counters", 
               value = 60000, step = 100),
  
  numericInput(inputId = "computers", 
               label = "Computer Equipment POS", 
               value = 15000, step = 100),
  
  numericInput(inputId = "vehicles", 
               label = "Vehicles", 
               value = 12000, step = 100),
  
  numericInput(inputId = "disp_case", 
               label = "Display Cases", 
               value = 15000, step = 100),
  
  numericInput(inputId = "refrige", 
               label = "Refrigeration", 
               value = 20000, step = 100),
  
  numericInput(inputId = "freezer", 
               label = "Freezers", 
               value = 20000, step = 100),
  
  numericInput(inputId = "meat_eqp", 
               label = "Meat Cutting Equipment", 
               value = 10000, step = 100),
  
  numericInput(inputId = "misc_one", 
               label = "Miscellaneous Asset One", 
               value = NA, step = 100),
  
  numericInput(inputId = "misc_one_life", 
               label = "Miscellaneous Asset One Life Use (years)", 
               value = NA, step = 100),
  
  numericInput(inputId = "misc_two", 
               label = "Miscellaneous Asset Two", 
               value = NA, step = 100),
  
  numericInput(inputId = "misc_two_life", 
               label = "Miscellaneous Asset Two Life Use (years)", 
               value = NA, step = 100),
  
  numericInput(inputId = "misc_three", 
               label = "Miscellaneous Asset Three", 
               value = NA, step = 100),
  
  numericInput(inputId = "misc_three_life", 
               label = "Miscellaneous Asset Three Life Use (years)", 
               value = NA, step = 100)
)