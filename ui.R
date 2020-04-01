shinyUI(dashboardPage(
  # App title ----
  dashboardHeader(title = "IEA Uploader"),
  
  # Sidebar layout with input and output definitions ----
  dashboardSidebar(
    
    # Input: Select a file ----
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    h5("e.g.", 
      a("slopewater_proportions.csv", 
        href="https://raw.githubusercontent.com/marinebon/ecodata/master/data-raw/slopewater_proportions.csv")),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons("sep", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ","),
    
    # Input: Select quotes ----
    radioButtons("quote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"'),
    
    # Horizontal line ----
    tags$hr(),
    
    # Input: Select number of rows to display ----
    radioButtons("disp", "Display",
                 choices = c(Head = "head",
                             All = "all"),
                 selected = "head")
    
  ),
  
  # Main panel for displaying outputs ----
  dashboardBody(
    fluidRow(
      shinyjs::useShinyjs(),
      
      # add logout button UI 
      div(class = "pull-right", logoutUI(id = "logout")),
      
      # add login panel UI function
      loginUI(id = "login"),
      
      # Output: Data file ----
      tableOutput("tbl"),
      plotOutput("fig")
    )
  )
  
))