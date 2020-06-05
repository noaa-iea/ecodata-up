ui <- dashboardPagePlus(
  title              = "IEA Uploader", 
  skin               = "blue-light",
  collapse_sidebar   = T, 
  #sidebar_background = "light",
  enable_preloader   = T,
  loading_duration   = 1,
  md                 = T,
  
  header = dashboardHeaderPlus(),
  
  sidebar = dashboardSidebar(collapsed = TRUE),
  
  body = dashboardBody(
    fluidRow(
      tabBox(
        title = "Datasets",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset_d", width = 12, # height = "250px",
        tabPanel(
          "Dataset",
          "Select existing:",
          DTOutput("dt_datasets"),
          "Or", 
          actionButton(
            "btn_newdataset", "Create new dataset", 
            icon("plus"), class = "btn btn-primary btn-lg")),
        tabPanel(
          "User", 
          "auth_output:", verbatimTextOutput("auth_output"))))),
  
  footer = dashboardFooter(
    left_text = HTML(paste("&copy;",year(Sys.Date()))),
    right_text = Sys.time())
)

ui <- secure_app(
  ui,
  enable_admin = TRUE)