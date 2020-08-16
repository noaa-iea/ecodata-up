ui <- tagList(
  # https://console.developers.google.com/apis/credentials?authuser=1&project=iea-uploader (ben@ecoquants.com as authuser=1)
  tags$head(
    tags$meta(name="google-signin-scope",content="profile email"),
    tags$meta(name="google-signin-client_id", content="596429062120-tbgia6e94mtqc8eoo27iqqd7l89s6apr.apps.googleusercontent.com"),
    HTML('<script src="https://apis.google.com/js/platform.js?onload=init"></script>'),
    includeScript("includes/signin.js"),
    useShinyjs()
  ),
  
  dashboardPagePlus(
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
    
    #   sidebarPanel(
        div(id="signin", class="g-signin2", "data-onsuccess"="onSignIn"),
        hidden(actionButton("signout", "Sign Out", onclick="signOut();", class="btn-danger")), # ),
      # mainPanel(
        with(tags, dl(dt("Name"), dd(textOutput("g_name")),
                      dt("Email"), dd(textOutput("g_email")),
                      dt("Image"), dd(uiOutput("g_image")) )), # ))
    
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
))