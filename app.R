if (!require(librarian)){
  remotes::install_github("DesiQuintans/librarian")
  library(librarian)
}
shelf(
  # noaa
  bbest/ecodata,
  # shiny
  shiny, shinyjs,
  # utility
  here,fs,glue,stringr,
  # tidyverse
  readr,dplyr,tidyr,
  # report
  rmarkdown,DT,
  # packages used by ecodata:chunk-scripts/*-setup.R
  colorRamps,
  cowplot,
  dplyr,
  ecodata,
  ggiraph,
  ggplot2,
  ggrepel,
  grid,
  heatwaveR,
  here,
  kableExtra,
  patchwork,
  raster,
  rgdal,
  rpart,
  sf,
  stringr,
  tidyr,
  vegan)

options(shiny.maxRequestSize = 30*1024^2) # 30MB limit

dir_ecodata  <- "/share/github/ecodata"
dir_uploader <- here()

datasets <- list(
  name      = "SOE 2020 Contributors (copy 2020-04-24)",
  url_csv   = "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/gviz/tq?tqx=out:csv&sheet=0",
  url_edit  = "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/edit",
  local_csv = "data/datasets.csv")

ui <- tagList(
  tags$head(
    tags$meta(name="google-signin-scope",content="profile email"),
    # https://console.developers.google.com/apis/credentials?authuser=1&project=iea-uploader (ben@ecoquants.com as authuser=1)
    tags$meta(name="google-signin-client_id", content="596429062120-tbgia6e94mtqc8eoo27iqqd7l89s6apr.apps.googleusercontent.com"),
    HTML('<script src="https://apis.google.com/js/platform.js?onload=init"></script>'),
    includeScript("signin.js"),
    useShinyjs()
  ),
  navbarPage(
    id = "tabs",
    title = "IEA Uploader",
    #windowTitle = "Browser window title",
    
    tabPanel(
      "1. Login",
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "input.g_id == null",
            "In order to select and upload a dataset, please login with Google email associated with datasets.",
            div(id="signin", class="g-signin2", "data-onsuccess"="onSignIn")),
          conditionalPanel(
            condition = "input.g_id",
            actionButton("btnSignout", "Sign Out", onclick="signOut();", class="btn-danger"),br(),br(),
            actionButton("btnNextSelect", "Next", class="btn-primary"),
            "step 2. Select")),
        
        mainPanel(
          conditionalPanel(
            condition = "input.g_id",
            with(tags, dl(
              dt("Name") , dd(textOutput("g_name")),
              dt("Email"), dd(textOutput("g_email")),
              dt("Image"), dd(uiOutput("g_image")) ))),
          ))),
    
    tabPanel(
      "2. Select",
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "input.g_id == null",
            "Please first go to step", strong("1. Login"), "to show datasets associated with your email."),
          conditionalPanel(
            condition = "input.g_id && input.dt_datasets_rows_selected.length == 0",
            "Please select a dataset row in the table below. 
            If you don't see your dataset below, we're working on uploading new datasets. 
            Meanwhile please email kimberly.bastille@noaa.gov."),
          conditionalPanel(
            condition = "input.dt_datasets_rows_selected.length > 0",
            actionButton("btnNextUpload", "Next", class="btn-primary"),"3. Upload")),
        mainPanel(
          DTOutput("dt_datasets")))),
      
    tabPanel(
        "3. Upload",
        sidebarLayout(
          sidebarPanel(
            conditionalPanel(
              condition = "input.g_id == null",
              "Please first go to step", strong("1. Login"), "to show and select a dataset associated with your email."),
            conditionalPanel(
              condition = "((input.g_id !== null) && (input.dt_datasets_rows_selected.length == 0))",
              "Please first select a dataset row in step", strong("2. Select")),
            conditionalPanel(
              condition = "input.dt_datasets_rows_selected.length > 0",
              fileInput(
                "inFile", "Choose File(s)",
                multiple = T,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")))),
          mainPanel(
            plotOutput("fig"),
            DTOutput("tbl"))))
  ))

server <- function(input, output, session) {
  
  output$g_name  = renderText({ input$g_name })
  output$g_email = renderText({ input$g_email })
  output$g_image = renderUI({ img(src=input$g_image) })
  output$row_sel = renderText(input$dt_datasets_rows_selected)
  
  values <- reactiveValues()
  values$load_success <- F
  
  observeEvent(input$btnNextSelect, {
    updateTabsetPanel(session, "tabs", selected = "2. Select")
  })
  
  observeEvent(input$btnNextUpload, {
    updateTabsetPanel(session, "tabs", selected = "3. Upload")
  })
  
  output$dt_datasets <- DT::renderDataTable({
    req(input$g_email)

    read_csv(datasets$url_csv) %>% 
      mutate(
        editor = str_split(`Contributor Email`, "[, ]+")) %>% 
      unnest(editor) %>% 
      filter(editor == input$g_email)
  
  }, 
  selection = "single",
  style     = "bootstrap", 
  rownames  = F,
  options   = list(
    pageLength = Inf, 
    searching  = F, 
    bPaginate  = F, 
    info       = F))
  
  get_dataset <- reactive({
    req(input$dt_datasets_rows_selected)
    
    values$load_success <- F
    
    read_csv(datasets$url_csv) %>% 
      mutate(
        editor = str_split(`Contributor Email`, "[, ]+")) %>% 
      unnest(editor) %>% 
      filter(
        editor == input$g_email) %>% 
      slice(input$dt_datasets_rows_selected)
  })
  
  get_data <- reactive({
    req(input$inFile)
    
    dataset <- get_dataset()

    values$load_success <- F
    tryCatch(
      {
        d <- get(dataset$dataset_code, pos = "package:ecodata")
        
        values$load_success <- T
      },
      error = function(e) {
        stop(safeError(e))
      },
      finally = setwd(dir_uploader))
    
    d
  })
  
  
  output$fig <- renderImage({
    req(input$inFile)
    
    dataset <- get_dataset()
    data    <- get_data()

    tryCatch(
      {
        dir_create(glue("{dir_ecodata}/tmp-uploader"))
        tmp_user_dataset_plot_R <- glue("{dir_ecodata}/tmp-uploader/{input$g_email}_{dataset$dataset_code}_plot.R")
        plot_png                <- here(glue("data/{input$g_email}/{dataset$dataset_code}/{dataset$dataset_code}.png"))
        
        # image too big using session$clientData$pixelratio per https://shiny.rstudio.com/articles/images.html
        fig_dpi    <- 72
        fig_width  <- session$clientData$output_fig_width / fig_dpi
        fig_height <- session$clientData$output_fig_height / fig_dpi
        
        str_detect(dataset$plot_chunk, "LTL")
        
        rmd_group <- str_replace(basename(dataset$plot_chunk), "^(.*)(\\.Rmd.*)$", "\\1")
        
        setup_R_files <- list.files(glue("{dir_ecodata}/chunk-scripts"), glue("{rmd_group}.*-setup.R"))
        setup_R       <- paste(glue("source(here('chunk-scripts/{setup_R_files}'))"), collapse = "\n")
        
        plot_code  <- glue("
          library(here)
          
          {setup_R}
          source(here('{dataset$plot_chunk}'))
          
          ggsave('{plot_png}', width = {fig_width}, height = {fig_height}, dpi = {fig_dpi})
          ") # cat(plot_code)
        writeLines(plot_code, tmp_user_dataset_plot_R)
        system(glue("cd {dir_ecodata}; Rscript {tmp_user_dataset_plot_R}"))
        
        list(src = plot_png,
             alt = glue("Plot output for {dataset$dataset_code}"))
      },
      error = function(e) {
        stop(safeError(e))
      })
    
  })
  
  output$tbl <- renderDT({
    req(input$inFile)
    req(values$load_success)
    
    get(dataset$dataset_code, pos = "package:ecodata")
  })
}

shinyApp(ui = ui, server = server)