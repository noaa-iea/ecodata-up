# libraries ----
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
  knitr,rmarkdown,DT,
  # package
  devtools,
  # ecodata:chunk-scripts/*-setup.R
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
  vegan,
  # ecodata::StatGLS()
  magrittr,
  AICcmodavg,
  nlme)

options(shiny.maxRequestSize = 30*1024^2) # 30MB limit

# variables ----

dir_ecodata_src  <- "/share/github/ecodata"
dir_uploader     <- here()

datasets <- list(
  name      = "SOE 2020 Contributors (copy 2020-04-24)",
  url_csv   = "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/gviz/tq?tqx=out:csv&sheet=0",
  url_edit  = "https://docs.google.com/spreadsheets/d/1ULxD4yIl1Mb189Q6d1iRy0cKdfd_B4XRvz1m4ad52Y4/edit",
  local_csv = "data/datasets.csv")

# ui ----

ui <- tagList(
  tags$head(
    tags$meta(name="google-signin-scope",content="profile email"),
    # https://console.developers.google.com/apis/credentials?authuser=1&project=iea-uploader (ben@ecoquants.com as authuser=1)
    tags$meta(name="google-signin-client_id", content="596429062120-tbgia6e94mtqc8eoo27iqqd7l89s6apr.apps.googleusercontent.com"),
    HTML('<script src="https://apis.google.com/js/platform.js?onload=init"></script>'),
    includeScript("signin.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    useShinyjs()
  ),
  
  navbarPage(
    id = "tabs",
    title = "IEA Uploader",
    #windowTitle = "Browser window title",
    
    # ui tabPanel 1. Login ----
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
    
    # ui tabPanel 2. Select ----
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
      
    # ui tabPanel 3. Upload ----
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
              condition = "((input.g_id !== null) && (input.dt_datasets_rows_selected.length > 0))",
              fileInput(
                "inFile", "Choose File(s)",
                multiple = T,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")))),
          mainPanel(
            uiOutput("uiUploadSuggestion"),
            uiOutput("figs"),
            DTOutput("tbl"))))
  ))

server <- function(input, output, session) {

  # server variables ----  
  output$g_name  = renderText({ input$g_name })
  output$g_email = renderText({ input$g_email })
  output$g_image = renderUI({ img(src=input$g_image) })
  output$row_sel = renderText(input$dt_datasets_rows_selected)
  
  # reactives ----
  
  values <- reactiveValues(
    load_success = F,
    plot_success = F,
    upload_state = NULL,
    git_branch  = NULL,       
    dir_ecodata_branch = NULL)
  
  observeEvent(input$inFile, {
    values$upload_state <- 'uploaded'
  })
  
  # git_branch_freshen ----
  git_branch_freshen <- function(){
    
    # https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
    gh_pat <- readLines("/share/iea-uploader_github-personal-access-token_bbest.txt")
    
    git_branch_delete_new_cmds <- glue(
      "
      cd {values$dir_ecodata_branch}
      
      if [ `git branch --list {git_branch}` ]
      then
        git branch -D {git_branch}
      fi
      git checkout -b {git_branch}
      git remote set-url origin https://bbest:{gh_pat}@github.com/bbest/ecodata.git
      git branch --set-upstream-to=origin/{values$git_branch} {values$git_branch}
      git pull
      ")
    
    if (dir_exists(values$dir_ecodata_branch)){
      git_cmds <- glue(
        "
        # get latest
        cd {values$dir_ecodata_branch}
        git checkout master
        git pull
          
        {git_branch_delete_new_cmds}") # cat(git_cmds)
      system(git_cmds)
    } else {
      git_cmds <- glue(
        "
        # freshen src
        cd {dir_ecodata_src}
        git pull
        
        # copy with attributes recursively
        cp -ar {dir_ecodata_src} {values$dir_ecodata_branch}
        
        {git_branch_delete_new_cmds}")
    }
    system(git_cmds)
  }
  
  git_commit_push_request <- function(){
    # install once on server in Terminal: sudo ap-get update; sudo apt-get install hub
    
    # git config --global user.email "you@example.com"
    # git config --global user.name "Your Name"
    # test:
    #   echo "test 8 after setting bbest@github_pat" > test_branch_pull-request.txt
    
    dataset <- get_dataset()
    
    if (nrow(dataset) == 1 & values$load_success & values$plot_success){
      # branch ecodata src
      git_msg <- glue("{input$g_email} updating {dataset$dataset_code} dataset via Uploader app")
      git_cmds <- glue(
        "
        git add -A && git commit -m '{git_msg}'
        git push
        hub pull-request -m '{git_msg}'")
      cat(git_cmds)
      system(git_cmds)
  }
  
  # observe dataset selection ----
  observeEvent(input$dt_datasets_rows_selected, {
    values$load_success <- F
    values$plot_success <- F
    values$upload_state <- 'reset'
    
    dataset <- get_dataset()
    
    if (nrow(dataset) == 1){
      # branch ecodata src
      values$git_branch         <- glue("{input$g_email}_{dataset$dataset_code}")
      values$dir_ecodata_branch <- glue("{dir_ecodata}_{git_branch}")
      
      git_branch_freshen()
    
      plot_pfx <- here(glue("data/{input$g_email}/{dataset$dataset_code}/{dataset$dataset_code}"))
      
      
    }
    # setup fresh dir_ecodata
    
    
  })
  
  observeEvent(input$btnNextSelect, {
    updateTabsetPanel(session, "tabs", selected = "2. Select")
  })
  
  observeEvent(input$btnNextUpload, {
    updateTabsetPanel(session, "tabs", selected = "3. Upload")
  })
  
  # file_input() ----
  file_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$inFile)
    } else if (values$upload_state == 'reset') {
      return(NULL)
    }
  })
  
  # get_dataset() ----
  get_dataset <- reactive({
    req(input$dt_datasets_rows_selected)
    
    read_csv(datasets$url_csv) %>% 
      mutate(
        editor = str_split(`Contributor Email`, "[, ]+")) %>% 
      unnest(editor) %>% 
      filter(
        editor == input$g_email) %>% 
      slice(input$dt_datasets_rows_selected)
  })
  
  # output$dt_datasets ----
  output$dt_datasets <- renderDataTable({
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
  
  # output$uiUploadSuggestion ----
  output$uiUploadSuggestion <- renderUI({
    show_suggestion = !is.null(input$g_id) & length(input$dt_datasets_rows_selected) == 1 && !values$load_success
    message(glue("show_suggestion: {show_suggestion}"))
    if (show_suggestion){
      dataset <- get_dataset()
      
      files     <- str_split(dataset$dataset_files, ", ", simplify = T) %>% as.vector()
      is_files  <- ifelse(length(files) > 1, T, F)
      file1_csv <- glue("{dir_ecodata}/data-raw/{files[1]}")
      
      file_url_pfx <- "https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw"
      file_links   <- lapply(files, function(f) a(f, href = glue("{file_url_pfx}/{f}.csv"), target="_blank")) %>% tagList()

      get_preview <- function(){
        if (!file.exists(file1_csv)) return("")
        tagList(
          ifelse(is_files, "They", "It"), "should have header names and values like",
          ifelse(is_files, glue("this ({basename(file1_csv)}):"), "this:"), br(),
          read_csv(file1_csv) %>% head() %>% kable(table.attr = "class='kable'") %>% HTML())
      }
      
      tagList(
        glue("Please upload the following {length(files)} {ifelse(is_files, 'files:', 'file:')}"), file_links, ".", br(), br(),
        get_preview())
    }
  })
  
  
  load_data <- function(){

    values$load_success <- F
    
    tryCatch(
      {
        dataset       <- get_dataset()
        
        message(glue("in load_data(): {dataset$dataset_code}"))
        
        dataset_files <- str_split(dataset$dataset_files, ", ", simplify = T) %>% as.vector()
        
        file_move(file_input()$datapath, glue("{dir_ecodata}/data-raw/{file_input()$name}"))
        
        load_R <- glue("{dir_ecodata}/data-raw/get_{dataset$dataset_code}.R")
        
        system(glue("cd {dir_ecodata}; Rscript {load_R}"))
        
        load_all(dir_ecodata)
      
        d <- get(dataset$dataset_code, pos = "package:ecodata")
        
        values$load_success <- T
      },
      error = function(e) {
        stop(safeError(e))
      })
    
    d
  }
  
  # DEBUG ----
  # input = list(g_email = "bdbest@gmail.com")
  # dataset = list(
  #   dataset_code = "slopewater",
  #   dataset_files = "slopewater_proportions.csv",
  #   plot_chunks = "LTL.Rmd-wsw-prop.R")
  # dataset = list(
  #   dataset_code = "bottom_temp",
  #   dataset_files = "bot_temp_SS.csv, bot_temp_GOM.csv, bot_temp_GB.csv, bot_temp_MAB.csv",
  #   plot_chunks = "LTL.Rmd-MAB-bot-temp.R, LTL.Rmd-NE-bot-temp.R")
  # session = list(clientData = list(
  #   output_fig_width  = 600,
  #   output_fig_height = 400))
  # values = list(
  #   git_branch = glue("{input$g_email}_{dataset$dataset_code}"),
  #   dir_ecodata_branch = glue("{dir_ecodata}_{git_branch}"))
  
  # output$figs ----
  output$figs <- renderUI({
    req(file_input())
    
    dataset <- get_dataset()
    
    dataset_files <- str_split(dataset$dataset_files, ", ", simplify = T) %>% as.vector()
    
    message(glue("in output$figs about to validate"))
    validate(
      need(
        length(dataset_files) == nrow(file_input()), 
        message = glue(
          "Expecting {length(dataset_files)} files for this dataset.
          Uploaded {nrow(file_input())}.
          Please try again."))
      # TODO: check filenames match exactly, report on setdiff()
      # TODO: check header names & value types per file
    )
  
    # load new data
    data <- load_data()

    tryCatch(
      {
        
        message(glue("in output$figs about to plot"))
        
        # figure (https://shiny.rstudio.com/articles/images.html)
        fig_dpi    <- 72
        fig_width  <- session$clientData$output_fig_width / fig_dpi
        fig_height <- session$clientData$output_fig_height / fig_dpi
        
        plot_pfx <- here(glue("data/{input$g_email}/{dataset$dataset_code}/{dataset$dataset_code}"))
        dir_create(dirname(plot_pfx))
        
        plot_chunks <- str_split(dataset$plot_chunks, ", ", simplify=T) %>% as.vector()
        rmd_grp     <- str_replace(plot_chunks[1], "^(.*)(\\.Rmd.*)$", "\\1")
        
        # TODO: loop over chunks
        tab_panels = list()
        
        #for (plot_chunk in plot_chunks){ # plot_chunk = plot_chunks[2]
          
        plot_chunk_to_tab_panel <- function(plot_chunk){
          if (length(plot_chunks) > 1){
            plot_rgn <- str_replace(plot_chunk, "^(.*?)(\\.Rmd)-(.*?)-(.*)\\.R$", "\\3")
            plot_R   <- glue("{plot_pfx}_{plot_rgn}_plot.R")
            plot_png <- glue("{plot_pfx}_{plot_rgn}.png")
          } else {
            plot_rgn <- "ALL"
            plot_R   <- glue("{plot_pfx}_plot.R")
            plot_png <- glue("{plot_pfx}.png")
          }
          plot_www <- glue("{dir_uploader}/www/figures/{input$g_email}_{basename(plot_png)}")
          plot_img <- glue("./figures/{input$g_email}_{basename(plot_png)}")
          dir_create(dirname(plot_www))
          
          setup_R_files <- tibble(
            file = list.files(glue("{dir_ecodata}/chunk-scripts"), glue("{rmd_grp}.*-setup.R")),
            nchar = nchar(file)) %>% 
            arrange(nchar) %>% 
            pull(file)
          setup_R <- paste(glue("source(here('chunk-scripts/{setup_R_files}'))"), collapse = "\n") # cat(setup_R)
          
          plot_code  <- glue("
            library(here)
            
            {setup_R}
            source(here('chunk-scripts/{plot_chunk}'))
            
            ggsave('{plot_png}', width = {fig_width}, height = {fig_height}, dpi = {fig_dpi})
            ") # cat(plot_code)
          writeLines(plot_code, plot_R)
          system(glue("cd {dir_ecodata}; Rscript {plot_R}"))
          
          stopifnot(file_exists(plot_png))
          file_copy(plot_png, plot_www, overwrite = T)
          
          tabPanel(
            plot_rgn, 
            img(
              src = plot_img,
              alt = glue("Plot output for {dataset$dataset_code}")))
        }
      
        message(glue("in output$figs values$plot_success"))
        values$plot_success <- T
        
        do.call(
          tabsetPanel,
          lapply(
            plot_chunks, 
            plot_chunk_to_tab_panel))
      },
      error = function(e) {
        stop(safeError(e))
      })
    
  })
  
  # output$tbl ----
  output$tbl <- renderDT({
    req(file_input())
    req(values$load_success)
    
    dataset <- get_dataset()
    
    get(dataset$dataset_code, pos = "package:ecodata")
  })
}

shinyApp(ui = ui, server = server)