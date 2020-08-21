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
  readr,dplyr,tidyr,purrr,readxl,
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

select = dplyr::select
options(shiny.maxRequestSize = 30*1024^2) # 30MB limit

# variables ----

dir_ecodata_src  <- "/share/github/ecodata_uploader/ecodata"
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
            Meanwhile please email: ben@ecoquants.com, kimberly.bastille@noaa.gov."),
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
                  ".csv",
                  ".xls",
                  ".xlsx"))),
            uiOutput("uiSubmit")),
          mainPanel(
            plotOutput("plot4size"),
            uiOutput("uiUploadSuggestion"),
            uiOutput("uiSubmitted"),
            uiOutput("figs"),
            DTOutput("tbl"))))
  ))

server <- function(input, output, session) {

  # output$plot4size ----
  output$plot4size <- renderImage({
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    
    # Generate a png
    png(outfile, width=400, height=400)
    #hist(rnorm(50))
    dev.off()
    
    # Return a list
    list(
      src   = outfile,
      alt   = "This is alternate text",
      style = "display: none;")
  }, 
  deleteFile = TRUE,
  outputArgs = list(style = "display:none"))
  
  # server variables ----  
  output$g_name  = renderText({ input$g_name })
  output$g_email = renderText({ input$g_email })
  output$g_image = renderUI({ img(src=input$g_image) })
  output$row_sel = renderText(input$dt_datasets_rows_selected)
  
  # reactives ----
  
  values <- reactiveValues(
    load_success       = F,
    plot_success       = F,
    submit_success     = F,
    upload_state       = NULL,
    pull_request       = NULL,
    git_branch         = NULL,       
    dir_ecodata_branch = NULL)
  
  observeEvent(input$inFile, {
    values$upload_state   <- 'uploaded'
    values$submit_success <- F
  })
  
  git_branch_freshen <- function(){
    
    # https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
    gh_pat <- readLines("/share/iea-uploader_github-personal-access-token_bbest.txt")
    
    dataset <- get_dataset()
    
    git_branch_reset_cmds <- glue(
      "
      # set git user defaults
      git config --global user.email 'ben@ecoquants.com'
      git config --global user.name 'Ben Best'
      GITHUB_TOKEN={gh_pat}
      GITHUB_USER=bbest
      
      # get latest
      #git fetch -p
      
      # delete local branch if exists
      #if [ `git branch --list {values$git_branch}` ]
      #then
      #  echo 'deleting local branch {values$git_branch}'
      #  git branch -D {values$git_branch}
      #fi
      
      # delete remote branch if exists
      #if [ `git branch -r | grep '{values$git_branch}' | wc -l` == '1' ]
      #then
      #  echo 'deleting remote branch {values$git_branch}'
      #  git push origin --delete {values$git_branch}
      #fi
      
      # branch and reset
      cd {values$dir_ecodata_branch}
      git fetch
      git pull
      git checkout -b {values$git_branch}
      git reset
      git checkout .
      git clean -f -d

      git branch --set-upstream-to=origin/{values$git_branch} {values$git_branch}
      git remote set-url origin https://bbest:{gh_pat}@github.com/bbest/ecodata.git
      git pull
      ")
    # git ls-remote --heads bbest@github.com:bbest/ecodata.git bdbest@gmail.com_slopewater | wc -l
    
    message(glue("before dir.exists() values$dir_ecodata_branch: {values$dir_ecodata_branch}"))
    if (dir.exists(values$dir_ecodata_branch)){
      git_cmds <- glue(
        "
        # get latest
        #cd {values$dir_ecodata_branch}
        #git checkout master
        #git pull
        #git fetch
          
        {git_branch_reset_cmds}")
    } else {
      git_cmds <- glue(
        "
        # freshen src
        cd {dir_ecodata_src}
        git config core.fileMode false
        git pull
        git fetch
        
        # copy with attributes recursively
        cp -ar {dir_ecodata_src} {values$dir_ecodata_branch}
        
        # turn off file chmod modifications
        cd {values$dir_ecodata_branch}
        git config core.fileMode false
        sudo chmod -R 775 {values$dir_ecodata_branch}

        {git_branch_reset_cmds}")
    }
    message(git_cmds)
    system(git_cmds)
  }
  
  git_commit_push_request <- function(comments = ""){
    # install once 
    
    # setup once on server in Terminal:
    #
    # # install hub
    # sudo ap-get update; sudo apt-get install hub
    #
    # # set github username default for user shiny
    # sudo su - shiny
    # cd '/share/github/ecodata_bdbest@gmail.com_slopewater'
    # hub pr list -h 'bdbest@gmail.com_slopewater' -f '%U|%i %t'
    # # enter username, password when prompted
    # 
    # # permission woes:
    # cd /share/github; sudo chmod -R 777 ecodata_uploader;  sudo chown -R shiny ecodata_uploader
    # cd /share/github; sudo chmod -R 777 iea-uploader/data; sudo chown -R shiny iea-uploader/data
    #
    # # restart app: 
    # touch /share/github/iea-uploader/restart.txt

    dataset <- get_dataset()
    
    if (nrow(dataset) == 1 & values$load_success & values$plot_success){
      # branch ecodata src
      
      sanitize_comments <- function(txt){
        txt <- txt %>% 
          str_replace_all("'", '"') %>% 
          str_replace_all(fixed('{'), "[") %>% 
          str_replace_all(fixed('}'), "]")
        txt
      }
      
      comments_sanitized <- sanitize_comments(comments)

      git_msg <- glue("{input$g_email} updating {dataset$dataset_code} dataset via Uploader app\n{comments_sanitized}")
      git_cmds <- glue(
        "
        cd '{values$dir_ecodata_branch}'
        git add -A && git commit -m '{git_msg}'
        git push --set-upstream origin '{values$git_branch}'
        ")
      
      # TODO: return URL to pull request
      # hub pr list -h 'bdbest@gmail.com_slopewater' -f '%U | %t'
      
      message(git_cmds)
      system(git_cmds)
      
      list_pull_request <- function(){
        git_cmd <- glue(
          "
          cd '{values$dir_ecodata_branch}'
          hub pr list -h '{values$git_branch}' -f '%U|%i %t'") # cat(git_cmd)
        pr_url_title <- system(git_cmd, intern = T)
      
        if (length(pr_url_title) == 0)
          return(NA)
        
        pr_url_title
      }
      
      pr <- list_pull_request()
      if (is.na(pr[1])){
        git_cmd <- glue(
          "cd '{values$dir_ecodata_branch}'
          hub pull-request {values$git_branch} -m '{git_msg}'")
        message(git_cmd)
        system(git_cmd)
        pr <- list_pull_request()
      }
      
      values$pull_request <- pr
      
      # share link to pull request
      return(pr)
    } else {
      return(NA)
    }
  }
  
  rm <- function(path){
    if (file.exists(path)) file_delete(path)}
  chmod <- function(path, mode = "775"){
    if (file.exists(path)) file_chmod(path, mode)}
  
  # observe dataset selection ----
  observeEvent(input$dt_datasets_rows_selected, {
    values$load_success   <- F
    values$plot_success   <- F
    values$submit_success <- F
    values$pull_request   <- NA
    values$upload_state   <- 'reset'
    
    dataset <- get_dataset()
    
    if (nrow(dataset) == 1){
      values$git_branch         <- glue("{input$g_email}_{dataset$dataset_code}")
      values$dir_ecodata_branch <- glue("{dir_ecodata_src}_{values$git_branch}")
      
      git_branch_freshen()
    }else{
      values$git_branch         <- NULL
      values$dir_ecodata_branch <- NULL
    }
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
      
      in_files <- input$inFile %>% 
        as_tibble() %>% 
        mutate(
          is_xl = path_ext(name) %in% c("xls","xlsx"))
      
      xl_files <- in_files %>% 
        filter(is_xl) %>% 
        mutate(
          xl    = datapath,
          type  = "xl",
          sheet = map(datapath, function(x) excel_sheets(x))) %>% 
        unnest(sheet) %>% 
        mutate(
          name     = glue("{sheet}.csv"),
          datapath = map2_chr(xl, sheet, function(xl, sheet){
            tmp_csv <- tempfile(fileext = ".csv")
            read_excel(xl, sheet) %>% 
              write_csv(tmp_csv)
            tmp_csv})) %>% 
        select(name, size, type, datapath)

      in_files <- in_files %>%
        filter(!is_xl) %>% 
        select(-is_xl) %>% 
        bind_rows(
          xl_files)

      return(in_files)
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
      file1_csv <- glue("{values$dir_ecodata_branch}/data-raw/{files[1]}")
      
      file_url_pfx <- "https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw"
      file_links   <- lapply(files, function(f) a(f, href = glue("{file_url_pfx}/{f}"), target="_blank")) %>% tagList()
      
      get_Rfiles <- function(){
        plot_R <- str_split(dataset$plot_chunks, ", ", simplify=T) %>% as.vector()
        
        r_tbl <- tibble(
          Operation = "Load",
          R_path    = glue("data-raw/get_{dataset$dataset_code}.R")) %>% 
          bind_rows(
            tibble(
              Operation = "Plot",
              R_path    = glue("chunk-scripts/{plot_R}"))) %>% 
          mutate(
            R_url   = glue("https://github.com/NOAA-EDAB/ecodata/blob/master/{R_path}"),
            R_fname = basename(R_path),
            `R script` = glue("<a href='{R_url}' target='_blank'>{R_fname}</a>")) %>% 
          select(Operation, `R script`)
        
        tagList(
          r_tbl %>% 
            kable(escape = F, table.attr = "class='kable'") %>% 
            HTML())
      }

      get_preview <- function(){
        if (!file.exists(file1_csv)) return("")
        tagList(
          ifelse(is_files, "They", "It"), "should have header names and values like",
          ifelse(is_files, glue("this ({basename(file1_csv)}):"), "this:"), br(),
          read_csv(file1_csv) %>% head() %>% kable(table.attr = "class='kable'") %>% HTML())
      }
      
      tagList(
        glue("Please upload the following {length(files)} {ifelse(is_files, 'files', 'file')} (or as sheets without the '.csv' in an Excel file):"), file_links, ".", br(), br(),
        get_preview(),br(),br(),
        "These files will be processed with the following:",
        get_Rfiles())
    }
  })
  
  load_data <- function(dataset){

    values$load_success <- F
    
    dataset_files <- str_split(dataset$dataset_files, ", ", simplify = T) %>% as.vector()
    
    files_dest <- glue("{values$dir_ecodata_branch}/data-raw/{file_input()$name}")

    message(glue('Sys.info()[["effective_user"]]: {Sys.info()[["effective_user"]]}'))
    load_log <- glue(
      "{dir_uploader}/www/figures/{input$g_email}/{dataset$dataset_code}_load_Rlog.txt")
    dir_create(dirname(load_log))
    chmod(dirname(load_log), "775")
    
    res <- system(glue("cd {values$dir_ecodata_branch}; Rscript {load_R} 2>{load_log}"), intern = T)
    
    # catch error
    if (!is.null(attr(res, "status")) && attr(res, "status") == 1){
      log <- readLines(load_log) %>% paste(collapse = "\n")
      stop(paste(
        "loading dataset within ecodata R package:\n\n----\n", 
        glue("source(data-raw/get_{dataset$dataset_code}.R)"), 
        "\n----\nHere is a log of messages from running above code:\n----\n", 
        log, 
        "\n----\nPlease try uploading fixed file(s)."))
    }
    
    load_all(values$dir_ecodata_branch)
    
    d <- get(dataset$dataset_code, pos = "package:ecodata")
    
    values$load_success <- T

    d
  }
  
  # DEBUG ----
  # input = list(g_email = "bdbest@gmail.com")
  # dataset = list(
  #   dataset_code = "slopewater",
  #   dataset_files = "slopewater_proportions.csv",
  #   plot_chunks = "LTL.Rmd-wsw-prop.R")
  # # dataset = list(
  # #   dataset_code = "bottom_temp",
  # #   dataset_files = "bot_temp_SS.csv, bot_temp_GOM.csv, bot_temp_GB.csv, bot_temp_MAB.csv",
  # #   plot_chunks = "LTL.Rmd-MAB-bot-temp.R, LTL.Rmd-NE-bot-temp.R")
  # session = list(clientData = list(
  #   output_fig_width  = 600,
  #   output_fig_height = 400))
  # values = list(
  #   git_branch = glue("{input$g_email}_{dataset$dataset_code}"),
  #   dir_ecodata_branch = glue("{dir_ecodata_src}_{git_branch}"))
  
  # output$figs: executes load_data(), plot chunks, git_commit_push_request() ----
  output$figs <- renderUI({
    req(file_input())
    #req(session$clientData$output_plot4size_hidden == F)
    #req(! "output_plot4size_hidden" %in% names(session$clientData))
    
    dataset <- get_dataset()
    
    # validate uploaded files ----
    
    dataset_files <- str_split(dataset$dataset_files, ", ", simplify = T) %>% as.vector()
    
    message(glue("in output$figs about to validate"))
    
    missing_files <- setdiff(dataset_files, file_input()$name) %>% paste(collapse = ", ")
    extra_files   <- setdiff(file_input()$name, dataset_files) %>% paste(collapse = ", ")
    
    validate(
      need(
        missing_files == "", 
        message = paste(
          "Missing files:", missing_files,
          ifelse(
            nchar(extra_files) > 0, 
            paste('\nRename one of these extra files?', extra_files),
            ""),
          "\nPlease try uploading again with fixed file names."))
    )
    
    # validate uploaded fields ----
    
    get_flds_type <- function(tbl_paths){
      tbl_paths %>% 
        mutate(
          data = map(path, function(x) read_csv(x)),
          flds_type = map(data, function(x){
            flds  <- names(x)
            types <- sapply(x, class)
            glue("{flds} <{types}>")
          })) %>% 
        select(-data, -path) %>% 
        unnest(flds_type)
    }
    
    d_files <- tibble(
      dfile = str_split(dataset$dataset_files, ", ", simplify = T) %>% as.vector()) %>% 
      mutate(
        fld_dataset = T,
        path = glue("{values$dir_ecodata_branch}/data-raw/{dfile}")) %>% 
      get_flds_type()
    
    u_files <- file_input() %>% 
      as_tibble() %>% 
      select(dfile = name, path = datapath) %>% 
      mutate(
        fld_upload = T) %>% 
      get_flds_type()
    u_files
    
    du_files <- d_files %>% 
      full_join(
        u_files, 
        by = c("dfile", "flds_type")) %>% 
      arrange(dfile, fld_dataset, flds_type, fld_upload) %>% 
      select(dfile, flds_type, fld_dataset, fld_upload) %>% 
      filter(is.na(fld_dataset) | is.na(fld_upload)) %>% 
      group_by(dfile) %>% 
      nest(data = -dfile) %>% 
      mutate(
        str_flds_missing = map_chr(data, function(d){
          d %>% 
            filter(fld_dataset) %>% 
            pull(flds_type) %>% 
            paste(collapse = ", ")}),
        str_flds_extra = map_chr(data, function(d){
          d %>% 
            filter(fld_upload) %>% 
            pull(flds_type) %>% 
            paste(collapse = ", ")})) %>% 
      select(-data) %>% 
      filter(nchar(str_flds_missing) > 0) %>% 
      mutate(
        str_missing = glue("  * {dfile}: {str_flds_missing}"),
        str_extra   = glue("  * {dfile}: {str_flds_extra}"))
    
    validate(
      need(
        nrow(du_files) == 0, 
        message = paste0(
          "Missing columns <class> in the following files:\n", 
          paste(du_files$str_missing, collapse = "\n"),
          "\nRename extra columns <class>?\n",
          paste(du_files$str_extra, collapse = "\n"),
          "\nPlease try uploading again with fixed columns.")))

    # load new data ----
    data <- load_data(dataset)

    message(glue("in output$figs about to plot"))
    
    # figure (https://shiny.rstudio.com/articles/images.html)
    fig_dpi    <- 72
    #fig_width  <- session$clientData$output_fig_width / fig_dpi
    #fig_height <- session$clientData$output_fig_height / fig_dpi
    if (session$clientData$output_plot4size_hidden){
      fig_width_in  <- 6
      fig_height_in <- 4
      message("output_plot4size_hidden: TRUE")
    } else {
      fig_width_in  <- session$clientData$output_plot4size_width / fig_dpi %>% round(2)
      fig_height_in <- session$clientData$output_plot4size_height / fig_dpi %>% round(2)
      message("output_plot4size_hidden: FALSE")
    }
    
    # plot_www <- glue("{dir_uploader}/www/figures/{input$g_email}_{basename(plot_png)}")
    # plot_pfx <- here(glue("data/{input$g_email}/{dataset$dataset_code}/{dataset$dataset_code}"))
    # dir_create(dirname(plot_pfx))
    
    plot_chunks <- str_split(dataset$plot_chunks, ", ", simplify=T) %>% as.vector()
    rmd_grp     <- str_replace(plot_chunks[1], "^(.*)(\\.Rmd.*)$", "\\1")
    
    plot_chunk_to_tab_panel <- function(plot_chunk){
      
      plot_rgn <- ifelse(
        length(plot_chunks) > 1,
        str_replace(plot_chunk, "^(.*?)(\\.Rmd)-(.*?)-(.*)\\.R$", "\\3"),
        "plot")
      
      plot_pfx <- glue(
        "{dir_uploader}/www/figures/{input$g_email}/{dataset$dataset_code}_{plot_rgn}")
      plot_R    <- glue("{plot_pfx}.R")
      plot_log  <- glue("{plot_pfx}_Rlog.txt")
      plot_png  <- glue("{plot_pfx}.png")
      plot_img  <- glue("./figures/{input$g_email}/{basename(plot_png)}")
      dir_create(dirname(plot_pfx))
      chmod(dirname(plot_pfx), "775")

      message(glue("\nplot_R: {plot_R} -> {plot_png}\n\n"))
      
      walk(c(plot_R, plot_png, plot_log), rm)

      setup_R_files <- tibble(
        file = list.files(glue("{values$dir_ecodata_branch}/chunk-scripts"), glue("{rmd_grp}.*-setup.R")),
        nchar = nchar(file)) %>% 
        arrange(nchar) %>% 
        pull(file)
      setup_R <- paste(glue("source(here('chunk-scripts/{setup_R_files}'))"), collapse = "\n") # cat(setup_R)
      plot_code  <- glue("
          library(here)
          
          {setup_R}
          source(here('chunk-scripts/{plot_chunk}'))
        
          ggsave('{plot_png}', width = {fig_width_in}, height = {fig_height_in}, dpi = {fig_dpi})") # cat(plot_code)
      message(c(glue("\nplot_code -> {plot_R}:\n"), plot_code, "\n\n"))
      writeLines(plot_code, plot_R)
      chmod(plot_R, "775")
      cmd <- glue("cd {values$dir_ecodata_branch}; Rscript {plot_R} 2>{plot_log}")
      res <- system(cmd, intern=T)
      walk(c(plot_png, plot_log), chmod, mode = "775")
      
      # catch error
      if (!is.null(attr(res, "status")) && attr(res, "status") == 1){
        log <- readLines(plot_log) %>% paste(collapse = "\n")
        
        stop(paste(
          "plotting dataset within ecodata R package:\n\n----\n", 
          plot_code, 
          "\n----\nHere is a log of messages from running above code:\n----\n", 
          log, 
          "\n----\nPlease try uploading fixed file(s)."))
      }
      stopifnot(file_exists(plot_png))
  
      tabPanel(
        plot_rgn, 
        img(
          src = plot_img,
          alt = glue("Plot output for {dataset$dataset_code}")))
    }

    tabset <- do.call(
      tabsetPanel,
      lapply(
        plot_chunks, 
        plot_chunk_to_tab_panel))
    
    message(glue("in output$figs values$plot_success"))
    values$plot_success <- T    
    tabset
  })
  
  # output$tbl ----
  output$tbl <- renderDT({
    req(file_input())
    req(values$load_success)
    req(values$plot_success)
    
    dataset <- get_dataset()
    
    get(dataset$dataset_code, pos = "package:ecodata")
  })
  
  # output$uiSubmit ----
  output$uiSubmit <- renderUI({
    
    if (!values$load_success | !values$plot_success | values$submit_success)
      return(tagList())
    
    tagList(
      "Success loading and plotting data!", br(),
      "Finally, are you ready to", strong("Submit"), "these updated data files for internal review?", br(), br(),
      
      textAreaInput("txtComments", "Comments", placeholder = "(optional with submission)"),
      
      actionButton("btnSubmit", "Submit", class="btn-primary"))
  })
  
  # observe btnSubmit
  observeEvent(input$btnSubmit, {
    pr <- git_commit_push_request(comments = input$txtComments)
    values$submit_success <- T
  })
  
  
  # output$uiSubmitted ----
  output$uiSubmitted <- renderUI({
    if (!values$submit_success | is.na(values$pull_request))
      return(tagList())

    pr_url_title <- values$pull_request %>% 
      str_split(fixed("|"), simplify = T) %>% as.vector() %>% str_trim()
    pr_a <- a(pr_url_title[2], href=pr_url_title[1], target="_blank")
    
    tagList(
      "SUCCESS! You can visit the submitted file changes to the ecodata R package here (and further comment after signing into Github):", br(), pr_a, br(), br())
  })
  
}

shinyApp(ui = ui, server = server)