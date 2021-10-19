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
  
  # load_data ----
  load_data <- function(dataset){
    
    values$load_success <- F
    
    #dataset_files <- str_split(dataset$dataset_files, ", ", simplify = T) %>% as.vector()
    #dataset_files <- dataset$data_files %>% unlist()
    
    files_dest <- glue("{values$dir_ecodata_branch}/data-raw/{file_input()$name}")
    
    file_move(file_input()$datapath, files_dest)
    walk(files_dest, chmod, mode = "775")
    
    message(glue('Sys.info()[["effective_user"]]: {Sys.info()[["effective_user"]]}'))
    
    load_R <- glue("{values$dir_ecodata_branch}/data-raw/get_{dataset$dataset_id}.R")
    load_log <- glue(
      "{dir_uploader}/www/figures/{input$g_email}/{dataset$dataset_id}_load_Rlog.txt")
    
    dir_create(dirname(load_log))
    #browser()
    chmod(dirname(load_log), "775")
    
    load_cmd <- glue("cd {values$dir_ecodata_branch}; Rscript {load_R} 2>{load_log}")
    
    #stop(paste("load_cmd DEBUG:\n\n----\n",  load_cmd))
    # cd /share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater; Rscript /share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater/data-raw/get_slopewater.R 2>/share/github/iea-uploader/www/figures/bdbest@gmail.com/slopewater_load_Rlog.txt
    # cd /share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater; Rscript /share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater/data-raw/get_slopewater.R 2>/share/github/iea-uploader/www/figures/bdbest@gmail.com/slopewater_load_Rlog.txt
    # browser()
    res <- system(load_cmd, intern = T)
    
    # values = list(
    #   dir_ecodata_branch="/share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater")
    # dataset = list(dataset_code)
    # load_R = glue("{values$dir_ecodata_branch}/data-raw/get_{dataset$dataset_code}.R")
    # 
    # ls -la /share/github/iea-uploader/www/figures/bdbest@gmail.com/slopewater_load_Rlog.txt
    # 
    # load("/share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater/data/slopewater.rda")
    # tail(slopewater)
    # View(slopewater)
    # browser()
    
    # catch error
    if (!is.null(attr(res, "status")) && attr(res, "status") == 1){
      log <- readLines(load_log) %>% paste(collapse = "\n") # %>% cat()
      stop(
        # cat(
          paste(
        "loading dataset within ecodata R package:\n\n----\n", 
        glue("source(data-raw/get_{dataset$dataset_id}.R)"), 
        "\n----\nHere is a log of messages from running above code:\n----\n", 
        log, 
        "\n----\nPlease try uploading fixed file(s).")
        # )
        )
    }
    
    load_all(values$dir_ecodata_branch)
    
    d <- get(dataset$dataset_id, pos = "package:ecodata")
    
    values$load_success <- T
    
    d
  }
  
  # observe dataset selection ----
  observeEvent(input$dt_datasets_rows_selected, {
    values$load_success   <- F
    values$plot_success   <- F
    values$submit_success <- F
    values$pull_request   <- NA
    values$upload_state   <- 'reset'
    
    dataset <- get_dataset()

    if (nrow(dataset) == 1){
      values$git_branch         <- glue("{input$g_email}_{dataset$dataset_id}")
      values$dir_ecodata_branch <- glue("{dir_ecodata_src}_{values$git_branch}")
      
      git_branch_freshen(values$git_branch, values$dir_ecodata_branch)
    }else{
      values$git_branch         <- NULL
      values$dir_ecodata_branch <- NULL
    }
  })
  
  git_commit_push_request <- function(comments = ""){
    # install once 
    
    # setup once on server in Terminal:
    #
    # # install hub
    # sudo apt-get update; sudo apt-get install hub
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
    
    gh_pat <- readLines("/share/iea-uploader_github-personal-access-token_bbest.txt")
    Sys.setenv(GITHUB_TOKEN = gh_pat)
    
    #browser()

    if (nrow(dataset) == 1 & values$load_success & values$plot_success){
      # branch ecodata src
      
      comments_sanitized <- sanitize_comments(comments)
      
      git_msg <- glue("{input$g_email} updating {dataset$dataset_id} dataset via Uploader app\n{comments_sanitized}")
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
  
  # get_datasets() ----
  get_datasets <- reactive({
    req(input$g_email)
    
    if (input$g_email %in% admin_emails){
      d <- datasets
    } else {
      d <- datasets_stewards  %>% 
        filter(email == input$g_email)
    }
    
    d
  })
  
  # get_dataset() ----
  get_dataset <- reactive({
    req(input$dt_datasets_rows_selected)
    
    get_datasets() %>% 
      slice(input$dt_datasets_rows_selected)
  })
  
  # output$dt_datasets ----
  output$dt_datasets <- DT::renderDataTable({
    req(input$g_email)

    # read_csv(datasets$url_csv) %>% 
    #   mutate(
    #     editor = str_split(`Contributor Email`, "[, ]+")) %>% 
    #   unnest(editor) %>% 
    #   filter(editor == input$g_email)
    
    # TODO: add admin (kimberly.bastille@noaa / ) permissions
    
    get_datasets() %>% 
      mutate(
        dataset_id = glue("<a href='{tech_doc_url}' target='_blank'>{dataset_id}</a>")) %>% 
      select(dataset_id)
  }, 
  selection = "single",
  style     = "bootstrap", 
  rownames  = T,
  escape    = F,
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
      
      files     <- dataset %>% unnest(data_files) %>% pull(data_files)
      is_files  <- ifelse(length(files) > 1, T, F)
      
      # TODO: handle RData, like blue_runner: data-raw/Blue_runner_presence.RData
      file1_csv <- glue("{values$dir_ecodata_branch}/data-raw/{files[1]}")
      
      file_url_pfx <- "https://github.com/NOAA-EDAB/ecodata/blob/master/data-raw"
      file_links   <- lapply(files, function(f) a(f, href = glue("{file_url_pfx}/{f}"), target="_blank")) %>% tagList()

      #browser()
      tagList(
        glue("Please upload the following {length(files)} {ifelse(is_files, 'files', 'file')} (or as sheets without the '.csv' in an Excel file):"), file_links, ".", br(), br(),
        
        # TODO: ch_bay_sal: SR_Salinity.csv has no headers so 
        preview_csv(file1_csv, is_files),br(),br(),
        
        "These files will be processed with the following:",
        get_Rfiles(dataset))
    }
  })
  
  
  
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
    
    # browser()
    #dataset_files <- str_split(dataset$dataset_files, ", ", simplify = T) %>% as.vector()
    data_files <- dataset$data_files %>% unlist()
    
    message(glue("in output$figs about to validate"))
    
    missing_files <- setdiff(data_files, file_input()$name) %>% paste(collapse = ", ")
    extra_files   <- setdiff(file_input()$name, data_files) %>% paste(collapse = ", ")
    
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
    d_files <- tibble(
      # dfile = str_split(dataset$dataset_files, ", ", simplify = T) %>% as.vector()) %>% 
      dfile = data_files) %>% 
      mutate(
        fld_dataset = T,
        path = glue("{values$dir_ecodata_branch}/data-raw/{dfile}")) %>% 
      get_flds_type()
    
    # browser()
    u_files <- file_input() %>% 
      as_tibble() %>% 
      select(dfile = name, path = datapath) %>% 
      mutate(
        fld_upload = T) %>% 
      get_flds_type()
    #u_files
    
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
    # browser()

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
    
    # plot_chunks <- str_split(dataset$plot_chunks, ", ", simplify=T) %>% as.vector()
    plot_scripts <- dataset$plot_scripts %>% unlist()
    rmd_grp      <- str_replace(plot_scripts[1], "^(.*)(\\.Rmd.*)$", "\\1")

    tabset <- do.call(
      tabsetPanel,
      lapply(
        # plot_chunks, 
        # plot_chunk_to_tab_panel))
        names(plot_scripts), 
        plot_chunk_to_tab_panel, plot_scripts, dataset, input$g_email, values$dir_ecodata_branch, rmd_grp, fig_width_in, fig_height_in, fig_dpi))
    
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
    
    
    # dataset <- list(
    #   dataset_code = "slopewater")
    # values <- list(
    #   dir_ecodata_branch = "/share/github/ecodata_uploader/ecodata_bdbest@gmail.com_slopewater")
    
    #d0 <- get(dataset$dataset_code, pos = "package:ecodata")
    
    load_all(values$dir_ecodata_branch)
    
    #slopewater
    #d1 <- get(dataset$dataset_code, pos = "package:ecodata")
    
    # browser()
    d <- get(dataset$dataset_id, pos = "package:ecodata")
    # TODO: not fetching updated
    # rda <- glue("{values$dir_ecodata_branch}/data/{dataset$dataset_code}.rda")
    # d <- read_rds(rda)
    # dat.s3 <- miceadds::load.Rdata2( filename="data_s3.Rdata")
    # readRDS()
    # slopewater %>% tail()
    # slopewater %>% summary()
    # load(rda)
    # local({
    #   load(rda)
    #   ls()
    # })
    d
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