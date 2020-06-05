server <- function(input, output, session) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  auth_out <- secure_server(
    check_credentials = check_credentials(
      db         = users_sqlite,
      passphrase = "supersecret"), 
    timeout = 0, 
    inputs_list = list(
      group = list(
        fun = "selectInput",
        args = list(
          choices = c("alls", "restricted"),
          multiple = TRUE,
          selected = c("all", "restricted")))))
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(auth_out)
  })
  
  rv <- reactiveValues(
    datasets = datasets)
  
  output$dt_datasets <- DT::renderDataTable({
    req(auth_out)
    req(auth_out$user)
    # auth_out$user
    #usr <- "bbest"
    d <- rv[["datasets"]] %>% 
      mutate(
        editor = str_split(`Uploader Editors`, " ")) %>% 
      unnest(editor) %>% 
      filter(editor == auth_out$user)

    d #, options = list(lengthChange = FALSE))
  }, 
  selection = "single", 
  editable  = T,
  style     = "bootstrap", 
  rownames  = F,
  server    = F,
  options   = list(
    pageLength = Inf, 
    #dom        = 't',
    searching  = F, 
    bPaginate  = F, 
    info       = F))
  
  proxy_datasets <- dataTableProxy("dt_datasets")
  
  # edit cell
  observeEvent(input$dt_datasets_cell_edit, {
    info <- input$dt_datasets_cell_edit
    i <- info$row
    j <- info$col #+ 1L  # column index offset by 1
    v <- info$value
    rv[["datasets"]][i, j] <- coerceValue(v, rv[["datasets"]][i, j])
    replaceData(proxy_datasets, rv[["datasets"]], resetPaging = F, rownames = F)  # important
  })

  # delete row
  observe({
    toggle("delete_datasets_row", condition = nrow(rv[["datasets"]]) > 0 & !is.null(input$dt_datasets_rows_selected))
  })
  observeEvent(input$delete_datasets_row,{
    req(input$dt_datasets_rows_selected)
    i <- input$dt_datasets_rows_selected
    rv[["datasets"]] <- rv[["datasets"]][-i,]
    replaceData(proxy_datasets, rv[["datasets"]], resetPaging = F, rownames = F)  # important
  })
  
  # add row
  observeEvent(input$add_teams_row,{
    #addRow() only works when server = FALSE
    req(rv[["datasets"]])
    
    # update all of the relevant tables
    ti <- nrow(rv[["datasets"]]) + 1L
    #rv[["datasets"]][ti,] <- list(tid, sample(ex_seasons, 1), sample(ex_leagues, 1), sample(ex_coaches, 1))
    rv[["datasets"]][ti,] <- names(rv[["datasets"]]) %>% as.list()
    replaceData(proxy_datasets, rv[["datasets"]], resetPaging = F, rownames = F)  # important
  })
  
  # observeEvent(input$btn_newdataset, {
  #   
  # })
    
}