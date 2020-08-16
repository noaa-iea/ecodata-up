server <- function(input, output, session) {
  
  output$g_name  <- renderText({ input$g_name })
  output$g_email <- renderText({ input$g_email })
  output$g_image <- renderUI({ img(src=input$g_image) })
  
  get_datasets <- reactiveFileReader(
    intervalMillis = 1000,
    session,
    filePath       = datasets$url_csv,
    readFunc       = read_csv)
  
  output$dt_datasets <- DT::renderDataTable({
    req(input$g_email)
    
    message("input$g_email: {input$g_email}")
  
    #g_email <- "bdbest@gmail.com"
    d <- get_datasets() %>% 
      mutate(
        editor = str_split(`Contributor Email`, " ")) %>% 
      unnest(editor) %>% 
      filter(editor == input$g_email)

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
  
  observe({
    if (isTruthy(input$g_id)) {
      show("signout")
      hide("signin")
    } else {
      show("signin")
      hide("signout")
    }
  })
  
  # proxy_datasets <- dataTableProxy("dt_datasets")
  
  # edit cell
  # observeEvent(input$dt_datasets_cell_edit, {
  #   info <- input$dt_datasets_cell_edit
  #   i <- info$row
  #   j <- info$col #+ 1L  # column index offset by 1
  #   v <- info$value
  #   d[["datasets"]][i, j] <- coerceValue(v, d[["datasets"]][i, j])
  #   replaceData(proxy_datasets, d[["datasets"]], resetPaging = F, rownames = F)  # important
  # })
  # 
  # # delete row
  # observe({
  #   toggle("delete_datasets_row", condition = nrow(d[["datasets"]]) > 0 & !is.null(input$dt_datasets_rows_selected))
  # })
  # observeEvent(input$delete_datasets_row,{
  #   req(input$dt_datasets_rows_selected)
  #   i <- input$dt_datasets_rows_selected
  #   d[["datasets"]] <- d[["datasets"]][-i,]
  #   replaceData(proxy_datasets, d[["datasets"]], resetPaging = F, rownames = F)  # important
  # })
  # 
  # # add row
  # observeEvent(input$add_teams_row,{
  #   #addRow() only works when server = FALSE
  #   req(d[["datasets"]])
  #   
  #   # update all of the relevant tables
  #   ti <- nrow(d[["datasets"]]) + 1L
  #   #d[["datasets"]][ti,] <- list(tid, sample(ex_seasons, 1), sample(ex_leagues, 1), sample(ex_coaches, 1))
  #   d[["datasets"]][ti,] <- names(d[["datasets"]]) %>% as.list()
  #   replaceData(proxy_datasets, d[["datasets"]], resetPaging = F, rownames = F)  # important
  # })
  
  # observeEvent(input$btn_newdataset, {
  #   
  # })
    
}