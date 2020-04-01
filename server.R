shinyServer(function(input, output) {
  
  get_df <- reactive({
    
    req(input$file1)
    
    inFile <- input$file1
    # if (is.null(inFile))
    #   return(NULL)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    df
  })
  
  output$tbl <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    
    df <- get_df()
    
    # TODO: head
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$fig <- renderPlot({
    df <- get_df()
    
    slopewater <- df %>%
      dplyr::rename(Time = year, Var = water.mass.flavor, Value = prop) %>% 
      mutate(EPU = "GOM", Units = "unitless", Var2 = "proportion ne channel") %>% 
      unite(.,Var,c(Var,Var2), sep = " ")
    
    slopewater %>% 
      mutate(Var, Var = plyr::mapvalues(Var, from = c("WSW proportion ne channel",
                                                      "LSLW proportion ne channel"),
                                        to = c("WSW","LSLW"))) %>% 
      dplyr::rename(Flavor  = Var) %>% 
      ggplot() +
      geom_line(aes(x = Time, y = Value, color = Flavor))+
      geom_point(aes(x = Time, y = Value, color = Flavor)) +
      ylab("Percent of Total Slopewater") +
      ggtitle("Slopewater Proportions in NE Channel")+
      theme_bw()+
      theme(strip.background = element_blank())
    
  })
  
  
})
