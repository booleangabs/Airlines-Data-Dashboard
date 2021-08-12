getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Define server logic required to draw a histogram
server <- function(input, output) {
  ################### INPUT ####################
  select_airline <- eventReactive(input$go, {
    airline_name <- input$airline
    
    return(airline_name)
  })
  
  select_range <- eventReactive(input$go, {
    start <- input$true_date
    end <- input$true_date_2
    
    return(list(start = start, end = end))
  })
  
  select_dt <- eventReactive(input$go, {
    data <- input$data
    if(data == "Despesas"){
      return(master_df = read.csv('data/airline_expenses.csv'))
    }else{
      if(data == "Receita"){
        return(master_df = read.csv('data/airline_revenues.csv'))
      }else{
        return(master_df= read.csv('data/airline_passengers.csv'))
      }
    }
  })
  
  info_selected <- eventReactive(input$go, {
    data <- input$data
    return(data)
  })
  
  
  output$starttime <- renderUI({
    
    airline_name <- input$airline
    
    df <- read.csv('data/airline_expenses.csv')
    
    years <- df$Year
    names(years) <- years
    
    selectInput("true_date", "Inicio da análise", choices= years)
    
  })
  
  output$endtime <- renderUI({
    airline_name <- input$airline
    
    df <- read.csv('data/airline_expenses.csv')
    
    years <- df$Year
    names(years) <- years
    
    selectInput("true_date_2", "Fim da análise", choices= years)
    
  })
  
  output$timedate_comp <- renderUI({
    
    stock_name <- input$stock_comp
    
    df <- master_df %>% 
      filter(Index %in% stock_name)
    
    maxmin_time <- df %>% 
      group_by(Index) %>% 
      summarise(MD = min(Date)) %>% 
      .$MD %>% 
      max()
    
    minmax_time <- df %>% 
      group_by(Index) %>% 
      summarise(MD = max(Date)) %>% 
      .$MD %>% 
      min()
    
    min_time <- maxmin_time
    max_time <- minmax_time
    
    
  })
  
  ################ OUTPUT #####################
  Info_DataTable <- eventReactive(input$go,{
    
    master_df <- select_dt()
    
    range <- select_range()
    
    if(range[[1]]> range[[2]]){
      Erro <- "PERIODO INVALIDO"
      df_tb <-  data.frame(Erro)
      df_tb <- as.data.frame(t(df_tb))
      return(df_tb)
    }
    
    df <- master_df %>% filter(Year >= range[[1]]) 
    df <- df %>% filter(Year <= range[[2]])
    df <- df %>% select(select_airline())
    
    mean <- df %>% colMeans()
    median <- median(as.numeric(unlist(df)))
    mode <- getMode(unlist(df))
    
    #Desvio padrao amostral
    standard_deviation <- sd(unlist(df))
    
    Media <- mean[[1]]
    Mediana <- median[[1]]
    Moda <- mode[[1]]
    Desvio_Padrao <- standard_deviation[[1]]
    Max <- max(df)
    Min <- min(df)
    
    Airline <- input$airline
    
    df_tb <-  data.frame(Airline, Media, Mediana, Moda, Desvio_Padrao, Max, Min)
    
    df_tb <- as.data.frame(t(df_tb))
    
    # tb  <- as_tibble(cbind(nms = names(df_tb), t(df_tb)))
    # tb <- tb %>% 
    #     rename('InformaÃ§Ãµes' = nms,
    #            'Valores' = V2)
    
    return(df_tb)
  })
  
  output$info <- renderDT({
    Info_DataTable() %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })
  
  output$sh <- renderPlot({
    # All the inputs
    airline_name <- select_airline()
    master_df <- select_dt()
    
    range <- select_range()
    if(range[[1]] >= range[[2]]){
      return()
    }
    
    graph_range <- range[[1]]:range[[2]]
    
    info <- info_selected()
    
    y_lab <- 'Despesas em $'
    if(info == 'Receita'){
      y_lab <- 'Receita em $'
    }else{
      if(info == 'Passageiros'){
        y_lab <- 'Passageiros'
      }
    }
    
    df <- master_df %>% filter(Year >= range[[1]]) 
    df <- df %>% filter(Year <= range[[2]])
    
    a <- df %>% 
      ggplot(aes_(x=as.name("Year"), y = as.name(airline_name))) +
      geom_path()+
      ylab(as.name(y_lab)) +
      xlab('Ano')+
      scale_x_continuous(breaks = graph_range)
    a
  })
  
}
