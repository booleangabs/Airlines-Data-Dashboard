getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


server <- function(input, output) {
  ################### INPUT ####################
  
  # Empresa na aba simples
  select_airline <- eventReactive(input$go, {
    airline_name <- input$airline
    
    return(airline_name)
  })
  
  # Empresas na aba para comparacao
  get_airline1 <- eventReactive(input$go_comp, {
    airline_name1 <- input$airline_c1
    
    return(airline_name1)
  })
  
  get_airline2 <- eventReactive(input$go_comp, {
    airline_name2 <- input$airline_c2
    
    return(airline_name2)
  })
  
  # Intervalo na aba simples
  select_range <- eventReactive(input$go, {
    start <- input$true_date
    end <- input$true_date_2
    
    return(list(start = start, end = end))
  })
  
  # Intervalo na aba para comparacao
  select_range_comp <- eventReactive(input$go_comp, {
    start <- input$true_date_c
    end <- input$true_date_c2
    
    return(list(start = start, end = end))
  })
  
  # Seleciona os dados para a aba simples
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
  
  # Seleciona os dados para a aba de comparacao
  select_dt_c <- eventReactive(input$go_comp, {
    data <- input$data_comp
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
  
  info_selected_c <- eventReactive(input$go_comp, {
    data <- input$data_comp
    return(data)
  })
  
  # Seletor do input de intervalo da aba simples
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
  
  # Seletor do input de intervalo da aba
  output$starttime_comp <- renderUI({
    
    airline_name <- input$airline
    
    df <- read.csv('data/airline_expenses.csv')
    
    years <- df$Year
    names(years) <- years
    
    selectInput("true_date_c", "Inicio da análise", choices= years)
    
  })
  
  output$endtime_comp <- renderUI({
    airline_name <- input$airline
    
    df <- read.csv('data/airline_expenses.csv')
    
    years <- df$Year
    names(years) <- years
    
    selectInput("true_date_c2", "Fim da análise", choices= years)
    
  })
  
  output$timedate__ <- renderUI({
    
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
  
  output$hs <- renderPlot({
    airline_name <- select_airline()
    master_df <- select_dt()
    
    range <- select_range()
    if(range[[1]] > range[[2]]){
      return()
    }
    
    hist_range <- range[[1]]:range[[2]]
    
    df <- master_df %>% filter(Year >= range[[1]]) 
    df <- df %>% filter(Year <= range[[2]])
    

    dif <- as.numeric(range[[2]]) - as.numeric(range[[1]])
    classes <- (sqrt(dif))
    
    #mean <- df %>% select(airline_name())
    #mean <- mean %>% colMeans()
    
    a <- df %>%
        ggplot(aes_(x=as.name(airline_name))) +
        geom_histogram(color = 'white', fill = 'lightblue', bins = classes) +
        theme_classic(base_size = 18) +
        xlab("Valores") +
        ylab("Frequencia")
    a
  })
  
  output$bp <- renderPlot({
      airline_name <- select_airline()
      master_df <- select_dt()
      
      range <- select_range()
      if(range[[1]] > range[[2]]){
        return()
      }
      
      hist_range <- range[[1]]:range[[2]]
      
      df <- master_df %>% filter(Year >= range[[1]]) 
      df <- df %>% filter(Year <= range[[2]])
      
      a <- df %>% 
          ggplot(aes_(x=as.name("Year"), y = as.name(airline_name)))+
          geom_boxplot()
      a
  })
  
  output$comp_sh1 <- renderPlot({
    airline1 <- get_airline1()
    master_df <- select_dt_c()
    
    range <- select_range_comp()
    if(range[[1]] >= range[[2]]){
      return()
    }
    
    graph_range <- range[[1]]:range[[2]]
    
    info <- info_selected_c()
    
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
      ggplot(aes_(x=as.name("Year"), y = as.name(airline1))) +
      geom_path()+
      ylab(as.name(y_lab)) +
      xlab('Ano')+
      scale_x_continuous(breaks = graph_range)
    a
  })
  
  output$comp_sh2 <- renderPlot({
    airline2 <- get_airline2()
    master_df <- select_dt_c()
    
    range <- select_range_comp()
    if(range[[1]] >= range[[2]]){
      return()
    }
    
    graph_range <- range[[1]]:range[[2]]
    
    info <- info_selected_c()
    
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
      ggplot(aes_(x=as.name("Year"), y = as.name(airline2))) +
      geom_path()+
      ylab(as.name(y_lab)) +
      xlab('Ano')+
      scale_x_continuous(breaks = graph_range)
    a
  })
  
  output$comp_sc <- renderPlot({
    airline1 <- get_airline1()
    airline2 <- get_airline2()
    if (airline1==airline2) {
      return()
    }
    master_df <- select_dt_c()
    
    range <- select_range_comp()
    if(range[[1]] >= range[[2]]){
      return()
    }
    
    graph_range <- range[[1]]:range[[2]]
    
    
    df <- master_df %>% filter(Year >= range[[1]]) 
    df <- df %>% filter(Year <= range[[2]])
    
    df %>% ggplot(aes(x=airline1, y=airline2)) + geom_point()
    
  })
  
  output$comp_bm <- renderPlot({
    airline1 <- get_airline1()
    airline2 <- get_airline2()
    if (airline1==airline2) {
      return()
    }
    master_df <- select_dt_c()
    
    range <- select_range_comp()
    if(range[[1]] >= range[[2]]){
      return()
    }
    
    graph_range <- range[[1]]:range[[2]]
  
    df <- master_df %>% filter(Year >= range[[1]]) 
    df <- df %>% filter(Year <= range[[2]])
    
    df1 <- df %>% select(airline1)
    df2 <- df %>% select(airline2)
    
    mean1 <- df1 %>% colMeans()
    mean2 <- df2 %>% colMeans()
    
    data <- data.frame(
      airline=c(airline1, airline2) ,  
      mean=c(mean1, mean2)
    )
    
    ggplot(data, aes(x=airline, y=mean)) + geom_bar(stat = "identity")
    
    
  })
  
  output$corr_cm <- renderDT({
    
    airline1 <- get_airline1()
    airline2 <- get_airline2()
    if (airline1==airline2) {
      return()
    }
    master_df <- select_dt_c()
    
    range <- select_range_comp()
    if(range[[1]] >= range[[2]]){
      return()
    }
    
    graph_range <- range[[1]]:range[[2]]
    
    df <- master_df %>% filter(Year >= range[[1]]) 
    df <- df %>% filter(Year <= range[[2]])
    df <- df %>% select(airline1, airline2)
    
    
    corr_dt <- data.frame(
      Correlacao=cor(df)
    )
    
    corr_dt %>%
      as.data.frame() %>% 
      DT::datatable(options=list(
        language=list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
        )
      ))
  })

}
