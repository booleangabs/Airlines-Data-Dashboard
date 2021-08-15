

header <- dashboardHeader(title = "Projeto de EstatÃ­stica")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
    menuItem('Comparando Empresas Aereas', tabName = 'comp', icon = icon('chart-bar'))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'm',
            fluidRow(
              box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                  selectInput('airline', 'Empresa', airline_list, multiple=FALSE),
                  selectInput('data', 'Tipo de informação', data_options, multiple=FALSE),
                  box(uiOutput("starttime"), solidHeader = TRUE),
                  box(uiOutput("endtime"), solidHeader = TRUE),
                  actionButton('go', 'Submeter')
              )
            ),
            fluidRow(
              box(title = "Informações sobre a empresa", width = 12, solidHeader = TRUE,
                  DTOutput('info')
              )
            ),
            fluidRow(
              box(title = "Série de Despesas", width = 12, solidHeader = TRUE,
                  plotOutput('sh')
              )
            ),
            fluidRow(
              box(title = 'Histograma', width = 12, solidHeader = TRUE,
                  plotOutput('hs'))
            ),
            fluidRow(
              box(title = 'Boxplot', width = 12, solidHeader = TRUE,
                  plotOutput('bp'))
            )
    ),
    tabItem(tabName = 'comp',
            fluidRow(
              box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                  selectInput('airline_comp', 'Empresa', airline_list, multiple=TRUE),
                  uiOutput("timedate_comp"),
                  actionButton('go_comp', 'Submeter')
              )
            ),            
    )
  )
)

ui <- dashboardPage(
  skin = 'blue',
  header, sidebar, body)
