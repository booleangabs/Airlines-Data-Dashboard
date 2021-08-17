

header <- dashboardHeader(title = "Projeto 2 - ET586")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
    menuItem('Comparando Empresas Aéreas', tabName = 'comp', icon = icon('chart-bar')),
    menuItem('Desenvolvedores', tabName = 'inform', icon=icon('cog'))
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
              box(title = "Série", width = 12, solidHeader = TRUE,
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
                  selectInput('airline_c1', 'Empresa 1', airline_list, multiple=FALSE),
                  selectInput('airline_c2', 'Empresa 2', airline_list, multiple=FALSE),
                  selectInput('data_comp', 'Tipo de informação', data_options, multiple=FALSE),
                  box(uiOutput("starttime_comp"), solidHeader = TRUE),
                  box(uiOutput("endtime_comp"), solidHeader = TRUE),
                  uiOutput("timedate_comp"),
                  actionButton('go_comp', 'Submeter')
              )
            ), 
            fluidRow(
              box(title = 'Série da Empresa 1', width = 12, solidHeader = TRUE,
                  plotOutput('comp_sh1')),
              box(title = 'Série da Empresa 2', width = 12, solidHeader = TRUE,
                  plotOutput('comp_sh2'))
            ),
            fluidRow(
              box(title = 'Gráfico de Dispersão', width = 12, solidHeader = TRUE,
                  plotOutput('comp_sc'))
            ),
            fluidRow(
              box(title= 'Barras das Médias', width = 12, solidHeader = TRUE,
                  plotOutput('comp_bm'))
            ),
            fluidRow(
              box(title = "Correlação", width = 12, solidHeader = TRUE,
                  DTOutput('corr_cm')
              )
            ),
    ),
    tabItem(tabName = 'inform',
            fluidRow(
              box(title='Desenvolvedores', width = 12, solidHeader = TRUE,
                  tags$p('>> Antônio Bento de Souza <absn3>'),
                  tags$p('>> José Gabriel Pereira Tavares <jgpt>'),
                  tags$br(),
                  tags$p('Graduação em Ciência da Computação 2020.2 (CIn-UFPE)')
              )
            ),
    )
  )
)

ui <- dashboardPage(
  skin = 'blue',
  header, sidebar, body)
