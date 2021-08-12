setwd('~/Documentos/shiny/modelo/primeiro_projeto')

source('global.R')
source('ui.R')
source('server.R')


shinyApp(
    ui = ui,
    server = server
)
