library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)

#master_df <- read.csv('airline_expenses.csv')
airline_list <- c('American', 'Continental', 'Delta')
data_options <- c('Despesas', 'Receita', 'Passageiros')

#master_df$X <- NULL

#master_df <- master_df %>% drop_na()
