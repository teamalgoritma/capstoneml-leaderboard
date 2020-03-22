library(shiny)
library(e1071)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(caret)
library(DT)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(shinythemes)
library(googlesheets)
library(MLmetrics)
library(shinyjs)
library(shinyURL)
library(sever)

gs_auth(token = "token/googlesheets_token.RDS")
list <- gs_ls()
for_gs <- gs_key(x = "1VcxbaYM-QT1umwSlCIDNpbiWjr48AXzNRD8uWt9KElg")

credentials <- gs_read(for_gs, ws = 'user')


