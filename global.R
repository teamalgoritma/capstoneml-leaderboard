library(shiny)
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

# gs_auth(token = "token/googlesheets_token.RDS")
list <- gs_ls()
for_gs <- gs_key(x = "1VcxbaYM-QT1umwSlCIDNpbiWjr48AXzNRD8uWt9KElg")

credentials <- list("test" = "123",
                    "husain" = "husain",
                    "winata" = "winata",
                    "hafiz" = "hafiz",
                    "arga" = "arga",
                    "dian" = "dian",
                    "daniel" = "daniel",
                    "rina" = "rina",
                    "edi cantona" = "edi")