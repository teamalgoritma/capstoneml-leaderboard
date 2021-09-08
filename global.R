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
#library(googlesheets)
library(googlesheets4)
library(MLmetrics)
library(shinyjs)
# library(shinyURL)
library(sever)
library(shinymanager)
library(shinybusy)

gs4_auth(path = "xxx",
         email = "email@algorit.ma",
         cache = ".secrets")

sss <- "1VcxbaYM-QT1umwSlCIDNpbiWjr48AXzNRD8uWt9KElg"
credentials <- googlesheets4::read_sheet(ss = sss,sheet = "user")

set_labels(language = "en",
           "Please authenticate" = "Algoritma Capstone ML")

source("ui.R")
source("server.R")

shinyApp(ui,server)