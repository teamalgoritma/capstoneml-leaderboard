
shinyServer(function(input, output) {

    # addClass(selector = "body", class = "sidebar-collapse")
    
    shinyURL.server()
    
    USER <- reactiveValues(Logged = FALSE)
    
    observeEvent(input$.login, {
        if (isTRUE(credentials[[input$.username]]==input$.password)) {
            USER$Logged <- TRUE
        } else {
            show("message")
            output$message = renderText("Invalid user name or password")
            delay(2000, hide("message", anim = TRUE, animType = "fade"))
        }
    }
    )
    
    output$app = renderUI(
        if (!isTRUE(USER$Logged)) {
            fluidRow(column(width=4, offset = 4,
                            wellPanel(id = "login",
                                      textInput(".username", "Username:"),
                                      passwordInput(".password", "Password:"),
                                      div(actionButton(".login", "Log in"), style="text-align: center;")
                            ),
                            textOutput("message")
            ))
        } else {
            
            fluidPage(
                includeCSS(path = "css/main.css"),
                includeCSS(path = "css/shinydashboard.css"),
                # Dashboard Page ----
                
                fluidRow(
                    
                    # First box, menu input ----
                    
                    box(
                        title = "Menu Input",
                        width = 3,
                        h6(
                            strong(
                                paste0("Hi, ", str_to_title(input$.username), ".")
                            )
                        ),
                        h6("Please read the following guidance:"),
                        hr(),
                        h6("1. Login with your account"),
                        h6("2. Make sure you validate your model before uploading your work!"),
                        h6("3. Submit your submission file"),
                        h6("4. Each participant can only submit", strong(" 3 times!"), "per day"), 
                        h6("5. If you are satisfied enough with the result, don't forget to send your report .Rmd and the submission file to",
                           strong("classroom")),
                        
                        hr(),
                        
                        # textInput(
                        #   inputId = "name",
                        #   label = "Input your name"
                        # ),
                        
                        selectInput(
                            inputId = "projectype",
                            label = "Choose Your Project: ",
                            choices = c("FNB", "Scotty Time Series", "Scotty Classification", "SMS", "Sentiment"), 
                            selected = "SMS"
                        ),
                        
                        
                        fileInput(inputId = "FileName",
                                  label = "Upload Submission File",
                                  multiple = TRUE, 
                                  accept= ".csv"),
                        
                        hr(),
                        h6("Click the Reset button first, if you want to see the other project's leaderboard"),
                        
                        actionButton(inputId = "reset",
                                     label = "Reset Input")
                    ),
                    
                    # Second box, metrics evaluation output ----
                    
                    box(
                        title = "Model Performance",
                        width = 5,
                        h6("The", "green value box", "means you success to achive the metrics, The red are otherwise."),
                        hr(),
                        uiOutput(outputId = "metric"),
                        uiOutput(outputId = "text")
                    ),
                    
                    # Third box, leaderboard score ----
                    
                    box(
                        title = "Leaderboard Score",
                        width = 4,
                        h6("Find out your current rank, here!"),
                        hr(),
                        h6("1. The score in Classification task using", strong("F1-measure"),  "metric evaluation. It considers both", strong("precision"), "and", strong("recall"), "score."),
                        h6("2. The score in Forecasting task using", strong("RMSE"),  "metric evaluation."),
                        dataTableOutput(
                            outputId = "board")
                        
                    )
                )
            )
        }
    )
    
    
    # Submission reactivity ----
    
    # submission <- reactive({
    #   inFile<-input$FileName
    #   
    #   if (is.null(inFile))
    #     return(NULL)
    #   
    #   data <-  read.csv(inFile$datapath, header = TRUE)
    # })
    
    values <- reactiveValues(
        upload_state = NULL
    )
    
    observeEvent(input$FileName, {
        values$upload_state <- 'uploaded'
    })
    
    observeEvent(input$reset, {
        values$upload_state <- 'reset'
    })
    
    submission <- reactive({
        if (is.null(values$upload_state)) {
            return(NULL)
        } 
        
        else if (values$upload_state == 'uploaded') {
            
            inFile <- input$FileName
            
            data <-  read.csv(inFile$datapath, header = TRUE)
            
            return(data)
        } 
        
        else if (values$upload_state == 'reset') {
            return(NULL)
        }
    })
    
    
    
    # UI output metrics evaluation ----
    
    output$metric <- renderUI({
        
        
        if (input$projectype == "SMS") {
            
            validate(
                need(
                    submission() != "",
                    message = "Waiting your submission..."
                )
            )
            
            validate(
                need(
                    "STATUS" %in% colnames(submission()),
                    message = "Hm, may you choose the wrong project?"
                )
            )
            
            metrics <- list(outputId = c("SMSacc","SMSrecall","SMSprec","SMSspec"), 
                            width = c(6,6,6,6))
            
            temp <- list()
            
            for (i in seq_len(lengths(metrics)[1])) {
                temp[[i]] <- infoBoxOutput(outputId = lapply(metrics[[1]][i], FUN = "print"), 
                                           width = lapply(metrics[[2]][i], FUN = "print"))
            }
            
            tagList(temp)
            
            
            
        }
        
        else if (input$projectype == "Scotty Classification") {
            
            validate(
                need(
                    submission() != "",
                    message = "Waiting your submission..."
                )
            )
            
            
            validate(
                need(
                    "coverage" %in% colnames(submission()),
                    message = "Hm, may you choose the wrong project?"
                )
            )
            
            metrics <- list(outputId = c("ScottClassacc","ScottClassrecall","ScottClassprec","ScottClassspec"), 
                            width = c(6,6,6,6))  
            
            temp <- list()
            
            for (i in seq_len(lengths(metrics)[1])) {
                temp[[i]] <- infoBoxOutput(outputId = lapply(metrics[[1]][i], FUN = "print"), 
                                           width = lapply(metrics[[2]][i], FUN = "print"))
            }
            
            tagList(temp)
            
        }
        
        else if (input$projectype == "Sentiment") {
            
            validate(
                need(
                    submission() != "",
                    message = "Waiting your submission..."
                )
            )
            
            validate(
                need(
                    "label" %in% colnames(submission()),
                    message = "Hm, may you choose the wrong project?"
                )
            )
            
            metrics <- list(outputId = c("Sentiacc","Sentirecall","Sentiprec"), 
                            width = c(4,4,4))  
            
            temp <- list()
            
            for (i in seq_len(lengths(metrics)[1])) {
                temp[[i]] <- infoBoxOutput(outputId = lapply(metrics[[1]][i], FUN = "print"), 
                                           width = lapply(metrics[[2]][i], FUN = "print"))
            }
            
            tagList(temp)
            
        }
        
        else if (input$projectype == "FNB") {
            
            validate(
                need(
                    submission() != "",
                    message = "Waiting your submission..."
                )
            )
            
            
            validate(
                need(
                    "visitor" %in% colnames(submission()),
                    message = "Hm, may you choose the wrong project?"
                )
            )
            
            
            metrics <- list(outputId = c("FNBrmse","FNBmae"), 
                            width = c(6,6))  
            
            temp <- list()
            
            for (i in seq_len(lengths(metrics)[1])) {
                temp[[i]] <- infoBoxOutput(outputId = lapply(metrics[[1]][i], FUN = "print"), 
                                           width = lapply(metrics[[2]][i], FUN = "print"))
            }
            
            tagList(temp)
            
        }
        
        else {NULL}
        
    })
    
    # SMS Spam ----
    
    confMatSMS <- reactive({
        smsmetrics <- confusionMatrix(
            submission()$STATUS %>% as.factor(),
            read_csv("solution/sol_class_sms.csv") %$% STATUS %>% as.factor(),
            positive = "spam"
        )
        
    })
    
    rubricsSMS <- reactive({
        data.frame(
            "metric" = c("accuracy", "recall", "precision","specificity"),
            "threshold" = c(83, 80, 90, 85),
            "prediction" = c(round(confMatSMS()$overall[1],2)*100, 
                             round(confMatSMS()$byClass[1],2)*100,
                             round(confMatSMS()$byClass[3],2)*100,
                             round(confMatSMS()$byClass[2],2)*100)
        )
        
    })
    
    output$SMSacc <- renderInfoBox({
        
        if (rubricsSMS()[1,2] <= rubricsSMS()[1,3]) {
            infoBox(paste(
                round(confMatSMS()$overall[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatSMS()$overall[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "red", fill = TRUE)
        }
        
    })
    
    output$SMSrecall <- renderInfoBox({
        
        if (rubricsSMS()[2,2] <= rubricsSMS()[2,3]) {
            infoBox(paste(
                round(confMatSMS()$byClass[1], 2)*100, "%"
            ), icon = icon("search-plus"), subtitle = "Recall", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatSMS()$byClass[1], 2)*100, "%"
            ), icon = icon("search-plus"), subtitle = "Recall", color = "red", fill = TRUE)
        }
        
    })
    
    output$SMSprec <- renderInfoBox({
        
        if (rubricsSMS()[3,2] <= rubricsSMS()[3,3]) {
            infoBox(paste(
                round(confMatSMS()$byClass[3], 2)*100, "%"
            ), icon = icon("key"), subtitle = "Precision", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatSMS()$byClass[3], 2)*100, "%"
            ), icon = icon("key"), subtitle = "Precision", color = "red", fill = TRUE)
        }
        
    })
    
    output$SMSspec <- renderInfoBox({
        
        if (rubricsSMS()[4,2] <= rubricsSMS()[4,3]) {
            infoBox(paste(
                round(confMatSMS()$byClass[2], 2)*100, "%"
            ), icon = icon("search-minus"), subtitle = "Specificity", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatSMS()$byClass[2], 2)*100, "%"
            ), icon = icon("search-minus"), subtitle = "Specificity", color = "red", fill = TRUE)
        }
        
        
    })
    
    # Scotty Classification ----
    
    confMatScotClass <- reactive({
        scottclas <- confusionMatrix(
            submission()$coverage %>% as.factor(),
            read_csv("solution/sol_class_scotty.csv") %>% 
                mutate(date = timeStamp %>% as.Date()) %>% 
                filter(date >= as.Date("2017-12-04") & date <= as.Date("2017-12-08")) %$%
                coverage %>% as.factor()
        )
        
    })
    
    rubricsScottyClass <- reactive({
        data.frame(
            "metric" = c("accuracy", "recall", "precision","specificity"),
            "threshold" = c(90, 90, 90, 90),
            "prediction" = c(round(confMatScotClass()$overall[1],2)*100, 
                             round(confMatScotClass()$byClass[1],2)*100,
                             round(confMatScotClass()$byClass[3],2)*100,
                             round(confMatScotClass()$byClass[2],2)*100)
        )
    })
    
    output$ScottClassacc <- renderInfoBox({
        
        
        if (rubricsScottyClass()[1,2] <= rubricsScottyClass()[1,3]) {
            infoBox(paste(
                round(confMatScotClass()$overall[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatScotClass()$overall[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "red", fill = TRUE)
        }
        
    })
    
    output$ScottClassrecall <- renderInfoBox({
        
        if (rubricsScottyClass()[2,2] <= rubricsScottyClass()[2,3]) {
            infoBox(paste(
                round(confMatScotClass()$byClass[1], 2)*100, "%"
            ), icon = icon("search-plus"), subtitle = "Recall", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatScotClass()$byClass[1], 2)*100, "%"
            ), icon = icon("search-plus"), subtitle = "Recall", color = "red", fill = TRUE)
        }
        
    })
    
    output$ScottClassprec <- renderInfoBox({
        
        if (rubricsScottyClass()[3,2] <= rubricsScottyClass()[3,3]) {
            infoBox(paste(
                round(confMatScotClass()$byClass[3], 2)*100, "%"
            ), icon = icon("key"), subtitle = "Precision", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatScotClass()$byClass[3], 2)*100, "%"
            ), icon = icon("key"), subtitle = "Precision", color = "red", fill = TRUE)
        }
        
    })
    
    output$ScottClassspec <- renderInfoBox({
        
        if (rubricsScottyClass()[4,2] <= rubricsScottyClass()[4,3]) {
            infoBox(paste(
                round(confMatScotClass()$byClass[2], 2)*100, "%"
            ), icon = icon("search-minus"), subtitle = "Specificity", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatScotClass()$byClass[2], 2)*100, "%"
            ), icon = icon("search-minus"), subtitle = "Specificity", color = "red", fill = TRUE)
        }
        
        
    })
    
    # Youtube Rewind Sentiment ----
    
    
    cofMatSenti <- reactive({
        label <- read.csv("solution/sol_class_youtube.csv")
        metrics_multi <- metric_set(accuracy, recall, precision)
        eval_met <-  data.frame(estimate = submission()$label, truth = label$sentiment_type) %>%
            metrics_multi(truth = truth, estimate = estimate)
    })
    
    rubricsSentiment <- reactive({
        data.frame(
            "metric" = c("accuracy", "recall", "precision"),
            "threshold" = c(70, 65, 65),
            "prediction" = c(round(cofMatSenti()$.estimate[1],2)*100, 
                             round(cofMatSenti()$.estimate[2],2)*100,
                             round(cofMatSenti()$.estimate[3],2)*100)
        )
    })
    
    output$Sentiacc <- renderInfoBox({
        
        
        
        if (rubricsSentiment()[1,2] <= rubricsSentiment()[1,3]) {
            infoBox(paste(
                round(cofMatSenti()$.estimate[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(cofMatSenti()$.estimate[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "red", fill = TRUE)
        }
        
    })
    
    output$Sentirecall <- renderInfoBox({
        
        if (rubricsSentiment()[2,2] <= rubricsSentiment()[2,3]) {
            infoBox(paste(
                round(cofMatSenti()$.estimate[2], 2)*100, "%"
            ), icon = icon("search-plus"), subtitle = "Recall", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(cofMatSenti()$.estimate[2], 2)*100, "%"
            ), icon = icon("search-plus"), subtitle = "Recall", color = "red", fill = TRUE)
        }
        
    })
    
    output$Sentiprec <- renderInfoBox({
        
        if (rubricsSentiment()[3,2] <= rubricsSentiment()[3,3]) {
            infoBox(paste(
                round(cofMatSenti()$.estimate[3], 2)*100, "%"
            ), icon = icon("key"), subtitle = "Precision", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(cofMatSenti()$.estimate[3], 2)*100, "%"
            ), icon = icon("key"), subtitle = "Precision", color = "red", fill = TRUE)
        }
        
    })
    
    # FNB Forcacsting ----
    
    metricsFNB <- reactive({
        
        # import evaluation dataset
        evaluation <- read_csv("solution/sol_forcast_fnb.csv")
        
        
        # check forecast performance
        eval <- evaluation %>%
            left_join(submission() %>% 
                          rename(estimate = visitor) %>% 
                          mutate(datetime = ymd_hms(datetime))) %>%
            select(datetime, outlet, visitor, estimate) %>% 
            mutate(
                error = visitor - estimate
            ) %>% 
            select(error)
        
        rmse <- mean(eval$error^2, na.rm = TRUE)^0.5
        mae <- mean(abs(eval$error), na.rm = TRUE)
        
        metrics <- tibble(rmse, mae)
        
    })
    
    rubricsFNB<- reactive({
        data.frame(
            "metric" = c("rmse", "mae"),
            "threshold" = c(50, 0),
            "prediction" = c(round(metricsFNB()$rmse,2), 
                             round(metricsFNB()$mae,2))
        )
    })
    
    output$FNBrmse <- renderInfoBox({
        
        if (rubricsFNB()[1,2] >= rubricsFNB()[1,3]) {
            infoBox(paste(
                round(metricsFNB()$rmse, 2)
            ), icon = icon("times-circle"), subtitle = "RMSE", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(metricsFNB()$rmse, 2)
            ), icon = icon("calendar-times"), subtitle = "RMSE", color = "red", fill = TRUE)
        }
        
    })
    
    output$FNBmae <- renderInfoBox({
        
        infoBox(paste(
            round(metricsFNB()$mae, 2)
        ), icon = icon("chart-line"), subtitle = "MAE", color = "blue", fill = TRUE)
        
    })
    
    # Text output ----
    
    output$text <- renderUI({
        
        
        
        if (input$projectype == "SMS") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    "STATUS" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h4(textOutput(outputId = "textSMS"))
            
        }
        
        else if (input$projectype == "FNB") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            
            validate(
                need(
                    "visitor" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h4(textOutput(outputId = "textFNB"))
            
        }
        
        else if (input$projectype == "Sentiment") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    "sentiment" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h4(textOutput(outputId = "textSentiment"))
            
        } 
        
        else if (input$projectype == "Scotty Classification") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            
            validate(
                need(
                    "coverage" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h4(textOutput(outputId = "textScottClass"))
            
        }
        
        else (NULL)
        
    })
    
    output$textSMS <- renderText({
        
        temp <- rubricsSMS()$threshold < rubricsSMS()$prediction
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", input$.username, ", you get full (8 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    output$textFNB <- renderText({
        
        temp <- rubricsFNB()$threshold > rubricsFNB()$prediction
        
        
        if (sum(temp) == 1) {
            
            paste("Congratulation", input$.username, ", you get full (8 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    output$textSentiment <- renderText({
        
        temp <- rubricsSentiment()$threshold < rubricsSentiment()$prediction
        
        
        if (sum(temp) == 3) {
            
            paste("Congratulation", input$.username, ", you get full (8 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    output$textScottClass <- renderText({
        
        temp <- rubricsScottyClass()$threshold < rubricsScottyClass()$prediction
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", input$.username, ", you get full (8 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    # Leaderboard ----
    
    
    output$board <- renderDataTable({
        if (input$projectype == "SMS" & is.null(submission())) {
            
            sheet_sms <- gs_read(for_gs, ws = "sms")
            
            sheet_sms %>% 
                mutate(Name = str_to_title(Name)) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`F1-Score`)) %>% 
                datatable(caption = 'Table 1: leaderboard scoring | SMS Spam Classification.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}")), 
                          rownames = T) %>% 
                formatStyle(names(sheet_sms),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "130%")
            
        }
        
        else if (input$projectype == "SMS" & input$.username != "") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    input$.username != "",
                    message = ""
                )
            )
            
            y_pred <-  submission()$STATUS %>% as.factor()
            y_true <-  read_csv("solution/sol_class_sms.csv") %$% STATUS %>% as.factor()
            F1_score <- F1_Score(y_true, y_pred, positive = "spam")
            
            gs_add_row(ss = for_gs, ws = "sms", input = c(input$.username, round(F1_score,2), format(Sys.time(), "%a, %b-%d %X %Y")))
            
            sheet_sms <- gs_read(for_gs, ws = "sms")
            
            sheet_sms %>%
                mutate(Name = str_to_title(Name)) %>%
                arrange(desc(`Last Submitted`)) %>%
                dplyr::filter(!duplicated(Name)) %>%
                dplyr::arrange(desc(`F1-Score`)) %>%
                datatable(caption = 'Table 1: leaderboard scoring | SMS Spam Classification.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}")),
                          rownames = T) %>%
                formatStyle(names(sheet_sms),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "130%")
            
        } 
        
        else if (input$projectype == "Scotty Classification" & is.null(submission())) {
            sheet_scottyclass <- gs_read(for_gs, ws = "scottyclass")
            sheet_scottyclass %>% 
                mutate(Name = str_to_title(Name)) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`F1-Score`)) %>% 
                datatable(caption = 'Table 1: leaderboard scoring | Scotty Classification. ',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}")), 
                          rownames = T) %>% 
                formatStyle(names(sheet_scottyclass),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "130%")
        }
        
        else if (input$projectype == "Scotty Classification" & input$.username != "") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    input$.username != "",
                    message = ""
                )
            )
            
            # withProgress(message = 'Calculation in progress',
            #              detail = 'This may take a while...', value = 0, {
            #                for (i in 1:20) {
            #                  incProgress(1/20)
            #                  Sys.sleep(0.2)
            #                }
            #              })
            
            
            y_pred <-  submission()$coverage %>% as.factor()
            y_true <-  read_csv("solution/sol_class_scotty.csv") %>% 
                mutate(date = timeStamp %>% as.Date()) %>% 
                filter(date >= as.Date("2017-12-04") & date <= as.Date("2017-12-08")) %$%
                coverage %>% as.factor()
            F1_score <- F1_Score(y_true, y_pred)
            
            gs_add_row(ss = for_gs, ws = "scottyclass", input = c(input$.username, round(F1_score,2), format(Sys.time(), "%a, %b-%d %X %Y")))
            
            sheet_scottyclass <- gs_read(for_gs, ws = "scottyclass")
            sheet_scottyclass %>% 
                mutate(Name = str_to_title(Name)) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`F1-Score`)) %>% 
                datatable(caption = 'Table 1: leaderboard scoring | Scotty Classification. ',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}")), 
                          rownames = T) %>% 
                formatStyle(names(sheet_scottyclass),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "130%")
            
        }
        
        else if (input$projectype == "Sentiment" & is.null(submission())) {
            sheet_sentiment <- gs_read(for_gs, ws = "sentiment")
            sheet_sentiment %>% 
                mutate(Name = str_to_title(Name)) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`F1-Score`)) %>% 
                datatable(caption = 'Table 1: leaderboard scoring | Classification Sentiment #YoutubeRewind2018.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}")), 
                          rownames = T) %>% 
                formatStyle(names(sheet_sentiment),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "130%")
        }
        
        else if (input$projectype == "Sentiment" & input$.username != "") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    input$.username != "",
                    message = ""
                )
            )
            
            # withProgress(message = 'Calculation in progress',
            #              detail = 'This may take a while...', value = 0, {
            #                for (i in 1:20) {
            #                  incProgress(1/20)
            #                  Sys.sleep(0.2)
            #                }
            #              })
            
            
            y_pred <-  submission()$sentiment
            label <- read.csv("solution/sol_class_youtube.csv")
            y_true <-  label$sentiment_type
            F1_score <- F1_Score(y_true, y_pred)
            
            gs_add_row(ss = for_gs, ws = "sentiment", input = c(input$.username, round(F1_score,2), format(Sys.time(), "%a, %b-%d %X %Y")))
            sheet_sentiment <- gs_read(for_gs, ws = "sentiment")
            sheet_sentiment %>% 
                mutate(Name = str_to_title(Name)) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`F1-Score`)) %>% 
                datatable(caption = 'Table 1: leaderboard scoring | Classification Sentiment #YoutubeRewind2018.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}")), 
                          rownames = T) %>% 
                formatStyle(names(sheet_sentiment),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "130%")
            
        }
        
        else if (input$projectype == "FNB" & is.null(submission())) {
            sheet_fnb <- gs_read(for_gs, ws = "fnb")
            sheet_fnb %>% 
                mutate(Name = str_to_title(Name)) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(`RMSE Score`) %>% 
                datatable(caption = 'Table 1: Leaderboard scoring | FNB Forecasting.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}")), 
                          rownames = T) %>% 
                formatStyle(names(sheet_fnb),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "130%")
        }
        
        else if (input$projectype == "FNB" & input$.username != "") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    input$.username != "",
                    message = ""
                )
            )
            
            # withProgress(message = 'Calculation in progress',
            #              detail = 'This may take a while...', value = 0, {
            #                for (i in 1:20) {
            #                  incProgress(1/20)
            #                  Sys.sleep(0.2)
            #                }
            #              })
            
            gs_add_row(ss = for_gs, ws = "fnb", input = c(input$.username, round(metricsFNB()$rmse,2), format(Sys.time(), "%a, %b-%d %X %Y")))
            sheet_fnb <- gs_read(for_gs, ws = "fnb")
            sheet_fnb %>% 
                mutate(Name = str_to_title(Name)) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(`RMSE Score`) %>% 
                datatable(caption = 'Table 1: Leaderboard scoring | FNB Forecasting.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}")), 
                          rownames = T) %>% 
                formatStyle(names(sheet_fnb),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "130%")
            
            
        }
        
        else {NULL}
        
    })
    


})
