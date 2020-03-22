
shinyServer(function(input, output) {
    
    sever(
        bg_color = "black"
    )

    # addClass(selector = "body", class = "sidebar-collapse")
    
    shinyURL.server()
    
    USER <- reactiveValues(Logged = FALSE)

    
    observeEvent({input$.login  
        input$ENTERKeyPressed}, {
        if (isTRUE(nrow(credentials %>% 
                        filter(user == input$.username & pass == input$.password)) != 0)) {
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
            fluidRow(
                
                
                tags$script(
                    '$(document).on("keyup", function(e) {
            if(e.keyCode == 13){Shiny.onInputChange("ENTERKeyPressed", Math.random());}
        });'
                ),
                
                column(width=4, offset = 4,
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
                        width = 2,
                        h6(
                            strong(
                                paste0("Hi, ", str_to_title(input$.username), ".")
                            )
                        ),
                        h6("Please read the following guidance:"),
                        hr(),
                        h6("1. Make sure you validate your model before uploading your work!"),
                        h6("2. Submit your submission file"),
                        h6("3. Each participant can only submit", strong(" 3 times!"), "per day"), 
                        h6("4. If you are satisfied enough with the result, don't forget to send your report .Rmd and the submission file to",
                           strong("classroom")),
                        
                        hr(),
                        
                        # textInput(
                        #   inputId = "name",
                        #   label = "Input your name"
                        # ),
                        
                        selectInput(
                            inputId = "projectype",
                            label = "Choose Your Project: ",
                            choices = c("FNB", 
                                        "Scotty Time Series", 
                                        "Scotty Classification", 
                                        "SMS", 
                                        "Concrete Prediction",
                                        "Concrete Analysis",
                                        "Airline Classification"), 
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
                        width = 5,
                        h6("Find out your current rank, here!"),
                        hr(),
                        h6("1. The score in Classification task using", strong("Accuracy"), strong("Recall"), strong("Precision"), strong("Specificity"), "metric evaluation."),
                        h6("2. The score in Forecasting task using", strong("MAE"),  "metric evaluation."),
                        h6("3. The score in Regression task using", strong("MAE"), "and", strong("R-Squared")),
                        dataTableOutput(
                            outputId = "board")
                        
                    )
                )
            )
        }
    )
    
    
    # Submission reactivity ----
    
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
                    "status" %in% colnames(submission()),
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
        
        else if (input$projectype == "Scotty Time Series"){
            
            validate(
                need(
                    submission() != "",
                    message = "Waiting your submission..."
                )
            )
            
            
            validate(
                need(
                    "demand" %in% colnames(submission()),
                    message = "Hm, may you choose the wrong project?"
                )
            )
            
            
            metrics <- list(outputId = c("Scottymae1","Scottymae2","Scottymae3","Scottymae4"), 
                            width = c(6,6,6,6))  
            
            temp <- list()
            
            for (i in seq_len(lengths(metrics)[1])) {
                temp[[i]] <- infoBoxOutput(outputId = lapply(metrics[[1]][i], FUN = "print"), 
                                           width = lapply(metrics[[2]][i], FUN = "print"))
            }
            
            tagList(temp)   
        }
        
        else if (input$projectype == "Concrete Prediction"){
            
            validate(
                need(
                    submission() != "",
                    message = "Waiting your submission..."
                )
            )
            
            
            validate(
                need(
                    "strength" %in% colnames(submission()),
                    message = "Hm, may you choose the wrong project?"
                )
            )
            
            
            metrics <- list(outputId = c("Concretemae","Concretersq"), 
                            width = c(6,6))  
            
            temp <- list()
            
            for (i in seq_len(lengths(metrics)[1])) {
                temp[[i]] <- infoBoxOutput(outputId = lapply(metrics[[1]][i], FUN = "print"), 
                                           width = lapply(metrics[[2]][i], FUN = "print"))
            }
            
            tagList(temp)   
        }
        
        else if (input$projectype == "Concrete Analysis"){
            
            validate(
                need(
                    submission() != "",
                    message = "Waiting your submission..."
                )
            )
            
            
            validate(
                need(
                    "strength" %in% colnames(submission()),
                    message = "Hm, may you choose the wrong project?"
                )
            )
            
            
            metrics <- list(outputId = c("ConcreteAnalysismae","ConcreteAnalysisrsq"), 
                            width = c(6,6))  
            
            temp <- list()
            
            for (i in seq_len(lengths(metrics)[1])) {
                temp[[i]] <- infoBoxOutput(outputId = lapply(metrics[[1]][i], FUN = "print"), 
                                           width = lapply(metrics[[2]][i], FUN = "print"))
            }
            
            tagList(temp)   
        }
        
        else if (input$projectype == "Airline Classification") {
            
            validate(
                need(
                    submission() != "",
                    message = "Waiting your submission..."
                )
            )
            
            validate(
                need(
                    "arr_status" %in% colnames(submission()),
                    message = "Hm, may you choose the wrong project?"
                )
            )
            
            metrics <- list(outputId = c("Airlineacc","Airlinerecall","Airlineprec", "Airlinespec"), 
                            width = c(6,6,6,6))  
            
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
            submission()$status %>% as.factor(),
            read_csv("solution/sol_class_sms_up.csv") %$% status %>% as.factor(),
            positive = "spam"
        )
        
    })
    
    rubricsSMS <- reactive({
        data.frame(
            "metric" = c("accuracy", "recall", "precision","specificity"),
            "threshold" = c(80, 80, 90, 85),
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
            submission()$coverage %>% factor(levels = c("insufficient", "sufficient")),
            read_csv("solution/sol_class_scotty_new.csv") %$%
                coverage %>% factor(levels = c("insufficient", "sufficient"))
        )
        
    })
    
    rubricsScottyClass <- reactive({
        data.frame(
            "metric" = c("accuracy", "recall", "precision","specificity"),
            "threshold" = c(75, 85, 70, 75),
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
    
    # FNB Forcacsting ----
    
    metricsFNB <- reactive({
        
        # import evaluation dataset
        evaluation <- read_csv("solution/sol_forecast_fnb_new.csv")
        
        
        # check forecast performance
        eval <- evaluation %>%
            rename(truth = visitor) %>% 
            mutate(datetime = ymd_hms(datetime)) %>% 
            left_join(submission() %>% 
                          rename(estimate = visitor) %>% 
                          mutate(datetime = ymd_hms(datetime))) %>%
            summarise(mae = mae_vec(truth = truth, estimate = estimate),
                      rmse = rmse_vec(truth = truth, estimate = estimate))
        
    })
    
    rubricsFNB<- reactive({
        data.frame(
            "metric" = c("rmse", "mae"),
            "threshold" = c(0, 6),
            "prediction" = c(round(metricsFNB()$rmse,2), 
                             round(metricsFNB()$mae,2))
        )
    })
    
    output$FNBrmse <- renderInfoBox({
        
        if (rubricsFNB()[2,2] >= rubricsFNB()[2,3]) {
            infoBox(paste(
                round(metricsFNB()$mae, 2)
            ), icon = icon("times-circle"), subtitle = "MAE", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(metricsFNB()$mae, 2)
            ), icon = icon("calendar-times"), subtitle = "MAE", color = "red", fill = TRUE)
        }
        
    })
    
    output$FNBmae <- renderInfoBox({
        
        infoBox(paste(
            round(metricsFNB()$rmse, 2)
        ), icon = icon("calendar-times"), subtitle = "RMSE", color = "green", fill = TRUE)
        
    })
    
    # Scotty Forecasting ----
    
    metricsScottyts <- reactive({
        
        # import evaluation dataset
        data_evaluation <- read_csv("solution/sol_forecast_scotty_new.csv") %>% 
            mutate(datetime = ymd_hms(datetime))
        
        
        
        # readjust column names
        data_evaluation <- data_evaluation %>%
            rename(truth = demand)
        
        data_submission <- submission() %>%
            rename(estimate = demand) %>% 
            mutate(datetime = ymd_hms(datetime))
        
        # combine all
        data_evaluation <- data_evaluation %>%
            left_join(data_submission)
        
        
        
        # calculate error
        src_area <- data_evaluation %>%
            group_by(src_sub_area) %>%
            summarise(mae = mae_vec(truth = truth, estimate = estimate)) %>%
            ungroup()
        
        all_area <- data_evaluation %>%
            summarise(mae = mae_vec(truth = truth, estimate = estimate)) %>% 
            mutate(src_sub_area = "all") %>% 
            select(src_sub_area, mae)
        
        src_area %>% bind_rows(all_area)
        
    })
    
    rubricsScottyts <- reactive({
        
        metricsScottyts() %>% 
            bind_cols("threshold" = c(12,11,10,11))
        
    })
    
    output$Scottymae1 <- renderInfoBox({
        
        if (rubricsScottyts()[1,3] >= rubricsScottyts()[1,2]) {
            infoBox(paste(
                round(rubricsScottyts()[1,2], 2)
            ), icon = icon("times-circle"), subtitle = "MAE sxk97", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(rubricsScottyts()[1,2], 2)
            ), icon = icon("times-circle"), subtitle = "MAE sxk97", color = "red", fill = TRUE)
        }
        
    })
    
    output$Scottymae2 <- renderInfoBox({
        
        if (rubricsScottyts()[2,3] >= rubricsScottyts()[2,2]) {
            infoBox(paste(
                round(rubricsScottyts()[2,2], 2)
            ), icon = icon("times-circle"), subtitle = "MAE sxk9e", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(rubricsScottyts()[2,2], 2)
            ), icon = icon("times-circle"), subtitle = "MAE sxk9e", color = "red", fill = TRUE)
        }
        
    })
    
    output$Scottymae3 <- renderInfoBox({
        
        if (rubricsScottyts()[3,3] >= rubricsScottyts()[3,2]) {
            infoBox(paste(
                round(rubricsScottyts()[3,2], 2)
            ), icon = icon("times-circle"), subtitle = "MAE sxk9s", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(rubricsScottyts()[3,2], 2)
            ), icon = icon("times-circle"), subtitle = "MAE sxk9s", color = "red", fill = TRUE)
        }
        
    })
    
    
    output$Scottymae4 <- renderInfoBox({
        
        if (rubricsScottyts()[4,3] >= rubricsScottyts()[4,2]) {
            infoBox(paste(
                round(rubricsScottyts()[4,2], 2)
            ), icon = icon("times-circle"), subtitle = "MAE all area", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(rubricsScottyts()[4,2], 2)
            ), icon = icon("times-circle"), subtitle = "MAE all area", color = "red", fill = TRUE)
        }
        
    })
    
    # Concrete Prediction ----
    
    metricsConcretePred <- reactive({
        
        # import evaluation dataset
        evaluation <- read_csv("solution/sol_rm_pred_concrete.csv")
        
        
        # check forecast performance
        eval <- evaluation %>%
            rename(truth = strength) %>% 
            bind_cols(submission() %>% 
                          rename(estimate = strength) %>%
                          select(estimate)) %>%
            summarise(
                mae = mae_vec(truth = truth, estimate = estimate),
                rsq = rsq_vec(truth = truth, estimate = estimate)
            )
        
    })
    
    rubricsConcrete <- reactive({
        data.frame(
            "metric" = c("mae", "rsq"),
            "threshold" = c(4, 90),
            "prediction" = c(round(metricsConcretePred()$mae,2), 
                             round(metricsConcretePred()$rsq,2)*100)
        )
    })
    
    output$Concretemae <- renderInfoBox({
        
        if (rubricsConcrete()[1,2] >= rubricsConcrete()[1,3]) {
            infoBox(paste(
                round(metricsConcretePred()$mae, 2)
            ), icon = icon("times-circle"), subtitle = "MAE", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(metricsConcretePred()$mae, 2)
            ), icon = icon("times-circle"), subtitle = "MAE", color = "red", fill = TRUE)
        }
        
    })
    
    output$Concretersq <- renderInfoBox({
        
        if (rubricsConcrete()[2,2] < rubricsConcrete()[2,3]) {
            
            infoBox(paste(
                round(metricsConcretePred()$rsq,2)*100, "%"
            ), icon = icon("chart-line"), subtitle = "R-Squared", color = "green", fill = TRUE)
            
        }
        
        else {
            
            infoBox(paste(
                round(metricsConcretePred()$rsq,2)*100, "%"
            ), icon = icon("chart-line"), subtitle = "R-Squared", color = "red", fill = TRUE)
            
        }
        

        
    })
    
    # Concrete Analysis ----
    
    metricsConcreteAnalysis <- reactive({
        
        # import evaluation dataset
        evaluation <- read_csv("solution/sol_rm_analysis_concrete.csv")
        
        
        # check forecast performance
        eval <- evaluation %>%
            rename(truth = strength) %>% 
            bind_cols(submission() %>% 
                          mutate(id = str_remove_all(id, "S") %>% 
                                     as.numeric()) %>% 
                          arrange(id) %>% 
                          rename(estimate = strength) %>%
                          select(estimate)) %>%
            summarise(
                mae = mae_vec(truth = truth, estimate = estimate),
                rsq = rsq_vec(truth = truth, estimate = estimate)
            )
        
    })
    
    rubricsConcreteAnalysis <- reactive({
        data.frame(
            "metric" = c("mae", "rsq"),
            "threshold" = c(7.5, 65),
            "prediction" = c(round(metricsConcreteAnalysis()$mae,2), 
                             round(metricsConcreteAnalysis()$rsq,2)*100)
        )
    })
    
    output$ConcreteAnalysismae <- renderInfoBox({
        
        if (rubricsConcreteAnalysis()[1,2] >= rubricsConcreteAnalysis()[1,3]) {
            infoBox(paste(
                round(metricsConcreteAnalysis()$mae, 2)
            ), icon = icon("times-circle"), subtitle = "MAE", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(metricsConcreteAnalysis()$mae, 2)
            ), icon = icon("times-circle"), subtitle = "MAE", color = "red", fill = TRUE)
        }
        
    })
    
    output$ConcreteAnalysisrsq <- renderInfoBox({
        
        if (rubricsConcreteAnalysis()[2,2] < rubricsConcreteAnalysis()[2,3]) {
            
            infoBox(paste(
                round(metricsConcreteAnalysis()$rsq,2)*100, "%"
            ), icon = icon("chart-line"), subtitle = "R-Squared", color = "green", fill = TRUE)
            
        }
        
        else {
            
            infoBox(paste(
                round(metricsConcreteAnalysis()$rsq,2)*100, "%"
            ), icon = icon("chart-line"), subtitle = "R-Squared", color = "red", fill = TRUE)
            
        }
        
        
        
    })
    
    # Airline Classification ----
    
    
    confMatAirline <- reactive({


        
        airlinemetrics <- confusionMatrix(
            submission() %>% 
                mutate(id = str_remove_all(string = id, pattern = "F") %>% 
                           as.numeric()) %>% 
                arrange(id) %>% 
                pull(arr_status) %>% 
                as.factor(),
            read_csv("solution/sol_class_airline.csv") %$% arr_status %>% as.factor(),
            positive = "Delay"
        )
        
    })
    
    rubricsAirline <- reactive({
        data.frame(
            "metric" = c("accuracy", "recall", "precision","specificity"),
            "threshold" = c(75, 73, 75, 70),
            "prediction" = c(round(confMatAirline()$overall[1],2)*100, 
                             round(confMatAirline()$byClass[1],2)*100,
                             round(confMatAirline()$byClass[3],2)*100,
                             round(confMatAirline()$byClass[2],2)*100)
        )
        
    })
    
    output$Airlineacc <- renderInfoBox({
        
        if (rubricsAirline()[1,2] <= rubricsAirline()[1,3]) {
            infoBox(paste(
                round(confMatAirline()$overall[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatAirline()$overall[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "red", fill = TRUE)
        }
        
    })
    
    output$Airlinerecall <- renderInfoBox({
        
        if (rubricsAirline()[2,2] <= rubricsAirline()[2,3]) {
            infoBox(paste(
                round(confMatAirline()$byClass[1], 2)*100, "%"
            ), icon = icon("search-plus"), subtitle = "Recall", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatAirline()$byClass[1], 2)*100, "%"
            ), icon = icon("search-plus"), subtitle = "Recall", color = "red", fill = TRUE)
        }
        
    })
    
    output$Airlineprec <- renderInfoBox({
        
        if (rubricsAirline()[3,2] <= rubricsAirline()[3,3]) {
            infoBox(paste(
                round(confMatAirline()$byClass[3], 2)*100, "%"
            ), icon = icon("key"), subtitle = "Precision", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatAirline()$byClass[3], 2)*100, "%"
            ), icon = icon("key"), subtitle = "Precision", color = "red", fill = TRUE)
        }
        
    })
    
    output$Airlinespec <- renderInfoBox({
        
        if (rubricsAirline()[4,2] <= rubricsAirline()[4,3]) {
            infoBox(paste(
                round(confMatAirline()$byClass[2], 2)*100, "%"
            ), icon = icon("search-minus"), subtitle = "Specificity", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatAirline()$byClass[2], 2)*100, "%"
            ), icon = icon("search-minus"), subtitle = "Specificity", color = "red", fill = TRUE)
        }
        
        
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
                    "status" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h6(textOutput(outputId = "textSMS"))
            
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
            
            h6(textOutput(outputId = "textFNB"))
            
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
            
            h6(textOutput(outputId = "textScottClass"))
            
        }
        
        else if (input$projectype == "Scotty Time Series") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            
            validate(
                need(
                    "demand" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h6(textOutput(outputId = "textScottyts"))
            
        }
        
        else if (input$projectype == "Concrete Prediction") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            
            validate(
                need(
                    "strength" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h6(textOutput(outputId = "textConcretepred"))
            
        }
        
        else if (input$projectype == "Concrete Analysis") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            
            validate(
                need(
                    "strength" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h6(textOutput(outputId = "textConcreteAnalysis"))
            
        }
        
        else if (input$projectype == "Airline Classification") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            
            validate(
                need(
                    "arr_status" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h6(textOutput(outputId = "textairline"))
            
        }
        
        else (NULL)
        
    })
    
    ## Text Output SMS Classification ----
    
    output$textSMS <- renderText({
        
        temp <- rubricsSMS()$threshold < rubricsSMS()$prediction
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", input$.username, ", you get full (4 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    ## Text Output FNB Forecasting ----
    
    output$textFNB <- renderText({
        
        temp <- rubricsFNB()$threshold > rubricsFNB()$prediction
        
        
        if (sum(temp) == 1) {
            
            paste("Congratulation", input$.username, ", you get full (4 points) on Evaluation Dataset!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    ## Text Output Scotty Classification ----
    
    output$textScottClass <- renderText({
        
        temp <- rubricsScottyClass()$threshold <= rubricsScottyClass()$prediction
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", input$.username, ", you get full (4 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    ## Text Output Concrete Prediction ----
    
    output$textConcretepred <- renderText({
        
        
        if (rubricsConcrete()$threshold[1] > rubricsConcrete()$prediction[1] &&
            rubricsConcrete()$threshold[2] < rubricsConcrete()$prediction[2]) {
            
            paste("Congratulation", input$.username, ", you get full (4 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    ## Text Output Scotty Forecasting ----
    
    output$textScottyts <- renderText({

        temp <- rubricsScottyts()$threshold > rubricsScottyts()$mae
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", input$.username, ", you get full (4 points) on Evaluation Dataset!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
        
    })
    
    ## Text Output Concrete Analysis ----
    
    output$textConcreteAnalysis <- renderText({
        
        
        if (rubricsConcreteAnalysis()$threshold[1] > rubricsConcreteAnalysis()$prediction[1] &&
            rubricsConcreteAnalysis()$threshold[2] < rubricsConcreteAnalysis()$prediction[2]) {
            
            paste("Congratulation", input$.username, ", you get full (2) points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    
    ## Text Output Airline Classification ----
    
    output$textairline <- renderText({
        
        temp <- rubricsAirline()$threshold < rubricsAirline()$prediction
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", input$.username, ", you get full (4 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    
    # Leaderboard -----
    # Leaderboard SMS Classification ----
    
    output$board <- renderDataTable({
        if ((input$projectype == "SMS" & is.null(submission())) | input$.username == "teamalgoritma") {
            
            sheet_sms <- gs_read(for_gs, ws = "sms")
            
            sheet_sms %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`Accuracy`)) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>% 
                datatable(caption = 'Table 1: leaderboard scoring | SMS Spam Classification.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_sms),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
        }
        
        else if (input$projectype == "SMS" & input$.username != "teamalgoritma" ) {
            
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
            
            gs_add_row(
                ss = for_gs, 
                ws = "sms", 
                input = c(input$.username, 
                          round(confMatSMS()$overall[1],2),
                          round(confMatSMS()$byClass[1],2),
                          round(confMatSMS()$byClass[3],2), 
                          round(confMatSMS()$byClass[2],2),
                          format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y"))
                )
            
            sheet_sms <- gs_read(for_gs, ws = "sms")
            
            sheet_sms %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`Accuracy`)) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: leaderboard scoring | SMS Spam Classification.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE),
                          rownames = T) %>%
                formatStyle(names(sheet_sms),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
        } 
        
        ## Leaderboard Scotty Classification ----
        
        else if ((input$projectype == "Scotty Classification" & is.null(submission())) | input$.username == "teamalgoritma") {
            sheet_scottyclass <- gs_read(for_gs, ws = "scottyclass")
            sheet_scottyclass %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`Recall`)) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: leaderboard scoring | Scotty Classification. ',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_scottyclass),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
        }
        
        else if (input$projectype == "Scotty Classification" & input$.username != "teamalgoritma") {
            
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
        
            
            gs_add_row(ss = for_gs, 
                       ws = "scottyclass", 
                       input = c(input$.username, 
                                 round(confMatScotClass()$overall[1],2),
                                 round(confMatScotClass()$byClass[1],2),
                                 round(confMatScotClass()$byClass[3],2),
                                 round(confMatScotClass()$byClass[2],2),
                                 format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y")))
            
            sheet_scottyclass <- gs_read(for_gs, ws = "scottyclass")
            sheet_scottyclass %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`Recall`)) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: leaderboard scoring | Scotty Classification. ',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_scottyclass),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
        }
        
        ## Leaderboard FNB Forecasting ----
        
        else if ((input$projectype == "FNB" & is.null(submission())) | input$.username == "teamalgoritma") {
            sheet_fnb <- gs_read(for_gs, ws = "fnb")
            sheet_fnb %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(`MAE Score`) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: Leaderboard scoring | FNB Forecasting.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_fnb),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
        }
        
        else if (input$projectype == "FNB" & input$.username != "teamalgoritma") {
            
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
            
            gs_add_row(ss = for_gs,
                       ws = "fnb",
                       input = c(input$.username,
                                 round(metricsFNB()$mae,2),
                                 format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y")))
            sheet_fnb <- gs_read(for_gs, ws = "fnb")
            sheet_fnb %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(`MAE Score`) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: Leaderboard scoring | FNB Forecasting.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_fnb),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
            
        }
        
        ## Leaderboard Scotty Forecasting ----
        
        else if ((input$projectype == "Scotty Time Series" & is.null(submission())) | input$.username == "teamalgoritma") {
            sheet_scottyts <- gs_read(for_gs, ws = "scottyts")
            sheet_scottyts %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(`all area`) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: Leaderboard scoring | Scotty Forecasting.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_scottyts),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
        }
        
        else if (input$projectype == "Scotty Time Series" & input$.username != "teamalgoritma") {
            
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
            
            gs_add_row(ss = for_gs,
                       ws = "scottyts",
                       input = c(input$.username,
                                 round(rubricsScottyts()$mae[1],2), 
                                 round(rubricsScottyts()$mae[2],2), 
                                 round(rubricsScottyts()$mae[3],2), 
                                 round(rubricsScottyts()$mae[4],2), 
                                 format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta") , "%a, %b-%d %X %Y")))
            
            sheet_scottyts <- gs_read(for_gs, ws = "scottyts")
            sheet_scottyts %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(`all area`) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: Leaderboard scoring | Scotty Forecasting.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_scottyts),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
            
        }
        
        ## Leaderboard Concrete Prediction ----
        
        else if ((input$projectype == "Concrete Prediction" & is.null(submission())) | input$.username == "teamalgoritma") {
            sheet_concreterm <- gs_read(for_gs, ws = "concreterm")
            sheet_concreterm %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(`MAE Score`) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: Leaderboard scoring | Concrete Prediction.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_concreterm),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
        }
        
        
        
        else if (input$projectype == "Concrete Prediction" & input$.username != "teamalgoritma") {
            
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
            
            gs_add_row(ss = for_gs, 
                       ws = "concreterm",
                       input = c(input$.username, 
                                 round(metricsConcretePred()$mae,2),
                                 round(metricsConcretePred()$rsq,2)*100, 
                                 format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y")))
            sheet_concreterm <- gs_read(for_gs, ws = "concreterm")
            sheet_concreterm %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(`MAE Score`) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: Leaderboard scoring | Concrete Prediction.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_concreterm),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
            
        }
        
        ## Leaderboard Concrete Analysis ----
        
        else if ((input$projectype == "Concrete Analysis" & is.null(submission())) | input$.username == "teamalgoritma") {
            sheet_concreteanalysis <- gs_read(for_gs, ws = "concreteanalysis")
            sheet_concreteanalysis %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(`MAE Score`) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: Leaderboard scoring | Concrete Prediction.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_concreteanalysis),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
        }
        

        
        else if (input$projectype == "Concrete Analysis" & input$.username != "teamalgoritma") {
            
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
            
            gs_add_row(ss = for_gs, 
                       ws = "concreteanalysis",
                       input = c(input$.username, 
                                 round(metricsConcretePred()$mae,2),
                                 round(metricsConcretePred()$rsq,2)*100, 
                                 format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y")))
            
            sheet_concreteanalysis <- gs_read(for_gs, ws = "concreteanalysis")
            sheet_concreteanalysis %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(`MAE Score`) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: Leaderboard scoring | Concrete Prediction.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_concreteanalysis),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
            
        }
        
        
        
        
        ## Leaderboard Airline Classification ----
        
        else if ((input$projectype == "Airline Classification" & is.null(submission())) | input$.username == "teamalgoritma") {
            
            sheet_airline <- gs_read(for_gs, ws = "airlineclass")
            
            sheet_airline %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`Accuracy`)) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>% 
                datatable(caption = 'Table 1: leaderboard scoring | Airline Classification Delay.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_airline),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
        }
        
        else if (input$projectype == "Airline Classification" & input$.username != "teamalgoritma" ) {
            
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
            
            gs_add_row(
                ss = for_gs, 
                ws = "airlineclass", 
                input = c(input$.username, 
                          round(confMatAirline()$overall[1],2),
                          round(confMatAirline()$byClass[1],2),
                          round(confMatAirline()$byClass[3],2), 
                          round(confMatAirline()$byClass[2],2),
                          format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y"))
            )
            
            sheet_airline <- gs_read(for_gs, ws = "airlineclass")
            
            sheet_airline %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`Accuracy`)) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: leaderboard scoring | Airline13 Classification Delay.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE),
                          rownames = T) %>%
                formatStyle(names(sheet_airline),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
        } 
        
        else {NULL}
        
    })
    


})
