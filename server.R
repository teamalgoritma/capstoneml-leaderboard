server <- function(input,output,session){
    
    res_auth <- callModule(
        module = auth_server,
        id = "auth",
        check_credentials = check_credentials(credentials)
    )
    
    sever(bg_color = "black")
    
    output$app <- renderUI({
        
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
                            paste0("Hi, ", str_to_title(res_auth$user), ".")
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
                   
                    selectInput(
                        inputId = "projectype",
                        label = "Choose Your Project: ",
                        choices = c("FNB", 
                                    "Scotty Time Series", 
                                    "Scotty Classification", 
                                    "SMS", 
                                    "Concrete Prediction",
                                    "Concrete Analysis",
                                    "Airline Classification", 
                                    "Cyberbully Text Classification",
                                    "Image Classification"
                        ), 
                        selected = "FNB"
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
                    uiOutput(outputId = "text"),
                    dataTableOutput(outputId = "confadd")
                    
                    # if (input$projectype == "Image Classification"){
                    #     dataTableOutput(outputId = "addconf")
                    # }
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
        
        
    })
    
    
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
        
        else if (input$projectype == "Cyberbully Text Classification") {
            
            validate(
                need(
                    submission() != "",
                    message = "Waiting your submission..."
                )
            )
            
            
            validate(
                need(
                    "bully" %in% colnames(submission()),
                    message = "Hm, may you choose the wrong project?"
                )
            )
            
            metrics <- list(outputId = c("CyberClassacc","CyberClassrecall","CyberClassprec","CyberClassspec"), 
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
        
        else if (input$projectype == "Image Classification") {
            
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
            
            
            metrics <- list(outputId = c("IMGacc","IMGrecall","IMGprec","IMGspec"),
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
        
        # gs_read(for_gs, ws = "rubrics-sms") %>% 
        data.frame(metrics = c("accuracy","recall","precision","specificity"),
                   threshold = c(80,80,90,85),
                   point = c(2,2,2,2)) %>% 
            bind_cols(
                data.frame(
                    "prediction" = c(round(confMatSMS()$overall[1],2)*100, 
                                     round(confMatSMS()$byClass[1],2)*100,
                                     round(confMatSMS()$byClass[3],2)*100,
                                     round(confMatSMS()$byClass[2],2)*100)
                ) 
            )
        
        
        
    })
    
    output$SMSacc <- renderInfoBox({
        
        if (rubricsSMS()$threshold[1] < rubricsSMS()$prediction[1]) {
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
        
        if (rubricsSMS()$threshold[2] < rubricsSMS()$prediction[2]) {
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
        
        if (rubricsSMS()$threshold[3] < rubricsSMS()$prediction[3]) {
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
        
        if (rubricsSMS()$threshold[4] < rubricsSMS()$prediction[4]) {
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
    
    # Cyberbully text Spam ----
    
    confMatCyber <- reactive({
        smsmetrics <- confusionMatrix(
            submission()$bully %>% as.factor(),
            read_csv("solution/sol_class_cyberbully.csv") %$% bully %>% as.factor(),
            positive = "yes"
        )
        
    })
    
    rubricsCyber <- reactive({
        
        data.frame(metrics = c("accuracy","recall","precision","specificity"),
                   threshold = c(80,80,75,75),
                   point = c(2,2,2,2)) %>% 
            bind_cols(
                data.frame(
                    "prediction" = c(round(confMatCyber()$overall[1],2)*100, 
                                     round(confMatCyber()$byClass[1],2)*100,
                                     round(confMatCyber()$byClass[3],2)*100,
                                     round(confMatCyber()$byClass[2],2)*100)
                ) 
            )
        
        
        
    })
    
    output$CyberClassacc <- renderInfoBox({
        
        if (rubricsCyber()$threshold[1] < rubricsCyber()$prediction[1]) {
            infoBox(paste(
                round(confMatCyber()$overall[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatCyber()$overall[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "red", fill = TRUE)
        }
        
    })
    
    output$CyberClassrecall <- renderInfoBox({
        
        if (rubricsCyber()$threshold[2] < rubricsCyber()$prediction[2]) {
            infoBox(paste(
                round(confMatCyber()$byClass[1], 2)*100, "%"
            ), icon = icon("search-plus"), subtitle = "Recall", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatCyber()$byClass[1], 2)*100, "%"
            ), icon = icon("search-plus"), subtitle = "Recall", color = "red", fill = TRUE)
        }
        
    })
    
    output$CyberClassprec <- renderInfoBox({
        
        if (rubricsCyber()$threshold[3] < rubricsCyber()$prediction[3]) {
            infoBox(paste(
                round(confMatCyber()$byClass[3], 2)*100, "%"
            ), icon = icon("key"), subtitle = "Precision", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatCyber()$byClass[3], 2)*100, "%"
            ), icon = icon("key"), subtitle = "Precision", color = "red", fill = TRUE)
        }
        
    })
    
    output$CyberClassspec <- renderInfoBox({
        
        if (rubricsCyber()$threshold[4] < rubricsCyber()$prediction[4]) {
            infoBox(paste(
                round(confMatCyber()$byClass[2], 2)*100, "%"
            ), icon = icon("search-minus"), subtitle = "Specificity", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatCyber()$byClass[2], 2)*100, "%"
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
        
        # gs_read(for_gs, ws = "rubrics-scottyclass") %>% 
        data.frame(
            metrics = c("accuracy","recall","precision","specificity"),
            threshold = c(75,85,75,70),
            point = rep(2,4)
        ) %>% 
            bind_cols(
                data.frame(
                    "prediction" = c(round(confMatScotClass()$overall[1],2)*100, 
                                     round(confMatScotClass()$byClass[1],2)*100,
                                     round(confMatScotClass()$byClass[3],2)*100,
                                     round(confMatScotClass()$byClass[2],2)*100)
                )
            )
        
    })
    
    output$ScottClassacc <- renderInfoBox({
        
        
        if (rubricsScottyClass()$threshold[1] < rubricsScottyClass()$prediction[1]) {
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
        
        if (rubricsScottyClass()$threshold[2] < rubricsScottyClass()$prediction[2]) {
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
        
        if (rubricsScottyClass()$threshold[3] < rubricsScottyClass()$prediction[3]) {
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
        
        if (rubricsScottyClass()$threshold[4] < rubricsScottyClass()$prediction[4]) {
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
        
        # gs_read(for_gs, ws = "rubrics-fnb") %>% 
        data.frame(
            metrics = c("rmse","mae"),
            threshold = c(0,6),
            point = c(0,6)
        ) %>% 
            bind_cols(
                data.frame(
                    "prediction" = c(round(metricsFNB()$rmse,2), 
                                     round(metricsFNB()$mae,2))
                ) 
            )
        
    })
    
    output$FNBmae <- renderInfoBox({
        
        if (rubricsFNB()$prediction[2] < rubricsFNB()$threshold[2]) {
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
    
    output$FNBrmse <- renderInfoBox({
        
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
            bind_cols(
                data.frame(
                    # gs_read(for_gs, ws = "rubrics-sms") %>% 
                    #     select(threshold)
                    # threshold = c(80,80,90,85)
                    threshold = c(12,11,10,11),
                    point = rep(2,4)
                )
            )
        
        
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
        
        # gs_read(for_gs, ws = "rubrics-concretepred") %>% 
        data.frame(
            metrics = c("mae","rsq"),
            threshold = c(4,90),
            point = c(3,3)
        ) %>% 
            bind_cols(
                data.frame(
                    "prediction" = c(round(metricsConcretePred()$mae,2), 
                                     round(metricsConcretePred()$rsq,2)*100)
                )
            )
        
        
    })
    
    output$Concretemae <- renderInfoBox({
        
        if (rubricsConcrete()$threshold[1] >= rubricsConcrete()$prediction[1]) {
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
        
        if (rubricsConcrete()$threshold[2] < rubricsConcrete()$prediction[2]) {
            
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
        
        # gs_read(for_gs, ws = "rubrics-concreteanalysis") %>% 
        data.frame(
            metrics = c("mae","rsq"),
            threshold = c(7.5,65),
            point = c(2,2)
        ) %>% 
            bind_cols(
                data.frame(
                    "prediction" = c(round(metricsConcreteAnalysis()$mae,2), 
                                     round(metricsConcreteAnalysis()$rsq,2)*100)
                )
            )
        
        
    })
    
    output$ConcreteAnalysismae <- renderInfoBox({
        
        if (rubricsConcreteAnalysis()$threshold[1] >= rubricsConcreteAnalysis()$prediction[1]) {
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
        
        if (rubricsConcreteAnalysis()$threshold[2] < rubricsConcreteAnalysis()$prediction[2]) {
            
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
        
        # gs_read(for_gs, ws = "rubrics-airline") %>% 
        data.frame(
            metrics = c("accuracy","recall","precision","specificity"),
            threshold = c(75,73,70,75),
            point = rep(2,4)
        ) %>% 
            bind_cols(
                data.frame(
                    "prediction" = c(round(confMatAirline()$overall[1],2)*100, 
                                     round(confMatAirline()$byClass[1],2)*100,
                                     round(confMatAirline()$byClass[3],2)*100,
                                     round(confMatAirline()$byClass[2],2)*100)
                )
            )
        
        
    })
    
    output$Airlineacc <- renderInfoBox({
        
        if (rubricsAirline()$threshold[1] < rubricsAirline()$prediction[1]) {
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
        
        if (rubricsAirline()$threshold[2] < rubricsAirline()$prediction[2]) {
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
        
        if (rubricsAirline()$threshold[3] < rubricsAirline()$prediction[3]) {
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
        
        if (rubricsAirline()$threshold[4] < rubricsAirline()$prediction[4]) {
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
    
    # Image Classification ----
    
    joined <- reactive({
        submission() %>% 
            left_join(y = read_csv("solution/sol_class_img.csv"),by = "id") %>% 
            setNames(c("id","submission","actual"))
    }) 
    
    confMatIMG_acc <- reactive({
        
        IMGmetrics <- confusionMatrix(
            as.factor(joined()$submission),
            as.factor(joined()$actual)
        )
        
    })
    
    rubricsIMG_acc <- reactive({
        # gs_read(for_gs, ws = "rubrics-imageclass") %>% 
        data.frame(
            metrics = "accuracy",
            threshold = 75,
            point = 1
        ) %>% 
            bind_cols(
                data.frame(
                    "prediction" = round(confMatIMG_acc()$overall[1],2)*100
                )
            )
        
    })
    
    output$IMGacc <- renderInfoBox({
        
        if (rubricsIMG_acc()$threshold[1] < rubricsIMG_acc()$prediction[1]) {
            infoBox(paste(
                round(confMatIMG_acc()$overall[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(paste(
                round(confMatIMG_acc()$overall[1], 2)*100, "%"
            ), icon = icon("bullseye"), subtitle = "Accuracy", color = "red", fill = TRUE)
        }
        
    })
    
    confMatIMG <- reactive({
        
        IMGmetrics_all <- confusionMatrix(
            as.factor(joined()$submission),
            as.factor(joined()$actual),positive = "beach"
        )
        
        IMGmetrics_all <- IMGmetrics_all$byClass %>% as.data.frame() %>% 
            select(Sensitivity,Specificity,Precision) %>% 
            setNames(c("Recall","Specificity","Precision")) %>% 
            tibble::rownames_to_column(var = "Class") %>% 
            mutate(Class = stringr::str_replace_all(Class,pattern = "Class: ","")) %>% 
            tidyr::pivot_longer(cols = c("Recall","Specificity","Precision")) %>% 
            mutate(classname = paste(Class,name,sep = "_"))
        
    })
    
    rubricsIMG <- reactive({
        
        # gs_read(for_gs, ws = "rubrics-imageclass") %>% 
        #     slice(-1) %>% 
        data.frame(
            metrics = c("beach_Recall","beach_Specificity","beach_Precision",
                        "forest_Recall","forest_Specificity","forest_Precision",
                        "mountain_Recall","mountain_Specificity","mountain_Precision"),
            threshold = c(rep(70,9)),
            point = c(rep(1,9))
        ) %>% 
            bind_cols(confMatIMG()) %>% 
            mutate(goal = ifelse(value*100 > threshold, "pass", "fail"))
        
        
    })
    
    
    
    output$IMGrecall <- renderInfoBox({
        
        if (all(rubricsIMG() %>% 
                filter(name == "Recall") %>% 
                pull(goal) == "pass")){
            infoBox(h4("PASS"), 
                    icon = icon("search-plus"), subtitle = "Recall", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(h4("FAIL"),
                    icon = icon("search-plus"), subtitle = "Recall", color = "red", fill = TRUE)
        }
        
    })
    
    output$IMGprec <- renderInfoBox({
        
        if (all(rubricsIMG() %>% 
                filter(name == "Precision") %>% 
                pull(goal) == "pass")){
            infoBox(h4("PASS"),
                    icon = icon("key"), subtitle = "Precision", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(h4("FAIL"), 
                    icon = icon("key"), subtitle = "Precision", color = "red", fill = TRUE)
        }
        
    })
    
    output$IMGspec <- renderInfoBox({
        
        if (all(rubricsIMG() %>% 
                filter(name == "Specificity") %>% 
                pull(goal) == "pass")){
            infoBox(h4("PASS"), 
                    icon = icon("search-minus"), subtitle = "Specificity", color = "green", fill = TRUE)
        }
        
        else  {
            infoBox(h4("FAIL"), 
                    icon = icon("search-minus"), subtitle = "Specificity", color = "red", fill = TRUE)
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
        
        else if (input$projectype == "Cyberbully Text Classification") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            
            validate(
                need(
                    "bully" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h6(textOutput(outputId = "textCyber"))
            
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
        
        else if (input$projectype == "Image Classification") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            
            validate(
                need(
                    "label" %in% colnames(submission()),
                    message = ""
                )
            )
            
            h6(textOutput(outputId = "textIMG"))
            
        }
        
        else (NULL)
        
    })
    
    ## Text Output SMS Classification ----
    
    output$textSMS <- renderText({
        
        temp <- rubricsSMS()$threshold < rubricsSMS()$prediction
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", res_auth$user, ", you get full (8 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    ## Text Output Cyberbully Text Classification ----
    
    output$textCyber <- renderText({
        
        temp <- rubricsCyber()$threshold < rubricsCyber()$prediction
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", res_auth$user, ", you get full (8 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    ## Text Output FNB Forecasting ----
    
    output$textFNB <- renderText({
        
        temp <- rubricsFNB()$threshold[2] > rubricsFNB()$prediction[2]
        
        
        if (sum(temp) == 1) {
            
            paste("Congratulation", res_auth$user, ", you get full (6 points) on Evaluation Dataset!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    ## Text Output Scotty Classification ----
    
    output$textScottClass <- renderText({
        
        temp <- rubricsScottyClass()$threshold <= rubricsScottyClass()$prediction
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", res_auth$user, ", you get full (8 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    ## Text Output Concrete Prediction ----
    
    output$textConcretepred <- renderText({
        
        
        if (rubricsConcrete()$threshold[1] > rubricsConcrete()$prediction[1] &&
            rubricsConcrete()$threshold[2] < rubricsConcrete()$prediction[2]) {
            
            paste("Congratulation", res_auth$user, ", you get full (6 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    ## Text Output Scotty Forecasting ----
    
    output$textScottyts <- renderText({
        
        temp <- rubricsScottyts()$threshold > rubricsScottyts()$mae
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", res_auth$user, ", you get full (8 points) on Evaluation Dataset!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
        
    })
    
    ## Text Output Concrete Analysis ----
    
    output$textConcreteAnalysis <- renderText({
        
        
        if (rubricsConcreteAnalysis()$threshold[1] > rubricsConcreteAnalysis()$prediction[1] &&
            rubricsConcreteAnalysis()$threshold[2] < rubricsConcreteAnalysis()$prediction[2]) {
            
            paste("Congratulation", res_auth$user, ", you get full (4) points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    
    ## Text Output Airline Classification ----
    
    output$textairline <- renderText({
        
        temp <- rubricsAirline()$threshold < rubricsAirline()$prediction
        
        
        if (sum(temp) == 4) {
            
            paste("Congratulation", res_auth$user, ", you get full (8 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    ## Text Output Image Classification ----
    
    output$textIMG <- renderText({
        
        temp <- rubricsIMG()$threshold < rubricsIMG()$value*100
        temp2 <- rubricsIMG_acc()$threshold < rubricsIMG_acc()$prediction*100
        
        if (sum(temp) + sum(temp2) == 10) {
            
            paste("Congratulation", res_auth$user, ", you get full (8 points) on Model Evaluation!")
            
        }
        
        else {
            
            "Oops! You did very well, but need to improve the model."
            
        }
        
    })
    
    # Additional table ----
    ## Additional table box for image classification
    
    output$confadd <- renderDataTable({
        if (input$projectype == "Image Classification" & is.null(submission()) == FALSE){
            confmat_df <- confMatIMG() %>% 
                select(Class,name,value) %>% 
                pivot_wider(names_from = name,values_from = value) %>% 
                mutate(Recall = round(Recall,3),
                       Specificity = round(Specificity,3),
                       Precision = round(Precision,3))
            
            
            datatable(data = confmat_df,
                      caption = "Detailed Metrics for Image Classification Case | Positive level: Beach | You need to reach the minimum 0.7 for every metrics and class to pass",
                      options = list(dom = "t",
                                     initComplete = JS(
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                         "}"), scrollX = TRUE),
                      rownames = F) %>%
                formatStyle(names(confmat_df),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
        }
        
        
    })
    
    
    
    # Leaderboard -----
    # Leaderboard SMS Classification ----
    
    output$board <- renderDataTable({
        if ((input$projectype == "SMS" & is.null(submission())) | res_auth$user == "teamalgoritma") {
            
            sheet_sms <- read_sheet(ss = sss,sheet = "sms")
            
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
        
        else if (input$projectype == "SMS" & res_auth$user != "teamalgoritma" ) {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    res_auth$user != "",
                    message = ""
                )
            )
            
            # gs_add_row(
            #     ss = for_gs, 
            #     ws = "sms", 
            #     input = c(res_auth$user, 
            #               round(confMatSMS()$overall[1],2),
            #               round(confMatSMS()$byClass[1],2),
            #               round(confMatSMS()$byClass[3],2), 
            #               round(confMatSMS()$byClass[2],2),
            #               format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y"))
            # )
            
            sheet_append(ss = sss,sheet = "sms",
                         data = data.frame(Name = res_auth$user, 
                                           Accuracy = as.numeric(round(confMatSMS()$overall[1],2)),
                                           Recall = as.numeric(round(confMatSMS()$byClass[1],2)),
                                           Precision = as.numeric(round(confMatSMS()$byClass[3],2)), 
                                           Specificity = as.numeric(round(confMatSMS()$byClass[2],2)),
                                           `Last Submitted` = format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y")))
            
            sheet_sms <- read_sheet(ss = sss,sheet = "sms")
            
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
        
        else if ((input$projectype == "Scotty Classification" & is.null(submission())) | res_auth$user == "teamalgoritma") {
            sheet_scottyclass <- read_sheet(ss = sss,sheet = "scottyclass")
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
        
        else if (input$projectype == "Scotty Classification" & res_auth$user != "teamalgoritma") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    res_auth$user != "",
                    message = ""
                )
            )
            
            
            sheet_append(ss = sss, 
                         sheet = "scottyclass", 
                         data = data.frame(Name = res_auth$user, 
                                           Accuracy = round(confMatScotClass()$overall[1],2),
                                           Recall = round(confMatScotClass()$byClass[1],2),
                                           Precision = round(confMatScotClass()$byClass[3],2),
                                           Specificity = round(confMatScotClass()$byClass[2],2),
                                           `Last Submitted` = format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y")))
            
            sheet_scottyclass <- read_sheet(ss = sss,sheet = "scottyclass")
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
        
        ## Leaderboard Cyberbully Text Classification ----
        
        else if ((input$projectype == "Cyberbully Text Classification" & is.null(submission())) | res_auth$user == "teamalgoritma") {
            sheet_cyberclass <- read_sheet(ss = sss,sheet = "cyberbully")
            sheet_cyberclass %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`Recall`)) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: leaderboard scoring | Cyberbully Text Classification. ',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_cyberclass),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
        }
        
        else if (input$projectype == "Cyberbully Text Classification" & res_auth$user != "teamalgoritma") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    res_auth$user != "",
                    message = ""
                )
            )
            
            
            sheet_append(ss = sss, 
                         sheet = "cyberbully", 
                         data = data.frame(Name = res_auth$user, 
                                           Accuracy = round(confMatCyber()$overall[1],2),
                                           Recall = round(confMatCyber()$byClass[1],2),
                                           Precision = round(confMatCyber()$byClass[3],2),
                                           Specificity = round(confMatCyber()$byClass[2],2),
                                           `Last Submitted` = format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y")))
            
            sheet_cyberclass <- read_sheet(ss = sss,sheet = "cyberbully")
            sheet_cyberclass %>% 
                mutate(Name = str_to_title(Name)) %>% 
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>% 
                arrange(desc(`Last Submitted`)) %>% 
                dplyr::filter(!duplicated(Name)) %>% 
                dplyr::arrange(desc(`Recall`)) %>% 
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: leaderboard scoring | Cyberbully Text Classification. ',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE), 
                          rownames = T) %>% 
                formatStyle(names(sheet_cyberclass),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
        }
        
        ## Leaderboard FNB Forecasting ----
        
        else if ((input$projectype == "FNB" & is.null(submission())) | res_auth$user == "teamalgoritma") {
            sheet_fnb <- read_sheet(ss = sss,sheet = "fnb")
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
        
        else if (input$projectype == "FNB" & res_auth$user != "teamalgoritma") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    res_auth$user != "",
                    message = ""
                )
            )
            
            sheet_append(ss = sss,
                         sheet = "fnb",
                         data = data.frame(Name = res_auth$user,
                                           `MAE Score` = round(metricsFNB()$mae,2),
                                           `Last Submitted` = format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y")))
            sheet_fnb <- read_sheet(ss = sss,sheet = "fnb")
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
        
        else if ((input$projectype == "Scotty Time Series" & is.null(submission())) | res_auth$user == "teamalgoritma") {
            sheet_scottyts <- read_sheet(ss = sss,sheet = "scottyts")
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
        
        else if (input$projectype == "Scotty Time Series" & res_auth$user != "teamalgoritma") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    res_auth$user != "",
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
            
            sheet_append(ss = sss,
                         sheet = "scottyts",
                         data = data.frame(Name = res_auth$user,
                                           sxk97 = round(rubricsScottyts()$mae[1],2), 
                                           sxk9e = round(rubricsScottyts()$mae[2],2), 
                                           sxk9s = round(rubricsScottyts()$mae[3],2), 
                                           `all area` = round(rubricsScottyts()$mae[4],2), 
                                           `Last Submitted` = format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta") , "%a, %b-%d %X %Y")))
            
            sheet_scottyts <- read_sheet(ss = sss,sheet = "scottyts")
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
        
        else if ((input$projectype == "Concrete Prediction" & is.null(submission())) | res_auth$user == "teamalgoritma") {
            sheet_concreterm <- read_sheet(ss = sss,sheet = "concreterm")
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
        
        
        
        else if (input$projectype == "Concrete Prediction" & res_auth$user != "teamalgoritma") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    res_auth$user != "",
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
            
            sheet_append(ss = sss, 
                         sheet = "concreterm",
                         data = data.frame(Name = res_auth$user, 
                                           `MAE Score` = round(metricsConcretePred()$mae,2),
                                           `R-square` = round(metricsConcretePred()$rsq,2)*100, 
                                           `Last Submitted` = format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y")))
            sheet_concreterm <- read_sheet(ss = sss,sheet = "concreterm")
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
        
        else if ((input$projectype == "Concrete Analysis" & is.null(submission())) | res_auth$user == "teamalgoritma") {
            sheet_concreteanalysis <- read_sheet(ss = sss,sheet = "concreteanalysis")
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
        
        
        
        else if (input$projectype == "Concrete Analysis" & res_auth$user != "teamalgoritma") {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    res_auth$user != "",
                    message = ""
                )
            )
            
            sheet_append(ss = sss, 
                         sheet = "concreteanalysis",
                         data = data.frame(Name = res_auth$user, 
                                           `MAE Score` = round(metricsConcretePred()$mae,2),
                                           `R-square` = round(metricsConcretePred()$rsq,2)*100, 
                                           `Last Submitted` = format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y")))
            
            sheet_concreteanalysis <- read_sheet(ss = sss,sheet = "concreteanalysis")
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
        
        else if ((input$projectype == "Airline Classification" & is.null(submission())) | res_auth$user == "teamalgoritma") {
            
            sheet_airline <- read_sheet(ss = sss,sheet = "airlineclass")
            
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
        
        else if (input$projectype == "Airline Classification" & res_auth$user != "teamalgoritma" ) {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    res_auth$user != "",
                    message = ""
                )
            )
            
            sheet_append(
                ss = sss, 
                sheet = "airlineclass", 
                data = data.frame(Name = res_auth$user, 
                                  Accuracy = round(confMatAirline()$overall[1],2),
                                  Recall = round(confMatAirline()$byClass[1],2),
                                  Precision = round(confMatAirline()$byClass[3],2), 
                                  Specificity = round(confMatAirline()$byClass[2],2),
                                  `Last Submitted` = format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y"))
            )
            
            sheet_airline <- read_sheet(ss = sss,sheet = "airlineclass")
            
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
        
        ## Leaderboard Image Classification ----
        
        else if ((input$projectype == "Image Classification" & is.null(submission())) | res_auth$user == "teamalgoritma") {
            
            sheet_IMG <- read_sheet(ss = sss,sheet = "image-class")
            
            sheet_IMG %>%
                mutate(Name = str_to_title(Name)) %>%
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>%
                arrange(desc(`Last Submitted`)) %>%
                dplyr::filter(!duplicated(Name)) %>%
                dplyr::arrange(desc(`Accuracy`)) %>%
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: leaderboard scoring | Image Classification.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE),
                          rownames = T) %>%
                formatStyle(names(sheet_IMG),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
        }
        
        else if (input$projectype == "Image Classification" & res_auth$user != "teamalgoritma" ) {
            
            validate(
                need(
                    input$FileName != "",
                    message = ""
                )
            )
            
            validate(
                need(
                    res_auth$user != "",
                    message = ""
                )
            )
            
            sheet_append(
                ss = sss,
                sheet = "image-class",
                data = data.frame(Name = res_auth$user,
                                  Accuracy = round(confMatIMG_acc()$overall[1],2),
                                  Recall = ifelse(all(rubricsIMG() %>% 
                                                          filter(name == "Recall") %>% 
                                                          pull(goal) == "pass"),"PASS","FAILL"),
                                  Precision = ifelse(all(rubricsIMG() %>% 
                                                             filter(name == "Precision") %>% 
                                                             pull(goal) == "pass"),"PASS","FAILL"),
                                  Specificity = ifelse(all(rubricsIMG() %>% 
                                                               filter(name == "Specificity") %>% 
                                                               pull(goal) == "pass"),"PASS","FAILL"),
                                  `Last Submitted` = format(Sys.time() %>% lubridate::with_tz(tzone = "Asia/Jakarta"), "%a, %b-%d %X %Y"))
            )
            
            sheet_IMG <- read_sheet(ss = sss,sheet = "image-class")
            
            sheet_IMG %>%
                mutate(Name = str_to_title(Name)) %>%
                mutate(`Last Submitted` = as.POSIXct(gsub(x = `Last Submitted`, pattern = " (AM|PM) ", replacement = " "),
                                                     format = "%a, %b-%d %X %Y")) %>%
                arrange(desc(`Last Submitted`)) %>%
                dplyr::filter(!duplicated(Name)) %>%
                dplyr::arrange(desc(`Accuracy`)) %>%
                mutate(`Last Submitted` = format(`Last Submitted`, "%a, %b-%d %X %Y")) %>%
                datatable(caption = 'Table 1: leaderboard scoring | Image Classification.',
                          options = list(dom = "t",
                                         initComplete = JS(
                                             "function(settings, json) {",
                                             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                             "}"), scrollX = TRUE),
                          rownames = T) %>%
                formatStyle(names(sheet_IMG),
                            backgroundColor = "black",
                            background = "black",
                            target = "row",
                            fontSize = "100%")
            
        }
        
        else {NULL}
        
    })
    
    
}


