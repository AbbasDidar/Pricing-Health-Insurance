

library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(readxl)
library(plotly)
library(treemapify)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)
library(shinymanager)
library(ChainLadder)
library(xtable)
# library(shinyauthr)
# library(tidyverse)
# library(glmnet)
library(randomForest)




# Premium Calculation 
PersonalDayData <- read_excel("PersonalDayData.xlsx")
Liability_Franchise_Tariff <- read_excel("Liability_Franchise_Tariff.xlsx")


PROVINCE <- read_excel("Liability_Franchise_Tariff.xlsx", 
                       sheet = "Province")
NUMBER_ins <- read_excel("Liability_Franchise_Tariff.xlsx", 
                         sheet = "number_ins")
JOBGROUP <- read_excel("Liability_Franchise_Tariff.xlsx", 
                       sheet = "job_group")


# data.frame with credentials info
credentials <- data.frame(
    user = c("1","Didar", "Mojtaba", "Amirhossein", "M_hassani" ),
    password = c("1","Abbas", "Abed", "Jafari", "51800"),
    stringsAsFactors = FALSE
)


server <- function(input, output, session) {
    
    
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    
    
    #-------------------------------------------------------------------------------
    #------------------------------- Time Series -----------------------------------
    #-------------------------------------------------------------------------------
    
    
    bike_transactions_tbl <- eventReactive(
        input$run_Time_Series | input$run_Time_Series_predict,
        bike_sharing_daily %>%
            mutate(cnt = cnt) |>
            select(dteday, cnt) %>%
            set_names(c("date", "value"))
    )
    
    
    
    splits <- eventReactive(
        input$run_Time_Series | input$run_Time_Series_predict,
        bike_transactions_tbl() %>%
            time_series_split(assess = paste0(input$TimeSeries_month, " month"), cumulative = TRUE)
    )
    
    recipe_spec <- eventReactive(
        input$run_Time_Series | input$run_Time_Series_predict,
        recipe(value ~ date, training(splits())) %>%
            step_timeseries_signature(date) %>%
            step_rm(
                contains("am.pm"), contains("hour"), contains("minute"),
                contains("second"), contains("xts")
            ) %>%
            step_fourier(date, period = 365, K = 5) %>%
            step_dummy(all_nominal())
    )
    
    
    
    calibration_table <- eventReactive(
        input$run_Time_Series,
        if (input$TimeSeries_Model == "GLMnet") {
            modeltime_table(
                workflow() %>%
                    add_model(
                        linear_reg(penalty = 0.01, mixture = 0.5) %>%
                            set_engine("glmnet")
                    ) %>%
                    add_recipe(recipe_spec() %>% step_rm(date)) %>%
                    fit(training(splits()))
            ) %>%
                modeltime_calibrate(testing(splits()))
        } else {
            if (input$TimeSeries_Model == "Random Forest") {
                modeltime_table(
                    workflow() %>%
                        add_model(
                            rand_forest(trees = 500, min_n = 30) %>%
                                set_engine("randomForest")
                        ) %>%
                        add_recipe(recipe_spec() %>% step_rm(date)) %>%
                        fit(training(splits()))
                ) %>%
                    modeltime_calibrate(testing(splits()))
            } else {
                modeltime_table(
                    workflow() %>%
                        add_model(
                            prophet_boost(seasonality_yearly = TRUE) %>%
                                set_engine("prophet_xgboost")
                        ) %>%
                        add_recipe(recipe_spec()) %>%
                        fit(training(splits()))
                ) %>%
                    modeltime_calibrate(testing(splits()))
            }
        }
    )
    
    output$TimeSeries_plot_forecast <- renderPlotly(
        calibration_table() %>%
            modeltime_forecast(actual_data = bike_transactions_tbl()) %>%
            plot_modeltime_forecast(.interactive = TRUE)
    )
    
    #-------------------------------------------------------------------------------
    
    
    calibration_table_Predict <- eventReactive(
        input$run_Time_Series_predict,
        if (input$TimeSeries_Model_predict == "GLMnet") {
            modeltime_table(
                workflow() %>%
                    add_model(
                        linear_reg(penalty = 0.01, mixture = 0.5) %>%
                            set_engine("glmnet")
                    ) %>%
                    add_recipe(recipe_spec() %>% step_rm(date)) %>%
                    fit(training(splits()))
            ) %>%
                modeltime_calibrate(testing(splits()))
        } else {
            if (input$TimeSeries_Model_predict == "Random Forest") {
                modeltime_table(
                    workflow() %>%
                        add_model(
                            rand_forest(trees = 500, min_n = 30) %>%
                                set_engine("randomForest")
                        ) %>%
                        add_recipe(recipe_spec() %>% step_rm(date)) %>%
                        fit(training(splits()))
                ) %>%
                    modeltime_calibrate(testing(splits()))
            } else {
                modeltime_table(
                    workflow() %>%
                        add_model(
                            prophet_boost(seasonality_yearly = TRUE) %>%
                                set_engine("prophet_xgboost")
                        ) %>%
                        add_recipe(recipe_spec()) %>%
                        fit(training(splits()))
                ) %>%
                    modeltime_calibrate(testing(splits()))
            }
        }
    )
    
    RESULT_PREDICT <- eventReactive(
        input$run_Time_Series_predict,
        calibration_table_Predict() %>%
            modeltime_refit(bike_transactions_tbl()) %>%
            modeltime_forecast(h = paste0(input$TimeSeries_month_predict, " month"), actual_data = bike_transactions_tbl())
    )
    
    output$TimeSeries_data_forecast <- DT::renderDataTable(
        DT::datatable(
            {
                RESULT_PREDICT()[, 3:5] |> rename(date = .index)
            },
            options = list(
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 10,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'lightsalmon', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    output$TimeSeries_plot_predict <- renderPlotly(
        RESULT_PREDICT() %>%
            plot_modeltime_forecast(.interactive = TRUE)
    )
    
    
    
    output$downloadDataTimeSeries <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(RESULT_PREDICT()[, 3:5] |> rename(date = .index), file)
        }
    )
    
    #-------------------------------------------------------------------------------
    #---------------------------- Prmium Calculation -------------------------------
    #-------------------------------------------------------------------------------
    
    
    Liability_Franchise_Tariff <- reactive({
        inFile <- input$fileCOVERAGES
        
        if (is.null(inFile)) {
            return(NULL) }
        
        read_excel( inFile$datapath )
        
    })
    
    
    
    Premium_Coverage <- eventReactive(
        input$run_PremiumCalc,
        PersonalDayData |>
            group_by(NationalCode, Coverage) |>
            summarise(Sum_ReqAmount = sum(RequestAmount)) |>
            left_join(Liability_Franchise_Tariff(), by = "Coverage") |>
            mutate(
                MUT_Sum_ReqAmount = Sum_ReqAmount * (1 + Tariff / 100),
                MUT_Ceil_Fr_Sum_ReqAmount = ifelse(MUT_Sum_ReqAmount > Ceiling, Ceiling, MUT_Sum_ReqAmount) * (1 - Franchise / 100)
            ) |>
            group_by(Coverage) |>
            summarise(Sum_Coverage = sum(MUT_Ceil_Fr_Sum_ReqAmount)) |>
            left_join( Liability_Franchise_Tariff() |> select(Coverage), by = "Coverage") |>
            mutate(
                Final_Sum_Coverage = Sum_Coverage * input$DevCoef,
                Pr_Coverage = round( Final_Sum_Coverage / input$N_person * 
                                         ( 1 + as.double( PROVINCE[ PROVINCE$province == input$ProvinceDis, 2] ) + 
                                               as.double( JOBGROUP[ JOBGROUP$JOB_GROUP == input$JobDis, 2] ) -
                                               input$DiscountRate ) )
            ) |>
            select(Coverage, Pr_Coverage) 
    )
    
    output$Premium_Cove_Data <- DT::renderDataTable(
        DT::datatable(
            {
                Premium_Coverage()
            },
            extensions = "Buttons",
            options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = 'tB',
                buttons = c('copy', 'excel'),
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 20,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'burlywood', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "auto",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    Yearly_Premium <- eventReactive(
        input$run_PremiumCalc,
        tibble( Yearly_Premium1 = as.numeric( colSums( Premium_Coverage()[,"Pr_Coverage"]  )  ),
                Monthly_interest = ((1 + input$Interest )^(1/12) - 1) * 12,
                Monthly_premium = round( Yearly_Premium1 / 12 * ( 1 + Monthly_interest )^(1/12) )
        ) |> select( - Monthly_interest )
    )
    
    output$Yearly_Premium_Data <- renderDataTable(
        DT::datatable(
            {
                Yearly_Premium()
            },
            options = list(
                paging = FALSE,
                searching = FALSE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = FALSE,
                dom = 'tB',
                buttons = c('copy', 'csv', 'excel'),
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 20,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'darksalmon', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "auto",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    
    #-------------------------------------------------------------------------------
    #----------------------------------- IBNR --------------------------------------
    #-------------------------------------------------------------------------------
    
    
    TRIANGLE_DATA <- reactive({
        inFile <- input$fileIBNR
        
        if (is.null(inFile)) {
            return(NULL) }
        
        read_excel( inFile$datapath )
        
    })
    
    MCL_Model <- eventReactive( 
        input$run_IBNR,
        MackChainLadder(TRIANGLE_DATA(), est.sigma= input$IBNR_Method )
        
    )
    
    
    
    output$Full_Triangle <- DT::renderDataTable(
        DT::datatable(
            {
                M <- MCL_Model()$FullTriangle
                class(M)[1] <- "matrix"
                round(M)
            },
            extensions = "Buttons",
            options = list(
                paging = TRUE,
                searching = TRUE,
                fixedColumns = TRUE,
                autoWidth = TRUE,
                ordering = TRUE,
                dom = 'tB',
                buttons = c('copy', 'csv', 'excel'),
                lengthMenu = list(c(10, 25, 50), c("10", "25", "50")), pageLength = 20,
                initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': 'burlywood', 'color': '1c1b1b'});",
                    "}"
                ),
                columnDefs = list(list(className = "dt-center", targets = "_all"))
            ),
            filter = "top",
            selection = "multiple",
            style = "bootstrap",
            class = "cell-border stripe",
            rownames = FALSE
        )
    )
    
    
    output$Plot_Full_Triangle <- renderPlot({    
        plot( MCL_Model() )}, height = 1000, width = 1700)
    
}








