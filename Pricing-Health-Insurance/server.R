

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
library(shinymanager)
library(ChainLadder)
library(xtable)
library(dplyr)
# library(shinyauthr)
# library(tidyverse)
# library(glmnet)





# Premium Calculation
PersonalDayData <- read_excel("PersonalDayData.xlsx")
Liability_Franchise_Tariff <- read_excel("Liability_Franchise_Tariff.xlsx")


Template_Liability_Franchise_Tariff <- read_excel("Template_Liability_Franchise_Tariff.xlsx")
Tempalte_IBNRDATA <- read_excel("Tempalte_IBNRDATA.xlsx")


PROVINCE <- read_excel("Liability_Franchise_Tariff.xlsx",
  sheet = "Province"
)
NUMBER_ins <- read_excel("Liability_Franchise_Tariff.xlsx",
  sheet = "number_ins"
)
JOBGROUP <- read_excel("Liability_Franchise_Tariff.xlsx",
  sheet = "job_group"
)


#-------------------------------------------------------------------------------

# New Contract

Limits <- read_excel("Limits_farsi.xlsx" , sheet = "limit")
Cover_Names<- read_excel("Limits_farsi.xlsx" , sheet = "franchise")
ostan = read_excel("Limits_farsi.xlsx" , sheet = "province")
JobGroup = read_excel("Limits_farsi.xlsx" , sheet = "job_group")
typeceiling = read_excel("Limits_farsi.xlsx" , sheet = "Type Ceiling")
servicenetwork = read_excel("Limits_farsi.xlsx" , sheet = "Service network")

# Template_Limits_1 = read_excel("Template_Limits.xlsx" )




# data.frame with credentials info
credentials <- data.frame(
  user = c("1", "Didar", "Mojtaba", "Amirhossein", "M_hassani"),
  password = c("1", "Abbas", "Abed", "Jafari", "51800"),
  stringsAsFactors = FALSE
)


server <- function(input, output, session) {
  result_auth <- secure_server(check_credentials = check_credentials(credentials))


  #-------------------------------------------------------------------------------
  #----------------------New Contract Premium Calculate-----------------------
  #-------------------------------------------------------------------------------
  
  


  

  Liab_pr = function( x , L , R ){
    
    pele = sum( x > L ) + 1
    
    L[pele] = ifelse( pele > 1 , x - L[pele-1] , x ) 
    
    
    return( sum( L[1:pele] *R[1:pele] / 1000 ) )
    
  }
  
  New_Contract_Premium = reactive({ 
  
  x = c( input$bastari ,input$j_takhasosi ,input$zayeman , input$para , input$kh_az ,
         input$visit , input$dandan , input$eynak , input$samak ,  input$sarpa ,
         input$badan ,  input$erotez 
  )
  
  
  FRANCHISE = c( input$f_bastari ,input$f_j_takhasosi ,input$f_zayeman , input$f_para  , input$f_kh_az ,
                 input$f_visit , input$f_dandan , input$f_eynak , input$f_samak , input$f_sarpa ,
                  input$f_badan ,  input$f_erotez 
  )
  

  
  #-------------------------------------------------------------------------------
 
 
  
  
  Service_Premium = c()
  
  for( i in 1:length(FRANCHISE) ){
    
    L = Limits[,2*i-1]
    L = L[ !is.na(L) ]
    
    R = Limits[,2*i]
    R = R[ !is.na(R) ]
    
    # FRANCHISE = Franchise %>% filter( Serivice == names( Limits[,2*i-1] ) ) %>% select( franchise )
    
    
    Service_Premium[i] = Liab_pr( x = x[i] , L , c(R,rev(R)[1]) ) * ( 1 + ( 30 - FRANCHISE[i] )*1.5/100 ) 
    
  }
  
  #-------------------------------------------------------------------------------
  
  N_pele = c(200,500,1000,3000,5000)
  N_takhfif = c(0,.05,.10,.15,.20,.30)
  
  d_N = N_takhfif[ sum( input$N_Total_insured >= N_pele )+1 ]
  
  
  #-------------------------------------------------------------------------------
  
  age_proportion = c( input$N_Total_insured - input$N_insured_60_70 - input$N_insured_70  ,  input$N_insured_60_70 , input$N_insured_70 ) / input$N_Total_insured
  
  d_AGE = ifelse( input$N_Total_insured < 1000 , sum( c(  input$Mean_Age - 60  , 55 , 105 ) * age_proportion ), input$Mean_Age - 60 )/100
  

  
  #-------------------------------------------------------------------------------
  
  SHABAKEH = -c(15,25,35)/100
  
  
  d_SHABAKE <- 
    ifelse( input$Service_network == servicenetwork$ServiceNetwork[1] , SHABAKEH[1], 
            ifelse( input$Service_network ==servicenetwork$ServiceNetwork[2], SHABAKEH[2], SHABAKEH[3] ) )
  
  
  
  OSTAN = input$New_Province
  
  d_OSTAN = ostan$discount[ ostan$province == OSTAN ]
  
  
  JOB_GROUP = input$NewJob_group
  
  d_JOB = JobGroup$discount[ JobGroup$JOB_GROUP == JOB_GROUP ]
  
  
  
  Limit_type = c( typeceiling$TYPE_CEILING )
  LIMIT_TYPE = c(15,25,50)/100
  
  
  d_LIMIT_TYPE <- 
    ifelse( input$Type_ceiling == typeceiling$TYPE_CEILING[1],  LIMIT_TYPE[1],
            ifelse(input$Type_ceiling == typeceiling$TYPE_CEILING[2], LIMIT_TYPE[2], LIMIT_TYPE[3] )
            )


  
  sum( Service_Premium )*(1 + d_N + d_AGE + d_OSTAN + d_JOB + d_LIMIT_TYPE + d_SHABAKE - input$NewDiscountRate ) * 500

})

# observeEvent( New_Contract_Premium(), d_AGE )


  
  output$Yearly_New_Premium_Data <- renderDataTable(
    DT::datatable(
      {
        data.frame( "Yearly Premium" = round( sum( New_Contract_Premium() ) * 10000),
                    "Monthly Premium" =  round( sum( New_Contract_Premium() ) * 10000 / 12 * (1 + .12)^(1 / 12))
        )
      },
      options = list(
        paging = FALSE,
        searching = FALSE,
        fixedColumns = TRUE,
        autoWidth = FALSE,
        ordering = FALSE,
        dom = "tB",
        buttons = c("copy", "csv", "excel"),
        lengthMenu = 1, pageLength = 1,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': 'plum', 'color': '1c1b1b'});",
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
  #---------------------------- Prmium Calculation -------------------------------
  #-------------------------------------------------------------------------------



  output$downloadPremiumTemplate <- downloadHandler(
    filename = function() {
      paste("PremiumTemplate", ".xlsx", sep = "")
    },
    content = function(file) {
      xlsx::write.xlsx(Template_Liability_Franchise_Tariff, file)
    }
  )


  Liability_Franchise_Tariff <- reactive({
    inFile <- input$fileCOVERAGES

    if (is.null(inFile)) {
      return(NULL)
    }

    read_excel(inFile$datapath)
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
      left_join(Liability_Franchise_Tariff() |> select(Coverage), by = "Coverage") |>
      mutate(
        Final_Sum_Coverage = Sum_Coverage * input$DevCoef,
        Pr_Coverage = round(Final_Sum_Coverage / input$N_person *
          (1 + as.double(PROVINCE[PROVINCE$province == input$ProvinceDis, 2]) +
            as.double(JOBGROUP[JOBGROUP$JOB_GROUP == input$JobDis, 2]) -
            input$DiscountRate))
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
        dom = "tB",
        buttons = c("copy", "excel"),
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
    tibble(
      Yearly_Premium1 = as.numeric(colSums(Premium_Coverage()[, "Pr_Coverage"])),
      Monthly_interest = ((1 + input$Interest)^(1 / 12) - 1) * 12,
      Monthly_premium = round(Yearly_Premium1 / 12 * (1 + Monthly_interest)^(1 / 12))
    ) |> select(-Monthly_interest)
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
        dom = "tB",
        buttons = c("copy", "csv", "excel"),
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



  output$downloadIBNRTemplate <- downloadHandler(
    filename = function() {
      paste("IBNRTemplate", ".xlsx", sep = "")
    },
    content = function(file) {
      xlsx::write.xlsx(Tempalte_IBNRDATA, file)
    }
  )

  TRIANGLE_DATA <- reactive({
    inFile <- input$fileIBNR

    if (is.null(inFile)) {
      return(NULL)
    }

    read_excel(inFile$datapath)
  })

  MCL_Model <- eventReactive(
    input$run_IBNR,
    MackChainLadder(TRIANGLE_DATA(), est.sigma = ifelse( input$IBNR_Method == "input$IBNR_Method", "Mack", input$IBNR_Method  ) )
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
        dom = "tB",
        buttons = c("copy", "csv", "excel"),
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


  output$Plot_Full_Triangle <- renderPlot(
    {
      plot(MCL_Model())
    },
    height = 1000,
    width = 1700
  )
}
