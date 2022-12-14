
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
library(shinymanager)
# library(bslib)

# PIN_FOLDER <- board_folder("DayDataBase", versioned = FALSE)
# # PIN_FOLDER |> pin_write( KhatamDetail )
# z





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
ostan = read_excel("Limits_farsi.xlsx" , sheet = "province")
JobGroup = read_excel("Limits_farsi.xlsx" , sheet = "job_group")
typeceiling = read_excel("Limits_farsi.xlsx" , sheet = "Type Ceiling")
servicenetwork = read_excel("Limits_farsi.xlsx" , sheet = "Service network")


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: Blue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"




inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"




# Define UI
ui <- secure_app(
  head_auth = tags$script(inactivity),
  fluidPage(
    navbarPage("",
      inverse = TRUE,
      # theme = bs_theme(bootswatch = "minty"),
      theme = shinytheme("cerulean"),

      #-------------------------------------------------------------------------------
      #----------------------New Contract Premium Calculate-----------------------
      #-------------------------------------------------------------------------------

      tabPanel("New Contract Premium Calculate",
               fluid = TRUE, icon = icon("umbrella", verify_fa = FALSE),
               tags$style(button_color_css),
               shinythemes::themeSelector(),
               sidebarLayout(
                 sidebarPanel(
                   h4("", style = "color:deeppink"),
                   br(),
           
                   fluidRow(
                     column(
                       6,
                       numericInput(
                         inputId = "N_Total_insured",
                         label = "?????????? ???????? ??????",
                         min = 5,
                         max = 1000000,
                         value = 500,
                         width = "350px"
                       ),
                       style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(6,
                            ofset = 2,
                            selectInput(
                              inputId = "New_Province",
                              label = "??????????",
                              choices =  sort( ostan$province ),
                              selected = ostan$province[10]
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(12,
                            ofset = 2,
                            sliderInput(
                              inputId = "Mean_Age",
                              label = "?????????????? ????",
                              min = 0, max = 80, value = 45, step = 1,
                              width = "650px"
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(6,
                            ofset = 2,
                            numericInput(
                              inputId = "N_insured_60_70",
                              label = "?????????? ?????????? ?????? 60 ???? 70 ??????",
                              min = 5,
                              max = 1000000,
                              value = 200,
                              width = "350px"
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(6,
                            ofset = 2,
                            numericInput(
                              inputId = "N_insured_70",
                              label = "?????????? ?????????? ?????????? ???? 70 ??????",
                              min = 5,
                              max = 1000000,
                              value = 100,
                              width = "350px"
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(12,
                            ofset = 2,
                            selectInput(
                              inputId = "Service_network",
                              label = "???????? ?????????? ??????????",
                              choices =  sort( servicenetwork$ServiceNetwork ),
                              selected = servicenetwork$ServiceNetwork[2]
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(6,
                            ofset = 2,
                            selectInput(
                              inputId = "NewJob_group",
                              label = "???????? ????????",
                              choices = JobGroup$JOB_GROUP ,
                              selected = JobGroup$JOB_GROUP[4]
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(6,
                            ofset = 2,
                            selectInput(
                              inputId = "Type_ceiling",
                              label = "?????? ??????",
                              choices =  sort( typeceiling$TYPE_CEILING ),
                              selected = sort( typeceiling$TYPE_CEILING )[1]
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(12,
                            ofset = 2,
                            sliderInput(
                              inputId = "NewDiscountRate",
                              label = "?????????? ??????????",
                              min = 0, max = .50, value = 0, step = .01,
                              width = "650px"
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     # column(
                     #   9,
                     #   actionButton("run_New_PremiumCalc", "apply", class = "btn-info")
                     # ),
                     column(
                       12,
                       style = "font-family: B Mitra;font-size: 15px;font-weight: bold",


                       dataTableOutput(outputId = "Yearly_New_Premium_Data")
                     )
                   )
                 ),
                 mainPanel(
                   
                 
                   
                   h1("?????????? ?????? ???????? - ???????????? ????????", style ="color:purple; font-family: B Mitra; font-size: 35px; text-align:center"),
                   hr(),
                   br(),
                   
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "bastari",
                            label = "??????????",
                            value = 500 
                          ),
                          
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "j_takhasosi",
                            label = "?????????? ??????????",
                            value = 1000 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "zayeman",
                            label = "????????????",
                            value = 75 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "para",
                            label ="??????????????????????",
                            value = 100 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
       
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "kh_az",
                            label = "?????????? ????????????????????",
                            value = 80 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "dandan",
                            label =  "????????????????????",
                            value = 100 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "eynak",
                            label = "???????? ?????? ?? ??????" ,
                            value = 10 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "samak",
                            label = "????????" , 
                            value = 40 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   
                   
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "sarpa",
                            label = "?????????? ???????? ????????????" , 
                            value = 100 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "visit",
                            label =  "???????????? ???????? ?? ?????????? ??????????????" ,
                            value = 100 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "badan",
                            label = "???????? ?????????? ?????????? ??????" , 
                            value = 500 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "erotez",
                            label =  "??????????" ,
                            value = 20 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   
                   #----------------------------------------------------------------------------------------------
               
                   h1("?????????? ?????????????? ???????????????", style ="color:purple; font-family: B Mitra; font-size: 35px; text-align:center"),
                   hr(),
                   br(),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_bastari",
                            label ="??????????",
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_j_takhasosi",
                            label = "?????????? ??????????",
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_zayeman",
                            label =  "????????????" , 
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_para",
                            label =  "??????????????????????" , 
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   
                   
                   
                 
              
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_kh_az",
                            label =  "?????????? ????????????????????" , 
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_dandan",
                            label = "????????????????????" , 
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_eynak",
                            label =  "???????? ?????? ?? ??????" , 
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_samak",
                            label = "????????" , 
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   
            
                   
                   
 
                   
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_sarpa",
                            label =  "?????????? ???????? ????????????" , 
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_visit",
                            label = "???????????? ???????? ?? ?????????? ??????????????" , 
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_badan",
                            label ="???????? ?????????? ?????????? ??????" , 
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "erotez",
                            label =  "??????????" ,
                            min = 0,
                            max = .5,
                            value = .1,
                            step = .1
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   
                   
                   
                   
                 )
               ),
               hr(),
               p(em("Developed by"), br(),
                 a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
                 style = "text-align:center;color:turquoise; font-size: 20px; font-family: times"
               )
      ),


      #-------------------------------------------------------------------------------
      #----------------------Renewal Contract Premium Calculate-----------------------
      #-------------------------------------------------------------------------------

      tabPanel("Renewal Contract Premium Calculate",
        fluid = TRUE, icon = icon("umbrella", verify_fa = FALSE),
        tags$style(button_color_css),
        shinythemes::themeSelector(),
        sidebarLayout(
          sidebarPanel(
            h4("", style = "color:deeppink"),
            br(),
            fluidRow(
              column(
                6,
                h4("???????? ?????????? ???????? ?????????? ???? ???????????? ?? ???? ????????????", style = "font-family: B Mitra;font-weight: bold;font-size: 15px;text-align:left"),
                downloadButton("downloadPremiumTemplate")
                
                
              ),
              column(
                6,
                fileInput(
                  "fileCOVERAGES",
                  h4("???????????????? ???????? ?????????? ???????? ??????", style = "font-family: B Mitra;font-weight: bold;font-size: 15px;text-align:center"),
                  accept = ".xlsx"
                )
              )
            ),
            fluidRow(
              column(
                6,
                numericInput(
                  inputId = "N_person",
                  label = "?????????? ????????????????????????",
                  min = 5,
                  max = 1000000,
                  value = 120,
                  width = "350px"
                ),
                style = "font-family: B Mitra;font-weight: bold;text-align:center"
              ),
              column(6,
                ofset = 2,
                numericInput(
                  inputId = "DevCoef",
                  label = "???????? ??????????",
                  min = 1, max = 2, value = round(13 / 11, 2),
                  width = "350px"
                ),
                style = "font-family: B Mitra;font-weight: bold;text-align:center"
              ),
              column(12,
                ofset = 2,
                sliderInput(
                  inputId = "Interest",
                  label = "?????? ????????",
                  min = .01, max = .99, value = .12, step = .01,
                  width = "650px"
                )
              ),
              column(6,
                ofset = 2,
                selectInput(
                  inputId = "ProvinceDis",
                  label = "??????????",
                  choices = PROVINCE$province,
                  selected = PROVINCE$province[26]
                ),
                style = "font-family: B Mitra;font-weight: bold;text-align:center"
              ),
              column(6,
                ofset = 2,
                selectInput(
                  inputId = "JobDis",
                  label = "???????? ????????",
                  choices = JOBGROUP$JOB_GROUP,
                  selected = JOBGROUP$JOB_GROUP[2]
                ),
                style = "font-family: B Mitra;font-weight: bold;text-align:center"
              ),
              column(12,
                ofset = 2,
                sliderInput(
                  inputId = "DiscountRate",
                  label = "?????????? ??????????",
                  min = 0, max = .50, value = 0, step = .01,
                  width = "650px"
                ),
                style = "font-family: B Mitra;font-weight: bold;text-align:center"
              ),
              column(
                9,
                actionButton("run_PremiumCalc", "apply", class = "btn-info")
              ),
              column(
                12,
                style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
                withSpinner(dataTableOutput(outputId = "Yearly_Premium_Data"))
              )
            )
          ),
          mainPanel(
            hr(),
            column(
              12,
              withSpinner(dataTableOutput(outputId = "Premium_Cove_Data")),
              style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
            )
          )
        ),
        hr(),
        p(em("Developed by"), br(),
          a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
          style = "text-align:center;color:turquoise; font-size: 20px; font-family: times"
        )
      ),
      #-------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------

      tabPanel("Claims Reserving",
        fluid = TRUE, icon = icon("calendar", verify_fa = FALSE),
        tags$style(button_color_css),
        shinythemes::themeSelector(),
        sidebarLayout(
          sidebarPanel(
            h4("", style = "color:deeppink"),
            br(),
            fluidRow(
              column(
                6,
                h4("???????? ?????????? ???????? ?????????? ???? ???????????? ?? ???? ????????????", style = "font-family: B Mitra;font-weight: bold;font-size: 15px;text-align:left"),
                downloadButton("downloadIBNRTemplate")
              ),
              column(
                6,
                fileInput(
                  "fileIBNR",
                  h4("???????????????? ???????? ?????????? ???????? ??????", style = "font-family: B Mitra;font-weight: bold;font-size: 15px;text-align:center"),
                  accept = ".xlsx"
                )
              )
            ),
            fluidRow(
              column(
                6,
                h2("??????", style = "font-family: B Mitra;font-weight: bold;font-size: 20px;text-align:left"),
                
                selectInput(
                  inputId = "IBNR_Method",
                  label = "",
                  choices = c("Mack", "log-linear", "Random Forest"),
                  selected = "Mack",
                  width = "350px"
                )
               
              ),
              column(
                9,
                actionButton("run_IBNR", "apply", class = "btn-info")
              )
            )
          ),
          mainPanel(
            hr(),
            column(
              12,
              withSpinner(dataTableOutput(outputId = "Full_Triangle")),
              style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
            )
          )
        ),
        column(
          12,
          withSpinner(plotOutput(outputId = "Plot_Full_Triangle")),
          style = "font-family: B Mitra;font-size: 15px;font-weight: bold",
        ),
        hr(),
        p(em("Developed by"), br(),
          a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
          style = "text-align:center;color:turquoise; font-size: 20px; font-family: times"
        )
      ),




      #-------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------
      #-------------------------------------------------------------------------------

      # tabPanel("Other Apps",
      #          fluid = TRUE, icon = icon("tachometer", verify_fa = FALSE),
      #          tags$style(button_color_css),
      #          br(),
      #          br(),
      #          br(),
      #          fluidRow(
      #              column(
      #                  width = 4,
      #                  p("(???????????????????????????? ?????????????????????????????????? ???????????????????????????? ??????????????????????(????????????? ??????????????????????????", br(),
      #                    actionButton(
      #                        inputId = "ab1", label = "????????????????? ?????????????????",
      #                        icon = icon("brain"),
      #                        onclick = "window.open('https://budgetrealizationdayinsurance.shinyapps.io/Model_Predictive/', '_blank')"
      #                    ),
      #                    style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:turquoise;padding:15px;border-radius:10px"
      #                  )
      #              ),
      #              column(
      #                  width = 4,
      #                  p("???????????????????????????? ?????????????????? ?????????????????????", br(),
      #                    actionButton(
      #                        inputId = "ab1", label = "????????????????? ?????????????????",
      #                        icon = icon("coins"),
      #                        onclick = "window.open('https://gitypardazesh.shinyapps.io/Gity_Budget_Dashboard/', '_blank')"
      #                    ),
      #                    style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:thistle;padding:15px;border-radius:10px"
      #                  )
      #              ),
      #              column(
      #                  width = 4,
      #                  p("???????????????????????????? ?????????????????????????? ????????? ?????????????????? ?????????????????????? ?????????????????????????? ?????????????????????????", br(),
      #                    actionButton(
      #                        inputId = "ab1", label = "????????????????? ?????????????????",
      #                        icon = icon("hospital-user"),
      #                        onclick = "window.open('https://abbasdidar5017.shinyapps.io/RATING_DASHBOARD3/', '_blank')"
      #                    ),
      #                    style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:turquoise;padding:15px;border-radius:10px"
      #                  )
      #              )
      #          ),
      #          hr(),
      #          p(em("Developed by"), br(),
      #            a(href = "https://www.linkedin.com/in/abbasdidar", "Abbas Didar", target = "_blank"),
      #            style = "text-align:center; color:turquoise; font-size: 20px; font-family: times"
      #          )
      # )
    )
  )
)
