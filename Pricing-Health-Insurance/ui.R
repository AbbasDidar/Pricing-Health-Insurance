
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
                         label = "تعداد بیمه شده",
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
                              label = "استان",
                              choices =  sort( ostan$province ),
                              selected = ostan$province[10]
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(12,
                            ofset = 2,
                            sliderInput(
                              inputId = "Mean_Age",
                              label = "میانگین سن",
                              min = 0, max = 80, value = 45, step = 1,
                              width = "650px"
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(6,
                            ofset = 2,
                            numericInput(
                              inputId = "N_insured_60_70",
                              label = "تعداد افراد بین 60 تا 70 سال",
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
                              label = "تعداد افراد بیشتر از 70 سال",
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
                              label = "شبکه ارائه خدمات",
                              choices =  sort( servicenetwork$ServiceNetwork ),
                              selected = servicenetwork$ServiceNetwork[2]
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(6,
                            ofset = 2,
                            selectInput(
                              inputId = "NewJob_group",
                              label = "گروه شغلی",
                              choices = JobGroup$JOB_GROUP ,
                              selected = JobGroup$JOB_GROUP[4]
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(6,
                            ofset = 2,
                            selectInput(
                              inputId = "Type_ceiling",
                              label = "نوع سقف",
                              choices =  sort( typeceiling$TYPE_CEILING ),
                              selected = sort( typeceiling$TYPE_CEILING )[1]
                            ),
                            style = "color:darkred; font-family: B Mitra; font-size: 15px;text-align:center"
                     ),
                     column(12,
                            ofset = 2,
                            sliderInput(
                              inputId = "NewDiscountRate",
                              label = "تخفیف نهایی",
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
                   
                 
                   
                   h1("تعیین سقف پوشش - میلیون ریال", style ="color:purple; font-family: B Mitra; font-size: 35px; text-align:center"),
                   hr(),
                   br(),
                   
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "bastari",
                            label = "بستری",
                            value = 500 
                          ),
                          
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "j_takhasosi",
                            label = "جراحی تخصصی",
                            value = 1000 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "zayeman",
                            label = "زایمان",
                            value = 75 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "para",
                            label ="پاراکلینیکی",
                            value = 100 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
       
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "kh_az",
                            label = "خدمات آزمایشگاهی",
                            value = 80 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "dandan",
                            label =  "دندانپزشکی",
                            value = 100 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "eynak",
                            label = "عینك طبی و لنز" ,
                            value = 10 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "samak",
                            label = "سمعك" , 
                            value = 40 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   
                   
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "sarpa",
                            label = "اعمال مجاز سرپایی" , 
                            value = 100 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "visit",
                            label =  "ویزیت، دارو و خدمات اورژانس" ,
                            value = 100 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "badan",
                            label = "تهیه اعضای طبیعی بدن" , 
                            value = 500 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   column(3,
                          ofset = 2,
                          numericInput(
                            inputId = "erotez",
                            label =  "اروتز" ,
                            value = 20 
                          ),
                          style = "font-family: B Mitra; font-size: 15px;text-align:center"
                   ),
                   
                   #----------------------------------------------------------------------------------------------
               
                   h1("تعیین فرانشیز پوشش‌ها", style ="color:purple; font-family: B Mitra; font-size: 35px; text-align:center"),
                   hr(),
                   br(),
                   column(3,
                          ofset = 2,
                          sliderInput(
                            inputId = "f_bastari",
                            label ="بستری",
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
                            label = "جراحی تخصصی",
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
                            label =  "زایمان" , 
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
                            label =  "پاراکلینیکی" , 
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
                            label =  "خدمات آزمایشگاهی" , 
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
                            label = "دندانپزشکی" , 
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
                            label =  "عینك طبی و لنز" , 
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
                            label = "سمعك" , 
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
                            label =  "اعمال مجاز سرپایی" , 
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
                            label = "ویزیت، دارو و خدمات اورژانس" , 
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
                            label ="تهیه اعضای طبیعی بدن" , 
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
                            label =  "اروتز" ,
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
                h4("لطفا ابتدا فایل نمونه را دانلود و پر نمایید", style = "font-family: B Mitra;font-weight: bold;font-size: 15px;text-align:left"),
                downloadButton("downloadPremiumTemplate")
                
                
              ),
              column(
                6,
                fileInput(
                  "fileCOVERAGES",
                  h4("بارگذاری فایل نمونه کامل شده", style = "font-family: B Mitra;font-weight: bold;font-size: 15px;text-align:center"),
                  accept = ".xlsx"
                )
              )
            ),
            fluidRow(
              column(
                6,
                numericInput(
                  inputId = "N_person",
                  label = "تعداد بیمه‌شده‌ها",
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
                  label = "ضریب توسعه",
                  min = 1, max = 2, value = round(13 / 11, 2),
                  width = "350px"
                ),
                style = "font-family: B Mitra;font-weight: bold;text-align:center"
              ),
              column(12,
                ofset = 2,
                sliderInput(
                  inputId = "Interest",
                  label = "نرخ بهره",
                  min = .01, max = .99, value = .12, step = .01,
                  width = "650px"
                )
              ),
              column(6,
                ofset = 2,
                selectInput(
                  inputId = "ProvinceDis",
                  label = "استان",
                  choices = PROVINCE$province,
                  selected = PROVINCE$province[26]
                ),
                style = "font-family: B Mitra;font-weight: bold;text-align:center"
              ),
              column(6,
                ofset = 2,
                selectInput(
                  inputId = "JobDis",
                  label = "گروه شغلی",
                  choices = JOBGROUP$JOB_GROUP,
                  selected = JOBGROUP$JOB_GROUP[2]
                ),
                style = "font-family: B Mitra;font-weight: bold;text-align:center"
              ),
              column(12,
                ofset = 2,
                sliderInput(
                  inputId = "DiscountRate",
                  label = "تخفیف نهایی",
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
                h4("لطفا ابتدا فایل نمونه را دانلود و پر نمایید", style = "font-family: B Mitra;font-weight: bold;font-size: 15px;text-align:left"),
                downloadButton("downloadIBNRTemplate")
              ),
              column(
                6,
                fileInput(
                  "fileIBNR",
                  h4("بارگذاری فایل نمونه کامل شده", style = "font-family: B Mitra;font-weight: bold;font-size: 15px;text-align:center"),
                  accept = ".xlsx"
                )
              )
            ),
            fluidRow(
              column(
                6,
                h2("مدل", style = "font-family: B Mitra;font-weight: bold;font-size: 20px;text-align:left"),
                
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
      #                  p("(ط¯ط§ط´ط¨ظˆط±ط¯ ظ…ط¯ظ„â€Œظ‡ط§غŒ غŒط§ط¯ع¯غŒط±غŒ ظ…ط§ط´غŒظ†(ظ‡ظˆط´ ظ…طµظ†ظˆط¹غŒ", br(),
      #                    actionButton(
      #                        inputId = "ab1", label = "ع©ظ„غŒع© ع©ظ†غŒط¯",
      #                        icon = icon("brain"),
      #                        onclick = "window.open('https://budgetrealizationdayinsurance.shinyapps.io/Model_Predictive/', '_blank')"
      #                    ),
      #                    style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:turquoise;padding:15px;border-radius:10px"
      #                  )
      #              ),
      #              column(
      #                  width = 4,
      #                  p("ط¯ط§ط´ط¨ظˆط±ط¯ طھط­ظ‚ظ‚ ط¨ظˆط¯ط¬ظ‡", br(),
      #                    actionButton(
      #                        inputId = "ab1", label = "ع©ظ„غŒع© ع©ظ†غŒط¯",
      #                        icon = icon("coins"),
      #                        onclick = "window.open('https://gitypardazesh.shinyapps.io/Gity_Budget_Dashboard/', '_blank')"
      #                    ),
      #                    style = "font-size: 25px;text-align:center;font-family: B Mitra;color:black;background-color:thistle;padding:15px;border-radius:10px"
      #                  )
      #              ),
      #              column(
      #                  width = 4,
      #                  p("ط¯ط§ط´ط¨ظˆط±ط¯ ظ…ط­ط§ط³ط¨ظ‡ ط­ظ‚ ط¨غŒظ…ظ‡ ط¯ط±ظ…ط§ظ† ط³ط§ط²ظ…ط§ظ† ط®ط¯ظ…ط§طھغŒ", br(),
      #                    actionButton(
      #                        inputId = "ab1", label = "ع©ظ„غŒع© ع©ظ†غŒط¯",
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
