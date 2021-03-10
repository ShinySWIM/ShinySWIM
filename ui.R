
ui <- navbarPage(
  
  #shiny theme
  theme = shinytheme("cerulean"),
  
  #title
  title =  a("SWIM",
           height = 40,
           href = "https://CRAN.R-project.org/package=SWIM",
           target = "_blank",
           style = "color: white"
         ),
    
    # MAIN BODY ---------------------------------------------------------------
    
   # tabsetPanel(
      tabPanel("Home", icon = icon("home"),  
               useShinyjs(),
               introjsUI(),
               fluidRow(
                 div(
                   style = "position: relative", 
                   column(width = 12,
                          #style = "background-color:#FFFFFF;",
                          includeMarkdown("www/Getting_started.md")
                   )
                 )
               )
      ),
      tabPanel("Data", 
               sidebarLayout(
                 sidebarPanel(width = 4,
                        h3("Input data"),
                        selectInput("readFunction", 
                                    "Function to read data:", c(
                                      # Base R:
                                      "read.csv",
                                      "read.csv2",
                                      "read.table",
                                      "readWorkbook"
                                    )),
                        
                        # Argument selector:
                        htmlOutput("ArgSelect"),
                        
                        # Argument field:
                        htmlOutput("ArgText"),
                        # Upload data:
                        fileInput("user_data_file", "Upload data-file:")),
                 mainPanel(width = 8,
                        h3("Review data"),
                        h4(textOutput("dataname")),
                        p("First 6 rows:"),
                        tableOutput("headtable"),
                         p("Selected quantiles:"),
                        tableOutput("quantiletable"))
                 )
               ),
  tabPanel("Stress",
  navlistPanel(widths = c(2, 10),"Stresses",
      tabPanel("Stress 1",
               sidebarLayout(
                 sidebarPanel(width = 4,
                          h3("Stress 1"),
                          selectInput("stressFunction1", 
                                      "Select function to stress simulations:", c(
                                        "stress_VaR",
                                        "stress_VaR_ES",
                                        "stress_mean",
                                        "stress_mean_sd"
                                      )
                                      ),
                          hr(tags$style(HTML("hr {border-top: 1px solid #b0b0b0;}"))),
                          # Argument selector:
                          uiOutput("condPanels1"),
                          hr(),
                          fluidRow(column(3,
                                          actionButton("run_stress1", "Run Stress",
                                                       style = "color: black; background-color: #337ab7 ;border-color: black")),
                                   div(
                                     style="padding:10px",
                                            column(9,
                                          textOutput(("status1")))))),
                 mainPanel(width = 8,
                          DTOutput("SummaryStressBase1"),
                          br(),
                          DTOutput("SummaryStress1"),
                          br(),
                          hidden(numericInput("table_digits1",label = "Digits",value = 2,min = 0,max=10,step = 1,width = "10%"))
                          ))
      ),
  tabPanel("Stress 2",
           sidebarLayout(
             sidebarPanel(width = 4,
                          h3("Stress 2"),
                          textOutput("text2"),
                          hidden(selectInput("stressFunction2", 
                                                        "Select function to stress simulations:", c(
                                                          "stress_VaR",
                                                          "stress_VaR_ES",
                                                          "stress_mean",
                                                          "stress_mean_sd"
                                                        ))
                          ),
                          hr(tags$style(HTML("hr {border-top: 1px solid #b0b0b0;}"))),
                          # Argument selector:
                          hidden(uiOutput("condPanels2")),
                          hr(),
                          hidden(actionButton("run_stress2", "Run Stress")))
             ,mainPanel(width = 8,
                        DTOutput("SummaryStressBase2"),
                        br(),
                        DTOutput("SummaryStress2"),
                        br(),
                        hidden(numericInput("table_digits2",label = "Digits",value = 2,min = 0,max=10,step = 1,width = "15%"))
             ))
  ),
  tabPanel("Stress 3",
           sidebarLayout(
             sidebarPanel(width = 4,
                          h3("Stress 3"),
                         textOutput("text3"),
                         hidden(selectInput("stressFunction3", 
                                                        "Select function to stress simulations:", c(
                                                          "stress_VaR",
                                                          "stress_VaR_ES",
                                                          "stress_mean",
                                                          "stress_mean_sd"
                                                        ))
                          ),
                          hr(tags$style(HTML("hr {border-top: 1px solid #b0b0b0;}"))),
                          # Argument selector:
                          hidden(uiOutput("condPanels3")),
                          hr(),
                          hidden(actionButton("run_stress3", "Run Stress")))
             ,mainPanel(width = 8,
                        DTOutput("SummaryStressBase3"),
                        br(),
                        DTOutput("SummaryStress3"),
                        br(),
                        hidden(numericInput("table_digits3",label = "Digits",value = 2,min = 0,max=10,step = 1,width = "15%"))
             ))
  ))
  ),
      tabPanel("Stress Comparison", 
               sidebarLayout(
                 sidebarPanel(width = 4,
                              wellPanel(
                                style = "background-color: #ffffff;",
                                DTOutput("summary_stress1")
                                ),
                              wellPanel(style = "background-color: #ffffff;",
                          h4("Control Panel"),
                          uiOutput("plotVariable"),
                          selectInput("plotFunction", 
                                      "Select plot", c(
                                        "plot_hist",
                                        "plot_cdf",
                                        "plot_quantile",
                                        "plot_weights"
                                      )
                                      ),
                          hr(),
                         fluidRow(column(6,checkboxGroupInput("plotStress1", label = h5("Select stress"), 
                                             choices = list("Stress 1" = 1),
                                             selected = 1)),
                         column(6,h5("Add"),
                         checkboxInput("plotBase","Baseline",value = TRUE))),
                        hr(),
                          uiOutput("dynamic_slider_x"),
                          uiOutput("dynamic_slider_y"))),
                 mainPanel(width = 8,
                        wellPanel(style = "background-color: #ffffff;",
                                #  h4("Plot Comparison"),
                         withSpinner(plotOutput("plot",width = "100%",height = "600px"),color="#0dc5c1")),
                        wellPanel(style = "background-color: #ffffff;",
                             #     h4("Numerical Comparison"),
                             DTOutput("comparisonfigures"))
                         )
                 )
      ),
  
 tabPanel("Sensitivity Comparison", 
          sidebarLayout(
            sidebarPanel(width = 4,
                         wellPanel(
                           style = "background-color: #ffffff;",
                           DTOutput("summary_stress2")
                         ),
                         wellPanel(style = "background-color: #ffffff;",
                                   h4("Control Panel"),
                                   selectInput(inputId = "sens_function",label = h5("Sensitivity measure"),width = "100%",choices = c("Gamma","Kolmogorov","Wasserstein"),selected = "Gamma"),
                                   hr(),
                                   fluidRow(column(6,checkboxGroupInput("plotStress2", label = h5("Select stress"), 
                                                                        choices = list("Stress 1" = 1),
                                                                        selected = 1))
                                            ))),
            mainPanel(width = 8,
                      fluidRow(
                        column(8,
                      wellPanel(style = "background-color: #ffffff;",
                        h4("Plot Sensitivity Comparison"),
                      withSpinner(plotOutput("plotSens",width = "100%"),color="#0dc5c1"))),
                      column(4,
                      wellPanel(style = "background-color: #ffffff;",
                                h4("Importance Ranking"),
                             tableOutput("importance_rank")))))
            )
          
 ),
  navbarMenu("More",
    tabPanel("Disclaimer",
               fluidRow(
                 div(
                   style = "position: relative", 
                   column(width = 12,
                          style = "background-color:#FFFFFF;",
                          includeMarkdown("www/Disclaimer.md")
                   )
                 )
               )
      ),
      tabPanel("Terms of Service",
               fluidRow(
                 div(
                   style = "position: relative", 
                   column(width = 12,
                          style = "background-color:#FFFFFF;",
                          includeMarkdown("www/TermsOfService.md")
                   )
                 )
               )
      )
  )
    )


