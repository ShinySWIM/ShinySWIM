
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
                      h3("Welcome to the", strong("ShinySWIM"), "app!",
                         style = "text-align:center"),
                      h4(a(strong("SWIM"), href = "https://CRAN.R-project.org/package=SWIM", target = "_blank"),
                         "Scenario Weights for Importance Measurement "),
                      p("This",
                        a(strong("Shiny"), href = "https://shiny.rstudio.com/", target = "_blank"),
                        "app provides an easy-to-use interface with the R package",
                        a(strong("SWIM"), href = "https://CRAN.R-project.org/package=SWIM", target = "_blank"),
                        "which implements a flexible sensitivity analysis framework, based primarily
                            on results and tools developed by Pesenti et al. (2019).",
                        br(),
                         a(strong("SWIM"), href = "https://CRAN.R-project.org/package=SWIM", target = "_blank"), 
                         "provides a stressed version of a stochastic model, subject to model components (random variables) fulfilling given probabilistic
                         constraints (stresses). Possible stresses can be applied on moments (first and second)
                         and risk measures such as Value-at-Risk and Expected Shortfall.",
                         a(strong("SWIM"), href = "https://CRAN.R-project.org/package=SWIM", target = "_blank"), 
                         "operates upon a single
                         set of simulated scenarios from a stochastic model, returning scenario weights, which encode
                         the required stress and allow monitoring the impact of the stress on all model components. The
                         scenario weights are calculated to minimise the relative entropy with respect to the baseline model,
                         subject to the stress applied. As well as calculating scenario weights, the package provides tools for
                         the analysis of stressed models, including plotting facilities and evaluation of sensitivity measures.",
                         a(strong("SWIM"), href = "https://CRAN.R-project.org/package=SWIM", target = "_blank"),
                         " does not require additional evaluations of the simulation model or explicit knowledge of
                         its underlying statistical and functional relations; hence it is suitable for the analysis of black box
                         models."),
                      br(),
                      h4("The",
                         a(strong("SWIM"), href = "https://CRAN.R-project.org/package=SWIM", target = "_blank"),
                         "Approach"),
                         "1. The starting point is a table of simulated scenarios, each column containing realisations of a
                         different model component. This table forms the baseline model as well as the dataset on which",
                      a(strong("SWIM"), href = "https://CRAN.R-project.org/package=SWIM", target = "_blank"),
                         "bases its calculations.",
                      br(),
                         "2. A stress is defined as a particular modification of a model component.
                         This could relate to a change in moments or risk measures,
                         such as Value-at-Risk or Expected Shortfall",
                      br(),
                         "3.",
                      a(strong("SWIM"), href = "https://CRAN.R-project.org/package=SWIM", target = "_blank"), 
                      "calculates a set of scenario weights, acting upon the simulated scenarios and thus modifying
                         the relative probabilities of scenarios occurring. Scenario weights are derived such that
                         the defined stress on model components is fulfilled, while keeping the distortion to the baseline
                         model to a minimum, as quantified by the Kullback-Leibler divergence (relative entropy).",
                      br(),
                         "4. Given the calculated scenario weights, the impact of the stress on the distributions of all model
                         components is worked out and sensitivity measures, useful for ranking model components, are
                         evaluated.",
                      br(),
                      br(),
                      h4("ShinySWIM in a Nutshell"),
                      p("1. Data: upload your Monte Carlo simulations.",
                      br(),
                      "2.  Stress: run stresses on one of the model components, e.g., mean, VaR, ES.",
                      br(),
                      "3. Stress Comparison: calculate statistics under different stresses.",
                      br(),
                      "4. Sensitivity Comparison: calculate sensitivity measures for different stresses."),
                      br(),
                      p("For more information on",
                        a(strong("SWIM"), href = "https://CRAN.R-project.org/package=SWIM", target = "_blank"),
                        "and its capabilities, including the design of custom stresses not implemented in this app, see ",
                      a(strong("SWIM Vignette"), href = "https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html", target = "_blank"), 
                      ". For feedback or for letting us know how you use SWIM in practice, please ",a(strong("email us"), href = "mailto:swimpackage@gmail.com"), ". Furthermore, if you would like to receive news and updates about SWIM, please complete", a(strong("this form"), href = "mailto:swimpackage@gmail.com?subject=Keep me updated with ShinySwim&body=Hi all,%0D%0A%0D%0AMy name is XXX and I would to be kept informed about news and developements of ShinySWIM.%0D%0A%0D%0AThanks%0D%0A%0D%0ANAME", target = "_blank"),".")
               )
           )
  )),
  tabPanel("Data", 
           sidebarLayout(
             sidebarPanel(width = 4,
                          h3("Input data"),
                          p("The authors of ShinySWIM do not store your data, which are deleted at the end of your session.", strong("However, we strongly recommend that you do not upload any data that are confidential or have not been anonymised.")),
                          
                          hr(),
                          h4("Upload a dataset"),
                          helpText(
                            p("Upload a file (*.csv, *.txt) containing",
                              "your own dataset (the default 'credit_data' of the SWIM package has been pre-uploaded for demonstration purposes). A preview of the dataset will be shown in the main",
                              "panel on the right."),
                          ),
                          fileInput("file_upload", "Choose file:",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values",
                                               "text/plain",
                                               ".csv",
                                               ".txt",
                                               ".dat"),
                                    buttonLabel = "Browse ..."),
                          hidden(
                            actionButton("reload", "Change Data",
                                         style = "color: black; background-color: #337ab7 ;border-color: black")
                            ),
                          hidden(textOutput(("file_upload_text"))),
                          
                          hr(),
                          strong("Header:"),
                          checkboxInput("header", "The file has a header containing the column names", TRUE),
                          radioButtons("sep", "Separator symbol:",
                                       choices = c("Comma" = ",",
                                                   "Semicolon" = ";",
                                                   "Tab" = "\t",
                                                   "Whitespace" = "")),
                          radioButtons("quote", "Quote symbol:",
                                       choices = c("None" = "",
                                                   "Double quote" = '"',
                                                   "Single quote" = "'"),
                                       selected = '"'),
                          radioButtons("dec", "Decimal symbol:",
                                       choices = c("Point" = ".",
                                                   "Comma" = ","))
             ),
             mainPanel(
               wellPanel(
                         style="padding:5px",
                         fluidRow(
                           column(width = 1, offset = 0,style = 'margin-top:5px',
                                  dropdownButton(
                                    size = "default",
                                    inputId = "mydropdowndigits1",
                                    label = "Preview Format",
                                    status = "primary",
                                    circle = FALSE,
                                    h4("Preview"),
                                    radioButtons("preview_type_radio", "Type",
                                                 choices = c("Dataset" = "datas",
                                                             "Structure" = "struc")),
                                    numericInput("table_digits0",label = "Number of digits",value = 2,min = 0,max=10,step = 1)
                                    )),
                           column(width = 1, offset = 10,
                                    dropdownButton(right = TRUE,
                                    size = "default",
                                    inputId = "mydropdownhelp1",
                                    label = NULL,
                                    icon = icon("question"),
                                    status = "info",
                                    circle = TRUE,
                                    h5("Help"),
                                    HTML('<a href="https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html" target="_blank">SWIM Vignette</a>'),
                                    HTML('<a href="https://cran.r-project.org/web/packages/SWIM/SWIM.pdf" target="_blank">SWIM Manual</a>'))
                                  )
                           )
               ),
               h4(textOutput("dataname")),
               conditionalPanel(
                 condition = "input.preview_type_radio == 'datas'",
                 DTOutput("da_view")
               ),
               conditionalPanel(
                 condition = "input.preview_type_radio == 'struc'",
                 verbatimTextOutput("da_str", placeholder = FALSE)
               )
             )
           )
  ),
  tabPanel("Stress",
           navlistPanel(widths = c(2, 10),"Stresses",
                        tabPanel("Stress 1",
                                 sidebarLayout(
                                   sidebarPanel(width = 4,
                                                h3("Stress 1"),
                                                helpText(
                                                p("Select the first stress to be applied to the data. 
                                                Possible stresses can be applied on risk measures (stress_VaR and stress_VaR_ES) or moments (stress_mean and stress_mean_sd).
                                                After choosing the arguments, press the run button and a comparison between the baseline and the stressed scenario will be displayed in the right panel."),
                                                ),
                                                hr(),
                                                selectInput("stressFunction1", 
                                                            "Stress function", c(
                                                              "stress_VaR",
                                                              "stress_VaR_ES",
                                                              "stress_mean",
                                                              "stress_mean_sd"
                                                            )
                                                ),
                                                textOutput(("selected_stress1")),
                                                hr(tags$style(HTML("hr {border-top: 1px solid #b0b0b0;}"))),
                                                # Argument selector:
                                                uiOutput("condPanels1"),
                                                hr(),
                                                fluidRow(column(3,
                                                                actionButton("run_stress1", "Run Stress",icon("play")
                                                                             )),
                                                         div(
                                                           style="padding:10px",
                                                           column(9,
                                                                  textOutput(("status1")))))),
                                   mainPanel(width = 8,
                                             wellPanel(
                                               style="padding:5px",
                                               fluidRow(
                                                 column(width = 1, offset = 0,style = 'margin-top:5px',
                                                        dropdownButton(
                                                          size = "default",
                                                          inputId = "mydropdowndigits1",
                                                          label = "Summary Format",
                                                          status = "primary",
                                                          circle = FALSE,
                                                          numericInput("table_digits1",label = "Number of digits",value = 2,min = 0,max=10,step = 1)
                                                        )),
                                                 column(width = 1, offset = 9,
                                                        dropdownButton(right=TRUE,
                                                          size = "default",
                                                          inputId = "mydropdown1",
                                                          label = NULL,
                                                          icon = icon("download"),
                                                          status = "primary",
                                                          circle = TRUE,
                                                          h5("Download Summary"),
                                                          radioButtons("download_stress1"
                                                                       ,label = "Stress",choices = c(Baseline="Baseline")),
                                                          radioButtons("download_type1"
                                                                       ,label = "Format",choices = c(csv="csv",xlsx="xlsx",txt="txt")),
                                                          downloadButton("dwn_btn1","Download",status="primary")
                                                        )),
                                                 column(width = 1, offset = 0,
                                                        dropdownButton(right = TRUE,
                                                                       size = "default",
                                                                       inputId = "mydropdownhelp2",
                                                                       label = NULL,
                                                                       icon = icon("question"),
                                                                       status = "info",
                                                                       circle = TRUE,
                                                                       h5("Help"),
                                                                       HTML('<a href="https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html" target="_blank">SWIM Vignette</a>'),
                                                                       HTML('<a href="https://cran.r-project.org/web/packages/SWIM/SWIM.pdf" target="_blank">SWIM Manual</a>'))
                                                 )
                                               )
                                             ),
                                              
                                             DTOutput("SummaryStressBase1"),
                                             br(),
                                             DTOutput("SummaryStress1"),
                                             br(),
                                             ))
                        ),
                        tabPanel("Stress 2",
                                 sidebarLayout(
                                   sidebarPanel(width = 4,
                                                h3("Stress 2"),
                                                textOutput("text2"),
                                                hidden(selectInput("stressFunction2", 
                                                                   "Stress function", c(
                                                                     "stress_VaR",
                                                                     "stress_VaR_ES",
                                                                     "stress_mean",
                                                                     "stress_mean_sd"
                                                                   ))
                                                ),
                                                hidden(textOutput(("selected_stress2"))),
                                                hr(tags$style(HTML("hr {border-top: 1px solid #b0b0b0;}"))),
                                                # Argument selector:
                                                hidden(uiOutput("condPanels2")),
                                                hr(),
                                                hidden(actionButton("run_stress2", "Run Stress",icon("play"))))
                                   ,mainPanel(width = 8,
                                              wellPanel(
                                                style="padding:5px",
                                                fluidRow(
                                                  column(width = 1, offset = 0,style = 'margin-top:5px',
                                                         dropdownButton(
                                                           size = "default",
                                                           inputId = "mydropdowndigits2",
                                                           label = "Summary Format",
                                                           status = "primary",
                                                           circle = FALSE,
                                                           numericInput("table_digits2",label = "Number of digits",value = 2,min = 0,max=10,step = 1)
                                                         )),
                                                  column(width = 1, offset = 9,
                                                         dropdownButton(right=TRUE,
                                                                        size = "default",
                                                                        inputId = "mydropdown2",
                                                                        label = NULL,
                                                                        icon = icon("download"),
                                                                        status = "primary",
                                                                        circle = TRUE,
                                                                        h5("Download Summary"),
                                                                        radioButtons("download_stress2"
                                                                                     ,label = "Stress",choices = c(Baseline="Baseline")),
                                                                        radioButtons("download_type2"
                                                                                     ,label = "Format",choices = c(csv="csv",xlsx="xlsx",txt="txt")),
                                                                        downloadButton("dwn_btn2","Download",status="primary")
                                                         )),
                                                  column(width = 1, offset = 0,
                                                         dropdownButton(right = TRUE,
                                                                        size = "default",
                                                                        inputId = "mydropdownhelp3",
                                                                        label = NULL,
                                                                        icon = icon("question"),
                                                                        status = "info",
                                                                        circle = TRUE,
                                                                        h5("Help"),
                                                                        HTML('<a href="https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html" target="_blank">SWIM Vignette</a>'),
                                                                        HTML('<a href="https://cran.r-project.org/web/packages/SWIM/SWIM.pdf" target="_blank">SWIM Manual</a>'))
                                                  )
                                                )
                                              ),
                                              DTOutput("SummaryStressBase2"),
                                              br(),
                                              DTOutput("SummaryStress2"),
                                              br(),
                                   )
                                   )
                        ),
                        tabPanel("Stress 3",
                                 sidebarLayout(
                                   sidebarPanel(width = 4,
                                                h3("Stress 3"),
                                                textOutput("text3"),
                                                hidden(selectInput("stressFunction3", 
                                                                   "Stress function", c(
                                                                     "stress_VaR",
                                                                     "stress_VaR_ES",
                                                                     "stress_mean",
                                                                     "stress_mean_sd"
                                                                   ))
                                                ),
                                                hidden(textOutput(("selected_stress3"))),
                                                hr(tags$style(HTML("hr {border-top: 1px solid #b0b0b0;}"))),
                                                # Argument selector:
                                                hidden(uiOutput("condPanels3")),
                                                hr(),
                                                hidden(actionButton("run_stress3", "Run Stress",icon("play"))))
                                   ,mainPanel(width = 8,
                                              wellPanel(
                                                style="padding:5px",
                                                fluidRow(
                                                  column(width = 1, offset = 0,style = 'margin-top:5px',
                                                         dropdownButton(
                                                           size = "default",
                                                           inputId = "mydropdowndigits3",
                                                           label = "Summary Format",
                                                           status = "primary",
                                                           circle = FALSE,
                                                           numericInput("table_digits3",label = "Number of digits",value = 2,min = 0,max=10,step = 1)
                                                         )),
                                                  column(width = 1, offset = 9,
                                                         dropdownButton(right=TRUE,
                                                                        size = "default",
                                                                        inputId = "mydropdown3",
                                                                        label = NULL,
                                                                        icon = icon("download"),
                                                                        status = "primary",
                                                                        circle = TRUE,
                                                                        h5("Download Summary"),
                                                                        radioButtons("download_stress3"
                                                                                     ,label = "Stress",choices = c(Baseline="Baseline")),
                                                                        radioButtons("download_type3"
                                                                                     ,label = "Format",choices = c(csv="csv",xlsx="xlsx",txt="txt")),
                                                                        downloadButton("dwn_btn3","Download",status="primary")
                                                         )),
                                                  column(width = 1, offset = 0,
                                                         dropdownButton(right = TRUE,
                                                                        size = "default",
                                                                        inputId = "mydropdownhelp4",
                                                                        label = NULL,
                                                                        icon = icon("question"),
                                                                        status = "info",
                                                                        circle = TRUE,
                                                                        h5("Help"),
                                                                        HTML('<a href="https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html" target="_blank">SWIM Vignette</a>'),
                                                                        HTML('<a href="https://cran.r-project.org/web/packages/SWIM/SWIM.pdf" target="_blank">SWIM Manual</a>'))
                                                  )
                                                )
                                              ),
                                              DTOutput("SummaryStressBase3"),
                                              br(),
                                              DTOutput("SummaryStress3"),
                                              br(),
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
                                    h3("Control Panel"),
                                    helpText(
                                      p("To analyse the effect of different stresses on the selected variable. In the right panels the comparison is performed both through a plot and a set of statistics."),
                                    ),
                                    hr(),
                                    uiOutput("plotVariable"),
                                    hr(),
                                    h4("Plot options"),
                                    selectInput("plotFunction", 
                                                "Select plot", c(
                                                  "Histogram",
                                                  "CDF",
                                                  "Quantiles",
                                                  "Scenario Weights"
                                                )
                                    ),
                                    fluidRow(column(6,checkboxGroupInput("plotStress1", label = h5("Select stress"), 
                                                                         choices = list("Stress 1" = 1),
                                                                         selected = 1)),
                                             column(6,h5("Add"),
                                                    checkboxInput("plotBase","Baseline",value = TRUE))),
                                    uiOutput("dynamic_slider_x"),
                                    uiOutput("dynamic_slider_y"),
                                    hr(),
                                    h4("Risk Measures"),
                          sliderInput("sliderVaR", label = "VaR level", min = 0.05,width = "100%",max = 0.995, value= 0.9 , round = -2,step = 0.005,ticks = TRUE),
                          sliderInput("sliderES", label = "ES level", min = 0.05,width = "100%",max = 0.995, value= 0.9 , round = -2,step = 0.005,ticks = TRUE))),      
             mainPanel(width = 8,
                       wellPanel(
                         style="padding:5px",
                         fluidRow(
                           column(width = 1, offset = 0,style = 'margin-top:5px',
                                  dropdownButton(
                                    size = "default",
                                    inputId = "mydropdowndigitsX",
                                    label = "Summary Format",
                                    status = "primary",
                                    circle = FALSE,
                                    numericInput("table_digits_comparison",label = "Number of digits",value = 2,min = 0,max=10,step = 1)
                                  )),
                           column(width = 1, offset = 9,
                                  dropdownButton(right=TRUE,
                                                 size = "default",
                                                 inputId = "mydropdownX",
                                                 label = NULL,
                                                 icon = icon("download"),
                                                 status = "primary",
                                                 circle = TRUE,
                                                 h4("Plot"),
                                                 radioButtons("download_plot_format1"
                                                              ,label = "Format",choices = c(png="png",jpg="jpg")),
                                                 sliderInput("slider_height1", label = "Height (px)", min = 1000,
                                                             max = 3000, value=2000),
                                                 sliderInput("slider_width1", label = "Width (px)", min = 1000,
                                                             max = 3000, value=2400),
                                                 downloadButton("dwn_btn_plot1","Download plot",status="primary"),
                                                 hr(),
                                                 h4("Summary"),
                                                 radioButtons("download_typeX"
                                                              ,label = "Format",choices = c(csv="csv",xlsx="xlsx",txt="txt")),
                                                 downloadButton("dwn_btnY","Download summary",status="primary"),
                                                 hr(),
                                                 h4("Risk Measures"),
                                                 radioButtons("download_typeXXX"
                                                              ,label = "Format",choices = c(csv="csv",xlsx="xlsx",txt="txt")),
                                                 downloadButton("dwn_btnXXX","Download risk measures",status="primary")
                                  )),
                           column(width = 1, offset = 0,
                                  dropdownButton(right = TRUE,
                                                 size = "default",
                                                 inputId = "mydropdownhelpX",
                                                 label = NULL,
                                                 icon = icon("question"),
                                                 status = "info",
                                                 circle = TRUE,
                                                 h5("Help"),
                                                 HTML('<a href="https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html" target="_blank">SWIM Vignette</a>'),
                                                 HTML('<a href="https://cran.r-project.org/web/packages/SWIM/SWIM.pdf" target="_blank">SWIM Manual</a>'))
                           )
                         )
                       ),
                       wellPanel(style = "background-color: #ffffff;",
                                 withSpinner(plotOutput("plot",width = "100%",height = "600px"),color="#0dc5c1")),
                       wellPanel(style = "background-color: #ffffff;",
                                 DTOutput("comparisonfigures")),
                       fluidRow(
                         column(width=6,
                       wellPanel(style = "background-color: #ffffff;",
                                 DTOutput("comparisonRM"))
                                 ))
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
                                    h3("Control Panel"),
                                    helpText(
                                      p("To select the sensitivity measure used to compare the effects of the stresses on different model components"),
                                    ),
                                    hr(),
                                    selectInput(inputId = "sens_function",label = h5("Sensitivity measure"),width = "100%",choices = c("Gamma","Wasserstein"),selected = "Gamma"),
                                    textOutput(("selected_sens_measures")),
                                    hr(),
                                    uiOutput("plotVariable2"),
                                    hr(),
                                    fluidRow(column(6,checkboxGroupInput("plotStress2", label = h5("Select stress"), 
                                                                         choices = list("Stress 1" = 1),
                                                                         selected = 1))
                                    )
                                    )),
             mainPanel(width = 8,
                       wellPanel(
                         style="padding:5px",
                         fluidRow(
                           column(width = 1, offset = 10,
                                  dropdownButton(right=TRUE,
                                                 size = "default",
                                                 inputId = "mydropdownZ",
                                                 label = NULL,
                                                 icon = icon("download"),
                                                 status = "primary",
                                                 circle = TRUE,
                                                 h4("Plot"),
                                                 radioButtons("download_plot_format2"
                                                              ,label = "Format",choices = c(png="png",jpg="jpg")),
                                                 sliderInput("slider_height2", label = "Height (px)", min = 1000,
                                                             max = 3000, value=2000),
                                                 sliderInput("slider_width2", label = "Width (px)", min = 1000,
                                                             max = 3000, value=2400),
                                                 downloadButton("dwn_btn_plot2","Download plot",status="primary"),
                                                 hr(),
                                                 h4("Ranking"),
                                                 radioButtons("download_typeZ"
                                                              ,label = "Format",choices = c(csv="csv",xlsx="xlsx",txt="txt")),
                                                 downloadButton("dwn_btnZ","Download ranking",status="primary")
                                  )),
                           column(width = 1, offset = 0,
                                  dropdownButton(right = TRUE,
                                                 size = "default",
                                                 inputId = "mydropdownhelpZ",
                                                 label = NULL,
                                                 icon = icon("question"),
                                                 status = "info",
                                                 circle = TRUE,
                                                 h5("Help"),
                                                 HTML('<a href="https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html" target="_blank">SWIM Vignette</a>'),
                                                 HTML('<a href="https://cran.r-project.org/web/packages/SWIM/SWIM.pdf" target="_blank">SWIM Manual</a>'))
                           )
                         )
                       ),
                       
                       wellPanel(style = "background-color: #ffffff;",
                                 #  h4("Plot Comparison"),
                                 withSpinner(plotOutput("plotSens",width = "100%"),color="#0dc5c1")),
                       wellPanel(style = "background-color: #ffffff;",
                                 #     h4("Numerical Comparison"),
                                 DTOutput("importance_rank"))
             )
           )
           
  ),
  navbarMenu("More",
             tabPanel("Credit Data",
                      fluidRow(
                        div(
                          style = "position: relative", 
                          column(width = 12,
                                 #style = "background-color:#FFFFFF;",
                                 h3("Credit Data"),
                                 
                                 p("The credit model
                                    is a conditionally binomial loan portfolio model, including systematic and specific
                                    portfolio risk. A key variable of interest is the total aggregate portfolio loss L = L1+L2+L3, where
                                    L1,L2,L3 are homogeneous subportfolios on a comparable scale (say, thousands of $). The dataset
                                    contains 100,000 simulations of the portfolio L, the subportfolios L1,L2,L3 as well as the random
                                    default probabilities within each subportfolio, H1,H2,H3. These default probabilities represent the
                                    systematic risk within each subportfolio, while their dependence structure represents a systematic risk
                                    effect between the subportfolios. We may thus think of L as the model output, H1,H2,H3 as model
                                    inputs, and L1,L2,L3 as intermediate model outputs."),
                                   br(),
                                p("We refer to the Appendix A of the",
                                a(strong("SWIM Vignette"), href = "https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html", target = "_blank"), 
                                "for details about the model and the generation of the simulated data.")
                                   
                                 
                          )
                        )
                      )
             ),
             tabPanel("About",
                      fluidRow(
                        div(
                          style = "position: relative", 
                          column(width = 12,
                                   h3("Basic Information"),
                                   p(strong("Version:"),"0.9.0"),
                                   p(strong("Authors:"),"Alberto Bettini, Silvana M. Pesenti, Pietro Millossovich, Andreas Tsanakas"),
                                   p(strong("Contributors:"),"Markus Gesmann"),
                                   p(strong("Date:"),"October 10, 2022"),
                                 
                                 h3("Citation"),
                                   p("If you use this package, please use the following citation information:",
                                     br(),
"Pesenti, Silvana M., et al. Scenario Weights for Importance Measurement (SWIM)-an R package for sensitivity analysis. Annals of Actuarial Science 15.2 (2021): 458-483.",
br(),
br(),
 "A BibTeX entry for LaTeX user is:", 
br(),
"@article{pesenti2021scenario,

  title={Scenario Weights for Importance Measurement (SWIM)--an R package for sensitivity analysis},

  author={Pesenti, Silvana M and Bettini, Alberto and Millossovich, Pietro and Tsanakas, Andreas},

  journal={Annals of Actuarial Science},

  volume={15},

  number={2},

  pages={458--483},

  year={2021},

  publisher={Cambridge University Press}

}"),
                                   h3("Issues"),
                                   p("If you need help or suggest improvements please open an issue on",a(strong("Github"), href = "https://github.com/ShinySWIM/ShinySWIM/issues", target = "_blank")),
br(),
br(),
p("Shiny: ShinySWIM is powered by the Shiny web application framework (RStudio)")
                          )
                        )
                      )
             ),
             tabPanel("Terms of Service",
                      fluidRow(
                        div(
                          style = "position: relative", 
                          column(width = 12,
                                h3("Terms of service"),
                                p("The materials on this website are provided “as is”. The authors of ShinySWIM make no warranties,
                                  expressed or implied, and hereby disclaim and negate all other warranties, including without limitation, 
                                  implied warranties or conditions of merchantability, fitness for a particular purpose, or non-infringement 
                                  of intellectual property or other violation of rights. Further, the authors of ShinySWIM do not warrant or 
                                  make any representations concerning the accuracy, likely results, or reliability of the use of the materials 
                                  on the ShinySWIM website or otherwise relating to such materials or on any sites linked to this site.")
                          )
                        )
                      )
             )
  )
)

