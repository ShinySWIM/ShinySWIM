
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
                      ".")
               )
           )
  )),
  tabPanel("Data", 
           sidebarLayout(
             sidebarPanel(width = 4,
                          h3("Input data"),
                          helpText(
                            p("Upload a file (*.csv, *.txt) containing",
                              "your own dataset (the default 'credit_data' of the SWIM package has been pre-uploaded for demonstration purposes). A preview of the dataset will be shown in the main",
                              "panel on the right."),
                          ),
                          hr(),
                          h4("Upload a dataset"),
                          fileInput("file_upload", "Choose file:",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values",
                                               "text/plain",
                                               ".csv",
                                               ".txt",
                                               ".dat"),
                                    buttonLabel = "Browse ..."),
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
                                                   "Comma" = ",")),
                          hr(),
                          h4("Preview"),
                          radioButtons("preview_type_radio", "Type of preview:",
                                       choices = c("Dataset" = "datas",
                                                   "Structure" = "struc"))
             ),
             mainPanel(
               wellPanel(style="padding:0px",
                         fluidRow(
                           column(width = 1, offset = 0, style = 'margin-top:27px; padding-left:35px',HTML('<b>Digits</b>')),
                           column(width = 2, offset = 0,style = 'margin-top:17px',numericInput("table_digits0",label = NULL,value = 2,min = 0,max=10,step = 1,width="50%")),
                           column(width = 1, offset = 7, style = 'margin-top:27px',HTML('<a href="https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html" target="_blank">Vignette</a>')),
                           column(width = 1, offset = 0, style = 'margin-top:27px',HTML('<a href="https://cran.r-project.org/web/packages/SWIM/SWIM.pdf" target="_blank">Manual</a>'))
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
                                                                actionButton("run_stress1", "Run Stress",
                                                                             style = "color: black; background-color: #337ab7 ;border-color: black")),
                                                         div(
                                                           style="padding:10px",
                                                           column(9,
                                                                  textOutput(("status1")))))),
                                   mainPanel(width = 8,
                                              wellPanel(style="padding:0px",
                                              fluidRow(
                                                column(width = 1, offset = 0, style = 'margin-top:27px; padding-left:35px',HTML('<b>Digits</b>')),
                                                column(width = 2, offset = 0,style = 'margin-top:17px',numericInput("table_digits1",label = NULL,value = 2,min = 0,max=10,step = 1,width="50%")),
                                                column(width = 1, offset = 0,style = 'margin-top:17px',actionButton("download1", "Download Summary")),
                                                column(width = 1, offset = 6, style = 'margin-top:27px',HTML('<a href="https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html" target="_blank">Vignette</a>')),
                                                column(width = 1, offset = 0, style = 'margin-top:27px',HTML('<a href="https://cran.r-project.org/web/packages/SWIM/SWIM.pdf" target="_blank">Manual</a>'))
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
                                                hidden(actionButton("run_stress2", "Run Stress")))
                                   ,mainPanel(width = 8,
                                              wellPanel(style="padding:0px",
                                                        fluidRow(
                                                          column(width = 1, offset = 0, style = 'margin-top:27px; padding-left:35px',HTML('<b>Digits</b>')),
                                                          column(width = 2, offset = 0,style = 'margin-top:17px',numericInput("table_digits2",label = NULL,value = 2,min = 0,max=10,step = 1,width="50%")),
                                                          column(width = 1, offset = 0,style = 'margin-top:17px',actionButton("download2", "Download Summary")),
                                                          column(width = 1, offset = 6, style = 'margin-top:27px',HTML('<a href="https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html" target="_blank">Vignette</a>')),
                                                          column(width = 1, offset = 0, style = 'margin-top:27px',HTML('<a href="https://cran.r-project.org/web/packages/SWIM/SWIM.pdf" target="_blank">Manual</a>'))
                                                        )
                                              ),
                                              DTOutput("SummaryStressBase2"),
                                              br(),
                                              DTOutput("SummaryStress2"),
                                              br(),
                                   ))
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
                                                hidden(actionButton("run_stress3", "Run Stress")))
                                   ,mainPanel(width = 8,
                                              wellPanel(style="padding:0px",
                                                        fluidRow(
                                                          column(width = 1, offset = 0, style = 'margin-top:27px; padding-left:35px',HTML('<b>Digits</b>')),
                                                          column(width = 2, offset = 0,style = 'margin-top:17px',numericInput("table_digits3",label = NULL,value = 2,min = 0,max=10,step = 1,width="50%")),
                                                          column(width = 1, offset = 0,style = 'margin-top:17px',actionButton("download3", "Download Summary")),
                                                          column(width = 1, offset = 6, style = 'margin-top:27px',HTML('<a href="https://cran.r-project.org/web/packages/SWIM/vignettes/SWIM-vignette.html" target="_blank">Vignette</a>')),
                                                          column(width = 1, offset = 0, style = 'margin-top:27px',HTML('<a href="https://cran.r-project.org/web/packages/SWIM/SWIM.pdf" target="_blank">Manual</a>'))
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
                                    h4("Control Panel"),
                                    helpText(
                                      p("To analyse the effect of different stresses on the selected variable. In the right panels the comparison is performed both through a plot and a set of statistics."),
                                    ),
                                    hr(),
                                    uiOutput("plotVariable"),
                                    selectInput("plotFunction", 
                                                "Select plot", c(
                                                  "Histogram",
                                                  "CDF",
                                                  "Quantiles",
                                                  "Scenario Weights"
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
             tabPanel("Credit Model",
                      fluidRow(
                        div(
                          style = "position: relative", 
                          column(width = 6,
                                 #style = "background-color:#FFFFFF;",
                                 h3("Credit Model"),
                                 wellPanel(
                                 p("The credit model
                                    in this section is a conditionally binomial loan portfolio model, including systematic and specific
                                    portfolio risk. We refer to the Appendix A for details about the model and the generation of the
                                    simulated data. A key variable of interest is the total aggregate portfolio loss L = L1+L2+L3, where
                                    L1,L2,L3 are homogeneous subportfolios on a comparable scale (say, thousands of $). The dataset
                                    contains 100,000 simulations of the portfolio L, the subportfolios L1,L2,L3 as well as the random
                                    default probabilities within each subportfolio, H1,H2,H3. These default probabilities represent the
                                    systematic risk within each subportfolio, while their dependence structure represents a systematic risk
                                    effect between the subportfolios. We may thus think of L as the model output, H1,H2,H3 as model
                                    inputs, and L1,L2,L3 as intermediate model outputs."
                                 )),    
                                 
                          )
                        )
                      )
             ),
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

