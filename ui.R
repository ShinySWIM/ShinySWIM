
ui <- dashboardPage(
  skin = "black",
  title = "SWIM",
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(
    title = span(img(src = "SWIM_logo.png", height = 35), "SWIM"),
    titleWidth = 300,
    
    tags$li(
      a(
        strong("SWIM"),
        height = 40,
        href = "https://CRAN.R-project.org/package=SWIM",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),
  
  
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(
    width = 300,
    column(width = 12,
           h4("Selection"),
           
           selectInput("Years", 
                       label = ("Time horizon"), 
                       choices = c("1-Yr", "3-Yr", "5-Yr", "7-Yr", "10-Yr"),
                       selected = "3-Yr",
                       width = '250px'),
           
           
           sliderInput("yearlychurn",
                       label = "Prior years' weighting discount rate",
                       min = 0, sep = ",",
                       max = 95, value = 20, post  = " %",
                       step = 5, width = '250px'),
           radioButtons("labelpeers", "Label peers:",
                        choices = c(
                          "Yes" = TRUE,
                          "No" = FALSE
                        ), inline = TRUE,
                        selected = FALSE),
           radioButtons("trend", "Chart type:",
                        choices = c(
                          "Scatter" = FALSE,
                          "Trend" = TRUE
                        ), inline = TRUE,
                        selected = FALSE)
    )
  ),
  
  # BODY --------------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "SWIM_style.css")
    ),
    
    useShinyjs(),
    introjsUI(),
    
    # MAIN BODY ---------------------------------------------------------------
    
    tabsetPanel(
      tabPanel("Getting started",
               fluidRow(
                 div(
                   style = "position: relative", 
                   column(width = 12,
                          style = "background-color:#FFFFFF;",
                          includeMarkdown("www/Getting_started.md")
                   )
                 )
               )
      ),
      tabPanel("Data", 
               fluidRow(
                 column(width = 12,
                        h1("Input data"),
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
                        fileInput("user_data_file", "Upload data-file:"),
                        h1("Review data"),
                        h3(textOutput("dataname")),
                        p("First 6 rows:"),
                        tableOutput("headtable"),
                        p("Selected quantiles:"),
                        tableOutput("qunatiletable")
                 )
                 
               )
      ),
      tabPanel("Stresses",
               fluidRow(
                 div(
                   style = "position: relative", 
                   column(width = 12,
                          style = "background-color:#FFFFFF;",
                          h3("Stress"),
                          selectInput("stressFunction", 
                                      "Function to stress simulations:", c(
                                        "stress_VaR_ES",
                                        "stress_VaR",
                                        "stress_mean_sd",
                                        "stress_mean",
                                        "stress_moment",
                                        "stress_prob"
                                      )),
                          # Argument selector:
                          htmlOutput("StressVaRArgSelect"),
                          
                          # Argument field:
                          htmlOutput("StressVaRArgText"),
                          br()#,
                          #DT::dataTableOutput("SummaryStressBase")
                          
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
)

