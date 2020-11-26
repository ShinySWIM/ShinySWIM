
ui <- dashboardPage(
  skin = "black",
  title = "SWIM",
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(
    title = span(img(src = "SWIM_logo.png", height = 35), "SWIM"),
    titleWidth = 150,
    
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
    width = 150,
    column(width = 12,
           h4("Shiny SWIM"),
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
                                      "Select function to stress simulations:", c(
                                        "stress_VaR",
                                        "stress_VaR_ES",
                                        "stress_mean",
                                        "stress_mean_sd",
                                        "stress_moment",
                                        "stress_prob"
                                      )),
                          # Argument selector:
                          uiOutput("condPanels"),
                          br(),
                          actionButton("run_stress", "Run Stress"),
                          br(), br(),
                          tableOutput("SummaryStressBase"),
                          tableOutput("SummaryStress")
                          
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

