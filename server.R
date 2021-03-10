server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  
  ##### Upload data: -------------------
  ## Argument names: -------------------------
  ArgNames <- reactive({
    Names <- names(formals(input$readFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  ## Argument selector: --------------------
  output$ArgSelect <- renderUI({
    if (length(ArgNames())==0) return(NULL)
    
    selectInput("arg","Argument:",ArgNames())
  })
  
  ## Arg text field: --------------------
  output$ArgText <- renderUI({
    fun__arg <- paste0(input$readFunction,"__",input$arg)
    
    if (is.null(input$arg)) return(NULL)
    
    Defaults <- formals(input$readFunction)
    
    if (is.null(input[[fun__arg]]))
    {
      textInput(fun__arg, label = "Enter value:", value = deparse(Defaults[[input$arg]])) 
    } else {
      textInput(fun__arg, label = "Enter value:", value = input[[fun__arg]]) 
    }
  })
  
  
  ## Data import: ------------------------
  Dataset <- reactive({
    
    if (is.null(input$user_data_file)) {
      ## User has not uploaded a file yet -------------
      data("credit_data")
      return(data.table(credit_data))
    }
    
    args <- grep(paste0("^",input$readFunction,"__"), names(input), value = TRUE)
    
    argList <- list()
    for (i in seq_along(args))
    {
      argList[[i]] <- eval(parse(text=input[[args[i]]]))
    }
    names(argList) <- gsub(paste0("^",input$readFunction,"__"),"",args)
    
    argList <- argList[names(argList) %in% ArgNames()]
    
    Dataset <- as.data.table(
      do.call(input$readFunction,
              c(list(input$user_data_file$datapath),
                argList)))
    return(Dataset)
  })
  
  ## Name of data set ---------------------------
  output$dataname <- renderText({
    if(is.null(input$user_data_file)){
      "Default data: credit_data"
    }else{
      paste("User data:", input$user_data_file)
    }
  })
  
  ## Select variables: --------------------------
  output$varselect <- renderUI({
    
    if (identical(Dataset(), '') || identical(Dataset(),
                                              data.table())) return(NULL)
    
    ## Variable selection: --------------------   
    selectInput("vars", "Variables to use:",
                names(Dataset()), names(Dataset()), multiple =TRUE)            
  })
  
  ## Show table: --------------------------
  output$headtable <- renderTable({
    
    return(head(Dataset()))
  }, rownames = TRUE)
  
  output$quantiletable <- renderTable({
    probs <- c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99 ,1)
    return(
      apply(Dataset(), 2, 
            quantile, 
            probs)  
    )
  }, rownames = TRUE)
  
  
  
  
  ##### Stress: -------------------
  ## Reactive variable, which stress are already performed ----------------
  performedStress <- reactiveValues(bool = c(FALSE,FALSE,FALSE))
  ## Stress1 argument names: -------------------------
  StressVarArgNames1 <- reactive({
    Names <- names(formals(input$stressFunction1)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  ## Stress2 argument names: -------------------------
  StressVarArgNames2 <- reactive({
    Names <- names(formals(input$stressFunction2)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  ## Stress3 argument names: -------------------------
  StressVarArgNames3 <- reactive({
    Names <- names(formals(input$stressFunction3)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  ## Create conditional stress1 UI ----------------
  output$condPanels1 <- renderUI({
    
    if (length(ArgNames())==0) return(NULL)
    
    stress_args <- StressVarArgNames1()
    pos_k <- which(stress_args=="k")
    stress_args <- c("k",stress_args[-pos_k])
    
    tagList(
      lapply(stress_args, function(arg){
        conditionalPanel(
          condition = "input.stressFunction1 != ''",
          div(
            #style="display: inline-block;vertical-align:bottom;width: 500px",
            helpText(p(subselect_text(arg)))),
          if ((arg != "normalise")&&(arg != "k")){
            div(
            #  style="display: inline-block;vertical-align:bottom;width: 150px",
              textInput(inputId = paste0("stress_arg_1", arg),label =  NULL, width = "50%"))
            },
          if (arg == "normalise"){
            div(
           #   style="display: inline-block;vertical-align:bottom;width: 150px",
              selectInput(inputId = paste0("stress_arg_1", arg), label = NULL, choices = c(TRUE,FALSE),selected = FALSE,width = "50%"))
            },
          if (arg == "k"){
            div(
           #   style="display: inline-block;vertical-align:bottom;width: 150px",
              selectInput(inputId = paste0("stress_arg_1", arg), label = NULL, choices = colnames(Dataset()),width = "50%"))
            }
        )
      }))
  })
  
  ## Create conditional stress2 UI ----------------
  output$condPanels2 <- renderUI({
    
    if (length(ArgNames())==0) return(NULL)
    
    stress_args <- StressVarArgNames2()
    pos_k <- which(stress_args=="k")
    stress_args <- c("k",stress_args[-pos_k])
    
    tagList(
      lapply(stress_args, function(arg){
        conditionalPanel(
          condition = "input.stressFunction2 != ''",
          div(
            #style="display: inline-block;vertical-align:bottom;width: 500px",
            helpText(p(subselect_text(arg)))),
          if ((arg != "normalise")&&(arg != "k")){
            div(
              #  style="display: inline-block;vertical-align:bottom;width: 150px",
              textInput(inputId = paste0("stress_arg_2", arg),label =  NULL, width = "50%"))
          },
          if (arg == "normalise"){
            div(
              #   style="display: inline-block;vertical-align:bottom;width: 150px",
              selectInput(inputId = paste0("stress_arg_2", arg), label = NULL, choices = c(TRUE,FALSE),selected = FALSE,width = "50%"))
          },
          if (arg == "k"){
            div(
              #   style="display: inline-block;vertical-align:bottom;width: 150px",
              selectInput(inputId = paste0("stress_arg_2", arg), label = NULL, choices = colnames(Dataset()),width = "50%"))
          }
        )
      }))
    
    
  })
  
  ## Create conditional stress3 UI ----------------
  output$condPanels3 <- renderUI({
    
    if (length(ArgNames())==0) return(NULL)
    
    stress_args <- StressVarArgNames3()
    pos_k <- which(stress_args=="k")
    stress_args <- c("k",stress_args[-pos_k])
    
    tagList(
      lapply(stress_args, function(arg){
        conditionalPanel(
          condition = "input.stressFunction3 != ''",
          div(
            #style="display: inline-block;vertical-align:bottom;width: 500px",
            helpText(p(subselect_text(arg)))),
          if ((arg != "normalise")&&(arg != "k")){
            div(
              #  style="display: inline-block;vertical-align:bottom;width: 150px",
              textInput(inputId = paste0("stress_arg_3", arg),label =  NULL, width = "50%"))
          },
          if (arg == "normalise"){
            div(
              #   style="display: inline-block;vertical-align:bottom;width: 150px",
              selectInput(inputId = paste0("stress_arg_3", arg), label = NULL, choices = c(TRUE,FALSE),selected = FALSE,width = "50%"))
          },
          if (arg == "k"){
            div(
              #   style="display: inline-block;vertical-align:bottom;width: 150px",
              selectInput(inputId = paste0("stress_arg_3", arg), label = NULL, choices = colnames(Dataset()),width = "50%"))
          }
        )
      }))
  })

  
  ## Create stress VaR object 1: ------------------------
  stress_VaR_obj1 <- eventReactive(input$run_stress1, {
    
    stress_VaR_args <- StressVarArgNames1()
    
    argList <- list()
    
    for (i in seq_along(stress_VaR_args))
    {
      if (stress_VaR_args[i]!="k"){
      argList[[i]] <- eval(parse(text=input[[paste0("stress_arg_1", stress_VaR_args[i])]]))}
      if (stress_VaR_args[i]=="k"){
        argList[[i]] <- eval(parse(text=which(colnames(Dataset())==input[["stress_arg_1k"]])))}
    }
    
   
    
    names(argList) <- gsub(paste0("^",input$stressFunction1,"__"),"",
                           stress_VaR_args)
    
    
    argList <- argList[names(argList) %in% StressVarArgNames1()]
  
    stress_obj <-  do.call(input$stressFunction1,c(list(Dataset()),argList))
    
    performedStress$bool[1] <- TRUE
    shinyjs::show("stressFunction2")
    shinyjs::show("condPanels2")
    shinyjs::show("run_stress2")
    shinyjs::hide("text2")
    #shinyjs::show("table_digits1")
    
    weights <- get_weights(stress_obj)
    diff <- max(weights)-min(weights)
    if (diff!=0) showNotification(ui = "The stress has been performed successfully! Wait...",type = "message",duration = 4)
    else showNotification(ui = "Something went wrong!",type = "warning",duration = 5)
   
    return(stress_obj)
  }) 
  
  ## Create stress VaR object 2: ------------------------
  stress_VaR_obj2 <- eventReactive(input$run_stress2, {
    
    stress_VaR_args <- StressVarArgNames2()
    
    argList <- list()
    
    for (i in seq_along(stress_VaR_args))
    {
      if (stress_VaR_args[i]!="k"){
        argList[[i]] <- eval(parse(text=input[[paste0("stress_arg_2", stress_VaR_args[i])]]))}
      if (stress_VaR_args[i]=="k"){
        argList[[i]] <- eval(parse(text=which(colnames(Dataset())==input[["stress_arg_2k"]])))}
    }
    
    names(argList) <- gsub(paste0("^",input$stressFunction2,"__"),"",
                           stress_VaR_args)
    
    
    argList <- argList[names(argList) %in% StressVarArgNames2()]
    
    stress_obj <- 
      do.call(input$stressFunction2,
              c(list(Dataset()),
                argList))
    
    performedStress$bool[2] <- TRUE
    shinyjs::show("stressFunction3")
    shinyjs::show("condPanels3")
    shinyjs::show("run_stress3")
    shinyjs::hide("text3")
    updateCheckboxGroupInput(session, inputId = "plotStress1",choices = list("Stress 1" = 1, "Stress 2" = 2), selected = 2)
    updateCheckboxGroupInput(session, inputId = "plotStress2",choices = list("Stress 1" = 1, "Stress 2" = 2), selected = 2)
    
    weights <- get_weights(stress_obj)
    diff <- max(weights)-min(weights)
    if (diff!=0) showNotification(ui = "The stress has been performed successfully! Wait...",type = "message",duration = 4)
    else showNotification(ui = "Something went wrong!",type = "warning",duration = 5)
    
    return(stress_obj)
  })
  
  ## Create stress VaR object 3: ------------------------
  stress_VaR_obj3 <- eventReactive(input$run_stress3, {
    
    stress_VaR_args <- StressVarArgNames3()
    
    argList <- list()
    
    for (i in seq_along(stress_VaR_args))
    {
      if (stress_VaR_args[i]!="k"){
        argList[[i]] <- eval(parse(text=input[[paste0("stress_arg_3", stress_VaR_args[i])]]))}
      if (stress_VaR_args[i]=="k"){
        argList[[i]] <- eval(parse(text=which(colnames(Dataset())==input[["stress_arg_3k"]])))}
    }
    
    names(argList) <- gsub(paste0("^",input$stressFunction3,"__"),"",
                           stress_VaR_args)
    
    
    argList <- argList[names(argList) %in% StressVarArgNames3()]
    
    stress_obj <- 
      do.call(input$stressFunction3,
              c(list(Dataset()),
                argList))
    
    performedStress$bool[3] <- TRUE
    updateCheckboxGroupInput(session, inputId = "plotStress1",choices = list("Stress 1" = 1, "Stress 2" = 2,"Stress 3" = 3), selected = 3)
    updateCheckboxGroupInput(session, inputId = "plotStress2",choices = list("Stress 1" = 1, "Stress 2" = 2,"Stress 3" = 3), selected = 3)
    
    weights <- get_weights(stress_obj)
    diff <- max(weights)-min(weights)
    if (diff!=0) showNotification(ui = "The stress has been performed successfully! Wait...",type = "message",duration = 4)
    else showNotification(ui = "Something went wrong!",type = "warning",duration = 5)
    
    return(stress_obj)
  }) 
  
  output$text2 <- renderText({"Stress 2 can only be added after Stress 1"})
  output$text3 <- renderText({"Stress 3 can only be added after Stress 2"})
  
  moments_f <- function(x,probs){
    n <- length(as.vector(x))
    mean <- mean(x)
    sd <- sqrt(mean((x - mean)^2)) * n / (n-1)
    quantile <- quantile(x = x,probs = probs)
    moments <- c(mean, sd, quantile)
    return(moments)
  }
  
  ## Summary_Base1: ------------------------
  output$SummaryStressBase1 <- renderDataTable(expr = {
    data <- Dataset()
    base_moments <- apply(X = data,MARGIN = 2,FUN=moments_f,probs=c(0.25,0.5,0.75,0.95,0.99))
    rownames(base_moments) <- c("mean", "sd", "Qu.25%","Qu.50%","Qu.75%", "Qu.95%","Qu.99%")
    base_moments <- round(base_moments, digits = input$table_digits1)
    return(base_moments)
  },caption = htmltools::tags$caption("Summary Baseline", style="color:black; font-weight: bold"),
  options=list(autoWidth = TRUE,
               columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  
  
  ## Summary_Base2: ------------------------
  output$SummaryStressBase2 <- renderDataTable(expr = {
    data <- Dataset()
    base_moments <- apply(X = data,MARGIN = 2,FUN=moments_f,probs=c(0.25,0.5,0.75,0.95,0.99))
    rownames(base_moments) <- c("mean", "sd", "Qu.25%","Qu.50%","Qu.75%", "Qu.95%","Qu.99%")
    base_moments <- round(base_moments, digits = input$table_digits2)
    return(base_moments)
  },caption = htmltools::tags$caption("Summary Baseline", style="color:black; font-weight: bold"),
  options=list(autoWidth = TRUE,
               columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  ## Summary_Base3: ------------------------
  output$SummaryStressBase3 <- renderDataTable(expr = {
    data <- Dataset()
    base_moments <- apply(X = data,MARGIN = 2,FUN=moments_f,probs=c(0.25,0.5,0.75,0.95,0.99))
    rownames(base_moments) <- c("mean", "sd", "Qu.25%","Qu.50%","Qu.75%", "Qu.95%","Qu.99%")
    base_moments <- round(base_moments, digits = input$table_digits3)
    return(base_moments)
  },caption = htmltools::tags$caption("Summary Baseline", style="color:black; font-weight: bold"),
  options=list(autoWidth = TRUE,
               columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  
  ## Summary_Stress1:------------------------
  output$SummaryStress1 <- renderDataTable({
    swim <- stress_VaR_obj1()
    swim_summary <- summary(swim, base = FALSE)
    swim_summary[[1]] <- swim_summary[[1]][-c(3,4),]
    swim_var <- SWIM::VaR_stressed(object = swim, alpha = c(0.95,0.99), base=TRUE)
    swim_var_matrix <- swim_var[,1:dim(swim$x)[2]]
    colnames(swim_var_matrix) <- colnames(swim_summary[[1]])
    swim_summary[[1]] <- rbind(swim_summary[[1]],swim_var_matrix)
    out <- round(swim_summary[[1]], digits = input$table_digits1)
    rownames(out) <- c("mean", "sd", "Qu.25%","Qu.50%","Qu.75%", "Qu.95%","Qu.99%")
    return(out)
  },caption = htmltools::tags$caption("Summary Stress 1", style="color:black; font-weight: bold"),
  options=list(autoWidth = TRUE,
               columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  
  ## Summary_Stress2:------------------------
  output$SummaryStress2 <- renderDataTable({
    swim <- stress_VaR_obj2()
    swim_summary <- summary(swim, base = FALSE)
    swim_summary[[1]] <- swim_summary[[1]][-c(3,4),]
    swim_var <- SWIM::VaR_stressed(object = swim, alpha = c(0.95,0.99), base=TRUE)
    swim_var_matrix <- swim_var[,1:dim(swim$x)[2]]
    colnames(swim_var_matrix) <- colnames(swim_summary[[1]])
    swim_summary[[1]] <- rbind(swim_summary[[1]],swim_var_matrix)
    out <- round(swim_summary[[1]], digits = input$table_digits2)
    rownames(out) <- c("mean", "sd", "Qu.25%","Qu.50%","Qu.75%", "Qu.95%","Qu.99%")
    return(out)
  },caption = htmltools::tags$caption("Summary Stress 2", style="color:black; font-weight: bold"),
  options=list(autoWidth = TRUE,
               columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  ## Summary_Stress3:------------------------
  output$SummaryStress3 <- renderDataTable({
    swim <- stress_VaR_obj3()
    swim_summary <- summary(swim, base = FALSE)
    swim_summary[[1]] <- swim_summary[[1]][-c(3,4),]
    swim_var <- SWIM::VaR_stressed(object = swim, alpha = c(0.95,0.99), base=TRUE)
    swim_var_matrix <- swim_var[,1:dim(swim$x)[2]]
    colnames(swim_var_matrix) <- colnames(swim_summary[[1]])
    swim_summary[[1]] <- rbind(swim_summary[[1]],swim_var_matrix)
    out <- round(swim_summary[[1]], digits = input$table_digits3)
    rownames(out) <- c("mean", "sd", "Qu.25%","Qu.50%","Qu.75%", "Qu.95%","Qu.99%")
    return(out)
  },caption = htmltools::tags$caption("Summary Stress 3", style="color:black; font-weight: bold"),
  options=list(autoWidth = TRUE,
               columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  
  ## Help Text:------------------------
  subselect_text <- function(arg){
    args <- list(
      x = "x - A vector, matrix or data frame containing realisations of random variables",
      alpha = "alpha - Level of the stressed VaR",
      q_ratio	= "q_ratio - The ratio of the stressed VaR to the baseline VaR",
      q	= "q - Stressed VaR at level alpha",
      k = "k - The variable of x that is stressed",
      s_ratio = "s_ratio - Numeric, vector, the ratio of the stressed ES to the baseline ES",
      s	= "s - The stressed ES at level alpha",
      new_means	= "new_means - Stressed mean",
      normalise	= "normalise - If true, values of f(x) are linearly scaled to the unit interval",
      new_sd = "new_sd - Stressed standard deviation"
    )
    return(args[arg])
  }
  
  
  
  
  ##### Stress Comparison: -------------------
  ## Comparison dynamic ui: ------------------------
  observeEvent(input$plotFunction, {
    
    if (input$plotFunction=="plot_hist") shinyjs::hide("dynamic_slider_y")
    if (input$plotFunction!="plot_hist") shinyjs::show("dynamic_slider_y")
    
    
    if (input$plotFunction=="plot_weights") shinyjs::hide("plotBase")
    if (input$plotFunction!="plot_weights") shinyjs::show("plotBase")
    
  })
  
  ## Plot Slider x : ------------------------
  output$dynamic_slider_x <- renderUI({
    
    suppressWarnings(expr = {
    data <- as.matrix(Dataset())
    var <- input$plotVar
    
    validate(
      need(isTRUE(any(performedStress$bool)), "Please insert at least one stress")
    )
    
    validate(
      need(isTRUE(any(performedStress$bool)), "Please insert at least one stress")
    )
    
      if (input$plotFunction=="plot_quantile") {
        min_slider <- 0
        max_slider <- 1
      } else  {
        min_slider <- round(min(data[,var]),4)
        max_slider <- round(max(data[,var]),4)
      }
    
      if (is.finite(min_slider) == TRUE){
      sliderInput("slider_x", label = "Range x-axis", min = min_slider,width = "100%",
          max = max_slider, value=c(min_slider,max_slider), round = -2,step = 0.01,ticks = FALSE)}
      })
      })
  
  ## Plot Slider y : ------------------------
  output$dynamic_slider_y <- renderUI({
    
    data <- as.matrix(Dataset())
    var <- input$plotVar
    
    if (isTRUE(performedStress$bool[1])) {
      swim1 <- stress_VaR_obj1()
      weights1 <- get_weights(swim1)
      weights_min_max <- c(min(weights1),max(weights1))
      }
    
    if (isTRUE(performedStress$bool[2])) {
      swim2 <- stress_VaR_obj2()
      weights2 <- get_weights(swim2)
      weights_min_max <- c(min(weights1),max(weights1))
      }
    
    if (isTRUE(performedStress$bool[1])&&isTRUE(performedStress$bool[2])) weights_min_max <- c(min(weights1,weights2),max(weights1,weights2))
    
    if (!is.null(var)){
      if (input$plotFunction=="plot_quantile") {
        min_slider <- round(min(data[,var]),4)
        max_slider <- round(max(data[,var]),4)
      } else if (input$plotFunction=="plot_weights")  {
        min_slider <- round(weights_min_max[1],4)
        max_slider <- round(weights_min_max[2],4)
      }
      else if (input$plotFunction=="plot_cdf")  {
        min_slider <- 0
        max_slider <- 1
      }
      else if (input$plotFunction=="plot_hist")  {
        min_slider <- 0
        max_slider <- 1
      }
      
      sliderInput("slider_y", label = "Range y-axis", min = min_slider, ticks = FALSE,width = "100%",
                  max = max_slider, value=c(min_slider,max_slider),round = -2)
      }
  })
  
  ##summary stress comparison: ------------------------
  output$summary_stress1 <- renderDataTable({
  
  validate(
    need(isTRUE(any(performedStress$bool)), "Please insert at least one stress")
  )
  
  swim <- swim_complete()
  specs1 <- get_specs(swim)
  k <- as.numeric(specs1[,2])
  w <- get_weights(swim)
  specs2 <- data.frame(colMeans(swim$x[,k]*w))
  colnames(specs2) <- "new_mean"
  out <- cbind(specs1,specs2)
  n.stress <- length(swim$type)
  out[,1][which(out[,1]=="VaR ES")] <- "VaR_ES"
  out[,1][which(out[,1]=="mean sd")] <- "mean_sd"
  rownames(out) <- paste("stress",1:n.stress,sep="")
  out1 <- cbind(out[,1:2],round(out[,3:dim(out)[2]],2))
  colnames(out1) <- colnames(out)
  out1
  },caption = htmltools::tags$caption("Stressess Loaded", style="color:black; font-weight: bold"),
  options=list(autoWidth = TRUE,
               columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE))
  
  
  ##summary sensitivity comparison: ------------------------
  output$summary_stress2 <- renderDataTable({
    
    validate(
      need(isTRUE(any(performedStress$bool)), "Please insert at least one stress")
    )
    
    swim <- swim_complete()
    specs1 <- get_specs(swim)
    k <- as.numeric(specs1[,2])
    w <- get_weights(swim)
    specs2 <- data.frame(colMeans(swim$x[,k]*w))
    colnames(specs2) <- "new_mean"
    out <- cbind(specs1,specs2)
    n.stress <- length(swim$type)
    out[,1][which(out[,1]=="VaR ES")] <- "VaR_ES"
    out[,1][which(out[,1]=="mean sd")] <- "mean_sd"
    rownames(out) <- paste("stress",1:n.stress,sep="")
    out1 <- cbind(out[,1:2],round(out[,3:dim(out)[2]],2))
    colnames(out1) <- colnames(out)
    out1
  },caption = htmltools::tags$caption("Stressess Loaded", style="color:black; font-weight: bold"),
  options=list(autoWidth = TRUE,
               columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE))
  
  ## Plot Variable: ------------------------
  output$plotVariable <- renderUI({
    var <- colnames(Dataset())
    selectInput(inputId = "plotVar", label = "Select Variable", choices = var,selected =var[1],multiple = FALSE)
  })
  
  ## Plot Comparison: ------------------------
  output$plot <- renderPlot({
    
    p <- NULL
    stress <- paste(input$plotStress1)
    
    validate(
      need(isTRUE(any(performedStress$bool)), "Please insert at least one stress")
    )
    
    
    validate(
      need(isFALSE(is.null(input$slider_x[1])), "Wait...")
    )
    
    validate(
      need(length(stress)>=1, "Please select at least one stress")
    )
    
    
    swim <- swim_complete()
    stress <- paste(input$plotStress1)
    base_label <- input$plotBase
    var <- input$plotVar
    plot_type <- input$plotFunction
    x_lim <- c(input$slider_x[1],input$slider_x[2])
    y_lim <- c(input$slider_y[1],input$slider_y[2])

    if (plot_type=="plot_weights")
      args_plot <- list("object"=swim, "xCol" = var, "x_limits" = x_lim,"y_limits" = y_lim, "wCol" = as.numeric(stress), n = 5000)
    else if (plot_type=="plot_quantile")
      args_plot <- list("object"=swim, "base" = base_label, "xCol" = var,"x_limits" = x_lim,"y_limits" = y_lim, "wCol" = as.numeric(stress))
    else if (plot_type=="plot_cdf")
      args_plot <- list("object"=swim, "base" = base_label, "xCol" = var,"x_limits" = x_lim,"y_limits" = y_lim, "wCol" = as.numeric(stress))
    else
      args_plot <- list("object"=swim, "base" = base_label, "xCol" = var,"x_limits" = x_lim, "wCol" = as.numeric(stress))
    if (!is.null(input$slider_x[1])){
      p <- do.call(plot_type, args_plot)
      p
    }
  },res = 120)
  
  
  swim_complete <- reactive({
    
    swim1 <- NULL
    swim2 <- NULL
    swim3 <- NULL
    swim <- NULL
    
    if (isTRUE(performedStress$bool[1])) swim1 <- stress_VaR_obj1()
    if (isTRUE(performedStress$bool[2])) swim2 <- stress_VaR_obj2()
    if (isTRUE(performedStress$bool[3])) swim3 <- stress_VaR_obj3()
    
    if (isTRUE(performedStress$bool[1])&&isTRUE(performedStress$bool[2])&&isTRUE(performedStress$bool[3])) swim = merge(swim1,merge(swim2,swim3))
    if (isTRUE(performedStress$bool[1])&&isTRUE(performedStress$bool[2])&&isFALSE(performedStress$bool[3])) swim = merge(swim1,swim2)
    if (isTRUE(performedStress$bool[1])&&isFALSE(performedStress$bool[2])&&isTRUE(performedStress$bool[3])) swim = merge(swim1,swim3)
    if (isFALSE(performedStress$bool[1])&&isTRUE(performedStress$bool[2])&&isTRUE(performedStress$bool[3])) swim = merge(swim2,swim3)
    if (isTRUE(performedStress$bool[1])&&isFALSE(performedStress$bool[2])&&isFALSE(performedStress$bool[3])) swim = swim1
    if (isFALSE(performedStress$bool[1])&&isTRUE(performedStress$bool[2])&&isFALSE(performedStress$bool[3])) swim = swim2
    if (isFALSE(performedStress$bool[1])&&isFALSE(performedStress$bool[2])&&isTRUE(performedStress$bool[3])) swim = swim3
    if (isFALSE(performedStress$bool[1])&&isFALSE(performedStress$bool[2])&&isFALSE(performedStress$bool[3])) swim = c()
    
    return(swim)
  })
  
  ## Figures Comparison: ------------------------
  
  output$comparisonfigures <- renderDataTable({
    
    stress1 <- NULL
    stress2 <- NULL
    stress3 <- NULL
    
    base <- NULL
    
    out <- NULL
    
    validate(
      need(isTRUE(any(performedStress$bool)), "Please insert at least one stress")
    )
    
    
    swim <- swim_complete()
    swim_summary <- summary(swim, base = TRUE)
    base_label <- input$plotBase
    var <- input$plotVar
    probs <- c(0.01,0.05, 0.25, 0.5, 0.75, 0.95,0.99)
    probs_s <- paste("Qu",probs*100,sep="")
    
    if (isTRUE(performedStress$bool[1])&&(!is.null(var))) {
      
      b_stress <- swim_summary[[1]][,var][1:4]
      base_q <- quantile(x = swim$x[,var],probs = probs)
      base <- c(b_stress,base_q)
      s1 <- swim_summary[[2]][,var][1:4]
      q1 <- quantile_stressed(object = swim,probs = probs,xCol = var,wCol = 1)
      stress1 <- c(s1,q1)
    }
    
    if (isTRUE(performedStress$bool[2])){ 
      s2 <- swim_summary[[3]][,var][1:4]
      q2 <- quantile_stressed(object = swim,probs = probs,xCol = var,wCol = 2)
      stress2 <- c(s2,q2)
    }
    if (isTRUE(performedStress$bool[3])) {
      s3 <- swim_summary[[4]][,var][1:4]
      q3 <- quantile_stressed(object = swim,probs = probs,xCol = var,wCol = 3)
      stress3 <- c(s3,q3)
    }
    
    out <- rbind(base,stress1,stress2,stress3)
    
    if (length(stress1)!=0) {
      colnames(out) <- c("mean" ,"sd"," skewness"," ex_kurtosis",probs_s)
      out <- round(out,3)
    out
    }
    
  },caption = htmltools::tags$caption(paste("Summary variable:",input$plotVar), style="color:black; font-weight: bold"),
  options=list(autoWidth = TRUE,
               columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE))
  
  
  
  ##### Sensitivity Comparison: -------------------
  ## Plot Sensitivity Measure: ------------------------
  output$plotSens <- renderPlot({
    
    p <- NULL
    stress <- paste(input$plotStress2)
    
    validate(
      need(isTRUE(any(performedStress$bool)), "Please insert at least one stress")
    )
    
    validate(
      need(length(stress)>=1, "Please select at least one stress")
    )
    
    swim <- swim_complete()
    
    plot_type <- input$sens_function

    args_plot <- list("object"=swim, "xCol" = "all", "wCol" = as.numeric(stress), type=plot_type)
    
      output$importance_rank <- renderTable(
      {
        
        validate(
          need(isTRUE(any(performedStress$bool)), "Please insert at least one stress")
        )
        
        m <- do.call(importance_rank, args_plot)
        m1 <- as.matrix(t(m)[-c(1,2),])
        colnames(m1) <- paste("stress",as.numeric(stress),sep="")
        m1
      },spacing = "m",rownames = TRUE,colnames=TRUE,align = "c",striped=TRUE,bordered = TRUE,width = "100%")
    
    if (!is.null(stress)){
      p <- do.call(plot_sensitivity, args_plot)
      p
    }
  },res = 115)
  
  
}
