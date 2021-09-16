server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  
  ##### Upload data: -------------------
  
  ## Data import: ------------------------
  Dataset <- reactive({
    
    if (is.null(input$file_upload)) {
      ## User has not uploaded a file yet -------------
      data("credit_data")
      return(data.table(credit_data))
    }
    
    req(input$file_upload)
    
    da_tmp <- try(read.csv(input$file_upload$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote,
                           dec = input$dec,
                           na.strings = c("NA", ".")),
                  silent = TRUE)
    if(inherits(da_tmp, "try-error")){
      showNotification(
        "File upload was not successful.",
        duration = NA,
        type = "error"
      )
      req(FALSE)
    }
    if("." %in% names(da_tmp)){
      if(!"X." %in% names(da_tmp)){
        names(da_tmp)[names(da_tmp) == "."] <- "X."
      } else{
        showNotification(
          HTML(paste(
            "The column name", code("."), "(dot) is not allowed. Automatically renaming this",
            "column to", code("X."), "failed since there already exists a column",
            code("X.", .noWS = "after"), "."
          )),
          duration = NA,
          type = "error"
        )
        req(FALSE)
      }
    }
    return(da_tmp)
  })
  
  #------------------------
  # Data preview
  
  output$da_view <- renderDataTable(expr = {
      d <- head(Dataset())
      d <- format(round(d,input$table_digits0),nsmall = input$table_digits0)
      return(d)
  }, options=list(autoWidth = FALSE,
               columnDefs = list(list(className = 'dt-center',width = '5%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  
  output$da_str <- renderPrint({
    str(Dataset())
  })
  
  ## Name of data set ---------------------------
  output$dataname <- renderText({
    d <- Dataset()
    if(is.null(input$file_upload)){
      "Default data: credit_data (100000 obs. of 7 variables)"
    }else{
      paste("User data: ", input$file_upload$name," (",dim(d)[1]," obs. of ",dim(d)[2]," variables)",sep="")
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
  
  output$headtable <- renderDataTable(expr = {
    d <- head(Dataset())
    d <- format(round(d,input$table_digits0),nsmall = input$table_digits0)
    return(d)
  }, caption = htmltools::tags$caption("Snippet of the data (first 6 rows)", style="color:black; font-weight: bold"),
  options=list(autoWidth = FALSE,
               columnDefs = list(list(className = 'dt-center',width = '5%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  
  
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
  
  #helper for selected stress1
  output$selected_stress1 <- renderText({ 
    s <- ""
    if (input$stressFunction1=="stress_VaR") s <- "For this stress, the VaR of a chosen variable is stressed in order to reach a specified level"
    if (input$stressFunction1=="stress_VaR_ES") s <- "For this stress, both the VaR and the ES of a chosen variable are stressed in order to reach the specified levels"
    if (input$stressFunction1=="stress_mean") s <- "For this stress, the mean of a chosen variable is stressed in order to reach a specified level"
    if (input$stressFunction1=="stress_mean_sd") s <- "For this stress, both the mean and the sd of a chosen variable are stressed in order to reach the specified levels"
    s
  })
  
  output$selected_stress2 <- renderText({ 
    s <- ""
    if (input$stressFunction2=="stress_VaR") s <- "For this stress, the VaR of a chosen variable is stressed in order to reach a specified level"
    if (input$stressFunction2=="stress_VaR_ES") s <- "For this stress, both the VaR and the ES of a chosen variable are stressed in order to reach the specified levels"
    if (input$stressFunction2=="stress_mean") s <- "For this stress, the mean of a chosen variable is stressed in order to reach a specified level"
    if (input$stressFunction2=="stress_mean_sd") s <- "For this stress, both the mean and the sd of a chosen variable are stressed in order to reach the specified levels"
    s
  })
  
  output$selected_stress3 <- renderText({ 
    s <- ""
    if (input$stressFunction3=="stress_VaR") s <- "For this stress, the VaR of a chosen variable is stressed in order to reach a specified level"
    if (input$stressFunction3=="stress_VaR_ES") s <- "For this stress, both the VaR and the ES of a chosen variable are stressed in order to reach the specified levels"
    if (input$stressFunction3=="stress_mean") s <- "For this stress, the mean of a chosen variable is stressed in order to reach a specified level"
    if (input$stressFunction3=="stress_mean_sd") s <- "For this stress, both the mean and the sd of a chosen variable are stressed in order to reach the specified levels"
    s
  })
  
  output$selected_sens_measures <- renderText({ 
    s <- ""
    if (input$sens_function=="Gamma") s <- "The Gamma measure quantified a normalised change in a variable's expectation, caused by a stress"
    if (input$sens_function=="Wasserstein") s <- "The Wasserstein measure gives the distance between the variable's distributions under the baseline and stressed models"
    s
  })
  
  ## Create conditional stress1 UI ----------------
  output$condPanels1 <- renderUI({
    
    stress_args <- StressVarArgNames1()
    pos_k <- which(stress_args=="k")
    stress_args <- c("k",stress_args[-pos_k])
    
    tagList(
      lapply(stress_args, function(arg){
        conditionalPanel(
          condition = "input.stressFunction1 != ''",
          if (input$stressFunction1!="stress_VaR"){
          div(
            helpText(p(subselect_text1(arg))))
            } else {
              div(
              helpText(p(subselect_text(arg))))
            },
          if ((arg != "normalise")&&(arg != "k")){
            div(
              textInput(inputId = paste0("stress_arg_1", arg),label =  NULL, width = "50%"))
          },
          if (arg == "normalise"){
            div(
             hidden(selectInput(inputId = paste0("stress_arg_1", arg), label = NULL, choices = c(TRUE,FALSE),selected = FALSE,width = "50%")))
          },
          if (arg == "k"){
            div(
              selectInput(inputId = paste0("stress_arg_1", arg), label = NULL, choices = colnames(Dataset()),width = "50%"))
          }
        )
      }))
  })
  
  ## Create conditional stress2 UI ----------------
  output$condPanels2 <- renderUI({
    
    stress_args <- StressVarArgNames2()
    pos_k <- which(stress_args=="k")
    stress_args <- c("k",stress_args[-pos_k])
    
    tagList(
      lapply(stress_args, function(arg){
        conditionalPanel(
          condition = "input.stressFunction2 != ''",
          if (input$stressFunction2!="stress_VaR"){
            div(
              helpText(p(subselect_text1(arg))))
          } else {
            div(
              helpText(p(subselect_text(arg))))
          },
          if ((arg != "normalise")&&(arg != "k")){
            div(
              textInput(inputId = paste0("stress_arg_2", arg),label =  NULL, width = "50%"))
          },
          if (arg == "normalise"){
            div(
              hidden(selectInput(inputId = paste0("stress_arg_2", arg), label = NULL, choices = c(TRUE,FALSE),selected = FALSE,width = "50%")))
          },
          if (arg == "k"){
            div(
              selectInput(inputId = paste0("stress_arg_2", arg), label = NULL, choices = colnames(Dataset()),width = "50%"))
          }
        )
      }))
  })
  
  ## Create conditional stress3 UI ----------------
  output$condPanels3 <- renderUI({
    
    stress_args <- StressVarArgNames3()
    pos_k <- which(stress_args=="k")
    stress_args <- c("k",stress_args[-pos_k])
    
    tagList(
      lapply(stress_args, function(arg){
        conditionalPanel(
          condition = "input.stressFunction3 != ''",
          if (input$stressFunction3!="stress_VaR"){
            div(
              helpText(p(subselect_text1(arg))))
          } else {
            div(
              helpText(p(subselect_text(arg))))
          },
          if ((arg != "normalise")&&(arg != "k")){
            div(
              textInput(inputId = paste0("stress_arg_3", arg),label =  NULL, width = "50%"))
          },
          if (arg == "normalise"){
            div(
              hidden(selectInput(inputId = paste0("stress_arg_3", arg), label = NULL, choices = c(TRUE,FALSE),selected = FALSE,width = "50%")))
          },
          if (arg == "k"){
            div(
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
    
    if (!is.null(argList$normalise)) argList$normalise <- TRUE
    
    #Dataset
    x <- as.matrix(Dataset())
    
    #validation for all functions
    validate(
      need(!anyNA(argList$x) ,"Data contains NA")
    )
    
    #Validation stress_VaR
    if (input$stressFunction1=="stress_VaR"){
      
      alpha <- argList$alpha
      q <- argList$q
      q_ratio <- argList$q_ratio
      k <- as.numeric(argList$k)
      n <- length(x[, k])
      
      validate(
        need( ((alpha > 0) & (alpha < 1) ), "Invalid alpha argument")
      )
      
      VaR <- stats::quantile(x[, k], alpha, names = FALSE, type = 1)
      
      validate(
        need( (is.null(q) || is.null(q_ratio)) ,"Only provide q or q_ratio"),
        need( (!is.null(q) || !is.null(q_ratio)) ,"No stress defined")
      )
      
      if(is.null(q)){
        validate(
          need(is.numeric(q_ratio) ,"Invalid q_ratio argument")
        )
        q <- q_ratio*VaR
      } else {
        validate(
          need(is.numeric(q) ,"Invalid q argument")
        )
      }
      
      validate(
        need(!any(VaR != q & (stats::ecdf(x[, k]))(VaR) == (stats::ecdf(x[, k]))(q)),"There are not enough data points, specifically, there is none between VaR and q."),
        need((!any(q >= max(x[, k])) && !any(q <= min(x[, k]))),"q needs to be smaller than the largest and larger than the smallest data point.")
      )
    }
    
    #Validation Stress_VaR_ES
    if (input$stressFunction1=="stress_VaR_ES"){
      
      
      alpha <- argList$alpha
      q <- argList$q
      q_ratio <- argList$q_ratio
      s <- argList$s
      s_ratio <- argList$s_ratio
      k <- as.numeric(argList$k)
      n <- length(x[, k])
      
      validate(
        need( ((alpha > 0) & (alpha < 1) ), "Invalid alpha argument"),
        need( (is.null(q) || is.null(q_ratio)) ,"Only provide q or q_ratio"),
        need( (is.null(s) || is.null(s_ratio)),"Only provide s or s_ratio"),
        need( (!is.null(q) || !is.null(q_ratio)) ,"no q or q_ratio defined"),
        need( (!is.null(s) || !is.null(s_ratio)) ,"no s or s_ratio defined")
      )
      
      VaR <- stats::quantile(x[, k], alpha, names = FALSE, type = 1)
      ecdfx <- stats::ecdf(x[, k])
        
        if(is.null(q)){
          validate(
          need(is.numeric(q_ratio) ,"Invalid q_ratio argument")
          )
          q <- q_ratio*VaR
          VaR_achieved <- max(x[, k][x[, k] <= q])
        } else {
          validate(
          need(is.numeric(q) ,"Invalid q argument")
          )
          VaR_achieved <- max(x[, k][x[, k] <= q])
        }
      
      if (is.null(s)) {
        validate(
          need(is.numeric(s_ratio) ,"Invalid s_ratio argument")
        )
        ES <- mean((x[, k] - VaR_achieved) * (x[, k] > VaR_achieved))/(1 - alpha) + VaR_achieved
        s <- s_ratio*ES
      }
      else {
        validate(
        need( (is.numeric(s)),"Invalid s argument")
        )
        ES <- mean((x[, k] - VaR_achieved) * (x[, k] > VaR_achieved))/(1 - alpha) + VaR_achieved
      }
      
      validate(
        need (any(q <= s),"q needs to be smaller than s."),
        need (!any(VaR != q & ecdfx(VaR) == ecdfx(q)) ,"There are not enough data points, specifically, there is none between VaR and q."),
        need (!any(abs(ecdfx(q) - ecdfx(s)) <= 1/n) ,"There are not enough data points, specifically, there is none between q and s."),
        need (!any(s >= max(x[, k])) && !any(q <= min(x[, k])) ,"s needs to be smaller than the largest and all q larger than the smallest data point.")
      )
    }
    
    #Validation stress_mean
    if (input$stressFunction1=="stress_mean"){
      k <- argList$k
      m <- argList$new_means
      f <- as.list(rep(list(function(x) x), length(k)))
      z <- matrix(0, ncol = length(f), nrow = nrow(x))
      for (i in 1:length(f)) {
        z[, i] <- apply(X = x[, k, drop = FALSE], MARGIN = 1, 
                        FUN = f[[i]])
      }
      min.fz <- apply(z, 2, min)
      max.fz <- apply(z, 2, max)
      
      validate(
        need(is.numeric(m) ,"Invalid stressed mean argument")
      )
      
      validate(
        need( (!any(m < min.fz) && !any(m > max.fz)),  "Value of stressed mean out of range")
      )
      
    }
    
    #Validation stress_mean_sd
    if (input$stressFunction1=="stress_mean_sd"){
      k <- argList$k
      new_mean <- argList$new_means
      
      validate(
        need(is.numeric(new_mean) ,"Invalid stressed mean argument")
      )
      
      new_sd <- argList$new_sd
      
      validate(
        need(is.numeric(new_sd) ,"Invalid stress sd argument")
      )
      
      m <- c(new_mean, new_mean^2 + new_sd^2)
      
      means <- rep(list(function(x) x), length(k))
      second_moments <- rep(list(function(x) x^2), length(k))
      f <- as.list(c(means, second_moments))
      z <- matrix(0, ncol = length(f), nrow = nrow(x))
      for (i in 1:length(f)) {
        z[, i] <- apply(X = x[, k, drop = FALSE], MARGIN = 1, 
                        FUN = f[[i]])
      }
      min.fz <- apply(z, 2, min)
      max.fz <- apply(z, 2, max)
      
      validate(
        need( (!any(m < min.fz) && !any(m > max.fz)),  "Values out of range")
      )
    }
    
    stress_obj <-  do.call(input$stressFunction1,c(list(Dataset()),argList))
    
    performedStress$bool[1] <- TRUE
    shinyjs::show("stressFunction2")
    shinyjs::show("condPanels2")
    shinyjs::show("run_stress2")
    shinyjs::show("selected_stress2")
    shinyjs::hide("text2")
    shinyjs::hide("file_upload")
    shinyjs::show("file_upload_text")
    
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
    
    if (!is.null(argList$normalise)) argList$normalise <- TRUE
    
    #Dataset
    x <- as.matrix(Dataset())
    
    #validation for all functions
    validate(
      need(!anyNA(argList$x) ,"Data contains NA")
    )
    
    #Validation stress_VaR
    if (input$stressFunction2=="stress_VaR"){
      
      alpha <- argList$alpha
      q <- argList$q
      q_ratio <- argList$q_ratio
      k <- as.numeric(argList$k)
      n <- length(x[, k])
      
      validate(
        need( ((alpha > 0) & (alpha < 1) ), "Invalid alpha argument")
      )
      
      VaR <- stats::quantile(x[, k], alpha, names = FALSE, type = 1)
      
      validate(
        need( (is.null(q) || is.null(q_ratio)) ,"Only provide q or q_ratio"),
        need( (!is.null(q) || !is.null(q_ratio)) ,"No stress defined")
      )
      
      if(is.null(q)){
        validate(
          need(is.numeric(q_ratio) ,"Invalid q_ratio argument")
        )
        q <- q_ratio*VaR
      } else {
        validate(
          need(is.numeric(q) ,"Invalid q argument")
        )
      }
      
      validate(
        need(!any(VaR != q & (stats::ecdf(x[, k]))(VaR) == (stats::ecdf(x[, k]))(q)),"There are not enough data points, specifically, there is none between VaR and q."),
        need((!any(q >= max(x[, k])) && !any(q <= min(x[, k]))),"q needs to be smaller than the largest and larger than the smallest data point.")
      )
    }
    
    #Validation Stress_VaR_ES
    if (input$stressFunction2=="stress_VaR_ES"){
      
      
      alpha <- argList$alpha
      q <- argList$q
      q_ratio <- argList$q_ratio
      s <- argList$s
      s_ratio <- argList$s_ratio
      k <- as.numeric(argList$k)
      n <- length(x[, k])
      
      validate(
        need( ((alpha > 0) & (alpha < 1) ), "Invalid alpha argument"),
        need( (is.null(q) || is.null(q_ratio)) ,"Only provide q or q_ratio"),
        need( (is.null(s) || is.null(s_ratio)),"Only provide s or s_ratio"),
        need( (!is.null(q) || !is.null(q_ratio)) ,"no q or q_ratio defined"),
        need( (!is.null(s) || !is.null(s_ratio)) ,"no s or s_ratio defined")
      )
      
      VaR <- stats::quantile(x[, k], alpha, names = FALSE, type = 1)
      ecdfx <- stats::ecdf(x[, k])
      
      if(is.null(q)){
        validate(
          need(is.numeric(q_ratio) ,"Invalid q_ratio argument")
        )
        q <- q_ratio*VaR
        VaR_achieved <- max(x[, k][x[, k] <= q])
      } else {
        validate(
          need(is.numeric(q) ,"Invalid q argument")
        )
        VaR_achieved <- max(x[, k][x[, k] <= q])
      }
      
      if (is.null(s)) {
        validate(
          need(is.numeric(s_ratio) ,"Invalid s_ratio argument")
        )
        ES <- mean((x[, k] - VaR_achieved) * (x[, k] > VaR_achieved))/(1 - alpha) + VaR_achieved
        s <- s_ratio*ES
      }
      else {
        validate(
          need( (is.numeric(s)),"Invalid s argument")
        )
        ES <- mean((x[, k] - VaR_achieved) * (x[, k] > VaR_achieved))/(1 - alpha) + VaR_achieved
      }
      
      validate(
        need (any(q <= s),"q needs to be smaller than s."),
        need (!any(VaR != q & ecdfx(VaR) == ecdfx(q)) ,"There are not enough data points, specifically, there is none between VaR and q."),
        need (!any(abs(ecdfx(q) - ecdfx(s)) <= 1/n) ,"There are not enough data points, specifically, there is none between q and s."),
        need (!any(s >= max(x[, k])) && !any(q <= min(x[, k])) ,"s needs to be smaller than the largest and all q larger than the smallest data point.")
      )
    }
    
    #Validation stress_mean
    if (input$stressFunction2=="stress_mean"){
      k <- argList$k
      m <- argList$new_means
      f <- as.list(rep(list(function(x) x), length(k)))
      z <- matrix(0, ncol = length(f), nrow = nrow(x))
      for (i in 1:length(f)) {
        z[, i] <- apply(X = x[, k, drop = FALSE], MARGIN = 1, 
                        FUN = f[[i]])
      }
      min.fz <- apply(z, 2, min)
      max.fz <- apply(z, 2, max)
      
      validate(
        need(is.numeric(m) ,"Invalid stressed mean argument")
      )
      
      validate(
        need( (!any(m < min.fz) && !any(m > max.fz)),  "Value of stressed mean out of range")
      )
      
    }
    
    #Validation stress_mean_sd
    if (input$stressFunction2=="stress_mean_sd"){
      k <- argList$k
      new_mean <- argList$new_means
      
      validate(
        need(is.numeric(new_mean) ,"Invalid stressed mean argument")
      )
      
      new_sd <- argList$new_sd
      
      validate(
        need(is.numeric(new_sd) ,"Invalid stress sd argument")
      )
      
      m <- c(new_mean, new_mean^2 + new_sd^2)
      
      means <- rep(list(function(x) x), length(k))
      second_moments <- rep(list(function(x) x^2), length(k))
      f <- as.list(c(means, second_moments))
      z <- matrix(0, ncol = length(f), nrow = nrow(x))
      for (i in 1:length(f)) {
        z[, i] <- apply(X = x[, k, drop = FALSE], MARGIN = 1, 
                        FUN = f[[i]])
      }
      min.fz <- apply(z, 2, min)
      max.fz <- apply(z, 2, max)
      
      validate(
        need( (!any(m < min.fz) && !any(m > max.fz)),  "Values out of range")
      )
    }
    
    stress_obj <-  do.call(input$stressFunction2,c(list(Dataset()),argList))
    
    performedStress$bool[2] <- TRUE
    shinyjs::show("stressFunction3")
    shinyjs::show("condPanels3")
    shinyjs::show("run_stress3")
    shinyjs::show("selected_stress3")
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
    
    if (!is.null(argList$normalise)) argList$normalise <- TRUE
    
    #Dataset
    x <- as.matrix(Dataset())
    
    #validation for all functions
    validate(
      need(!anyNA(argList$x) ,"Data contains NA")
    )
    
    #Validation stress_VaR
    if (input$stressFunction3=="stress_VaR"){
      
      alpha <- argList$alpha
      q <- argList$q
      q_ratio <- argList$q_ratio
      k <- as.numeric(argList$k)
      n <- length(x[, k])
      
      validate(
        need( ((alpha > 0) & (alpha < 1) ), "Invalid alpha argument")
      )
      
      VaR <- stats::quantile(x[, k], alpha, names = FALSE, type = 1)
      
      validate(
        need( (is.null(q) || is.null(q_ratio)) ,"Only provide q or q_ratio"),
        need( (!is.null(q) || !is.null(q_ratio)) ,"No stress defined")
      )
      
      if(is.null(q)){
        validate(
          need(is.numeric(q_ratio) ,"Invalid q_ratio argument")
        )
        q <- q_ratio*VaR
      } else {
        validate(
          need(is.numeric(q) ,"Invalid q argument")
        )
      }
      
      validate(
        need(!any(VaR != q & (stats::ecdf(x[, k]))(VaR) == (stats::ecdf(x[, k]))(q)),"There are not enough data points, specifically, there is none between VaR and q."),
        need((!any(q >= max(x[, k])) && !any(q <= min(x[, k]))),"q needs to be smaller than the largest and larger than the smallest data point.")
      )
    }
    
    #Validation Stress_VaR_ES
    if (input$stressFunction3=="stress_VaR_ES"){
      
      
      alpha <- argList$alpha
      q <- argList$q
      q_ratio <- argList$q_ratio
      s <- argList$s
      s_ratio <- argList$s_ratio
      k <- as.numeric(argList$k)
      n <- length(x[, k])
      
      validate(
        need( ((alpha > 0) & (alpha < 1) ), "Invalid alpha argument"),
        need( (is.null(q) || is.null(q_ratio)) ,"Only provide q or q_ratio"),
        need( (is.null(s) || is.null(s_ratio)),"Only provide s or s_ratio"),
        need( (!is.null(q) || !is.null(q_ratio)) ,"no q or q_ratio defined"),
        need( (!is.null(s) || !is.null(s_ratio)) ,"no s or s_ratio defined")
      )
      
      VaR <- stats::quantile(x[, k], alpha, names = FALSE, type = 1)
      ecdfx <- stats::ecdf(x[, k])
      
      if(is.null(q)){
        validate(
          need(is.numeric(q_ratio) ,"Invalid q_ratio argument")
        )
        q <- q_ratio*VaR
        VaR_achieved <- max(x[, k][x[, k] <= q])
      } else {
        validate(
          need(is.numeric(q) ,"Invalid q argument")
        )
        VaR_achieved <- max(x[, k][x[, k] <= q])
      }
      
      if (is.null(s)) {
        validate(
          need(is.numeric(s_ratio) ,"Invalid s_ratio argument")
        )
        ES <- mean((x[, k] - VaR_achieved) * (x[, k] > VaR_achieved))/(1 - alpha) + VaR_achieved
        s <- s_ratio*ES
      }
      else {
        validate(
          need( (is.numeric(s)),"Invalid s argument")
        )
        ES <- mean((x[, k] - VaR_achieved) * (x[, k] > VaR_achieved))/(1 - alpha) + VaR_achieved
      }
      
      validate(
        need (any(q <= s),"q needs to be smaller than s."),
        need (!any(VaR != q & ecdfx(VaR) == ecdfx(q)) ,"There are not enough data points, specifically, there is none between VaR and q."),
        need (!any(abs(ecdfx(q) - ecdfx(s)) <= 1/n) ,"There are not enough data points, specifically, there is none between q and s."),
        need (!any(s >= max(x[, k])) && !any(q <= min(x[, k])) ,"s needs to be smaller than the largest and all q larger than the smallest data point.")
      )
    }
    
    #Validation stress_mean
    if (input$stressFunction3=="stress_mean"){
      k <- argList$k
      m <- argList$new_means
      f <- as.list(rep(list(function(x) x), length(k)))
      z <- matrix(0, ncol = length(f), nrow = nrow(x))
      for (i in 1:length(f)) {
        z[, i] <- apply(X = x[, k, drop = FALSE], MARGIN = 1, 
                        FUN = f[[i]])
      }
      min.fz <- apply(z, 2, min)
      max.fz <- apply(z, 2, max)
      
      validate(
        need(is.numeric(m) ,"Invalid stressed mean argument")
      )
      
      validate(
        need( (!any(m < min.fz) && !any(m > max.fz)),  "Value of stressed mean out of range")
      )
      
    }
    
    #Validation stress_mean_sd
    if (input$stressFunction3=="stress_mean_sd"){
      k <- argList$k
      new_mean <- argList$new_means
      
      validate(
        need(is.numeric(new_mean) ,"Invalid stressed mean argument")
      )
      
      new_sd <- argList$new_sd
      
      validate(
        need(is.numeric(new_sd) ,"Invalid stress sd argument")
      )
      
      m <- c(new_mean, new_mean^2 + new_sd^2)
      
      means <- rep(list(function(x) x), length(k))
      second_moments <- rep(list(function(x) x^2), length(k))
      f <- as.list(c(means, second_moments))
      z <- matrix(0, ncol = length(f), nrow = nrow(x))
      for (i in 1:length(f)) {
        z[, i] <- apply(X = x[, k, drop = FALSE], MARGIN = 1, 
                        FUN = f[[i]])
      }
      min.fz <- apply(z, 2, min)
      max.fz <- apply(z, 2, max)
      
      validate(
        need( (!any(m < min.fz) && !any(m > max.fz)),  "Values out of range")
      )
    }
    
    stress_obj <-  do.call(input$stressFunction3,c(list(Dataset()),argList))
    
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
  output$file_upload_text <- renderText({"Once you have run the first stress you cannot change the data anymore"})
  
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
    rownames(base_moments) <- c("mean", "sd", "Q25","Q50","Q75", "Q95","Q99")
    base_moments <- format(round(base_moments, digits = input$table_digits1),nsmall = input$table_digits1)
    return(base_moments)
  },caption = htmltools::tags$caption("Summary Baseline", style="color:black; font-weight: bold"),
  options=list(autoWidth = FALSE,
               columnDefs = list(list(className = 'dt-center',width = '5%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  
  
  ## Summary_Base2: ------------------------
  output$SummaryStressBase2 <- renderDataTable(expr = {
    
    data <- Dataset()
    base_moments <- apply(X = data,MARGIN = 2,FUN=moments_f,probs=c(0.25,0.5,0.75,0.95,0.99))
    rownames(base_moments) <- c("mean", "sd", "Q25","Q50","Q75", "Q95","Q99")
    base_moments <- format(round(base_moments, digits = input$table_digits2),nsmall = input$table_digits2)
    return(base_moments)
  },caption = htmltools::tags$caption("Summary Baseline", style="color:black; font-weight: bold"),
  options=list(autoWidth = FALSE,
               columnDefs = list(list(className = 'dt-center',width = '5%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  ## Summary_Base3: ------------------------
  output$SummaryStressBase3 <- renderDataTable(expr = {
    data <- Dataset()
    base_moments <- apply(X = data,MARGIN = 2,FUN=moments_f,probs=c(0.25,0.5,0.75,0.95,0.99))
    rownames(base_moments) <- c("mean", "sd", "Q25","Q50","Q75", "Q95","Q99")
    base_moments <- format(round(base_moments, digits = input$table_digits3),nsmall = input$table_digits3)
    return(base_moments)
  },caption = htmltools::tags$caption("Summary Baseline", style="color:black; font-weight: bold"),
  options=list(autoWidth = FALSE,
               columnDefs = list(list(className = 'dt-center',width = '5%', targets = "_all")),
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
    out <- format(round(swim_summary[[1]], digits = input$table_digits1),nsmall = input$table_digits1)
    rownames(out) <- c("mean", "sd", "Q25","Q50","Q75", "Q95","Q99")
    return(out)
  },caption = htmltools::tags$caption("Summary Stress 1", style="color:black; font-weight: bold"),
  options=list(autoWidth = FALSE,
               columnDefs = list(list(className = 'dt-center',width = '5%', targets = "_all")),
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
    out <- format(round(swim_summary[[1]], digits = input$table_digits2),nsmall = input$table_digits2)
    rownames(out) <- c("mean", "sd", "Q25","Q50","Q75", "Q95","Q99")
    return(out)
  },caption = htmltools::tags$caption("Summary Stress 2", style="color:black; font-weight: bold"),
  options=list(autoWidth = FALSE,
               columnDefs = list(list(className = 'dt-center',width = '5%', targets = "_all")),
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
    out <- format(round(swim_summary[[1]], digits = input$table_digits3),nsmall = input$table_digits3)
    rownames(out) <- c("mean", "sd", "Q25","Q50","Q75", "Q95","Q99")
    return(out)
  },caption = htmltools::tags$caption("Summary Stress 3", style="color:black; font-weight: bold"),
  options=list(autoWidth = FALSE,
               columnDefs = list(list(className = 'dt-center',width = '5%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE)
  )
  
  ## Help Text:------------------------
  subselect_text <- function(arg){
    args <- list(
      x = "x - A vector, matrix or data frame containing realisations of random variables",
      alpha = "alpha - Level of the stressed VaR",
      q_ratio	= "q_ratio - The ratio of the stressed VaR to the baseline VaR",
      q	= "q - Stressed VaR at level alpha (alternative to 'q_ratio')",
      k = "k - The column of the data to be stressed",
      s_ratio = "s_ratio - Numeric, vector, the ratio of the stressed ES to the baseline ES",
      s	= "s - The stressed ES at level alpha (alternative to 's_ratio')",
      new_means	= "new_mean - Stressed mean",
      new_sd = "new_sd - Stressed standard deviation"
    )
    return(args[arg])
  }
  
  subselect_text1 <- function(arg){
    args <- list(
      x = "x - A vector, matrix or data frame containing realisations of random variables",
      alpha = "alpha - Level of the stressed VaR and ES",
      q_ratio	= "q_ratio - The ratio of the stressed VaR to the baseline VaR",
      q	= "q - Stressed VaR at level alpha",
      k = "k - The column of the data to be stressed",
      s_ratio = "s_ratio - Numeric, vector, the ratio of the stressed ES to the baseline ES",
      s	= "s - The stressed ES at level alpha",
      new_means	= "new_mean - Stressed mean",
      new_sd = "new_sd - Stressed standard deviation"
    )
    return(args[arg])
  }
  
  
  
  ##### Stress Comparison: -------------------
  ## Comparison dynamic ui: ------------------------
  observeEvent(input$plotFunction, {
    
    if (input$plotFunction=="Histogram") shinyjs::hide("dynamic_slider_y")
    if (input$plotFunction!="Histogram") shinyjs::show("dynamic_slider_y")
    
    
    if (input$plotFunction=="Scenario Weights") shinyjs::hide("plotBase")
    if (input$plotFunction!="Scenario Weights") shinyjs::show("plotBase")
    
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
      
      if (input$plotFunction=="Quantiles") {
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
      if (input$plotFunction=="Quantiles") {
        min_slider <- round(min(data[,var]),4)
        max_slider <- round(max(data[,var]),4)
      } else if (input$plotFunction=="Scenario Weights")  {
        min_slider <- round(weights_min_max[1],4)
        max_slider <- round(weights_min_max[2],4)
      }
      else if (input$plotFunction=="CDF")  {
        min_slider <- 0
        max_slider <- 1
      }
      else if (input$plotFunction=="Histogram")  {
        min_slider <- 0
        max_slider <- 1
      }
      
      sliderInput("slider_y", label = "Range y-axis", min = min_slider, ticks = FALSE,width = "100%",
                  max = max_slider, value=c(min_slider,max_slider),round = -2)
    }
  })
  
  moments_s <- function(x, w){
    n <- length(as.vector(x))
    mean_w <- stats::weighted.mean(x = x, w = w)
    sd_w <- sqrt(mean(w * (x - mean_w)^2)) * n / (n-1)
    moments_w <- c(mean_w,sd_w)
    return(moments_w)
  }
  
  ##summary stress comparison: ------------------------
  output$summary_stress1 <- renderDataTable({
    
    validate(
      need(isTRUE(any(performedStress$bool)), "Please insert at least one stress")
    )
    
    swim <- swim_complete()
    specs1 <- get_specs(swim)
    k <- as.numeric(specs1[,2])
    n.stress <- length(k)
    w <- get_weights(swim)
    m <- matrix(nrow=n.stress,ncol=2)
    for (i in 1:n.stress){
      m[i,] <- moments_s(swim$x[,k[i]],w[,i])
    }
    specs2 <- data.frame(m)
    colnames(specs2) <- c("new_mean","new_sd")
    out <- cbind(specs1,specs2)
    out[,1][which(out[,1]=="VaR ES")] <- "VaR&ES"
    out[,1][which(out[,1]=="mean sd")] <- "mean&sd"
    pos_m <- which(colnames(out)=="new_mean")
    pos_s <- which(colnames(out)=="new_sd")
    var_pos <- which(out[,1]=="VaR")
    vares_pos <- which(out[,1]=="VaR&ES")
    varm_pos <- which(out[,1]=="mean")
    out[var_pos,pos_m] <- NA
    out[vares_pos,pos_m] <- NA
    out[var_pos,pos_s] <- NA
    out[vares_pos,pos_s] <- NA
    out[varm_pos,pos_s] <- NA
    rownames(out) <- paste("stress",1:n.stress,sep="")
    out1 <- cbind(out[,1:2],round(out[,3:dim(out)[2]],2))
    colnames(out1) <- colnames(out)
    out1
  },caption = htmltools::tags$caption("Stresses Loaded", style="color:black; font-weight: bold"),
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
    n.stress <- length(k)
    w <- get_weights(swim)
    m <- matrix(nrow=n.stress,ncol=2)
    for (i in 1:n.stress){
      m[i,] <- moments_s(swim$x[,k[i]],w[,i])
    }
    specs2 <- data.frame(m)
    colnames(specs2) <- c("new_mean","new_sd")
    out <- cbind(specs1,specs2)
    out[,1][which(out[,1]=="VaR ES")] <- "VaR&ES"
    out[,1][which(out[,1]=="mean sd")] <- "mean&sd"
    pos_m <- which(colnames(out)=="new_mean")
    pos_s <- which(colnames(out)=="new_sd")
    var_pos <- which(out[,1]=="VaR")
    vares_pos <- which(out[,1]=="VaR&ES")
    varm_pos <- which(out[,1]=="mean")
    out[var_pos,pos_m] <- NA
    out[vares_pos,pos_m] <- NA
    out[var_pos,pos_s] <- NA
    out[vares_pos,pos_s] <- NA
    out[varm_pos,pos_s] <- NA
    rownames(out) <- paste("stress",1:n.stress,sep="")
    out1 <- cbind(out[,1:2],round(out[,3:dim(out)[2]],2))
    colnames(out1) <- colnames(out)
    out1
  },caption = htmltools::tags$caption("Stresses Loaded", style="color:black; font-weight: bold"),
  options=list(autoWidth = TRUE,
               columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
               paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE))
  
  ## Plot Variable: ------------------------
  output$plotVariable <- renderUI({
    var <- colnames(Dataset())
    selectInput(inputId = "plotVar", label = "Select Variable", choices = var,selected =var[1],multiple = FALSE)
  })
  
  output$plotVariable2 <- renderUI({
  var <- colnames(Dataset())
  selectInput(inputId = "plotVar2", label = "Select Variables", choices = var,selected = var,multiple = TRUE)
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
    
    plot_t <- input$plotFunction
    if (plot_t=="Histogram") plot_type = "plot_hist"
    if (plot_t=="CDF") plot_type =  "plot_cdf"
    if (plot_t=="Quantiles") plot_type =  "plot_quantile"
    if (plot_t=="Scenario Weights") plot_type =  "plot_weights"
    
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
    probs_s <- paste("Q",probs*100,sep="")
    
    if (isTRUE(performedStress$bool[1])&&(!is.null(var))) {
      
      b_stress <- swim_summary[[1]][,var][1:4]
      base_q <- quantile(x = swim$x[,var],probs = probs)
      base <- c(b_stress,base_q)
      s1 <- swim_summary[[2]][,var][1:4]
      q1 <- quantile_stressed(object = swim,probs = probs,xCol = var,wCol = 1)
      stress1 <- c(s1,q1)
    }
    
    if (isTRUE(performedStress$bool[2])&&(!is.null(var))){ 
      s2 <- swim_summary[[3]][,var][1:4]
      q2 <- quantile_stressed(object = swim,probs = probs,xCol = var,wCol = 2)
      stress2 <- c(s2,q2)
    }
    if (isTRUE(performedStress$bool[3])&&(!is.null(var))) {
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
    var <- input$plotVar2
    
    validate(
      need(length(var)>=1, "Wait..")
    )
    
    args_plot <- list("object"=swim, "xCol" = var, "wCol" = as.numeric(stress), type=plot_type)
  
    if (!is.null(stress)){
      p <- do.call(plot_sensitivity, args_plot)
      p
    }
  },res = 115)
  
  
  output$importance_rank <- renderDataTable(
    {
      
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
      var <- input$plotVar2
      
      validate(
        need(length(var)>=1, "Wait..")
      )
      
      args_plot <- list("object"=swim, "xCol" = var, "wCol" = as.numeric(stress), type=plot_type)
      
      m <- do.call(importance_rank, args_plot)
      
      m1 <- as.data.frame(m)[,-c(1,2)]
      rownames(m1) <- paste("stress",as.numeric(stress),sep="")
      
      if (length(swim)!=0) {
        m1
      }
      
    },caption = htmltools::tags$caption("Importance Ranking", style="color:black; font-weight: bold"),
    options=list(autoWidth = FALSE,
                 columnDefs = list(list(className = 'dt-center',width = '7%', targets = "_all")),
                 paging=FALSE,rownames = TRUE, colnames = TRUE,  scrollX = TRUE, searching=FALSE, ordering = FALSE, info = FALSE, lengthChange = FALSE))
  
  
  
}