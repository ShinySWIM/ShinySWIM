server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30*1024^2)
  # Upload data functions -------------------
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
              c(list(input$file$datapath),
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
  
  output$qunatiletable <- renderTable({
    probs <- c(0, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.995,1)
    return(
      apply(Dataset(), 2, 
            quantile, 
            probs)  
    )
  }, rownames = TRUE)
  
  
  ## Stress argument names: -------------------------
  StressVarArgNames <- reactive({
    Names <- names(formals(input$stressFunction)[-1])
    Names <- Names[Names!="..."]
    return(Names)
  })
  
  ## Create conditional stress UI ----------------
  output$condPanels <- renderUI({
    
    if (length(ArgNames())==0) return(NULL)
    
    tagList(
      lapply(StressVarArgNames(), function(arg){
        conditionalPanel(
          condition = "input.stressFunction != ''",
          textInput(paste0("stress_arg_", arg), arg)
        )
      })
    )
  })
  
  ## Create stress VaR object: ------------------------
  stress_VaR_obj <- eventReactive(input$run_stress, {
    
    stress_VaR_args <- StressVarArgNames()
    
    argList <- list()
    for (i in seq_along(stress_VaR_args))
    {
      argList[[i]] <- eval(parse(text=input[[paste0("stress_arg_", stress_VaR_args[i])]]))
    }
    names(argList) <- gsub(paste0("^",input$stressFunction,"__"),"",
                           stress_VaR_args)
   
    
    argList <- argList[names(argList) %in% StressVarArgNames()]
    
    stress_obj <- 
      do.call(input$stressFunction,
              c(list(Dataset()),
                argList))
    
    return(stress_obj)
  })
  
  output$SummaryStressBase <- renderTable({

    swim <- stress_VaR_obj()
    swim_summary <- summary(swim, base = TRUE)
    return(swim_summary$base)
  }, rownames = TRUE)
  
  output$SummaryStress <- renderTable({
    
    swim <- stress_VaR_obj()
    swim_summary <- summary(swim, base = TRUE)
    return(swim_summary[2])
  }, rownames = TRUE)
  
  
}
