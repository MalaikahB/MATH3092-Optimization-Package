library("Optimization")
library('shiny')
library('plotly')


# Define UI for app ----
ui <- navbarPage(
  "Optimization Package",


  #input for using gss function
  tabPanel("Golden Section Search",
           sidebarLayout(
             sidebarPanel(

               textInput("goldfn", "Function", value = "Enter Function..."),
               numericInput("goldlower", "Lower Bound", value = 0),
               numericInput("goldupper" , "Upper Bound", value = 0),
               numericInput("goldtol", "Tolerance", value = 1e-07),
               numericInput("goldmaxit", "Maximum number of Iterations", max = 10, value = 10),
               actionButton("goldupdate", "Compute"),
               uiOutput("goldslide")


             ),

             # Main panel for displaying outputs ----
             mainPanel(

               tabsetPanel(type = "tabs",
                           tabPanel("Plot", plotlyOutput(outputId = "goldplot")),
                           tabPanel("Summary", verbatimTextOutput("goldsummary"))
               ),
               #Output: Example details ---
               h4("Final Results"),
               verbatimTextOutput("golddetail")

             )
           )
  ),

  #input for brents function
  tabPanel("Brent's Method",
           sidebarLayout(
             sidebarPanel(

               textInput("brentsfn", "Function", value = "Enter Function..."),
               numericInput("brentlower", "Lower Bound", value = 0),
               numericInput("brentupper" , "Upper Bound", value = 0),
               numericInput("brenttol", "Tolerance", value = 1e-07),
               numericInput("brentmaxit", "Maximum number of Iterations", max = 20, value = 10),
               actionButton("brentupdate", "Compute"),
               uiOutput("brentslide")

             ),
             mainPanel(

               tabsetPanel(type = "tabs",
                           tabPanel("Plot", plotlyOutput(outputId = "brentsplot")),
                           tabPanel("Summary", verbatimTextOutput("brentssummary"))
               ),
               #Output: Example details ---
               h4("Final Results"),
               verbatimTextOutput("brentsdetail")
             )
           )
  ),
  # input for newtons in 2d
  tabPanel("Newton's Method 2-Dimensional",
           sidebarLayout(
             sidebarPanel(


               textInput("newt2fn", "Function", value = "Enter Function..."),
               numericInput("newt2x0", "Intial x Guess", value = 0),
               numericInput("newt2tol", "Tolerance", value = 1e-07),
               numericInput("newt2maxit", "Maximum number of Iterations", max = 20, value = 10),
               actionButton("newt2update", "Compute"),
               uiOutput("newt2slide")

             ),
             mainPanel(

               tabsetPanel(type = "tabs",
                           tabPanel("Plot", plotlyOutput(outputId = "plot2dnewtons")),
                           tabPanel("Summary", verbatimTextOutput("summary2dnewtons"))
               ),
               #Output: Example details ---
               h4("Final Results"),
               verbatimTextOutput("detail2dnewtons")
             )
           )
  ),
  #input for 3d newtons
  tabPanel("Newton's Method 3-Dimensional",
           sidebarLayout(
             sidebarPanel(

               textInput("newt3fn", "Function", value = "Enter Function..."),
               numericInput("newt3x0", "Intial x1 Guess", value = 0),
               numericInput("newt3y0", "Intial x2 Guess", value = 0),
               numericInput("newt3tol", "Tolerance", value = 1e-07),
               numericInput("newt3maxit", "Maximum number of Iterations", max = 20, value = 10),
               actionButton("newt3update", "Compute")


             ),
             mainPanel(

               tabsetPanel(type = "tabs",
                           tabPanel("Plot", plotlyOutput(outputId = "plot3dnewtons")),
                           tabPanel("Summary", verbatimTextOutput("summary3dnewtons"))
               ),
               #Output: Example details ---
               h4("Final Results"),
               verbatimTextOutput("detail3dnewtons")
             )
           )
  ),
  #input log barrier 2d
  tabPanel("Log-Barrier Method 2-Dimensional",
           sidebarLayout(
             sidebarPanel(

               textInput("lb2fn", "Function", value = "Enter Objective Function..."),
               textInput("A2", "Sum of Inequality RHS", value = "Enter A..."),
               numericInput("b2", "Sum of Inequality LHS", value = 0),
               numericInput("barrier2x0", "Intial Guess", value = 0),
               numericInput("lb2maxit", "Maximum number of Iterations", max = 10 , value = 10),
               numericInput("m2", "Total Number of Constraints", value = 10),
               numericInput("epsilon2", "Enter epsilon", value = 1e-03),
               numericInput("mu2", "Enter Mu", value = 10),
               numericInput("t2", "Enter t", value = 10),
               actionButton("lb2update", "Compute"),
               uiOutput("lbslide")

             ),
             mainPanel(

               tabsetPanel(type = "tabs",
                           tabPanel("Plot", plotlyOutput(outputId = "plot2lb")),
                           tabPanel("Summary", verbatimTextOutput("summary2lb"))
               ),
               #Output: Example details ---
               h4("Final Results"),
               verbatimTextOutput("detail2lb")
             )
           )
  ),
  #input log barrier 3d
  tabPanel("Log-Barrier Method 3-Dimensional",
           sidebarLayout(
             sidebarPanel(

               textInput("lbfn", "Function", value = "Enter Objective Function..."),
               textInput("A", "Sum of Inequality RHS", value = "Enter Constraints..."),
               numericInput("b", "Sum of Inequality LHS", value = 0),
               numericInput("barrierx0", "Intial x1 Guess", value = 0),
               numericInput("barriery0", "Intial x2 Guess", value = 0),
               numericInput("lbmaxit", "Maximum number of Iterations", max = 10 , value = 10),
               numericInput("m", "Total Number of Constraints", value = 10),
               numericInput("mu", "Enter Mu", value = 10),
               numericInput("epsilon3", "Enter epsilon", value = 1e-03),
               numericInput("t", "Enter t", value = 10),
               actionButton("lbupdate", "Compute")

             ),
             mainPanel(

               tabsetPanel(type = "tabs",
                           tabPanel("Plot", plotlyOutput(outputId = "plotlb")),
                           tabPanel("Summary", verbatimTextOutput("summarylb"))
               ),
               #Output: Example details ---
               h4("Final Results"),
               verbatimTextOutput("detaillb")
             )
           )
  )
)




#Here is the server function
server <- function(input, output, session) {


  ##-----GOLDEN
  exampleInputgold <- eventReactive(input$goldupdate, {


    #convert user input into a function
    str_fn <- c(expr = input$goldfn)
    charfn <- as.character(str_fn)

    all_vars <- all.vars(input$goldfn)
    n_vars<-length(all_vars)


    func<-function(){

    }

    functext<-paste0("func<-function(", "x")
    if(n_vars==2){
      functext<-paste0(functext,",", all_vars[2])}
    if(n_vars>2){
      for(i in 2:n_vars){
        functext<-paste0(functext,",",all_vars[i])}}
    functext<-paste0(functext,"){ ")
    functext <- paste0(functext, substr(x = charfn, start = 1, stop = nchar(charfn)), "}")

    golduserfn <- eval(parse(text = functext))
    #run gss on inputs
    gss(golduserfn, input$goldlower, input$goldupper, input$goldmaxit, input$goldtol)
  })

  #golden -- summary
  output$goldsummary <- renderPrint({
    algorithm <- exampleInputgold()
    cat("Function Inputted", "\n")
    print(input$goldfn)
    summary(algorithm)
  })

  #golden -- plot
  output$goldplot <- renderPlotly({
    algorithm <- exampleInputgold()
    x = algorithm$optimal_x[input$goldslide]
    y = algorithm$func(x)


    fig <- plot(algorithm)
    fig <- fig %>% add_trace(x = x, y = y, mode = "markers", name = " Root of Iteration")
    fig


  })

  #golden -- final results
  output$golddetail <- renderPrint({
    algorithm <- exampleInputgold()
    print(algorithm)
  })

  #reactive slider for golden plots
  output$goldslide <- renderUI({
    algorithm <- exampleInputgold()
    sliderInput(inputId = "goldslide",
                label = "Minumum at Iteration:",
                min = 1, max = algorithm$iteration,
                value = 1, step = 1)
  })





  ## ----BRENTS
  exampleInputbrents <- eventReactive(input$brentupdate, {


    #convert user input into a function
    str_fn <- c(expr = input$brentsfn)
    charfn <- as.character(str_fn)

    all_vars <- all.vars(input$brentsfn)
    n_vars<-length(all_vars)


    func<-function(){

    }

    functext<-paste0("func<-function(", "x")
    if(n_vars==2){
      functext<-paste0(functext,",", all_vars[2])}
    if(n_vars>2){
      for(i in 2:n_vars){
        functext<-paste0(functext,",",all_vars[i])}}
    functext<-paste0(functext,"){ ")
    functext <- paste0(functext, substr(x = charfn, start = 1, stop = nchar(charfn)), "}")

    brentuserfn <- eval(parse(text = functext))
    #compute brents method on input
    brents(brentuserfn,
           input$brentlower, input$brentupper,
           input$brentmaxit, input$brenttol)
  })

  #brents -- summary
  output$brentssummary <- renderPrint({
    algorithm <- exampleInputbrents()
    cat("Function Inputted", "\n")
    print(input$brentsfn)
    summary(algorithm)
  })

  # brents -- plot
  output$brentsplot <- renderPlotly({
    algorithm <- exampleInputbrents()
    x = algorithm$root[input$brentslide]
    y = algorithm$func(x)


    fig <- plot(algorithm)
    fig <- fig %>% add_trace(x = x, y = y, mode = "markers", name = " Root of Iteration")
    fig


  })
  #brents -- final results
  output$brentsdetail <- renderPrint({
    algorithm <- exampleInputbrents()
    print(algorithm)
  })

  #slider for brents plot
  output$brentslide <- renderUI({
    algorithm <- exampleInputbrents()
    sliderInput(inputId = "brentslide",
                label = "Minumum at Iteration:",
                min = 1, max = algorithm$iterations,
                value = 1, step = 1)
  })




  ## ----NEWTONS2D
  exampleInput2dnewton <- eventReactive(input$newt2update, {


    #convert user input into a function
    str_fn <- c(expr = input$newt2fn)
    charfn <- as.character(str_fn)

    all_vars <- all.vars(input$newt2fn)
    n_vars<-length(all_vars)


    func<-function(){

    }

    functext<-paste0("func<-function(", "x")
    if(n_vars==2){
      functext<-paste0(functext,",", all_vars[2])}
    if(n_vars>2){
      for(i in 2:n_vars){
        functext<-paste0(functext,",",all_vars[i])}}
    functext<-paste0(functext,"){ ")
    functext <- paste0(functext, substr(x = charfn, start = 1, stop = nchar(charfn)), "}")

    newt2userfn <- eval(parse(text = functext))
    #compute newtons on user inputs
    newtons(newt2userfn,
            input$newt2x0, input$newt2maxit, input$newt2tol)

    })

  #newtons -- summary
  output$summary2dnewtons <- renderPrint({
    algorithm <- exampleInput2dnewton()
    cat("Function Inputted", "\n")
    print(input$newt2fn)
    summary(algorithm)
  })

  #newtons -- plots
  output$plot2dnewtons <- renderPlotly({
    algorithm <- exampleInput2dnewton()
    x = algorithm$optimal_x[input$newt2slide]
    y = algorithm$func(x)


    fig <- plot(algorithm)
    fig <- fig %>% add_trace(x = x, y = y, mode = "markers", name = " Root of Iteration")
    fig


  })
  #newtons -- final results
  output$detail2dnewtons <- renderPrint({
    algorithm <- exampleInput2dnewton()
    print(algorithm)
  })

  #slider for newtons 2d plot
   output$newt2slide <- renderUI({
     algorithm <- exampleInput2dnewton()
      sliderInput(inputId = "newt2slide",
                  label = "Minumum at Iteration:",
                  min = 1, max = algorithm$it,
                  value = 1, step = 1)
    })




  ## ----NEWTONS3D
  exampleInput3dnewton <- eventReactive(input$newt3update, {


    #convert user input into a function
    str_fn <- c(expr = input$newt3fn)
    charfn <- as.character(str_fn)

    all_vars <- all.vars(input$newt3fn)
    n_vars<-length(all_vars)


    func<-function(){

    }

    functext<-paste0("func<-function(", "x")
    if(n_vars==2){
      functext<-paste0(functext,",", all_vars[2])}
    if(n_vars>2){
      for(i in 2:n_vars){
        functext<-paste0(functext,",",all_vars[i])}}
    functext<-paste0(functext,"){ ")
    functext <- paste0(functext, substr(x = charfn, start = 1, stop = nchar(charfn)), "}")

    newt3userfn <- eval(parse(text = functext))
    #compute newtons on inputs
    newtons(newt3userfn,
            c(input$newt3x0, input$newt3y0), input$newt3maxit,
            input$newt3tol)
  })

  #summary of newtons 3d
  output$summary3dnewtons <- renderPrint({
    algorithm <- exampleInput3dnewton()
    cat("Function Inputted", "\n")
    print(input$newt3fn)
    summary(algorithm)
  })

   #plot of newtons 3d
  output$plot3dnewtons <- renderPlotly({
    algorithm <- exampleInput3dnewton()
    fig <- plot(algorithm)
    fig


  })
  #final results newtons 3d
  output$detail3dnewtons <- renderPrint({
    algorithm <- exampleInput3dnewton()
    print(algorithm)
  })

  ##----- LB2

  exampleInput2lb <- eventReactive(input$lb2update, {


    #convert objective fn input into a function
    str_fn <- c(expr = input$lb2fn)
    charfn <- as.character(str_fn)

    all_vars <- all.vars(input$lb2fn)
    n_vars<-length(all_vars)


    func<-function(){

    }

    functext<-paste0("func<-function(", "x")
    if(n_vars==2){
      functext<-paste0(functext,",", all_vars[2])}
    if(n_vars>2){
      for(i in 2:n_vars){
        functext<-paste0(functext,",",all_vars[i])}}
    functext<-paste0(functext,"){ ")
    functext <- paste0(functext, substr(x = charfn, start = 1, stop = nchar(charfn)), "}")

    lb2userfn <- eval(parse(text = functext))


    #constraints -> working fn
    consstr_fn <- c(expr = input$cons2)
    conscharfn <- as.character(consstr_fn)

    cons_vars <- all.vars(input$cons2)
    cons_totvars<-length(cons_vars)

    constext<-paste0("cons<-function(", "x")
    if(cons_totvars==2){
      constext<-paste0(constext,",", cons_vars[2])}
    if(cons_totvars>2){
      for(i in 2:cons_totvars){
        constext<-paste0(constext,",",cons_vars[i])}}
    constext<-paste0(constext,"){ ")
    constext <- paste0(constext, substr(x = conscharfn, start = 1, stop = nchar(conscharfn)), "}")

    usercons2 <- eval(parse(text = constext))

    #log barrier on user inputs
    logbarrier(lb2userfn, usercons2, input$barrier2x0,
               input$lb2maxit, input$m2, input$mu2, input$epsilon2, input$t2)


  })

  #lb -- summary 2d
  output$summary2lb <- renderPrint({
    algorithm <- exampleInput2lb()
    cat("Function Inputted", "\n")
    print(input$lb2fn)
    summary(algorithm)
  })

  #lb -- plot 2d
  output$plo2tlb <- renderPlotly({
    algorithm <- exampleInput2lb()
    x = algorithm$x_optimal[input$lbslide]
    y = algorithm$func(x)


    fig <- plot(algorithm)
    fig <- fig %>% add_trace(x = x, y = y, mode = "markers", name = " Root of Iteration")
    fig

  })

  #lb -- final results 2d
  output$detail2lb <- renderPrint({
    algorithm <- exampleInput2lb()
    print(algorithm)
  })

  #slider for lb 2d plot
  output$lbslide <- renderUI({
    algorithm <- exampleInput2lb()
    sliderInput(inputId = "lbslide",
                label = "Minumum at Iteration:",
                min = 1, max = algorithm$it,
                value = 1, step = 1)
  })


  ##----- LB3

  exampleInputlb <- eventReactive(input$lbupdate, {


    #convert objective fn input into a function
    str_fn <- c(expr = input$lbfn)
    charfn <- as.character(str_fn)

    all_vars <- all.vars(input$lbfn)
    n_vars<-length(all_vars)


    func<-function(){

    }

    functext<-paste0("func<-function(", "x")
    if(n_vars==2){
      functext<-paste0(functext,",", all_vars[2])}
    if(n_vars>2){
      for(i in 2:n_vars){
        functext<-paste0(functext,",",all_vars[i])}}
    functext<-paste0(functext,"){ ")
    functext <- paste0(functext, substr(x = charfn, start = 1, stop = nchar(charfn)), "}")

    lbuserfn <- eval(parse(text = functext))


    #constraints -> working fn
    consstr_fn <- c(expr = input$A)
    conscharfn <- as.character(consstr_fn)

    cons_vars <- all.vars(input$A)
    cons_totvars<-length(cons_vars)

    constext<-paste0("cons<-function(", "x")
    if(cons_totvars==2){
      constext<-paste0(constext,",", cons_vars[2])}
    if(cons_totvars>2){
      for(i in 2:cons_totvars){
        constext<-paste0(constext,",",cons_vars[i])}}
    constext<-paste0(constext,"){ ")
    constext <- paste0(constext, substr(x = conscharfn, start = 1, stop = nchar(conscharfn)), "}")

    usera <- eval(parse(text = constext))
    print(usera)
    print(lbuserfn)

    #log barrier on inputs
    logbarrier(lbuserfn, usera, input$b, c(input$barrierx0, input$barriery0),
               input$lbmaxit, input$m, input$mu, input$epsilon3, input$t)


  })

  #lb --summary 3d
  output$summarylb <- renderPrint({
    algorithm <- exampleInputlb()
    cat("Function Inputted", "\n")
    print(input$lbfn)
    summary(algorithm)
  })

  #lb -- plots 3d
  output$plotlb <- renderPlotly({
    algorithm <- exampleInputlb()
    fig <- plot(algorithm)
    fig


  })

  ##lb -- final results 3d
  output$detaillb <- renderPrint({
    algorithm <- exampleInputlb()
    print(algorithm)
  })


}

#run app
shinyApp(ui = ui, server = server)
