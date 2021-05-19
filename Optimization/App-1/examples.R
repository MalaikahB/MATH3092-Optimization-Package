library("Optimization")
library('shiny')
library('plotly')


# Define UI for app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Optimization Package Examples"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "n",
                  label = "Number of iterations:",
                  min = 1,
                  max = 15,
                  value = 15),

      selectInput("example", "Choose an Algorithm:",
                  choices = c("Golden Section Search", "Brent's Method", "Newton's Method", "Log-Barrier Method")),


      actionButton("update", "View Example")


    ),


    # Main panel for displaying outputs ----
    mainPanel(


      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotlyOutput(outputId = "plot")),
                  tabPanel("Summary", verbatimTextOutput("summary"))
                  ),
      #Output: Example details ---
      h4("Results"),
      verbatimTextOutput("detail"),


      #second main pane detailing each iteration indicated by slider
      h4("Iteration Detail"),
      verbatimTextOutput("iteration")
      )
  )
)


#example function for GSS
f <- function(x){
  return((x- 2)**2)
}
gs <- gss(f, 1, 5)

#test 2
brentex <- function(x){
  return((x+3)*((x+1)**2))
}
exampbrents <- brents(brentex, -4, 4/3)

#Rosenbrock function constrained to a disk
ex1 <- function(x){return((1 - x[1])**2 + 100*(x[2] - x[1]**2)**2)}
ex2 <- function(x){return((x[1] - 1)**3 - 1)}
lbexample <-logbarrier(ex1, ex2, c(1.2, 1.2), m = 2, mu = 3)

#example for Newton's Method
matyas <- function(x){
  return(0.26*(x[1]**2 + x[2]**2) - 0.48*x[1]*x[2] )
}
newtex <- newtons(matyas, c(0.02, 0.05))




#Here is the server function for the Hello Shiny example.


server <- function(input, output) {


  exampleInput <- eventReactive(input$update, {
    switch(input$example,
           "Golden Section Search" = gs,
           "Brent's Method" = exampbrents,
           "Newton's Method" = newtex,
           "Log-Barrier Method" = lbexample)
  })

  output$summary <- renderPrint({
    algorithm <- exampleInput()
    summary(algorithm)
  })

  output$plot <- renderPlotly({
    algorithm <- exampleInput()
    x = algorithm$root[input$n]
    y = algorithm$func(x)


    fig <- plot(algorithm)
      fig <- fig %>% add_trace(x = x, y = y, mode = "markers")
      fig


  })

  output$detail <- renderPrint({
    algorithm <- exampleInput()
    print(algorithm)
  })


  output$iteration <-renderPrint({
    algorithm <- exampleInput()
    print(summary(algorithm)[input$n, ])

  })

}

shinyApp(ui = ui, server = server)
