library("Optimization")
library('shiny')
library('plotly')





# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Optimization Package Examples"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

 #      Input: Slider for the number of bins ----
      sliderInput(inputId = "n",
                  label = "Number of iterations:",
                  min = 1,
                  max = 10,
                  value = 10),

      selectInput("example", "Choose an Algorithm:",
                  choices = c("Golden Section Search", "Brent's Method", "Newton's Method", "Log-Barrier Method")),


      actionButton("update", "View Example")



    ),

    # Main panel for displaying outputs ----
    mainPanel(

      #Output : Plots
      h4("Plot"),
      plotlyOutput(outputId = "plot"),

      # Output: Header + summary of distribution ----
      h4("Summary"),
      verbatimTextOutput("summary")


    )
  )
)

#example function for GSS
f <- function(x){
  return((x-2)**2)
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
beale <- function(x){
  return((1.5 - x[1] + x[1]*x[2])**2 + (2.25 - x[1] + x[1]*(x[2]**2))**2 + (2.625 - x[1] + x[1]*(x[2]**3))**2)
} #beale fn
nra <- newtons(beale, c(3.1,0.55))




#Here is the server function for the Hello Shiny example.


server <- function(input, output) {


 # output$GSSplot <- renderPlot({

  #  gs <- gss(f, 1, 5)

  #  plot(xi, fx, type = 'l', xlab = 'x', ylab = 'f(x)', main = "Golden Section Search Algorithm") #+
  #  points(x = (x$lowerinterval[i]), y = f(x$lowerinterval[i]), pch = '|', col = 'green' )
  #  points(x = (x$upperinterval[i]), y = f(x$upperinterval[i]), pch = '|', col = 'green')
#    points(x = gs$min[n], y = 3, pch = 18, col = 'blue')



#   })

  exampleInput <- eventReactive(input$update, {
    switch(input$example,
           "Golden Section Search" = gs,
           "Brent's Method" = exampbrents,
           "Newton's Method" = nra,
           "Log-Barrier Method" = lbexample)
  })

  output$summary <- renderPrint({
    algorithm <- exampleInput()
    summary(algorithm)
  })

  output$plot <- renderPlotly({
    plot(algorithm)
  })


}

shinyApp(ui = ui, server = server)



