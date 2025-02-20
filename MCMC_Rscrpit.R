library(shiny)
source("MCMC.R")
# Define UI for application
ui <- fluidPage(
  titlePanel("MCMC"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("show", "Show burn-in:", choices = c("No", "Yes")),
      selectInput("func_choice", "Choose one distribution:", choices = c("Normal", "Uniform", "Gamma")),
      sliderInput("sd", "SD for proposal distribution:", min = 0, max = 10, value = 1, step = 0.1),
      numericInput("Trials", "Number of trials", 100, min = 1, max = 10000000),
      numericInput("Sample", "Number of samples", 1, min = 1, max = 10000),
      uiOutput("param_inputs"),  # Dynamic UI for additional parameters
      uiOutput("param_inputs2")
    ),
    
    mainPanel(
      plotOutput(outputId = "main_plot", height = "500px")
    )
  )
)

server <- function(input, output) {
  output$param_inputs2<-renderUI({if (input$show == "Yes") {
    numericInput("xlim", "Range of xlim:", 50, min = 0, max = 1000000)
  }})
  
  output$param_inputs <- renderUI({
    req(input$func_choice)
    
    if (input$func_choice == "Normal") {
      tagList(
        numericInput("x", "Input mean:", 0, min = -100, max = 100),
        numericInput("y", "Input SD:", 1, min = 0, max = 100)
      )
    } else if (input$func_choice == "Uniform") {
      tagList(
        numericInput("lower_bound", "Lower:", -1, min = -100, max = 100),
        numericInput("upper_bound", "Upper:", 1, min = -100, max = 100)
      )
    } else if (input$func_choice == "Gamma") {
      tagList(
        numericInput("z", "Give lambda:", 1, min = 0.0001, max = 100),
        numericInput("a", "Give alpha:", 1, min = 0.0001, max = 100)
      )
    }
    
    # Burn-in range input when "Yes" is selected
    
  })
  
  targetnormal <- reactive({
    req(input$func_choice)
    
    if (input$func_choice == "Normal") {
      req(input$x, input$y)
      return(function(x) dnorm(x, mean = input$x, sd = input$y))
    } else if (input$func_choice == "Uniform") {
      req(input$lower_bound, input$upper_bound)
      return(function(x) dunif(x, min = input$lower_bound, max = input$upper_bound))
    } else if (input$func_choice == "Gamma") {
      req(input$z, input$a)
      return(function(x) dgamma(x, shape = input$z, scale = input$a))
    }
  })
  
  reactive_k <- reactive({
    req(targetnormal())
    # Call the Test function (make sure this function is defined)
    Test(input$Sample, input$Trials, input$sd, targetnormal())
  })
  
  output$main_plot <- renderPlot({
    req(input$func_choice)
    req(targetnormal())
    req(input$show)
    k <- reactive_k()
    kk <- targetnormal()
    if (input$show == "No") {
      plotfunc(k)
      curve(kk, add = TRUE, lwd = 2.5)
    }
    else if (input$show == "Yes") {
      plotburnin(input$Sample,input$xlim,k)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

