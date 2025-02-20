library(shiny)
library(plotly)
library(RColorBrewer)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Money Simulation Over Time"),
  sidebarLayout(
    sidebarPanel(
      numericInput("p", "Probability of Success:", 0.5, min = 0, max = 1, step = 0.01),
      numericInput("moneyget", "Money Won on Success:", 2, min = 1),
      numericInput("moneypay", "Money Lost on Failure:", 2, min = 1),
      numericInput("samplenumber", "Number of Simulations:", 1, min = 1, max = 20),
      numericInput("goal", "Goal Money:", 150, min = 50),
      numericInput("ourmoney", "Starting Money:", 100, min = 1),
      numericInput("nsim", "Number of Simulation Steps:", 400, min = 1, max = 1000),
      numericInput("step_interval", "Step Interval for Animation:", 20, min = 1, max = 100)
    ),
    mainPanel(
      plotlyOutput("animatedPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$animatedPlot <- renderPlotly({
    # Initialize lists to store data for each simulation
    simulations_data <- list()
    
    # Run the simulations and store data for each sample
    for (start in 1:input$samplenumber) {
      x1 <- numeric()
      y1 <- numeric()
      startmoney <- input$ourmoney
      for (trials in 1:input$nsim) {
        x <- rbinom(1, 1, input$p)
        if (x == 1) {
          startmoney <- startmoney + input$moneyget
        } else {
          startmoney <- startmoney - input$moneypay
        }
        x1 <- c(x1, trials)
        y1 <- c(y1, startmoney)
        if (startmoney <= 0 || startmoney >= input$goal) break
      }
      simulations_data[[start]] <- list(x = x1, y = y1)
    }
    
    # Prepare the data for Plotly animation
    frame_list <- list()
    colors <- brewer.pal(min(input$samplenumber, 9), "Set1")
    
    for (i in seq(1, input$nsim, by = input$step_interval)) {
      x_frame <- numeric()
      y_frame <- numeric()
      color_frame <- character()
      
      for (start in 1:input$samplenumber) {
        sim_data <- simulations_data[[start]]
        if (i <= length(sim_data$x)) {
          x_frame <- c(x_frame, sim_data$x[1:i])
          y_frame <- c(y_frame, sim_data$y[1:i])
          color_frame <- c(color_frame, rep(colors[start], length(sim_data$x[1:i])))
        }
      }
      frame_list[[i]] <- data.frame(x = x_frame, y = y_frame, color = color_frame, frame = rep(i, length(x_frame)))
    }
    
    data_plot <- do.call(rbind, frame_list)
    
    plot_ly(data = data_plot, 
            x = ~x, y = ~y, frame = ~frame, color = ~color,
            type = 'scatter', mode = 'lines',
            line = list(width = 2)) %>%
      layout(title = "Simulation of Money Over Time",
             xaxis = list(title = "Number of Simulations (n)"),
             yaxis = list(title = "Money", range = c(0, input$goal)),
             showlegend = FALSE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

