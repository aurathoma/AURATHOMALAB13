library(shiny)
library(AURATHOMALAB12)

ui <- fluidPage(
  titlePanel("Task 3: Interactive Alpha Level Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Select Alpha Level:", min = 0.01, max = 0.10, value = 0.05, step = 0.005),
      numericInput("iter", "Simulation Iterations:", value = 1000, min = 100, max = 5000)
    ),
    mainPanel(
      plotOutput("logLambdaPlot"),
      plotOutput("ybarPlot"),
      verbatimTextOutput("cutoffStats")
    )
  )
)

server <- function(input, output, session) {
  y_data <- c(4.4562, 5.4889, 4.4074, 5.0774, 4.9697, 4.6756, 4.8956, 5.9755, 4.6879, 5.1052)

  sim_results <- reactive({
    mylambda(iter = input$iter, y = y_data, alpha = input$alpha)
  })

  output$logLambdaPlot <- renderPlot({
    res <- sim_results()
    hist(res$xstat1, probability = TRUE, col = "sienna",
         main = paste("Distribution of log(lambda) with Alpha =", input$alpha), xlab = "log(lambda)")
    abline(v = res$loglamcut, col = "blue", lwd = 2)
  })

  output$ybarPlot <- renderPlot({
    res <- sim_results()
    hist(res$xstat2, probability = TRUE, col = "khaki",
         main = paste("Distribution of Ybar with Alpha =", input$input), xlab = "Ybar")
    abline(v = res$ybarcut, col = "blue", lwd = 2)
  })

  output$cutoffStats <- renderPrint({
    res <- sim_results()
    cat("Log Lambda Cutoff:", res$loglamcut, "\n")
    cat("Ybar Cutoff (Empirical):", res$ybarcut, "\n")
    cat("Ybar Cutoff (Formula):", res$ybarcutest, "\n")
  })
}

shinyApp(ui = ui, server = server)
