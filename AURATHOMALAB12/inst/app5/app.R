library(shiny)
library(AURATHOMALAB12)

ui <- fluidPage(
  titlePanel("Task 5: Interactive Power App with Clickable Plot"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Sample Size (n):", min = 1, max = 30, value = 10, step = 1),
      numericInput("delta", "Delta (effect size):", value = 2, min = 0.1, max = 10, step = 0.1),
      numericInput("sigma", "Sigma (std deviation):", value = 2, min = 0.1, max = 10, step = 0.1),
      sliderInput("alpha", "Significance Level (alpha):", min = 0.01, max = 0.10, value = 0.05, step = 0.005)
    ),

    mainPanel(
      plotOutput("powerPlot", click = "plot_click"),
      br(),
      h4("Clicked Point Information (4 Decimal Places):"),
      tableOutput("clickTable")
    )
  )
)

server <- function(input, output, session) {

  # Reactive expression to compute curve data
  curveData <- reactive({
    n_vals <- 0:30
    power_vals <- sapply(n_vals, function(ni) {
      if (ni == 0) return(0)
      pnorm(qnorm(1 - input$alpha/2) - (input$delta * sqrt(ni)) / input$sigma) +
        pnorm(-qnorm(1 - input$alpha/2) - (input$delta * sqrt(ni)) / input$sigma)
    })
    data.frame(n = n_vals, power = power_vals)
  })

  # Render the power curve plot
  output$powerPlot <- renderPlot({
    df <- curveData()
    plot(df$n, df$power, type = "l", col = "black", lwd = 2,
         ylim = c(0, 1), xlim = c(0, 30),
         xlab = "n", ylab = "Power",
         main = paste0("Power Calculation: delta = ", input$delta,
                       ", sigma = ", input$sigma, ", alpha = ", input$alpha))

    # Add a point for the currently selected sample size
    current_power <- if(input$n == 0) 0 else {
      pnorm(qnorm(1 - input$alpha/2) - (input$delta * sqrt(input$n)) / input$sigma) +
        pnorm(-qnorm(1 - input$alpha/2) - (input$delta * sqrt(input$n)) / input$sigma)
    }
    points(input$n, current_power, col = "red", pch = 19, cex = 1.5)
  })

  # Render table showing 4-decimal value upon plot click
  output$clickTable <- renderTable({
    req(input$plot_click)
    click_x <- input$plot_click$x
    click_y <- input$plot_click$y

    # Calculate power based on clicked x coordinate (sample size n)
    approx_n <- round(click_x)
    if (approx_n < 0) approx_n <- 0
    if (approx_n > 30) approx_n <- 30

    clicked_power <- if(approx_n == 0) 0 else {
      pnorm(qnorm(1 - input$alpha/2) - (input$delta * sqrt(approx_n)) / input$sigma) +
        pnorm(-qnorm(1 - input$alpha/2) - (input$delta * sqrt(approx_n)) / input$sigma)
    }

    data.frame(
      Selected_Sample_Size_n = approx_n,
      Clicked_X_Coord = round(click_x, 4),
      Clicked_Y_Coord = round(click_y, 4),
      Calculated_Power = round(clicked_power, 4)
    )
  })
}

shinyApp(ui = ui, server = server)
