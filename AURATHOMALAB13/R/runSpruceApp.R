#' Run the Spruce Interactive Shiny App
#'
#' Launches a Shiny application that visualizes the SPRUCE dataset
#' with LOESS, linear, and quadratic models and interactive brushing.
#'
#' @export
runSpruceApp <- function() {
  library(shiny)
  library(ggplot2)
  library(dplyr)

  spruce <- read.csv("SPRUCE.csv")

  format_quad_subtitle <- function(b0, b1, b2) {
    b0_fmt <- sprintf("%.4f", b0)
    b1_fmt <- sprintf("%.4f", abs(b1))
    b2_fmt <- sprintf("%.4f", abs(b2))
    sign1 <- if (b1 >= 0) "+" else "-"
    sign2 <- if (b2 >= 0) "+" else "-"

    bquote(
      widehat(Height) == .(b0_fmt) ~ .(sign1) ~ .(b1_fmt) %*% BHDiameter ~
        .(sign2) ~ .(b2_fmt) %*% BHDiameter^2
    )
  }

  ui <- fluidPage(
    titlePanel("SPRUCE Tree Analysis: Interactive Models & Brushing"),
    sidebarLayout(
      sidebarPanel(
        p("Drag a brush box over points on any plot to remove them and refit the model."),
        actionButton("reset", "Reset Brushed Data"),
        hr(),
        textOutput("rowCount"),
        hr(),
        helpText(
          "Explore the LOESS smoother, linear regression model, and quadratic regression model ",
          "with live updates as brushed points are removed."
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "1. LOESS Smoother",
            br(),
            plotOutput("loessPlot", brush = "plot_brush")
          ),
          tabPanel(
            "2. Linear Model",
            br(),
            plotOutput("linearPlot", brush = "plot_brush")
          ),
          tabPanel(
            "3. Quadratic Model",
            br(),
            plotOutput("quadPlot", brush = "plot_brush")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    current_data <- reactiveVal(spruce)

    observeEvent(input$reset, {
      current_data(spruce)
      session$resetBrush("plot_brush")
    })

    observeEvent(input$plot_brush, {
      df <- current_data()
      brushed <- brushedPoints(df, input$plot_brush)

      if (nrow(brushed) > 0) {
        remaining <- anti_join(df, brushed, by = names(spruce))
        current_data(remaining)
        session$resetBrush("plot_brush")
      }
    })

    output$rowCount <- renderText({
      df <- current_data()
      paste0("Showing ", nrow(df), " of ", nrow(spruce), " observations.")
    })

    output$loessPlot <- renderPlot({
      df <- current_data()
      req(nrow(df) >= 3)

      ggplot(df, aes(x = BHDiameter, y = Height)) +
        geom_point(color = "darkblue") +
        stat_smooth(method = "loess", se = TRUE, color = "blue", fill = "grey") +
        labs(
          title = "SPRUCE: LOESS Smoother",
          x = "BHDiameter",
          y = "Height"
        ) +
        theme_minimal()
    })

    output$linearPlot <- renderPlot({
      df <- current_data()
      req(nrow(df) >= 2)

      lm_fit <- lm(Height ~ BHDiameter, data = df)
      coefs <- coef(lm_fit)
      b0 <- unname(coefs[1])
      b1 <- unname(coefs[2])

      b0_fmt <- sprintf("%.4f", b0)
      b1_fmt <- sprintf("%.4f", abs(b1))
      sign1 <- if (b1 >= 0) "+" else "-"

      ggplot(df, aes(x = BHDiameter, y = Height)) +
        geom_point(color = "darkblue") +
        stat_smooth(method = "lm", se = TRUE, color = "blue", fill = "grey") +
        labs(
          title = "SPRUCE: Linear Model",
          subtitle = bquote(widehat(Height) == .(b0_fmt) ~ .(sign1) ~ .(b1_fmt) %*% BHDiameter),
          x = "BHDiameter",
          y = "Height"
        ) +
        theme_minimal()
    })

    output$quadPlot <- renderPlot({
      df <- current_data()
      req(nrow(df) >= 3)

      quad_fit <- lm(Height ~ BHDiameter + I(BHDiameter^2), data = df)
      coefs <- coef(quad_fit)
      b0 <- unname(coefs[1])
      b1 <- unname(coefs[2])
      b2 <- unname(coefs[3])

      ggplot(df, aes(x = BHDiameter, y = Height)) +
        geom_point(color = "darkblue") +
        stat_smooth(
          method = "lm",
          formula = y ~ x + I(x^2),
          se = TRUE,
          color = "blue",
          fill = "grey"
        ) +
        labs(
          title = "SPRUCE: Quadratic Model",
          subtitle = format_quad_subtitle(b0, b1, b2),
          x = "BHDiameter",
          y = "Height"
        ) +
        theme_minimal()
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}
