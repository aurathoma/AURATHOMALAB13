library(shiny)

# Load spruce data if not already in environment
if (!exists("spruce.df")) {
  spruce.df <- read.csv("SPRUCE.csv")
}

# Helper: fit single-knot piecewise model and return model + R^2
fit_single_knot <- function(xk, x = spruce.df$BHDiameter, y = spruce.df$Height) {
  ix <- (x - xk) * (x > xk)
  mod <- lm(y ~ x + ix)
  list(
    model = mod,
    r2 = summary(mod)$r.squared,
    xk = xk
  )
}

# Helper: predict from single-knot piecewise model across a grid
pred_single_knot <- function(x_grid, xk, mod) {
  coefs <- coef(mod)
  ix_grid <- (x_grid - xk) * (x_grid > xk)
  coefs[1] + coefs[2] * x_grid + coefs[3] * ix_grid
}

x_min <- floor(min(spruce.df$BHDiameter) * 10) / 10
x_max <- ceiling(max(spruce.df$BHDiameter) * 10) / 10

# Precompute R^2 curve across fine grid for global max and plotting
grid_xk <- seq(min(spruce.df$BHDiameter), max(spruce.df$BHDiameter), length.out = 500)
grid_r2 <- vapply(grid_xk, function(k) fit_single_knot(k)$r2, numeric(1))

global_idx <- which.max(grid_r2)
global_xk <- grid_xk[global_idx]
global_r2 <- grid_r2[global_idx]
global_fit <- fit_single_knot(global_xk)
global_y <- pred_single_knot(global_xk, global_xk, global_fit$model)

ui <- fluidPage(
  titlePanel("Spruce Data Set: Piecewise Regression"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "xk",
        label = "Choose knot:",
        min = x_min,
        max = x_max,
        value = round(global_xk, 2),
        step = 0.05
      ),
      sliderInput(
        inputId = "lu_interval",
        label = "Choose L and U for root interval:",
        min = x_min,
        max = x_max,
        value = c(10.40, 14.27),
        step = 0.05
      )
    ),
    mainPanel(
      plotOutput("piecewise_plot", click = "plot_click", height = "340px"),
      plotOutput("r2_plot", height = "300px"),
      tableOutput("opt_table")
    )
  )
)

server <- function(input, output, session) {
  current_xk <- reactiveVal(round(global_xk, 2))

  # Sync slider changes to current_xk
  observeEvent(input$xk, {
    if (!isTRUE(all.equal(current_xk(), input$xk))) {
      current_xk(input$xk)
    }
  })

  # Update current_xk and slider when user clicks the piecewise plot
  observeEvent(input$plot_click, {
    clicked_x <- input$plot_click$x
    if (!is.null(clicked_x) && !is.na(clicked_x)) {
      clamped_x <- min(max(clicked_x, x_min), x_max)
      clamped_x <- round(clamped_x, 2)
      current_xk(clamped_x)
      updateSliderInput(session, "xk", value = clamped_x)
    }
  })

  # Local maximum optimization inside [L, U]
  local_opt <- reactive({
    L <- input$lu_interval[1]
    U <- input$lu_interval[2]
    if (L >= U) {
      U <- min(x_max, L + 0.1)
    }
    opt <- optimize(
      f = function(k) fit_single_knot(k)$r2,
      interval = c(L, U),
      maximum = TRUE
    )
    loc_xk <- opt$maximum
    loc_r2 <- opt$objective
    loc_fit <- fit_single_knot(loc_xk)
    loc_y <- pred_single_knot(loc_xk, loc_xk, loc_fit$model)
    list(
      xk = loc_xk,
      r2 = loc_r2,
      y = loc_y,
      L = L,
      U = U
    )
  })

  output$piecewise_plot <- renderPlot({
    xk_val <- current_xk()
    cur_fit <- fit_single_knot(xk_val)

    x_seq <- seq(min(spruce.df$BHDiameter), max(spruce.df$BHDiameter), length.out = 300)
    y_seq <- pred_single_knot(x_seq, xk_val, cur_fit$model)

    plot(
      spruce.df$BHDiameter, spruce.df$Height,
      pch = 19,
      xlab = "BHDiameter",
      ylab = "Height",
      main = "Piecewise regression"
    )
    lines(x_seq, y_seq, col = "blue", lwd = 2)

    # Green point: global maximum R^2 knot position
    points(global_xk, global_y, pch = 19, col = "green", cex = 1.6)

    # Purple point: local maximum knot position in [L, U]
    loc <- local_opt()
    points(loc$xk, loc$y, pch = 19, col = "purple", cex = 1.6)

    text(
      x = mean(range(spruce.df$BHDiameter)),
      y = min(spruce.df$Height) + 0.08 * diff(range(spruce.df$Height)),
      labels = paste0("R.sq = ", round(cur_fit$r2, 4))
    )
  })

  output$r2_plot <- renderPlot({
    loc <- local_opt()
    plot(
      grid_xk, grid_r2,
      type = "l",
      lwd = 2,
      xlab = "xk",
      ylab = "R^2",
      main = "Determination of possible knots"
    )
    abline(v = loc$xk, col = "gray50", lty = 2)
    points(loc$xk, loc$r2, pch = 19, col = "purple", cex = 1.4)
    points(global_xk, global_r2, pch = 19, col = "green", cex = 1.4)
    text(
      x = loc$xk,
      y = loc$r2,
      labels = paste0("local: ", round(loc$xk, 4)),
      pos = 3
    )
  })

  output$opt_table <- renderTable({
    loc <- local_opt()
    data.frame(
      root = round(loc$xk, 4),
      objective_R2 = round(loc$r2, 6),
      L = round(loc$L, 2),
      U = round(loc$U, 2)
    )
  })
}

shinyApp(ui = ui, server = server)
