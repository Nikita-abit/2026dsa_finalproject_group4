#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(tidymodels)
library(vip)
library(here)

# Load saved data/model objects
yield_data <- readRDS(here("output", "yield_model_data.rds"))
xgb_pred <- readRDS(here("output", "xgb_validation_predictions.rds"))
xgb_model <- readRDS(here("output", "final_xgb_model.rds"))

# Calculate metrics
rmse_val <- rmse(xgb_pred, truth = yield_mg_ha, estimate = .pred) %>%
  pull(.estimate)

r2_val <- rsq(xgb_pred, truth = yield_mg_ha, estimate = .pred) %>%
  pull(.estimate)

ui <- fluidPage(
  
  titlePanel("XGBoost Yield Prediction Results"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "predictor",
        label = "Choose predictor for EDA:",
        choices = names(yield_data)[names(yield_data) != "yield_mg_ha"]
      ),
      
      sliderInput(
        inputId = "bins",
        label = "Number of bins for yield histogram:",
        min = 10,
        max = 60,
        value = 30
      )
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel(
          "Yield EDA",
          plotOutput("yield_dist")
        ),
        
        tabPanel(
          "Predictor EDA",
          plotOutput("predictor_plot")
        ),
        
        tabPanel(
          "Variable Importance",
          plotOutput("vip_plot")
        ),
        
        tabPanel(
          "Predicted vs Observed",
          h4(paste("RMSE:", round(rmse_val, 3))),
          h4(paste("R²:", round(r2_val, 3))),
          plotOutput("pred_plot")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # EDA on yield from training data
  output$yield_dist <- renderPlot({
    ggplot(yield_data, aes(x = yield_mg_ha)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = input$bins,
                     fill = "skyblue",
                     color = "black") +
      geom_density(color = "red", linewidth = 1) +
      labs(
        title = "Distribution of Yield",
        x = "Yield (Mg/ha)",
        y = "Density"
      ) +
      theme_minimal()
  })
  
  # Interactive EDA on selected predictor
  output$predictor_plot <- renderPlot({
    
    selected_var <- input$predictor
    
    if (is.numeric(yield_data[[selected_var]])) {
      ggplot(yield_data, aes(x = .data[[selected_var]])) +
        geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
        labs(
          title = paste("Distribution of", selected_var),
          x = selected_var,
          y = "Count"
        ) +
        theme_minimal()
    } else {
      yield_data %>%
        count(.data[[selected_var]]) %>%
        arrange(desc(n)) %>%
        slice_head(n = 15) %>%
        ggplot(aes(x = reorder(.data[[selected_var]], n), y = n)) +
        geom_col(fill = "orange") +
        coord_flip() +
        labs(
          title = paste("Top categories of", selected_var),
          x = selected_var,
          y = "Count"
        ) +
        theme_minimal()
    }
  })
  
  # Variable importance plot
  output$vip_plot <- renderPlot({
    xgb_model %>%
      extract_fit_parsnip() %>%
      vip(num_features = 15) +
      labs(title = "Top Important Variables - XGBoost")
  })
  
  # Predicted vs observed plot
  output$pred_plot <- renderPlot({
    ggplot(xgb_pred, aes(x = yield_mg_ha, y = .pred)) +
      geom_point(alpha = 0.4) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      annotate(
        "text",
        x = min(xgb_pred$yield_mg_ha, na.rm = TRUE),
        y = max(xgb_pred$.pred, na.rm = TRUE),
        label = paste0(
          "RMSE = ", round(rmse_val, 3),
          "\nR² = ", round(r2_val, 3)
        ),
        hjust = 0
      ) +
      labs(
        title = "Predicted vs Observed Yield",
        x = "Observed Yield (Mg/ha)",
        y = "Predicted Yield (Mg/ha)"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)