library(tidyverse)
library(shiny)

data <- read_csv("BodyFat.csv")

# A function to reset the selection based on brush interaction
reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}

# A function to create scatter plots without legends and add units to axis labels
scatter <- function(x, selected_, var1, var2, var2_unit) {
  x %>%
    mutate(selected_ = selected_) %>%
    ggplot(aes_string(var1, var2)) +
    geom_point(aes(alpha = as.numeric(selected_))) + 
    scale_alpha(range = c(0.1, 1)) + 
    labs(x = "Body Fat (%)", y = paste(var2, "(", var2_unit, ")")) +  # Add units to axis labels
    theme(legend.position = "none") 
}

# Linear regression model
model <- lm(BODYFAT ~ ABDOMEN + WEIGHT, data = data)

# Predict body fat percentage based on the linear model
predict_body_fat <- function(weight, abdomen) {
  intercept <- coef(model)["(Intercept)"]
  coef_abdomen <- coef(model)["ABDOMEN"]
  coef_weight <- coef(model)["WEIGHT"]
  predicted_y <- intercept + coef_abdomen * abdomen + coef_weight * weight
  return(predicted_y)
}

ui <- fluidPage(
  titlePanel("Body Fat Analysis and Prediction"),
  
  tabsetPanel(
    # First tab: Scatter plot visualization
    tabPanel("Scatter Plots", 
             p("This Shiny app visualizes the relationships between body fat percentage and other physical measurements in the dataset. You can brush over points in any scatter plot to filter the data, and the selected points will appear in the table below."),
             
             h4("Variable explanations:"),
             p("1. DENSITY: Body density (gm/cm³) 2. AGE: Age (years) 3. WEIGHT: Weight (lb) 4. HEIGHT: Height (inches)"),
             p("5. ADIPOSITY: Adiposity index (kg/m²) 6. NECK: Neck circumference (cm) 7. CHEST: Chest circumference (cm) 8. ABDOMEN: Abdomen circumference (cm)"),
             p("9. HIP: Hip circumference (cm) 10. THIGH: Thigh circumference (cm) 11. KNEE: Knee circumference (cm) 12. ANKLE: Ankle circumference (cm)"),
             p("13. BICEPS: Biceps circumference (cm) 14. FOREARM: Forearm circumference (cm) 15. WRIST: Wrist circumference (cm)"),
             
             fluidRow(
               column(4, plotOutput("scatter1", brush = "plot_brush1")),
               column(4, plotOutput("scatter2", brush = "plot_brush2")),
               column(4, plotOutput("scatter3", brush = "plot_brush3"))
             ),
             fluidRow(
               column(4, plotOutput("scatter4", brush = "plot_brush4")),
               column(4, plotOutput("scatter5", brush = "plot_brush5")),
               column(4, plotOutput("scatter6", brush = "plot_brush6"))
             ),
             fluidRow(
               column(4, plotOutput("scatter7", brush = "plot_brush7")),
               column(4, plotOutput("scatter8", brush = "plot_brush8")),
               column(4, plotOutput("scatter9", brush = "plot_brush9"))
             ),
             fluidRow(
               column(4, plotOutput("scatter10", brush = "plot_brush10")),
               column(4, plotOutput("scatter11", brush = "plot_brush11")),
               column(4, plotOutput("scatter12", brush = "plot_brush12"))
             ),
             fluidRow(
               column(4, plotOutput("scatter13", brush = "plot_brush13")),
               column(4, plotOutput("scatter14", brush = "plot_brush14")),
               column(4, plotOutput("scatter15", brush = "plot_brush15"))
             ),
             dataTableOutput("table")
    ),
    
    # Second tab: Body fat prediction page
    tabPanel("Body Fat Predictor",
             fluidRow(
               column(6,
                      div(style = "background-color: #f7f7f7; padding: 40px; border-radius: 8px;",
                          h4("We use a regression model built on Weight and Abdomen measurements to predict body fat percentage.", align = "center"),
                          h4("Please input your Weight and Abdomen:", align = "center"),
                          numericInput("WEIGHT", "Weight (lbs):", value = NA, min = 50, max = 500, width = "100%"),
                          numericInput("ABDOMEN", "Abdomen circumference (cm):", value = NA, min = 50, max = 200, width = "100%"),
                          actionButton("predict", "Predict Body Fat", class = "btn-primary", style = "width: 100%; margin-top: 20px;"),
                          textOutput("bodyfat_result"),
                          verbatimTextOutput("warning_message"),
                          div(style = "margin-top: 40px;", 
                              h4("Prediction Result", align = "center"),
                              verbatimTextOutput("prediction")
                          )
                      )
               ),
               column(6,
                      div(style = "border: 2px solid #007BFF; padding: 20px; border-radius: 8px;",
                          h4("Model Summary and Explanation"),
                          p("This linear regression model was built using the Abdomen and Weight measurements from a dataset. The formula for predicting body fat percentage is:"),
                          p("Body Fat % = -41.35 + 0.915 × Abdomen - 0.137 × Weight", style = "font-weight: bold;"),
                          plotOutput("contour_plot")
                      )
               )
             )
    )
  )
)

server <- function(input, output) {
  
  variables <- c("DENSITY", "AGE", "WEIGHT", "HEIGHT", "ADIPOSITY", "NECK", 
                 "CHEST", "ABDOMEN", "HIP", "THIGH", "KNEE", "ANKLE", 
                 "BICEPS", "FOREARM", "WRIST")
  
  # Corresponding units for the variables
  units <- c("gm/cm³", "years", "lb", "inches", "kg/m²", "cm", 
             "cm", "cm", "cm", "cm", "cm", "cm", 
             "cm", "cm", "cm")
  
  selected <- reactiveVal(rep(TRUE, nrow(data)))
  
  lapply(seq_along(variables), function(i) {
    observeEvent(input[[paste0("plot_brush", i)]], {
      selected(reset_selection(data, input[[paste0("plot_brush", i)]]))
    })
  })
  
  lapply(seq_along(variables), function(i) {
    output[[paste0("scatter", i)]] <- renderPlot({
      scatter(data, selected(), "BODYFAT", variables[i], units[i])
    })
  })
  
  output$table <- renderDataTable({
    filter(data, selected())
  })
  
  observeEvent(input$predict, {
    inputs <- c(
      WEIGHT = input$WEIGHT,
      ABDOMEN = input$ABDOMEN
    )
    
    if (any(is.na(inputs))) {
      output$prediction <- renderPrint({
        cat("Error: Please fill in all predictor values.")
      })
    } else {
      predicted_y <- predict_body_fat(weight = input$WEIGHT, abdomen = input$ABDOMEN)
      
      output$prediction <- renderPrint({
        cat("Predicted Body Fat is", round(predicted_y, 2), "%")
      })
      
      if (predicted_y < 5 || predicted_y > 50) {
        output$warning_message <- renderText({
          paste("Warning: The predicted body fat percentage (", round(predicted_y, 2), "%) is outside the normal range!")
        })
      } else {
        output$warning_message <- renderText({ "" })
      }
    }
  })
  
  # Contour plot
  output$contour_plot <- renderPlot({
    abdomen_range <- seq(min(data$ABDOMEN), max(data$ABDOMEN), length.out = 500)
    weight_range <- seq(min(data$WEIGHT), max(data$WEIGHT), length.out = 500)
    
    grid_data <- expand.grid(ABDOMEN = abdomen_range, WEIGHT = weight_range)
    grid_data$PREDICTED_BODYFAT <- predict(model, newdata = grid_data) 
    
    blue_shades <- c("#deebf7", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
    
    grid_data$BODYFAT_CATEGORY <- cut(grid_data$PREDICTED_BODYFAT,
                                      breaks = c(-Inf, 6, 14, 18, 25, Inf), 
                                      labels = c("Below Athlete", "Athletes (6-14%)", 
                                                 "Fit (14-18%)", "Average (18-25%)", "Obese (25%+)"))
    
    plot <- ggplot(grid_data, aes(x = ABDOMEN, y = WEIGHT, z = PREDICTED_BODYFAT)) +
      geom_contour_filled(aes(fill = BODYFAT_CATEGORY)) +
      labs(title = "Contour Plot of Predicted Body Fat",
           x = "Abdomen (cm)",
           y = "Weight (lb)", 
           fill = "Body Fat Category") +
      theme_minimal() +
      scale_fill_manual(values = blue_shades)
    
    # Mark the input position
    if (!is.na(input$WEIGHT) && !is.na(input$ABDOMEN)) {
      plot <- plot + 
        geom_point(aes(x = input$ABDOMEN, y = input$WEIGHT), color = "red", size = 3) +
        geom_text(aes(x = input$ABDOMEN, y = input$WEIGHT, label = "Your Input"), 
                  vjust = -1.5, color = "red", size = 4)
    }
    plot
  })
}

shinyApp(ui = ui, server = server)
