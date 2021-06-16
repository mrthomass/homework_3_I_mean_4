library(shiny)
library(tidyverse)

diamonds1 <- diamonds %>%
  select(carat, price, x, y, z)

# signature color palette
my_cols <- c("#9C27B0", "#AB47BC", "#BA68C8", "#CE93D8", "#E1BEE7")

ui <- fluidPage(
  titlePanel("Frequency Plots"),

  selectInput(
    # diamonds1 vars inputs
    inputId = "DVvar",
    label = "Diamond Variables",
    choices = names(diamonds1)
  ),
  
  # app aesthetics/outputs
  HTML("<br><hr>"),

  plotOutput(outputId = "plot"),
  
  HTML("<br><hr>"),

  plotOutput(outputId = "boxplot"),
  
  HTML("<br><hr>"),

  dataTableOutput(outputId = "table")
)


server <- function(input, output) {
  output$plot <- renderPlot({
    # plots the count of diamonds vs interactive independent variable
    diamonds %>%
      ggplot(aes(x = .data[[input$DVvar]], color = cut)) +
      geom_freqpoly(binwidth = 0.1) +
      labs(title = "Frequency Polygon") +
      theme_bw()
  })

  output$boxplot <- renderPlot({
    # plots each cuts distribution vs interactive independent variable (y)
    diamonds %>%
      ggplot(aes(x = cut, y = .data[[input$DVvar]])) +
      geom_boxplot(aes(group = cut, fill = cut)) +
      theme_bw() +
      theme(legend.position = "none") +
      labs(title = "Boxplot") +
      scale_fill_manual(values = my_cols)
  })

  output$table <- renderDataTable(diamonds1,
    # produces interactive table of full dataset
    options = list(pageLength = 10)
  )
}

shinyApp(ui, server)

