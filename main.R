library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
covid_data <- read.csv(url)

covid_data_long <- covid_data %>%
  pivot_longer(cols = starts_with("X"), names_to = "date", values_to = "cases") %>%
  mutate(date = as.Date(gsub("X", "", date), format = "%m.%d.%y")) %>%
  group_by(Country.Region, date) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  ungroup()

ui <- fluidPage(
  titlePanel("COVID-19 Fallzahlen weltweit"),

  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Wähle ein Land:", choices = unique(covid_data$Country.Region)),
      dateRangeInput("date_range", "Wähle Datumsbereich:",
                     start = min(covid_data_long$date),
                     end = max(covid_data_long$date)),
      checkboxInput("log_scale", "Logarithmische Skala", FALSE),
      colorInput("line_color", "Wähle Linienfarbe:", value = "blue"),
      textInput("plot_title", "Diagrammtitel:", value = "COVID-19 Fälle"),
      checkboxInput("smoothing", "Glättungslinie hinzufügen", FALSE)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Kumulative Fälle", plotOutput("cumulative_plot")),
        tabPanel("Tägliche Fälle", plotOutput("daily_plot")),
        tabPanel("Daten anzeigen", tableOutput("data_table")),
        downloadButton("download_plot", "Plot herunterladen"),
        downloadButton("download_data", "Daten herunterladen")
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    covid_data_long %>%
      filter(Country.Region == input$country,
             date >= input$date_range[1] & date <= input$date_range[2])
  })

  output$cumulative_plot <- renderPlot({
    data <- filtered_data()
    p <- ggplot(data, aes(x = date, y = total_cases)) +
      geom_line(color = input$line_color) +
      labs(title = input$plot_title,
           x = "Datum",
           y = "Kumulative Fälle") +
      theme_minimal() +
      scale_y_continuous(trans = ifelse(input$log_scale, "log10", "identity"))

    if (input$smoothing) {
      p <- p + geom_smooth(method = "loess", se = FALSE, color = "red")
    }

    p
  })

  output$daily_plot <- renderPlot({
    data <- filtered_data() %>%
      arrange(date) %>%
      mutate(daily_cases = total_cases - lag(total_cases, default = 0))
    p <- ggplot(data, aes(x = date, y = daily_cases)) +
      geom_line(color = input$line_color) +
      labs(title = input$plot_title,
           x = "Datum",
           y = "Tägliche Fälle") +
      theme_minimal() +
      scale_y_continuous(trans = ifelse(input$log_scale, "log10", "identity"))

    if (input$smoothing) {
      p <- p + geom_smooth(method = "loess", se = FALSE, color = "red")
    }

    p
  })

  output$data_table <- renderTable({
    filtered_data()
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste("covid_plot_", input$country, ".png", sep = "")
    },
    content = function(file) {
      data <- filtered_data()
      p <- ggplot(data, aes(x = date, y = total_cases)) +
        geom_line(color = input$line_color) +
        labs(title = input$plot_title,
             x = "Datum",
             y = "Fälle") +
        theme_minimal() +
        scale_y_continuous(trans = ifelse(input$log_scale, "log10", "identity"))

      if (input$smoothing) {
        p <- p + geom_smooth(method = "loess", se = FALSE, color = "red")
      }

      ggsave(file, plot = p)
    }
  )

  output$download_data <- downloadHandler(
    filename = function() {
      paste("covid_data_", input$country, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)