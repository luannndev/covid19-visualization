library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(leaflet)

url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
covid_data <- read.csv(url)

population_data <- data.frame(
  Country.Region = unique(covid_data$Country.Region),
  Population = sample(1e5:1e8, length(unique(covid_data$Country.Region)), replace = TRUE),
  Continent = rep(c("Europe", "Asia", "Americas", "Africa", "Oceania"), length.out = length(unique(covid_data$Country.Region)))
)

covid_data_long <- covid_data %>%
  pivot_longer(cols = starts_with("X"), names_to = "date", values_to = "cases") %>%
  mutate(date = as.Date(gsub("X", "", date), format = "%m.%d.%y")) %>%
  group_by(Country.Region, date) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(population_data, by = "Country.Region") %>%
  mutate(cases_per_100k = (total_cases / Population) * 1e5)

ui <- fluidPage(
  titlePanel("COVID-19 Fallzahlen weltweit"),
  theme = shinythemes::shinytheme("flatly"),

  sidebarLayout(
    sidebarPanel(
      selectInput("continent", "Wähle Kontinent:", choices = unique(population_data$Continent), selected = "Europe"),
      selectizeInput("countries", "Wähle Länder:", choices = NULL, selected = NULL, multiple = TRUE),
      dateRangeInput("date_range", "Wähle Datumsbereich:",
                     start = min(covid_data_long$date),
                     end = max(covid_data_long$date)),
      checkboxInput("log_scale", "Logarithmische Skala", FALSE),
      colorInput("line_color", "Wähle Linienfarbe:", value = "blue"),
      textInput("plot_title", "Diagrammtitel:", value = "COVID-19 Fälle"),
      checkboxInput("smoothing", "Glättungslinie hinzufügen", FALSE),
      radioButtons("metric", "Wähle Metrik", choices = c("Kumulative Fälle", "Tägliche Fälle", "Fälle pro 100k")),
      checkboxInput("show_test_capacity", "Hypothetische Testkapazitäten anzeigen", FALSE),
      shinyWidgets::switchInput("theme_toggle", "Dark Mode", value = FALSE)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Diagramm", plotlyOutput("plot")),
        tabPanel("Heatmap", plotlyOutput("heatmap_plot")),
        tabPanel("Karte", leafletOutput("map")),
        tabPanel("Daten anzeigen", tableOutput("data_table")),
        downloadButton("download_plot", "Plot herunterladen"),
        downloadButton("download_data", "Daten herunterladen")
      )
    )
  )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'countries', choices = unique(population_data$Country.Region[population_data$Continent == input$continent]), server = TRUE)

  filtered_data <- reactive({
    covid_data_long %>%
      filter(Country.Region %in% input$countries,
             date >= input$date_range[1] & date <= input$date_range[2])
  })

  output$plot <- renderPlotly({
    data <- filtered_data()

    if (input$metric == "Kumulative Fälle") {
      p <- ggplot(data, aes(x = date, y = total_cases, color = Country.Region)) +
        geom_line() +
        labs(title = input$plot_title,
             x = "Datum",
             y = "Kumulative Fälle") +
        theme_minimal() +
        scale_y_continuous(trans = ifelse(input$log_scale, "log10", "identity"))
    } else if (input$metric == "Tägliche Fälle") {
      data <- data %>%
        arrange(date) %>%
        group_by(Country.Region) %>%
        mutate(daily_cases = total_cases - lag(total_cases, default = 0)) %>%
        ungroup()
      p <- ggplot(data, aes(x = date, y = daily_cases, color = Country.Region)) +
        geom_line() +
        labs(title = input$plot_title,
             x = "Datum",
             y = "Tägliche Fälle") +
        theme_minimal() +
        scale_y_continuous(trans = ifelse(input$log_scale, "log10", "identity"))
    } else if (input$metric == "Fälle pro 100k") {
      p <- ggplot(data, aes(x = date, y = cases_per_100k, color = Country.Region)) +
        geom_line() +
        labs(title = input$plot_title,
             x = "Datum",
             y = "Fälle pro 100k Einwohner") +
        theme_minimal() +
        scale_y_continuous(trans = ifelse(input$log_scale, "log10", "identity"))
    }

    if (input$smoothing) {
      p <- p + geom_smooth(method = "loess", se = FALSE, color = "red")
    }

    if (input$show_test_capacity) {
      hypothetical_test_capacity <- data %>%
        mutate(test_capacity = runif(n(), min = 1e3, max = 1e5))
      p <- p + geom_bar(aes(y = test_capacity), stat = "identity", alpha = 0.2, position = "dodge")
    }

    ggplotly(p)
  })

  output$heatmap_plot <- renderPlotly({
    data <- filtered_data()
    heat_data <- data %>%
      select(Country.Region, date, total_cases) %>%
      spread(Country.Region, total_cases)

    p <- ggplot(data = reshape2::melt(heat_data, id = "date"), aes(x = date, y = variable, fill = value)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = "COVID-19 Fälle Heatmap",
           x = "Datum",
           y = "Land",
           fill = "Fälle") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    ggplotly(p)
  })

  output$map <- renderLeaflet({
    data <- filtered_data() %>%
      group_by(Country.Region) %>%
      summarize(total_cases = sum(total_cases, na.rm = TRUE)) %>%
      left_join(population_data, by = "Country.Region")

    map <- leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        weight = 1,
        radius = ~sqrt(total_cases) / 10000,
        popup = ~paste0(Country.Region, ": ", total_cases, " Fälle")
      )

    map
  })

  output$data_table <- renderTable({
    filtered_data()
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      paste("covid_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      data <- filtered_data()
      p <- ggplot(data, aes(x = date, y = total_cases, color = Country.Region)) +
        geom_line() +
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
      paste("covid_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file)
    }
  )

  observe({
    theme <- if (input$theme_toggle) "slate" else "flatly"
    session$sendCustomMessage("update-theme", list(theme = theme))
  })
}

shinyApp(ui = ui, server = server)