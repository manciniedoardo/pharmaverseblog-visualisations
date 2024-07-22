# Load necessary library
library(shiny)
library(ggplot2)
library(rvest)
library(stringr)
library(dplyr)
library(tibble)
library(lubridate)
library(tidyr)
library(plotly)
library(shinydashboard)
library(shinyWidgets)

# Scrape pharmaverseblog data from website ----
source("source_data.R")

# Define UI for application ----
ui <- fluidPage(
  
  suppressWarnings(useShinydashboard()), # this may need to be updated in the future
  
  # Application title
  titlePanel("pharmaverseblog summary stats"),
  
  # Main panel for displaying the outputs
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Summary Statistics",
        
        br(),
        
        h3("Here are some summary stats for the pharmaversblog:"),
        
        br(),
        
        infoBox(
          title = tags$p("Unique Posts", style = "font-size:8px;"),
          value = unique_posts,
          icon = icon("book"),
          color = "blue",
          fill = TRUE,
          width = 4
        ),
        
        infoBox(
          title = tags$p("Unique Authors", style = "font-size:8px;"),
          value = unique_authors,
          icon = icon("person"),
          color = "orange",
          fill = TRUE,
          width = 4
        ),
        
        infoBox(
          title = tags$p("Earliest Post Date", style = "font-size:8px;"),
          value = earliest_post_date,
          icon = icon("clock"),
          color = "green",
          fill = TRUE,
          width = 4
        ),
        
        infoBox(
          title = tags$p("Latest Post Date", style = "font-size:8px;"),
          value = latest_post_date,
          icon = icon("clock"),
          color = "green",
          fill = TRUE,
          width = 4
        ),
        
        infoBox(
          title = tags$p("Days since last post", style = "font-size:8px;"),
          value = days_since_last_post,
          icon = icon("clock"),
          color = "green",
          fill = TRUE,
          width = 4
        )
        
        
      ),
      tabPanel("Posts by Month", plotlyOutput("posts_by_month")),
      tabPanel("Posts by Author", plotlyOutput("posts_by_author")),
      tabPanel("Posts by Category", plotlyOutput("posts_by_category")),
      tabPanel("Raw Data", DT::DTOutput("posts"))
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Render a bar plot of the iris dataset
  output$posts_by_author <- renderPlotly({
    ggplot(post_authors, aes(x = authors, y = n)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      theme_minimal() +
      labs(title = "Counts by Person", x = "Author", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
  })
  
  output$posts_by_category <- renderPlotly({
    ggplot(post_categories, aes(x = categories, y = n)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      theme_minimal() +
      labs(title = "Counts by Category", x = "Category", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
  })
  
  output$posts_by_month <- renderPlotly({
    ggplot(posts_per_month, aes(x = month_year, y = n)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Number of Posts per Month", x = "Month", y = "Number of Posts")
  })
  
  output$posts <- DT::renderDT(DT::datatable(posts, escape = FALSE))
 
}

# Run the application
shinyApp(ui = ui, server = server)
