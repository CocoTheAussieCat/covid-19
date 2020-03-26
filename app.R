library(tidyverse)
library(lubridate)
library(here)
library(scales)
library(RColorBrewer)
library(glue)
library(wesanderson)
library(shiny)
library(shinydashboard)
library(plotly)
library(patchwork)
library(jsonlite)

### PREPARTION CODE -----------------------------------------------------------
# Imports data, cleans and manipulates, creates plots
source("01_who_data_import.R")
source("02_growth_rate.R")
source("03_aus_testing.R")

### SHINY APP -----------------------------------------------------------------
# Define UI dashboard
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(
                      title = "COVID-19 Data", titleWidth =250
                    ),
                    dashboardSidebar(width = 250,
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Case growth rate", 
                                 tabName = "growth_total_tab", icon = icon("line-chart")),
                        menuItem("Cases per capita growth rate", 
                                 tabName = "growth_cap_tab", icon = icon("users")),
                        menuItem("Daily cases per capita", 
                                 tabName = "new_daily_cap_tab", icon = icon("calendar")),
                        menuItem("Cases by country", 
                                 tabName = "country_tab", icon = icon("flag")),
                        menuItem("Cases by region", 
                                 tabName = "region_tab", icon = icon("map-pin")),
                        menuItem("Australian testing", 
                                 tabName = "aus_tab", icon = icon("vial")),
                        br(),
                        " Created by Rebecca O'Dwyer",
                        br(),
                        " Twitter ",
                        tags$a(href = "https://twitter.com/RebeccaDataSci", "@RebeccaDataSci"),
                        br(),
                        " LinkedIn ",
                        tags$a(href = "https://www.linkedin.com/in/rebecca-odwyer", "Rebecca O'Dwyer"),
                        br(),
                        " Code at ",
                        tags$a(href = "https://github.com/CocoTheAussieCat/covid-19", "github"),
                        br(),
                        br(),
                        " #flattenthecurve",
                        br(),
                        ' #stayathome',
                        br(),
                        ' #bekind'
                      ) # closes sidebarMenu
                    ), # closes dashboardSidebar
                    
                    dashboardBody(
                      tabItems(
                        
                        # tab 1 - growth total --------------------------------
                        tabItem(tabName = "growth_total_tab",
                                fluidRow(
                                  box(width = 8,
                                      plotOutput("growth_plot")
                                  ),
                                  box(width = 4,
                                      "Case growth rate versus China, Italy, South Korea and US",
                                      br(),
                                      br(),
                                      selectInput("growth_select", "Select country to add to chart", 
                                                  hundred_cases, selected = "United Kingdom",
                                                  multiple = T),
                                      br(),
                                      "Data from ",
                                      tags$a(href=who_link, "Johns Hopkins"),
                                      br(),
                                      br(),
                                      downloadButton("growth_download", "Download")
                                  ) # closes box
                                ) # closes fluidRow
                        ), # closes tabItem 1 ---------------------------------
                        
                        
                        # tab 2 - growth per capita  --------------------------
                        tabItem(tabName = "growth_cap_tab",
                                fluidRow(
                                  box(width = 8,
                                      plotOutput("growth_cap_plot")
                                  ),
                                  box(width = 4,
                                      "Case growth rate versus China, Italy, South Korea and US",
                                      br(),
                                      br(),
                                      selectInput("growth_cap_select", "Select country to add to chart", 
                                                  hundred_cases, selected = "United Kingdom", multiple = T),
                                      br(),
                                      "COVID-19 data from ",
                                      tags$a(href=who_link, "Johns Hopkins"),
                                      br(),
                                      "Population data from ",
                                      tags$a(href=gapminder_link, "Gapminder"),
                                      br(),
                                      br(),
                                      downloadButton("per_cap_download", "Download")
                                  ) # closes box
                                ) # closes fluidRow
                        ), # closes tabItem 2 ---------------------------------
                        
                        # tab 3 - new daily cases growth per capita  --------------------------
                        tabItem(tabName = "new_daily_cap_tab",
                                fluidRow(
                                  box(width = 8,
                                      plotOutput("new_daily_cap_plot")
                                  ),
                                  box(width = 4,
                                      "Case growth rate versus:",
                                      "China, France, Germany, Italy, Spain, South Korea, UK, US",
                                      br(),
                                      br(),
                                      selectInput("new_daily_cap_select", "Select country to add to chart", 
                                                  hundred_cases, selected = "Australia"),
                                      br(),
                                      "COVID-19 data from ",
                                      tags$a(href=who_link, "Johns Hopkins"),
                                      br(),
                                      "Population data from ",
                                      tags$a(href=gapminder_link, "Gapminder"),
                                      br(),
                                      br(),
                                      downloadButton("daily_cap_download", "Download")
                                  ) # closes box
                                ) # closes fluidRow
                        ), # closes tabItem 3 ---------------------------------
                        
                        
                        # tab 4 cases total -----------------------------------
                        tabItem(tabName = "country_tab",
                                fluidRow(
                                  box(width = 8,
                                      plotlyOutput("country_plot")
                                  ),
                                  box(width = 4,
                                      selectInput("country_select", "Select Country", 
                                                  countries, selected = "World"),
                                      radioButtons("country_plot_select", "", 
                                                  plot_type, selected = plot_type[1]),
                                      br(),
                                      "Data from ",
                                      tags$a(href=who_link, "Johns Hopkins"),
                                      br(),
                                      br(),
                                      downloadButton("country_download", "Download")
                                  
                                  ) # closes box
                                ) # closes fluidRow
                        ), # closes tabItem 4 ---------------------------------
                        
                        
                        # tab 5 new cases total -------------------------------
                        tabItem(tabName = "region_tab",
                                fluidRow(
                                  box(width = 8,
                                      plotlyOutput("region_plot")
                                  ),
                                  box(width = 4,
                                      selectInput("region_select", "Select Region",
                                                  regions, selected = "New South Wales"),
                                      radioButtons("region_plot_select", "", 
                                                  plot_type, selected = plot_type[1]),
                                      br(),
                                      "Data from ",
                                      tags$a(href=who_link, "Johns Hopkins"),
                                      br(),
                                      br(),
                                      downloadButton("region_download", "Download")
                                      
                                  ) # closes box
                                ) # closes fluidRow
                        ), # closes tabItem 5 ----------------------------------
                        
                        # tab 5 new cases total -------------------------------
                        tabItem(tabName = "aus_tab",
                                fluidRow(
                                  box(width = 12, plotOutput("aus_plot"), 
                                      br(),
                                      "Data from ",
                                      tags$a(href = "https://www.theguardian.com/au", "Guardian Australia")
                                  ) # closes box
                                ) # closes fluidRow
                        ) # closes tabItem 5 ----------------------------------
                        
                      ) # closes tabItems
                    ) # closes dashboardBody
) # closes dashboardPage



### SERVER LOGIC --------------------------------------------------------------
server <- function(input, output) {
  
  # Get country filter for growth plot from user input
  growth_filter <- reactive({
    growth_filter <- input$growth_select
  })
  
  # Get country filter for growth per capita plot from user input
  growth_cap_filter <- reactive({
    growth_cap_filter <- input$growth_cap_select
  })
  
  # Get country filter for new daily growth per capita plot from user input
  new_daily_cap_filter <- reactive({
    new_daily_cap_filter <- as.character(input$new_daily_cap_select) 
  })
  
  # Get country from user input
  country_filter <- reactive({
    country_filter <- as.character(input$country_select) 
  })
  
  # Get country plot type from user input
  country_plot_type <- reactive({
    country_plot_type <- as.character(input$country_plot_select) 
  })
  
  # Get region from user input
  region_filter <- reactive({
    region_filter <- as.character(input$region_select) 
  })
  
  # Get region plot type from user input
  region_plot_type <- reactive({
    region_plot_type <- as.character(input$region_plot_select) 
  })
  
  ### PLOTS -------------------------------------------------------------------

  # Create growth rate plot
  output$growth_plot <- renderPlot({
    growth_filter <- growth_filter()
    growthPlot(who_day_zero, growth_filter)
  })
  
  output$growth_download <- downloadHandler(
    filename = function() {
      paste("case_growth_data", ".csv", sep = "")
    },
    content = function(file) {
      growth_filter <- growth_filter()
      growth_table <- growthData(who_day_zero, growth_filter)
      write.csv(growth_table, file, row.names = TRUE)
    }
  )
  
  # Create growth rate per capita plot
  output$growth_cap_plot <- renderPlot({
    
    growth_cap_filter <- growth_cap_filter()
    growthPerCapitaPlot(who_day_zero, growth_cap_filter)
  })
  
  output$per_cap_download <- downloadHandler(
    filename = function() {
      paste("case_per_cap_data", ".csv", sep = "")
    },
    content = function(file) {
      growth_cap_filter <- growth_cap_filter()
      per_cap_table <- growthData(who_day_zero, growth_cap_filter)
      write.csv(per_cap_table, file, row.names = TRUE)
    }
  )
  
  # Create new daily growth rate per capita plot
  output$new_daily_cap_plot <- renderPlot({
    
    new_daily_cap_filter <- new_daily_cap_filter()
    dailyCasesPerCapitaPlot(who_day_zero, new_daily_cap_filter)
  })
  
  output$daily_cap_download <- downloadHandler(
    filename = function() {
      paste("daily_cases_per_cap_data", ".csv", sep = "")
    },
    content = function(file) {
      new_daily_cap_filter <- new_daily_cap_filter()
      daily_cap_table <- dailyCasesPerCapitaData(who_day_zero, new_daily_cap_filter)
      write.csv(daily_cap_table, file, row.names = TRUE)
    }
  )
  
  # Create country plot
  output$country_plot <- renderPlotly({
    country_filter <- country_filter()
    country_plot_type <- country_plot_type()
    parsed_country_plot <- case_when(country_plot_type == "New cases each day" ~"new_cases",
                                     country_plot_type == "Total cases" ~"cases")
    
    countryPlot(who_clean, country_filter, parsed_country_plot)
  })
  
  output$country_download <- downloadHandler(
    filename = function() {
      country_filter <- country_filter()
      paste(as.character(country_filter), "_data.csv", sep = "")
    },
    content = function(file) {
      country_filter <- country_filter()
      country_table <- countryData(who_clean, country_filter)
      write.csv(country_table, file, row.names = TRUE)
    }
  )
  
  # Create region plot
  output$region_plot <- renderPlotly({
    region_filter <- region_filter()
    region_plot_type <- region_plot_type()
    parsed_region_plot <- case_when(region_plot_type == "New cases each day" ~"new_cases",
                                     region_plot_type == "Total cases" ~"cases")
    
    regionPlot(who_clean, region_filter, parsed_region_plot)
  })
    
    output$region_download <- downloadHandler(
      filename = function() {
        region_filter <- region_filter()
        paste(as.character(region_filter), "_data.csv", sep = "")
      },
      content = function(file) {
        region_filter <- region_filter()
        region_table <- regionData(who_clean, region_filter)
        write.csv(region_table, file, row.names = TRUE)
      }
    )

  
  # Create aus testing plot
  output$aus_plot <- renderPlot({
    patch
  })
}

### RUN APPLICATION -----------------------------------------------------------
shinyApp(ui = ui, server = server)
