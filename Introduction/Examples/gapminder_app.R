
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Load required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gapminder)
library(colourpicker)
library(devtools)
library(rsconnect)
#install.packages("RJSONIO")
library(RJSONIO)
library(scales)
#devtools::install_github("daattali/colourpicker")


# Load Gapminder data
gDat<- gapminder

# Coverting population into a more readable format : Million
gDat$pop <- gDat$pop/1000000 
gDat$lifeExp <- round(gDat$lifeExp)
#



library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),
                
                titlePanel("Gapminder Data Visualization using ShinyApps"),
                
                sidebarLayout(position = "right",
                              sidebarPanel("Compare population, life-expectancy and GDP per Capita between two countries over the years by selecting the quantity to be compared and the year",
                                           hr(),
                                           
                                           tags$a(class="btn btn-default", href="https://github.com/akshi8", "See Github"),
                                           tags$a(class="btn btn-default", href="https://github.com/rebjoh/Gapminder-app", "Attribution"),
                                           radioButtons("variable_from_gapminder",
                                                        label = h5("Compare:"),
                                                        choices = c("Population" = "pop",
                                                                    "Life Expectancy" = "lifeExp",
                                                                    "GDP Per Capita" = "gdpPercap"),
                                                        selected = "gdpPercap"),
                                           hr(),
                                           uiOutput("choose_country"),
                                           hr(),
                                           uiOutput("choose_country_2"),
                                           hr(),
                                           sliderInput("year_range",
                                                       label = h5("Range of years:"),
                                                       min = 1952,
                                                       max = 2007,
                                                       value = c(1952, 2007), 
                                                       format = "####", 
                                                       step = 5)
                                           
                              ),
                              
                              
                              
                              mainPanel(h3(textOutput("output_countries_years")),
                                        textOutput("info"),
                                        textOutput("more_info"),
                                        plotOutput("ggplot_variable_vs_two_countries"),
                                        hr(),
                                        numericInput("yearinput", "Enter year between 1952-2007",
                                                     value = 2002,
                                                     min = 1952,
                                                     max = 2007,
                                                     step = 5
                                        ),
                                        h3("Tabulated results"),
                                        tableOutput("gapminder_table")
                                        
                                        
                              )
                )
)

# Define server logic required to draw a histogram
server <- (function(input, output){
  
  # Create drop-down selection for country generated from Gapminder dataset
  output$choose_country <- renderUI({
    selectInput("country_from_gapminder",
                h5("First country:"),
                levels(gDat$country),
                selected = "United States")
  })
  
  # Create drop-down selection for a 2nd country
  # Exclude "country_from_gapminder" as an option
  output$choose_country_2 <- renderUI({
    selectInput("country_2_from_gapminder",
                h5("Second country:"),
                levels(gDat$country)[levels(gDat$country) != 
                                       input$country_from_gapminder],
                selected = "Canada")
  })
  
  # Add reactive function for two countries and year input from UI
  two_country_data <- reactive({
    
    if(is.null(input$country_from_gapminder)){
      return(NULL)
    }
    
    if(is.null(input$country_2_from_gapminder)){
      return(NULL)
    }
    
    gDat %>%
      select(country, year, continent, 
             matches(input$variable_from_gapminder)) %>%
      filter(country %in% c(input$country_from_gapminder, 
                            input$country_2_from_gapminder),
             year >= min(input$year_range) &
               year <= max(input$year_range))
  })
  
  # Render two countries and year input from UI as a table
  output$gapminder_table <- renderTable({
    filtered <- two_country_data() %>% filter(year == input$yearinput)
    filtered
  })
  
  # Render country and range of years input from UI as text
  output$output_countries_years <- renderText({
    paste(input$country_from_gapminder, "and",
          input$country_2_from_gapminder,
          min(input$year_range), "-", max(input$year_range))
  })
  
  # Print info on years selected to console (renderPrint prints to UI)
  output$info <- renderText({
    str(two_country_data()$year)
  })
  
  output$more_info <- renderText({
    str(two_country_data())
  })
  
  # Render ggplot plot based on variable input from radioButtons
  output$ggplot_variable_vs_two_countries <- renderPlot({
    
    if(is.null(two_country_data()$year)){
      return(NULL)
    }
    
    if(input$variable_from_gapminder == "pop") y_axis_label <- "Population (millions)"
    if(input$variable_from_gapminder == "lifeExp") y_axis_label <- "Life Expectancy (years)"
    if(input$variable_from_gapminder == "gdpPercap") y_axis_label <- "GDP Per Capita, PPP (in US dollars)"
    
    theme_new <-  theme(axis.text.x = element_text(colour="black", face = "bold", size=7, hjust=.5, vjust=.5),
                        axis.text.y = element_text(colour="black",face = "bold", size=7),
                        #plot.title = element_text(size = rel(2), color = "teal"),
                        plot.title = element_text(
                          size = rel(2),
                          face = "bold",
                          family = "American Typewriter",
                          color = "Blue",
                          hjust = 0.5,
                          lineheight = 1.2
                        ),
                        text=element_text(size=7, face = "bold"),
                        legend.title = element_text(size = 8,face = "bold"),
                        legend.box.background = element_rect(),
                        plot.background = element_rect(fill = "azure")
                        
    ) + theme_bw()
    
    ggplot(two_country_data(), aes_string(x = "year",
                                          y = input$variable_from_gapminder,
                                          colour = "country")) +
      geom_line(size = 1) + geom_point(size = 3) + theme_new + labs(color = "Countries") +  scale_y_continuous(labels= comma) +
      scale_color_brewer(palette = "Set2") +
      xlab("Year") +
      ylab(y_axis_label) 
    
    
  })
  
  
  
})


# Run the application 
shinyApp(ui = ui, server = server)
