library(tidyverse)
library(ggplot2)
library(plotly)
library(leaflet)
library(plotly)
library(bslib)

seabirds <- read_csv("data/seabirds_cleaned.csv")

rare_plot <- seabirds %>% 
  select(species_scientific, species_common, species_abbreviation, count) %>%
  filter(!str_detect(species_common, "sensu lato")) %>% 
  filter(str_detect(species_scientific, "[A-Z]{1}[a-z]* [a-z]*$")) %>% 
  filter(!str_detect(species_common, "unidentified")) %>% 
  filter(!str_detect(species_scientific, "\\/")) %>% 
  group_by(species_scientific, species_common, species_abbreviation) %>% 
  summarise(count_individual_birds = sum(count)) %>% 
  arrange(count_individual_birds) %>% 
  # now you only have the rarest birds ever seen.
  head(7) %>% 
  ggplot() +
  aes(x = species_common, y = count_individual_birds) +
  geom_col(fill = "#e34a33") +
  coord_flip() +
  ylab("") +
  xlab("") +
  theme_classic() +
  scale_y_continuous(breaks = 1:5) +
  expand_limits(y = 5)

common_plot <- seabirds %>% 
  select(species_scientific, species_common, species_abbreviation, count) %>%
  filter(!str_detect(species_common, "sensu lato")) %>% 
  filter(str_detect(species_scientific, "[A-Z]{1}[a-z]* [a-z]*$")) %>% 
  filter(!str_detect(species_common, "unidentified")) %>% 
  filter(!str_detect(species_scientific, "\\/")) %>% 
  group_by(species_scientific, species_common, species_abbreviation) %>% 
  summarise(count_individual_birds = sum(count)) %>% 
  arrange(desc(count_individual_birds)) %>% 
  # now you only have the most common birds
  head(7) %>% 
  ggplot() +
  aes(x = species_common, y = count_individual_birds) +
  geom_col(fill = "#addd8e") +
  coord_flip() +
  ylab("") +
  xlab("") +
  theme_classic() +
  scale_y_continuous(labels = scales::comma)

all_common <- 
  seabirds %>% 
  select(species_common) %>%
  filter(!str_detect(species_common, "sensu lato")) %>% 
  filter(!str_detect(species_common, "unidentified")) %>% 
  distinct(species_common)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "lux"),
  
  fluidRow(
    titlePanel(tags$b("Seabird Observations New Zealand"))
  ),
  fluidRow(p("This app offers key insights on seabird observations in the waters around New Zealand")),
  
  br(),br(),
  fluidRow(
    column(width = 11.5,
           offset = 0.5,
           titlePanel(tags$h4("Total bird species observed per year"))
    )
  ),
  
  fluidRow(
    # I want to have a graph at the top of my dashboard showing a minimal representation
    # of total bird species observed in recent years. Gives a good overview of decline of obervations
    # over the years (biodiversity crisis or lack of observers?!)
    column(width = 10, plotOutput("plot_totalobserved"))
  ),
  
  br(),br(),
  fluidRow(
    column(width = 11.5,
           offset = 0.5,
           titlePanel(tags$h4("Rarest and commonest seabirds in New Zealand"))
    )
  ),
  fluidRow(
    column(width = 2.5,
           offset = 1,
           radioButtons("scarcity_input",
                        "",
                        choices = c("Commonest", "Rarest"), # unfortunately, John's method does not work (since both arrange() and plot type has be changed!)
                        inline = TRUE)),
  ),
  fluidRow(
    
    column(width = 9,
           offset = 1,
           # This plot shows the top 7 most rare or common birds seen based on the viewers choice. 
           # By hoovering over the bars, the viewers can see the exact number of individuals recorded in total.
           plotlyOutput("scarcity_plot"))
  ),
  br(),
  fluidRow(
    column(width = 5,
           offset = 0.5,
           titlePanel(tags$h4("bird sighting locations"))
    )
  ),
  fluidRow(
    column(width = 4,
           offset = 2.5,
           selectInput(inputId = "species_input",
                       label = "Select bird species",
                       choices = all_common,
                       selected = "Royal albatros"),
    ),
    fluidRow(
    column(offset = 1,
      width = 9,
           #  # This map shows the locations of observations for the species of choice!
           leafletOutput("leaflet_map"))
    
  ),
  fluidRow(
    column(width = 2,
           offset = 10,
           titlePanel(tags$h6("This app is created by Thijmen Breeschoten"))
  )
  )
)
)

server <- function(input, output, session) {
  
  # I want to have a graph at the top of my dashboard showing a minimal representation
  # of total bird species observed in recent years. Gives a good overview of decline of obervations
  # over the years (biodiversity crisis or lack of observers?!)
  
  output$plot_totalobserved <- renderPlot({
    seabirds %>% 
      mutate(year = lubridate::year(date)) %>% 
      select(species_scientific, species_common, species_abbreviation, year) %>% 
      filter(!str_detect(species_common, "sensu lato")) %>% 
      filter(str_detect(species_scientific, "[A-Z]{1}[a-z]* [a-z]*$")) %>% 
      filter(!str_detect(species_common, "unidentified")) %>% 
      filter(!str_detect(species_scientific, "\\/")) %>% 
      select(year, species_scientific) %>% 
      group_by(year) %>% 
      summarise(total_species_per_year = n()) %>% 
      mutate(label_text = ifelse(year == 1990, total_species_per_year, NA)) %>% 
      ggplot() +
      aes(x = year, y = total_species_per_year) +
      geom_line(colour = "#08519c") +
      geom_area(fill = "#9ecae1") +
      geom_point(colour = "#08519c", size = 2) +
      geom_text(aes(label = label_text), hjust = -0.5, size = 14) +
      theme_void() +
      expand_limits(x = 1991)
  })
  
  plot <-  reactive({
    if (input$scarcity_input == "Rarest") {
      rare_plot
    } else {
      common_plot
    }
  })
  # This plot shows the top 7 most rare or common birds seen based on the viewers choice. 
  # By hoovering over the bars, the viewers can see the exact number of individuals recorded in total.
  output$scarcity_plot <- renderPlotly({plot()})
  
  # This map shows the locations of observations for the species of choice!
  output$leaflet_map <- renderLeaflet({
    seabirds %>% 
      filter(str_detect(species_common, input$species_input)) %>% 
      leaflet() %>% 
      addProviderTiles(provider = providers$OpenStreetMap) %>% 
      addMarkers(lng = ~long, lat = ~lat, popup = ~species_scientific)
  })

}

shinyApp(ui, server)