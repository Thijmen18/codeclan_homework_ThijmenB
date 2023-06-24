library(tidyverse)
library(ggplot2)
library(plotly)
library(leaflet)

seabirds <- read_csv("data/seabirds_cleaned.csv")

plot_totalobserved <- seabirds %>% 
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
  geom_point(colour = "#08519c", size = 4) +
  geom_text(aes(label = label_text), hjust = -0.5, size = 8) +
  theme_void() +
  expand_limits(x = 1991)

ui <- fluidPage(
  fluidRow(
    titlePanel(tags$h1("Seabird Observations New Zealand")),
  ),
  fluidRow(
    titlePanel(tags$h3("Key insights into bird obervations in the waters around New Zealand"))
  ),
  fluidRow(
    plotOutput(plot_totalobserved)
    
  ),
  fluidRow(),
  fluidRow()
  
)

server <- function(input, output, session) {
  
  
  
}

shinyApp(ui, server)