library(tidyverse)
library(ggplot2)
library(plotly)
library(leaflet)
library(plotly)

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
  ylab("Total individual birds observed") +
  xlab("") +
  ggtitle("Rarest birds observed") +
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
  ylab("Total individual birds observed") +
  xlab("") +
  ggtitle("Most common birds seen") +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) 

ui <- fluidPage(
  fluidRow(
    titlePanel("Seabird Observations New Zealand"),
    h3("Key insights into bird obervations in the waters around New Zealand")
  ),
  br(),br(),
  fluidRow(
    # I want to have a graph at the top of my dashboard showing a minimal representation
    # of total bird species observed in recent years. Gives a good overview of decline of obervations
    # over the years (biodiversity crisis or lack of observers?!)
    plotOutput("plot_totalobserved")
  ),
  fluidRow(
    column(width = 5,
           offset = 7,
           titlePanel(tags$h2("Total bird species observed in recent years"))
    )
  ),
  fluidRow(
    column(width = 3,
           radioButtons("scarcity_input",
                        "Most common or rare birds in New Zealand",
                        choices = c("Rarest", "Commonest"), # unfortunately, John's method does not work (since both arrange() and plot type has be changed!)
                        inline = TRUE)
    ),
    column(width = 9,
           # This plot shows the top 7 most rare or common birds seen based on the viewers choice. 
           # By hoovering over the bars, the viewers can see the exact number of individuals recorded in total.
           plotOutput("scarcity_plot"))
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
      geom_point(colour = "#08519c", size = 4) +
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
  
  output$scarcity_plot <- renderPlot({plot()})
  


}

shinyApp(ui, server)