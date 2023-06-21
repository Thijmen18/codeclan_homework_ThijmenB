# load packages
library(shiny) 
library(tidyverse) 
library(CodeClanData)
library(bslib)

# define your data

energy_scotland <- CodeClanData::energy_scotland

# get a list of the different sectors:
all_sectors <- energy_scotland %>% 
  distinct(Sector) %>% 
  pull()

# In your UI:

ui <- fluidPage(
  #add a theme
  theme = bs_theme(bootswatch = "spacelab"),
  
  #first row is title
  titlePanel(tags$h1("Energy Production in Scotland")), 
  
  #we want a break here
  br(),
  
  # We want to have the tabs here, with the first tab being the plots window
  tabsetPanel(
    tabPanel("Energy Production per Year and Sector",
             # we want a break here
             br(),
             # second row is dropdown menu, with next to it the plot:
             
             fluidRow(
               
               column(width = 3,
                      
                      selectInput(inputId = "sector_input",
                                  label = "Select the energy sector of interest",
                                  choices = all_sectors,
                                  selected = "Renewables") # here it starts as a default on Renewables
               ),
               # than the column with the plot:
               
               column(width = 9,
                      plotOutput("sector_plot")
               )
             ),
             
             # We want a break here
             br(),
             br(),
             
             #than a next row with the other choice panel (year)
             fluidRow(
               
               column(width = 3,
                      offset = 1,
                      numericInput(inputId = "year_input", # this is a open field where people can insert the year
                                   value = 2005,
                                   label = "Select the year of interest",
                                   min = 2005,
                                   max = 2017)
               ),
               # than again the column with the plot:
               
               column(width = 8,
                      plotOutput("year_plot")
               )
               
             ),
             # Here the final plot at the bottom!
             fluidRow(
               plotOutput("line_plot")
             )
    ),
    
    # Here comes the other tab with the link
    tabPanel("More Information",
             tags$h3(tags$a("Scottish Energy Webpage",
                            href = "https://www.gov.scot/energy/"))
    )
  )
)


# In server:

server <- function(input, output) {
  
  #render your plot:
  output$sector_plot <- renderPlot({
    
    energy_scotland %>% 
      filter(Sector == input$sector_input) %>% 
      ggplot() +
      aes(x = Year, y = EnergyProd) +
      geom_col(fill = "yellow", colour = "black") +
      xlab("\nEnergy Production (KWh)") +
      ylab("Year\n") +
      ggtitle("Energy Production in Scotland per Year\n") +
      scale_x_continuous(breaks = 2005:2017) +
      theme(
        text = element_text(size = 14),
        title = element_text(size = 15),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey90", linetype = "dashed")
      )
    
  }
  )
  
  #render the second plot:
  output$year_plot <- renderPlot({
    
    energy_scotland %>% 
      filter(Year == input$year_input) %>% 
      ggplot() +
      aes(x = Sector, y = EnergyProd) +
      geom_col(fill = "darkgreen", colour = "black") +
      coord_flip() +
      xlab("\n\nEnergy Production (KWh)\n") +
      ylab("\nSector\n\n") +
      ggtitle("Energy Production in Scotland per Sector\n") +
      theme(
        text = element_text(size = 14),
        title = element_text(size = 15),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey90", linetype = "dashed")
      )
  })
  
  #render the third plot:
  output$line_plot <- renderPlot({
    
    energy_scotland %>%
      ggplot() +
      aes(x = Year, y = EnergyProd, colour = Sector) +
      geom_line() +
      scale_x_continuous(breaks = 2005:2017) +
      ylab("\n\nEnergy Production (KWh)\n") +
      xlab("") +
      ggtitle("Out with non-renewables!!!\n") +
      theme(
        text = element_text(size = 14),
        title = element_text(size = 15),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_line(colour = "grey90", linetype = "dashed"))
  })
  
  
}

shinyApp(ui = ui, server = server) # run the function with ui and server as inputs