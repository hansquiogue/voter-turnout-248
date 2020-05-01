#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(ggridges)
library(scales)
library(gridExtra)
library(viridis)
library(fiftystater)
library(mapproj)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year", "Year", 1980, 2016, 2016, step = 2, animate = TRUE, sep = ""),
            checkboxInput(
                "relscale",
                "Use relative color scale",
                TRUE
            )
        ),
        mainPanel(
            plotOutput("elecPlot")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    gen.turn <- read_excel(
        "1980-2014 November General Election.xlsx",
        skip = 1
    )
    
    gen.turn.2016 = read_excel(
        "2016 November General Election.xlsx",
        skip = 1
    )
    
    gen.turn <- gen.turn %>%
        select(-(2:3), -c(5, 7, 8, 11), -(12:17)) 
    
    colnames(gen.turn) <- c("year", "region", "turnout", "ballots", "VEP")
    gen.turn$region <- sapply(gen.turn$region, tolower)
    gen.turn <- mutate(gen.turn, type = ifelse(year %in% seq(1980, 2020, 4), "Presidential", "Midterm"))
    
    gen.turn.2016 <- gen.turn.2016 %>%
        select(-(2:3), -c(4, 6, 7, 10), -(11:17)) 
    
    colnames(gen.turn.2016) <- c("region", "turnout", "ballots", "VEP")
    gen.turn.2016$region <- sapply(gen.turn.2016$region, tolower)
    gen.turn.2016$year <- rep(2016, nrow(gen.turn.2016))
    gen.turn.2016$type <- rep("Presidential", nrow(gen.turn.2016))
    
    gen.turn <- full_join(gen.turn, gen.turn.2016)
    
    states.turnout <- gen.turn %>% filter(region != "united states")
    total.turnout <- gen.turn %>% filter(region == "united states")
    
    data("fifty_states")
    
    # states_map <- us_map()

    output$elecPlot <- renderPlot({
        
        # merge <- states.turnout %>% 
        #     filter(year == input$year)
        # 
        # merge <- inner_join(states_map, merge, by = "region")
        
        data("fifty_states")
        
        ret_plot <- states.turnout %>% 
            filter(year == input$year) %>% 
            ggplot(aes(map_id = region)) +
                geom_map(aes(fill = turnout), map = fifty_states, color = "white") +
                expand_limits(x = fifty_states$long, y = fifty_states$lat) +
                coord_map(projection = "albers", lat0=30, lat1=40) +
                labs(title = paste("Election Turnout Rates by State (", input$year, ")", sep = ""), fill = "Turnout") +
                theme_void() +
                fifty_states_inset_boxes() + 
                facet_wrap(~type)
        
        if (input$relscale) {
            ret_plot + scale_fill_viridis(label = scales::percent)
        }
        else {
            ret_plot + scale_fill_viridis(limits = c(0, 1), label = scales::percent)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
