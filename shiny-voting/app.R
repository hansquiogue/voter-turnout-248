library(shiny)
library(tidyverse)
library(DT)

ui <- fluidPage(

    h3(titlePanel("U.S. Voting Turnout by Age Groups")),
    
    sidebarLayout(
        sidebarPanel(
            # Main inputs (Can change from historical to different years, etc)
            selectInput("main_inputs", h3("Data Selected:"), 
                        choices = list("Historical" = 1, 
                                       "2018" = 2),
                        selected = 1),
            # If historical is chosen, this layout will be displayed
            conditionalPanel(
                condition = "input.main_inputs  == '1'",
                checkboxInput("hist_comb", "Combine votes/registration data", TRUE),
                checkboxGroupInput("hist_ages", "Choose an age group:",
                                    choices = list("Total" = 1,"Young" = 2,
                                                    "Adult" = 3,"Mid Age" = 4,
                                                    "Elderly" = 5),
                                    selected = c(1, 2, 3, 4, 5)),
                radioButtons("hist_status", "Choose voting status:",
                             choices = list("Voted" = 1, "Registered" = 2),
                             selected = 1))),
     
        mainPanel(
           plotOutput("hist_plots"),
           dataTableOutput("hist_table"),
        )
    )
)

server <- function(input, output) {
    
    # hist_df is clean dataframe of historical voting data
    hist_df <- read.csv("clean_hist.csv")
    age_groups <- c("Total", "18 to 24", "25 to 44", "45 to 64", "65+")
    vote_status <- c("Voted", "Registered")
    
    output$hist_plots <- renderPlot({
        # Dataframe of filtered and selected age groups from user input
        plot_df <- hist_df[hist_df$Age %in% age_groups[as.numeric(input$hist_ages)], ] 
        # Historical plot will be used
        if(input$main_inputs == 1) {
            # Historical plot of combined votes and registration data
            if(input$hist_comb == TRUE) {
                ggplot(data = plot_df) +
                    aes(Year, Population.Count, group = Age, color = Age) + 
                    geom_point() + geom_line() + 
                    ggtitle("Votes and Registration Between Age Groups (1964-2018)") +
                    ylab("Total Population (In Thousands)") + 
                    xlab("Years (1964-2018)") + 
                    theme_minimal() +
                    theme(axis.text.x = element_blank())
            }
            # Historical plot of either votes and registration data
            else {
                plot_df <- plot_df %>% filter(Status == vote_status[as.numeric(input$hist_status)])
                # Historical plot
                ggplot(data = plot_df) +
                    aes(Year, Population.Percent, group = Age, color = Age) + 
                    geom_point() + geom_line() + 
                    ggtitle("Vote/Registration Percentage Between Age Groups (1964-2018)") +
                    ylab("Percentage") + 
                    xlab("Years (1964-2018)") + 
                    theme_minimal() +
                    theme(axis.text.x = element_blank())
            }
        }
    })
    
    output$hist_table <- renderDataTable({
        if(input$main_inputs == 1) {
            # Dataframe of filtered and selected age groups from user input
            table_df <- hist_df[hist_df$Age %in% age_groups[as.numeric(input$hist_ages)], ]
            select_vote <- as.numeric(input$hist_status)
            # Table of historical data
            table_df %>% 
                filter(Status == vote_status[select_vote]) %>% 
                mutate(Population.Count = (Population.Count * 1000)) %>% 
                select(Year, Age, Population.Percent, Population.Count) %>%
                rename(`Population Count Voted and Registered` = Population.Count) %>% 
                rename(`Voted/Registered Percentage` = Population.Percent)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
