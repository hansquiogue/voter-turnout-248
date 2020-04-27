library(shiny)
library(tidyverse)
library(readxl)

# Apologies for this garbage, will be heavily revised later

# === Wrangling data ==== #
voting_col_names <- c("Year", "Total Population (Thousands)", "Total Population %",
                      "Citizen Population %", "Total White Population %",
                      "Citizen White Population %", "Total White Non Hispanic Population %",
                      "Citizen White Non Hispanic Population %", "Total Black Population %",
                      "Citizen Black Population %", "Total Asian Population %",
                      "Citizen Asian Population %", "Total Hispanic/Other Population %",
                      "Citizen Hispanic/Other Population %", "Male %", "Female %")

hist_df <- read_xlsx("a1.xlsx", skip = 5, na = "NA")
# Assigns proper column names 
colnames(hist_df) <- voting_col_names
# Converts all columns to numeric
hist_df[] <- lapply(hist_df, function(x) as.numeric(x))
# Converts year column to a categorical value
hist_df$Year <- as.factor(hist_df$Year)
# Total data
total_voted <- hist_df[1:28,]
total_registered <- hist_df[33:60,]
# 18 - 24 year olds data
young_voted <- hist_df[66:93,]
young_registered <- hist_df[99:126,]
# 25 - 44 year olds data
adult_voted <- hist_df[132:159,]
adult_registered <- hist_df[165:192,]
# 45 - 64 year olds data
mid_aged_voted <- hist_df[198:225,]
mid_aged_registered <- hist_df[231:258,]
# 65+ year olds data
elderly_voted <- hist_df[264:291,]
elderly_registered <- hist_df[297:323,]
# Adds age groups
total_voted <- data.frame(append(total_voted, c(Age="Total"), after=0))
total_registered <- data.frame(append(total_registered, c(Age="Total"), after=0))
young_voted <- data.frame(append(young_voted, c(Age="18 to 24"), after=0))
young_registed <- data.frame(append(young_registered, c(Age="18 to 24"), after=0))
adult_voted <- data.frame(append(adult_voted, c(Age="25 to 44"), after=0))
adult_registered <- data.frame(append(adult_registered, c(Age="25 to 44"), after=0))
mid_aged_voted <- data.frame(append(mid_aged_voted, c(Age="45 to 64"), after=0))
mid_aged_registered <- data.frame(append(mid_aged_registered, c(Age="45 to 64"), after=0))
elderly_voted <- data.frame(append(elderly_voted, c(Age="65+"), after=0))
elderly_registered <- data.frame(append(elderly_registered, c(Age="65+"), after=0))

# === Shiny code ==== #

ui <- fluidPage(

    # Application title
    h3(titlePanel("Voting Project Title")),
    # TODO: Add buttons seperate buttons for voting/registration
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("main_inputs", h3("Data Selected:"), 
                        choices = list("Historical" = 1, 
                                       "Etc." = 2),
                        selected = 1)),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("hist_plots"),
           tableOutput("hist_table"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # TODO: A user will be able to select age groups they desire (Possibily by an array of dataframes? [Sort by index])
    output$hist_plots <- renderPlot({
        # Historical plot will be used
        if(input$main_inputs == 1) {
            ggplot() +
                geom_point(aes(Year, Total.Population..Thousands., group = Age, color = "Total"), data = total_voted) +
                geom_line(aes(Year, Total.Population..Thousands., group = Age, color = "Total"), data = total_voted) + 
                geom_point(aes(Year, Total.Population..Thousands., group = Age, color = "18 to 24 yrs"), data = young_voted) +
                geom_line(aes(Year, Total.Population..Thousands., group = Age, color = "18 to 24 yrs"), data = young_voted) +
                geom_line(aes(Year, Total.Population..Thousands., group = Age, color = "25 to 44 yrs"), data = adult_voted) +
                geom_point(aes(Year, Total.Population..Thousands., group = Age, color = "25 to 44 yrs"), data = adult_voted) +
                geom_line(aes(Year, Total.Population..Thousands., group = Age, color = "45 to 64 yrs"), data = mid_aged_voted) +
                geom_point(aes(Year, Total.Population..Thousands., group = Age, color = "45 to 64 yrs"), data = mid_aged_voted) +
                geom_line(aes(Year, Total.Population..Thousands., group = Age, color = "65+ yrs"), data = elderly_voted) +
                geom_point(aes(Year, Total.Population..Thousands., group = Age, color = "65+ yrs"), data = elderly_voted) + 
                ylab("Total Population (In Thousands)") + 
                ggtitle("Voting Between Young People Compared to Other Age Groups (1964-2018)") +
                labs(color='Age Groups') + 
                xlab("Years (1964-2018)") + 
                theme_minimal() +
                theme(axis.text.x=element_blank())
        }
    })
    
    output$hist_table <- renderTable({
        if(input$main_inputs == 1) {
            total_voted
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
