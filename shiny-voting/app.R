library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(reshape2)

# Apologies for this garbage, will be heavily revised later

# TODO: Perhaps put data wrangling into a seperate R script 
# to write into a csv file and read into this Shiny app to
# avoid current code clutter

# === Wrangling data ==== #
# Uncleaned historical data
hist_df <- read_xlsx("a1.xlsx", skip = 5, na = "NA")
# Different voting category names to put into columns for the dataframe
voting_col_names <- c("Year", "Population-Count", "Population-Percent",
                      "Citizen-Percent", "White-Percent", "Citizen-White-Percent", 
                      "White-Non-Hispanic-Percent", "Citizen-White-Non-Hispanic-Perc", 
                      "Black-Percent", "Citizen-Black-Percent", "Asian-Percent",
                      "Citizen-Asian-Percent", "Hispanic/Other-Percent",
                      "Citizen-Hispanic/Other-Perc", "Male-Percent", "Female-Percent")
# Assigns proper column names 
colnames(hist_df) <- voting_col_names
# Converts all columns to numeric
hist_df[] <- lapply(hist_df, function(x) as.numeric(x))
# Converts year column to a categorical value
hist_df$Year <- as.factor(hist_df$Year)

# 1964 to 2018 (Goes by every 2 year)
years_length <- (2018 - 1964) / 2
# List of proper age groups and voting status to put into dataframe
age_groups <- c("Total", "18 to 24", "25 to 44", "45 to 64", "65+")
vote_status <- c("Voted", "Registered")

temp_df <- data.frame()

# Integer values representing current index values of corresponding list
curr_status <- 1
curr_age <- 1

# Loop to cleans up hist_df (Loop increments are added by 6 to account for missing columns)
for(i in seq(1, nrow(hist_df), years_length + 6)) {
    # Appends new rows to temp_df from subset of hist_df
    if(i != 1) temp_df <- bind_rows(temp_df, hist_df[(i - 1):((i - 1) + years_length), ])
    else temp_df <- bind_rows(temp_df, hist_df[i:(i + years_length), ])
}

# Appends new columns for age groups and voting statuses
temp_df <- data.frame(append(temp_df, c(Age = age_groups[curr_status], 
                                        Status = vote_status[curr_age]),
                             after = 0), stringsAsFactors = FALSE)

temp_index = 1
# Loop that corrects age group and voting status rows
for(i in 1:(length(age_groups) * length(vote_status))) {
    temp_df$Age[temp_index:(temp_index + years_length)] <- age_groups[curr_age]
    temp_df$Status[temp_index:(temp_index + years_length)] <- vote_status[curr_status]
    temp_index <- (temp_index + years_length) + 1
    # Updates age group and voting status to match rows
    if(curr_status == 1) curr_status = curr_status + 1
    else {
        curr_status = 1
        curr_age = curr_age + 1
    }
}
# Removes unnessary rows at the nd
hist_df <- temp_df[1:279, ]

hist_df$Age <- as.factor(hist_df$Age)
hist_df$Status <- as.factor(hist_df$Status)

# === Shiny code ==== #

ui <- fluidPage(

    h3(titlePanel("Voting Project Title")),
    
    sidebarLayout(
        sidebarPanel(
            # Main inputs (Can change from historical to state to education data)
            selectInput("main_inputs", h3("Data Selected:"), 
                        choices = list("Historical" = 1, 
                                       "Etc." = 2),
                        selected = 1),
            checkboxGroupInput("hist_ages", "Choose an age group:",
                               choices = list("Total" = 1,"Young" = 2,
                                              "Adult" = 3,"Mid Age" = 4,
                                              "Elderly" = 5),
                               selected = c(1, 2, 3, 4, 5)),
            radioButtons("hist_status", "Choose voting status:",
                         choices = list("Voted" = 1, "Registered" = 2),
                         selected = 1)
            
            ),

        mainPanel(
           plotOutput("hist_plots"),
           dataTableOutput("hist_table"),
        )
    )
)

server <- function(input, output) {
    output$hist_plots <- renderPlot({
        # Historical plot will be used
        if(input$main_inputs == 1) {
            # Filtered age categories to use in plot
            select_category <- c()
            cnt = 1
            # Loop to check what categories should be used based on input
            for(i in 1:length(age_groups)) {
                if(i %in% input$hist_ages) {
                    select_category[cnt] <- age_groups[i]
                    cnt <- cnt + 1
                }
            }
            # Plot of filtered and selected age groups 
            plot_df <- hist_df[hist_df$Age %in% select_category, ]
            
            ggplot(data = plot_df) +
                aes(Year, Population.Count, group = Age, color = Age) + 
                geom_point() + geom_line() + 
                ggtitle("Voting Between Young People Compared to Other Age Groups (1964-2018)") +
                ylab("Total Population (In Thousands)") + 
                xlab("Years (1964-2018)") + 
                theme_minimal() +
                theme(axis.text.x = element_blank())
        }
    })
    
    output$hist_table <- renderDataTable({
        if(input$main_inputs == 1) {
            hist_df %>% 
                filter(Age == "Total" & Status == "Voted") %>% 
                select(Year, Population.Count) %>% 
                rename(`Pop. Count (In Thousands)` = Population.Count)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
