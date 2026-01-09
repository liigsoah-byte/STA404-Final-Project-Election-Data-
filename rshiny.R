library(shiny)
library(tidyverse)
library(readr)
library(dplyr)
library(magrittr)
library(maps)
library(ggplot2)
library(sf)
library(extrafont)
library(ggtext)

load("full_data.Rdata")
load("electoral_votes_data.Rdata")
load("win_state.Rdata")
load("us_50_states.Rdata")


# CUSTOM THEME FOR MAP AND GRAPHS
theme_map <- function() {
  theme(axis.line = element_blank(),  
        axis.text = element_blank(),  
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        # plot title and subtitle
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#3E3E3E", family = "Times New Roman"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "#3E3E3E", family = "Times New Roman"),
        # panel border
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),     
        panel.grid = element_blank(),       
        panel.background = element_rect(fill = "gray90", color = NA),
        
        # legend
        legend.justification = c(0, 0),
        legend.position.inside = c(0.01, 0.02), 
        legend.background = element_rect(fill = NA , color = NA),
        legend.title = element_text(hjust = 0.5, size = 8, face = "italic", color = "#3E3E3E", family = "Times New Roman"),
        legend.text = element_text(hjust = 0.5, size = 8, face = "italic", color = "#3E3E3E", family = "Times New Roman"))
}

theme_graph <- function() {
  theme(
    panel.background = element_rect(fill = "gray90", color = NA), 
    plot.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5), 
    panel.grid.minor = element_blank(), 
    
    # Text settings
    text = element_text(hjust = 0.5, size = 8, color = "#3E3E3E", family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#3E3E3E", family = "Times New Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold", color = "#3E3E3E", family = "Times New Roman"),
    axis.title.x = element_text(size = 14, face = "bold", family = "Times New Roman"),
    axis.title.y = element_text(size = 14, face = "bold", family = "Times New Roman"),
    axis.text = element_text(size = 12),
    
    # legend
    legend.background = element_rect(fill = "gray90", color = NA), 
    legend.position.inside = c(0.01, 0.02),  
    legend.title = element_text(size = 14, face = "bold", family = "Times New Roman"),
    legend.text = element_text(size = 12, family = "Times New Roman")
  )
}



# RSHINY

ui <- fluidPage(
  titlePanel("US Presidential Elections"),
  
  tabsetPanel(id = "tabs",
              
              # TABS
              tabPanel("About", # CREATE ABOUT TAB
                       br(),
                       
                       # user explanation
                       strong("The User"),
                       p("The person that will use this app is someone who is interested in how the president is voted on. They will be able to see how each state votes. They are able to view the winner of each state in map form, see the total amount of voters in each party over time in line form, and see the breakdown of votes in each state."),
                       br(),
                       
                       #electoral votes table explanation
                       strong("Electoral Votes Table"), 
                       p("This table shows the breakdown of electoral votes in each states. This is helpful for the user to know when viewing the next map, which shows which party won each state."),
                       br(),
                       
                       #cloropleth map explanation
                       strong("Map"),  
                       p("This is a cloropleth map that shows which party won each state. This map can be made for each election year. Also, the user can select a region to look at, as well as a specific state in that region. This can be helpful to identify overall trends in the data for the overall US, a specific region, or a state."),
                       br(),
                       
                       # line graph explanation
                       strong("Line Graph"), 
                       p("This is a line graph that shows the number of voters in each party over the years. The user can select the party (or parties) that they want to see on the graph if they want to compare them. The user can also select the start year to look at the map on a smaller scale. This is helpful to view overall trends in party affiliation over the years."),
                       br(),
                       
                       # bar chart explanation
                       strong("Bar Chart"),
                       p("This is a bar chart that shows the breakdown of votes in each state for each year. The user is able to select the year and the state(s) they wish to analyze. The user can select multiple years to compare the percentage of voters for each party over the years. This is especially important when looking at swing states to see how they have changed over the years.")
              ),
              
              #electoral votes tab
              tabPanel("Electoral Votes Table",
                       mainPanel(
                         tableOutput("electoral_votes_table")  
                       )
              ),
              
              # map tab
              tabPanel("Map",
                       sidebarLayout(
                         sidebarPanel(
                           
                           #select input region, All Regions is baseline
                           selectInput("selected_region", "Select Region",
                                       choices = c("All Regions", unique(full_data$region)), 
                                       selected = "All Regions"),
                           
                           #select input state, All States is baseline
                           selectInput("selected_states", "Select State", 
                                       choices = c("All States", unique(win_state$state_po)), 
                                       selected = "All States"),
                           
                           # select input year, minimum year is base line
                           selectInput("selected_year", "Select Year", 
                                       choices = sort(unique(win_state$year), decreasing = TRUE), 
                                       selected = min(win_state$year))  # Default to the latest year
                         ),
                         mainPanel(
                           plotOutput("map")
                         )
                       )
              ),
              
              # line plot tab
              tabPanel("Line Plot",
                       sidebarLayout(
                         sidebarPanel(
                           
                           # select party input, can select multiple, DEMOCRAT is baseline
                           selectInput("selected_party", "Select Party:", 
                                       choices = c(unique(full_data$party_simplified)),
                                       selected = "DEMOCRAT", multiple = TRUE),
                           
                           # select year input, minimum year is baseline
                           selectInput("min_year", "Select Minimum Year:",
                                       choices = unique(full_data$year), 
                                       selected = min(full_data$year))
                         ),
                         mainPanel(
                           plotOutput("linePlot")
                         )
                       )
              ),
              
              # bar chart tab
              tabPanel("Bar Chart",
                       sidebarLayout(
                         sidebarPanel(
                           
                           # select state, All states is baseline
                           selectInput("selected_state_bar", "Select State:", 
                                       choices = c("All States", unique(full_data$state_po)), 
                                       selected = "OH", multiple = TRUE),
                           
                           # select year input, minimum year is baseline
                           selectInput("selected_year_bar", "Select Year(s):", 
                                       choices = unique(full_data$year), 
                                       selected = min(full_data$year), multiple = TRUE)
                         ),
                         mainPanel(
                           plotOutput("barChart")
                         )
                       )
              )
  )
)


server <- function(input, output, session) {
  
  # ELECTORAL VOTES TABLE
  
  # Render Electoral Votes Table
  output$electoral_votes_table <- renderTable({
    electoral_votes_data %>%
      mutate(electoral_votes = round(electoral_votes, 0)) %>% 
      rename(
        `State Name` = state_po,
        `Electoral Votes` = electoral_votes)
  })
  
  # MAP OUTPUT
  
  # Dynamically update state choices based on selected region
  observeEvent(input$selected_region, {
    # Dynamically set states based on the selected region from "full_data"
    states <- if (input$selected_region == "All Regions") {
      c("All States", unique(full_data$state_po))
    } else {
      full_data %>%
        filter(region == input$selected_region) %>%
        pull(state_po) %>%
        c("All States", .)
    }
    updateSelectInput(session, "selected_states", choices = states, selected = "All States")
  })
  
  # Reactive Filtered Data for Map
  filtered_data_map <- reactive({
    req(input$selected_year)  # Ensure input is not NULL
    full_data %>%
      filter(year == input$selected_year)
  })
  
  # Reactive States and Regions Handling
  selected_states <- reactive({
    # Filter states within the selected region
    region_states <- if (input$selected_region == "All Regions") {
      unique(full_data$state_po)
    } else {
      full_data %>%
        filter(region == input$selected_region) %>%
        pull(state_po)
    }
    
    # Consider both region filter and specific state selections
    if (input$selected_states == "All States") {
      region_states
    } else {
      intersect(region_states, input$selected_states)
    }
  })
  
  # Render Map
  output$map <- renderPlot({
    req(input$selected_year)  # Ensure year is selected
    
    # Prepare Data for Map
    winner_data <- filtered_data_map() %>%
      group_by(state_po) %>%
      summarise(winner = party_simplified[which.max(candidatevotes)], .groups = "drop")
    
    map_data_sf <- left_join(us_50_states, winner_data, by = c("STATE" = "state_po"))
    
    # Add a column for selected vs non-selected states
    map_data_sf <- map_data_sf %>%
      mutate(is_selected = ifelse(STATE %in% selected_states(), "Selected", "Not Selected"))
    
    # Plot Map
    ggplot(map_data_sf) +
      geom_sf(aes(fill = ifelse(is_selected == "Selected", winner, "Not Selected")), color = "black") +
      scale_fill_manual(values = c("DEMOCRAT" = "royalblue", 
                                   "REPUBLICAN" = "red2", 
                                   "LIBERTARIAN" = "darkgreen", 
                                   "OTHER" = "gray",
                                   "Not Selected" = "lightgray")) +
      labs(title = paste("US Presidential Elections:", input$selected_year),
           subtitle = "State-level Results",
           fill = "Winner") +
      theme_void() +
      theme_map()  
  })
  
  # LINE PLOT
  
  # Render Line Plot
  output$linePlot <- renderPlot({
    # Filter data based on selected minimum year and selected party
    filtered_data <- full_data %>%
      filter(party_simplified %in% input$selected_party) %>%  # Filter by selected parties
      filter(year >= input$min_year) %>%  # Filter by selected minimum year
      group_by(year, party_simplified) %>%  # Group by year and party
      summarise(total = sum(candidatevotes, na.rm = TRUE), .groups = "drop")  # Summarize total votes
    
    # Plot the line plot
    ggplot(filtered_data, aes(x = year, y = total, color = party_simplified)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = c("DEMOCRAT" = "royalblue", "REPUBLICAN" = "red2", 
                                    "LIBERTARIAN" = "darkgreen", "OTHER" = "gray")) +
      labs(title = paste("Votes for", paste(input$selected_party, collapse = ", "), 
                         "From Year", input$min_year, "Onward"),
           x = "Year", y = "Total Votes") +
      theme_graph() 
  })
  
  # Filtered Data for Bar Chart
  filtered_data_bar <- reactive({
    data <- full_data %>%
      filter(year %in% input$selected_year_bar) %>%
      filter(if ("All States" %in% input$selected_state_bar) TRUE else state_po %in% input$selected_state_bar) %>%
      group_by(year, state_po, party_simplified) %>%
      summarise(total_votes = sum(candidatevotes, na.rm = TRUE), .groups = "drop")
    
  })
  
  # BAR CHART
  
  # Render Bar Chart
  output$barChart <- renderPlot({
    chart_data <- filtered_data_bar()
    
    # plot bar chart
    ggplot(chart_data, aes(x = factor(year), y = total_votes, fill = party_simplified)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("DEMOCRAT" = "royalblue", "REPUBLICAN" = "red2", 
                                   "LIBERTARIAN" = "darkgreen", "OTHER" = "gray")) +
      labs(title = "Candidate Votes by Party and Year",
           x = "Year", y = "Total Votes", fill = "Party") +
      theme_graph() +
      facet_wrap(~ state_po)
  })
}

# Launch the Shiny App
shinyApp(ui = ui, server = server)