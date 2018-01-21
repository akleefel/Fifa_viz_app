#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Alexander Kleefeldt - January 2018
#
#some code borrowed from the shiny movie explorer app:
#https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer




library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(cowplot)
library(ggvis)

#reading in data 
data_fifa <- read_csv("data/fifa_clean.csv")

#variables that can be picked for x and y axis
axis_vars <- c(
  "Overall Skill" = "Overall_skill",
  "Age" = "age",
  "Height (cm)" = "Height_cm",
  "Weight (kg)" = "Weight_kg",
  "Pace" = "Pace",
  "Shooting" = "Shooting",
  "Dribbling" = "Dribbling",
  "Passing" = "Passing",
  "Defending" = "Defending",
  "Physical" = "Physical"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Fifa 2018 Attribute Explorer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        countries <- selectInput("countryInput",h3("Country"),
                                  c("England", "France", "Germany","Italy", "Spain")),
        
        uiOutput("secondSelection"),
        
        position <- selectInput("positionInput",h3("Position"),
                                c("Defence", "Midfield", "Attack")),
        br(),
        
      wellPanel(
        selectInput("xvar", "X-axis variable", axis_vars, selected = "Height_cm"),
        selectInput("yvar", "Y-axis variable", axis_vars, selected = "Overall_skill")
      ),
      
      br(),
      
      
      wellPanel(
        span("Number of players selected:",
             textOutput("n_players")))
      
      
      ),
      
      #Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Visualization",ggvisOutput("plot1"),ggvisOutput("plot2")),
          tabPanel("Data Explorer",dataTableOutput("results"))
          
            
          
        
      ))))
   
   
   


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  fifa_filtered <- reactive({
    
    fifa_filt <- data_fifa %>% filter(country == input$countryInput,
                                      league %in% input$Competitions,
                                      position == input$positionInput)
    
    fifa_filt <- as.data.frame(fifa_filt)
    
    fifa_filt
    
  })
  
  # Function for generating tooltip text
  fifa_tooltip <- function(x) {
     if (is.null(x)) return(NULL)
     if (is.null(x$ID)) return(NULL)

    
    #Pick out the movie with this ID
    all_players <- isolate(fifa_filtered())
    player <- all_players[all_players$ID == x$ID, ]
    
    paste0("<b>", player$name, "</b><br>",
           player$club,"</b><br>", "Overall Skill: ", player$Overall_skill )
    
  }
  
  
  
  vis <- reactive({


    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]


    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))

    plot1 <- fifa_filtered %>%
        ggvis(x = xvar, y = yvar, fill = ~league) %>%
        add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name) %>%
        layer_points(size := 50, size.hover := 200,
                     fillOpacity := 0.4, fillOpacity.hover := .8, key := ~ID) %>%
      add_tooltip(fifa_tooltip, "hover") %>%
      layer_smooths(opacity:= 0.4, fill:= "Blue", span = .8) %>% 
      set_options(width = 700, height = 500)
      


    plot1
      })

  vis %>% bind_shiny("plot1")
  
  vis2 <- reactive({
    

    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    plot2 <- fifa_filtered %>% 
      ggvis(x=xvar) %>%   
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = "Density") %>%
      set_options(width = 650, height = 400) %>% 
      layer_densities() 
    
  
    
    plot2
  })
  
  vis2 %>% bind_shiny("plot2")
  
  
  var <- reactive({
    
    
    #"All" = as.list(data_fifa %>% distinct(league))$league,
    switch(input$countryInput,
           "England" = as.list(data_fifa %>% filter(country == "England") %>% distinct(league))$league,
           "France" = as.list(data_fifa %>% filter(country == "France") %>% distinct(league))$league,
           "Germany" = as.list(data_fifa %>% filter(country == "Germany") %>% distinct(league))$league,
           "Italy" = as.list(data_fifa %>% filter(country == "Italy") %>% distinct(league))$league,
           "Spain" = as.list(data_fifa %>% filter(country == "Spain") %>% distinct(league))$league)
    
  })
  
  output$secondSelection <- renderUI({
    checkboxGroupInput("Competitions", h3("Select Competitions"), choices = var(), selected = head(var()))
    
  })
  
  output$results <- renderDataTable({
    filtered <-
      data_fifa %>% filter(country == input$countryInput,
                           league %in% input$Competitions,
                           position == input$positionInput
                           
                           ) %>% select(name, club, league, Overall_skill, age, Height_cm, Weight_kg,  Passing,
                                        Shooting, dribbling, Pace, Defending, Physical)
                      
    
    
  })
  
  output$n_players <- renderText({ nrow(fifa_filtered()) })
}

# Run the application 
shinyApp(ui = ui, server = server)

