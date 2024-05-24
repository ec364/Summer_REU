#### Hubbard Brook Aquatic Insect Emergence Shiny App
#### Code contributors: Erin Chen, Heili Lowman

#### Setup ####

# Anything that should only happen ONCE should be placed in this setup section, prior to the actual shiny structure.

# Load all necessary packages.
library(shiny)
library(shinythemes)
library(tidyverse)
library(viridis)

# Load all necessary data.
df <- trees # using a built-in R dataset of cherry trees

# Perform all data transformation needed
# prior to use in the shiny application here.

#### User Interface ####

# More shiny themes available at https://rstudio.github.io/shinythemes/
ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # App title
  titlePanel(title="Emergent Aquatic Insects of Hubbard Brook"),
  
  # Main panel for displaying outputs
  mainPanel(width = 12,
            
    # Output: set of 4 tabs
    tabsetPanel(type = "tabs",
                        
      #### Introduction tab ####        
      tabPanel("Introduction"),
      
      #### Methods tab ####
      tabPanel("Methods"),
      
      #### Plotting tab ####
      tabPanel("Plot",
               
               # line break
               br(),
               
               # prints plot generated in server
               plotOutput(outputId = "practicePlot",
                          width = 500, height = 250)
               
               ),
      
      #### ML Development tab ####
      tabPanel("Machine Learning")
      
    ) # closes out set of tabs
    
  ) # closes out main panel

)

#### Server ####

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Creates plot for inclusion on plotting tab.
    output$practicePlot <- renderPlot({
        
      ggplot(df, aes(x = Girth, y = Height, color = Volume)) +
        geom_point(size = 5, alpha = 0.8) +
        labs(x = "Diameter (in)",
             y = "Height (ft)",
             color = "Volume (cubic ft)") +
        scale_color_viridis() +
        theme_bw()
      
    })
    
}

#### Bind UI & Server ####

# Run the application 
shinyApp(ui = ui, server = server)

# End of script.

