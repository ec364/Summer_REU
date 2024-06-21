#### Hubbard Brook Aquatic Insect Emergence Shiny App
#### Code contributors: Erin Chen, Heili Lowman

#### Setup ####

# Anything that should only happen ONCE should be placed in this setup section, prior to the actual shiny structure.

# Load all necessary packages.
library(shiny)
library(shinythemes)
library(tidyverse)
library(viridis)
library(bslib)
library(lubridate)
library(shinyWidgets)
library(plotly)

# Load all necessary data.
df <- trees # using a built-in R dataset of cherry trees
sticky <- read_csv("data_raw/sticky_trap_counts.csv")

# Perform all data transformation needed
# prior to use in the shiny application here.
sticky2 <- sticky %>% 
  mutate(dipteran = dipteran_large + dipteran_small,
         terrestrial = terrestrial_large + terrestrial_small,
         caddisfly = caddisfly_large + caddisfly_small,
         other = other_large + other_small)

stickysum <- sticky2 %>% ##count grouped by date and watershed 
  select(sample_id, side_or_trapnum, watershed, date, dipteran, terrestrial, caddisfly, other, mayfly_large, stonefly_large) %>% 
  group_by(date, watershed) %>% 
  summarize(
    dipteranSum = sum(dipteran, na.rm = TRUE),
    terrestrialSum = sum(terrestrial, na.rm = TRUE),
    caddisflySum = sum(caddisfly, na.rm = TRUE),
    otherSum = sum(other, na.rm = TRUE),
    mayflySum = sum(mayfly_large, na.rm = TRUE),
    stoneflySum = sum(stonefly_large, na.rm = TRUE),
    .groups = 'drop'
  )

#### User Interface ####

# More shiny themes available at https://rstudio.github.io/shinythemes/
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # App title
                div(style = "text-align: center;",
                    titlePanel(title = "Emergent Aquatic Insects of Hubbard Brook", windowTitle = "Hubbard Brook Insects")
                ),
                
                # Custom CSS for paragraph indentation
                tags$style(HTML("
        .indented-paragraph {
            text-indent: 50px;
        }
    ")),
             
                # Custom CSS for hanging indent
                tags$style(HTML("
        .bibliography {
            margin-bottom: 20px; /* Optional: Add some space between bibliography entries */
            padding-left: 20px;
            text-indent: -20px;
        }
    ")),
                # Main panel for displaying outputs
                mainPanel(width = 12,
                          
                      # Output: set of 4 tabs
                      tabsetPanel(type = "tabs",
                                      
                              #### Introduction tab ####        
                              tabPanel("Introduction",
                                       
                                  h3("What is Aquatic Insect Emergence and Why is it Important?"),
                                  
                                  # for info on arranging rows see this blogpost:
                                  # https://rshiny.blog/2023/05/04/shinys-grid-layout-or-how-to-arrange-elements-side-by-side/
                                  fluidRow(
                                    
                                    column(width = 7, # paragraphs will take up 7/12 of the page
                                           
                                          p(class = "indented-paragraph",
                                          "In some species of aquatic insects, life cycles are divided between a larval stage in an aquatic environment and the emergence at the adult stage to the terrestrial environment", tags$a(href ="https://onlinelibrary.wiley.com/doi/10.1111/geb.13700", "(Nash et al., 2023)."), "Many species of aquatic insects serve as shredders, who are macroinvertebrates that play animportant role in the food system by turning coarse leaf litter from annual leaf fall into finer particulate organic matter", tags$a(href = "https://academic.oup.com/bioscience/article-lookup/doi/10.2307/1310804", "(Cummins et al., 1989)."), "Once shredders convert the leaf litter into a mixture of fecal matter and organic material, the macroinvertebrates known as collectors are able to consume the particulate matter through filtration. Other aquatic feeding groups include scrapers, who feed on algae, and predators, who feed on prey", tags$a(href = "https://academic.oup.com/bioscience/article-lookup/doi/10.2307/1310804", "(Cummins et al., 1989)."), "When aquatic insects emerge from the water, they act as an important transporter of nutrients from aquatic to terrestrial ecosystems", tags$a(href = "http://link.springer.com/10.1007/s10021-016-0050-7", "(Schindler & Smits, 2017).")),
                                          
                                         #this is the start to a new paragraph 
                                          p(class = "indented-paragraph", 
                                          "Since the emergence of aquatic insects is an essential resource pulse for consumers such as other insects, birds, and mammals, changes in the emergence patterns of aquatic insects can threaten the availability of quality food sources in the ecosystem. Shifts in water temperatures, chemistry, and velocity can all impact the size, quantity, and time frame in which insects emerge", tags$a(href = "https://conbio.onlinelibrary.wiley.com/doi/10.1111/cobi.13477", "(Baranov et al., 2020;"), tags$a(href = "https://pnas.org/doi/10.1073/pnas.2310513121", " Leathers et al., 2024;"), tags$a(href = "https://linkinghub.elsevier.com/retrieve/pii/S0960982222001191", "Shipley et al., 2022)."), "These changes can result in a phenological mismatch between the emergence of aquatic insects and their terrestrial predators, leading to an overall decrease in the transfer of nutrients to the terrestrial ecosystem. One such nutrient is essential fatty acids that can only be found in aquatic insects, who accumulate the fatty acids from their algae rich diets", tags$a(href = "https://linkinghub.elsevier.com/retrieve/pii/S0960982222001191", "(Shipley et al., 2022)."), "Since these fatty acids can be crucial for the development of some terrestrial predators, the misalignment between insect life cycles and terrestrial predators can lead to an overall decrease in survival fitness for some terrestrial species", tags$a(href = "https://linkinghub.elsevier.com/retrieve/pii/S0960982222001191", "(Shipley et al., 2022)."))
                                         ), # closes column containing text only
                                    
                                    fluidRow(
                                      
                                      column(width = 4, # image + caption will take up 4/12 of page
                                             
                                           tags$img(src = "nutrients2.png",
                                                    width = "75%"), # edits image size only
                                           tags$figcaption("Figure 1: This figure demonstrates differences in nutritional content by aquatic vs terrestrial insect emergence. The availability of eicosapentaenoic acid (EPA), a type of polyunsaturated fatty acid (PUFA), is far greater in aquatic than terrestrial insects. Source: Shipley et al 2022. ")
                                           
                                           )
                                      
                                      )
                                    
                                    ),
                                  
                                  h3("Collecting Aquatic Insects in Hubbard Brook "),
                                  
                                  fluidRow(
                                    
                                    column(width = 6, # image + caption will take up 5/12 of the page
                                           tags$img(src = "Watershed_Location.png", 
                                                     width = "80%"), # edits image size only
                                           tags$figcaption("Figure 2: This map shows the location of watersheds in which sticky traps are set (denoted in yellow). Source: Edwards, 2022, Adapted from Holmes & Liken, 1999.")
                                           
                                           ),
                                    
                                    column(width = 6, # text will extend 6/12 of the page
                                           
                                          p(class = "indented-paragraph", 
                                          "Located in the White Mountains of New Hampshire, the Hubbard Brook Experimental Forest has been the host
                                          site of continuous water chemistry monitoring since 1963 (Edwards, 2022). In 2018, researchers began 
                                          collecting the sticky traps records of aquatic insects above eight different streams in the Hubbard Brook
                                          Experimental Forest. Five double sided sticky traps collected weekly were attached to a tree branch in a
                                          20m long section next to a stream. These traps were set in watersheds labeled 1, 2 , 3, 4, 5, 6, 9, 
                                          and Hubbard Brook (Edwards, 2022). Researchers at the site have carried out numerous treatments to the 
                                          forest and its watersheds to study the impacts on the overall ecosystem. Watershed 1 was exposed to a 
                                          calcium treatment in 1999 to counteract decreases in soil pH resulting from acid rain, (Likens, 2013). 
                                          Watersheds 2, 3, and 4 were exposed to varying levels of deforestation in different time intervals ranging 
                                          from 1965-1984. Watershed 3 serves as a hydrological control watershed and watershed 6 serves as a 
                                          biogeochemical reference site (Likens, 2013). Watershed 9 has not been exposed to any experiments. Each trap 
                                          site was located in a different watershed in the forest, and hence exposed to the unique spatial landscapes on each watershed. The data collected by the sticky traps as well as the continuous chemical and physical sampling in Hubbard Brook can help reveal how the distinct environmental traits of each site lead to changes in aquatic insect emergence.")
                                          
                                          )
                                    
                                    ),
                                  
                                  h4("Please see the Works Cited tab for complete references."),
                                  
                                  ), # closes out introduction tab - CHECK TO BE SURE
                                      
                              ### at a glance tab
                              

                                      #### Methods tab ####
                                      tabPanel("Methods", 
                                               h3("In field collection"),
                                               fluidRow(
                                                 column(width = 7, 
                                                        p(class = "indented-paragraph", "Insect collection began in April 2018 (Edwards, 2022). Insects 
                                               are collected weekly on double sided 4” x 7” sticky traps from watersheds labeled 1, 2, 3, 4, 5, 6, 9,
                                               and Hubbard Brook (Edwards, 2022). At each collection site, five sticky traps were spread across a 
                                               20m long section. The traps were attached to tree branches along a stream in each 
                                               watershed (Edwards, 2022). The sticky traps are set out after ice melts each year (around March or April)
                                               collection ends when traps are found to be sparsely filled (November or December). Once collected, 
                                               sticky traps were placed inside a plastic page protector and shipped to the Bernhardt Lab for identification."),
                                                        ),
                                                   column(width = 4,
                                                          tags$img(src = "trap_location.png", width = "80%", height = "80%"),
                                                          tags$figcaption("Figure 3: Sticky Traps employed in the field. Double sided traps are laid out in a 20m long strip along the stream to 
                                                          capture emerging insects. Source: Edwards, 2022.")
                                                          )
                                                 
                                               ),
                                               h3("Insect Identification"),
                                               fluidRow(
                                                 column(width = 7,
                                                        p(class = "indented-paragraph", "At the Bernhardt Lab, insects were identified using dissecting 
                                                microscopes and a color coding system (Edwards, 2022). Insects were labeled by the categories: 
                                                 Terrestrial Diptera, Aquatic Diptera, Caddisflies, Mayflies, Stoneflies, or Other. Insects 
                                                 were also labeled for their size, with “Small” insects composing of bodies less than 5mm 
                                                 and “Large” insects composing of bodies greater than 5mm. Marks were made directly on the sheet
                                                 , with different colors representing different orders and different shapes (dash or circle) 
                                                 representing different sizes. Count of each order and size is tallied up and marked on the side
                                                 of the paper protector. Information about each trap was uploaded to the hbwater database.")),
                                                 column(width = 4,
                                                        tags$img(src = "annotated_trap.png", width = "50%", height = "40%"),
                                                        tags$figcaption("Figure 4: An example of a sticky trap after annoation. Dashes and circles denote size of insect. Color of annotation represents order. Count of insects by order and size on the right. Photo taken by Erin Chen.")
                                                 )
                                               )
                                      ), 
                                  
                                      
                                      #### Plotting tab ####
                                      tabPanel("Insect Count Over Time",
                                      #inserting a slider for year
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   dateRangeInput("timeRange", 
                                                                  label = h3("Date range"),
                                                                  min = as.Date("2018-01-01","%Y-%m-%d"),
                                                                  max = as.Date("2023-12-31","%Y-%m-%d"),
                                                                  start = as.Date("2018-01-01","%Y-%m-%d"),
                                                                  end = as.Date("2023-12-31","%Y-%m-%d")
                                                                  ),
                                                   checkboxGroupInput("bugType", 
                                                               "Select Bug Type:",
                                                               choices = c("Stoneflies" = "stonefly_large", 
                                                                           "Caddisflies" = "caddisfly", 
                                                                           "Mayflies" = "mayfly_large", 
                                                                           "Dipteran" = "dipteran",
                                                                           "Terrestrial" = "terrestrial",
                                                                           "Other" = "other"),
                                                               selected = "stonefly_large"),
                                                   helpText("Stoneflies refer to the order Plecoptera. Caddisflies refer to the order Trichoptera.
                                                            Mayflies refer to the order Ephemeroptera. Dipteran refers to aquatic blackflies. 
                                                            Terrestrial refers to non-aquatic terrestrial flies. Other refers to any insect that does not 
                                                            follow into the categories above, such as Hymenopterans, Neuroptera, Coleoptera, etc."),
                                                   checkboxGroupInput(
                                                     inputId = "shedInput",
                                                     label = "Choose a watershed",
                                                     choices = c("1", "2", "3", "4", "5", "6","9", "Hubbard Brook"), #use filter in server: filter(column_name %in% list) %>%
                                                     selected = "6",
                                                     inline = TRUE
                                                   ),
                                                   helpText("This app utilizes Plotly, an interactive plotting program, to visualize to data. 
                                                            Icons in the top right corner offers users the ability to capture photos of data, zoom in, and return to original display.
                                                            Other elements on the graph it self, such as legends, can provide further information and displays for the user upon clicking or hovering,
                                                            to return to the original view, click the home button on the top left corner.")
                                                 ),
                                                
                                               
                                               # prints plot generated in server
                                               mainPanel(
                                                
                                                 plotlyOutput("stoneflyPlot", width = "800px", height = "500px")      
                                      )
                                      )
                                      ), # closes plot tab - DO NOT DELETE
                              ####plotting peak emergence tab####
                              tabPanel("Peak Emergence",
                                       sidebarLayout(
                                         sidebarPanel(
                                           awesomeRadio(inputId = "bugType2", 
                                                              label = "Select Bug Type:",
                                                              choices = c("Stoneflies" = "stoneflySum", 
                                                                          "Caddisflies" = "caddisflySum", 
                                                                          "Mayflies" = "mayflySum", 
                                                                          "Dipteran" = "dipteranSum",
                                                                          "Terrestrial" = "terrestrialSum",
                                                                          "Other" = "otherSum"),
                                                              selected = "dipteranSum"),
                                           helpText("Stoneflies refer to the order Plecoptera. Caddisflies refer to the order Trichoptera.
                                                            Mayflies refer to the order Ephemeroptera. Dipteran refers to aquatic blackflies. 
                                                            Terrestrial refers to non-aquatic terrestrial flies. Other refers to any insect that does not 
                                                            follow into the categories above, such as Hymenopterans, Neuroptera, Coleoptera, etc."),
                                           checkboxGroupInput(
                                             inputId = "shedInput2",
                                             label = "Choose a watershed",
                                             choices = c("1", "2", "3", "4", "5", "6","9", "Hubbard Brook"), #use filter in server: filter(column_name %in% list) %>%
                                             selected = "6",
                                             inline = TRUE
                                           ),
                                           helpText("This app utilizes Plotly, an interactive plotting program, to visualize to data. 
                                                            Icons in the top right corner offers users the ability to capture photos of data, zoom in, and return to original display.
                                                            Other elements on the graph it self, such as legends, can provide further information and displays for the user upon clicking or hovering,
                                                            to return to the original view, click the home button on the top left corner.")
                                         ),
                                         
                                         
                                         # prints plot generated in server
                                         mainPanel(
                                           plotlyOutput("EmergencePlot", width = "800px", height = "500px")      
                                         )
                                       ) ),
                                      
                                      #### ML Development tab ####
                                      tabPanel("Machine Learning",
                                               h3("What's happening"),
                                               p("Beginning in May 2024, the lab is looking into incorporating machine 
                                               learning into the insect identification process. The limited biodiversity 
                                               in the Hubbard Brook Ecosystem makes this project an excellent candidate 
                                               for supervised object oriented machine learning. As of June 2024, a sample 
                                               training dataset has been composed using a photo annotation software called 
                                               ", tags$a(href ="https://www.robots.ox.ac.uk/~vgg/software/via/via.html", "VGG Image Annotator (VIA).")),
                                               h4("Stay tuned for further updates!"),
                                               p(
                                                 div(
                                                   style = "text-align: center;",
                                                   tags$img(src = "VGG.png", width = "40%", height = "10%"),
                                                   tags$figcaption("Screenshot of the photo annotation process using", tags$a(href ="https://www.robots.ox.ac.uk/~vgg/software/via/via.html", "VGG Image Annotator (VIA)."), style = "width: 40%; margin: 0 auto; text-align: center;")
                                                 )
                                               )),

                                      
                                      
                                      #### Works Cited tab ####
                                      tabPanel("Works Cited",
                                               h4("Works Cited"),
                                               div(
                                                 p(class = "bibliography", "Baranov, V., Jourdan, J., Pilotto, F., Wagner, R., & Haase, P. (2020). Complex and nonlinear climate‐driven changes in freshwater insect communities over 42 years. Conservation Biology, 34(5), 1241–1251.", tags$a(href ="https://doi.org/10.1111/cobi.13477", "https://doi.org/10.1111/cobi.13477")),
                                                 p(class = "bibliography", "Cummins, K. W., Wilzbach, M. A., Gates, D. M., Perry, J. B., & Taliaferro, W. B. (1989). Shredders and Riparian Vegetation. BioScience, 39(1), 24–30.", tags$a(href =" https://doi.org/10.2307/1310804", "https://doi.org/10.2307/1310804")),
                                                 p(class = "bibliography", "Edwards, Tyler. 2022. (2022). Adult Insects and a Baby Record: Assessing Aquatic Insect Emergence at the Hubbard Brook Experimental Forest [Unpublished undergraduate thesis]. Duke University."),
                                                 p(class = "bibliography", "Leathers, K., Herbst, D., De Mendoza, G., Doerschlag, G., & Ruhi, A. (2024). Climate change is poised to alter mountain stream ecosystem processes via organismal phenological shifts. Proceedings of the National Academy of Sciences, 121(14), e2310513121", 
                                                   tags$a(href ="https://doi.org/10.1073/pnas.2310513121", "https://doi.org/10.1073/pnas.2310513121")),
                                                 p(class = "bibliography", "Likens, Gene E.. Biogeochemistry of a Forested Ecosystem. United States, Springer New York, 2013."),
                                                 p(class = "bibliography", "Nash, L. N., Zorzetti, L. W., Antiqueira, P. A. P., Carbone, C., Romero, G. Q., & Kratina, P. (2023). Latitudinal patterns of aquatic insect emergence driven by climate. Global Ecology and Biogeography, 32(8), 1323–1335."
                                                   ,tags$a(href ="https://doi.org/10.1111/geb.13700", "https://doi.org/10.1111/geb.13700")),
                                                 p(class = "bibliography", "Schindler, D. E., & Smits, A. P. (2017). Subsidies of Aquatic Resources in Terrestrial Ecosystems. Ecosystems, 20(1), 78–93."
                                                   ,tags$a(href ="https://doi.org/10.1007/s10021-016-0050-7", "https://doi.org/10.1007/s10021-016-0050-7")),
                                                 p(class = "bibliography", "Shipley, J. R., Twining, C. W., Mathieu-Resuge, M., Parmar, T. P., Kainz, M., Martin-Creuzburg, D., Weber, C., Winkler, D. W., Graham, C. H., & Matthews, B. (2022). Climate change shifts the timing of nutritional flux from aquatic insects. Current Biology, 32(6), 1342-1349.e3." 
                                                   , tags$a(href ="https://doi.org/10.1016/j.cub.2022.01.057", "https://doi.org/10.1016/j.cub.2022.01.057)"))
                                               )
                                               
                                      ) # closes out final panel
                                      
                          ) # closes out set of tabs
                          
                ) # closes out main panel 
                
)

#### Server ####

# Define server logic required to generate figures
server <- function(input, output) {
  
  # Creates plot for inclusion on plotting tab.
  output$stoneflyPlot <- renderPlotly({
    filtered_data <- sticky2 %>% 
      mutate(Date = as.Date(date, "%Y-%m-%d")) %>% 
      filter(Date >= input$timeRange[1] & Date <= input$timeRange[2]) %>%
      filter(watershed %in% input$shedInput)
    
    # set the list of chosen insects to the new variable "chosenBugs"
    chosenBugs <- input$bugType
    
    #bug input aggreg
    agg_data <- filtered_data %>%
      # first need to pivot data from wide format to long format
      # pivoting the dipteran_large through other_small columns
      # column names will appear in one column and values in another
      pivot_longer(cols = dipteran_large:other, 
                   names_to = "taxa", # names the new names column
                   values_to = "count") %>% # names the new values column
      # there will be lots of NAs, but that's alright, that just means
      # that none of those insects were counted for that trap
      # now we can filter by chosen insect inputs
      filter(taxa %in% chosenBugs) %>%
      # group by every month of each year
      group_by(Date, watershed) %>%
      # and finally sum all of the chosen insect counts, omitting NAs
      summarise(total_bug = sum(count, na.rm = TRUE)) %>%
      ungroup() %>% 
      mutate(month = month(Date))
    
    #plotting the graph 
    BugCountPlot <- agg_data %>% 
        ggplot(aes(x = Date, y = total_bug, color = watershed)) +
      geom_line() +  
      labs(title = "Count of Insects in Hubbard Brook by Time",
           x = "Time Period",
           y = "Insect Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),  # Title font size
        axis.title.x = element_text(size = 15),              # X-axis title font size
        axis.title.y = element_text(size = 15),              # Y-axis title font size
        axis.text.x = element_text(size = 12),               # X-axis text font size
        axis.text.y = element_text(size = 12),               # Y-axis text font size
        legend.title = element_text(size = 14),              # Legend title font size
        legend.text = element_text(size = 12)                # Legend text font size
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
ggplotly(BugCountPlot)
 

        
  })
  #making emergence plot
  output$EmergencePlot <- renderPlotly({
    sticky3 <- stickysum %>%
      mutate(
        Year = year(date), #gives columns year 
        DayOfYear = yday(date) #gives column day of year
      )
    
    filtered_data <- sticky3 %>%
      filter(
        watershed %in% input$shedInput2 #looking in the watershed selected 
      )
    
    max_bugs_per_year <- filtered_data %>%
      group_by(Year, watershed) %>%
      summarise(
        BugCount = max(.data[[input$bugType2]], na.rm = TRUE),
        DayOfYear = DayOfYear[which.max(.data[[input$bugType2]])]
      ) %>%
      ungroup()
    
    max_bugs_per_year <- max_bugs_per_year %>%
      filter(!is.na(DayOfYear))
   
    bug_max <- ggplot(max_bugs_per_year, aes(x = Year, y = DayOfYear, size = BugCount, color = watershed)) +
      geom_point(alpha = 0.7) +
      scale_y_continuous(name = "Day of Year") +
      scale_x_continuous(name = "Year", breaks = 2018:2024, limits = c(2018, 2024))  +
      labs(
        title = "Maximum Number Each Year by Watershed",
        caption = "Size of point denotes magnitude of count.", ##does not work
        size = "Max Bugs",
        color = "Watershed",
        
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      ) +
      guides(size = guide_legend(size = guide_legen("BugCount")), color = guide_legend("watershed")) #does not work
 
plotly_bug_max <- ggplotly(bug_max)

plotly_bug_max %>% layout(margin = list(l = 50, r = 50, b = 100, t = 50),
                          annotations = list(x = 1, y = -0.3, text = "Note: size of point indicate magnitude of count.",
                                             xref='paper', yref='paper', showarrow = F, 
                                             xanchor='right', yanchor='auto', xshift=5, yshift=5,
                                             font = list(size = 10)))


  })
}

#### Bind UI & Server ####

 

# Run the application 
shinyApp(ui = ui, server = server)

# End of script.



