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

# Load all necessary data.
df <- trees # using a built-in R dataset of cherry trees
sticky <- read_csv("data_raw/sticky_trap_counts.csv")
sticky2 <- sticky
# Perform all data transformation needed
# prior to use in the shiny application here.

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
                                        p(tags$figure
                                          (class = "centerFigure",
                                              tags$img(src = "nutrients.png", 
                                                width = "30%", 
                                                height = "30%",
                                                style="float:right"),
                                                tags$figcaption("Source: Shipley et al 2022. This figure demonstrates differences in nutritional content by source (terrestrial vs aquatic insect emergence). The availability of eicosapentaenoic acid (EPA), a type of polyunsaturated fatty acid (PUFA), is far greater in aquatic insects than terrestrial insects.")
                                          )
                                        ),
                                        # p(tags$img(src = "nutrients.png", width = "30%", height = "30%", style="float:right")
                                        # ),
                                          p(class = "indented-paragraph",
                                          "In some species of aquatic insects, life cycles are divided between a larval stage in an aquatic environment and the emergence at the adult stage to the terrestrial environment", 
                                           tags$a(href ="https://onlinelibrary.wiley.com/doi/10.1111/geb.13700", 
                                           "(Nash et al., 2023)."),
                                           "Many species of aquatic insects serve as shredders, who are macroinvertebrates that play animportant role in the food system by turning coarse leaf litter from annual leaf fall into finer particulate organic matter",
                                           tags$a(href = "https://academic.oup.com/bioscience/article-lookup/doi/10.2307/1310804", 
                                           "(Cummins et al., 1989)."), 
                                           "Once shredders convert the leaf litter into a mixture of fecal matter and organic material, the macroinvertebrates known as collectors are able to consume the particulate matter through filtration. Other aquatic feeding groups include scrapers, who feed on algae, and predators, who feed on prey",
                                           tags$a(href = "https://academic.oup.com/bioscience/article-lookup/doi/10.2307/1310804", 
                                           "(Cummins et al., 1989)."),
                                           "When aquatic insects emerge from the water, they act as an important transporter of nutrients from aquatic to terrestrial ecosystems",
                                           tags$a(href = "http://link.springer.com/10.1007/s10021-016-0050-7", 
                                           "(Schindler & Smits, 2017).")),
                                          p(class = "indented-paragraph", 
                                          "Since the emergence of aquatic insects is an essential resource pulse for consumers such as other insects, birds, and mammals, changes in the emergence patterns of aquatic insects can threaten the availability of quality food sources in the ecosystem. Shifts in water temperatures, chemistry, and velocity can all impact the size, quantity, and time frame in which insects emerge",
                                            tags$a(href = "https://conbio.onlinelibrary.wiley.com/doi/10.1111/cobi.13477", 
                                            "(Baranov et al., 2020;"),
                                            tags$a(href = "https://pnas.org/doi/10.1073/pnas.2310513121",
                                            " Leathers et al., 2024;"),
                                            tags$a(href = "https://linkinghub.elsevier.com/retrieve/pii/S0960982222001191", 
                                            "Shipley et al., 2022)."),
                                            "These changes can result in a phenological mismatch between the emergence of aquatic insects and their terrestrial predators, leading to an overall decrease in the transfer of nutrients to the terrestrial ecosystem. One such nutrient is essential fatty acids that can only be found in aquatic insects, who accumulate the fatty acids from their algae rich diets",
                                                 #this is the start to a new paragraph 
                                            tags$a(href = "https://linkinghub.elsevier.com/retrieve/pii/S0960982222001191", 
                                            "(Shipley et al., 2022)."),
                                            "Since these fatty acids can be crucial for the development of some terrestrial predators, the misalignment between insect life cycles and terrestrial predators can lead to an overall decrease in survival fitness for some terrestrial species",
                                            tags$a(href = "https://linkinghub.elsevier.com/retrieve/pii/S0960982222001191", 
                                            "(Shipley et al., 2022).")
                                                 ),
                                  h3("Collecting Aquatic Insects in Hubbard Brook "),
                                          p(tags$figure
                                             (class = "centerFigure",
                                                tags$img(src = "Watershed_Location.png", 
                                                     width = "40%", 
                                                     height = "10%",
                                                     style="float:left"),
                                                   
                                                tags$figcaption("Figure 2: This map shows the location of watersheds in which sticky traps are set in.")
                                               )
                                            ), 
                                          p(class = "indented-paragraph", 
                                          "Located in the White Mountains of New Hampshire, the Hubbard Brook Experimental Forest has been the host site of continuous water chemistry monitoring since 1963 (Edwards, 2022). In 2018, researchers began collecting the sticky traps records of aquatic insects above eight different streams in the Hubbard Brook Experimental Forest. Five double sided sticky traps collected weekly were attached to a tree branch in a 20m long section next to a stream. These traps were set in watersheds labeled 1, 2 , 3, 4, 5, 6, 9, and Hubbard Brook (Edwards, 2022). Researchers at the site have carried out numerous treatments to the forest and its watersheds to study the impacts on the overall ecosystem. Watershed 1 was exposed to a calcium treatment in 1999 to counteract decreases in soil pH resulting from acid rain, (Likens, 2013). Watersheds 2, 3, and 4 were exposed to varying levels of deforestation in different time intervals ranging from 1965-1984. Watershed 3 serves as a hydrological control watershed and watershed 6 serves as a biogeochemical reference site (Likens, 2013). Watershed 9 has not been exposed to any experiments. Each trap site was located in a different watershed in the forest, and hence exposed to the unique spatial landscapes on each watershed. The data collected by the sticky traps as well as the continuous chemical and physical sampling in Hubbard Brook can help reveal how the distinct environmental traits of each site lead to changes in aquatic insect emergence."),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  h5("Please see the Works Cited tab for complete references.")
                                  ),
                                      

                                      #### Methods tab ####
                                      tabPanel("Methods", 
                                               h3("In field collection"),
                                               p(class = "indented-paragraph", "Insect collection began in April 2018 (Edwards, 2022). Insects 
                                               are collected weekly on double sided 4” x 7” sticky traps from watersheds labeled 1, 2, 3, 4, 5, 6, 9,
                                               and Hubbard Brook (Edwards, 2022). At each collection site, five sticky traps were spread across a 
                                               20m long section. The traps were attached to tree branches along a stream in each 
                                               watershed (Edwards, 2022). The sticky traps are set out after ice melts each year (around March or April)
                                               collection ends when traps are found to be sparsely filled (November or December). Once collected, 
                                               sticky traps were placed inside a plastic page protector and shipped to the Bernhardt Lab for identification."),
                                               h3("Insect Identification"),
                                               p(class = "indented-paragraph", "At the Bernhardt Lab, insects were identified using dissecting 
                                                 microscopes and a color coding system (Edwards, 2022). Insects were labeled by the categories: 
                                                 Terrestrial Diptera, Aquatic Diptera, Caddisflies, Mayflies, Stoneflies, or Other. Insects 
                                                 were also labeled for their size, with “Small” insects composing of bodies less than 5mm 
                                                 and “Large” insects composing of bodies greater than 5mm. Marks were made directly on the sheet
                                                 , with different colors representing different orders and different shapes (dash or circle) 
                                                 representing different sizes. Count of each order and size is tallied up and marked on the side
                                                 of the paper protector. Information about each trap was uploaded to the hbwater database."),
                                      p(tags$figure
                                        (
                                       # class = "centerFigure",
                                        tags$img
                                          (
                                          src = "annotated_trap.png", 
                                          width = "30%", 
                                          height = "30%",
                                            style="float:right"
                                        )
                                        ,
                                        tags$figcaption("Figure #: An example of a sticky trap after annoation. Dashes and circles denote size of insect. Color of annotation represents order. Count of insects by order and size on the right.")
                                        )
                                        ), 
                                      p(tags$figure 
                                        (
                                          class = "centerFigure",
                                          tags$img(
                                            src = "trap_location.png",
                                            width = "30%",
                                            height = "30%",
                                            style="float:left"
                                          ),
                                          tags$figcaption("Figure #: Example of a collection site. Yellow double-sided sticky traps are located in a 20m stretch next to a stream to capture insect emergence. Edwards, 2022"
                                                          )
                                        ))), #come back and find/fix caption 
                                      
                                      #page break
                                      br(),
                                      
                                      #### Plotting tab ####
                                      tabPanel("Plot",
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
                                                   #sliderInput("timeRange",
                                                              # "Select the time period:",
                                                               #min = as.Date("2018-01-01","%Y-%m-%d"), #start 112018
                                                              # max = as.Date("2023-12-31","%Y-%m-%d"), #end 123123
                                                               #value = c(as.Date("2018-01-01","%Y-%m-%d"), as.Date("2023-12-31","%Y-%m-%d")),
                                                               #sep = "",
                                                               #step = 1),
                                                   #chosing bug type
                                                   checkboxGroupInput("bugType", 
                                                               "Select Bug Type:",
                                                               choices = c("Stoneflies" = "stonefly_large", 
                                                                           "Caddisflies" = "caddisfly_large", 
                                                                           "Mayflies" = "mayfly_large", 
                                                                           "Dipteran" = "dipteran_large",
                                                                           "Other" = "other_large"),
                                                               selected = "stonefly_large")
                                                 
                                                 ),
                                                
                                               
                                               # prints plot generated in server
                                               mainPanel(
                                                 plotOutput("stoneflyPlot")      
                                      )
                                      )
                                      ), # closes plot tab - DO NOT DELETE
                                      
                                      #### ML Development tab ####
                                      tabPanel("Machine Learning"
                                               ),
                                      
                                      
                                      #### Works Cited tab ####
                                      tabPanel("Works Cited",
                                               h4("Works Cited"),
                                               div(
                                                 p(class = "bibliography", "Baranov, V., Jourdan, J., Pilotto, F., Wagner, R., & Haase, P. (2020). Complex and nonlinear climate‐driven changes in freshwater insect communities over 42 years. Conservation Biology, 34(5), 1241–1251. https://doi.org/10.1111/cobi.13477"),
                                                 p(class = "bibliography", "Cummins, K. W., Wilzbach, M. A., Gates, D. M., Perry, J. B., & Taliaferro, W. B. (1989). Shredders and Riparian Vegetation. BioScience, 39(1), 24–30. https://doi.org/10.2307/1310804"),
                                                 p(class = "bibliography", "Edwards, Tyler. 2022. (2022). Adult Insects and a Baby Record: Assessing Aquatic Insect Emergence at the Hubbard Brook Experimental Forest [Unpublished undergraduate thesis]. Duke University."),
                                                 p(class = "bibliography", "Leathers, K., Herbst, D., De Mendoza, G., Doerschlag, G., & Ruhi, A. (2024). Climate change is poised to alter mountain stream ecosystem processes via organismal phenological shifts. Proceedings of the National Academy of Sciences, 121(14), e2310513121. https://doi.org/10.1073/pnas.2310513121"),
                                                 p(class = "bibliography", "Likens, Gene E.. Biogeochemistry of a Forested Ecosystem. United States, Springer New York, 2013."),
                                                 p(class = "bibliography", "Nash, L. N., Zorzetti, L. W., Antiqueira, P. A. P., Carbone, C., Romero, G. Q., & Kratina, P. (2023). Latitudinal patterns of aquatic insect emergence driven by climate. Global Ecology and Biogeography, 32(8), 1323–1335. https://doi.org/10.1111/geb.13700"),
                                                 p(class = "bibliography", "Schindler, D. E., & Smits, A. P. (2017). Subsidies of Aquatic Resources in Terrestrial Ecosystems. Ecosystems, 20(1), 78–93. https://doi.org/10.1007/s10021-016-0050-7"),
                                                 p(class = "bibliography", "Shipley, J. R., Twining, C. W., Mathieu-Resuge, M., Parmar, T. P., Kainz, M., Martin-Creuzburg, D., Weber, C., Winkler, D. W., Graham, C. H., & Matthews, B. (2022). Climate change shifts the timing of nutritional flux from aquatic insects. Current Biology, 32(6), 1342-1349.e3. https://doi.org/10.1016/j.cub.2022.01.057")
                                               )
                                               
                                      ) # closes out final panel
                                      
                          ) # closes out set of tabs
                          
                ) # closes out main panel 
                
)

#### Server ####

# Define server logic required to generate figures
server <- function(input, output) {
  
  # Creates plot for inclusion on plotting tab.
  output$stoneflyPlot <- renderPlot({
    filtered_data <- sticky %>% 
    mutate(Date = ymd(date)) %>% 
      # create year and month columns first
      mutate(year = year(date),
             month = month(date)) %>%
      filter(Date >= input$timeRange[1] & Date <= input$timeRange[2])
    
    # set the list of chosen insects to the new variable "chosenBugs"
    chosenBugs <- input$bugType
    
    #bug input aggreg
    agg_data <- filtered_data %>%
      # first need to pivot data from wide format to long format
      # pivoting the dipteran_large through other_small columns
      # column names will appear in one column and values in another
      pivot_longer(cols = dipteran_large:other_small, 
                   names_to = "taxa", # names the new names column
                   values_to = "count") %>% # names the new values column
      # there will be lots of NAs, but that's alright, that just means
      # that none of those insects were counted for that trap
      # now we can filter by chosen insect inputs
      filter(taxa %in% chosenBugs) %>%
      # group by every month of each year
      group_by(year, month) %>%
      # and finally sum all of the chosen insect counts, omitting NAs
      summarise(total_bug = sum(count, na.rm = TRUE)) %>%
      ungroup()
    
    #plotting the graph 
    ggplot(agg_data, aes(x = year, y = total_bug)) +
      geom_point() +  
      facet_wrap(~ month, scales = "fixed") +  # Ensure the same scale for all y-axes
      labs(title = "Total Number of Stoneflies by Year and Month",
           x = "Year",
           y = "Total Number of Stoneflies") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
  })
}

#### Bind UI & Server ####

 

# Run the application 
shinyApp(ui = ui, server = server)

# End of script.



