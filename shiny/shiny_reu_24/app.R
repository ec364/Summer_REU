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

# Load all necessary data.
df <- trees # using a built-in R dataset of cherry trees

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
                                               #this is tutorial code: p(class = "indented-paragraph", "attempt", tags$a(href = "https://www.youtube.com", "link"), "one"),
                                               p(class = "indented-paragraph","In some species of aquatic insects, 
                 life cycles are divided between a larval stage in an aquatic environment 
                 and the emergence at the adult stage to the terrestrial environment", 
                                                 tags$a(href ="https://onlinelibrary.wiley.com/doi/10.1111/geb.13700", "(Nash et al., 2023)."),"Many species of aquatic insects serve as shredders, who are macroinvertebrates that play an
                 important role in the food system by turning coarse leaf litter from annual leaf fall into finer
                 particulate organic matter",
                                                 tags$a(href = "https://academic.oup.com/bioscience/article-lookup/doi/10.2307/1310804", "(Cummins et al., 1989)."), 
                                                 "Once shredders convert the leaf litter into a
                 mixture of fecal matter and organic material, the macroinvertebrates known as collectors are able
                 to consume the particulate matter through filtration. Other aquatic feeding groups include scrapers,
                 who feed on algae, and predators, who feed on prey",
                                                 tags$a(href = "https://academic.oup.com/bioscience/article-lookup/doi/10.2307/1310804", "(Cummins et al., 1989)."),
                                                 "When aquatic insects emerge 
                 from the water, they act as an important transporter of nutrients from aquatic to terrestrial ecosystems",
                                                 tags$a(href = "http://link.springer.com/10.1007/s10021-016-0050-7", "(Schindler & Smits, 2017).")),
                                               p(class = "indented-paragraph", "Since the emergence of aquatic insects is an essential resource pulse for consumers such 
                 as other insects, birds, and mammals, changes in the emergence patterns of aquatic insects can threaten the availability of 
                 quality food sources in the ecosystem. Shifts in water temperatures, chemistry, and velocity can all impact the size, quantity, 
                 and time frame in which insects emerge",
                                                 tags$a(href = "https://conbio.onlinelibrary.wiley.com/doi/10.1111/cobi.13477", "(Baranov et al., 2020;"),
                                                 tags$a(href = "https://pnas.org/doi/10.1073/pnas.2310513121"," Leathers et al., 2024;"),
                                                 tags$a(href = "https://linkinghub.elsevier.com/retrieve/pii/S0960982222001191", "Shipley et al., 2022)."),
                                                 "These changes can 
                 result in a phenological mismatch between the emergence of aquatic insects and their terrestrial predators, leading to an overall 
                 decrease in the transfer of nutrients to the terrestrial ecosystem. One such nutrient is essential fatty acids that can only be 
                 found in aquatic insects, who accumulate the fatty acids from their algae rich diets",
                                                 tags$a(href = "https://linkinghub.elsevier.com/retrieve/pii/S0960982222001191", "(Shipley et al., 2022)."),
                                                 "Since these fatty 
                 acids can be crucial for the development of some terrestrial predators, the misalignment between insect life cycles and 
                 terrestrial predators can lead to an overall decrease in survival fitness for some terrestrial species",
                                                 tags$a(href = "https://linkinghub.elsevier.com/retrieve/pii/S0960982222001191", "(Shipley et al., 2022).")),
                                               h3("Collecting Aquatic Insects in Hubbard Brook "),
                                               p(class = "indented-paragraph", "Located in the White Mountains of New Hampshire, the Hubbard Brook Experimental Forest has been the host site of
                 continuous water chemistry monitoring since 1963 (Edwards, 2022). In 2018, researchers began collecting the sticky traps records of aquatic insects
                 above eight different streams in the Hubbard Brook Experimental Forest. Five double sided sticky traps collected weekly were attached to a tree branch
                 in a 20m long section next to a stream. The sticky traps are set out after ice melts each year (around March or April) and collection ends when traps
                 are found to be sparsely filled (November or December). These traps were set in watersheds labeled 1, 2 , 3, 4, 5, 6, 9, and Hubbard Brook (Edwards, 2022).
                 Researchers at the site have carried out numerous treatments to the forest and its watersheds to study the impacts on the overall ecosystem. Watershed 1
                 was exposed to a calcium treatment in 1999 to counteract decreases in soil pH resulting from acid rain,
                 (Likens, 2013). Watersheds 2, 3, and 4 were
                 exposed to varying levels of deforestation in different time intervals ranging from 1965-1984. Watershed 3 serves as a hydrological control watershed
                 and watershed 6 serves as a biogeochemical reference site (Likens, 2013). Watershed 9 has not been exposed to any experiments. Each trap site was
                 located in a different watershed in the forest, and hence exposed to the unique spatial landscapes on each watershed. The data collected by the sticky
                 traps as well as the continuous chemical and physical sampling in Hubbard Brook can help reveal how the distinct environmental traits of each site lead
                 to changes in aquatic insect emergence."),
                                               p(tags$img(src = "Watershed_Location.png", width = "95%", height = "95%")
                                      )),
                                      
                                      
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
                                      tabPanel("Machine Learning"),
                                      
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
                                      )
                                      
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



