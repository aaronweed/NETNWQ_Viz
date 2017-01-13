
# The user-interface (ui) script controls the layout and appearance of your app. 

library(shiny)
library(leaflet)
library(shinyjs)
library(DT)




shinyUI(navbarPage(title=HTML("<div> <a href='https://science.nature.nps.gov/im/units/netn/'> <img src='ah_small_black.gif',
          alt='WQ Visualizer'> </a> NETN Lakes, Ponds, and Streams Visualizer</div>"),position = "static-top", inverse=TRUE, collapsible = FALSE, fluid=TRUE, 
           windowTitle = "NETN Lakes, Ponds, and Streams Visualizer", id="MainNavBar",
           
######################################### Benthics Panel ####################################################################
           
tabPanel(title="Water quality  data",
         #style="padding: 0",
                    useShinyjs(),
         div(class="outer",
             #tags$head(includeCSS("./www/mapstyles.css") ), # defines css file
             tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
              #puts up icon on tab
            #, tags$head(includeScript("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))#,
         ),
         
fluidPage(
  sidebarPanel(
    h1("Plot summary stats from each sampled site."),
    br(),
    #Park selection
    tags$div(title="Choose the park you want to work with",selectInput(inputId='park', label='Select Park', choices= ParkList, selected = "ACAD")),
    
    # Site selection
    uiOutput("SiteResults"),
    
    # Parameter selection
    uiOutput("VarResults"),
    
    ##Add in options
    tags$div(title="Add a trend line ",selectInput(inputId='trend', label='Add trend line',choices=c("No", "Linear"), selected = "No")),
    tags$div(title="Histogram ",selectInput(inputId='AA', label='Plot histogram',choices=c("No", "Histogram"), selected = "No")),
    br(),
    p("Set plot histogram to No for time series plot"),
    br(),
    #downloadButton('downloadData', 'Download Data'),
    #img(src = "BMI_sampling.jpg", height = 140, width = 180),
    br(),
    br(),
    p("For further information about this sampling protocol, visit the ", 
    a("NETN protocol page.", href= "https://science.nature.nps.gov/im/units/netn/monitor/programs/lakesPonds/lakesPonds.cfm")),
    br()
    ),
    
    mainPanel(plotOutput("plot")
    
              
                        )
    #dygraphOutput("dygraph")
    
  )
  )#end navbarPage()
)
)