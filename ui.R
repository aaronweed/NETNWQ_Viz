
# The user-interface (ui) script controls the layout and appearance of your app. 

library(shiny)
library(leaflet)
library(shinyjs)
library(DT)

ParkList<-c("ACAD" ,"MABI" ,"MIMA" ,"MORR", "ROVA" ,"SAGA" ,"SAIR" ,"SARA", "WEFA")

SiteList<-c("Aunt Betty Pond inlet/Gilmore Meadow Outlet" ,"Aunt Bettys","The Bowl","Bear Brook Pond","Breakneck Brook","Browns Brook","Bubble Pond","Eagle Lake inlet/ Bubble pond outlet","Cadillac Stream" ,"Duck Pond Brook-inlet to Long Pond","Duck Brook- North of Rt. 233","Eagle Lake",
 "Echo Lake","Duck bk- Outlet of Eagle Lake","Hadlock Brook (Upper Hadlock Pond)"  ,"Hunters Brook","Hodgdon Pond","Heath Brook"      ,                          
 "Jordan Pond","Jordan Stream- Jordan Pond outlet","Kebo Brook", "Lower Breakneck","Lower Hadlock","Lake Wood Outlet",
 "Long Pond (MDI)","Long Pond (IAH)","Lurvey Spring Brook (Echo Lake inlet)","Lurvey Brook ","Man o'War Brook","Marshall Brook",
"Otter Creek","Round Pond","Sargent Mtn Pond",
 "Seal Cove Pond","Seawall Pond","Sargent Brook",
"Stanley Brook","Upper Breakneck","Upper Hadlock","Witch Hole Pond","Lake Wood","The Pogue",
 "Pogue Brook (Stream A)","Mill Brook (Stream A)","Elm Brook (Stream B)",
"Concord River","East Primrose Brook (Stream A)","Primrose Brook Confluence (Stream B)"   ,    
"West Primrose Brook (Stream C)","Indian Grove Brook (Stream D)","Passaic River (Stream E)" ,                  
"Pond A","Lower FDR Brook (Stream A)","Upper FDR Brook (Stream B)",
 "Upper Crum Elbow Creek (Stream C)","Lower Crum Elbow Creek (Stream D)","Maritje Kill (Stream E)"   ,                 
 "Fall Kill (Stream F)","Blow-Me-Down Pond","Blow-Me-Up Brook (Stream A)","Blow-Me-Down Brook (Stream B)","Saugus River","Turning Basin" ,"Kroma Kill (Stream A)","American's Creek (Stream B)","Upper Mill Creek (Stream C)","Mill Creek Confluence (Stream D)","Weir Pond"   )

VarList<-c("Water Temperature" , "Specific conductance","DO_mg.L","DO (%)","pH","BP_mmHg","Discharge","TotalArea","TotalVel", "FPH","eqPH", "ACOLOR","TCOLOR",
        "COND","ANC","Ca_ueqL","Mg_ueqL","K_ueqL","Na_ueqL","Cl_ueqL"  , "SO4_ueqL","NO3_ueq/L","NO2+NO3_mg/L","NO2_mg/L","T_Dis_N_mgL" ,
        "NH4_mg/L","NH3_mg/L","TN_mg/L", "TP_ug/L","T_Dis_P_ugL","T_Dis_P_Flag","PO4_ug/L","CHLA_ugL" ,"DOC_mgL","DIC_mgL","Si_mgL",
        "Al_ugL","FPHMETH" , "CONDMETH","COLORMETH","ALKMETH")

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
    tags$div(title="Choose the site you want to work with",selectInput(inputId='x', label='Select Site', choices= SiteList, selected = "Pogue Brook (Stream A)")),
    #selectInput(inputId='y',label= 'Select site', choices=as.character(unique(df[,"Stream_Name"]))),
    tags$div(title="Choose the metric you want to plot ",selectInput(inputId='z', label='Select metric to plot', choices=VarList, selected = "Temp_C")),
    tags$div(title="Add a trend line ",selectInput(inputId='y', label='Add trend line',choices=c("No", "Linear"), selected = "No")),
    tags$div(title="Histogram ",selectInput(inputId='AA', label='Plot histogram',choices=c("No", "Histogram"), selected = "No")),
    br(),
    p("Set plot histogram to No for time series plot"),
    br(),
    #img(src = "BMI_sampling.jpg", height = 140, width = 180),
    br(),
    br(),
    #img(src = "mayfly.jpeg", height = 140, width = 180),
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