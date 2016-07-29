#################################################
#               Joint Space Map                 #
#################################################


library("shiny")
#library("foreign")

shinyUI(fluidPage(
  
  tags$head(includeScript("google_analytics.js")),
  # Header:
  titlePanel("Joint Space Map"),
  # Input in sidepanel:
  sidebarPanel(

    # Upload data:
    h5(p("Perceptual Data Input")),
    fileInput("file", "Upload Perceptual data (tab delimited txt file)"),
    # upload secon data
    h5(p("Preference Data Input")),
    fileInput("file1", "Upload Preference Data (tab delimited txt file)"),
    # Variable selection:
    h5(p("Data Selection (Optional)")),
    h6(p("A -  Perceptual (Attributes)")),
    htmlOutput("varselect"),
    
    h6(p("B -  Perceptual (Firms - only for Spider Chart)")),
    htmlOutput("varselect2"),
    
    h6(p("C -  Preference")),
    # upload secon data
    htmlOutput("varselect1"),
    br()
  ),
  # Main:
  mainPanel(
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",h4(p("How to Use this app")),
                         h4(p("Data input")),
                         p("To plot joint space map, this app needs two input from the user. In left-sidebar panel, click on Browse and upload 
the perceptual data. Note that your input data should be in tab delimited txt file. As soon as upload is complete, this app will read the data 
and all the variables in perceptual file will reflect in left-sidebar panel. Now click on second Browse link and upload the preference data in 
tab delimited txt format. As soon as upload is complete, respondents in prefernce data file will reflect in left-sidebar panel. Now you can 
navigate acrross different tab viz. 'PCA Variance Plot' tab, 'JSM plot' tab and 'Data' tab. Sample perceptual file and preference file is shwon 
below",align="justify"),
                
img(src = "perceptual.png", height = 180, width = 400),
img(src = "preference.png", height = 180, width = 400),

h4(p("Download Sample perceptual & prefernce file")), 
downloadButton('downloadData1', 'Download Perceptual file'), downloadButton('downloadData2', 'Download Prefernce file'),
p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
img(src = "example1.png"), #, height = 280, width = 400

                h4(p("Data Selection")),
p("This app gives user additional functionality to modiify the data. By default all the variables in perceptual file are selected for PCA but user 
can deselect/select variables in perceptual data in left-sidebar panel as per requirement. Similarly user can also deselect/select respondents from preference data
  in left-sidebar panel. As soon as user makes a change in data selection, accordingly all the tabs will be updated. ",align="justify")),
                
                tabPanel("PCA Variance Plot",plotOutput("plot1", width = "100%")),
                tabPanel("JSM Plot",plotOutput("plot", height = 800, width = 840)),
                tabPanel("Spider Chart",plotOutput("spiderplot", height = 800, width = 840)),
                tabPanel("Data",h5(p("Perceptual Data")),tableOutput("table"),h5(p("Preference Data")),tableOutput("table1"))
#     tableOutput("table"),  
#     tableOutput("table1"),
#     plotOutput("plot", width = "100%"),
#     plotOutput("plot1")
  )
))
)