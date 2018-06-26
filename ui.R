library(shiny)
library(shinydashboard)
library(shinyjs)
library(colourpicker)

ui <- dashboardPage(
  dashboardHeader(title = "Syncronized HeatMaps",
                  titleWidth = 250,
                  tags$li(class = "dropdown",
                          tags$a(href="http://ueb.vhir.org", target="_blank",
                                 tags$img(height = "17px", alt="VHIR Campus logo", 
                                          src="vhir_campus.png"),
                                 tags$img(height = "17px", alt="UEB logo", 
                                          src="http://ueb.vhir.org/display41")
                          )
                  )
  ),
  skin = "purple", 
  dashboardSidebar(
    #Generem el menu lateral amb 'sidebarMenu'
    sidebarMenu(
      menuItem("Upload files", tabName = "upload", icon = icon("upload")),
      #menuItem("View transposed", tabName = "settings", icon = icon("share")),
      menuItem("HeatMaps", tabName = "hmplots", icon = icon("bar-chart"))
      #menuItem("Help", tabName = "help", icon = icon("question-circle"))
    ),
    #Inserim la imatge del logo del VHIR
    tags$a(href="http://vhir.org", target="_blank",
           tags$img(width = "230px", alt="VHIR", 
                    src="VHIR.jpg")
    ),
    #Inserim la imatge del logo de la UEB
    tags$a(href="http://ueb.vhir.org", target="_blank",
           tags$img(width = "230px", alt="UEB", 
                    src="UEBblanc2.jpg")
    ),
    #Inserim el mail de contacte
    div(align="center", h5(icon("envelope"),tags$a(href="mailto:ueb@vhir.org","ueb@vhir.org"))),
    #Inserim la imatge del logo del campus
    #tags$a(href="http://www.vallhebron.com", target="_blank",
    #       tags$img(width = "230px", alt="CampusVH", 
    #                src="logoVH.jpg")
    #),
    #Inserim la imatge del logo del GRBio
    tags$a(href="http://www.grbio.eu", target="_blank",
           tags$img(width = "230px", alt="GRBio", 
                    src="logoGRBio.png")
    )
  ),
  
  #########################################################################
  #Generem els panells de cada apartat del menu lateral amb 'dashboardBody'
  #########################################################################
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tabItems(
      
      #####################################################################
      tabItem(tabName = "upload",
      #####################################################################              
              #h3("Choose input files"), br(),
              
              fluidPage(splitLayout(
                fileInput('file1', 'Upload mixed data file',
                          accept = c('text/csv',
                                     'text/comma-separated-values',
                                     'text/tab-separated-values',
                                     'text/plain',
                                     '.csv',
                                     '.tsv')),
                radioButtons('sep1', 'Separator', c('Tab' ='\t','Comma (,)' =',', 'Semicolon (;)' =';'),"\t"),
                radioButtons('dec1', 'Decimal',   c('Point (.)'='.', 'Comma (,)'=','), '.'),
                #hr(),
                fileInput('file2', 'Upload expression file',
                          accept = c('text/csv',
                                     'text/comma-separated-values',
                                     'text/tab-separated-values',
                                     'text/plain',
                                     '.csv',
                                     '.tsv')),
                radioButtons('sep2', 'Separator', c('Tab' ='\t','Comma (,)' =',', 'Semicolon (;)' =';'),"\t"),
                radioButtons('dec2', 'Decimal',   c('Point (.)'='.', 'Comma (,)'=','), '.')
                )#endsplitlayout
              ),#endfluidpage
                
              fluidPage(splitLayout(
                        box(title = "First rows and columns of the data matrix containing mixed (categorical + numerical, heterogeneous) data", status = "primary", solidHeader = TRUE, 
                            tableOutput("table1"), height = "auto", width = "auto")
                        )#endsplitlayout
              ),#endfluidpage
              
              fluidPage(splitLayout(
                        box(title = "First rows and columns of the data matrix of numerical expression (homogeneous) data", status = "primary", solidHeader = TRUE, 
                            tableOutput("table2"), height = "auto", width = "auto")
                        )#endsplitlayout
              )#endfluidpage
      ),#endtabitem
      
      
      #####################################################################
      tabItem(tabName = "settings",
      #####################################################################
              h3("Look at the transposed data from both data sets"), br(),

              fluidPage(splitLayout(
                        box(title = "Transposed rows and (some) columns of the mixed (categorical + numerical, heterogeneous) data", status = "primary", solidHeader = TRUE, 
                        tableOutput("table3"), height = "auto", width = "auto")
                        )#endsplitlayout
              ),#endfluidpage
              
              fluidPage(splitLayout(box(title = "Transposed rows and (some) columns of the expression data", status = "primary", solidHeader = TRUE, 
                        tableOutput("table4"), height = "auto", width = "auto")
                        )#endsplitlayout
              )#endfluidpage
              
      ),#endtabitem
      
      
      #####################################################################
      tabItem(tabName="hmplots",
      #####################################################################
        #     box(title = "Simple HeatMap Plot", status = "primary", solidHeader = TRUE, width = 40, 
        #     plotOutput("plotS2")),
  
              fluidPage(
                  fluidRow(
                      column(width = 3,
                             colourInput("colMix1", label = "Choose colours for upper plot",  "lightgreen"),
                             colourInput("colMix2", label = NULL, "orange"),
                             radioButtons("selectSet", label = "Apply ordering by",
                                          c("Mixed (clinical) data" = "clin",
                                            "Expression (numeric) data" = "num")),
                             #uiOutput("num2sort"), ## dinamically created selectInput
                             uiOutput("var2sort"), ## dinamically created selectInput
                             radioButtons("orderSet", label = "Order variable",
                                          c("Ascending" = "asc",
                                            "Descending" = "desc"))
                      ),
                      column(width = 8,
                             plotOutput("plotMix")
                      )
                  ),
                  fluidRow(
                      column(width = 3,
                            # radioButtons("scale", label = "Scale expression data?", 
                            #              choices = c("None"="none", "Rows"="row","Columns"="column"), "row"),
                            #radioButtons("dendro", label = "Draw dendrogram on rows?", 
                            #              choices = c("Yes"="row", "No"="none"), "none"),
                            colourInput("colNum1", label = "Choose colours for lower plot",  "blue"),
                            colourInput("colNum2", label = NULL, "red"),
                            sliderInput("colorBreaks", label ="Select number of color breaks", 
                                        min = 2, max = 128, value = 16),
                            radioButtons("scale", label = "Scale/Bound expression data?",
                                         c("Autoscale" = "auto",
                                           "Keep unscaled" = "asis",
                                           "Unscaled with boundaries" = "bound")),
                            #checkboxInput("dendroR", label = "Draw dendogram on rows", value = TRUE, width = NULL)
                            #checkboxInput("dendroC", label = "Draw dendogram on columns", value = FALSE, width = NULL)
                            conditionalPanel(condition = "input.scale == 'bound'",
                                             h6("Set lower and upper boundaries"),
                                             column(width = 5,
                                                    numericInput("infVal", label = "Lower", 
                                                                 value = 0)
                                                    ),
                                             column(width = 5, 
                                                    numericInput("supVal", label = "Upper", 
                                                                 value = 10)
                                                    )
                                             )
                      ),
                      column(width = 8,
                             plotOutput("plotNum")
                             )
                  )
              ),
              fluidPage(splitLayout(
                        #textInput("title", "Enter here the title of the plot:"),
                        downloadButton("down", "Download as pdf")
                        )#endsplitlayout
              )#endfluidpage
      ),#endtabitem
      
      
      #####################################################################
      tabItem(tabName = "help",
      #####################################################################
              p("Heat map  is a graphical representation of data where the individual values contained in
                a matrix are represented as colors. Consists of a rectangular array of colored blocks, with 
                the color of each block representing the expression level of one gene on one array. Typically,
                in a heatmap, shades of red are used to represent degreees of increasing expression, and
                shades of green are used to represent degrees of decreasing expression. Each column of
                boxes represents a sample and each row of boxes corresponds to a gene. Besides, it is very 
                common to perform a hierarchical clustering of samples and/or genes and to sort the columns and/or
                rows according to the resulting dendogram to emphasize the presence of groups.
                This web tool performs a strong(heat map plot) of a dataset uploaded by the user.
                You need to perform this steps to have a nice heat map plot :) :"),
              tags$ol(
                tags$li(p(strong("Upload your file: ")," We need the data file to perform heat map with the
                               web tool:"),
                    tags$ul(tags$li(p(strong("Data file: "), "Load your datamatrix of interest in .csv or .txt 
                              format, knowing in advance how the values and decimal numbers are separated.
                              You can easily do it with any spreadsheet. Be sure you put the samples in columns
                              and the variables of study in rows. Use the first column to put the variables'
                              names or leave it in blank if you want  (since this column is not used). See ",
                              tags$a(href="dades.csv", target="_blank","dades.csv"), "to see and example of 
                              a file.")))), 
                tags$li(p(strong("Settings: "), "We need to configure some parameters before run the 
                                    application"),
                    tags$ul(tags$li(p(strong("Does the data need to be scaled?"), "Sometimes it is necessary
                                    to transform the data to put it in the same scale."))),
                    tags$ul(tags$li(p(strong("add some text to the title:"), "Here you have to put a word or
                                    few words to complete the sentence: Heat Map of"))),
                    tags$ul(tags$li(p(strong("Do you want to draw a dendogram?"), "You can decide if you want to 
                                    draw a dendogram by columns, row, both or none"))))
        
      )
    )#endtabitem
  )#endtabitems
)#enddashboardbody
)#enddashboardPage

