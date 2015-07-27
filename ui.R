library(shiny)


shinyUI(
  
	fluidPage(
	  theme = "spacelabmin.css",
	 # tags$style(type="text/css",
	 #            ".shiny-output-error { visibility: hidden; }",
	 #            ".shiny-output-error:before { visibility: hidden; }"),
    titlePanel("Single Case Analyses"),
    fluidRow(
      HTML("<p style=\"padding: 20px; \">Explore your single case data. First import your data. Note that certain data requirements have to be met in order to guarentee a correct analysis. </p>")
    ),
	  fluidRow(
      column(2, 
        wellPanel(
          h4("set data"),
          hr(),
          fileInput("file", "", accept="txt"),
  		    uiOutput("response"),
  		    uiOutput("standardized"),
          uiOutput("cases"),
  		    uiOutput("studies"),
  		    uiOutput("treatment"),
  		    uiOutput("time"),
  		    uiOutput("transformed"),
  		    hr(),
          fluidRow(
  		      column(6, uiOutput("selectedStudies")),
  		      column(6, uiOutput("selectedCases"))
  		    )
        )
		  ),
		  column(3, 
        wellPanel(
          fluidRow(
            column(6, 
                   h4("set model"),
                   hr()
            ),
            column(6, 
                   actionButton("r", "<< RUN >>")
            )
          ),
          fluidRow(
            verbatimTextOutput('showmodel'),
            verbatimTextOutput('mymodelinfo')
          ),
          fluidRow(
            column(6, uiOutput("predictors")),
            column(6, 
              uiOutput("varyPerCase"),
              uiOutput("varyPerStudy")
            )
          ),
          hr()
        )
		  ),
      column(7, 
        fluidRow(
        wellPanel(
          tabsetPanel(type = "tabs", 
            tabPanel("Info", 
              HTML("<br><p style=\"padding: 20px;\">This Shiny App helps you to explore your single case analyses.<br><br>Note that you first need to import your data and that the data is required to adhere to a certain format.<br>Ones included, the various key variables in your data should be identified, the response variable, the cases if there are more than one, the treatment and the time.<br></p><br>")
            ), 
            tabPanel("Selected Data", 
                     fluidRow(
                       br(),
                       br()
                     ),
                     fluidRow(
                       column(1, br()),
                       column(10, 
                              h4("data structure"),
                              br(),
                              verbatimTextOutput('mydatastructure')
                       ),
                       column(1, br())
                     ),
                     fluidRow(
                       column(1, br()),
                       column(10, 
                              br(),
                              h4("data table"),
                              downloadLink('downloadData', 'Download Data'),
                              br(),
                              dataTableOutput('mydata')
                       ),
                       column(1, br())
                     )
            ),                
            tabPanel("Case Regression",
                     fluidRow(
                       column(1, br()),
                       column(10,
                              br(),
                              h4("regression output"),
                              verbatimTextOutput("caseresults")
                       ),
                       column(1, br())
                     ),
                     fluidRow(
                       column(1, br()),
                       column(10, verbatimTextOutput('myregressioninfo')),
                       column(1, br())
                     )
            ),         
            tabPanel("Case Plot",
                     fluidRow(
                       column(4,
                              br(),
                              sliderInput("casePlotSlider", label = h4("set height"), min = 0, max = 100, value = 50),
                              plotOutput("casePlot")
                       ),
                       column(8, 
                          fluidRow(
                            column(8, 
                                   br(),
                                   uiOutput("sliderEffectPlot")
                            ),
                            column(4, 
                                   br(),
                                   br(),
                                   br(),
                                   actionButton("pred", "<< predict >>")
                            )
                          ),
                          fluidRow(
                            column(4, 
                                   br(),
                                   plotOutput("effectPlot")
                            ),
                            column(8, 
                                   br(),
                                   verbatimTextOutput("predictioninfo"),
                                   verbatimTextOutput("predictions")
                            )
                          )
                       )
                     )
            ),
            tabPanel("Meta-Analysis",
                     fluidRow(
                       column(1, 
                              br(),
                              br()
                       ),
                       column(10,
                              br(),
                              h4("results"),
                              verbatimTextOutput('metaresults')
                       ),
                       column(1, 
                              br(),
                              br()
                       ) 
                     ),
                     fluidRow(
                       column(1, br()),
                       column(10, br()),
                       column(1, br())
                     )
            ), 
            tabPanel("Plot", plotOutput("scplot")),
            tabPanel("Testing Stuff", verbatimTextOutput("testing"))
          )
        )
		  ))
	  )
  )
)