# SHINY APP to evaluate single case designs, possibly with multilevel data analysis

# a shiny app consists of two main parts, 
# 1. UI part for the input-output interface -> THIS PART
# 2. SERVER part for the functionality
# note, parts of the UI are SERVER dependent, which is responsiveness that is implemented using Render functions

#+ TMP select working directory with menu to point to the data

# SHINY
require(shiny)


shinyUI(
    
	fluidPage(

		# TWITTER BOOTSTRAP THEMES
		theme = "spacelabmin.css",
		 # tags$style(type="text/css",
		 #            ".shiny-output-error { visibility: hidden; }",
		 #            ".shiny-output-error:before { visibility: hidden; }"),
		
		# MAIN TITLE
		titlePanel("Single Case Experimental Design"),
		
		# Heading Information in top ROW
		fluidRow(
			HTML("<p style=\"padding: 10px; \">
			Shiny App that provides an interface to R for exploring single case data of type AB.
			</p>"
			)
		),
		
		# DATA / MODEL / OUTPUT & RESULTS

		# ROW for TAB-PANEL
		fluidRow(

			# TAB PANEL to specify the type of output / results
			tabsetPanel(type = "tabs", 
			   
## START ##
			   
				# START tabset to give general information on the App (default)
				tabPanel("start",
				
					# FILEINPUT, function to upload a file
					# ! only accepts *.txt input
					# TMP !! check types & convertions !!
					column(5,
						br(),
						wellPanel(
							fileInput("file", "upload a datafile in text format", accept="txt"),
							HTML("<p align=\"justify\">
								The requirements for the data are the following:<br>
								The response is numeric/integer, required<br>
								The treatment identification is character/factor (quoted), required<br>
								The time (observation order) identification is numeric/integer, required<br>
								Case and study identifications is character/factor (quoted), not required<br>
							</p>")
						)
					),
					column(7,
						br(),
						HTML("<br><p style=\"padding: 5px;\">
								This Shiny App helps you to explore your single case analyses.<br><br>Note that you first need to import your data and that the data is required to adhere to a certain format.<br>Once included, the various key variables in your data should be identified, the response variable, the cases if there are more than one, the treatment and the time.<br>
								<br/>
								This tool is in full development and is not ready for use yet. We would appreciate comments and corrections. 
								<br/>
								<br/>
								<small><strong>
								Cools, W., Beretvas, S.N., Ferron, J., Moeyaert, M. & Van den Noortgate, W. (2015). <i>SCED data analysis [software] </i>. Retrieved from www.single-case.com
								</small></strong>
								<br/>
								<br/>
								<cite>
								Note that the tool is being build as part of research funded by the Institute of Education Sciences, U.S. Department of Education, Grant number R305D150007. The opinions expressed are those of the authors and do not represent views of the Institute or the U.S. Department of Education or the Research Foundation Flanders.
								</cite>
								<br/>
							</p><br>"
						)
					)
				), 
				
## DATA ##

				# < DATA > tabset for upload, subsetting, summarizing
				tabPanel("DATA", 

					# DATA, the left column to identify the variables, standardize and center
					column(5, br(),
						# info on data specification
						verbatimTextOutput('dataInfo'),                         
						br(),
						wellPanel(
							# Data specification
							# dropdown to select response variable
							uiOutput("response"),
							# checkbox to standardize the response (dividing by RMSE of casewise regression)
							uiOutput("standardized"),
							# dropdown to select case and study variables
							uiOutput("cases"),
							uiOutput("studies"),
							# dropdown to select treatment variable
							uiOutput("treatment"),
							# dropdown to select time/order variable
							uiOutput("time"),
							# checkbox to center time with zero as last Base observation of first Base phase
							# TMP !! is this usefull ? what if treatment is intermittent ? !!
							uiOutput("transtime"),
							hr()
						),
						# checkbox to request response * time | treatment plots (design plots)
						uiOutput("showDesignPlots"),
						# link to request download of the design plots
						uiOutput("showDownloadDesignPlots"),
						# reactive slider to specify height of the casewise design plots
						uiOutput("sliderCaseDesign"),
						# casewise design plots
						plotOutput("rCasePlots")
					),
					# DATA, the right column to show the data structure, subset, summary and table
					column(7, br(),
						verbatimTextOutput('dataTypes'),
						br(),
						h4("data structure"),
						# show the data in R format, uses R command str()
						verbatimTextOutput('dataStructure'),
						br(),
						h4("subsetting"),
						# ROW with three columns, for the selection checkboxes and the summary
						fluidRow(
							# dynamically list the cases and studies included in the identified variables
							# out of which a subset can be selected for further processing
							column(2, uiOutput("selectedStudies")),
							column(2, uiOutput("selectedCases")),
							column(8, 
								verbatimTextOutput("rCaseSummary")
							)
						),
						fluidRow(
							# checkbox to show data table or not 
							uiOutput("showDataTable"),
							# show download link to download data table 
							uiOutput("showDownloadDataTable"),
							# shiny table of the data
							dataTableOutput('rDataTable')
						)
					)
				),

## MODEL ##

				# < MODEL > tabset for model specification
				tabPanel("MODEL", 
					# all predictors in the fixed part
					column(3,
						br(),
						wellPanel(
							# checkboxes for the main effects, the interactions and second order effects
							uiOutput("fixedMain"),
							uiOutput("fixedOther")
						)
					),
					# all predictors in the random part
					# note, only up to three level models with cases within studies
					column(3,
						br(),
						wellPanel(
							# checkboxes for the variable with variance over cases / studies 
							uiOutput("varCases"),
							uiOutput("varStudies")
						)
					),
					column(6, 
						br(),
						# reactive information on the selected model (still implement)
						verbatimTextOutput('modelInfo'),
						h4("R model for case regressions"),
						# the regression model for the casewise regression in R format
						verbatimTextOutput('showCasesModel'),
						h4("R model for meta-analysis"),
						# the regression model for the meta analysis in R format
						verbatimTextOutput('showMetaModel')
					)
				),
				
## CASE Analysis ##

				# per case regressions
				# TMP !! include MODEL for the prediction
				tabPanel("Case Analysis",
					column(6, br(),
						# reactive info on the casewise regressions (still implement)
						verbatimTextOutput("casewiseRegressionInfo"),
						wellPanel(
							h4("regression output"),
							# download link for the table of regression results
							uiOutput("showDownloadCaseResults"),
							# table of case regression results in R format, using summary()
							tableOutput("regressCaseResults")							
						)
					),					
					column(6, br(),
						# reactive info on the prediction regressions (currently just the time indicator)
						verbatimTextOutput("casewisePredictionInfo"),
						# reactive slider to specify time range
						wellPanel(
							uiOutput("sliderPredictTime"),
							# h4("predictions at the given time"),
							# download link for the table of regression results
							uiOutput("showDownloadCasePredictions"),
							# table of predictions in R format using predict()
							tableOutput("predictionCaseResults")
						)
					)
				),

## META ANALYSIS ##

				# meta analysis using a linear mixed model (see RUN)
				tabPanel("Meta-Analysis",
					column(6, br(),
						verbatimTextOutput('metaInfo'),
						actionButton("r", "<< run meta-analysis as linear mixed model >>", width="100%"),
						wellPanel(
							h4("regression output"),
							# show case regression results in R format, using summary()
							verbatimTextOutput('metaResults')						
						)
					),
					column(6, br(),
						verbatimTextOutput('metaInterpret')
					)
				), 

## CASE PLOTS ##

				# case plots
				# note, heights can be specified
				# note, all plots are on the same axis
				# note, responsive to the selection of cases (studies)
				tabPanel("Selected Plots",

				   # top ROW with 2 columns, one for plots and one for the predictions
				   # !! predictions to be removed !!
				   # !! other type of plots to be included !!
#					fluidRow(
					   # slider for the height of the plots and the actual plots
					column(6,  br(),
						# checkbox to request response * time | treatment plots (design plots)
						uiOutput("showLmCasePlots"),
						# link to request download of the design plots
						uiOutput("showDownloadLmCasePlots"),
						# reactive slider to specify height of the casewise design plots
						uiOutput("sliderLmCasePlots"),
						# casewise design plots
						plotOutput("rLmCasePlots")
					),
					# slider 
					column(6, br() #,
						# show the selected time point in the slider
						# verbatimTextOutput("plotCaseInfo"),
						# sliderInput("sliderCasePlot", label = h4("set plot height"), min = 0, max = 100, value = 50),
						# plotOutput("casePlot")
					)
				)
			) # > tabsetPanel
		), # > fluidrow 
		# bottom
		fluidRow(
			HTML("<p style=\"padding: 20px; \">	</p>"),
			hr()
		)
	) # > fluidPage			
) # > shinyUI