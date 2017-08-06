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
		theme="cosmo",
		includeCSS("mystyle.css"),

		# theme = "spacelabmin.css",
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
				id = "tabset",
			   
## START ##
			   
				# START tabset to give general information on the App (default)
				tabPanel(title = "start",
					value = "start",
				
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
						),
						checkboxInput("runExample", "testing", FALSE)
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
								<cite><small>
								Note that the tool is being build as part of research funded by the Institute of Education Sciences, U.S. Department of Education, Grant number R305D150007. The opinions expressed are those of the authors and do not represent views of the Institute or the U.S. Department of Education or the Research Foundation Flanders.
								</small></cite>
								<br/>
							</p><br>"
						)
					)
				), 
				
## DATA ##

				# < DATA > tabset for upload, subsetting, summarizing
				tabPanel(title = "DATA", 
					value = "data",

					# DATA, the left column to identify the variables, standardize and center
					column(5, br(),
						# info on data specification
						h4("data specification"),
						# verbatimTextOutput("data.txt.require"),                         
						br(),
						wellPanel(
							# Data specification
							# dropdown to select response variable
							uiOutput("data.select.response"),
							# checkbox to standardize the response (dividing by RMSE of casewise regression)
							uiOutput("data.check.standardized"),
							# dropdown to select case and study variables
							uiOutput("data.select.studies"),
							uiOutput("data.select.cases"),
							# checkbox to combine names study and case
							uiOutput("data.check.concatenate"),
							# dropdown to select treatment variable
							uiOutput("data.select.treatment"),
							# TMP !! is this usefull ? what if treatment is intermittent ? !!
							uiOutput("data.select.control"),
							# dropdown to select time/order variable
							uiOutput("data.select.time"),
							# checkbox to center time with zero as last Base observation of first Base phase
							# TMP !! is this usefull ? what if treatment is intermittent ? !!
							uiOutput("data.check.transtime"),
							hr()
						)
					),
					# DATA, the right column to show the data structure, subset, summary and table
					column(2, br(),
						h4("subsetting"),
						# ROW with three columns, for the selection checkboxes and the summary
						# fluidRow(
							# dynamically list the cases and studies included in the identified variables
							# out of which a subset can be selected for further processing
						column(6, checkboxInput('data.check.toggleStudies', 'All/None'), uiOutput("data.checks.selectedStudies")),
						column(6, checkboxInput('data.check.toggleCases', 'All/None'), uiOutput("data.checks.selectedCases"))
					),
					column(5, br(),
						h4("data structure"),
						# show the data in R format, uses R command str()
						verbatimTextOutput('data.txt.structure'),
						# column(6, 
						h4("warnings"), 
						wellPanel(
							HTML("<p>
							Variables automatically converted<br>Type selection error prone!!<br>Check data structure (left)<br>Note: factors for quoted text or numbers<br><br>Centering time is on first treatment observation<br>
							</p>")
						),
						fluidRow(
							# checkbox to show data table or not 
							uiOutput("data.check.table"),
							# show download link to download data table 
							uiOutput("data.link.downloadTable"),
							# shiny table of the data
							dataTableOutput('data.table.selected')
						)
					)
				),

## CASE Analysis, estimation ##

				# per case regressions
				# TMP !! include MODEL for the prediction
				tabPanel(title = "Data Description",
					value = "desc",
					column(1,br(),
						uiOutput("desc.checks.selectedCases")	
					),
					column(6, br(),
						# reactive info on the casewise regressions
						# verbatimTextOutput("desc.txt.formula"),
						wellPanel(
							h4("data description"),
							# download link for the table of regression results
							# uiOutput("desc.link.downloadDescription"),
							# table of case regression results in R format, using summary()
							dataTableOutput("desc.table.descriptives")
						)
					),					
					column(5, br(),
						# uiOutput("case.check.plots"),
						# reactive slider to specify time range
						wellPanel(
							# link to request download of the design plots
							uiOutput("desc.link.showDownloadCasePlots"),
							uiOutput("desc.plots"),
							
							# checkbox to request response * time | treatment plots (design plots)
							uiOutput("desc.check.showDesignPlots"),
							# link to request download of the design plots
							uiOutput("desc.link.showDownloadDesignPlots"),
							# reactive slider to specify height of the casewise design plots
							uiOutput("desc.slider.caseDesign"),
							# casewise design plots
							plotOutput("desc.plot.caseDesign")
						)
					)
				),


## MODEL ##

				# < MODEL > tabset for model specification
				tabPanel(title = "MODEL", 
					value = "model",
					# all predictors in the fixed part
					column(5, 
						br(),
						h4("model specification"),
						# reactive information on the selected model (still implement)
						wellPanel(
							HTML("<p>Specify the model. Note this is dependent on data specifications.<br>The case regressions are independent of the multilevel structure.</p>")
						),
						wellPanel(
							uiOutput('model.check.select'),
							verbatimTextOutput('model.txt.formula')
						)
					),
					column(7,
						fluidRow(
						br(),
						h4("R formulas"),
						# the regression model for the casewise regression and meta analysis in R format
						verbatimTextOutput('model.txt.Rformula'),
						h4("model refinement"),
							column(6,
								wellPanel(
									# checkboxes for the main effects, the interactions and second order effects
									uiOutput("model.check.simpleFixed"),
									uiOutput("model.check.extendedFixed")
								)
							),
							# all predictors in the random part
							# note, only up to three level models with cases within studies
							column(6,
								wellPanel(
									# checkboxes for the variable with variance over cases / studies 
									uiOutput("model.check.casesRandom"),
									uiOutput("model.check.studiesRandom")
								)
							)
						)
					)
				),
				
## CASE Analysis, estimation ##

				# per case regressions
				# TMP !! include MODEL for the prediction
				tabPanel(title = "Casewise Estimation",
					value = "ests",
					
					column(1,br(),
						uiOutput("caseEst.checks.selectedCases")	
					),
					column(6, br(),
						# reactive info on the casewise regressions
						verbatimTextOutput("caseEst.txt.formula"),
						wellPanel(
							h4("regression output"),
							# download link for the table of regression results
							uiOutput("caseEst.link.downloadEstimates"),
							# table of case regression results in R format, using summary()
							tableOutput("caseEst.table.estimates")							
						)
					),					
					column(5, br(),
						# uiOutput("case.check.plots"),
						# reactive slider to specify time range
						wellPanel(
							# checkbox to request response * time | treatment plots (design plots)
							uiOutput("caseEst.check.showCasePlots"),
							# link to request download of the design plots
							uiOutput("caseEst.link.showDownloadCasePlots"),
							uiOutput("caseEst.plots")
						)
					)
				),

## CASE Analysis, prediction ##

				# per case regressions
				# TMP !! include MODEL for the prediction
				tabPanel(title = "Casewise Prediction & Effect Sizes",
					value = "preds",
				
					column(1,br(),
						uiOutput("casePred.checks.selectedCases")	
					),
					# slider 
					column(6, br(),
						# reactive slider to specify time range
						wellPanel(
							fluidRow(
								column(4,
									uiOutput("casePred.select.alpha")
								),
								column(8,
								# reactive info on the prediction regressions (currently just the time indicator)
									verbatimTextOutput("casePred.txt.slider")						
								)
							),
							uiOutput("casePred.slider.timePredict"),
							# h4("predictions at the given time"),
							# download link for the table of regression results
							uiOutput("casePred.link.downloadEffectSizes"),
							# table of predictions in R format using predict()
							tableOutput("casePred.table.effectsizes")
						)
					),
					# slider 
					column(5, br(),
						wellPanel(
							fluidRow(
								column(3, numericInput("minTime", "Min", 0)),
								column(3, numericInput("maxTime", "Max", 10)),
								column(3, numericInput("stepSize", "Step Nr.", 100))
							),
							# checkbox to request response * time | treatment plots (design plots)
							uiOutput("casePred.check.showCasePlots"),
							# link to request download of the design plots
							uiOutput("casePred.link.showDownloadCasePlots"),
							uiOutput("casePred.plots")


						)
					)
				),

## META ANALYSIS ##

				# meta analysis using a linear mixed model (see RUN)
				tabPanel(title = "Meta-Analysis",
					value = "meta",

					column(6, br(),
						verbatimTextOutput('meta.txt.formula'),
						actionButton("runMeta", "<< run meta-analysis as linear mixed model >>", width="100%"),
						wellPanel(
							h4("regression output"),
							# show case regression results in R format, using summary()
							verbatimTextOutput('meta.txt.note')						
						)
					),
					column(6, br(),
						verbatimTextOutput('meta.txt.info')
					)
				), 

## CASE PLOTS ##

				# case plots
				# note, heights can be specified
				# note, all plots are on the same axis
				# note, responsive to the selection of cases (studies)
				tabPanel(title = "Test Page",
					value = "test",
				
					column(6,br(),
						tableOutput("test.table"),
						verbatimTextOutput('test.text')
					),
					column(6,  br(),
						uiOutput("test.check.show"),
						uiOutput("test.plot"),
						uiOutput("test.link.download"),
						uiOutput("test.slider"),
						plotOutput("test.plotset")
					)
				)
			) # > tabsetPanel
		) # > fluidrow 
	) # > fluidPage			
) # > shinyUI