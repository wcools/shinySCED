# SHINY APP to evaluate single case designs, possibly with multilevel data analysis
# a shiny app consists of two main parts, 
# 1. UI part for the input-output interface
# 2. SERVER part for the functionality -> THIS PART
# note, parts of the UI are SERVER dependent, which is responsiveness that is implemented using Render functions


# install required packages not yet installed
# !! check if can be included only in ui or server !!
# !! check which ones are required !!
pckgs <- c('shiny','reshape2','ggplot2','plyr','arm')
todo <- pckgs[!is.element(pckgs, names(installed.packages()[,"Package"]))]
if(length(todo) > 0) install.packages(todo)  #,repos="http://cran.us.r-project.org")
# load in the packages
library(shiny)
library(ggplot2)  # ggplot2 -> plotting
library(plyr)     # ddply -> data manipulation
library(arm)      # lmer -> multilevel modeling
library(reshape2)

# server file
shinyServer(function(input,output){

## START ##

	# >> UPLOAD DATA and READ VARIABLE NAMES
	# when data selected, read tab delimited text file
	uploadData <- reactive({
		#+ TMP pre-specify a dataframe
		# ..dta <- read.table(file="wdta.txt",sep="\t",header=T)

		if(is.null(input$file)){		NULL		}
		else{
		   ..dta <- read.table(file=input$file$datapath,sep="\t",header=T)
		}
	})  
	# when data selected, get variable names from data
	getVarnames <- reactive({
		validate(	    need(input$file, '')	  )
		names(uploadData())
	})
  	
## DATA ##
	
	# information on the variable identification
	output$dataInfo <- renderText({
		# out <- "Assign a variable to the response and treatment and if relevant for the case, study and time"
		..info <- ""
		validate(
			need(input$file,"!!! return to START to upload data first !!!"),
			need(input$response, "numeric response variable: required"),	
			need(input$treatment, "treatment variable (character/factor): required"),
			need(input$cases,"cases variables: only if multiple exist (subsets can be made)"),				
			need(input$studies,"studies variable: only if multiple exist (subsets can be made)"),			
			need(input$time,"numeric with values 1upwards: required, not necessarilly part of the model")						
		)
		if(is.null(input$file)) ..info <- paste(..info,"!!! first UPLOAD your data at the start tab\n")
		else(
			if(input$response=="---" || input$treatment=="---" || input$cases=="---"  || input$studies=="---"  || input$time=="---"){
				# tmp <- c(input$response,input$treatment,input$cases,input$studies,input$time)
				# if(!any(table(tmp[tmp!="---"]))>1) ..info <- paste(..info,"\n","all variables can be assigned only once\n")	
				if(input$response=="---") ..info <- paste0(..info,"numeric response variable: required\n")	
				if(input$treatment=="---") ..info <- paste0(..info,"treatment variable (character/factor): required\n")
				if(input$cases=="---") ..info <- paste0(..info,"cases variables: only if multiple exist (subsets can be made)\n")
				if(input$studies=="---") ..info <- paste0(..info,"studies variable: only if multiple exist (subsets can be made)\n")
				if(input$time=="---") ..info <- paste0(..info,"numeric with values 1upwards: required, not necessarilly part of the model\n")
			}
			else(
				..info <- paste0(..info,"subsets of cases/studies can be made below using the checkboxes.\nstandardization -> response/RMSE(casewiseregression)\ncentering -> set time 1 as first treatment phase\n")
			) 
		)
		if(any(table(c(input$response,input$treatment,input$cases,input$studies,input$time))[names(table(c(input$response,input$treatment,input$cases,input$studies,input$time)))!="---"]>1)) ..info <- paste0(..info,"variables can not be assigned double\n")
		..info <- paste0(..info,"\n")
		..info
	})

	# >> IDENTIFY VARIABLES & CASES: Response, Case level, Study level, treatment indicator and time specification
  
	# identify variables
	
	# to select the response
	output$response<-renderUI({
	# TMP !! extend the validates here ? !!
		validate(      need(input$file, 'what does validate for response do?')	  )
		# TMP selectInput("response", "Response:", choices=c("---",setdiff(getVarnames(), "---")))
		selectInput("response", "Response (numeric/integer):", choices=c("---",setdiff(getVarnames(), "---")), selected="y")
	})
	# to select the case
	output$cases<-renderUI({
		validate(      need(input$file, '')	  )
		# TMP selectInput("cases", "Cases (factor):", choices=c("---",setdiff(getVarnames(), "---")))
		selectInput("cases", "Cases (character/factor):", choices=c("---",setdiff(getVarnames(), "---")), selected="case")
	})
	# to select the study
	output$studies<-renderUI({
		validate(      need(input$file, '')	  )
		# TMP selectInput("studies", "Studies (factor):", choices=c("---",setdiff(getVarnames(), "---")))
		selectInput("studies", "Studies (character/factor):", choices=c("---",setdiff(getVarnames(), "---")), selected="study")
	})
	# to select the treatment
	output$treatment<-renderUI({
		validate(      need(input$file, '')	  )
		# TMP selectInput("treatment", "Treatment (factor):", choices=c("---",setdiff(getVarnames(), "---")))
		selectInput("treatment", "Treatment (character/factor):", choices=c("---",setdiff(getVarnames(), "---")), selected="wthn")
	})
	# to select the time
	output$time<-renderUI({
		validate(      need(input$file, '')	  )
		# TMP selectInput("time", "Time (numeric):", choices=c("---",setdiff(getVarnames(), "---")))
		selectInput("time", "Time/Order (numeric/integer):", choices=c("---",setdiff(getVarnames(), "---")), selected="t")
	})

	# transformations

	# to choose to standardize or not
	output$standardized <- renderUI({
		validate(     
			# check whether dataset is uploaded / response variable assigned
			need(input$file, ''),
			need(input$response != "---", '')    
		)
		# show checkbox, with default NOT STANDARDIZED
		checkboxInput("standardize", "Standardized (response/RMSE)", FALSE)
	})
  
	# to choose to center time or not
	output$transtime <- renderUI({
		validate(   
			# check whether dataset is uploaded / response variable assigned
			need(input$file, ''),
			need(input$time != "---", '')    
		)
		# show checkbox, with default NOT CENTERED
		checkboxInput("transtime", "Center Time (0 for last observation of first phase)", FALSE)
	})
	
	# identify cases

	# to choose the cases to include
	output$selectedCases <- renderUI({
		validate(   
			# check whether dataset is uploaded / response variable assigned
			need(input$file, ''),
			need(input$cases != "---", '')    
		)
		# use selected data to populate the cases to select
		..dta <- uploadData()		
		# show checkbox, with default all cases selected
		checkboxGroupInput("selectCases", "Cases:", choices=as.character(unique(..dta[,input$cases])), selected=as.character(unique(..dta[,input$cases]))) 
	})
  
	# to choose the studies to include
	output$selectedStudies <- renderUI({
		# check whether dataset is uploaded / response variable assigned
		validate(      
			need(input$file, ''),
			need(input$studies != "---", '')    
		)
		# use selected data to populate the studies to select
		..dta <- uploadData()
		# show checkbox, with default all studies selected 
		checkboxGroupInput("selectStudies", "Studies:", choices=as.character(unique(..dta[,input$studies])), selected=as.character(unique(..dta[,input$studies])))
	})
  
	# TMP !! maybe consider case and study selection to be responsive to one another !!
	
	# end idenfity variables and cases
	
	# RETRIEVE USER SELECTED DATA 
	
	# use subsetted data (based on selected cases / studies)
	getData <- reactive({
	
		# check whether dataset is uploaded
		validate(      
			need(input$file, 'an uploaded file is required'),
			need(input$cases, 'a case identifyer is required'),
			need(input$studies, 'a study identifyer is required'),
			need(input$treatment, 'a treatment identifyer is required'),
			need(input$time, 'a time identifyer is required')
		)
		# use imported data 
		..dta <- uploadData()
		# return a treatment factor (used in predictions), numeric response and time
		..dta[,input$treatment] <- as.factor(..dta[,input$treatment])
		..dta[,input$response] <- as.numeric(..dta[,input$response])
		..dta[,input$time] <- as.numeric(..dta[,input$time])
		# subset the selected cases if any
		if(input$cases != "---"){
			case <- input$cases
			..dta <- subset(..dta,case%in%input$selectCases)
			..dta[,input$cases] <- as.factor(..dta[,input$cases])
		} 
		# subset the selected studies if any
		if(input$studies != "---"){
			study <- input$studies
			..dta <- subset(..dta,study%in%input$selectStudies)
			..dta[,input$studies] <- as.factor(..dta[,input$studies])
		} 
		if(input$cases == "---" & input$studies == "---"){
			..dta <- ..dta[order(..dta[,input$time]),]
		}
		else if(input$cases == "---" & input$studies != "---"){
			..dta <- ..dta[order(..dta[,input$studies],..dta[,input$time]),]
		}
		else if(input$cases != "---" & input$studies == "---"){
			..dta <- ..dta[order(..dta[,input$cases],..dta[,input$time]),]
		}
		else ..dta <- ..dta[order(..dta[,input$studies],..dta[,input$cases],..dta[,input$time]),] 
		
		droplevels(..dta)
	})
	# standardize the data using the casewise regression output
	getProcessedData <- reactive({
		# check whether dataset is uploaded
		validate(      
			need(input$transtime%in%c(TRUE,FALSE), 'centering checkbox available'),
			need(input$standardize%in%c(TRUE,FALSE), 'standardize checkbox available')
		)
		# TMP !! remove stuff here !!
		# if(is.null(input$file)){    modelinfo <- "first load & define the data"		}
		# else{
		..dta <- getData()
		# run transformation if requested and when time/treatment are specified
		# for each case, order the observations in time and subtract the number of treatment == 0 - 1
		if(input$transtime){
		# TMP !! remove this !!
			# ..dta[,input$time] <- as.numeric(..dta[,input$time])
			# ..dta <- ..dta[order(..dta[,input$time]),]
			..dta <- ddply(..dta,"case",.fun = function(.x,mytreat) mutate(.x, ..csm=c(0,cumsum(.x[,mytreat][-length(.x[,mytreat])]!=.x[,mytreat][-1]))), mytreat=input$treatment)
			..dta <- ddply(..dta,"case",.fun = function(.x,mytime,mytreat) mutate(.x[order(.x[,mytime]),], ..time=.x[,mytime] - .x[max(which(.x[,"..csm"]==levels(.x[,mytreat])[1])),mytime]), mytime=input$time,mytreat=input$treatment)
			# ..dta <- ddply(..dta,"case",.fun = function(.x,mytime,mytreat) mutate(.x[order(.x[,mytime]),], ..time=.x[,mytime]-1-sum(.x[,mytreat]==unique(.x[,mytreat])[1])), mytime=input$time,mytreat=input$treatment)
			..dta[,input$time] <- ..dta$..time
			..dta$..time <- NULL
			..dta$..csm <- NULL
		}
		# if a response is selected, that should be standardized
		if(input$standardize){
			# perform regression to get the standard error
			..tmp <- do.call(rbind,lapply(getCaseRegressions(),function(.x) summary(.x)[["sigma"]]))
			# divide the responses for each case by the standard error for that case's regression [RMSE]
			..dta[,input$response] <- ..dta[,input$response] / ..tmp[match(..dta[,input$cases],row.names(..tmp))]
		}
		..dta    
		# }
	})
	# perform case regressions (used in standardize and getProcessedData)
	getCaseRegressions <- reactive({
		validate(      
			need(input$file, ''),
			need(input$cases != '---', 'case regressions require case identification')
		)
		..dta <- getData()    
		..lm <- vector("list",length=length(unique(..dta[,input$cases])))
		..cntr <- 0
		for(it in unique(..dta[,input$cases])){
			..cntr <- ..cntr + 1
			..tmp <- ..dta[..dta[,input$cases]==it,]
			..lm[[..cntr]] <- lm(as.formula(getCasesModel()),data=..tmp)
		}
		names(..lm) <- unique(..dta[,input$cases])
		..lm  
	})

	# end Retrieve User Selected Data
	
	
	# suggestion to verify the data types
	output$dataTypes <- renderPrint({
		validate( need(input$file,'first upload a datafile !!'))
		cat("Variables are automatically converted to the appropriate type. \nBeware that invalid convertions can result in errors. \nBest check the data types and examplary values in the datastructure given below. \nNote that factors automatically result from using text or quoted numbers in your data file.")
	})
	# str() function on the subsetted data
	output$dataStructure <- renderPrint({
		# show structure of data, responsive to input identification (cases / studies)
		str(getProcessedData())
	})
	# case specific summary table (observations in value and frequency, nr missing)
	output$rCaseSummary <- renderPrint({
	   validate(      
	      need(input$cases!="---", 'multiple cases must be present to get summary statistics'),
	      need(input$treatment!="---", 'treatment should be identified with 0 for base and 1 for treatment')
	   )
	   ..tmp <- melt(getProcessedData(),id.vars=c(input$treatment,input$cases),measure.vars=input$response)
	   ..rng <- aggregate(list(range=..tmp[,"value"]),list(case=..tmp[,input$cases]),range,na.rm=T)
	   ..tbl <- aggregate(list(freq=..tmp[,input$treatment]),list(case=..tmp[,input$cases]),table)
	   ..mss <- aggregate(list(miss=..tmp[,"value"]),list(case=..tmp[,input$cases]),function(.x) sum(is.na(.x)))
	   ..tmp <- merge(..rng,..tbl,by=input$cases)
	   ..tmp <- cbind(..tmp$case,as.data.frame(..tmp$range),as.data.frame(..tmp$freq))
	   names(..tmp)[1:3] <- c("case","min","max")
	   ..tmp <- merge(..tmp,..mss,by="case")
	   ..tmp
	})
	
	# design plots: response over time per treatment for all selected cases
	
	# show design plots
	output$showDesignPlots <- renderUI({
		checkboxInput("showDesignPlots","show design plots (downloadable)", FALSE)
	})	
	# show the download link for design plots
	output$showDownloadDesignPlots <- renderUI({
		validate(need(input$showDesignPlots,"download link for design plots shown only on request"))
		if(input$showDesignPlots) downloadLink('downloadDesignPlots', 'Download Design Plots in Png Format')
	})
	# download the selected design plots (used in showDownloadDesignPlots)
	output$downloadDesignPlots <- downloadHandler(
		filename = function() { 
			paste('designPlot_', format(Sys.time(), "%d-%b_%Hh%M"), '.png', sep='')
		},
		content = function(file) {
			device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
			ggsave(file, plot = getPlotDesign(), device = device)
		}
	)
	# show slider to specify the plot height (used in getHeightFromSiderDesign)
	output$sliderCaseDesign <- renderUI({
		validate(need(input$showDesignPlots,"design plots shown only on request"))
		if(input$showDesignPlots) sliderInput("sliderCaseDesign", label = h4("set height"), min = 0, max = 100, value = 50)
	})
	# get height from slider for the design plots (used in rCasePlots)
	getHeightFromSliderDesign <- function() {
		validate(need(input$sliderCaseDesign,"note: generating many plots can be time consuming"))
		input$sliderCaseDesign * 100
	}
	# render the design plots 
	output$rCasePlots <- renderPlot(height=getHeightFromSliderDesign,{
		if(input$showDesignPlots) getPlotDesign()
	})
	# create the design plots (used in downloadDesignPlots / rCasePlots)
	getPlotDesign <- function(){
		..dta <- getProcessedData()
		..dta <- ddply(..dta,"case",.fun = function(.x,mytreat) mutate(.x, ..csm=c(0,cumsum(.x[,mytreat][-length(.x[,mytreat])]!=.x[,mytreat][-1]))), mytreat=input$treatment)
		..plots <- ggplot(..dta,aes(x=..dta[,input$time],y=..dta[,input$response])) 
		..plots <- ..plots + geom_line(aes(group = as.factor(..csm),color=..dta[,input$treatment]),size=1.3)
		..plots <- ..plots + geom_point()
		if(input$cases!="---" & length(unique(..dta[,input$cases]))>1) ..plots <- ..plots + facet_grid(reformulate(".",input$cases)) 
		..plots <- ..plots + scale_x_continuous("time",limits=c(min(..dta[,input$time]), max(..dta[,input$time]))) + scale_y_continuous("response")
		..plots <- ..plots + guides(linetype=FALSE) 
		..plots <- ..plots + theme(legend.position="none")
		..plots <- ..plots + ggtitle("casewise responses over time (base and treatments)")
		..plots
	}

	# shiny data table

	# show shiny datatable
	output$showDataTable <- renderUI({
		checkboxInput("showDataTable","show shiny datatable (downloadable)", FALSE)
	})
	# show the download link for data table
	output$showDownloadDataTable <- renderUI({
		validate(need(input$showDataTable,"download link for table shown only on request"))
		if(input$showDataTable) downloadLink('downloadDataTable', 'Download Table in Tab Delimited Text File Format')
	})
	# download the selected data (used in showDownloadDataTable)
	output$downloadDataTable <- downloadHandler(
		filename = function() {
			paste('data_', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
		},
		content = function(file) {
			write.table(getProcessedData(), file, sep="\t", row.names=F)
		}
	)	
	# get data for shiny table
	output$rDataTable <- renderDataTable({
		validate(need(input$showDataTable,"table shown only on request\n"))
		if(input$showDataTable) getProcessedData()
	})

## MODEL ##

	# >> SELECT variables using checkboxes for the fixed variables, and the random variables within either case / study
  
	# information on the selected model
	output$modelInfo <- renderText({
		# TMP !! user input dependent information can be added if interesting ? !!
		# show structure of data, responsive to input identification (cases / studies)
		"Specify the model. Note this is dependent on data specifications. \nThe case regressions are independent of the multilevel structure." 
	})
	# render fixed variable names
	output$fixedMain <-renderUI({
		# if file is uploaded and response is selected
		validate( 
			need(input$file, ''),
			need(input$response != "---", '')    
		)
	  	# show checkbox with all variables, their interactions and 2nd order polynomial with default unchecked except for the main ones
		checkboxGroupInput("vars", "Fixed:", choices=as.list(c(getPredictors())), selected=getSelection())
	})
	# render fixed variable names
	output$fixedOther<-renderUI({
		# if file is uploaded and response is selected
		validate( 
			need(input$file, ''),
			need(input$response != "---", '')    
		)
		# if a treatment is selected
		checkboxGroupInput("varx", "", choices=as.list(c(getInteractions(),getPower2())))
	})
	# random variable, per case
	output$varCases <- renderUI({
		# if file is uploaded and cases are selected
		validate(      
			need(input$file, ''),
			need(input$cases != "---", ''),
			need(input$response != "---", '')
		)
		# retrieve all predictors with the intercept to select from
		selected <- c("intercept",getFixed())
		# show checkbox with all random variables over cases, with only the intercept checked by default 
		checkboxGroupInput("varCases", "case (random):", choices=selected, selected="intercept")
	})  
	# random variables, per study
	output$varStudies <- renderUI({
		# if file is uploaded and cases are selected
		validate(      
			need(input$file, ''),
			need(input$studies != "---", '')    
		)
		# retrieve all predictors with the intercept to select from
		selected <- c("intercept",getFixed())
		# show checkbox with all random variables over studies, with only the intercept checked by default 
		checkboxGroupInput("varStudies", "study (random):", choices=selected, selected="intercept") 
	})	
	# show the case specific model in R format
	output$showCasesModel <- renderPrint({
		cat(getCasesModel())  
	})
	# show the multilevel model in R format
	output$showMetaModel <- renderPrint({
		cat(getMetaModel())  
	})

	# >> determine potential FIXED VARiABLE NAMES, INTERACTIONS & POWER_2
  
	# retrieve all main effects in the model (first order effects) (used in getInteractions and getPower2)
	getMain <- reactive({
		validate(      need(input$file, '')	  )
		# main effects selected as part of the model
		input$vars    
	})  
	# retrieve all fixed effects in the model (including interactions and second order) (used in varCases and varStudies)
	getFixed <- reactive({
		validate(      need(input$file, '')	  )
		# variables selected as part of the model
		c(input$vars,input$varx)    
	}) 
	# retrieve treatment and time if specified (used for default selection in MODEL) (used in fixedMain)
	getSelection <- reactive({
		validate(      need(input$file, '')	  )
		# variables selected as part of the model
		tmp <- setdiff(c(input$treatment,input$time),"---")
		if(length(tmp)>0) tmp
	}) 
		
	# get potential predictors, (used for default selection in MODEL) (used in fixedMain)
	# TMP !! can case/study be a predictor ? !!
	getPredictors <- reactive({
		# setdiff(getVarnames(),c(input$response,input$cases,input$studies))
		setdiff(getVarnames(),c(input$response))
	})
	# construct predictor interactions (used for selection in MODEL) (used in fixedOther)
	getInteractions <- reactive({
		if(length(getMain())>1) apply(combn(getMain(),2),2,function(.x) paste(.x,collapse=":"))
	})
	# construct predictor to the power 2 (used for selection in MODEL) (used in fixedOther)
	getPower2 <- reactive({
		if(length(getMain())>0) paste0(getMain(),"^2")
	})
  
	# >> determine the model formula for the casewise regressions and the linear mixed model (meta)
  
	# when data are selected, determine the R model for the case regressions
	getCasesModel <- reactive({
		validate( need(input$file, '\nmodel specification requires data') )
		if(length(getFixed())==0) ..mdl <- paste(input$response,"~ 1")
		else ..mdl <- paste(input$response, "~", paste(getFixed(),collapse=" + "))
		..mdl
	})
	# when data are selected, determine the R model for multilevel regression
	getMetaModel <- reactive({
		# if file is uploaded
		validate( need(input$file, '\nmodel specification requires data') )
		if(length(getFixed())==0) ..mdl <- paste(input$response,"~ 1")
		else ..mdl <- paste(input$response, "~", paste(getFixed(),collapse=" + "))
		# when the case level is specified add it within the random part of the model
		if(input$cases != "---" & !is.null(input$varCases)){
			..varCases <- input$varCases
			if(!any(..varCases=="intercept")) ..varCases <- c("0",..varCases) 
			..varCases[..varCases=="intercept"] <- "1"
			..tmp <- input$cases
			# when also the study level is specified, embed the case level within the study level
			if(input$studies != "---" & !is.null(input$varStudies)){
				..tmp <- paste0(input$studies,":",input$cases) 
			}          
			..mdl <- paste(..mdl," + (",paste(..varCases,collapse=" + ")," | ",..tmp,")",sep="")
		} 
		# when the study level is specified add it within the random part of the model
		if(input$studies != "---"  & !is.null(input$varStudies)){
			..varStudies <- input$varStudies
			if(!any(..varStudies=="intercept")) ..varStudies <- c("0",..varStudies) 
			..varStudies[..varStudies=="intercept"] <- "1"
			..mdl <- paste(..mdl," + (",paste(..varStudies,collapse=" + ")," | ",input$studies,")",sep="")
		} 
		# return the model
		..mdl
	})

	# 2do -> include || into the models
	# lmer(y ~ treat*xcase + (1 + treat | study:case) + (1 + treat || study), data = dta)
	# lmer(y ~ treat + time + treat*time + (1 + treat + time + treat*time || subject), data = dta)

	### end of the input of data and model ###
	
	### begin analysis ###
	
## CASE REGRESSIONS ##
	
	# regressions
	
	# show the selected model
	output$casewiseRegressionInfo <- renderPrint({
		cat(getCasesModel())  
	})
	# per case regression, only considering the fixed part
	output$regressCaseResults <- renderTable({
		getProcessedCaseRegressionCoef()
	})
	# for all case regressions, retrieve the coefficients (used in regressCaseResults)
	getProcessedCaseRegressionCoef <- reactive({
		..res <- getProcessedCaseRegressions()
		..lm <- lapply(..res,function(.x) round(coef(summary(.x)),3))
		..lm <- ldply(lapply(..lm, function(.x) data.frame(pred=row.names(.x),.x)),.id="case")
		..lm
	})	
	# perform case regressions (used in getProcessedCaseRegressionCoef)
	getProcessedCaseRegressions <- reactive({
		validate(      
			need(input$file, ''),
			need(input$cases != '---', 'case regressions require case identification')
		)
		..dta <- getProcessedData()
		..lm <- vector("list",length=length(unique(..dta[,input$cases])))
		..cntr <- 0
		for(it in unique(..dta[,input$cases])){
			..cntr <- ..cntr + 1
			..tdta <- ..dta[..dta[,input$cases]==it,]
			..lm[[..cntr]] <- lm(as.formula(getCasesModel()),data=..tdta)
		}
		names(..lm) <- unique(..dta[,input$cases])
		..lm  
	})
	# show the download link for data table
	output$showDownloadCaseResults <- renderUI({
		downloadLink('downloadCaseResults', 'Download Regression Results in Tab Delimited Text File Format')
	})
	# download the selected data (used in showDownloadCaseResults)
	output$downloadCaseResults <- downloadHandler(
		filename = function() {
			paste('caseRegression_', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
		},
		content = function(file) {
			write.table(getProcessedCaseRegressionCoef(), file, sep="\t", row.names=F)
		}
	)
	
	
	# lm case plots: response over time per treatment for all selected cases, with prediction/confidence bands
	
	# show lm case plots
	output$showLmCasePlots <- renderUI({
		checkboxInput("showLmCasePlots","show casewise regression plots (downloadable)", FALSE)
	})	
	# show the download link for casewise regression plots
	output$showDownloadLmCasePlots <- renderUI({
		validate(need(input$showLmCasePlots,"download link for casewise regression plots shown only on request"))
		if(input$showLmCasePlots) downloadLink('downloadLmCasePlots', 'Download Design Plots in Png Format')
	})
	# download the selected lm case plots (used in showDownloadLmCasePlots)
	output$downloadLmCasePlots <- downloadHandler(
		filename = function() { 
			paste('lmCasePlot_', format(Sys.time(), "%d-%b_%Hh%M"), '.png', sep='')
		},
		content = function(file) {
			device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
			ggsave(file, plot = getLmCasePlots(), device = device)
		}
	)
	# show slider to specify the plot height (used in getHeightFromSiderDesign)
	output$sliderLmCasePlots <- renderUI({
		validate(need(input$showLmCasePlots,"casewise regression plots shown only on request"))
		if(input$showLmCasePlots) sliderInput("sliderLmCasePlots", label = h4("set height"), min = 0, max = 100, value = 50)
	})
	# get height from slider for the lm case plots (used in rCasePlots)
	getHeightFromSliderLmCasePlots <- function() {
		validate(need(input$sliderLmCasePlots,"note: generating many plots can be time consuming"))
		input$sliderLmCasePlots * 100
	}
	# for all case regressions, retrieve the coefficients (used in regressCaseResults)
	addLmResults <- function(.x,..fm){
		..lm <- lm(as.formula(..fm),data=.x)
		..cfd <- predict(..lm, interval="confidence")
		dimnames(..cfd)[[2]] <- c("fit","clwr","cupr")
		..prd <- predict(..lm, interval="prediction")[,-1]
		dimnames(..prd)[[2]] <- c("plwr","pupr")
		..sdta <- merge(.x,..cfd,by="row.names",all.x=T)
		row.names(..sdta) <- ..sdta[,"Row.names"]
		..sdta$Row.names <- NULL
		..sdta <- merge(..sdta,..prd,by="row.names",all.x=T)[,-1]
		..sdta
	}
	getProcessedCaseRegressionBands <- reactive({
		..dta <- getProcessedData()
		..dta <- ddply(..dta,"case",.fun = function(.x,mytreat) mutate(.x, ..csm=c(0,cumsum(.x[,mytreat][-length(.x[,mytreat])]!=.x[,mytreat][-1]))), mytreat=input$treatment)
		..dta$..csm <- factor(..dta$..csm)
		..formula <- getCasesModel()
		..formula <- gsub(input$treatment,"..csm" , ..formula)
		..lmb <- ddply(..dta,"case",.fun = function(.x,.fm) addLmResults(.x,.fm),.fm=..formula)
		..lmb
	})	

	# render the lm case plots 
	output$rLmCasePlots <- renderPlot(height=getHeightFromSliderLmCasePlots,{
		if(input$showLmCasePlots) getLmCasePlots()
	})
	# create the lm case plots (used in downloadLmCasePlots / rCasePlots)
	getLmCasePlots <- function(){
		# ..dta <- getProcessedData()
		# ..dta <- ddply(..dta,"case",.fun = function(.x,mytreat) mutate(.x, ..csm=c(0,cumsum(.x[,mytreat][-length(.x[,mytreat])]!=.x[,mytreat][-1]))), mytreat=input$treatment)
		# ..plots <- ggplot(..dta,aes(x=..dta[,input$time],y=..dta[,input$response])) 
		# ..plots <- ..plots + geom_line(aes(group = as.factor(..csm),color=..dta[,input$treatment]),size=1.3)
		# ..plots <- ..plots + geom_point()
		# if(input$cases!="---" & length(unique(..dta[,input$cases]))>1) ..plots <- ..plots + facet_grid(reformulate(".",input$cases)) 
		# ..plots <- ..plots + scale_x_continuous("time",limits=c(min(..dta[,input$time]), max(..dta[,input$time]))) + scale_y_continuous("response")
		# ..plots <- ..plots + guides(linetype=FALSE) 
		# ..plots <- ..plots + theme(legend.position="none")
		# ..plots <- ..plots + ggtitle("casewise responses over time (base and treatments)")
		# ..plots
		
		..tdta <- getProcessedCaseRegressionBands()
		# create plot
		..plot <- ggplot(..tdta, aes(x=..tdta[,input$time],y=fit,col=..tdta[,input$treatment]))
		..plot <- ..plot + geom_point(aes(x=..tdta[,input$time],y=..tdta[,input$response])) + facet_grid(case ~ .)
		# copy paste
		..plot <- ..plot + geom_line(aes(group = as.factor(..csm),color=..tdta[,input$treatment]),size=1.3)
		#..plot <- ..plot + geom_point() + facet_grid(case ~ .)
		..plot <- ..plot + scale_x_continuous("time",limits=c(min(..tdta[,input$time]), max(..tdta[,input$time]))) + scale_y_continuous("response")
		..plot <- ..plot + guides(linetype=FALSE) 
		..plot <- ..plot + theme(legend.position="none")
		..plot <- ..plot + ggtitle("casewise responses over time (base and treatments)")

		# for confidence band
		..plot <- ..plot + geom_ribbon(data=..tdta,aes(ymin=clwr,ymax=cupr,group=..csm),alpha=.4)
		# for prediction band
		..plot <- ..plot + geom_ribbon(data=..tdta,aes(ymin=plwr,ymax=pupr,group=..csm),alpha=.2)
		# plot
		..plot 

	}

	# predictions
	
	# information on the prediction, now only showing the time specification
	output$casewisePredictionInfo <- renderPrint({
		cat(paste("predictions for time = ",input$effectPlotSlider))
	})
	# show the predictions
	output$predictionCaseResults <- renderTable({
		getCaseRegressionPreds()
	})
	# slider to select time of treatment (used as input in info, prediction and plots)
	output$sliderPredictTime <- renderUI({
		sliderInput("effectPlotSlider", label = h4("time specific prediction"), min = 0, max = getTimeRange(), value = 0)
	})
	# get time range to build slider for prediction (used in sliderPredictionTime)
	getTimeRange <- function() {
		validate(      
			need(input$file, ''),
			need(input$time != '---', 'time is not specified')
		)
		..dta <- getProcessedData()
		range(..dta[,input$time])%*%c(-1.5,1.5)
	}
	# for all case regressions, predict the score for a given time (used in predictionCaseResults)
	# TMP !! use getFixed() for the model as.formula
	# TMP !! include more input options ?? !!
	# consider extension, now only treatment and time is considered
	getCaseRegressionPreds <- reactive({
		validate(
			need(input$effectPlotSlider,'something wrong with slider')
		)
		..lm <- getProcessedCaseRegressions()
		# TMP !! assumes that "1" is the treatment condition
		..new <- data.frame("1",input$effectPlotSlider)
		names(..new) <- c(input$treatment,input$time)
		..prd <- lapply(..lm,function(.x) round(data.frame(predict(.x, ..new, interval="predict",se.fit=T) ),3))
		..prd <- do.call(rbind,..prd) #[,1:5]
		names(..prd) <- c("fit","lower","upper","se","df","resid.")
		..prd 
	})
	# show the download link for data table
	output$showDownloadCasePredictions <- renderUI({
		downloadLink('downloadCasePredictions', 'Download Prediction Results in Tab Delimited Text File Format')
	})
	# download the selected data (used in showDownloadCaseResults)
	output$downloadCasePredictions <- downloadHandler(
		filename = function() {
			paste('casePrediction_', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
		},
		content = function(file) {
			write.table(getCaseRegressionPreds(), file, sep="\t", row.names=F)
		}
	)
	
	
## META ANALYSIS ##

	# show the selected model
	output$metaInfo <- renderPrint({
		cat(getMetaModel())
	})
	# show the selected model
	output$metaInterpret <- renderPrint({
		cat("The fixed effects indicate the population average\nThe random effects are estimated to incorporate the case/study specific deviations.\n")
	})
	# button to calculate the linear mixed model
	resultsMeta <- eventReactive(
		input$r, {
		# note, button is linked to 'isolate', alternative to if(button>0)
			..dta <- getProcessedData()
			mdl <- getMetaModel()
			# when the button is pressed !!
			if(input$cases != "---" || input$studies != "---"){
				lmer(as.formula(mdl),data=..dta)
			}
			else{ lm(as.formula(mdl),data=..dta)	}
	})
	# output the calculated linear mixed model summary
	output$metaResults <- renderPrint({
		validate(      
			need(input$r, 'the << RUN >> button is not pressed yet')
		)
		summary(resultsMeta())
	})
	
## CASE PLOTS ##

	# information on the prediction, now only showing the time specification
	output$plotCaseInfo <- renderPrint({
		cat(paste("select the plot height",input$sliderCasePlot,"%"))
	})
	getHeightFromSlider <- function() {
		input$sliderCasePlot * 100
	}
	output$casePlot <- renderPlot(height=getHeightFromSlider,{
		validate(      need(input$file, 'first load & define the data')	  )
		# read in the data and rename variables for use in the plots
		..dta <- getProcessedData()
		# plot the case regressions and raw data      
		..dta$..treat <- factor(..dta[,input$treatment])
		levels(..dta$..treat) <- 1:length(levels(..dta$..treat))
		out <- ggplot(data=..dta,aes_string(x=input$time,y=input$response,linetype="..treat")) + geom_line() + geom_smooth(method='lm',lwd=1.4) 
		if(input$cases!="---") out <- out + facet_grid(reformulate(".",input$cases)) 
		out <- out + guides(linetype=FALSE) 
		out <- out + scale_x_continuous("time",limits=c(min(..dta[,input$time]), max(..dta[,input$time]))) + scale_y_continuous("response")
		out + ggtitle("responses")
	})
	
	####################################################################################################
# browser() to interupt
# display is showcase
  
	# CASE PLOTS
	  
	# output$effectPlot <- renderPlot(height=getHeightFromSlider,{
		# validate(      need(input$file, '')    )
		# ..dta <- getProcessedData()
		# ..dta$..treat <- factor(..dta[,input$treatment])
		# levels(..dta$..treat) <- 1:length(levels(..dta$..treat))
		# out <- ggplot(data=..dta,aes_string(x=input$time,y=input$response,linetype="..treat")) + geom_line() + geom_smooth(method='lm',lwd=1.4) 
		# if(input$cases!="---") out <- out + facet_grid(reformulate(".",input$cases)) 
		# out <- out + guides(linetype=FALSE) 
		# out <- out + scale_x_continuous("time",limits=c(min(..dta[,input$time]), max(..dta[,input$time]))) + scale_y_continuous("response")
		# out <- out + geom_vline(xintercept = input$effectPlotSlider)
		# out <- out + ggtitle("effects")
		# out
	# })
  
	################################################################################################
	# NOT INCLUDED
	# do.call(rbind,describeBy(dta$y,dta$case))[,-1]
	# cast(aggregate(tdta$y,by=list(case=tdta$case,treat=tdta$treat),mean),case~treat)

})