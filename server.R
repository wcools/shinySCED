# SHINY APP to evaluate single case designs, possibly with multilevel data analysis
# the shiny app consists of two main parts
# 1. UI part for the input-output interface
# 2. SERVER part for the functionality -> THIS PART

# part of the UI is SERVER dependent
# reactive functions get triggered by changes in input$

#? for possible future changes
#- for removals when finalized

# to do list::
# check if packages can be removed
# support other datafile types
# shinyapps.io // Amazon Web
# standardization uses y ~ treat only
# check and recode the work around for centering (transtime)
# rewrite getPlotDesign from facets to list of single plots
# add Data tab for visualisation and summary
# reconsider the use of default models and further refinements
		
# FLOW
# open data -> trigger reading of variables + data structure
# assign variables -> trigger checkboxes studies/cases
# assign cases and response -> trigger data summary

# 2do -> include || into the models
# lmer(y ~ treat*xcase + (1 + treat | study:case) + (1 + treat || study), data = dta)
# lmer(y ~ treat + time + treat*time + (1 + treat + time + treat*time || subject), data = dta)


# --------------------------------------------------------------------------------------------------------------------------------- #

# install required packages not yet installed
pckgs <- c('shiny','reshape2','ggplot2','plyr','arm')
todo <- pckgs[!is.element(pckgs, names(installed.packages()[,"Package"]))]
if(length(todo) > 0) install.packages(todo)  #,repos="http://cran.us.r-project.org")

# load in the packages
library(shiny)
library(ggplot2)  # ggplot2 -> plotting
library(plyr)     # ddply -> data manipulation
library(arm)      # lmer -> multilevel modeling
library(reshape2)

# note: maybe in future reshape2 should be replaced by tidyr

# --------------------------------------------------------------------------------------------------------------------------------- #

# The UI consists of several tabs. 
# The SERVER functionality is described per tab for convenience

# The logic to understand
# changes in input$ result in triggering reactive, render and observer functions
# reactive and render functions return values as output$

# ---------------------------------------- #

# server file
shinyServer(function(input,output,session){

## START ## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# The start page offers some information defined in the UI
# The SERVER supports uploading data
	
	# >> UPLOAD DATA
	
	#? allow for excel data files, and others
	
	# when data selected, read tab delimited text file and return
	uploadData <- reactive({
		if(is.null(input$file)){		NULL		}
		else{
			if(input$runExample)	..dta <- read.table(file="testData.txt",sep="\t",header=T)
			else ..dta <- read.table(file=input$file$datapath,sep="\t",header=T)			
		..dta
		}
	})
	
## end start  <<

## DATA ## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# The DATA page is set up to assign variable names and transform some of them
# A subselection of studies and cases within studies is possible
# The resulting data structure, descriptive statistics and plots are included
	
	# >> READ VARIABLE NAMES
	
	# read data from the uploaded data file and return
	getVarnames <- reactive({
		validate(	    need(input$file, '')	  )
		names(uploadData())
	})
	
	# == reactive output for the UI
	
	# provide information on the variable identification
	output$data.txt.require <- renderText({	
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
				if(input$response=="---") ..info <- paste0(..info,"numeric response variable: required\n")	
				if(input$treatment=="---") ..info <- paste0(..info,"treatment variable (character/factor): required\n")
				if(input$cases=="---") ..info <- paste0(..info,"cases variables: only if multiple exist (subsets can be made)\n")
				if(input$studies=="---") ..info <- paste0(..info,"studies variable: only if multiple exist (subsets can be made)\n")
				if(input$time=="---") ..info <- paste0(..info,"numeric with values 1upwards: required, not necessarilly part of the model\n")
			}
			else(
				..info <- paste0(..info,"subsets of cases/studies can be made below using the checkboxes.\nstandardization -> response/RMSE(casewiseregression)\ncentering -> set time to 0 for the first observation within the treatment phase\n")
			) 
		)
		if(any(table(c(input$response,input$treatment,input$cases,input$studies,input$time))[names(table(c(input$response,input$treatment,input$cases,input$studies,input$time)))!="---"]>1)) ..info <- paste0(..info,"variables can not be assigned double\n")
		..info <- paste0(..info,"\n")
		..info
	})

	# == IDENTIFY VARIABLES & CASES: Response, Case level, Study level, treatment indicator and time specification
  
	# identify variables
	
	# to select the response
	output$data.select.response <- renderUI({
	#? !! extend the validates here ? !!
		validate(      need(input$file, '')	  )
		if(input$runExample) selectInput("response", "Response (numeric/integer):", choices=c("---",setdiff(getVarnames(), "---")), selected="Y")
		else selectInput("response", "Response (numeric/integer):", choices=c("---",setdiff(getVarnames(), "---")))
	})
	# to select the case
	output$data.select.cases <- renderUI({
		validate(      need(input$file, '')	  )
		if(input$runExample) selectInput("cases", "Cases (character/factor):", choices=c("---",setdiff(getVarnames(), "---")), selected="caseId")
		else selectInput("cases", "Cases (character/factor):", choices=c("---",setdiff(getVarnames(), "---")))
	})
	# to select the study
	output$data.select.studies <- renderUI({
		validate(      need(input$file, '')	  )
		if(input$runExample) selectInput("studies", "Studies (character/factor):", choices=c("---",setdiff(getVarnames(), "---")), selected="studyId")
		else selectInput("studies", "Studies (character/factor):", choices=c("---",setdiff(getVarnames(), "---")))
	})
	# to select the treatment
	output$data.select.treatment <- renderUI({
		validate(      need(input$file, '')	  )
		if(input$runExample) selectInput("treatment", "Treatment (character/factor):", choices=c("---",setdiff(getVarnames(), "---")), selected="treat")
		else selectInput("treatment", "Treatment (character/factor):", choices=c("---",setdiff(getVarnames(), "---")))
	})
	# to select the control level indicator for the treatment
	output$data.select.control <- renderUI({
		validate(      need(input$treatment, '')	  )
		..choices <- c("no control selected")
		if(input$treatment != "---") ..choices <- c(unique(uploadData()[,input$treatment]))
		if(input$runExample) selectInput("control", "Control group indicator", choices=..choices, , selected="0", width='50%')
		else selectInput("control", "Control group indicator", choices=..choices, width='50%')
	})
	# to select the time
	output$data.select.time <- renderUI({
		validate(      need(input$file, '')	  )
		if(input$runExample) selectInput("time", "Time/Order (numeric/integer):", choices=c("---",setdiff(getVarnames(), "---")), selected="time")
		else selectInput("time", "Time/Order (numeric/integer):", choices=c("---",setdiff(getVarnames(), "---")))
	})

	# transformations

	# to choose to standardize or not
	output$data.check.standardized <- renderUI({
		validate(     
			# check whether dataset is uploaded / response variable assigned
			need(input$file, ''),
			need(input$response != "---", '')    
		)
		# show checkbox, with default NOT STANDARDIZED
		checkboxInput("standardize", "Standardized (response/RMSE) with treatment as predictor", FALSE)
	})
  
	# to choose to center time or not
	output$data.check.transtime <- renderUI({
		validate(   
			# check whether dataset is uploaded / response variable assigned
			need(input$file, ''),
			need(input$time != "---", '')    
		)
		# show checkbox, with default NOT CENTERED
		checkboxInput("transtime", "Center Time (0 for first observation of treatment)", FALSE)
	})
	
	# to choose to center time or not
	output$data.check.concatenate <- renderUI({
		validate(   
			# check whether dataset is uploaded / response variable assigned
			need(input$file, ''),
			need(input$time != "---", '')    
		)
		# show checkbox, with default NOT CENTERED
		checkboxInput("concatenate", "Concatenate study names to case names", TRUE)
	})
	
	# identify cases

	# to choose the studies to include
	output$data.checks.selectedStudies <- renderUI({
		# check whether dataset is uploaded / response variable assigned
		validate(      
			need(input$file, ''),
			need(input$studies != "---", '')    
		)
		# use selected data to populate the studies to select
		..dta <- uploadData()
		..sdta <- ..dta
		if(input$cases != "---") ..sdta <- ..dta[..dta[,input$cases]%in%input$selectCases,]
		# show checkbox, with default all studies selected 
		checkboxGroupInput("selectStudies", "Studies:", choices=as.character(unique(..dta[,input$studies])), selected=as.character(unique(..sdta[,input$studies])))
	})
	# to choose the cases to include
	output$data.checks.selectedCases <- renderUI({
		..dta <- uploadData()		
		validate(   
			# check whether dataset is uploaded / response variable assigned
			need(input$file, ''),
			need(input$cases != "---", 'no cases defined')    
		)
		..sdta <- ..dta
		# use selected data to populate the cases to select
		if(input$studies != "---") ..sdta <- ..dta[..dta[,input$studies]%in%input$selectStudies,]
		# show checkbox, with default all cases selected
		checkboxGroupInput("selectCases", "Cases:", choices=as.character(unique(..dta[,input$cases])), selected=as.character(unique(..sdta[,input$cases]))) 
	}) 
 	# add an all or none to the selected studies
    observe({
		..dta <- uploadData()
		validate(      
			need(input$file, ''),
			need(input$studies != "---", '')    
		)
		# use selected data to populate the studies to select
		if(input$studies != "---"){
			updateCheckboxGroupInput(
				session, 'selectStudies', choices = as.character(unique(..dta[,input$studies])),
				selected = if (!input$data.check.toggleStudies) as.character(unique(..dta[,input$studies]))
			)
		}
    })
	# add an all or none to the selected cases
    observe({
		..dta <- uploadData()
		validate(      
			need(input$file, ''),
			need(input$cases != "---", '')    
		)
		# use selected data to populate the studies to select
		updateCheckboxGroupInput(
			session, 'selectCases', choices = as.character(unique(..dta[,input$cases])),
			selected = if (!input$data.check.toggleCases) as.character(unique(..dta[,input$cases]))
		)
    })
	# end identify variables and cases
	
	# >> RETRIEVE USER SELECTED DATA: make appropriate modifications and apply transformations (process)
	
	# get subsetted data (based on selected cases / studies) and make type conversions
	getData <- reactive({
	
		# check whether dataset is uploaded
		validate(      
			need(input$file, 'an uploaded file is required'),
			need(input$cases, 'a case identifyer is required'),
			need(input$studies, 'a study identifyer is required'),
			need(input$treatment, 'a treatment identifyer is required'),
			need(input$time, 'a time identifyer is required')
		)
		req(input$selectCases)
		# use imported data 
		..dta <- uploadData()
		# return a treatment factor (used in predictions), numeric response and time
		..tmp <- ..dta[,input$treatment]
		..dta[,input$treatment] <- factor(..tmp,levels=c(input$control,unique(..tmp)[!unique(..tmp)%in%input$control]))
		# ..dta[,input$treatment] <- as.factor(..dta[,input$treatment])
		..dta[,input$response] <- as.numeric(..dta[,input$response])
		..dta[,input$time] <- as.numeric(..dta[,input$time])
		# subset the selected cases if any
		if(input$cases != "---"){
			..dta <- ..dta[..dta[,input$cases] %in% input$selectCases,]
			..dta[,input$cases] <- as.factor(..dta[,input$cases])
		} 
		# subset the selected studies if any
		if(input$studies != "---"){
			..dta <- ..dta[..dta[,input$studies] %in% input$selectStudies,]
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
		if(input$concatenate & input$cases != "---" & input$studies != "---") ..dta[,input$cases] <- factor(paste0(..dta[,input$studies],"_",..dta[,input$cases]))
		
		droplevels(..dta)
	})
	
	# standardize the data using the casewise regression output
	# center the time variable to have 0 for first treatment observation
	centerTime <- function(..tdta){
		..nr <- min(..tdta[..tdta[,input$treatment]!=input$control,input$time])
		..tdta[,input$time] <- ..tdta[,input$time] - ..nr
		return(..tdta)
	}
	getProcessedData <- reactive({
		# check whether dataset is uploaded
		validate(      
			need(input$transtime%in%c(TRUE,FALSE), 'centering checkbox available'),
			need(input$standardize%in%c(TRUE,FALSE), 'standardize checkbox available')
		)
		..dta <- getData()
		# run transformation if requested and when time/treatment are specified
		# for each case, order the observations and translate time to set first treatment observation to 0
		if(input$transtime){
			..dta <- ddply(..dta,input$cases,function(.x) centerTime(.x))
		}
		# if a response is selected, that should be standardized
		#? currently for standardization a simple model is used response ~ treatment, maybe this should be altered (use getCaseRegressions or getCaseModel)
		if(input$standardize){
			# perform regression to get the standard error
			..lmL <- dlply(..dta,c(input$cases),function(.x) list(data=.x,lm=lm(as.formula(paste0(input$response,"~",input$treatment)),data=.x)))
			..tmp <- do.call(rbind,lapply(..lmL,function(.x) summary(.x$lm)[["sigma"]]))
			# divide the responses for each case by the standard error for that case's regression [RMSE]
			..dta[,input$response] <- ..dta[,input$response] / ..tmp[match(..dta[,input$cases],row.names(..tmp))]
		}
		..dta    
	})

	# str() function on the subsetted data
	output$data.txt.structure <- renderPrint({		str(getProcessedData())		})

	# >> SHOW DATA structure, descriptives, plots, data
		
	# DESCRIPTIVES
	
	
	# to choose the cases to include
	output$desc.checks.selectedCases <- renderUI({
		..dta <- getProcessedData()		
		validate(   
			# check whether dataset is uploaded / response variable assigned
			need(input$file, ''),
			need(input$cases != "---", 'no cases defined')    
		)
		..sdta <- ..dta[..dta[,input$cases]%in%input$selectCases,]
		# use selected data to populate the cases to select
		if(input$studies != "---") ..sdta <- ..dta[..dta[,input$studies]%in%input$selectStudies,]
		# call for plot construction
		# setPlots()
		checkboxGroupInput("subSelectCases4Description", "Cases:", choices=as.character(unique(..sdta[,input$cases])), selected=as.character(unique(..sdta[,input$cases]))) 
	})

	# case specific summary table (observations in value and frequency, nr missing)
	output$desc.table.descriptives <- renderDataTable({
	   validate(      
	      need(input$cases!="---", 'multiple cases must be present to get summary statistics'),
	      need(input$treatment!="---", 'treatment should be identified'),
		  need(input$response!="---", 'response should be identified')
	   )
	   ..dta <- getProcessedData()
	   ..dta <- ..dta[..dta[,input$cases] %in% input$subSelectCases4Description,]
	   ..tmp <- melt(..dta,id.vars=c(input$treatment,input$cases),measure.vars=input$response)
	   ..rng <- aggregate(list(range=..tmp[,"value"]),list(..case=..tmp[,input$cases]),range,na.rm=T)
	   ..tbl <- aggregate(list(freq=..tmp[,input$treatment]),list(..case=..tmp[,input$cases]),table)
	   ..mss <- aggregate(list(miss=..tmp[,"value"]),list(..case=..tmp[,input$cases]),function(.x) sum(is.na(.x)))
	   ..tmp <- merge(..rng,..tbl,by="..case")
	   ..tmp <- cbind(..tmp[,"..case"],as.data.frame(..tmp$range),as.data.frame(..tmp$freq))
	   names(..tmp)[1:3] <- c("..case","min","max")
	   ..tmp <- merge(..tmp,..mss,by="..case")
	   names(..tmp)[1] <- c(input$cases)
	   ..tmp
	})
	
	# PLOTS
	
	# request design plots
	output$desc.check.showDesignPlots <- renderUI({
		checkboxInput("showDesignPlots","show design plots (downloadable)", FALSE)
	})	
	# request the download link for design plots
	output$desc.link.showDownloadDesignPlots <- renderUI({
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
	output$desc.slider.caseDesign <- renderUI({
		validate(need(input$showDesignPlots,"design plots shown only on request (time consuming)"))
		if(input$showDesignPlots) sliderInput("sliderCaseDesign", label = h4("set height"), min = 0, max = 100, value = 50)
	})
	# get height from slider for the design plots (used in rCasePlots)
	getHeightFromSliderDesign <- function() {
		validate(need(input$sliderCaseDesign,"note: generating many plots can be time consuming"))
		input$sliderCaseDesign * 20
	}
	# render the design plots 
	output$desc.plot.caseDesign <- renderPlot(height=getHeightFromSliderDesign,{
		if(input$showDesignPlots) getPlotDesign()
	})
	# create the design plots (used in downloadDesignPlots / data.plot.caseDesign)
	getPlotDesign <- function(){
		..dta <- getProcessedData()
		..dta <- ..dta[..dta[,input$cases] %in% input$subSelectCases4Description,]
		..plots <- ggplot(..dta,aes(x=..dta[,input$time],y=..dta[,input$response])) 
		..plots <- ..plots + geom_line(aes(group = ..dta[,input$treatment],color=..dta[,input$treatment]),size=1.3)
		..plots <- ..plots + geom_point()
		if(input$cases!="---" & length(unique(..dta[,input$cases]))>1) ..plots <- ..plots + facet_grid(reformulate(".",input$cases)) 
		..plots <- ..plots + scale_x_continuous("time",limits=c(min(..dta[,input$time]), max(..dta[,input$time]))) + scale_y_continuous("response")
		..plots <- ..plots + guides(linetype=FALSE) 
		..plots <- ..plots + theme(legend.position="none")
		..plots <- ..plots + ggtitle("casewise responses over time (base and treatments)")
		..plots
	}

	# TABLE 
	
	# show shiny datatable
	output$data.check.table <- renderUI({
		checkboxInput("showDataTable","show shiny datatable (downloadable)", FALSE)
	})
	# show the download link for data table
	output$data.link.downloadTable <- renderUI({
		validate(need(input$showDataTable,"download link for table shown only on request"))
		if(input$showDataTable) downloadLink('downloadDataTable', 'Download Table in Tab Delimited Text File Format')
	})
	# download the selected data (used in data.link.downloadTable)
	output$downloadDataTable <- downloadHandler(
		filename = function() {
			paste('data_', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
		},
		content = function(file) {
			write.table(getProcessedData(), file, sep="\t", row.names=F)
		}
	)	
	# get data for shiny table
	output$data.table.selected <- renderDataTable({
		validate(need(input$showDataTable,"table shown only on request\n"))
		if(input$showDataTable) getProcessedData()
	})

## end DATA  <<
	
## MODEL ## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# The model page offers the identification and refinement of models
# One of three models and individual predictors, both fixed and random, can be selected
#
	
	# >> MODEL SPECIFICATION and REFINEMENT
	
	# model selection
	
	# three base models to select from
	getModel <- function(id){
		validate(      need(input$file, '')	  )
		if(id==1) model <- paste0("model 1:: Y ~ 1 + ",input$treatment)
		else if(id==2) model <- paste0("model 2:: Y ~ 1 + ",input$treatment," + ",input$time," + ",input$treatment,":",input$time)
		else if(id==3) model <- paste0("model 3:: Y ~ 1 + ",input$treatment," + ",input$time," + ",input$time,"² + ",input$treatment,":",input$time," + ",input$treatment,":",input$time,"²")
		model
	}

	# default model dependent selection of main effects
	getSimpleModelPredictors <- reactive({
		validate(      need(input$file, '')	  )
		# variables selected as part of the model
		tmp <- setdiff(c(input$treatment,input$time),"---")
		if(substring(input$model,7,7)=="1") tmp <- c(input$treatment) 
		if(substring(input$model,7,7)=="2") tmp <- c(input$treatment,input$time)
		if(substring(input$model,7,7)=="3") tmp <- c(input$treatment,input$time)
		if(length(tmp)>0) tmp
	}) 
	# default model dependent selection of higher order effects
	getExtendedModelPredictors <- reactive({
		validate(      need(input$file, '')	  )
		tmp <- "" 
		if(substring(input$model,7,7)=="2") tmp <- paste0(input$treatment,":",input$time)
		if(substring(input$model,7,7)=="3"){
			tmp <- c(paste0(input$treatment,":",input$time),paste0(input$treatment,":I(",input$time,"^2)"),paste0("I(",input$time,"^2)"))
		} 
		if(length(tmp)>0) tmp
	}) 


	# all variables not response, case nor study
	getPredictors <- reactive({
		setdiff(getVarnames(),c(input$response,input$cases,input$studies))
	})
	
	# model refinement 

	# render fixed variable names
	output$model.check.simpleFixed <- renderUI({
		# if file is uploaded and response is selected
		validate( 
			need(input$file, ''),
			need(input$response != "---", '')    
		)
		req(input$model)
	  	# show checkbox with all variables, their interactions and 2nd order polynomial with default unchecked except for the main ones
		checkboxGroupInput("simpleFixed", "fixed:", choices=as.list(c(getPredictors())), selected=getSimpleModelPredictors())
	})
	# render fixed variable names
	output$model.check.extendedFixed <- renderUI({
		# if file is uploaded and response is selected
		validate( 
			need(input$file, ''),
			need(input$response != "---", '')    
		)
		req(input$model)
		# if a treatment is selected
		checkboxGroupInput("extendedFixed", "", choices=as.list(c(getInteractions(),getPower2())), selected=getExtendedModelPredictors())
	})
	# random variable, per case
	output$model.check.casesRandom <- renderUI({
		# if file is uploaded and cases are selected
		validate(      
			need(input$file, ''),
			need(input$cases != "---", ''),
			need(input$response != "---", '')
		)
		# retrieve all predictors with the intercept to select from
		selected <- c("intercept",getAllSelectedPredictors())
		# show checkbox with all random variables over cases, with only the intercept checked by default 
		checkboxGroupInput("varCases", "case (random):", choices=selected, selected="intercept")
	})  
	# random variables, per study
	output$model.check.studiesRandom <- renderUI({
		# if file is uploaded and cases are selected
		validate(      
			need(input$file, ''),
			need(input$studies != "---", '')    
		)
		# retrieve all predictors with the intercept to select from
		selected <- c("intercept",getAllSelectedPredictors())
		# show checkbox with all random variables over studies, with only the intercept checked by default 
		checkboxGroupInput("varStudies", "study (random):", choices=selected, selected="intercept") 
	})	
	
	
	# retrieve all main effects in the model
	getSimpleSelectedPredictors <- reactive({
		validate(      need(input$file, '')	  )
		input$simpleFixed    
	})  
	# construct predictor interactions
	getInteractions <- reactive({
		if(length(getSimpleSelectedPredictors())>1) apply(combn(getSimpleSelectedPredictors(),2),2,function(.x) paste(.x,collapse=":"))
	})
	# construct predictor to the power 2 for time
	getPower2 <- reactive({
		out <- ""
		if(length(getSimpleSelectedPredictors())>0){
			if(input$time %in% getSimpleSelectedPredictors())	out <- paste0("I(",input$time,"^2)")
			if(all(c(input$time,input$treatment) %in% getSimpleSelectedPredictors())) out <- c(out,paste0(input$treatment,":","I(",input$time,"^2)"))
		}
	})
	# retrieve all fixed effects in the model
	getAllSelectedPredictors <- reactive({
		validate(      need(input$file, '')	  )
		c(input$simpleFixed,input$extendedFixed)    
	}) 
	
	# to select the model
	output$model.check.select <- renderUI({
	# TMP !! extend the validates here ? !!
		validate(      need(input$file, '')	  )
		#+ TMP set ... and remaining variable names [TMP remove: with y selected if available]
		if(input$runExample) selectInput("model", "Select Model", choices=c(getModel(1),getModel(2),getModel(3)), selected=getModel(2))
		else selectInput("model", "Select Model", choices=c(getModel(1),getModel(2),getModel(3)))
	})
	# show the multilevel model in R format
	output$model.txt.formula <- renderPrint({
	# TMP !! extend the validates here ? !!
		validate(      need(input$file, '')	  )
		req(input$model)
		out <- "type of model"
		if(substring(input$model,7,7)=="1") out <- "treatment effect"
		if(substring(input$model,7,7)=="2") out <- "treatment over time effect"
		if(substring(input$model,7,7)=="3") out <- "treatment over polynomial time effect"
		if(substring(input$model,7,7)=="1" && !all(c(input$treatment) %in% getAllSelectedPredictors())) out <- paste0(out,">> model implies treatment")
		if(substring(input$model,7,7)=="2" && !all(c(input$treatment,input$time) %in% getAllSelectedPredictors())) out <- paste0(out,">> model implies time and treament")
		if(substring(input$model,7,7)=="3" && !all(c(input$treatment,input$time) %in% getAllSelectedPredictors())) out <- paste0(out,">> model implies time and treament")
		cat(out)
	})
		
	# show the case specific model in R format
	output$model.txt.Rformula <- renderPrint({
		cat("case regression\n",getCasesModel(),"\nmeta analysis\n",getMetaModel())  		
	})

	
## end MODEL  <<

## CASE ANALYSIS: estimation and prediction

	# when data are selected, determine the R model for the case regressions
	getCasesModel <- reactive({
		validate( need(input$file, '\nmodel specification requires data') )
		if(length(getAllSelectedPredictors())==0) ..mdl <- paste(input$response,"~ 1")
		else ..mdl <- paste(input$response, "~", paste(getAllSelectedPredictors(),collapse=" + "))
		..mdl
	})
	
	## >> CASE REGRESSION ANALYSIS: linear regression for each of the cases, with confidence bounds and prediction intervals

	# perform case regressions and return list with data and lm output (uses all cases selected at DATA)
	getCaseRegressionList <- reactive({
		validate(      
			need(input$file, ''),
			need(input$cases != '---', 'case regressions require case identification')
		)
		..dta <- getProcessedData()    
		..lmL <- dlply(..dta,c(input$cases),function(.x) list(data=.x,lm=lm(as.formula(getCasesModel()),data=.x)))
		..lmL
	})
	
	# add confidence/prediction interval information and return dataframe (uses all cases selected at DATA)
	getCaseRegressionBounds <- reactive({
		..lmL <- getCaseRegressionList()
		..lmCfd <- lapply(..lmL, function(.x) data.frame(.x$data,predict(.x$lm, interval = 'confidence')))
		..lmPrd <- lapply(..lmL, function(.x) data.frame(.x$data,predict(.x$lm, interval = 'prediction')))
		changeNms <- function(df,txt){
		  names(df)[names(df)%in%c("lwr","upr")] <- paste0(txt,c("Lwr","Upr"))
		  df
		}
		..lmCfd <- lapply(..lmCfd, changeNms, "cfd")
		..lmPrd <- lapply(..lmPrd, changeNms, "prd")
		..lmX <- Map(function(.x,.y) merge(.x,.y), ..lmCfd,..lmPrd)
		..lmX		
	})
  
## CASE ANALYSIS: estimates ## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# First analysis considers cases independently of one another, referred to as case analysis
# The first part of the case analysis page show the casewise regression estimates and related plots
# A number of cases (nested within studies) can be sub-selected

  	# to choose the cases to include
	output$caseEst.checks.selectedCases <- renderUI({
		..dta <- getProcessedData()		
		validate(   
			# check whether dataset is uploaded / response variable assigned
			need(input$file, ''),
			need(input$cases != "---", 'no cases defined')    
		)
		..sdta <- ..dta[..dta[,input$cases]%in%input$selectCases,]
		# use selected data to populate the cases to select
		if(input$studies != "---") ..sdta <- ..dta[..dta[,input$studies]%in%input$selectStudies,]
		# call for plot construction
		# setPlots()
		checkboxGroupInput("subSelectCases4Estimation", "Cases:", choices=as.character(unique(..sdta[,input$cases])), selected=as.character(unique(..sdta[,input$cases]))) 
	})
  	
	## >> show CASE REGRESSION results
	
	# show the selected model
	output$caseEst.txt.formula <- renderPrint({
		cat(getCasesModel())
	})
	# per case regression, only considering the fixed part
	output$caseEst.table.estimates <- renderTable({
		getProcessedCaseRegressionCoef()
	})
	# for all case regressions, retrieve the coefficients (used in regressCaseResults)
	getProcessedCaseRegressionCoef <- reactive({
		..res <- getCaseRegressionList()
		..res <- ..res[input$subSelectCases4Estimation]
		..lm <- lapply(..res,function(.x) round(coef(summary(.x$lm)),3))
		..lm <- lapply(..lm, function(.x) data.frame(..pred=row.names(.x),.x))
		..Lm <- do.call(rbind, Map(data.frame,..id=names(..lm),..lm))
		row.names(..Lm) <- NULL
		..Lm
	})	
	
	# show the download link for data table
	output$caseEst.link.downloadEstimates <- renderUI({
		downloadLink('downloadCaseResults', 'Download Regression Results in Tab Delimited Text File Format')
	})
	# download the selected data (used in case.link.downloadEstimates)
	output$downloadCaseResults <- downloadHandler(
		filename = function() {
			paste('caseRegression_', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
		},
		content = function(file) {
			write.table(getProcessedCaseRegressionCoef(), file, sep="\t", row.names=F)
		}
	)
	
	## >> show CASE PLOTS
	
	# plot holder
	output$caseEst.plots <- renderUI({
		plot_output_list <- lapply(paste0("est_",input$subSelectCases4Estimation), function(i) {
			plotOutput(i, height = 300, width = 450)
		}) 
	})
	# plot for estimation and boundaries
	plot_case_1LA <- function(case_data, prediction, confidence){
		# vline = as.numeric(unique(subset(case_data, input$treatment == 0)[,"input$time"])-.5)
		outp <- ggplot(case_data, aes_string(x = colnames(case_data)[colnames(case_data) == input$time], 
								   y = colnames(case_data)[colnames(case_data) == input$response],
								   group = colnames(case_data)[colnames(case_data) == input$treatment])) + 
		# geom_vline(xintercept = vline, linetype = 'dashed') +
		geom_ribbon(aes(ymin = prdLwr, ymax = prdUpr), alpha = I(0.2)) +
		geom_ribbon(aes(ymin = cfdLwr, ymax = cfdUpr), alpha = I(0.4)) +
		geom_point() +
		geom_line(aes(y = fit), size = 1) +
		ggtitle(paste('Study ',unique(case_data[,input$studies]),', case ',unique(case_data[,input$cases]), sep = ''))
		outp
	}
	# populate plot holder with selected plots (activated when selection buttons are created)
	setEstPlots <- reactive({
		..lmX <- getCaseRegressionBounds()
		..lmPlots <- llply(..lmX, function(.x) plot_case_1LA(.x, prediction, confidence))
		names(..lmPlots) <- paste0("est_",names(..lmPlots))
		for (i in 1:length(..lmPlots)) {
			local({
				my_i <- i
				plotname <- names(..lmPlots)[my_i]
				output[[plotname]] <- renderPlot({
					..lmPlots[[my_i]]
				})
			})
		}
		..lmPlots
	})  
	
	# request design plots
	output$caseEst.check.showCasePlots <- renderUI({
		checkboxInput("showCaseEstPlots","show case plots (may be time consuming)", FALSE)
	})	
	# request the download link for case plots
	output$caseEst.link.showDownloadCasePlots <- renderUI({
		if(input$showCaseEstPlots){ setEstPlots() }
		validate(need(input$showCaseEstPlots,"download link for case plots shown only on request"))
		if(input$showCaseEstPlots){
			downloadLink('downloadCaseEstPlots', 'Download Case Plot in Png Format [Only downloads first]')
		} 
	})
	# download the selected design plots (used in showDownloadDesignPlots)
	output$downloadCaseEstPlots <- downloadHandler(
		filename = function() { 
			paste('caseEstPlot_',format(Sys.time(), "%d-%b_%Hh%M"), '.png', sep='')
		},
		content = function(file) {
			..plotList <- setEstPlots()
			device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
			# for(it in 1:length(..plotList)){
				ggsave(file, plot = ..plotList[[1]], device = device)
			# }
			# ggsave(file, plot = setPlots()[[1]], device = device)
			# mapply(ggsave, file=paste0("plot-", names(..plotList), ".pdf"), plot=..plotList)
		}
	)

## end CASE ANALYSIS, estimates <<


## CASE ANALYSIS, prediction ## >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# The second part of the case analysis page show the casewise time specific predictions
# A number of cases (nested within studies) can be sub-selected
	observeEvent(input$tabset=="preds", {
		#updateSliderInput(session, "effectPlotSlider", value = input$sliderCasePlot)	
		print(input$tabset)
	})
		
  	# to choose the cases to include
	output$casePred.checks.selectedCases <- renderUI({
		..dta <- getProcessedData()		
		validate(   
			# check whether dataset is uploaded / response variable assigned
			need(input$file, ''),
			need(input$cases != "---", 'no cases defined')    
		)
		..sdta <- ..dta[..dta[,input$cases]%in%input$selectCases,]
		# use selected data to populate the cases to select
		if(input$studies != "---") ..sdta <- ..dta[..dta[,input$studies]%in%input$selectStudies,]
		# call for plot construction
		# setPlots()
		checkboxGroupInput("subSelectCases4Prediction", "Cases:", choices=as.character(unique(..sdta[,input$cases])), selected=as.character(unique(..sdta[,input$cases]))) 
	})
	# to choose to standardize or not
	output$casePred.select.alpha <- renderUI({
		validate(      need(input$file, '')	  )
		selectInput("setAlpha", "", choices=c("alpha = .1","alpha = .05","alpha = .025","alpha = .01","alpha = .001"), selected=".05")
	})

	# information on the prediction, now only showing the time specification
	output$casePred.txt.slider <- renderPrint({
		# validate(
			# need(input$time %in% getAllSelectedPredictors(),'NOTE: no time is specified')
		# )
		cat(paste("predictions for time = ",input$effectPlotSlider))
	})
	# slider to select time of treatment (used as input in info, prediction and plots)
	output$casePred.slider.timePredict <- renderUI({
		sliderInput("effectPlotSlider", label = h4("time specific prediction"), min = 0, max = getTimeRange(), value = 1)
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
	# show the download link for data table
	output$casePred.link.downloadEffectSizes <- renderUI({
		downloadLink('downloadCaseEffectSizes', 'Download Prediction Results in Tab Delimited Text File Format')
	})
	# download the selected data (used in case.link.downloadEstimates)
	output$downloadCaseEffectSizes <- downloadHandler(
		filename = function() {
			paste('casePrediction_', format(Sys.time(), "%d-%b_%Hh%M"), '.txt', sep='')
		},
		content = function(file) {
			write.table(getCaseEffectSize(), file, sep="\t", row.names=F)
		}
	)

	# generate predictions
	lmES_pred <- function(..case, alpha, TaT){
		case_lm <- ..case$lm
		case_data <- ..case$data
		..prd <- names(coef(case_lm))
		if(substring(input$model,7,7)=="3"){
			..a=as.numeric(coefficients(case_lm)[..prd[2]])
			..b=as.numeric(coefficients(case_lm)[..prd[5]]*TaT)
			..c=as.numeric(coefficients(case_lm)[..prd[6]]*TaT^2)
			es_hat <- as.numeric(..a+..b+..c)
			t = qt(1-alpha/2, nrow(case_data) - 2)
			V = vcov(case_lm)
			se = as.numeric(sqrt(V[..prd[2],..prd[2]] + V[..prd[5],..prd[5]]*TaT^2 + V[..prd[6],..prd[6]]*TaT^4
							 + V[..prd[2],..prd[5]]*TaT + V[..prd[2],..prd[6]]*TaT^2 + V[..prd[6],..prd[5]]*TaT^3))
			es_lwr = es_hat - t*se
			es_upr = es_hat + t*se
		}
		if(substring(input$model,7,7)=="2"){
			..a=as.numeric(coefficients(case_lm)[..prd[2]])
			..b=as.numeric(coefficients(case_lm)[..prd[4]]*TaT)
			es_hat <- as.numeric(..a+..b)
			t = qt(1-alpha/2, nrow(case_data) - 2)
			V = vcov(case_lm)
			se = as.numeric(sqrt(V[..prd[2],..prd[2]] + V[..prd[4],..prd[4]]*TaT^2 + V[..prd[4],..prd[2]]*TaT))
			es_lwr = es_hat - t*se
			es_upr = es_hat + t*se
			}
		data.frame(time=TaT, es_hat = es_hat, es_lwr = es_lwr, es_upr = es_upr)
	}
	
	# calculate effect sizes per time point
	getCaseEffectSize <- reactive({
		req(substring(input$model,7,7) %in% c("3","2"))
		..lmL <- getCaseRegressionList()
		# ..lmL <- ..lmL[input$subSelectCases4Prediction]
		setAlpha <- as.numeric(substring(input$setAlpha,9,15))
		..lmES <- ldply(..lmL, function(.x) lmES_pred(.x, setAlpha, input$effectPlotSlider))
		..lmES
	})
	# show the effect sizes
	output$casePred.table.effectsizes <- renderTable({
		# getCaseRegressionPreds()
		getCaseEffectSize()
	})
	
	## >> show CASE PLOTS
	
	# calculate effect sizes for a range of time points
	getCaseEffectSizes <- reactive({
		req(substring(input$model,7,7) %in% c("3","2"))
		..lmL <- getCaseRegressionList()
		# ..lmL <- ..lmL[input$subSelectCases4Prediction]
		setAlpha <- as.numeric(substring(input$setAlpha,9,15))
		..lmESs <- ldply(..lmL, function(.x) lmES_pred(.x, setAlpha, seq(input$minTime, input$maxTime, length=input$stepSize)))
		..lmESlist <- dlply(..lmESs,input$cases)
		..lmESlist
	})
	
	# plot holder
	output$casePred.plots <- renderUI({
		# plot_output_list <- lapply(paste0("prd_,",input$subSelectCases4Prediction), function(i) {
		plot_output_list <- lapply(input$subSelectCases4Prediction, function(i) {
			plotOutput(i, height = 300, width = 450)
		}) 
	})
	
	# function to make plots for each case in the list, for effect sizes over time
	makeESplots <- function(case_es){
		outp <- ggplot(case_es, aes(x = time, y = es_hat)) + geom_ribbon(aes(ymin = es_lwr, ymax = es_upr), alpha = I(0.2)) + geom_line()
		outp <- outp + ylab('Effect size') + xlab('Time after treatment') + ggtitle(paste('Case ',unique(case_es[,input$cases])))
		outp
	}

	# populate plot holder with selected plots (activated when selection buttons are created)
	setPredPlots <- reactive({
		# ..lmL <- getCaseRegressionList()
		..lmESlist <- getCaseEffectSizes()
		#..alpha <- .05
		#..esRibbon <- T
		#..esPlots <- llply(..lmL, function(.x) lmES_plot(.x, ..alpha, ..esRibbon))
		..esPlots <- llply(..lmESlist, function(.x) makeESplots(.x))
		# names(..esPlots) <- paste0("prd_",names(..esPlots))
		for (i in 1:length(..esPlots)) {
			local({
				my_i <- i
				plotname <- names(..esPlots)[my_i]
				output[[plotname]] <- renderPlot({
					..esPlots[[my_i]]
				})
			})
		}
		..esPlots
	})  

	# request design plots
	output$casePred.check.showCasePlots <- renderUI({
		checkboxInput("showCasePredPlots","show case plots (may be time consuming)", FALSE)
	})	
	# request the download link for case plots
	output$casePred.link.showDownloadCasePlots <- renderUI({
		if(input$showCasePredPlots){ setPredPlots() }
		validate(need(input$showCasePredPlots,"download link for case plots shown only on request"))
		if(input$showCasePredPlots){
			downloadLink('downloadCasePredPlots', 'Download Case Plot in Png Format [Only downloads first]')
		} 
	})
	# download the selected design plots (used in showDownloadDesignPlots)
	output$downloadCasePredPlots <- downloadHandler(
		filename = function() { 
			paste('casePredPlot_',format(Sys.time(), "%d-%b_%Hh%M"), '.png', sep='')
		},
		content = function(file) {
			..plotList <- setPredPlots()
			device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
			# for(it in 1:length(..plotList)){
				ggsave(file, plot = ..plotList[[1]], device = device)
			# }
			# ggsave(file, plot = setPlots()[[1]], device = device)
			# mapply(ggsave, file=paste0("plot-", names(..plotList), ".pdf"), plot=..plotList)
		}
	)
	
## end CASE ANALYSIS, predictions <<

	
## META ANALYSIS ##


	# when data are selected, determine the R model for multilevel regression
	getMetaModel <- reactive({
		# if file is uploaded
		validate( need(input$file, '\nmodel specification requires data') )
		if(length(getAllSelectedPredictors())==0) ..mdl <- paste(input$response,"~ 1")
		else ..mdl <- paste(input$response, "~", paste(getAllSelectedPredictors(),collapse=" + "))
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

	# show the selected model
	output$meta.txt.formula <- renderPrint({
		cat(getMetaModel())
	})
	# show the selected model
	output$meta.txt.info <- renderPrint({
		cat("The fixed effects indicate the population average\nThe random effects are estimated to incorporate the case/study specific deviations.\n")
	})
	# button to calculate the linear mixed model
	getResultsMeta <- eventReactive(
		input$runMeta, {
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
	output$meta.txt.note <- renderPrint({
		validate(      
			need(input$runMeta, 'the << RUN >> button is not pressed yet')
		)
		summary(getResultsMeta())
	})
	
	
	####################################################################################################
# browser() to interupt
# display is showcase

	################################################################################################
	# NOT INCLUDED
		
	#? replaced with LIES code :: calculate predictions
	getCaseRegressionPreds <- reactive({
		validate(
			need(input$effectPlotSlider,'something wrong with slider')
		)
		..lm <- getCaseRegressionList()
		..lm <- ..lm[input$subSelectCases4Estimation]
		..new <- data.frame("1",input$effectPlotSlider)
		names(..new) <- c(input$treatment,input$time)
		..prd <- lapply(..lm,function(.x) round(data.frame(predict(.x$lm, ..new, interval="predict",se.fit=T) ),3))
		..prd <- do.call(rbind,..prd) #[,1:5]
		names(..prd) <- c("fit","lower","upper","se","df","resid.")
		..prd 
	})	
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
		..dta <- ddply(..dta,input$cases,.fun = function(.x,mytreat) mutate(.x, ..csm=c(0,cumsum(.x[,mytreat][-length(.x[,mytreat])]!=.x[,mytreat][-1]))), mytreat=input$treatment)
		..dta$..csm <- factor(..dta$..csm)
		..formula <- getCasesModel()
		..formula <- gsub(input$treatment,"..csm" , ..formula)
		..lmb <- ddply(..dta,input$cases,.fun = function(.x,.fm) addLmResults(.x,.fm),.fm=..formula)
		..lmb
	})	

	#? replaced with LIES code :: show the predictions
	output$test.table <- renderTable({
		getCaseRegressionPreds()
	})
	# show lm case plots
	output$test.check.show <- renderUI({
		checkboxInput("showLmCasePlots","show casewise regression plots (downloadable)", FALSE)
	})	
	# show the download link for casewise regression plots
	output$test.link.download <- renderUI({
		validate(need(input$showLmCasePlots,"download link for casewise regression plots shown only on request"))
		if(input$showLmCasePlots) downloadLink('testDownload', 'Download Design Plots in Png Format')
	})
	# download the selected lm case plots (used in test.link.download)
	output$testDownload <- downloadHandler(
		filename = function() { 
			paste('lmCasePlot_', format(Sys.time(), "%d-%b_%Hh%M"), '.png', sep='')
		},
		content = function(file) {
			device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
			ggsave(file, plot = getLmCasePlots(), device = device)
		}
	)
	# show slider to specify the plot height (used in getHeightFromSiderDesign)
	output$test.slider <- renderUI({
		validate(need(input$showLmCasePlots,"casewise regression plots shown only on request"))
		if(input$showLmCasePlots) sliderInput("testSlider", label = h4("set height"), min = 0, max = 100, value = 50)
	})
	# get height from slider for the lm case plots (used in data.plot.caseDesign)
	testHeightSlider <- function() {
		validate(need(input$testSlider,"note: generating many plots can be time consuming"))
		input$testSlider * 20
	}	
	# render the lm case plots 
	output$test.plotset <- renderPlot(height=testHeightSlider,{
		if(input$showLmCasePlots) testPlotset()
	})
	# create the lm case plots (used in testDownload / data.plot.caseDesign)
	testPlotset <- function(){
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

	# information on the prediction, now only showing the time specification
	# output$plotCaseInfo <- renderPrint({
		# cat(paste("select the plot height",input$sliderCasePlot,"%"))
	# })
	getHeightFromSlider <- function() {
		input$sliderCasePlot * 20
	}
	output$test.plot <- renderPlot(height=getHeightFromSlider,{		
		# ..lmX <- getCaseRegressionList()
		# ..lmPlots <- llply(..lmX, function(.x) plot_case_1LA(.x, prediction, confidence))
		# ..lmPlots[[1]]
		..lmESlist <- getCaseEffectSizes()
		makePlots(..lmESlist[[1]])
	})
	output$test.text <- renderPrint({		paste0(input$tabset,":",input$test)		})

})