# install required packages not yet installed
pckgs <- c('shiny','reshape','ggplot2','plyr','psych','arm')
todo <- pckgs[!is.element(pckgs, names(installed.packages()[,"Package"]))]
if(length(todo) > 0) install.packages(todo)#,repos="http://cran.us.r-project.org")
# load in the packages
library(shiny)
library(ggplot2)  # ggplot2 -> plotting
library(plyr)     # ddply -> data manipulation
library(arm)      # lmer -> multilevel modeling

# server file
shinyServer(function(input,output){
  
  # >> DATA and VARIABLE NAMES
  
  # when data selected, read tab delimited text file
  getDataFile <- reactive({
		if(is.null(input$file)){		NULL		}
		else{
			read.table(file=input$file$datapath,sep="\t",header=T)
		}
	})  
  # when data selected, get variable names from data
	getVarnames<-reactive({
	  validate(	    need(input$file, '')	  )
	  names(getDataFile())
	})
  
  # >> identify Response, Case level, Study level, treatment indicator and time specification
  
  # to select the response
  output$response<-renderUI({
    validate(      need(input$file, '')	  )
    selectInput("response", "Response:", choices=c("---",setdiff(getVarnames(), "---")), selected="y")
  })
  # to select the case
  output$cases<-renderUI({
    validate(      need(input$file, '')	  )
    selectInput("cases", "Cases (factor):", choices=c("---",setdiff(getVarnames(), "---")), selected="case")
  })
  # to select the study
  output$studies<-renderUI({
    validate(      need(input$file, '')	  )
    # get varnames (excluding the response and cases)
    selectInput("studies", "Studies (factor):", choices=c("---",setdiff(getVarnames(), "---")), selected="study")
  })
  # to select the treatment
  output$treatment<-renderUI({
    validate(      need(input$file, '')	  )
    # get varnames (exclusing the response and cases and studies and time)
    selectInput("treatment", "Treatment (factor):", choices=c("---",setdiff(getVarnames(), "---")), selected="treat")
  })
  # to select the time
  output$time<-renderUI({
    validate(      need(input$file, '')	  )
    # get varnames (exclusing the response and cases and studies)
    selectInput("time", "Time (numeric):", choices=c("---",setdiff(getVarnames(), "---")), selected="time")
  })
  
  # to choose to standardize or not
  output$standardized <- renderUI({
    validate(      
      need(input$file, ''),
      need(input$response != "---", '')    
    )
    checkboxInput("standardize", "Standardized", FALSE)
  })
  
  # to choose to center time or not
  output$transformed <- renderUI({
    validate(      
      need(input$file, ''),
      need(input$time != "---", '')    
    )
    checkboxInput("transtime", "Center Time", TRUE)
  })

  # >> SELECT using checkboxes the subset of data, for cases and studies

  # to choose the cases to include
  output$selectedCases <- renderUI({
    validate(      
      need(input$file, ''),
      need(input$cases != "---", '')    
    )
    data <- getDataFile()
    caseNames<-list()
    for(i in unique(data[,input$cases])){
      caseNames[[i]]<-i
    }
    checkboxGroupInput("selectCases", "Cases:", choices=caseNames, selected=caseNames) 
  })
  
  # to choose the studies to include
  output$selectedStudies <- renderUI({
    validate(      
      need(input$file, ''),
      need(input$studies != "---", '')    
    )
    data <- getDataFile()
    studyNames<-list()
    for(i in unique(data[,input$studies])){
      studyNames[[i]]<-i
    }
    checkboxGroupInput("selectStudies", "Studies:", choices=studyNames, selected=studyNames)
  })
  # after case/study selection, determine the subsetted data
  getData <- reactive({
    validate(      need(input$file, '')	  )
    tdta <- getDataFile()
    if(input$cases != "---"){
      case <- input$cases
      tdta <- subset(tdta,case%in%strsplit(input$selectCases," "))
    } 
    if(input$studies != "---"){
      study <- input$studies
      tdta <- subset(tdta,study%in%strsplit(input$selectStudies," "))
    } 
    if(input$transtime & input$time != "---" & input$treatment != "---"){
      tdta <- tdta[order(tdta[,input$time]),]
      tdta <- ddply(tdta,"case",.fun = function(.x,mytime,mytreat) mutate(.x, ..time=.x[,mytime]-1-sum(.x[,mytreat]==unique(.x[,mytreat])[1])), mytime=input$time,mytreat=input$treatment)
      tdta[,input$time] <- tdta$..time
      tdta$..time <- NULL
    }
    tdta[,input$treatment] <- factor(tdta[,input$treatment])
    tdta
  })

  
  # >> determine potential FIXED VARiABLE NAMEs, INTERACTIONS & POWER_2
  
  # when data selected, get relevant variable names
  getFixed<-reactive({
    validate(      need(input$file, '')	  )
    input$vars    
  })
  
  # get names for selected variables in their second order interaction
  getInteractions <- reactive({
    apply(combn(setdiff(getVarnames(),c(input$response,input$cases,input$studies)),2),2,function(.x) paste(.x,collapse=":"))
  })
  # get names for selected variables with power to the second
  getPower2 <- reactive({
    paste0(setdiff(getVarnames(),c(input$response,input$cases,input$studies)),"^2")
  })

  # >> SELECT variables using checkboxes for the fixed variables, and the random variables within either case / study
  
  # fixed variables
  output$predictors<-renderUI({
    validate( 
      need(input$file, ''),
      need(input$response != "---", '')    
    )
    if(input$treatment != "---"){
      varnames<-list()
      for(i in setdiff(getVarnames(), c(input$response,input$cases,input$studies))){
        varnames[[i]]<-i
      }
      for(iti in getInteractions()){
        varnames[[length(varnames)+1]] <- iti  
      }
      for(iti in getPower2()){
        varnames[[length(varnames)+1]] <- iti  
      }      
      checkboxGroupInput("vars", "Fixed:", choices=varnames, selected=c(input$time,input$treatment,paste0(input$time,":",input$treatment)))
    }  
  })
  # random variable, per case
  output$varyPerCase <- renderUI({
    validate(      
      need(input$file, ''),
      need(input$cases != "---", '')    
    )
    if(input$response != "---" & input$time != "---" & input$treatment != "---"){
      varnames<-list()
      for(i in setdiff(getFixed(), c(input$response,input$studies))){
        varnames[[i]] <- i
      }
    }
    selected <- c("intercept",varnames)
    checkboxGroupInput("varyPerCase", "case (random):", choices=selected, selected="intercept")
  })  
  # random variables, per study
  output$varyPerStudy <- renderUI({
    validate(      
      need(input$file, ''),
      need(input$studies != "---", '')    
    )
    if(input$response != "---" & input$time != "---" & input$treatment != "---"){
      varnames<-list()
      for(i in setdiff(getFixed(), c(input$response,input$studies))){
        varnames[[i]] <- i
      }
    }
    selected <- c("intercept",varnames)
    checkboxGroupInput("varyPerStudy", "study (random):", choices=selected, selected="intercept")
  })

  # 2do -> include || into the models
  # lmer(y ~ treat*xcase + (1 + treat | study:case) + (1 + treat || study), data = dta)
  # lmer(y ~ treat + time + treat*time + (1 + treat + time + treat*time || subject), data = dta)
  
  # MODEL for ANALYSIS (per case or within a meta-analyses)
  
  # when data are selected, determine the R model specification for analyses
  getModel <- reactive({
    validate(      
      need(input$file, '\nmodel specification requires data'),
      need(length(input$vars)>0, '\nvariables must be included')
    )
    mymodel <- paste(input$response, "~", paste(input$vars,collapse=" + "))
    # when the case level is specified add it within the random part of the model
    if(input$cases != "---"){
      tmpc <- input$varyPerCase
      tmpc[tmpc=="intercept"] <- "1"
      tmpcases <- input$cases
      # when also the study level is specified, embed the case level within the study level
      if(input$studies != "---"){
        tmpcases <- paste0(input$studies,":",input$cases) 
      }          
      mymodel <- paste(mymodel," + (",paste(tmpc,collapse=" + ")," | ",tmpcases,")",sep="")
    } 
    # when the study level is specified add it within the random part of the model
    if(input$studies != "---"){
      tmps <- input$varyPerStudy
      tmps[tmps=="intercept"] <- "1"
      mymodel <- paste(mymodel," + (",paste(tmps,collapse=" + ")," | ",input$studies,")",sep="")
    } 
    mymodel
  })
  # $ operator is invalid for atomic vectors ??
  getCaseRegressions <- reactive({
    validate(      
      need(input$file, ''),
      need(input$cases != '---', 'case regressions require case identification')
    )
    tdta<-getData()    
    out <- ""
    res <- vector("list",length=length(unique(tdta[,input$cases])))
    cntr <- 0
    for(it in unique(tdta[,input$cases])){
      cntr <- cntr + 1
      tmp <- tdta[tdta[,input$cases]==it,]
      res[[cntr]] <- lm(as.formula(paste(input$response, "~", paste(input$vars,collapse=" + "))),data=tmp)
    }
    names(res) <- unique(tdta[,input$cases])
    res  
  })
  getCaseRegressionsCoef <- reactive({
    res <- getCaseRegressions()
    out <- lapply(res,function(.x) round(coef(summary(.x)),3))
    out
  })
  getCaseRegressionRMSE <- reactive({
    res <- getCaseRegressions()
    #out <- lapply(res,function(.x) summary(.x)[["sigma"]])
    out <- lapply(res,function(.x) sum(resid(.x)^2)/length(resid(.x)))
    out
  })
  getCaseRegressionPreds <- eventReactive(
    input$pred, {
      res <- getCaseRegressions()
      newdata <- data.frame("1",input$effectPlotSlider)
      names(newdata) <- c(input$treatment,input$time)
      out <- lapply(res,function(.x) round(data.frame(predict(.x, newdata, interval="predict",se.fit=T) ),3))
      out <- do.call(rbind,out)[,1:5]
      names(out) <- c("fit","lower","upper","se","df")
      out 
    })
  
 # getCaseRegressionPreds <- reactive({
#  })  
  ####################################################################################################
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(getProcessedData(), file)
    }
  )

  output$predictioninfo <- renderPrint({
    cat(paste("predictions for time = ",input$effectPlotSlider))
  })
  # SPECIFIED MODEL
  
  # show the model
  output$showmodel <- renderPrint({
    cat(getModel())  
  })
  
  # SELECTED DATA
  
  # DATA STRUCTURE
  output$mydatastructure <- renderPrint({
    str(getData())
  })
  # TABLE
  getProcessedData <- reactive({
    if(is.null(input$file)){    modelinfo <- "first load & define the data"		}
    else{
      tdta <- getData()
      if(input$standardize  & (input$response != "---")){
        tmp <- do.call(rbind,lapply(getCaseRegressions(),function(.x) summary(.x)[["sigma"]]))
        tdta[,input$response] <- tdta[,input$response] / as.numeric(tmp[match(tdta[,input$cases],row.names(tmp))])
      }
      tdta    
    }
  })
  output$mydata <- renderDataTable({
    getProcessedData()
  })

  ####################################################################################################

  results <- eventReactive(
    input$r, {
      dta <- getData()
      mdl <- getModel()
      # when the button is pressed !!
      if(input$cases != "---" || input$studies != "---"){
        lmer(as.formula(mdl),data=dta)
      }
      else{
        lm(as.formula(mdl),data=dta)
      }
  })
  output$metaresults <- renderPrint({
    validate(      
      need(input$r, 'the << RUN >> button is not pressed yet')
    )
    summary(results())
  })
  # per case regression, only considering the fixed part
  output$caseresults <- renderPrint({
    getCaseRegressionsCoef()
  })
  
  # CASE PLOTS
  getHeightFromSlider <- function() {
    input$casePlotSlider * 100
  }
  getTimeRange <- function() {
    validate(      
      need(input$file, ''),
      need(input$time != '---', 'time is not specified')
    )
    tdta <- getData()
    range(tdta[,input$time])%*%c(-1,1)
  }
  output$casePlot <- renderPlot(height=getHeightFromSlider,{
    validate(      need(input$file, 'first load & define the data')	  )
    # read in the data and rename variables for use in the plots
    tdta<-getData()
    # plot the case regressions and raw data      
    tdta$sc.treat <- factor(tdta[,input$treatment])
    levels(tdta$sc.treat) <- factor(c(1,2))
    out <- ggplot(data=tdta,aes_string(x=input$time,y=input$response,linetype="sc.treat")) + geom_line() + geom_smooth(method='lm',lwd=1.4) 
    if(input$cases!="---") out <- out + facet_grid(reformulate(".",input$cases)) 
    out <- out + guides(linetype=FALSE) 
    out <- out + scale_x_continuous("time",limits=c(min(tdta[,input$time]), max(tdta[,input$time]))) + scale_y_continuous("response")
    out + ggtitle("responses")
  })
  output$sliderEffectPlot <- renderUI({
    sliderInput("effectPlotSlider", label = h4("select time"), min = 0, max = getTimeRange(), value = 0)
  })
  
  output$effectPlot <- renderPlot(height=getHeightFromSlider,{
    validate(      need(input$file, '')    )
    # read in the data and rename variables for use in the plots
    tdta<-getData()
    tdta <- tdta[tdta$time>=0,]
    # plot the case regressions and raw data      
    tdta$sc.treat <- factor(tdta[,input$treatment])
    levels(tdta$sc.treat) <- factor(c(1,2))
    out <- ggplot(data=tdta,aes_string(x=input$time,y=input$response,linetype="sc.treat")) + geom_line() + geom_smooth(method='lm',lwd=1.4) 
    if(input$cases!="---") out <- out + facet_grid(reformulate(".",input$cases)) 
    out <- out + guides(linetype=FALSE) 
    out <- out + scale_x_continuous("time",limits=c(min(tdta[,input$time]), max(tdta[,input$time]))) + scale_y_continuous("response")
    out <- out + geom_vline(xintercept = input$effectPlotSlider)
    out + ggtitle("effects")
  })
  
  ################################################################################################
  # NOT INCLUDED
  # do.call(rbind,describeBy(dta$y,dta$case))[,-1]
 # cast(aggregate(tdta$y,by=list(case=tdta$case,treat=tdta$treat),mean),case~treat)
  output$predictions <- renderPrint({
    getCaseRegressionPreds()
  })
 output$testing <- renderPrint({
   
 })
  output$scplot<-renderPlot({
  
  })
 


})