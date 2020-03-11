###### Continuous_Bayesian ######

#if packages not installed, please install them first by running the line below.
#install.packages(c("dplyr","metafor", "netmeta","shiny", "shinyAce","rmarkdown", "knitr", "gemtc", "plyr", data.table"
#  , "shinyalert", "plotly"))

library(dplyr)
library(metafor)
library(netmeta)
library(shiny)
library(shinyAce)
library(rmarkdown)
library(knitr)
library(gemtc)
library(plyr)
library(data.table)
library(shinyalert)
library(plotly)


source("PlotFunctionsRKO.R", local = TRUE) # Plot functions
#source("PlotFunctionsContRKO.R", local = TRUE) # Plot fun
load("blank.rds") # Objects to store data for plot functions

shinyServer(function(input, output, session) {

showModal(modalDialog(
   title = "Important message",
    easyClose = FALSE,
    p(tags$strong("In accordance with Data Protection legislation, we would like to inform you of the following before you use our website:
                             "), "We collect your usage data within the MetaInsight app to perform analytics of usage and improve our app. By clicking",
      tags$i(tags$u("I consent")), "below, you consent to the use of data by us through Google Analytics. 
      For details of policy, please check the 'Privacy notice' tab within the app, and ",tags$a(href="https://policies.google.com/privacy?hl=en", "Google Privacy & Terms.",target="_blank") ), 
    br(),
    modalButton("I consent"),
    footer = NULL
  ))
  #############################################
  ############ Continuous Data ################
  #############################################
  
  
  
  ############################################
  ############ Home page - linking pages #####
  ############################################
  
  ### view the full update history
  observeEvent(input$history_click, {
    newvalue <- "history"
    updateNavbarPage(session,"meta", selected="Full update history")
  })

  
  

  #############################################
  ############ Load Data ################
  #############################################
  auditC <- read.csv("./TRY.csv")
  #auditC <- read.csv("C:/Users/yx8w/Desktop/TRY.csv")
  #####
  # Default data
  #####
  defaultData <- reactive({
    auditC
  })
  


  
  
  ############################################
  #####Downloadable csv and labels of example dataset
  ############################################
  datad <- as.data.frame(auditC)
  
##### in the 'upload long data' tab
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("MetaInsightdataLONG","csv", sep = ".")
    },
    content <- function(file){
      file.copy("./TRY.csv", file)
    }
  )
  
  output$downloadlabel <- downloadHandler(
    filename <- function() {
      paste("treatmentlabels","txt", sep = ".")
    },
    content <- function(file){
      file.copy("./defaultlabels.txt", file)
    }
  )
  
##### in the 'UPload wide data' tab
  output$downloadDataWide <- downloadHandler(
    filename <- function() {
      paste("MetaInsightdataWIDE","csv", sep = ".")
    },
    content <- function(file){
      file.copy("./TRY_WIDE.csv", file)
    }
  )
  
  output$downloadlabel2 <- downloadHandler(
    filename <- function() {
      paste("treatmentlabels","txt", sep = ".")
    },
    content <- function(file){
      file.copy("./defaultlabels.txt", file)
    }
  )
  
  
  #####
  # Make data reactive
  #####
  
  # Make the data file that was uploaded reactive by giving it the name file1
  # now I can use file1 to refer to the different parts of the data file
  # Put data into a data frame so it can be used in analysis
  data <- reactive({ 
    file1 <- input$data
    if(is.null(file1)){return(auditC)}
    else
      a <- read.table(file = file1$datapath, sep =",", header=TRUE, stringsAsFactors = FALSE)
    
  })
  
  #####
  # Data analysis tab
  #####
  # Create a table which displays the raw data just uploaded by the user
  output$rawtable <- renderTable({
    if(is.null(data())){return()}
    data()
    
  })
  
  # In the "Load data" tab (created in the UI section) we divide the main panel into mutlipe tabs and add the content
  # we've just created
  # When there is no data loaded display the instructions for how the data should be formatted
  # Once data is oaded display the raw data
  output$tb <- renderUI({
    if(is.null(data())){return("Please select a file to upload.")}
    else
      tableOutput("rawtable")
  })
  
####################################################################################


  
  #########
  ### Get studies for check box input
  #########
  
  output$ChoicesCont2 <- renderUI({
    newData <- data()
    newData1 <- as.data.frame(newData)
    # if the data in long format, then the dataset will contain exactly six columns. 
    # whereas the wide format will contain at least 2+4*2=10 columns.
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
    }
    else {
      data_wide<- newData1
    }
    
    checkboxGroupInput("exclusionCont2",
                       label = NULL,
                       choices = as.character(data_wide$Study))
  })
  
  

  
  ######
  # Grouped Forest Plot (all studies) - Continuous Data
  ######
  
  make_netStudyCont = function() {
    
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
    }
    else {
      # if the data is in wide form, the following code order the treatment in a descending order
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
      # generate additional columns if less than 6 arms for the reshape
      if (numbertreat < 6) {
        for (k in (numbertreat+1):6) {
          data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
        }
      }
      widetolong <- reshape(data_wide, direction = "long",
                           varying = 3:ncol(data_wide), 
                           times=c(".1", ".2", ".3", ".4", ".5", ".6"), sep=".", idvar= c("StudyID", "Study"))
      widetolong<-subset(widetolong, select=-time)
      newData2 <- widetolong[!is.na(widetolong$T), ]
      newData2<-newData2[order(newData2$StudyID, -newData2$T), ]
      
    }
    
    # create counting variable for number of arms within each study.
    newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
    # reshape
    data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
    numbertreat=max(newData2$number)
    
    
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    
    # Get subset of data to use
    data_sub <- filter(data_wide, !Study %in% input$exclusionCont2)

    
    if (input$outcomeCont=="MD"){
      d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="MD")
    }
    else{
      d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="SMD")
    }
    
    data_final<- merge(d1,data_wide,by="StudyID")
    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)
    
    text_label <- character()
    n_stud <- integer()
    
    for (i in 1:ntx) {
      for (j in 1:ntx) {
        if (nrow(d1[(d1$treat1 == i & d1$treat2 == j),]) > 0) {
          text_label <- c(paste(lstx[i], "vs", lstx[j]), text_label)
          n_stud <- c(n_stud, nrow(d1[(d1$treat1 == i & d1$treat2 == j),]))
        }
      }
    }
    
    
    gaps <- integer(length(n_stud))
    n_stud <- rev(n_stud)
    
    for (k in 1:length(n_stud)) {
      
      if (k == 1) {
        gaps[k] <- n_stud[k] + 1
      }
      
      else {
        gaps[k] <- gaps[k-1] + n_stud[k] + 2
      }
    }
    
    lines <- rev(c(1:(nrow(d1) + 2*length(text_label)-1)))
    lines <- lines[!lines %in% gaps]
    lines <- lines[!lines %in% (gaps+1)]
    
    forest(d1$TE, d1$seTE, slab = paste(data_final$Study.y), subset = order(d1$treat1, d1$treat2), ylim = c(1, nrow(d1) + 2*length(text_label) + 2), rows=lines)
    
    text(-6, gaps, pos=4, font = 4, text_label)
    
    title("Individual study results (for all studies) grouped by treatment comparison")
    
  }
  
  
  output$forestPlotCont <- renderPlot({
    
    withProgress(message = 'Loading', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste(""))
        
      }
    })
    
    make_netStudyCont()
    
  })
  
  output$downloadStudyCont <- downloadHandler(
    filename = function() {
      paste0('StudyResults.', input$format_freq0)
    },
    
    content = function(file) {
      if (input$format_freq0=="PDF"){pdf(file=file)}
      else {svg(file=file)}
      make_netStudyCont()
      dev.off()
    }
  )

  ##########
  ### Network Plot static - continuous
  ##########

  make_netgraphCont = function() {

    #### same codes for inputting the data
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
      numbertreat=max(newData2$number)
    }
    else {
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
    }
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    #####
    
    ## for network plot, it does not matter whether it is MD or MSD.
    d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="MD")
    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)

    # Static NMA of all studies

    
    d1$treat1 <- factor(d1$treat1,
                        levels = c(1:ntx),
                        labels = as.character(treat_list$Label))
    
    d1$treat2 <- factor(d1$treat2,
                        levels = c(1:ntx),
                        labels = as.character(treat_list$Label))
    
    
    
    netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                         sm = "MD", level=0.95, level.comb=0.95,
                         comb.random=TRUE, reference.group="",
                         all.treatments=NULL, seq=NULL, tau.preset=NULL,
                         tol.multiarm = 0.05, warn=TRUE)

    netgraph(netStatic, lwd=2, number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=1.25, cex.points=2, col.points=1, col=8, pos.number.of.studies=0.43)

    title("Network plot of all studies")
  }

  output$netGraphStaticCont <- renderPlot ({

    make_netgraphCont()
  }, height="auto", width = "auto")

  output$downloadNetworkCont <- downloadHandler(
    filename = function() {
      paste0('Network.', input$format_freq1)
    },

    content = function(file) {
      if (input$format_freq1=="PDF"){pdf(file=file)}
      else {png(file=file)}
      make_netgraphCont()
      dev.off()
    }
  )

  ##########
  ### Network Plot - sensitivity analysis
  ##########

  make_netgraphUpdateCont = function(){

    #### same codes for inputting the data
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
      numbertreat=max(newData2$number)
    }
    else {
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
    }
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    #####
    
    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)

    # Get subset of data to use
    data_sub <- filter(data_wide, !Study %in% input$exclusionCont2)
    d1_sub <- d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="MD")
    
    # Network meta-analysis

    d1_sub$treat1 <- factor(d1_sub$treat1,
                            levels = c(1:ntx),
                            labels = as.character(treat_list$Label))
    
    d1_sub$treat2 <- factor(d1_sub$treat2,
                            levels = c(1:ntx),
                            labels = as.character(treat_list$Label))
    
    
    net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                    sm = "MD", level=0.95, level.comb=0.95,
                    comb.random=TRUE, reference.group="",
                    all.treatments=NULL, seq=NULL, tau.preset=NULL,
                    tol.multiarm = 0.05, warn=TRUE)

    netgraph(net1, lwd=2, number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=1.25, cex.points=2, col.points=1, col=8, pos.number.of.studies=0.43)

    title("Network plot with studies excluded")

  }

  output$netGraphUpdatingCont <- renderPlot({

    withProgress(message = 'Loading', value = 0, {
      # Number of times we'll go through the loop
      n <- 10

      for (i in 1:n) {

        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste(""))

      }
    })

    make_netgraphUpdateCont()

  })

  output$downloadNetworkUpdateCont <- downloadHandler(
    filename = function() {
      paste0('NetworkUpdate.', input$format_freq2)
    },

    content = function(file) {
      if (input$format_freq2=="PDF"){pdf(file=file)}
      else {png(file=file)}
      make_netgraphUpdateCont()
      dev.off()
    }
  )


  ##########
  ### Comparison and rank table - Continuous
  ##########
  make_netrankCont = function() {
    
    #### same codes for inputting the data
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
      numbertreat=max(newData2$number)
    }
    else {
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
    }
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    #####
    
    
    if (input$outcomeCont=="MD"){
    d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="MD")
    }
    else{
    d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="SMD")
    }
    
    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)
    
    d1$treat1 <- factor(d1$treat1,
                            levels = c(1:ntx),
                            labels = as.character(treat_list$Label))
    
    d1$treat2 <- factor(d1$treat2,
                            levels = c(1:ntx),
                            labels = as.character(treat_list$Label))

    # Static NMA of all studies
    if (input$modelCont=="RE" & input$outcomeCont=="MD"){
      netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                           sm = "MD", level=0.95, level.comb=0.95,
                           comb.random=TRUE, reference.group="",
                           all.treatments=NULL, seq=NULL, tau.preset=NULL,
                           tol.multiarm = 0.05, warn=TRUE)
    }

    else if (input$modelCont=="RE" & input$outcomeCont=="SMD"){
      netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                           sm = "SMD", level=0.95, level.comb=0.95,
                           comb.random=TRUE, reference.group="",
                           all.treatments=NULL, seq=NULL, tau.preset=NULL,
                           tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
    }
    else if (input$modelCont=="FE" & input$outcomeCont=="MD"){
      netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                           sm = "MD", level=0.95, level.comb=0.95,
                           comb.fixed=TRUE, reference.group="",
                           all.treatments=NULL, seq=NULL, tau.preset=NULL,
                           tol.multiarm = 0.05, warn=TRUE)
    }

    else {
      netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                           sm = "SMD", level=0.95, level.comb=0.95,
                           comb.fixed=TRUE, reference.group="",
                           all.treatments=NULL, seq=NULL, tau.preset=NULL,
                           tol.multiarm = 0.05,tol.multiarm.se = 0.15, warn=TRUE)
    }

    if (input$modelCont=="RE" & input$rankoptsCont=="good"){
      league <- netleague(netStatic, comb.random=TRUE, digits =2, seq= netrank(netStatic, small = "good"))
      leaguedf<- as.data.frame(league$random)
      }
    else if (input$modelCont=="FE" & input$rankoptsCont=="good"){
      league <- netleague(netStatic, comb.random=FALSE, digits =2, seq= netrank(netStatic, small = "good"))
      leaguedf<- as.data.frame(league$fixed)
      }
    
    else if (input$modelCont=="RE" & input$rankoptsCont=="bad"){
      league <- netleague(netStatic, comb.random=TRUE, digits =2, seq= netrank(netStatic, small = "bad"))    
      leaguedf<- as.data.frame(league$random)
      }
    
    else {
      league <- netleague(netStatic, comb.random=FALSE, digits =2, seq= netrank(netStatic, small = "bad"))      
      leaguedf<- as.data.frame(league$fixed)
    }
    leaguedf
  }
  
  output$rankChartStaticCont<- renderTable(colnames=FALSE,{
    
    withProgress(message = 'Loading', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste(""))
        
      }
    })
    
    make_netrankCont()
  })
  
  output$downloadRankCont <- downloadHandler(
    filename = function() {
      paste('Rank.', '.csv', sep='')
    },

    content = function(file) {
      write.csv({make_netrankCont()}, file)
    }
  )

  ##########
  ### Comparison and rank table updating
  ##########

  make_netrankUpdateCont = function(){

    #### same codes for inputting the data
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
      numbertreat=max(newData2$number)
    }
    else {
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
    }
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    #####
    
    
    d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="MD")
    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)
    
    
    # Get subset of data to use
    data_sub <- filter(data_wide, !Study %in% input$exclusionCont2)
    if (input$outcomeCont=="MD"){
      d1_sub<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="MD")
    }
    else{
      d1_sub<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="SMD")
    }
    
    d1_sub$treat1 <- factor(d1_sub$treat1,
                        levels = c(1:ntx),
                        labels = as.character(treat_list$Label))
    
    d1_sub$treat2 <- factor(d1_sub$treat2,
                        levels = c(1:ntx),
                        labels = as.character(treat_list$Label))

    # Network meta-analysis
    if (input$modelCont=="RE" & input$outcomeCont=="MD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "MD", level=0.95, level.comb=0.95,
                      comb.random=TRUE, reference.group="",
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, warn=TRUE)
    }

    else if (input$modelCont=="RE" & input$outcomeCont=="SMD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "SMD", level=0.95, level.comb=0.95,
                      comb.random=TRUE, reference.group="",
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
    }
    else if (input$modelCont=="FE" & input$outcomeCont=="MD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "MD", level=0.95, level.comb=0.95,
                      comb.fixed=TRUE, reference.group="",
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, warn=TRUE)
    }

    else {
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "SMD", level=0.95, level.comb=0.95,
                      comb.fixed=TRUE, reference.group="",
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
    }

    if (input$modelCont=="RE" & input$rankoptsCont=="good"){
      league1 <- netleague(net1, comb.random=TRUE, digits =2, seq= netrank(net1, small = "good"))
      leaguedf1<- as.data.frame(league1$random)
    }
    else if (input$modelCont=="FE" & input$rankoptsCont=="good"){
      league1 <- netleague(net1, comb.random=FALSE, digits =2, seq= netrank(net1, small = "good"))
      leaguedf1<- as.data.frame(league1$fixed)
    }
    
    else if (input$modelCont=="RE" & input$rankoptsCont=="bad"){
      league1 <- netleague(net1, comb.random=TRUE, digits =2, seq= netrank(net1, small = "bad"))    
      leaguedf1<- as.data.frame(league1$random)
    }
    
    else {
      league1 <- netleague(net1, comb.random=FALSE, digits =2, seq= netrank(net1, small = "bad"))      
      leaguedf1<- as.data.frame(league1$fixed)
    }
    leaguedf1
  }
  
  output$rankChartUpdatingCont<- renderTable(colnames=FALSE,{
    
    withProgress(message = 'Loading', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste(""))
        
      }
    })
    
    make_netrankUpdateCont()
  })
  
  output$downloadRankUpdateCont <- downloadHandler(
    filename = function() {
      paste('RankUpdate.', '.csv', sep='')
    },
    
    content = function(file) {
      write.csv({make_netrankUpdateCont()}, file)
    }
  )


  ################
  ### Forest Plot - Updating ##
  ################

  make_netCompCont = function(){
   
    
   #### same codes for inputting the data
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
      numbertreat=max(newData2$number)
    }
    else {
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
    }
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    #####


    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)

    # Get subset of data to use
    data_sub <- filter(data_wide, !Study %in% input$exclusionCont2)
    if (input$outcomeCont=="MD"){
      d1_sub<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="MD")
    }
    else{
      d1_sub<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="SMD")
    }

      d1_sub$treat1 <- factor(d1_sub$treat1,
                              levels = c(1:ntx),
                              labels = as.character(treat_list$Label))

      d1_sub$treat2 <- factor(d1_sub$treat2,
                              levels = c(1:ntx),
                              labels = as.character(treat_list$Label))


    # Network meta-analysis for subgroup

      if (input$modelCont=="RE" & input$outcomeCont=="MD"){
        net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                        sm = "MD", level=0.95, level.comb=0.95,
                        comb.random=TRUE, reference.group =as.character(lstx[1]),
                        all.treatments=NULL, seq=NULL, tau.preset=NULL,
                        tol.multiarm = 0.05, warn=TRUE)
        forest(net1, reference.group=as.character(lstx[1]), pooled="random")
      }
      else if (input$modelCont=="RE" & input$outcomeCont=="SMD"){
        net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                        sm = "SMD", level=0.95, level.comb=0.95,
                        comb.random=TRUE, reference.group =as.character(lstx[1]),
                        all.treatments=NULL, seq=NULL, tau.preset=NULL,
                        tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
        forest(net1, reference.group=as.character(lstx[1]), pooled="random")
      }
      else if (input$modelCont=="FE" & input$outcomeCont=="MD"){
        net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                        sm = "MD", level=0.95, level.comb=0.95,
                        comb.fixed=TRUE, reference.group =as.character(lstx[1]),
                        all.treatments=NULL, seq=NULL, tau.preset=NULL,
                        tol.multiarm = 0.05, warn=TRUE)
        forest(net1, reference.group=as.character(lstx[1]), pooled="fixed")
      }
      else {
        net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                        sm = "SMD", level=0.95, level.comb=0.95,
                        comb.fixed=TRUE, reference.group =as.character(lstx[1]),
                        all.treatments=NULL, seq=NULL, tau.preset=NULL,
                        tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
        forest(net1, reference.group=as.character(lstx[1]), pooled="fixed")
    }

      output$refcont3<- renderText({"All outcomes are versus the reference treatment (treatment labelled 1)"})
  }

  output$SFPUpdatingContComp <- renderPlot({

     withProgress(message = 'Loading', value = 0, {
      # Number of times we'll go through the loop
      n <- 10

      for (i in 1:n) {

        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste(""))

      }
    })

    make_netCompCont()
    title("Results with studies excluded")
  })
  
  output$downloadCompCont<- downloadHandler(
    filename = function() {
      paste0('Excluded_studies.', input$format_freq4)
    },
    content = function(file) {
      if (input$format_freq4=="PDF"){pdf(file=file)}
      else {png(file=file)}
      make_netCompCont()
      dev.off()
    }
  )

  ###### text under the forest plot - updating


  textunder=function() {
    
    #### same codes for inputting the data
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
      numbertreat=max(newData2$number)
    }
    else {
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
    }
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    #####
    
    
    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)
    

    # Get subset of data to use
    data_sub <- filter(data_wide, !Study %in% input$exclusionCont2)
    if (input$outcomeCont=="MD"){
      d1_sub<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="MD")
    }
    else{
      d1_sub<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="SMD")
    }

    d1_sub$treat1 <- factor(d1_sub$treat1,
                            levels = c(1:ntx),
                            labels = as.character(treat_list$Label))

    d1_sub$treat2 <- factor(d1_sub$treat2,
                            levels = c(1:ntx),
                            labels = as.character(treat_list$Label))

    # Network meta-analysis
    if (input$modelCont=="RE" & input$outcomeCont=="MD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "MD", level=0.95, level.comb=0.95,
                      comb.random=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, warn=TRUE)
    }
    else if (input$modelCont=="RE" & input$outcomeCont=="SMD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "SMD", level=0.95, level.comb=0.95,
                      comb.random=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
    }

    else if (input$modelCont=="FE" & input$outcomeCont=="MD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "MD", level=0.95, level.comb=0.95,
                      comb.fixed=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, warn=TRUE)
    }

    else {
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "SMD", level=0.95, level.comb=0.95,
                      comb.fixed=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
    }
    tau<- round(net1$tau,2)
    list(tau=tau, net1_k=net1$k, net1_n=net1$n )

    
  }

output$textcont5 <-renderText({
  textunder_list=textunder()
  tau=textunder_list$tau
  net1_k=textunder_list$net1_k
  net1_n=textunder_list$net1_n
    if (input$modelCont=="RE"){
      paste("Between-study standard deviation:", tau,
            ", Number of studies:", net1_k,
            ", Number of treatments:", net1_n)}
    else{paste("Between-study standard deviation set at 0. Number of studies:", net1_k,
               ", Number of treatments:", net1_n)}
  }
  )   
  


  ##########
  ### Forest Plot - static
  ##########
  make_netCompCont2 = function() {
   
    #### same codes for inputting the data
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
      numbertreat=max(newData2$number)
    }
    else {
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
    }
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    #####
    

    if (input$outcomeCont=="MD"){
      d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="MD")
    }
    else{
      d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="SMD")
    }
    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)


      d1$treat1 <- factor(d1$treat1,
                          levels = c(1:ntx),
                          labels = as.character(treat_list$Label))

      d1$treat2 <- factor(d1$treat2,
                          levels = c(1:ntx),
                          labels = as.character(treat_list$Label))


      # Static NMA of all studies
      if (input$modelCont=="RE" & input$outcomeCont=="MD"){
        netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                             sm = "MD", level=0.95, level.comb=0.95,
                             comb.random=TRUE, reference.group=as.character(lstx[1]),
                             all.treatments=NULL, seq=NULL, tau.preset=NULL,
                             tol.multiarm = 0.05, warn=TRUE)
        forest(netStatic, reference.group=as.character(lstx[1]), pooled="random")
        }

      else if (input$modelCont=="RE" & input$outcomeCont=="SMD"){
        netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                             sm = "SMD", level=0.95, level.comb=0.95,
                             comb.random=TRUE, reference.group=as.character(lstx[1]),
                             all.treatments=NULL, seq=NULL, tau.preset=NULL,
                             tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
        forest(netStatic, reference.group=as.character(lstx[1]), pooled="random")
      }
      else if (input$modelCont=="FE" & input$outcomeCont=="MD"){
        netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                        sm = "MD", level=0.95, level.comb=0.95,
                        comb.fixed=TRUE, reference.group =as.character(lstx[1]),
                        all.treatments=NULL, seq=NULL, tau.preset=NULL,
                        tol.multiarm = 0.05, warn=TRUE)
        forest(netStatic, reference.group=as.character(lstx[1]), pooled="fixed")
      }
      
      else {
        netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                        sm = "SMD", level=0.95, level.comb=0.95,
                        comb.fixed=TRUE, reference.group =as.character(lstx[1]),
                        all.treatments=NULL, seq=NULL, tau.preset=NULL,
                        tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
        forest(netStatic, reference.group=as.character(lstx[1]), pooled="fixed")
      }

     output$refcont4<- renderText({"All outcomes are versus the reference treatment (treatment labelled 1)"})

    }


  output$ComparisonCont2<- renderPlot({
    make_netCompCont2()
    title("Results for all studies")
  })


  ######## Forest plot - text under the plot
  
  output$textcompcont<- renderText({
   
    #### same codes for inputting the data
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
      numbertreat=max(newData2$number)
    }
    else {
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
    }
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    #####
    
    
    if (input$outcomeCont=="MD"){
      d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="MD")
    }
    else{
      d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="SMD")
    }
    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)
    

  d1$treat1 <- factor(d1$treat1,
                      levels = c(1:ntx),
                      labels = as.character(treat_list$Label))


  d1$treat2 <- factor(d1$treat2,
                      levels = c(1:ntx),
                      labels = as.character(treat_list$Label))

  # Static NMA of all studies
  if (input$modelCont=="RE" & input$outcomeCont=="MD"){
    netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                         sm = "MD", level=0.95, level.comb=0.95,
                         comb.random=TRUE, reference.group="",
                         all.treatments=NULL, seq=NULL, tau.preset=NULL,
                         tol.multiarm = 0.05, warn=TRUE)
  }

  else if (input$modelCont=="RE" & input$outcomeCont=="SMD"){
    netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                         sm = "SMD", level=0.95, level.comb=0.95,
                         comb.random=TRUE, reference.group="",
                         all.treatments=NULL, seq=NULL, tau.preset=NULL,
                         tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
  }
  else if (input$modelCont=="FE" & input$outcomeCont=="MD"){
    netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                         sm = "MD", level=0.95, level.comb=0.95,
                         comb.fixed=TRUE, reference.group="",
                         all.treatments=NULL, seq=NULL, tau.preset=NULL,
                         tol.multiarm = 0.05, warn=TRUE)
  }

  else {
    netStatic <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                         sm = "SMD", level=0.95, level.comb=0.95,
                         comb.fixed=TRUE, reference.group="",
                         all.treatments=NULL,  seq=NULL, tau.preset=NULL,
                         tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
  }
  tau<- round(netStatic$tau,2)
  if (input$modelCont=="RE"){
    paste("Between-study standard deviation:", tau,
          ", Number of studies:", netStatic$k,
          ", Number of treatments:", netStatic$n)}
  else{paste("Between-study standard deviation set at 0. Number of studies:", netStatic$k,
             ", Number of treatments:", netStatic$n)}
  })

  output$downloadCompCont2 <- downloadHandler(
    filename = function() {
      paste0('All_studies.', input$format_freq3)
    },

    content = function(file) {
      if (input$format_freq3=="PDF"){pdf(file=file)}
      else {png(file=file)}
      make_netCompCont2()
      dev.off()
    }
  )


  
  
  ############################
  ###### Inconsistency - all studies #######
  ############################
  make_InconCont = function() {
    
    #### same codes for inputting the data
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
      numbertreat=max(newData2$number)
    }
    else {
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
    }
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    #####
    

    if (input$outcomeCont=="MD"){
      d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="MD")
    }
    else{
      d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="SMD")
    }
    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)
    
    
    
    d1$treat1 <- factor(d1$treat1,
                            levels = c(1:ntx),
                            labels = as.character(treat_list$Label))
    
    d1$treat2 <- factor(d1$treat2,
                            levels = c(1:ntx),
                            labels = as.character(treat_list$Label))
    
    # Network meta-analysis
    if (input$modelCont=="RE" & input$outcomeCont=="MD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                      sm = "MD", level=0.95, level.comb=0.95,
                      comb.random=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, warn=TRUE)
    }
    else if (input$modelCont=="RE" & input$outcomeCont=="SMD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                      sm = "SMD", level=0.95, level.comb=0.95,
                      comb.random=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
    }
    
    else if (input$modelCont=="FE" & input$outcomeCont=="MD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                      sm = "MD", level=0.95, level.comb=0.95,
                      comb.fixed=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, warn=TRUE)
    }
    
    else {
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1, subset=NULL,
                      sm = "SMD", level=0.95, level.comb=0.95,
                      comb.fixed=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
    }
    
    incona<- netsplit(net1)
    Comparison<- incona$comparison
    No.Studies<- as.integer(incona$k)
    
    if (input$modelCont=="RE"){
      Direct<- incona$direct.random$TE
      Indirect<- incona$indirect.random$TE
      Difference<- incona$compare.random$TE
      Diff_95CI_lower<- incona$compare.random$lower
      Diff_95CI_upper<- incona$compare.random$upper
      NMA<- incona$random$TE
      pValue<- incona$compare.random$p}
    else{
      Direct<- incona$direct.fixed$TE
      Indirect<- incona$indirect.fixed$TE
      Difference<- incona$compare.fixed$TE
      Diff_95CI_lower<- incona$compare.fixed$lower
      Diff_95CI_upper<- incona$compare.fixed$upper
      NMA<- incona$fixed$TE
      pValue<- incona$compare.fixed$p}
    df<- data.frame(Comparison, No.Studies, NMA, Direct, Indirect, Difference, Diff_95CI_lower, Diff_95CI_upper, pValue)
  }
  
  output$Incon1<- renderTable(colnames=TRUE, {
    make_InconCont()}
  )
  
  output$downloadInconCont <- downloadHandler(
    filename = function() {
      paste('Inconsistency.', '.csv', sep='')
    },
    
    content = function(file) {
      write.csv({make_InconCont()}, file)
    }
  )
  
  
  
  
  
  ############################
  ###### Inconsistency - updating #######
  ############################
  make_InconCont_sub =   function(){
    
    #### same codes for inputting the data
    newData <- data()
    newData1 <- as.data.frame(newData)
    
    if (ncol(newData1)==6){
      newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
      # create counting variable for number of arms within each study.
      newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
      # reshape
      data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
      numbertreat=max(newData2$number)
    }
    else {
      data_wide<- newData1
      numbertreat=(ncol(newData1)-2)/4
    }
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    #####
    
    # Get subset of data to use
    data_sub <- filter(data_wide, !Study %in% input$exclusionCont2)
    
    if (input$outcomeCont=="MD"){
      d1_sub<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="MD")
    }
    else{
      d1_sub<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="SMD")
    }
    treat_list <- read.csv(text=input$listCont2, sep = "\t")
    lstx <- treat_list$Label
    ntx <- length(lstx)
    
    
    
    d1_sub$treat1 <- factor(d1_sub$treat1,
                            levels = c(1:ntx),
                            labels = as.character(treat_list$Label))
    
    d1_sub$treat2 <- factor(d1_sub$treat2,
                            levels = c(1:ntx),
                            labels = as.character(treat_list$Label))
    
    # Network meta-analysis
    if (input$modelCont=="RE" & input$outcomeCont=="MD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "MD", level=0.95, level.comb=0.95,
                      comb.random=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, warn=TRUE)
    }
    else if (input$modelCont=="RE" & input$outcomeCont=="SMD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "SMD", level=0.95, level.comb=0.95,
                      comb.random=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, tol.multiarm.se = 0.15, warn=TRUE)
    }
    
    else if (input$modelCont=="FE" & input$outcomeCont=="MD"){
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "MD", level=0.95, level.comb=0.95,
                      comb.fixed=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05,  warn=TRUE)
    }
    
    else {
      net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = d1_sub, subset=NULL,
                      sm = "SMD", level=0.95, level.comb=0.95,
                      comb.fixed=TRUE, reference.group =as.character(lstx[1]),
                      all.treatments=NULL, tol.multiarm.se = 0.15, seq=NULL, tau.preset=NULL,
                      tol.multiarm = 0.05, warn=TRUE)
    }
    
    incona<- netsplit(net1)
    Comparison<- incona$comparison
    No.Studies<- as.integer(incona$k)
    
    if (input$modelCont=="RE"){
      Direct<- incona$direct.random$TE
      Indirect<- incona$indirect.random$TE
      Difference<- incona$compare.random$TE
      Diff_95CI_lower<- incona$compare.random$lower
      Diff_95CI_upper<- incona$compare.random$upper
      NMA<- incona$random$TE
      pValue<- incona$compare.random$p}
    else{
      Direct<- incona$direct.fixed$TE
      Indirect<- incona$indirect.fixed$TE
      Difference<- incona$compare.fixed$TE
      Diff_95CI_lower<- incona$compare.fixed$lower
      Diff_95CI_upper<- incona$compare.fixed$upper
      NMA<- incona$fixed$TE
      pValue<- incona$compare.fixed$p}
    df<- data.frame(Comparison, No.Studies, NMA, Direct, Indirect, Difference, Diff_95CI_lower, Diff_95CI_upper, pValue)
  }
  
  output$Incon2<- renderTable(colnames=TRUE, {
    make_InconCont_sub()}
  )
  
  output$downloadInconCont2 <- downloadHandler(
    filename = function() {
      paste('Inconsistency_sub.', '.csv', sep='')
    },
    
    content = function(file) {
      write.csv({make_InconCont_sub()}, file)
    }
  )
  
  




############################
###### Baysian gemtc #######
############################
  
observeEvent(input$baye_do, {
  if (input$outcomeCont=="SMD") {
   # shinyalert(title="Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
  showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
  }
  })
  
  
  
  
model <- eventReactive(input$baye_do, {
  if (input$outcomeCont=="SMD") {
    #showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type="warning")
  }
  else {
  
    # Adding progress bars
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Updating.This may take up to 10 minute", value=0)
    

  
  #### inputting the data in long form
  newData <- data()
  newData1 <- as.data.frame(newData)
  
  if (ncol(newData1)==6){
    long <- newData1}
  else {
    data_wide <-newData1
    numbertreat=(ncol(newData1)-2)/4
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    else {
      data_wide<-newData1
    }
    
    long_pre <- reshape(data_wide, direction = "long",
                        varying = 3:ncol(data_wide), 
                        times=c(".1", ".2", ".3", ".4", ".5", ".6"), sep=".", idvar= c("StudyID", "Study"))
    long_pre<-subset(long_pre, select=-time)
    
    # delete the empty rows
    long <- long_pre[!is.na(long_pre$T), ]
  }
  
  long_sort<-long[order(long$StudyID, -long$T), ]
  
  
  # create standard error
  long_sort$se<-long_sort$SD/sqrt(long_sort$N)
  
  # attach treatment names to the T in dataset
  treat_list <- read.csv(text=input$listCont2, sep = "\t")
  lstx <- treat_list$Label
  treat_list2<-data.frame(treat_list)
  ntx <- nrow(treat_list)
  
  colnames(treat_list2)[1] <- "T"
  long_sort2<-merge(long_sort, treat_list2, by=c("T"))
  long_sort2<-subset(long_sort2, select=-T)
  names(long_sort2)[names(long_sort2) == 'Label'] <- 'T'
  
  
  progress$inc(0.2, detail="Preparing to run simulation models")
  
  
  
  #Create arm level data set with correct field names for genmeta
  head(long_sort2)
  armData <- data.frame(study=long_sort2$Study,
                        treatment=long_sort2$T,
                        mean=long_sort2$Mean,
                        std.err=long_sort2$se)
  
  
  #Create gemtc network object for analysis
  mtcNetwork <- mtc.network(data.ab=armData,description="Network")

  
  #Create gemtc model object for analysis

  if (input$modelCont=="RE") {
    mtcModel <- mtc.model(network=mtcNetwork,
                          type = "consistency",
                          linearModel="random", dic=TRUE)
  } 
  else  {
    mtcModel <- mtc.model(network=mtcNetwork,
                          type = "consistency",
                          linearModel="fixed", dic=TRUE)
  }

  
  #Create text for random/fixed effect
  if (input$modelCont=="RE") {
    a <- "random effect"
  } 
  else {
    a <- "fixed effect"
  }
  
  progress$inc(0.4, detail="Running simulation models")
  
  #Run gemtc model object for analysis
  mtcResults <- mtc.run(mtcModel)
  
  progress$inc(0.4, detail="Rendering results")
  
  #Set "Placebo" as reference treatment
  ref<-treat_list2[1,2]
  mtcRelEffects <- relative.effect(mtcResults,t1=ref)
  
  #Review results
  sumresults<-summary(mtcRelEffects)
  
  #SD and its 2.5% and 97.5%
  if (input$modelCont=="RE") {
    sd_mean<- round(sumresults$summaries$statistics[ntx,1], digits = 2)
    sd_lowCI<-round(sumresults$summaries$quantiles[ntx,1], digits = 2)
    sd_highCI<-round(sumresults$summaries$quantiles[ntx,5], digits=2)
  }
  else {
    sd_mean =0
    sd_lowCI=0
    sd_highCI=0
  }
  
  # DIC
  sumoverall<-summary(mtcResults)
  dic<-as.data.frame(sumoverall$DIC)
  
  # codes
  #code<-cat(mtcResults$model$code)
  cat(mtcResults$model$code, file="codes.txt", fill=FALSE, labels=NULL, append=FALSE)

  # initial values
  lapply(mtcResults$model$inits[[1]], write,"initialvalues_chain1.txt", append=TRUE,ncolumns=1000)
  lapply(mtcResults$model$inits[[2]], write,"initialvalues_chain2.txt", append=TRUE,ncolumns=1000)
  lapply(mtcResults$model$inits[[3]], write,"initialvalues_chain3.txt", append=TRUE,ncolumns=1000)
  lapply(mtcResults$model$inits[[4]], write,"initialvalues_chain4.txt", append=TRUE,ncolumns=1000)
  
  # data
  data1<-as.data.frame(mtcResults$samples[[1]])
  data2<-as.data.frame(mtcResults$samples[[2]])
  data3<-as.data.frame(mtcResults$samples[[3]])
  data4<-as.data.frame(mtcResults$samples[[4]])
  
  
  #Review probabilites of rankings	
  if (input$rankoptsCont=="good"){
    prob <- as.data.frame(print(rank.probability(mtcResults,preferredDirection=-1)))
  }
  else {
    prob <- as.data.frame(print(rank.probability(mtcResults,preferredDirection=1)))
  }
  names(prob)[1:ncol(prob)] <- paste("Rank ", 1:(ncol(prob)), sep="")
  
  
  # median rank chart objects to be passed.
  lstx <- treat_list$Label
  ntx <- length(lstx)
  rankl <- rownames(prob)
  
  
  #comparison table of all possible pairs
  tbl <- relative.effect.table(mtcResults)
  tb<-round(tbl, digits=2)
  tb_allpairs<-as.data.frame(tb)
  
  # deviance
  x<-mtc.deviance({mtcResults})
  c <- data.frame(x$dev.ab)
  
  list(mtcNetwork=mtcNetwork, mtcResults=mtcResults, mtcRelEffects=mtcRelEffects, prob=prob, sumresults=sumresults, sd_mean=sd_mean,
       sd_lowCI=sd_lowCI,sd_highCI=sd_highCI, tb_allpairs=tb_allpairs, a=a, dic=dic, code=code, 
       data1=data1, data2=data2, data3=data3, data4=data4, ntx=ntx, rankl=rankl, c=c, x=x)
  }
  
})

  

  
output$gemtc_rank <- renderPlot ({
  req(model(), cancelOutput = FALSE)  
  mod_list=model()
    prjtitle <- "Ranking with all studies - network meta-analysis median rank chart"
    mtcRank2(prjtitle, mod_list$ntx, mod_list$rankl, mod_list$prob, bcolr=FALSE)
  }
  )
  
  
output$gemtc_gelman <- renderPlot ({
  req(model(), cancelOutput = FALSE) 
  mod_list=model()
  data_gelman=mod_list$mtcResults
  gelman.plot(data_gelman)
  
}
) 



output$gemtc <- renderPlot({
  req(model(), cancelOutput = FALSE) 
  mod_list=model()
  data_gemtc=mod_list$mtcRelEffects
  forest(data_gemtc,digits=3)
  title(paste("All studies: 
              Bayesian", mod_list$a, "consistency model forest plot results"))
  
  
}
) 





output$textcont_gemtc <-renderText({
  req(model(), cancelOutput = FALSE) 
  mod_list=model()
  sd_mean=mod_list$sd_mean
  sd_lowCI=mod_list$sd_lowCI
  sd_highCI=mod_list$sd_highCI
  if (mod_list$a=="random effect") {
    paste("Between-study standard deviation:", sd_mean, ". 95% credible interval:",sd_lowCI,", ", sd_highCI, ".")}
  else{paste("Between-study standard deviation set at 0")}
}
) 


output$gemtc_results <- renderPrint ({
  req(model(), cancelOutput = FALSE) 
  mod_list=model()
  data_gemtc_results=mod_list$sumresults
  data_gemtc_results
})


output$code <- renderPrint ({
  req(model(), cancelOutput = FALSE) 
  mod_list=model()
  code=mod_list$code
  code
})

output$dic <- renderTable ({
  req(model(), cancelOutput = FALSE) 
  model()$dic
}, digits=3, rownames=TRUE, colnames=FALSE)


output$prob <- renderTable ({
  req(model(), cancelOutput = FALSE) 
  model()$prob
  
}, digits=5, rownames=TRUE, colnames = TRUE

)

output$downloadBaye_plot <- downloadHandler(
  filename = function() {
    
    paste0('All_studies.', input$format2)
  },
  
  content = function(file) {
    req(model(), cancelOutput = FALSE) 
    if (input$format2=="pdf"){pdf(file=file)}
    else {png(file=file)}
    mod_list=model()
    data_gemtc=mod_list$mtcRelEffects
    forest(data_gemtc,digits=3)
    title("Baysian random effect consistency model forest plot results")
    dev.off()
  }
)


output$downloadBaye_rank <- downloadHandler(
  filename = function() {
    paste('Rank_allstudies', '.csv', sep='')
  },
  
  content = function(file) {
    req(model(), cancelOutput = FALSE) 
    write.csv({model()$prob}, file)
  }
)


# download codes
output$download_code <- downloadHandler(
  filename <- function() {
    paste("code","txt", sep = ".")
  },
  content <- function(file){
    req(model(), cancelOutput = FALSE) 
    file.copy("./codes.txt", file)
  }
)

output$code <- renderPrint({
  req(model(), cancelOutput = FALSE) 
  cat(model()$mtcResults$model$code, fill=FALSE, labels=NULL, append=FALSE)
}
)

# render initial values
output$inits <- renderPrint({
  req(model(), cancelOutput = FALSE) 
  model()$mtcResults$model$inits
}
)

output$download_inits_1 <- downloadHandler(
  filename <- function() {
    paste("intialvalues_chain1","txt", sep = ".")
  },
  content <- function(file){
    req(model(), cancelOutput = FALSE) 
    file.copy("./initialvalues_chain1.txt", file)
  }
)

output$download_inits_2 <- downloadHandler(
  filename <- function() {
    paste("intialvalues_chain2","txt", sep = ".")
  },
  content <- function(file){
    req(model(), cancelOutput = FALSE) 
    file.copy("./initialvalues_chain2.txt", file)
  }
)

output$download_inits_3 <- downloadHandler(
  filename <- function() {
    paste("intialvalues_chain3","txt", sep = ".")
  },
  content <- function(file){
    req(model(), cancelOutput = FALSE) 
    file.copy("./initialvalues_chain3.txt", file)
  }
)

output$download_inits_4 <- downloadHandler(
  filename <- function() {
    paste("intialvalues_chain4","txt", sep = ".")
  },
  content <- function(file){
    req(model(), cancelOutput = FALSE) 
    file.copy("./initialvalues_chain4.txt", file)
  }
)



# download data
output$download_data1 <- downloadHandler(
  filename = function() {
    paste('Data for chain 1', '.csv', sep='')
  },
  
  content = function(file) {
    req(model(), cancelOutput = FALSE) 
    write.csv(model()$data1, file)
  }
)

output$download_data2 <- downloadHandler(
  filename = function() {
    paste('Data for chain 2', '.csv', sep='')
  },
  
  content = function(file) {
    req(model(), cancelOutput = FALSE) 
    write.csv(model()$data2, file)
  }
)

output$download_data3 <- downloadHandler(
  filename = function() {
    paste('Data for chain 3', '.csv', sep='')
  },
  
  content = function(file) {
    req(model(), cancelOutput = FALSE) 
    write.csv(model()$data3, file)
  }
)

output$download_data4 <- downloadHandler(
  filename = function() {
    paste('Data for chain 4', '.csv', sep='')
  },
  
  content = function(file) {
    req(model(), cancelOutput = FALSE) 
    write.csv(model()$data4, file)
  }
)


# output deviance
output$dev <- renderPrint({
  req(model(), cancelOutput = FALSE) 
  mtc.deviance({model()$mtcResults})
}
)

# output deviance
'
output$dev1 <- renderPlot({
  req(model(), cancelOutput = FALSE) 
  x<-mtc.deviance({model()$mtcResults})
  mtc.devplot(x)
}
)
'
'
output$dev2 <- renderPlot({
  req(model(), cancelOutput = FALSE) 
  x<-mtc.deviance({model()$mtcResults})
  mtc.levplot(x)
}
)
'


# output comparison table
output$baye_comparison <- renderTable ({
  req(model(), cancelOutput = FALSE) 
  model()$tb_allpairs
}, digits=2, rownames=TRUE, colnames = TRUE

)

output$downloadbaye_comparison <- downloadHandler(
  filename = function() {
    paste('baye_comparison', '.csv', sep='')
  },
  
  content = function(file) {
    req(model(), cancelOutput = FALSE) 
    write.csv({model()$tb_allpairs}, file)
  }
)









############################
###### Baysian gemtc: sensitivity analysis #######
############################

## sensitivity aanlsysis only reactive to the 'run' button, otherwise, 
## separate simulations will run once the 'Bayesian result' page is selected.


observeEvent(input$sub_do, {
  if (input$outcomeCont=="SMD") {
    # shinyalert(title="Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
    showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
  }
})

model_sub <- eventReactive(input$sub_do, {
  if (input$outcomeCont=="SMD") {
    # shinyalert(title="Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
    #showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
  }
  else {
    
    # Adding progress bars
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Updating.This may take up to 10 minute", value=0)
    
  
  #### inputting the data in long form
  newData <- data()
  newData1 <- as.data.frame(newData)
  
  if (ncol(newData1)==6){
    long <- newData1 }
  
  else {
    data_wide <-newData1
    numbertreat=(ncol(newData1)-2)/4
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    else {
      data_wide<-newData1
    }
    
    long_pre <- reshape(data_wide, direction = "long",
                        varying = 3:ncol(data_wide), 
                        times=c(".1", ".2", ".3", ".4", ".5", ".6"), sep=".", idvar= c("StudyID", "Study"))
    long_pre<-subset(long_pre, select=-time)
    
    # delete the empty rows
    long <- long_pre[!is.na(long_pre$T), ]
  }
  
  long_sort<-long[order(long$StudyID, -long$T), ]
  
  
  # create standard error
  long_sort$se<-long_sort$SD/sqrt(long_sort$N)
  
  # attach treatment names to the T in dataset
  treat_list <- read.csv(text=input$listCont2, sep = "\t")
  lstx <- treat_list$Label
  treat_list2<-data.frame(treat_list)
  ntx <- nrow(treat_list)
  
  colnames(treat_list2)[1] <- "T"
  long_sort2<-merge(long_sort, treat_list2, by=c("T"))
  long_sort2<-subset(long_sort2, select=-T)
  names(long_sort2)[names(long_sort2) == 'Label'] <- 'T'
  
  # filter to subgroup
  long_sort2_sub <- filter(long_sort2, !Study %in% input$exclusionCont2)
  
  #Create arm level data set with correct field names for genmeta
  progress$inc(0.2, detail="Preparing to run simulation models")
  
  armData <- data.frame(study=long_sort2_sub$Study,
                        treatment=long_sort2_sub$T,
                        mean=long_sort2_sub$Mean,
                        std.err=long_sort2_sub$se)
  
  # number of treatment after subgrouping
  u <- unique(armData$treatment)
  ntx <- length(u)
  
  # treatment names after filtering.
  u2 <- lstx[!lstx %in% u]   # the treament thats has been removed through the filtering
  lstx_sub <- lstx[!lstx %in% u2]  # the rest of the treatment in ascending order
  

  
  #Create gemtc network object for analysis
  mtcNetwork <- mtc.network(data.ab=armData,description="Network")
  
  #plot network
  #plot(mtcNetwork)
  
  #Create gemtc model object for analysis
  
  if (input$modelCont=="RE" & input$outcomeCont=="MD") {
    mtcModel <- mtc.model(network=mtcNetwork,
                          type = "consistency",
                          linearModel="random")
  } 
  else if (input$modelCont=="FE" & input$outcomeCont=="MD") {
    mtcModel <- mtc.model(network=mtcNetwork,
                          type = "consistency",
                          linearModel="fixed")
  }
  else {
    shinyalert(title="Please note: risk difference currently cannot be analysed in Bayesian analysis", type = "warning")
  }
  
  #Create text for random/fixed effect
  if (input$modelCont=="RE") {
    a <- "random effect"
  } 
  else {
    a <- "fixed effect"
  }
  
  progress$inc(0.4, detail="Running simulation models")
  
  #Run gemtc model object for analysis
  mtcResults <- mtc.run(mtcModel)
  
  progress$inc(0.4, detail="Rendering results")
  
  sumoverall<-summary(mtcResults)
  dic<-as.data.frame(sumoverall$DIC)

  
  #Set "Placebo" as reference treatment
  ref<-treat_list2[1,2]
  mtcRelEffects <- relative.effect(mtcResults,t1=ref)
  
  #Review results
  sumresults<-summary(mtcRelEffects)
  
  #SD and its 2.5% and 97.5%
  if (input$modelCont=="RE") {
    sd_mean<- round(sumresults$summaries$statistics[ntx,1], digits = 2)
    sd_lowCI<-round(sumresults$summaries$quantiles[ntx,1], digits = 2)
    sd_highCI<-round(sumresults$summaries$quantiles[ntx,5], digits=2)
  }
  else {
    sd_mean =0
    sd_lowCI=0
    sd_highCI=0
  }
  
  #Review probabilites of rankings	
  if (input$rankoptsCont=="good"){
    prob <- as.data.frame(print(rank.probability(mtcResults,preferredDirection=-1)))
  }
  else {
    prob <- as.data.frame(print(rank.probability(mtcResults,preferredDirection=1)))
  }
  names(prob)[1:ncol(prob)] <- paste("Rank ", 1:(ncol(prob)), sep="")
  
  
  # median rank chart objects to be passed.
  rankl <- rownames(prob)
  
  
  #comparison table of all possible pairs
  tbl <- relative.effect.table(mtcResults)
  tb<-round(tbl, digits=2)
  tb_allpairs<-as.data.frame(tb)
  
  # deviance
  x<-mtc.deviance({mtcResults})
  c <- data.frame(x$dev.ab)
  
  list(mtcNetwork=mtcNetwork, mtcResults=mtcResults, mtcRelEffects=mtcRelEffects, prob=prob, sumresults=sumresults, sd_mean=sd_mean,
       sd_lowCI=sd_lowCI,sd_highCI=sd_highCI, tb_allpairs=tb_allpairs, a=a, dic=dic, ntx=ntx, rankl=rankl, c=c, x=x)
}
  })


output$gemtc_rank_sub <- renderPlot ({
  req(model_sub(), cancelOutput = FALSE)
  mod_list=model_sub()
  prjtitle <- "Ranking with studies excluded - network meta-analysis median rank chart"
  mtcRank2(prjtitle, mod_list$ntx, mod_list$rankl, mod_list$prob, bcolr=FALSE)
}
)



output$gemtc_gelman_sub <- renderPlot ({
  req(model_sub(), cancelOutput = FALSE)
  mod_list=model_sub()
  data_gelman=mod_list$mtcResults
  gelman.plot(data_gelman)
  
}
) 


output$gemtc_sub <- renderPlot({
  req(model_sub(), cancelOutput = FALSE)
  
  mod_list=model_sub()
  data_gemtc=mod_list$mtcRelEffects
  forest(data_gemtc,digits=3)
  title(paste("Results with studies excluded: 
              Bayesian", mod_list$a,"consistency model forest plot results"))
})

output$dic_sub <- renderTable ({
  req(model_sub(), cancelOutput = FALSE)
  model_sub()$dic
}, digits=3, rownames=TRUE, colnames=FALSE)

output$textcont_gemtc_sub <-renderText({
  req(model_sub(), cancelOutput = FALSE)
  mod_list=model_sub()
  sd_mean=mod_list$sd_mean
  sd_lowCI=mod_list$sd_lowCI
  sd_highCI=mod_list$sd_highCI
  if (mod_list$a=="random effect"){
    paste("Between-study standard deviation:", sd_mean, ". 95% credible interval:",sd_lowCI,", ", sd_highCI, ".")}
  else{paste("Between-study standard deviation set at 0")}
}
) 

output$gemtc_results_sub <- renderPrint ({
  req(model_sub(), cancelOutput = FALSE)
  mod_list=model_sub()
  data_gemtc_results=mod_list$sumresults
  data_gemtc_results
})


output$prob_sub <- renderTable ({
  req(model_sub(), cancelOutput = FALSE)
  model_sub()$prob
  
}, digits=5, rownames=TRUE, colnames = TRUE

)

output$downloadBaye_plot_sub <- downloadHandler(
  filename = function() {
    paste0('subgroup.', input$format4)
  },
  
  content = function(file) {
    req(model_sub(), cancelOutput = FALSE)
    if (input$format4=="pdf"){pdf(file=file)}
    else {png(file=file)}
    mod_list=model_sub()
    data_gemtc=mod_list$mtcRelEffects
    forest(data_gemtc,digits=3)
    
    title("Subgroup analysis: Baysian random effect consistency model forest plot results")
    dev.off()
  }
)


output$downloadBaye_rank_sub <- downloadHandler(
  filename = function() {
    paste('Rank_subgroup', '.csv', sep='')
  },
  
  content = function(file) {
    req(model_sub(), cancelOutput = FALSE)
    write.csv({model_sub()$prob}, file)
  }
)


# output deviance
output$dev_sub <- renderPrint({
  req(model_sub(), cancelOutput = FALSE)
  mtc.deviance({model_sub()$mtcResults})
}
)

# output deviance
'
output$dev1_sub <- renderPlot({
  req(model_sub(), cancelOutput = FALSE)
  x<-mtc.deviance({model_sub()$mtcResults})
  mtc.devplot(x)
}
)

output$dev2_sub <- renderPlot({
  req(model_sub(), cancelOutput = FALSE)
  x<-mtc.deviance({model_sub()$mtcResults})
  mtc.levplot(x)
}
)
'



output$baye_comparison_sub <- renderTable ({
  req(model_sub(), cancelOutput = FALSE)
  model_sub()$tb_allpairs
}, digits=2, rownames=TRUE, colnames = TRUE

)

output$downloadbaye_comparison_sub <- downloadHandler(
  filename = function() {
    paste('baye_comparison_sub', '.csv', sep='')
  },
  
  content = function(file) {
    req(model_sub(), cancelOutput = FALSE)
    write.csv({model_sub()$tb_allpairs}, file)
  }
)




###############################################
###### Bayesian node splitting model - all studies ##########
###############################################

observeEvent(input$node, {
  if (input$outcomeCont=="SMD") {
    # shinyalert(title="Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
    showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
  }
})

model_nodesplit <- eventReactive(input$node, {
  if (input$outcomeCont=="SMD") {
    #showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
  }
  else {
    
    # Adding progress bars
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Updating.This may take up to 20 minute", value=0)
    
  
  
  #### inputting the data in long form
  newData <- data()
  newData1 <- as.data.frame(newData)
  
  if (ncol(newData1)==6){
    long <- newData1}
  else {
    data_wide <-newData1
    numbertreat=(ncol(newData1)-2)/4
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    else {
      data_wide<-newData1
    }
    
    long_pre <- reshape(data_wide, direction = "long",
                        varying = 3:ncol(data_wide), 
                        times=c(".1", ".2", ".3", ".4", ".5", ".6"), sep=".", idvar= c("StudyID", "Study"))
    long_pre<-subset(long_pre, select=-time)
    
    # delete the empty rows
    long <- long_pre[!is.na(long_pre$T), ]
  }
  
  long_sort<-long[order(long$StudyID, -long$T), ]
  
  
  # create standard error
  long_sort$se<-long_sort$SD/sqrt(long_sort$N)
  
  # attach treatment names to the T in dataset
  treat_list <- read.csv(text=input$listCont2, sep = "\t")
  lstx <- treat_list$Label
  treat_list2<-data.frame(treat_list)
  treat_row <- nrow(treat_list)-1
  
  colnames(treat_list2)[1] <- "T"
  long_sort2<-merge(long_sort, treat_list2, by=c("T"))
  long_sort2<-subset(long_sort2, select=-T)
  names(long_sort2)[names(long_sort2) == 'Label'] <- 'T'
  
  #Create arm level data set with correct field names for genmeta
  
  progress$inc(0.2, detail="Preparing to run simulation models")
  armData <- data.frame(study=long_sort2$Study,
                        treatment=long_sort2$T,
                        mean=long_sort2$Mean,
                        std.err=long_sort2$se)
  
  
  #Create gemtc network object for analysis
  mtcNetwork <- mtc.network(data.ab=armData,description="Network")
  
  progress$inc(0.4, detail="Running simulation models")
  
  #Create gemtc nodesplitting model for analysis
  if (input$modelCont=="RE") {
    nodeSplitResults <- mtc.nodesplit(network=mtcNetwork,
                                      #likelihood="normal",
                                      #link="identity",
                                      linearModel="random",
                                      comparisons=mtc.nodesplit.comparisons(mtcNetwork))
    
    
  } 
  else {
    nodeSplitResults <- mtc.nodesplit(network=mtcNetwork,
                                      #likelihood="normal",
                                      #link="identity",
                                      linearModel="fixed",
                                      comparisons=mtc.nodesplit.comparisons(mtcNetwork))
    
  }
  progress$inc(0.4, detail="Rendering results")
  
  
  #Review Results
  node<-as.data.frame(print(summary(nodeSplitResults)))
  list(node=node)
  
  }
})


###### outputs######

output$node_table<- renderTable(colnames=TRUE, {
  req(model_nodesplit(), cancelOutput = FALSE)
  # add in withprogress status bar
  model_nodesplit()$node
}
)

output$downloadnode <- downloadHandler(
  filename = function() {
    paste('Nodesplit', '.csv', sep='')
  },
  
  content = function(file) {
    req(model_nodesplit(), cancelOutput = FALSE)
    write.csv({model_nodesplit()$node}, file)
  }
)




###############################################
###### Bayesian node splitting model - with studies excluded ##########
###############################################

observeEvent(input$node_sub, {
  if (input$outcomeCont=="SMD") {
    # shinyalert(title="Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
    showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
  }
})


model_nodesplit_sub <- eventReactive(input$node_sub, {
  
  if (input$outcomeCont=="SMD") {
    #showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
  }
  else {
    
    # Adding progress bars
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Updating.This may take up to 20 minute", value=0)
    
  
  #### inputting the data in long form
  newData <- data()
  newData1 <- as.data.frame(newData)
  
  if (ncol(newData1)==6){
    long <- newData1}
  else {
    data_wide <-newData1
    numbertreat=(ncol(newData1)-2)/4
    # generate additional columns if less than 6 arms.
    if (numbertreat < 6) {
      for (k in (numbertreat+1):6) {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      }
    }
    else {
      data_wide<-newData1
    }
    
    long_pre <- reshape(data_wide, direction = "long",
                        varying = 3:ncol(data_wide), 
                        times=c(".1", ".2", ".3", ".4", ".5", ".6"), sep=".", idvar= c("StudyID", "Study"))
    long_pre<-subset(long_pre, select=-time)
    
    # delete the empty rows
    long <- long_pre[!is.na(long_pre$T), ]
  }
  
  long_sort<-long[order(long$StudyID, -long$T), ]
  
  
  # create standard error
  long_sort$se<-long_sort$SD/sqrt(long_sort$N)
  
  # attach treatment names to the T in dataset
  treat_list <- read.csv(text=input$listCont2, sep = "\t")
  lstx <- treat_list$Label
  treat_list2<-data.frame(treat_list)
  treat_row <- nrow(treat_list)-1
  
  colnames(treat_list2)[1] <- "T"
  long_sort2<-merge(long_sort, treat_list2, by=c("T"))
  long_sort2<-subset(long_sort2, select=-T)
  names(long_sort2)[names(long_sort2) == 'Label'] <- 'T'
  
  long_sort2_sub <- filter(long_sort2, !Study %in% input$exclusionCont2)
  
  
  #Create arm level data set with correct field names for genmeta
  progress$inc(0.2, detail="Preparing to run simulation models")
  
  
  armData_sub <- data.frame(study=long_sort2_sub$Study,
                            treatment=long_sort2_sub$T,
                            mean=long_sort2_sub$Mean,
                            std.err=long_sort2_sub$se)
  
  
  #Create gemtc network object for analysis
  mtcNetwork_sub <- mtc.network(data.ab=armData_sub,description="Network")
  
  progress$inc(0.4, detail="Running simulation models")
  #Create gemtc nodesplitting model for analysis
  if (input$modelCont=="RE") {
    nodeSplitResults_sub <- mtc.nodesplit(network=mtcNetwork_sub,
                                          #likelihood="normal",
                                          #link="identity",
                                          linearModel="random",
                                          comparisons=mtc.nodesplit.comparisons(mtcNetwork_sub))
    
    
  } 
  else {
    nodeSplitResults_sub <- mtc.nodesplit(network=mtcNetwork_sub,
                                          #likelihood="normal",
                                          #link="identity",
                                          linearModel="fixed",
                                          comparisons=mtc.nodesplit.comparisons(mtcNetwork_sub))
    
  }
  
  
  progress$inc(0.4, detail="Rendering results")
  
  #Review Results
  node_sub<-as.data.frame(print(summary(nodeSplitResults_sub)))
  list(node_sub=node_sub)
  } 
  
})


###### outputs######

output$node_table_sub<- renderTable(colnames=TRUE, {
  # add in withprogress status bar
  req(model_nodesplit_sub(), cancelOutput = FALSE)
  model_nodesplit_sub()$node_sub
}
)

output$downloadnode_sub <- downloadHandler(
  filename = function() {
    paste('Nodesplit_sen', '.csv', sep='')
  },
  
  content = function(file) {
    req(model_nodesplit_sub(), cancelOutput = FALSE)
    write.csv({model_nodesplit_sub()$node_sub}, file)
  }
)




##### network connectivity
make_netconnect = function() {
  
  #### same codes for inputting the data
  newData <- data()
  newData1 <- as.data.frame(newData)
  
  if (ncol(newData1)==6){
    newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
    # create counting variable for number of arms within each study.
    newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
    # reshape
    data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
    numbertreat=max(newData2$number)
  }
  else {
    data_wide<- newData1
    numbertreat=(ncol(newData1)-2)/4
  }
  # generate additional columns if less than 6 arms.
  if (numbertreat < 6) {
    for (k in (numbertreat+1):6) {
      data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
    }
  }
  #####
  
  ## for network plot, it does not matter whether it is MD or MSD.
  d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_wide,sm="MD")
  treat_list <- read.csv(text=input$listCont2, sep = "\t")
  lstx <- treat_list$Label
  ntx <- length(lstx)
  
  # Static NMA of all studies
  
  
  d1$treat1 <- factor(d1$treat1,
                      levels = c(1:ntx),
                      labels = as.character(treat_list$Label))
  
  d1$treat2 <- factor(d1$treat2,
                      levels = c(1:ntx),
                      labels = as.character(treat_list$Label))
  
  
  nc1 <- netconnection(d1$treat1,d1$treat2,d1$studlab, data=NULL)
  print(nc1)
  
}


output$netconnect <- renderPrint ({
  make_netconnect()
})

#### network connection underneath network plot
make_netconnectUpdate = function(){
  #### same codes for inputting the data   
  #### same codes for inputting the data
  newData <- data()
  newData1 <- as.data.frame(newData)
  
  if (ncol(newData1)==6){
    newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
    # create counting variable for number of arms within each study.
    newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)
    # reshape
    data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")
    numbertreat=max(newData2$number)
  }
  else {
    data_wide<- newData1
    numbertreat=(ncol(newData1)-2)/4
  }
  # generate additional columns if less than 6 arms.
  if (numbertreat < 6) {
    for (k in (numbertreat+1):6) {
      data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
    }
  }
  #####
  
  treat_list <- read.csv(text=input$listCont2, sep = "\t")
  lstx <- treat_list$Label
  ntx <- length(lstx)
  
  # Get subset of data to use
  data_sub <- filter(data_wide, !Study %in% input$exclusionCont2)
  d1_sub <- d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data_sub,sm="MD")
  
  # Network meta-analysis
  
  d1_sub$treat1 <- factor(d1_sub$treat1,
                          levels = c(1:ntx),
                          labels = as.character(treat_list$Label))
  
  d1_sub$treat2 <- factor(d1_sub$treat2,
                          levels = c(1:ntx),
                          labels = as.character(treat_list$Label))
  
  
  
  nc_sub1 <- netconnection(d1_sub$treat1,d1_sub$treat2,d1_sub$studlab, data=NULL)
  print(nc_sub1)
  
}


output$netconnect_sub <- renderPrint ({
  make_netconnectUpdate()
})



####### scatter plot

umepp <- function(){
  mod_list=model()
  if (input$modelCont=="RE" & input$outcomeCont=="MD") {
    umep <- mtc.model(network=mod_list$mtcNetwork,
                     type = "consistency",
                     linearModel="random")
  } 
  else if (input$modelCont=="FE" & input$outcomeCont=="MD") {
    umep <- mtc.model(network=mod_list$mtcNetwork,
                     type = "ume",
                     linearModel="fixed")
  }
  else
  {
  }
  ume_resultspp <- mtc.run(umep)
  dev_umepp<-mtc.deviance(ume_resultspp)
  dev_umepp
  
}

output$dev_ume <- renderPrint({
  #req(ume(), cancelOutput = FALSE)
  umepp()
  
  #return ((ume() %>% format() %>% toString()))
}
)



scat_plot = function(){
  mod_list=model()
  c <- mod_list$c
  c$names <- rownames(c)
  # Adding progress bars
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message="Updating.This may take up to 10 minutes", value=0)
  
  
  if (input$modelCont=="RE" & input$outcomeCont=="MD") {
    ume <- mtc.model(network=mod_list$mtcNetwork,
                     type = "ume",
                     linearModel="random", dic = TRUE)
  } 
  else if (input$modelCont=="FE" & input$outcomeCont=="MD") {
    ume <- mtc.model(network=mod_list$mtcNetwork,
                     type = "ume",
                     linearModel="fixed", dic = TRUE)
  }
  else
  {
  }
  
  progress$inc(0.4, detail="Running UME models")
  ume_results <- mtc.run(ume)
  
  progress$inc(0.4, detail="Rendering results")
  y<-mtc.deviance({ume_results})
  inc <-data.frame(y$dev.ab)
  
  inc$names <- rownames(inc)
  all <-merge(c,inc, by="names")
  
  names(all)[names(all) == "X1.x"] <- "NMAmodel_arm1"
  names(all)[names(all) == "X1.y"] <- "UMEmodel_arm1"
  names(all)[names(all) == "X2.x"] <- "NMAmodel_arm2"
  names(all)[names(all) == "X2.y"] <- "UMEmodel_arm2"
  
  # alternative to plotly - firstly create a ggplot, then convert to plotly.
  #p<-ggplot() + 
  #  geom_point(data=all, aes(x=NMAmodel_arm1, y=UMEmodel_arm1, text=paste("Study:", names))) +
  #  geom_point(data=all, aes(x=NMAmodel_arm2, y=UMEmodel_arm2, text=paste("Study:", names))) +
  #  geom_abline(intercept=0, slope=1, linetype="dashed") +
  #  labs(x="NMA model", y="Inconsistency model")
  
  #if (ncol(c)>3) { p = p + geom_point(data=all, aes(x=X3.x, y=X3.y)) }
  #if (ncol(c)>4) { p = p + geom_point(data=all, aes(x=X4.x, y=X4.y)) }
  #if (ncol(c)>5) { p = p + geom_point(data=all, aes(x=X5.x, y=X5.y)) }
  #if (ncol(c)>6) { p = p + geom_point(data=all, aes(x=X6.x, y=X6.y)) }
  #p<-ggplotly(p)
  
  
  #### to define the maximum range of the equality line: find the maximum number of the dev data in the dataset. 
  
  k<-all[ , names(all) != "names"]
  j <- max(k, na.rm=TRUE)
  m <- c(0,1,j)
  n <- c(0,1,j)
  dline<-data.frame(m,n)
  
  # plot
  p = plot_ly() %>%
    add_trace(data=dline, x = ~m, y = ~n, type = 'scatter', mode = 'lines',
              line = list(color = '#45171D'))
  p = p %>%
    add_trace(data=all, x=~NMAmodel_arm1, y=~UMEmodel_arm1, type='scatter', mode='markers',
              marker=list(size=4, color = '#CAEFD1',
                          line = list(color = 'rgb(0,128,0)',
                                      width = 2)),
              hoverinfo='text',
              text=~paste('</br> Author', all$names,
                          '</br> Arm 1',
                          '</br> Deviance from NMA model:',round(NMAmodel_arm1, digits=2),
                          '</br> Deviance from UME model:',round(UMEmodel_arm1, digits=2)
              ))
  
  p=p %>% 
    add_trace(
      x=~NMAmodel_arm2, y=~UMEmodel_arm2, type='scatter', mode='markers', 
      marker=list(size=4, color = '#CAEFD1',
                  line = list(color = 'rgb(0,128,0)',
                              width = 2)),
      hoverinfo='text',
      text=~paste('</br> Author', all$names,
                  '</br> Arm 2',
                  '</br> Deviance from NMA model:',round(NMAmodel_arm2, digits=2),
                  '</br> Deviance from UME model:',round(UMEmodel_arm2, digits=2)
                  
      ))%>%
    layout(showlegend = FALSE, xaxis=list(title="Deviance from NMA model"), 
           yaxis=list(title="Deviance from UME inconsistency model"))
  
  if (ncol(c)>3) { 
    p=p %>% 
      add_trace(data=all,
                x=~X3.x, y=~X3.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 3',
                            '</br> Deviance from NMA model:',round(X3.x, digits=2),
                            '</br> Deviance from UME model:',round(X3.y, digits=2)))}
  if (ncol(c)>4) { 
    p=p %>% 
      add_trace(data=all,
                x=~X4.x, y=~X4.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 4',
                            '</br> Deviance from NMA model:',round(X4.x, digits=2),
                            '</br> Deviance from UME model:',round(X4.y, digits=2)))}
  if (ncol(c)>5) { 
    p=p %>% 
      add_trace(data=all,
                x=~X5.x, y=~X5.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 5',
                            '</br> Deviance from NMA model:',round(X5.x, digits=2),
                            '</br> Deviance from UME model:',round(X5.y, digits=2)))}
  if (ncol(c)>6) { 
    p=p %>% 
      add_trace(data=all,
                x=~X6.x, y=~X6.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 6',
                            '</br> Deviance from NMA model:',round(X6.x, digits=2),
                            '</br> Deviance from UME model:',round(X6.y, digits=2)))}
  p 
  
  
}

output$dev_scat <- renderPlotly({
  scat_plot()
}
)











## sensitivity analysis - scatter plot for deviance
scat_plot_sub = function(){
  mod_list_sub=model_sub()
  c <- mod_list_sub$c
  c$names <- rownames(c)
  
  # Adding progress bars
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message="Updating.This may take up to 10 minutes", value=0)
  
  if (input$modelCont=="RE" & input$outcomeCont=="MD") {
    ume <- mtc.model(network=mod_list_sub$mtcNetwork,
                     type = "ume",
                     linearModel="random")
  } 
  else if (input$modelCont=="FE" & input$outcomeCont=="MD") {
    ume <- mtc.model(network=mod_list_sub$mtcNetwork,
                     type = "ume",
                     linearModel="fixed")
  }
  else
  {
  }
  progress$inc(0.4, detail="Running UME models")
  ume_results <- mtc.run(ume)
  progress$inc(0.4, detail="Rendering results")
  y<-mtc.deviance({ume_results})
  inc <-data.frame(y$dev.ab)
  
  inc$names <- rownames(inc)
  all <-merge(c,inc, by="names")
  names(all)[names(all) == "X1.x"] <- "NMAmodel_arm1"
  names(all)[names(all) == "X1.y"] <- "UMEmodel_arm1"
  names(all)[names(all) == "X2.x"] <- "NMAmodel_arm2"
  names(all)[names(all) == "X2.y"] <- "UMEmodel_arm2"
  
  k<-all[ , names(all) != "names"]
  j <- max(k, na.rm=TRUE)
  m <- c(0,1,j)
  n <- c(0,1,j)
  dline<-data.frame(m,n)
  
  # plot
  p = plot_ly() %>%
    add_trace(data=dline, x = ~m, y = ~n, type = 'scatter', mode = 'lines',
              line = list(color = '#45171D'))
  p = p %>%
    add_trace(data=all, x=~NMAmodel_arm1, y=~UMEmodel_arm1, type='scatter', 
              mode='markers',
              marker=list(size=4, color = '#CAEFD1',
                          line = list(color = 'rgb(0,128,0)',
                                      width = 2)),
              hoverinfo='text',
              text=~paste('</br> Author', all$names,
                          '</br> Arm 1',
                          '</br> Deviance from NMA model:',round(NMAmodel_arm1, digits=2),
                          '</br> Deviance from UME model:',round(UMEmodel_arm1, digits=2)
              ))
  
  p=p %>% 
    add_trace(
      x=~NMAmodel_arm2, y=~UMEmodel_arm2, type='scatter', mode='markers', 
      marker=list(size=4, color = '#CAEFD1',
                  line = list(color = 'rgb(0,128,0)',
                              width = 2)),
      hoverinfo='text',
      text=~paste('</br> Author', all$names,
                  '</br> Arm 2',
                  '</br> Deviance from NMA model:',round(NMAmodel_arm2, digits=2),
                  '</br> Deviance from UME model:',round(UMEmodel_arm2, digits=2)
                  
      ))%>%
    layout(showlegend = FALSE, xaxis=list(title="Deviance from NMA model"), 
           yaxis=list(title="Deviance from UME inconsistency model"))
  
  if (ncol(c)>3) { 
    p=p %>% 
      add_trace(data=all,
                x=~X3.x, y=~X3.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 3',
                            '</br> Deviance from NMA model:',round(X3.x, digits=2),
                            '</br> Deviance from UME model:',round(X3.y, digits=2)))}
  if (ncol(c)>4) { 
    p=p %>% 
      add_trace(data=all,
                x=~X4.x, y=~X4.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 4',
                            '</br> Deviance from NMA model:',round(X4.x, digits=2),
                            '</br> Deviance from UME model:',round(X4.y, digits=2)))}
  if (ncol(c)>5) { 
    p=p %>% 
      add_trace(data=all,
                x=~X5.x, y=~X5.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 5',
                            '</br> Deviance from NMA model:',round(X5.x, digits=2),
                            '</br> Deviance from UME model:',round(X5.y, digits=2)))}
  if (ncol(c)>6) { 
    p=p %>% 
      add_trace(data=all,
                x=~X6.x, y=~X6.y, type='scatter', mode='markers', 
                marker=list(size=4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
                hoverinfo='text',
                text=~paste('</br>', all$names,
                            '</br> Arm 6',
                            '</br> Deviance from NMA model:',round(X6.x, digits=2),
                            '</br> Deviance from UME model:',round(X6.y, digits=2)))}
  
  
  p
}

output$dev_scat_sub <- renderPlotly({
  scat_plot_sub()
}
)







####### Deviance plot

stemplot = function(){
  mod_list=model()
  c <- mod_list$c
  c$names <- rownames(c)
  
  # Adding progress bars
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message="Updating.This may take up to 10 minutes", value=0)
  
  # deviance plot
  x<-mod_list$x
  tpl <- x[['dev.ab']]
  study <- matrix(rep(1:nrow(tpl), times=ncol(tpl)), nrow=nrow(tpl), ncol=ncol(tpl))
  study <- t(study)[t(!is.na(tpl))]
  devbar <- t(x[['dev.ab']])[t(!is.na(tpl))]
  title <- "Per-arm residual deviance"
  xlab <- "Arm"
  
  k<-rowSums(!is.na(tpl))
  studynames <- rep(c$names, k)
  
  
  v<-1:length(devbar)
  sep<-study%%2
  d<- data.frame(v, devbar, sep, study, studynames)
  
  
  xl<- list(
    title = xlab,
    range= c(0,length(devbar)+5),
    tick0 = 0,
    dtick = 5,
    zeroline = TRUE,
    showline = TRUE
  )
  
  yl <- list(
    title="Residual deviance",
    range=c(0, ceiling(devbar)),
    autorange=TRUE,
    tick0 = 0,
    dtick = 0.5,
    zeroline = TRUE,
    showline = TRUE
  )
  
  progress$inc(0.4, detail="Updating")
  
  p <- plot_ly(data=d, x=~v, y=~devbar)
  #add_segments(x=1, xend=1, y=0, yend=1)
  for (i in 1:length(devbar)) {
    p = p %>%
      add_segments(x=i, xend=i, y=0, yend=devbar[i], marker=list(color='white', 
                                                                 line=list(color='white')),
                   line=list(color='black', width=1))
  }
  p = p%>% 
    add_trace(data=d, x=~v, y=~devbar, type = 'scatter', mode='markers', 
              marker=list(size=4, color = '#CAEFD1',
                          line = list(color = 'rgb(0,128,0)',
                                      width = 2)),
              symbol =~sep, symbols = c('circle','o'), 
              hoverinfo='text',
              text=~paste('</br> Study', d$studynames,
                          '</br> Deviance from NMA model:',round(d$devbar, digits=2)
                          
              )) %>%
    layout(
      xaxis = xl, yaxis = yl, showlegend = FALSE)
  
  progress$inc(0.4, detail="Rendering results")
  
  p
 
}

output$dev1 <- renderPlotly({
  stemplot()
}
)




####### Deviance plot - sensitivity analysis

stemplot_sub = function(){
  mod_list=model_sub()
  c <- mod_list$c
  c$names <- rownames(c)
  
  # Adding progress bars
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message="Updating.This may take up to 10 minutes", value=0)
  
  # deviance plot
  x<-mod_list$x
  tpl <- x[['dev.ab']]
  study <- matrix(rep(1:nrow(tpl), times=ncol(tpl)), nrow=nrow(tpl), ncol=ncol(tpl))
  study <- t(study)[t(!is.na(tpl))]
  devbar <- t(x[['dev.ab']])[t(!is.na(tpl))]
  title <- "Per-arm residual deviance"
  xlab <- "Arm"
  
  k<-rowSums(!is.na(tpl))
  studynames <- rep(c$names, k)
  
  
  v<-1:length(devbar)
  sep<-study%%2
  d<- data.frame(v, devbar, sep, study, studynames)
  
  
  xl<- list(
    title = xlab,
    range= c(0,length(devbar)+5),
    tick0 = 0,
    dtick = 5,
    zeroline = TRUE,
    showline = TRUE
  )
  
  yl <- list(
    title="Residual deviance",
    range=c(0, ceiling(devbar)),
    autorange=TRUE,
    tick0 = 0,
    dtick = 0.5,
    zeroline = TRUE,
    showline = TRUE
  )
  
  progress$inc(0.4, detail="Updating")
  
  p <- plot_ly(data=d, x=~v, y=~devbar)
  #add_segments(x=1, xend=1, y=0, yend=1)
  for (i in 1:length(devbar)) {
    p = p %>%
      add_segments(x=i, xend=i, y=0, yend=devbar[i], marker=list(color='white', 
                                                                 line=list(color='white')),
                   line=list(color='black', width=1))
  }
  p = p%>% 
    add_trace(data=d, x=~v, y=~devbar, type = 'scatter', mode='markers', 
              marker=list(size=4, color = '#CAEFD1',
                          line = list(color = 'rgb(0,128,0)',
                                      width = 2))
              , symbol =~sep, symbols = c('circle','o'), 
              hoverinfo='text',
              text=~paste('</br> Study', d$studynames,
                          '</br> Deviance from NMA model:',round(d$devbar, digits=2)
                          
              )) %>%
    layout(
      xaxis = xl, yaxis = yl, showlegend = FALSE)
  
  progress$inc(0.4, detail="Rendering results")
  
  p
  
}

output$dev1_sub <- renderPlotly({
  stemplot_sub()
}
)



####### Leverage plot

levplot = function(){
  mod_list=model()

  # Adding progress bars
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message="Updating.This may take up to 10 minutes", value=0)
  
  # deviance plot
  x<-mod_list$x
  
  fit.ab <- apply(x[['fit.ab']], 1, sum, na.rm=TRUE)
  dev.ab <- apply(x[['dev.ab']], 1, sum, na.rm=TRUE)
  lev.ab <- dev.ab - fit.ab
  fit.re <- x[['fit.re']]
  dev.re <- x[['dev.re']]
  lev.re <- dev.re - fit.re
  nd <- c(x[['nd.ab']], x[['nd.re']])
  w <- sqrt(c(dev.ab, dev.re) / nd)
  lev <- c(lev.ab, lev.re) / nd
  d<-data.frame(w,lev)
  d$names <- rownames(d)
  
  a<-seq(from=0, to=3, by=0.05)
  b1 <- 1-a^2
  b2 <- 2-a^2
  b3 <- 3-a^2
  b4<- 4-a^2
  parabola <- data.frame(a, b1, b2, b3, b4)
  
  xlab="Square root of average residual deviance across the arms for each study"
  'sqrt(average(residual deviance for arm 1, residual deviance for arm 2...))'
  
  ylab="Average leverage across the arms for each study"
  
  
  xl<- list(
    title = xlab,
    range= c(0,max(c(w,2.5))),
    tick0 = 0,
    dtick = 0.5,
    zeroline = TRUE,
    showgrid = TRUE
  )
  
  yl <- list(
    title=ylab,
    range=c(0, max(c(lev, 4))),
    tick0 = 0,
    dtick = 1,
    zeroline = TRUE,
    showgrid = TRUE
  )
  
  progress$inc(0.4, detail="Updating")
  
  p<- plot_ly(parabola, x=~a) %>%
    add_trace(y=b1, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(y=b2, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(y=b3, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(y=b4, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(data=d, x=~w, y=~lev, 
              marker=list(size=4, color = '#CAEFD1',
                          line = list(color = 'rgb(0,128,0)',
                                      width = 2)),
              hoverinfo='text',
              text=~paste('</br> Study:', d$names,
                          '</br> Deviance',round(d$w, digits=2),
                          '</br> Leverage',round(d$lev, digits=2)
              )) %>%
    layout(
      xaxis = xl, yaxis = yl, showlegend = FALSE, title="Leverage versus residual deviance")
  
  progress$inc(0.4, detail="Rendering results")
  
  p
  
}

output$dev2 <- renderPlotly({
  levplot()
}
)



####### Leverage plot - sensitivity analysis

levplot_sub = function(){
  mod_list=model_sub()
  
  # Adding progress bars
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message="Updating.This may take up to 10 minutes", value=0)
  
  # deviance plot
  x<-mod_list$x
  
  fit.ab <- apply(x[['fit.ab']], 1, sum, na.rm=TRUE)
  dev.ab <- apply(x[['dev.ab']], 1, sum, na.rm=TRUE)
  lev.ab <- dev.ab - fit.ab
  fit.re <- x[['fit.re']]
  dev.re <- x[['dev.re']]
  lev.re <- dev.re - fit.re
  nd <- c(x[['nd.ab']], x[['nd.re']])
  w <- sqrt(c(dev.ab, dev.re) / nd)
  lev <- c(lev.ab, lev.re) / nd
  d<-data.frame(w,lev)
  d$names <- rownames(d)
  
  a<-seq(from=0, to=3, by=0.05)
  b1 <- 1-a^2
  b2 <- 2-a^2
  b3 <- 3-a^2
  b4<- 4-a^2
  parabola <- data.frame(a, b1, b2, b3, b4)
  
  xlab="Square root of average residual deviance across the arms for each study"
  'sqrt(average(residual deviance for arm 1, residual deviance for arm 2...))'
  
  ylab="Average leverage across the arms for each study"
  
  
  xl<- list(
    title = xlab,
    range= c(0,max(c(w,2.5))),
    tick0 = 0,
    dtick = 0.5,
    zeroline = TRUE,
    showgrid = TRUE
  )
  
  yl <- list(
    title=ylab,
    range=c(0, max(c(lev, 4))),
    tick0 = 0,
    dtick = 1,
    zeroline = TRUE,
    showgrid = TRUE
  )
  
  progress$inc(0.4, detail="Updating")
  
  p<- plot_ly(parabola, x=~a) %>%
    add_trace(y=b1, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(y=b2, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(y=b3, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(y=b4, mode='lines', line=list(color='black', width=1), hoverinfo='skip') %>%
    add_trace(data=d, x=~w, y=~lev, 
              marker=list(size=4, color = '#CAEFD1',
                          line = list(color = 'rgb(0,128,0)',
                                      width = 2)),
              hoverinfo='text',
              text=~paste('</br> Study:', d$names,
                          '</br> Deviance',round(d$w, digits=2),
                          '</br> Leverage',round(d$lev, digits=2)
              )) %>%
    layout(
      xaxis = xl, yaxis = yl, showlegend = FALSE, title="Leverage versus residual deviance")
  
  progress$inc(0.4, detail="Rendering results")
  
  p
  
}

output$dev2_sub <- renderPlotly({
  levplot_sub()
}
)










})

