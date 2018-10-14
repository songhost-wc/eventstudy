options(shiny.maxRequestSize=1024*1024^2)
source("option1.R")
library(shiny)
library(shinyjs)
library(ggplot2)
library(matrixStats)
library(data.table)

shinyServer(function(input, output, session) {
  
  allplotvals <- reactiveValues(pdata = ggplot())
  fullplot <- reactiveValues(pdata = ggplot())
  fullsummary <- reactiveValues(summarytable = data.frame())
  countvals <- reactiveValues(linecount = 0)
  linecolours <- reactiveValues(lcolours = character())
  
  inputreturn <- reactive({
    dat1 <- input$inputReturnData
    if (is.null(dat1)) # User has not uploaded a file yet
      return(NULL)
    #shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0) # reinitialize to 0 if you run the calculation several times
    #session$sendCustomMessage(type = 'launch-modal', "my-modal") # launch the modal
    withProgress(message = 'Loading Dataset', detail = 'Loading returns dataset',value = 0,{
      read.csv(dat1$datapath,header=TRUE,sep=",",check.names=FALSE)
    })
    #shinyWidgets::updateProgressBar(session = session, id = "pb", value = 100)
  })
  
  # the input file of parameter/characteristic data (data file #2)
  inputpar <- reactive({
    dat2 <- input$inputpardata
    if (is.null(dat2)) # User has not uploaded a file yet
      return(NULL)
    withProgress(message = 'Loading Dataset', detail = 'Loading characteristics dataset',value = 0,{
      read.csv(dat2$datapath,header=TRUE,sep=",",check.names=FALSE)
    })
  })
  
  originalrangedf <- reactive({
    initiate_original_range(inputpar())
  })
  
  selectedrangedf <- reactive({
    initiate_selected_range(inputpar())
  })
  
  #merge the two datasets by the first two columns (company code & event date)
  mergedata <- reactive({
    shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0) # reinitialize to 0 if you run the calculation several times
    session$sendCustomMessage(type = 'launch-modal', "my-modal") # launch the modal
    withProgress(message = 'Preparing', detail = 'Merging data: this may take a while...',value = 0,{
      merge_data(inputreturn(),inputpar())
    })
    
  })
  
  # to store observers and make sure only once is created per slider bar
  obsList <- list()
  
  observeEvent(input$inputpardata, {
    #check the PR values fall between 1 and 100
    check1 <- reactive({
      #pardata <- inputpar()
      if(!is.null(inputpar())){
        validate(
          need(all(findInterval(as.numeric(unlist(inputpar()[seq(4,length(colnames(inputpar())),by=2)])),
                                c(1,100),rightmost.closed = TRUE)), 
               "ERROR: PR column values must be between 1 and 100. Please check your data and reupload.")
        )
      }
    })
    output$alert1 <- renderText(check1())
    
    #dynamically generate resettable slideinputs for event window and parameters
    output$resetable_input <- renderUI({
      originalrange <<- originalrangedf()
      print("original range is:")
      print(originalrange)
      selectedrange <<- selectedrangedf()
      
      if(!is.null(originalrange)){
        times <- input$reset_input
        div(id=letters[(times %% length(letters)) + 1],
            sliderInput("window",
                        "Event Window period",
                        min = -60,  max = 60, value = c(-60,60)),
            #create a sliderinput for each parameter (from the 3rd header, every other header)
            lapply(seq(1,length(originalrange$Characteristic),by=1), function(i) {
              sliderName <- paste0("range", i)
              if (is.null(obsList[[sliderName]])) {
                obsList[[sliderName]] <<- observeEvent(input[[sliderName]], {
                  #cat("SliderInput ", i, "\n")
                  #print(selectedrange)
                  selectedrange$From[i] <- input[[paste0("range", i)]][1]
                  selectedrange$To[i] <- input[[paste0("range", i)]][2]
                  originalrange$From[i] <- median(inputpar()[inputpar()[,(i*2+2)]==input[[paste0("range", i)]][1],(i*2+1)])
                  originalrange$To[i] <- median(inputpar()[inputpar()[,(i*2+2)]==input[[paste0("range", i)]][2],(i*2+1)])
                  #print(originalrange$From[i])
                  #print(originalrange$To[i])
                  print("UPDATE: ")
                  print(HTML("<i>",paste(originalrange$Characteristic[i]), " | from ", "<span style = \'color:red\'>", originalrange$From[i],"</span>", " to ", "<span style = \'color:red\'>", originalrange$To[i], "</span><i>"))
                  updateSliderInput(session, paste0("range", i), 
                                    label = HTML("<i>",paste(originalrange$Characteristic[i]),"<i>", " | from ", originalrange$From[i], " to ", originalrange$To[i]))
                                    #label = HTML(paste(originalrange$Characteristic[i])," | from ", originalrange$From[i]," to ", originalrange$To[i]))
                  
                })
              }
              print(HTML("<i>",paste(originalrange$Characteristic[i]), " | from ", "<span style = \'color:red\'>", originalrange$From[i],"</span>", " to ", "<span style = \'color:red\'>", originalrange$To[i], "</span><i>"))
              sliderInput(inputId = sliderName, 
                          label = HTML("<i>",paste(originalrange$Characteristic[i]),"<i>", " | from ", originalrange$From[i], " to ", originalrange$To[i]),
                          #label = HTML(paste(originalrange$Characteristic[i]), " | from ", originalrange$From[i], " to ",originalrange$To[i]),
                          min = 1, max = 100, value = c(1, 100))
            }
          )
        )
      }
    })
  })
  
  observeEvent(input$startbutton,{
    #shinyjs::disable("startbutton")
    #shinyjs::enable("startbutton")
    shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0) # reinitialize to 0 if you run the calculation several times
    session$sendCustomMessage(type = 'launch-modal', "my-modal") # launch the modal
    output$meancumreturnplot <- renderPlot({
      allplotvals$pdata
    })
    
    rangedf <- reactive({
      #pardata <<- inputpar()
      originalrangedf <- isolate(originalrangedf())
      if(!is.null(originalrangedf) & !is.null(input[[paste0("range", 1)]])){
        #print(originalrangedf$Characteristic)
        data.frame(
          Characteristic = originalrangedf$Characteristic,
          From = sapply(seq(1,length(originalrangedf$Characteristic),by=1), function(i) {
            isolate(input[[paste0("range", i)]][1])}),
          To = sapply(seq(1,length(originalrangedf$Characteristic),by=1), function(i) {
            isolate(input[[paste0("range", i)]][2])}))
      }
    })
    
    selected <<- rangedf()
    
    #filterdata <- reactive({
    #shinyWidgets::updateProgressBar(session = session, id = "pb", value = 0) # reinitialize to 0 if you run the calculation several times
    #session$sendCustomMessage(type = 'launch-modal', "my-modal") # launch the modal
    #print("updated range is:")
    #print(selected)
    filterdata <- filter_data(mergedata(),selected)
    shinyWidgets::updateProgressBar(session = session, id = "pb", value = 100/10*3.3)
    #session$sendCustomMessage(type = 'launch-modal', "my-modal") # launch the modal
    #withProgress(message = 'Step 1/3', detail = 'Filtering data: this may take a while...',value = 0,{})
    #})
    
    #calculatedata <- reactive({
    calculatedata <- calculate_data(filterdata)
    meancumreturn <- get_meancumreturn(calculatedata)
    summarydf <- get_summary(calculatedata)
    #print(class(summarydf))
    
    shinyWidgets::updateProgressBar(session = session, id = "pb", value = 100/10*6.6)
      #withProgress(message = 'Step 2/3', detail = 'Calculation in progress',value = 0,{})
    #})
    
    #meancumreturn <- reactive({
    #  get_meancumreturn(calculatedata())
    #})
    
    #summarydf <- reactive({
    #  get_summary(calculatedata())
    #})
    
    checkfilteredempty <- reactive({
      filteredmergedata <- filterdata
      validate(
        need(nrow(filterdata)!=0, 
             "ERROR: No data can be found as per your selected ranges. Please rescale them."))
    })
    
    #drawchart <- reactive({
    
      #withProgress(message = 'Step 3/3', detail = 'Plotting: finishing soon ...',value = 0,{})
    #})
    
    output$filteredempty <- renderText({
      checkfilteredempty()
    })
    if(nrow(filterdata!=0)){
      output$summary <- renderTable({
        isolate(summarydf)
        
      },digits=5)
      
      #drawchart()
      if(countvals$linecount == 0){
        currentplot <<- ggplot(meancumreturn) + 
          geom_line(aes(x = Group.1, y = x, colour = "current")) + scale_size_area() +
          #geom_line(aes(x = Group.1, y = x, colour = "current"), size=1.5) + scale_size_area() +
          xlab("Day relative to event date") + 
          ylab("Mean cumulative return") +
          coord_cartesian(xlim = c(input$window[1],input$window[2])) +
          theme(legend.position="none") +
          scale_color_manual(labels = c("current","previous"), values = c("#FF0000","#6E6E6E"))
        countvals$linecount <- countvals$linecount + 1
        #print(countvals$linecount)
        fullplot$pdata <- currentplot
        fullsummary$summarytable <- summarydf
        #print("full:", fullplot$layers[[1]]$aes_params$size)
        #print("current:", currentplot$layers[[1]]$aes_params$size)
      } else{
        #currentplot$layers[[countvals$linecount]]$aes_params$size <<- 0.5
        currentplot$layers[[countvals$linecount]]$mapping$colour <<- "previous"
        currentplot <<- currentplot +
          geom_line(data=meancumreturn, aes(x = Group.1, y = x, colour = "current")) +
          #geom_line(data=meancumreturn, aes(x = Group.1, y = x, colour = "current"), size=1.5) +
          coord_cartesian(xlim = c(input$window[1],input$window[2]))
        countvals$linecount <- countvals$linecount + 1
        print(countvals$linecount)
      }
      allplotvals$pdata <- currentplot
      shinyWidgets::updateProgressBar(session = session, id = "pb", value = 100)
    }
  })
  
  observeEvent(input$window,{
    allplotvals$pdata <- allplotvals$pdata +
      coord_cartesian(xlim = c(input$window[1],input$window[2]))
  })
  
  observeEvent(allplotvals$pdata,{
    output$meancumreturnplot <- renderPlot({
      allplotvals$pdata
    })
  })
  
  observeEvent(input$erase_plot,{
    #shinyjs::disable("erase_plot")
    #shinyjs::enable("erase_plot")
    allplotvals$pdata <- ggplot()
    countvals$linecount <- 0
    output$meancumreturnplot <- renderPlot({
      allplotvals$pdata
    })
    output$summary <- renderTable({
    })
  })
  
  observeEvent(input$reset_input,{
    #shinyjs::disable("erase_plot")
    #shinyjs::enable("erase_plot")
    currentplot <<- fullplot$pdata
    allplotvals$pdata <<- currentplot
    countvals$linecount <<- 1
    #allplotvals$layers[[1]]$aes_params$size <- 1.5
    output$meancumreturnplot <- renderPlot({
      allplotvals$pdata
    })
    output$summary <- renderTable({
      fullsummary$summarytable
    })
  })
})
