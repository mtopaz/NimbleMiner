####################################
# NimbleMiner: a software that allows clinicians to interact with word embedding models (skip-gram models - word2vec by package rword2vec) to rapidly create lexicons of similar terms.
# version: 1.0 (Model building, Search of similar terms)
# input .csv file with column TEXT should be specified 
#####################################

library(shiny)
library(data.table)
library(DT)
#library(rword2vec)
library(wordVectors)
library(magrittr)
library(plyr)
library(rsconnect)
 
# User interface 

ui <- fluidPage(
  includeCSS("styles.css"),
  navbarPage("NimbleMiner",
             tabPanel("1. Model building",
                    
                      fileInput(inputId = 'file_input','Please, upload .csv file with one column data:',accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), multiple = FALSE),
                      tags$hr(),
                      fluidRow(
                        column(4,sliderInput(inputId = "setting_window", 
                                            label = "Set a word window width:", 
                                            value = 10, min = 1, max = 10)),
                        column(6,img(src = "window_width_img.JPG"))
                      ),
                      sliderInput(inputId = "setting_min_count", 
                                  label = "Set a minimum count:", 
                                  value = 20, min = 1, max = 100),
                      sliderInput(inputId = "setting_similar_terms_count", 
                                  label = "How many similar terms should be presented for every simclin?", 
                                  value = 50, min = 5, max = 200),
                      tags$hr(),                      
                      actionButton("buildModel_click", "Build the model")                        
             ),
             
             tabPanel("2. Simclin finder ",

                      
                      wellPanel( h1('Simclin\'s'),
                                 textInput(inputId = 'newWord_input', label = 'Enter the new simclin'),
                                 actionButton("addNewWord_click", "Add"),
                                 hr(),
                                 DT::dataTableOutput(outputId = 'simclins_table'),
                                 actionButton("findSimilar_terms_click", "Find similar terms for new simclins"),
                                 actionButton("deleteSimclin_click", "Delete selected simclins")),
                      
                      wellPanel(
                        h1('New similar terms'),                      
                        DT::dataTableOutput(outputId = 'similar_terms_table'),
                        actionButton("saveAsSimclins_click", "Save selected similar terms as simclins"),
                        actionButton("clearSimilar_terms_click", "Clear all unselected similar terms")),
                        textInput( inputId = 'lastClickId', label = 'lastClickId' ),
                        textInput( inputId = 'lastClickSimilarTermId', label = 'lastClickSimilarTermId' )
                      
             ),
             
             tabPanel("Log ",
                      wellPanel(
                        h1('System efficacy'),
                        textInput(inputId = "trueSimilarTerms",label = 'Number of true similar terms'),
                        textInput(inputId = "suggestedSimilarTerms",label = 'Number of suggested similar terms'),
                        textInput(inputId = "systemEfficacy",label = 'System Efficacy')
                      ),
                      
                      
                      wellPanel(
                        h1('Log'),
                        DT::dataTableOutput(outputId = 'log_table'),
                        actionButton("saveAsFile_click", "Save log in file"),
                        actionButton("clearLog_click", "Clear log"))

             ),
             
             tabPanel("Irrelevant similar terms",
                      
                      
                      wellPanel(
                        h1('Irrelevant similar terms'),
                        DT::dataTableOutput(outputId = 'trash_similar_terms_table'),
                        actionButton("moveToSimclins_click", "Move selected similar terms to simclins\' list"))
                      
             )
             

             #tabPanel("3. Search EHRs by simclins"),


             
  )
  
)


# Server part

server <- function(input, output, session) {
  # Size limit for files (uploading) - 500Mb
  options(shiny.maxRequestSize=500*1024^2)   
  
  # set current apps directory as working directory
  app_dir = paste0(getSrcDirectory(function(x) {x}),"/");
  
  #in examples - chars before and after pattern 
  const_side_chars <- 20

  #TODO: user authentification
  currentUserId = 1  
  
  # txt file for examples
  if (file.exists("train2.txt")) {
    fileName <- paste0(app_dir,"train2.txt")
    source_txt <- readChar(fileName, file.info(fileName)$size)  
  } else source_txt <- ""
  
  ######################
  # Function loadSystemMetrics - read system metrics from the log and display it
  #####################  
  loadSystemMetrics <-function(){
    
    
    df_trueSimilarTerms_rows<-(subset(df_log, (df_log$Operation=='Update System Metrics' & df_log$Parameters=='trueSimilarTerms')))  
    df_trueSimilarTerms_rows<-df_trueSimilarTerms_rows[ order(df_trueSimilarTerms_rows$DateTime, na.last = TRUE, decreasing = TRUE), ]
    if (nrow(df_trueSimilarTerms_rows)>0) num_trueSimilarTerms <-as.numeric(df_trueSimilarTerms_rows[1,6])
    else num_trueSimilarTerms <- 0    
    
    df_suggestedSimilarTerms_rows<-(subset(df_log, (df_log$Operation=='Update System Metrics' & df_log$Parameters=='suggestedSimilarTerms')))  
    df_suggestedSimilarTerms_rows<-df_suggestedSimilarTerms_rows[ order(df_suggestedSimilarTerms_rows$DateTime, na.last = TRUE, decreasing = TRUE), ]
    if (nrow(df_suggestedSimilarTerms_rows)>0) num_suggestedSimilarTerms <-as.numeric(df_suggestedSimilarTerms_rows[1,6])
    else num_suggestedSimilarTerms <- 0    
    
    if (length(num_suggestedSimilarTerms)>0 & (!is.null(num_suggestedSimilarTerms)) & num_suggestedSimilarTerms>0)
      systemEfficacy = num_trueSimilarTerms / num_suggestedSimilarTerms
    else systemEfficacy = 0
    
    updateTextInput(session,"trueSimilarTerms",label = "Number of true similar terms", value = as.character(num_trueSimilarTerms))    
    updateTextInput(session,"suggestedSimilarTerms",label = "Number of suggested similar terms", value = as.character(num_suggestedSimilarTerms))    
    updateTextInput(session,"systemEfficacy",label = 'System Efficacy', value = as.character(round(systemEfficacy,2)))
  }
  
  
    
  # open log file
  df_log <-  read.csv(paste0(app_dir,"log.csv"),header = TRUE, col.names = c('DateTime','UserId','Operation','Parameters','ValueBefore','ValueAfter','Duration'),stringsAsFactors=FALSE)
  output$log_table <- DT::renderDataTable({DT::datatable(df_log,options = list(order=list(0,'desc')),rownames=FALSE, escape = FALSE)})
  loadSystemMetrics()
  
  # open file with irrelevant similar_terms
  df_similar_terms_trash <-  read.csv(paste0(app_dir,"trash.csv"),header = TRUE, col.names = c('Similar_terms'),stringsAsFactors=FALSE)
  output$trash_similar_terms_table <- DT::renderDataTable({DT::datatable(df_similar_terms_trash,rownames=FALSE, escape = FALSE)})  
  
  ######################
  # Function getExamples - returns string with 10 examples of appearance "pattern" string in "source_txt" text. 
  # The search of matches begins from the "last_pos" position in source_txt in forward direction (direction=1) or reverse direction (direction=0)
  #####################
  getExamples <- function(direction,pattern, last_pos,control_id){
    
    # txt file for examples
    if (is.na(source_txt) | (source_txt=="")){
      if (file.exists("train2.txt")) {
        fileName <- paste0(app_dir,"train2.txt")
        source_txt <- readChar(fileName, file.info(fileName)$size)  
      } else {
        return (paste0("<div class='example-text'> There is no active model for examples search!</div>"))
      }
    }

    #construct the pattern
    pattern_str <- paste0("\\b",pattern,"\\b")

    progress <- shiny::Progress$new(min=1, max=4)
    on.exit(progress$close())
    progress$set(message = "Search for examples.")    
    
    progress$set(value = 1, detail ="cut the source text for search...")
    
    exmpl_number<-0
    examples_str<-""
    offset_pos <-0
    
    #cut the source text in needed direction
    if (direction==1) 
      source_txt_in_direction = substring(source_txt,last_pos,nchar(source_txt))
    else 
      source_txt_in_direction = substring(source_txt,1,last_pos)
    
    #if source text is more than 20.000.000 chars, then divide it
    const_size_of_part_text <- 20000000
    
    if (nchar(source_txt_in_direction)>const_size_of_part_text)
      step_count<-round(nchar(source_txt_in_direction) / const_size_of_part_text) #TODO: words divided by boundaries will not be found
    else step_count<-1
    
    if (direction==1){
      step_boundary_start  <- 1
      step_boundary_finish <- step_count
    } else { 
      step_boundary_start <- step_count
      step_boundary_finish <- 1
    }  
    
    for (step in step_boundary_start:step_boundary_finish){
      
      if (step==step_boundary_finish){
        if (direction==1)
          next_part_of_source_txt = substring(source_txt_in_direction,const_size_of_part_text*(step-1)+1,nchar(source_txt_in_direction))
        else next_part_of_source_txt = substring(source_txt_in_direction,1,const_size_of_part_text*(step))
      }  
      else {
        if (direction==1)
          next_part_of_source_txt = substring(source_txt_in_direction,const_size_of_part_text*(step-1)+1,const_size_of_part_text*step)
        else next_part_of_source_txt = substring(source_txt_in_direction,const_size_of_part_text*(step-1)+1,const_size_of_part_text*step)
      }  
      
      #search the matches
      pos = gregexpr(pattern_str, next_part_of_source_txt,perl = TRUE)
      
      if (direction==1) {
        for (j in 1:length(pos[1][[1]])){
          if(!is.na(pos[1][[1]][j])){
            pos_start = pos[1][[1]][j]
            width_phrase =  attr(pos[1][[1]], 'match.length')[j]
            pos_before_start <- pos_start - const_side_chars
            boundary_addition <- ""
            if (pos_before_start<1) {
              if (last_pos+pos_before_start>1)
                boundary_addition <- substring(source_txt,last_pos+pos_before_start,last_pos-1)
              pos_before_start = 1
            }  
            
            pos_after_end <- pos_start + const_side_chars
            if (pos_after_end>file.info(fileName)$size) pos_after_end = file.info(fileName)$size

            new_example_str <- paste0("<div class='example-text'>...",substring(next_part_of_source_txt, pos_before_start-10, pos_before_start-1),
                                      "<span class='window-text'>",paste0(boundary_addition,substring(next_part_of_source_txt, pos_before_start, pos_after_end)),"</span>",
                                      substring(next_part_of_source_txt, pos_after_end+1, pos_after_end+10),"...</div>")
            
            new_example_str <- gsub(pattern, paste0("<b>",pattern,"</b>"), new_example_str)

            if (length(examples_str)>1) examples_str <-paste0(examples_str,'<br>')              
            examples_str <- paste0(examples_str,new_example_str)
            exmpl_number<-exmpl_number+1
            
            if (exmpl_number==10) break
          }
        }
      } else {
        for (j in length(pos[1][[1]]):1){
          if(!is.na(pos[1][[1]][j])){
            pos_start = pos[1][[1]][j]
            width_phrase =  attr(pos[1][[1]], 'match.length')[j]

            pos_before_start <- pos_start - const_side_chars
            if (pos_before_start<1) pos_before_start = 1
            
            pos_after_end <- pos_start + const_side_chars
            boundary_addition <- ""
            if (pos_after_end>nchar(next_part_of_source_txt)) {
              if (pos_after_end>file.info(fileName)$size) pos_after_end = file.info(fileName)$size
              boundary_addition <- substring(source_txt,nchar(next_part_of_source_txt)+1,pos_after_end)
              pos_after_end = nchar(next_part_of_source_txt)
            }
            new_example_str <- paste0("<div class='example-text'>...",substring(next_part_of_source_txt, pos_before_start-10, pos_before_start-1),
                                      "<span class='window-text'>",paste0(substring(next_part_of_source_txt, pos_before_start, pos_after_end),boundary_addition),"</span>",
                                      substring(next_part_of_source_txt, pos_after_end+1, pos_after_end+10),"...</div>")
            
            new_example_str <- gsub(pattern, paste0("<b>",pattern,"</b>"), new_example_str)

            if (length(examples_str)>1) examples_str <-paste0('<br>',examples_str)
            examples_str <- paste0(new_example_str,examples_str)
            exmpl_number<-exmpl_number+1
            if (exmpl_number==10) break
          }
        }
      }   
      if (exmpl_number==10 || direction==0) break
    } # for step
    
    #add prev and next buttons
    offset_pos <- const_size_of_part_text*(step-1)
    absolute_last_pos_match = offset_pos + pos_start

    if (direction==1){   #next
      examples_str<-paste0(examples_str," <button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"",control_id,"\",this.id);' id='prev_examples_for_",pattern,"#",
                           as.character(last_pos),"'><i class='icon-left'></i>Prev</button>")
      if (j<length(pos[1][[1]]) || step<step_count) #ToDO: next steps could not contain examples
        examples_str<-paste0(examples_str," <button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"",control_id,"\",this.id);' id='next_examples_for_",pattern,"#",
                             as.character(last_pos+absolute_last_pos_match+width_phrase),"'><i class='icon-right'></i>Next</button>")
    } else {  #prev
      if (j>1 || step>1)  #ToDO: prev steps could not contain examples 
        examples_str<-paste0(examples_str," <button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"",control_id,"\",this.id);' id='prev_examples_for_",pattern,"#",
                             as.character(absolute_last_pos_match),"'><i class='icon-left'></i>Prev</button>")                       
      examples_str<-paste0(examples_str," <button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"",control_id,"\",this.id);' id='next_examples_for_",pattern,"#",
                           as.character(last_pos),"'><i class='icon-left'></i>Next</button>")                     
    }
    progress$set(value = 4, detail ="show results...")
    
    return (examples_str)
  }
  
  ######################
  # Function logAction - adds the new record to the log
  #####################
  logAction <- function(actionDataTime=Sys.time(),userId,operation,parameters="",valueBefore="",valueAfter="",actionDuration=""){
    
    df_new_record <- data.frame(DateTime=format(actionDataTime, "%Y-%m-%d %H:%M:%S"), UserId = userId, Operation = operation, Parameters = parameters, ValueBefore=valueBefore,
                                ValueAfter=valueAfter,Duration=actionDuration, stringsAsFactors=FALSE)
    df_log <<- rbind(df_log,df_new_record)
    refreshTable('log')
  }  
  
  
  ######################
  # Function addNewSimclin - adds the new record to the simclin's list 
  #####################
  
  addNewSimclin <- function(new_simclin_str,fl_show_msg_about_duplicates = TRUE){
    
        # trim the new simclin
        new_simclin_str = gsub("[[:space:]]", "_", trimws(new_simclin_str))
        
        progress <- shiny::Progress$new(min=1, max=nrow(df_similar_terms)+1)
        on.exit(progress$close())
        progress$set(message = paste0("Add new simclin: ",new_simclin_str,". Search for examples."))    
        
        
        #check for duplicates
        if(nrow(df_simclins)>0 && any(df_simclins$Simclins==new_simclin_str)) {
          if (fl_show_msg_about_duplicates)
            showModal(modalDialog(title = "Error message",  paste0("The simclin \"",new_simclin_str,"\" is in the list already!"),easyClose = TRUE))
        } else {
          #add button 'Examples'
          examples_str <- paste0("<button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"lastClickId\",this.id);' id='next_examples_for_",new_simclin_str,"#0'><i class='icon-left'></i>Examples</button>")
          #add simclin to data frame          
          newSimclin_row=data.frame(Simclins = new_simclin_str,Processed = FALSE, Examples = examples_str,stringsAsFactors=FALSE)
          df_simclins<<-rbind(df_simclins,newSimclin_row)
          #clean the input control
          updateTextInput(session,"newWord_input", value = " ")  
          refreshTable('simclins')
          
        }
    
    
  }
  ######################
  # Function refreshTable - outputs data from dataframe to the table and save data to csv-file
  #####################
  
  refreshTable <- function(tableName, saveSelection = FALSE){
    
    if (tableName=='similar_terms') {
        if(saveSelection & !is.null(input$similar_terms_table_rows_selected)){
          output$similar_terms_table = DT::renderDataTable({DT::datatable(df_similar_terms,selection = list(target = 'row', selected=input$similar_terms_table_rows_selected),
                                                                     filter = 'top',
                                                                     options = list(order=list(1,'desc'),pageLength = 100,columnDefs = list(list(targets = c(1,2,3), searchable = FALSE))),rownames=FALSE, escape = FALSE)})
          write.csv(df_similar_terms, file = paste0(app_dir,"similar_terms.csv"))
        } else {
          output$similar_terms_table = DT::renderDataTable({DT::datatable(df_similar_terms,
                                                                     filter = 'top',
                                                                     options = list(order=list(1,'desc'),pageLength = 100,columnDefs = list(list(targets = c(1,2,3), searchable = FALSE))),rownames=FALSE, escape = FALSE)})
          write.csv(df_similar_terms, file = paste0(app_dir,"similar_terms.csv"))
        }
    } 
      else if (tableName=='simclins') {
        
        if(saveSelection & !is.null(input$simclins_table_rows_selected)){
          write.csv(df_simclins, file = paste0(app_dir,"simclins.csv"))        
          output$simclins_table = DT::renderDataTable(df_simclins,selection = list(selected=input$simclins_table_rows_selected),options = list(stateSave = TRUE),rownames=FALSE, escape = FALSE)          
          
        }  else {
          write.csv(df_simclins, file = paste0(app_dir,"simclins.csv"))        
          output$simclins_table = DT::renderDataTable(df_simclins, options = list(stateSave = TRUE),rownames=FALSE, escape = FALSE)        
        }
    } else if (tableName=='log'){
      output$log_table <- DT::renderDataTable({DT::datatable(df_log,options = list(order=list(0,'desc')),rownames=FALSE, escape = FALSE)})
      write.csv(df_log, file = paste0(app_dir,"log.csv"))
    } else if (tableName=='trash'){
      output$trash_similar_terms_table = DT::renderDataTable({DT::datatable(df_similar_terms_trash,rownames=FALSE, escape = FALSE)})
      write.csv(df_similar_terms_trash, file = paste0(app_dir,"trash.csv"))
    }
  }
  
  ######################
  # Function refreshSystemEfficacy - updates system efficacy and save it in log
  #####################
   
  refreshSystemEfficacyMetric <-function(metricName, metricValue){
    # get current metrics' values
    old_num_trueSimilarTerms <-as.numeric(input$trueSimilarTerms)
    if(is.na(old_num_trueSimilarTerms)) old_num_trueSimilarTerms <- 0    

    old_num_suggestedSimilarTerms <-as.numeric(input$suggestedSimilarTerms)
    if(is.na(old_num_suggestedSimilarTerms)) old_num_suggestedSimilarTerms <- 0    

    old_systemEfficacy <-as.numeric(input$systemEfficacy)
    if(is.na(old_systemEfficacy)) old_systemEfficacy <- 0      
            
    #update value of the current metric
    if(metricName=="trueSimilarTerms"){
      num_trueSimilarTerms = old_num_trueSimilarTerms+metricValue
      num_suggestedSimilarTerms = old_num_suggestedSimilarTerms
    }  
    else if(metricName=="suggestedSimilarTerms") {
      num_suggestedSimilarTerms = old_num_suggestedSimilarTerms+metricValue
      num_trueSimilarTerms = old_num_trueSimilarTerms
    }
    
    #update value of System Efficacy

    if (num_suggestedSimilarTerms>0)
      systemEfficacy = num_trueSimilarTerms / num_suggestedSimilarTerms
    else systemEfficacy = 0
    
    if(metricName=="trueSimilarTerms")
      logAction(userId = currentUserId,operation = 'Update System Metrics',parameters = "trueSimilarTerms",valueBefore=as.character(old_num_trueSimilarTerms),valueAfter = as.character(num_trueSimilarTerms))
    else if(metricName=="suggestedSimilarTerms")      
      logAction(userId = currentUserId,operation = 'Update System Metrics',parameters = "suggestedSimilarTerms",valueBefore=as.character(old_num_suggestedSimilarTerms),valueAfter = as.character(num_suggestedSimilarTerms))
    logAction(userId = currentUserId,operation = 'Update System Metrics',parameters = "systemEfficacy",valueBefore=as.character(old_systemEfficacy),valueAfter = as.character(round(systemEfficacy,2)))        
    
    loadSystemMetrics()
    
  }

  ######################
  # 1. Build the model
  #####################   
    
  observeEvent(input$buildModel_click, {
    #clean memory
    rm(list=ls())
    # create a progress object
    progress <- shiny::Progress$new(min=1, max=9)
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + 1 
      }
      progress$set(value = value, detail = detail)
    }
    on.exit(progress$close())
    
    library(readr)
    start_time <- Sys.time()  
    
    #pre-process the data-remove punctuation  
    progress$set(message = "Pre-process the data", value = 1)

    inFile <- input$file_input
    if (is.null(inFile))
    {
      showModal(modalDialog(title = "Error message",  paste0("Please, specify the .csv file with one column TEXT!"),easyClose = TRUE))
      return(NULL) 
    } 
    NOTEEVENTS <- read_csv(inFile$datapath)
      
    #exctract text only from column TEXT
    NOTEEVENTS_texts1= NOTEEVENTS$TEXT
      
    #rename to train 
    train<- NOTEEVENTS_texts1
      
    #pre-process the data-remove punctuation  
    progress$set(message = "Pre-process the data: ", value = 1, detail = "remove punctuation")
    train=gsub("[[:punct:]]", " ", train)
      
    # conver to lowerer case 
    updateProgress(detail = "conver to lowerer case")
    train=tolower(train)
      
    #write txt for model creation 
    updateProgress(detail = "write text for model creation")
    write(train,paste0(app_dir,"train2.txt"))
      
    #learn unigram model - here we need to give user the option to define the word window (window)
    updateProgress(detail = "learn unigram model")
    
    model = train_word2vec(paste0(app_dir,"train2.txt"),paste0(app_dir,"train.bin"),vectors=200,threads=4,window=input$setting_window,iter=5,negative_samples=0,force=TRUE)

    
    if (1<0) #rword2vec implementation
      {
        library("rword2vec")
  
    #create bigram trained word2vec here 
    updateProgress(detail = "create bigram trained word2vec")
    word2phrase(train_file = paste0(app_dir,"train2.txt"),output_file = paste0(app_dir,"train3.txt"))
    
    #create bigram model- the file path here needs to be without spaces- otheriwise it doesn't work!!! 
    updateProgress(detail = "create bigram model")
    model=word2vec(train_file = paste0(app_dir,"train3.txt"),output_file = paste0(app_dir,"train3.bin"),layer1_size = 300,min_count = input$setting_min_count,num_threads = 4,window = input$setting_window,sample = 0.001,binary=1)

    #create up to four gram model 
    updateProgress(detail = "create up to four gram model")
    word2phrase(train_file = paste0(app_dir,"train3.txt"),output_file = paste0(app_dir,"train4.txt"))
    
    updateProgress(detail = "create final model")      
    model=word2vec(train_file = paste0(app_dir,"train4.txt"),output_file = paste0(app_dir,"train4.bin"),layer1_size = 300,min_count = input$setting_min_count,num_threads = 4,window = input$setting_window,sample = 0.001,binary=1)
    
    }
    
    updateProgress(detail = "the model is ready!")

    logAction(actionDataTime = start_time, userId = currentUserId, operation = "Build the model",parameters = paste0("Window: ",input$setting_window,"; min number: ",input$setting_min_count,"; similar terms count: ",input$setting_similar_terms_count,
               "; source file: ", input$file_input['name'],"."), actionDuration = round((Sys.time() - start_time),2))
  })

  ######################
  # 2. Simclins finder
  #####################  
  
  # Uploading data from files
  # Simclins upload

  df_simclins <-  read.csv(paste0(app_dir,"simclins.csv"),header = TRUE, col.names = c('Simclins','Processed','Examples'),stringsAsFactors=FALSE)
  output$simclins_table <- DT::renderDataTable({DT::datatable(df_simclins,options = list(pageLength = 10,stateSave = TRUE,order = list(list(1, 'asc'))),rownames=FALSE, escape = FALSE)})

  # Similar_terms uploading
  df_similar_terms <- read.csv(paste0(app_dir,"similar_terms.csv"),header = TRUE, col.names = c('Similar_terms','Distance','By_simclins','Examples'),stringsAsFactors=FALSE)
  output$similar_terms_table = DT::renderDataTable({DT::datatable(df_similar_terms,filter = 'top',options = list(pageLength = 100,order=list(1,'desc'),columnDefs = list(list(targets = c(1,2,3), searchable = FALSE))),rownames=FALSE, escape = FALSE)})
  
  if (file.exists(paste0(app_dir,"train.bin"))) 
    model = read.vectors(paste0(app_dir,"train.bin"))
  else model = NULL
  
  
  # Add new simclin by user
  observeEvent(input$addNewWord_click, {
    
    addNewSimclin(input$newWord_input)
    # save user action in log file
    logAction(userId = currentUserId, operation = "Add user simclin",valueAfter=input$newWord_input)
    
  })  
  ######################
  # Function dataModal - display modal window with examples` list
  #####################   
  dataModal <- function(htmlContent, failed = FALSE) {
    showModal(modalDialog(inputId = 'dialog_examples', title = "Examples", HTML(htmlContent),easyClose = TRUE, footer = modalButton("Close") ))
  }
  
  # Show/hide next 10 examples for simclin
  observeEvent(input$lastClickId,
               {
                 if (input$lastClickId%like%"next_examples_for_" || input$lastClickId%like%"prev_examples_for_")
                 {
                   if (input$lastClickId%like%"next_examples_for_"){
                     fl_direction <- 1     #next
                   }
                   else {
                     fl_direction <- 0          #prev
                   }
                   
                   #get position of previous matches
                   
                   width_phrase = 0
                   if (fl_direction==1)
                    id_str=gsub("next_examples_for_","",input$lastClickId)
                   else
                    id_str=gsub("prev_examples_for_","",input$lastClickId)                   
                   pos_prev_search=regexpr("#",id_str)
                   simclin_str = substring(id_str,1,pos_prev_search[1][[1]]-1)
                   last_pos_str=substring(id_str,pos_prev_search[1][[1]]+1)   
                   last_pos_int = as.numeric(last_pos_str) #first letter of last previous match

                   dataModal(getExamples(fl_direction,simclin_str,last_pos_int,'lastClickId'))
                 } 

               }
  )
  
  # Delete selected simclins - TODO: 1.get user confirmation 
  observeEvent(input$deleteSimclin_click, {
   
    if(!is.null(input$simclins_table_rows_selected)){
      # save selected simclins in trash      
      df_selected_rows = data.table (Similar_terms=df_simclins[input$simclins_table_rows_selected,"Simclins"])
      deleted_simclins_str = paste0(as.character(nrow(df_selected_rows))," simclins deleted: ",paste(df_selected_rows[['Similar_terms']],collapse=" "))
      logAction (userId = currentUserId, operation = "Delete simclins", valueAfter = deleted_simclins_str)          
      df_similar_terms_trash <<- rbind(df_similar_terms_trash, df_selected_rows)
      #exclude duplicates in list
      df_similar_terms_trash<<-df_similar_terms_trash[ order(df_similar_terms_trash$Similar_terms, na.last = TRUE, decreasing = FALSE), ]
      df_similar_terms_trash<<-unique( df_similar_terms_trash[ , 1 ] )
      refreshTable('trash')

      df_simclins <<- df_simclins[-input$simclins_table_rows_selected,]
      refreshTable('simclins')
    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected simclins to delete!",easyClose = TRUE))
    }
   
  })
  

  # Find Similar_terms- presents those to the user iteratively and exclude duplicates the user identifyes 
  observeEvent(input$findSimilar_terms_click, {
    
  progress <- shiny::Progress$new(min=1, max=nrow(df_simclins[df_simclins[,'Processed']==FALSE,]))
  on.exit(progress$close())
  progress$set(message = 'Find Similar_terms for:')
    
  # for each new simclin get num closest words  
  df_new_simclins=df_simclins[df_simclins[,'Processed']==FALSE,] 
  if (nrow(df_new_simclins)>0) {
   for(i in 1:nrow(df_new_simclins)) {
        start_time <- Sys.time()
        row <- df_new_simclins[i,]
        simclin_str <- as.character(row$Simclins)
        progress$set(value = i, detail =simclin_str)        
        # get num closest words 
        #df_new_similar_terms = distance(file_name = "train3.bin",search_word = as.character(row$Simclins),num = input$setting_similar_terms_count)
        if (is.null(model)) 
        { 
          showModal(modalDialog(title = "Error message",  "There is no model! Please, build the model before.",easyClose = TRUE))
          return (NULL)
        } 
        
        
        df_new_similar_terms = closest_to(model,simclin_str,input$setting_similar_terms_count,FALSE)
        # exclude empty rows 
        df_new_similar_terms <-  df_new_similar_terms[!(is.na(df_new_similar_terms$word) | df_new_similar_terms$word==""), ] 
        if (nrow(df_new_similar_terms)>0) {
          # prepare new dataframe for rbind command (adding to list of similar_terms)   - create the same structure as df_similar_terms
            # rename column with distance
          colnames(df_new_similar_terms)[2] <-"Distance"
            # create column with simclin name
          df_new_similar_terms$By_simclins <- simclin_str
            # change col name Word to Similar_terms  
          colnames(df_new_similar_terms)[1] <-"Similar_terms"
            # change type of first colm  
          factor_cols <- sapply(df_new_similar_terms, is.factor)
          df_new_similar_terms[factor_cols] <- lapply(df_new_similar_terms[factor_cols], as.character)
          df_new_similar_terms[,'Distance']=round(as.numeric(df_new_similar_terms[,'Distance']),2)
            # create column with examples
          df_new_similar_terms$Examples <- ""
          #how many new similar terms were found 
          new_similar_terms_count<-nrow(subset(df_new_similar_terms, !(df_new_similar_terms$Similar_terms %in% df_similar_terms$Similar_terms) & !(df_new_similar_terms$Similar_terms %in% df_similar_terms_trash$Similar_terms)))  
          #how many similar terms were found before
          duplicated_similar_terms_count<-nrow(subset(df_new_similar_terms, (df_new_similar_terms$Similar_terms %in% df_similar_terms$Similar_terms)))  
          #how many similar terms were deleted by user before
          irrelevant_similar_terms_count <- nrow(subset(df_new_similar_terms, (df_new_similar_terms$Similar_terms %in% df_similar_terms_trash$Similar_terms)))   
          #add all closest words to the  list of Similar terms
          df_similar_terms <<- rbind(df_similar_terms, df_new_similar_terms)
          
          refreshSystemEfficacyMetric ('suggestedSimilarTerms',new_similar_terms_count)  
          new_similar_terms_str <- paste0(as.character(new_similar_terms_count)," similar_terms found ( ",as.character(irrelevant_similar_terms_count)," were deleted before by user as irrelevant, ",as.character(duplicated_similar_terms_count)," are duplicates of obtained before): ",paste(df_new_similar_terms[['Similar_terms']],collapse=" "))          
        } else {
          new_similar_terms_str <- "0 similar_terms found."
        }
        # mark this simclin as processed and update number of found similar_terms
        df_simclins[df_simclins[,'Simclins']==simclin_str,'Processed'] <<-TRUE

        rm(df_new_similar_terms)
        finish_time <-Sys.time()
        
        logAction(actionDataTime <- start_time,userId = currentUserId,operation = 'System search similar_terms for simclin',parameters = paste0("Simclin: ",simclin_str),valueAfter = new_similar_terms_str,actionDuration = round(finish_time-start_time,2))
   }
    refreshTable('simclins')
    
    progress$close()
    search_start_time <-Sys.time()
    
    if (nrow(df_similar_terms)>0) {

        #exclude duplicates in updated list of similar_terms
        df_similar_terms<<-df_similar_terms[ order(df_similar_terms$Distance, na.last = TRUE, decreasing = TRUE), ]
        df_similar_terms$Examples <<-NULL
        df_similar_terms<<-aggregate(. ~ Similar_terms,data = df_similar_terms,toString)
        ## restore examples after aggregation        
        for(i in 1:nrow(df_similar_terms)) {
          df_similar_terms[i,'Examples'] <<- paste0("<button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"lastClickSimilar_termId\",this.id);' id='next_examples_for_",df_similar_terms[i,'Similar_terms'],"#0'><i class='icon-left'></i>Examples</button>")
        }  
        
        #exclude duplicates with simclins
        df_similar_terms<<- subset(df_similar_terms, !(df_similar_terms$Similar_terms %in% df_simclins$Simclins))
        
        #delete similar_terms which are in trash
        df_similar_terms<<- df_similar_terms[!(df_similar_terms$Similar_terms %in% df_similar_terms_trash$Similar_terms),]
        # order distances for each similar_term
        for(i in 1:nrow(df_similar_terms)) {
          distance_vector = unlist(strsplit(as.character(df_similar_terms[i,2]), split=", "))
          simclins_vector = unlist(strsplit(as.character(df_similar_terms[i,3]), split=", "))
          distances_of_synonim_df <- data.frame(distance=distance_vector, simclins=simclins_vector)
          distances_of_synonim_df<-distances_of_synonim_df[ order(distances_of_synonim_df$distance, na.last = TRUE, decreasing = TRUE), ]
          df_similar_terms[i,2]<<-toString(distances_of_synonim_df$distance)
          df_similar_terms[i,3]<<-toString(distances_of_synonim_df$simclins)
          rm(distances_of_synonim_df)
        }
        
        refreshTable('similar_terms')
      }  
      progress$set(detail ='Similar_terms search has completed!')
      search_duration <- Sys.time() - search_start_time
      showModal(modalDialog(title = "Similar_terms search","Similar terms` search has completed!",easyClose = TRUE))
    } else {
     showModal(modalDialog(
       title = "Error message",
       "There are no new simclins for search!",
       easyClose = TRUE
     ))
    }
  
  })
  
  # Show/hide next 10 examples for similar_terms
  observeEvent(input$lastClickSimilar_termId,
               {
                 if (input$lastClickSimilar_termId%like%"next_examples_for_" || input$lastClickSimilar_termId%like%"prev_examples_for_")
                 {
                   if (input$lastClickSimilar_termId%like%"next_examples_for_"){
                     fl_direction <- 1     #next
                   }
                   else {
                     fl_direction <- 0          #prev
                   }
                   
                   #get position of previous matches
                   
                   width_phrase = 0
                   if (fl_direction==1)
                     id_str=gsub("next_examples_for_","",input$lastClickSimilar_termId)
                   else
                     id_str=gsub("prev_examples_for_","",input$lastClickSimilar_termId)                   
                   pos_prev_search=regexpr("#",id_str)
                   similar_term_str = substring(id_str,1,pos_prev_search[1][[1]]-1)
                   last_pos_str=substring(id_str,pos_prev_search[1][[1]]+1)   
                   last_pos_int = as.numeric(last_pos_str) #first letter of last previous match
                   
                   dataModal(getExamples(fl_direction,similar_term_str,last_pos_int,'lastClickSimilar_termId'))
                 } 
                 
               }
  )  
  
  # Save selected similar terms as simclins 
  observeEvent(input$saveAsSimclins_click, {
    if(!is.null(input$similar_terms_table_rows_selected)){
      df_selected_similar_terms <<- df_similar_terms[input$similar_terms_table_rows_selected,]
      df_selected_similar_terms$By_simclins <- NULL
      df_selected_similar_terms$Processed <- FALSE
      colnames(df_selected_similar_terms)[1] <-"Simclins"
      df_selected_similar_terms$Distance <- NULL
      df_selected_similar_terms$Examples <- gsub('lastClickSimilarTermId', 'lastClickId', df_selected_similar_terms$Examples)
      df_simclins <<- rbind(df_simclins, df_selected_similar_terms)
      for (i in 1:nrow(df_selected_similar_terms)) 
        logAction (userId = currentUserId, operation = "Select similar term as simclin", parameters = paste0("Selected similar term: ",df_selected_similar_terms[i,'Simclins']))
      
      refreshSystemEfficacyMetric('trueSimilarTerms',nrow(df_selected_similar_terms))

      rm(df_selected_similar_terms)
      df_similar_terms <<- df_similar_terms[-input$similar_terms_table_rows_selected,]
      
      
      refreshTable('similar_terms')
      refreshTable('simclins')

    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected similar terms!",easyClose = TRUE))
    }
  })
  
  # Clear all unselected similar terms TODO: 1.get user confirmation
  observeEvent(input$clearSimilar_terms_click, {
    # save unselected similar_terms in trash
    if(!is.null(input$similar_terms_table_rows_selected))
      df_unselected_similar_terms = data.table (Similar_terms=df_similar_terms[-input$similar_terms_table_rows_selected,"Similar_terms"])
    else 
      df_unselected_similar_terms <<- data.table (Similar_terms=df_similar_terms[,"Similar_terms"])
    
    deleted_similar_terms_str = paste0(as.character(nrow(df_unselected_similar_terms))," similar terms deleted: ",paste(df_unselected_similar_terms[['Similar_terms']],collapse=" "))
    logAction (userId = currentUserId, operation = "Delete similar terms", valueAfter = deleted_similar_terms_str)    
    
    df_similar_terms_trash <<- rbind(df_similar_terms_trash, df_unselected_similar_terms)
    #exclude duplicates in list
    df_similar_terms_trash<<-df_similar_terms_trash[ order(df_similar_terms_trash$Similar_terms, na.last = TRUE, decreasing = FALSE), ]
    df_similar_terms_trash<<-unique( df_similar_terms_trash[ , 1 ] )
    
    write.csv(df_similar_terms_trash, file = paste0(app_dir,"trash.csv"))

    # clear unselected similar_terms
    df_similar_terms <<- df_similar_terms[input$similar_terms_table_rows_selected,]
    
    refreshTable('similar_terms')
    
  })
  
  ######################
  # 3. Log
  ##################### 
  
  # Clear log - TODO: 1.get user confirmation 
  observeEvent(input$clearLog_click, {
    df_log <<- df_log[0,]
    refreshTable('log')
    updateTextInput(session,"trueSimilarTerms",label = "Number of true similar terms", value = 0)    
    updateTextInput(session,"suggestedSimilarTerms",label = "Number of suggested similar terms", value = 0)    
    updateTextInput(session,"systemEfficacy",label = 'System Efficacy', value = 0)
  })
  
  # Save current log to the file
  
  observeEvent(input$saveAsFile_click, {
      fileName <- paste0(app_dir,"logs/log-",format(Sys.time(), "%Y%m%d-%H%M%S"),".csv")
      write.csv(df_log, file = fileName)
      showModal(modalDialog(title = "Save log as file",  paste0("The log was saved in the file ",fileName,"."),easyClose = TRUE))
  })
  
  ######################
  # 4. Irrelevant similar terms
  #####################  
  
  # Save selected similar_terms as simclins 
  observeEvent(input$moveToSimclins_click, {
    
    if(!is.null(input$trash_similar_terms_table_rows_selected)){
      
      df_selected_rows <-  data.table (df_similar_terms_trash[input$trash_similar_terms_table_rows_selected,])
      for(i in 1:nrow(df_selected_rows)){
        new_simclin = as.character(df_selected_rows[i,1])
        addNewSimclin(new_simclin, FALSE)
        
        # save user action in log file
        logAction(userId = currentUserId, operation = "Select simclin from trash",valueAfter=new_simclin)
      }
      
      df_similar_terms_trash <<- data.table (Similar_terms=df_similar_terms_trash[-input$trash_similar_terms_table_rows_selected,])
      refreshTable('trash')

    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected similar terms!",easyClose = TRUE))
    }
  })
  

  
  
}

shinyApp(ui = ui, server = server)
