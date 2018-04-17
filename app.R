####################################
# NimbleMiner: a software that allows clinicians to interact with word embedding models (skip-gram models - word2vec by package wordVectors) to rapidly create lexicons of similar terms.
# version: 2.0 (Model building, Search of similar terms,negations, irrelevant similar terms)
# input .csv file with column TEXT should be specified 
#####################################

library(shiny)
library(data.table)
library(DT)
library(wordVectors)
library(magrittr)
library(plyr)
library(rsconnect)
library(stringi)
library(shinythemes) 

# User interface 

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  includeCSS("styles.css"),
  navbarPage("NimbleMiner",
             tabPanel("1. Model builder",
                    
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
                      actionButton("buildModel_click", "Build the model", icon = icon("play")),
                      tags$hr(),                      
                      icon = icon("cogs")
             ),
             
             tabPanel("2. Simclin explorer",

                      
                      wellPanel( h1('Simclins'),
                                 textInput(inputId = 'newWord_input', label = 'Enter new simclin'),
                                 actionButton("addNewWord_click", "Add", icon = icon("plus")),
                                 hr(),
                                 DT::dataTableOutput(outputId = 'simclins_table'),
                                 actionButton("findSimilar_terms_click", "Find similar terms for new simclins", icon = icon("search-plus")),
                                 #actionButton("addExampleButton_click", "Add example button", icon = icon("smile")),
                                 actionButton("deleteSimclin_click", "Delete selected simclins", icon = icon("trash"))),
                      
                      wellPanel(
                        h1('New similar terms'),                      
                        DT::dataTableOutput(outputId = 'similar_terms_table'),
                        actionButton("saveAsSimclins_click", "Save selected similar terms as simclins", icon = icon("arrow-up")),
                        actionButton("clearSimilar_terms_click", "Clear all unselected similar terms", icon = icon("trash")),
                        textInput( inputId = 'lastClickId', label = 'lastClickId' ),
                        textInput( inputId = 'lastClickSimilarTermId', label = 'lastClickSimilarTermId' )
                      ),
                      icon = icon("search-plus", class = NULL, lib = "font-awesome")
                      
             ),
             
             tabPanel("3. Irrelevant similar terms explorer",
                      
                      
                      wellPanel(
                        h1('New irrelevant similar terms'), 
                        textInput(inputId = 'newIrrSimilarTerm_input', label = 'Enter new irrelevant similar term'),
                        actionButton("addNewIrrSimilarTerm_click", "Add irrelevant similar term", icon = icon("plus")),
                        hr(),
                        DT::dataTableOutput(outputId = 'irrelevant_similar_terms_table'),
                        actionButton("moveToSimclins_click", "Move selected similar terms to simclins\' list", icon = icon("undo")),
                        actionButton("deleteIrrSimilarTerm_click", "Delete selected similar terms", icon = icon("eraser")))
                      ,icon = icon("search-minus", class = NULL, lib = "font-awesome")
                      
             ),
             
             
             
             tabPanel("4. Negation explorer",
                      
                      
                      wellPanel(
                        navbarPage("Negations",
                          tabPanel("Pre & post negations",       
                                fluidRow(column(5,textInput(inputId = 'newPrepNegation_input', label = 'Enter new negation BEFORE simclin:')), 
                                         column(2,p(id="simclinLabel","Simclin")),
                                         column(5,textInput(inputId = 'newPostNegation_input', label = 'Enter new negation AFTER simclin:'))),
                                fluidRow(column(12,actionButton("addNewNegation_click", "Add negation(s)"))),
                                hr(),
                                DT::dataTableOutput(outputId = 'negations_table'),
                                actionButton("deleteNegation_click", "Delete selected negations", icon = icon("eraser"))
                          ),
                          tabPanel("Exceptions",       
                                   h1('New exception'), 
                                   textInput(inputId = 'newException_input', label = 'Enter new exception:'),
                                   actionButton("addNewException_click", "Add exception", icon = icon("plus")),
                                   hr(),
                                   DT::dataTableOutput(outputId = 'exceptions_table'),
                                   actionButton("deleteException_click", "Delete selected exceptions", icon = icon("eraser"))
                          )
                        )    
                    )
                    ,icon = icon("times-circle")
             ),
             
             tabPanel("5. Assign and review labels",
                      wellPanel(
                          h1('Data labeling'),  
                          fileInput(inputId = 'fileEHR_input','Please, upload .csv file with column "Note":',accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), multiple = FALSE),
                          actionButton("makeLabels_click","Assign labels",icon = icon("tags")),
                          hr(),
                          navbarPage("Labeled data",
                          tabPanel("With simclins",
                                   wellPanel(
                                      DT::dataTableOutput(outputId = 'posLabeledData_table')
                                   ),
                                   icon = icon("search-plus")    
                                  ),
                          tabPanel("With irrelevant similar terms",
                                   wellPanel(
                                     DT::dataTableOutput(outputId = 'posIrrelevantLabeledData_table')
                                   ),
                                   icon = icon("search-minus")  
                          ),                          
                          tabPanel("With negations",
                                   wellPanel(
                                     DT::dataTableOutput(outputId = 'posNegatedLabeledData_table')
                                   ),icon = icon("times-circle")  
                                  ),
                          tabPanel("Visualization",
                                   wellPanel(
                                     h1('Labeling statistics'),
                                     fluidRow(
                                       column(2,'Total of notes'),
                                       column(2,'Total of positive notes'),
                                       column(3,'Total of notes with negated simclins'),
                                       column(3,'Total of notes with irrelevant terms'),
                                       column(2,'Total of negative  notes')
                                     ),
                                     fluidRow(
                                       column(2,textInput(inputId = "totalNotes",label = '')),
                                       column(2,textInput(inputId = "totalPositiveNotes",label = '')),
                                       column(3,textInput(inputId = "totalNegatedPositiveNotes",label = '')),
                                       column(3,textInput(inputId = "totalIrrelevantPositiveNotes",label = '')),
                                       column(2,textInput(inputId = "totalNegativeNotes",label = ''))
                                     )
                                   ),
                                   wellPanel(
                                     fluidRow(
                                       column(6,
                                              #plotlyOutput("distPlot") 
                                              plotOutput("allLabelsPlot")),
                                       column(6,
                                              plotOutput("posLabelsPlot"))
                                     )   
                                   ),icon = icon("stats", lib = "glyphicon")
                          )
                          
                          )
                      
                      ),
                      
                      icon = icon("tags")
              ),
             tabPanel("Log ",
                      wellPanel(
                        h1('System efficacy'),
                        textInput(inputId = "trueSimilarTerms",label = 'Number of simclins'),
                        textInput(inputId = "suggestedSimilarTerms",label = 'Number of suggested similar terms'),
                        textInput(inputId = "systemEfficacy",label = 'System efficacy (number of simclins/Number of suggested similar terms)')
                      ),
                      
                      
                      wellPanel(
                        h1('Log'),
                        DT::dataTableOutput(outputId = 'log_table'),
                        actionButton("saveAsFile_click", "Save log in file", icon = icon("save")),
                        actionButton("clearLog_click", "Clear log", icon = icon("eraser"))),
                      icon = icon("archive")

             )
             
             
             

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

  #in negations - words between term and negation
  const_distance_between_simclin_and_negation <- 2  
  
  
  #TODO: user authentification
  currentUserId = 1  
  
  # txt file for examples of simclins and similar_terms
  if (file.exists("train2.txt")) {
    fileName <- paste0(app_dir,"train2.txt")
    source_txt <- readChar(fileName, file.info(fileName)$size)  
  } else source_txt <- ""

  model = NULL  
    
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
    
    rm(df_trueSimilarTerms_rows)
    rm(df_suggestedSimilarTerms_rows)
  }
  
  ######################
  # Function loadLabelingStatistics - read labeling statistics from the log and display it
  #####################  
  loadLabelingStatistics <-function(){
    
    
    df_totalNotes_rows<-(subset(df_log, (df_log$Operation=='Labeling' & df_log$Parameters=='totalNotes')))  
    df_totalNotes_rows<-df_totalNotes_rows[ order(df_totalNotes_rows$DateTime, na.last = TRUE, decreasing = TRUE), ]
    if (nrow(df_totalNotes_rows)>0) num_totalNotes <-as.numeric(df_totalNotes_rows[1,6])
    else num_totalNotes <- 0  
    
    
    df_positiveNotes_rows<-(subset(df_log, (df_log$Operation=='Labeling' & df_log$Parameters=='totalPositiveNotes')))  
    df_positiveNotes_rows<-df_positiveNotes_rows[ order(df_positiveNotes_rows$DateTime, na.last = TRUE, decreasing = TRUE), ]
    if (nrow(df_positiveNotes_rows)>0) num_totalPositiveNotes <-as.numeric(df_positiveNotes_rows[1,6])
    else num_totalPositiveNotes <- 0    
    
    df_negatedNotes_rows<-(subset(df_log, (df_log$Operation=='Labeling' & df_log$Parameters=='totalNegatedPositiveNotes')))  
    df_negatedNotes_rows<-df_negatedNotes_rows[ order(df_negatedNotes_rows$DateTime, na.last = TRUE, decreasing = TRUE), ]
    if (nrow(df_negatedNotes_rows)>0) num_totalNegatedPositiveNotes <-as.numeric(df_negatedNotes_rows[1,6])
    else num_totalNegatedPositiveNotes <- 0    
    
    df_irrelevantNotes_rows<-(subset(df_log, (df_log$Operation=='Labeling' & df_log$Parameters=='totalIrrelevantPositiveNotes')))  
    df_irrelevantNotes_rows<-df_irrelevantNotes_rows[ order(df_irrelevantNotes_rows$DateTime, na.last = TRUE, decreasing = TRUE), ]
    if (nrow(df_irrelevantNotes_rows)>0) num_totalIrrelevantPositiveNotes <-as.numeric(df_irrelevantNotes_rows[1,6])
    else num_totalIrrelevantPositiveNotes <- 0    

    df_negativeNotes_rows<-(subset(df_log, (df_log$Operation=='Labeling' & df_log$Parameters=='totalNegativeNotes')))  
    df_negativeNotes_rows<-df_negativeNotes_rows[ order(df_negativeNotes_rows$DateTime, na.last = TRUE, decreasing = TRUE), ]
    if (nrow(df_negativeNotes_rows)>0) num_totalNegativeNotes <-as.numeric(df_negativeNotes_rows[1,6])
    else num_totalNegativeNotes <- 0    
    
    
    updateTextInput(session,"totalNotes",value = as.character(num_totalNotes))
    updateTextInput(session,"totalPositiveNotes",value = as.character(num_totalPositiveNotes))    
    updateTextInput(session,"totalIrrelevantPositiveNotes",value = as.character(num_totalIrrelevantPositiveNotes))
    updateTextInput(session,"totalNegatedPositiveNotes",value = as.character(num_totalNegatedPositiveNotes))        
    updateTextInput(session,"totalNegativeNotes",value = as.character(num_totalNegativeNotes))      
   
    rm(df_totalNotes_rows)
    rm(df_positiveNotes_rows)
    rm(df_negatedNotes_rows)
    rm(df_irrelevantNotes_rows)
    rm(df_negativeNotes_rows)
    
    sum_pos_total = num_totalPositiveNotes + num_totalNegativeNotes
    percent_totalPositiveNotes = round(num_totalPositiveNotes/sum_pos_total*100,2)
    percent_num_totalNegativeNotes = round(100 - percent_totalPositiveNotes,2)
    
    df_statistics_all_labels <- data.frame(
      Label = c("Positive", "Negative"),
      total = c(percent_totalPositiveNotes, percent_num_totalNegativeNotes)
    )
    
    sum_pos_total = num_totalPositiveNotes + num_totalNegatedPositiveNotes + num_totalIrrelevantPositiveNotes
    percent_totalPositiveNotes = round(num_totalPositiveNotes/sum_pos_total*100,2)
    percent_totalNegatedPositiveNotes = round(num_totalNegatedPositiveNotes/sum_pos_total*100,2)
    percent_totalIrrelevantPositiveNotes = round(100-percent_totalPositiveNotes-percent_totalNegatedPositiveNotes,2)
    
    df_statistics_pos_labels <- data.frame(
      Label = c("Positive", "Negated positive", "Irrelevant positive"),
      total = c(percent_totalPositiveNotes, percent_totalNegatedPositiveNotes, percent_totalIrrelevantPositiveNotes),
      text = c(paste0(percent_totalPositiveNotes,"%"),paste0(percent_totalNegatedPositiveNotes,"%"),paste0(percent_totalIrrelevantPositiveNotes,"%"))
    )

    library(ggplot2)

    output$allLabelsPlot <-   renderPlot({
        bp<-  ggplot(data = df_statistics_all_labels, aes(x = "", y = total, fill = Label)) + 
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste0(total,"%")), position = position_stack(vjust = 0.5),size=6,fontface = "bold",colour = "#525760") +
          coord_polar(theta = "y")+
          theme(panel.background = element_blank(),
                text = element_text(size=14),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                plot.title = element_text(face = "bold", hjust = 0.5, color="#317eac"),
                legend.text=element_text(size=14))+
          ggtitle("Notes with positive and negative labels")+
          labs(size=14)+
          scale_fill_manual(values = (c("#F08080","#3CB371")))
          bp
    })
    
    output$posLabelsPlot <-   renderPlot({

      bp<- ggplot(df_statistics_pos_labels, aes(x="", y=total, fill=Label))+
        geom_bar(width = 1, stat = "identity")  +
        geom_text(aes(label = paste0(total,"%")), position = position_stack(vjust = 0.5),size=6, fontface="bold",colour = "#525760") +
        theme(panel.background = element_blank(),
              text = element_text(size=14),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              plot.title = element_text(face = "bold", hjust = 0.5, color="#317eac"),
              legend.text=element_text(size=14))+
        ggtitle("Ratio of notes with true, negated and irrelevant positive labels")+
        labs(size=14)+
        scale_fill_manual(values = (c("#ff6600","#F08080","#3CB371")))
      bp
      
    })
  }  
    
  # open log file
  df_log <-  read.csv(paste0(app_dir,"log.csv"),header = TRUE, col.names = c('DateTime','UserId','Operation','Parameters','ValueBefore','ValueAfter','Duration'),stringsAsFactors=FALSE)
  output$log_table <- DT::renderDataTable({DT::datatable(df_log,options = list(order=list(0,'desc')),colnames=c('Date&time','User Id','Operation','Parameters','Value before','Value after','Duration'),rownames=FALSE, escape = FALSE)})
  loadSystemMetrics()
  
  # open file with irrelevant similar_terms
  df_irrelevant_terms <-  read.csv(paste0(app_dir,"irrelevant_terms.csv"),header = TRUE, col.names = c('Similar_term','Frequency','Examples'),stringsAsFactors=FALSE)
  output$irrelevant_similar_terms_table <- DT::renderDataTable({DT::datatable(df_irrelevant_terms, options = list(order=list(1,'desc')), colnames=c('Similar term','Frequency (%)','Examples'), rownames=FALSE, escape = FALSE)})  
  
  # open file with negations
  df_negations <-  read.csv(paste0(app_dir,"negations.csv"),header = TRUE, col.names = c('Negation','Type','Frequency','Examples'),stringsAsFactors=FALSE)
  output$negations_table <- DT::renderDataTable({DT::datatable(df_negations,options = list(order=list(2,'desc')),colnames = c('Negation','Type','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)})
  
  df_exceptions <-  read.csv(paste0(app_dir,"negations-exceptions.csv"),header = TRUE, col.names = c('Exception','Frequency','Examples'),stringsAsFactors=FALSE)
  output$exceptions_table <- DT::renderDataTable({DT::datatable(df_exceptions,options = list(order=list(1,'asc')),colnames = c('Exception','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)})  
  
  # open file with positive labeled data
  df_positiveLabels <-  read.csv(paste0(app_dir,"pos_labeled_data.csv"),header = TRUE, col.names = c('Note'), stringsAsFactors=FALSE,colClasses = c("character","character"),comment.char = "")
  output$posLabeledData_table <- DT::renderDataTable({DT::datatable(df_positiveLabels,rownames=FALSE, escape = FALSE,filter = 'top',options = list(pageLength = 100))})  

  # open file with negated simclins
  df_negatedPosLabels <-  read.csv(paste0(app_dir,"pos_negated_labeled_data.csv"),header = TRUE, col.names = c('Note','Negation'), stringsAsFactors=FALSE,colClasses = c("character","character","character"),comment.char = "")
  output$posNegatedLabeledData_table <- DT::renderDataTable({DT::datatable(df_negatedPosLabels,rownames=FALSE, escape = FALSE,filter = 'top',options = list(pageLength = 100))})
  
  # open file with irrelevant simclins
  df_irrelevantPosLabels <-  read.csv(paste0(app_dir,"pos_irrelevant_labeled_data.csv"),header = TRUE, col.names = c('Note','Similar_term'), stringsAsFactors=FALSE,colClasses = c("character","character","character"),comment.char = "")
  output$posIrrelevantLabeledData_table <- DT::renderDataTable({DT::datatable(df_irrelevantPosLabels,colnames = c('Note','Similar term'),rownames=FALSE, escape = FALSE,filter = 'top',options = list(pageLength = 100))})  

  loadLabelingStatistics()

    
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
      #pos = gregexpr(pattern_str, next_part_of_source_txt,perl = TRUE)
      pos = stri_locate_all(next_part_of_source_txt, regex = pattern_str)
      
      if (direction==1) {
        for (j in 1:nrow(pos[[1]])){
            #pos_start = pos[1][[1]][j]
            pos_start = pos[[1]][j,'start']
            pos_end   = pos[[1]][j,'end']   
            if(!is.na(pos_start)){
            width_phrase =  pos_end - pos_start+1
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
        for (j in nrow(pos[[1]]):1){

          pos_start = pos[[1]][j,'start']
          pos_end   = pos[[1]][j,'end']    
          if(!is.na(pos_start)){
          width_phrase =  pos_end - pos_start+1

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
    if(examples_str!=''){
    if (direction==1){   #next
      
      if (last_pos>0)
        examples_str<-paste0(examples_str," <button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"",control_id,"\",this.id);' id='prev_examples_for_",pattern,"#",
                             as.character(last_pos),"'><i class='icon-left'></i>Prev</button>")  
      if (j<nrow(pos[[1]]) || step<step_count) #ToDO: next steps could not contain examples
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
    } else {
      examples_str <- "There are no examples for this term in the text."
    }
    
    return (examples_str)
  }
  
  
  ######################
  # Function getExamples_negatedNotes - returns positive notes negated by this negation
  #####################
  getExamples_negatedNotes <- function(negation){
    
    df_negatedNotes = df_negatedPosLabels[df_negatedPosLabels$Negation==negation,]
    
    examples_str <- ""
    
    if (nrow(df_negatedNotes)<200)
      examples_count <- nrow(df_negatedNotes)
    else examples_count <- 200
    
    for (i in 1:examples_count){
      examples_str <- paste0(examples_str,"<div class='example-text'>",df_negatedNotes[i,"Note"],"</div>")
    }

    return (examples_str)
  }
  
  
  
  
  ######################
  # Function getExamples_irrelevantNotes - returns notes with irrelevant simclins
  #####################
  getExamples_irrelevantNotes <- function(irrelevant_term){
    
    df_irrelevantNotes = df_irrelevantPosLabels[df_irrelevantPosLabels$Similar_term==irrelevant_term,]
    
    examples_str <- ""
    
    if (nrow(df_irrelevantNotes)<200)
      examples_count <- nrow(df_irrelevantNotes)
    else examples_count <- 200    
    
    for (i in 1:examples_count){
      examples_str <- paste0(examples_str,"<div class='example-text'>",df_irrelevantNotes[i,"Note"],"</div>")
    }
    
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
            return(FALSE)
        } else {
          #add button 'Examples'
          examples_str <- paste0("<button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"lastClickId\",this.id);' id='next_examples_for_",new_simclin_str,"#0'><i class='icon-left'></i>Examples</button>")
          #add simclin to data frame          
          newSimclin_row=data.frame(Simclins = new_simclin_str,Processed = FALSE, Examples = examples_str,stringsAsFactors=FALSE)
          df_simclins<<-rbind(df_simclins,newSimclin_row)
          #clean the input control
          updateTextInput(session,"newWord_input", value = " ")  
          refreshTable('simclins')
          return(TRUE)
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
                                                                     options = list(order=list(1,'desc'),pageLength = 100,columnDefs = list(list(targets = c(1,2,3), searchable = FALSE))),colnames = c("Similar terms","Distance","By simclins","Examples"),rownames=FALSE, escape = FALSE)})
          write.csv(df_similar_terms, file = paste0(app_dir,"similar_terms.csv"))
        } else {
          output$similar_terms_table = DT::renderDataTable({DT::datatable(df_similar_terms,
                                                                     filter = 'top',
                                                                     options = list(order=list(1,'desc'),pageLength = 100,columnDefs = list(list(targets = c(1,2,3), searchable = FALSE))),colnames = c("Similar terms","Distance","By simclins","Examples"),rownames=FALSE, escape = FALSE)})
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
      output$log_table <- DT::renderDataTable({DT::datatable(df_log,options = list(order=list(0,'desc')),colnames = c("Date & time","User Id","Operation","Parameters","Value before","Value after","Duration"),rownames=FALSE, escape = FALSE)})
      write.csv(df_log, file = paste0(app_dir,"log.csv"))
    } else if (tableName=='irrelevant_terms'){
      output$irrelevant_similar_terms_table = DT::renderDataTable({DT::datatable(df_irrelevant_terms,rownames=FALSE, escape = FALSE)})
      write.csv(df_irrelevant_terms, file = paste0(app_dir,"irrelevant_terms.csv"))
    } else if (tableName=='negations'){
      output$negations_table = DT::renderDataTable({DT::datatable(df_negations,rownames=FALSE, escape = FALSE)})
      write.csv(df_negations, file = paste0(app_dir,"negations.csv"))
    } else if (tableName=='exceptions'){
      output$exceptions_table = DT::renderDataTable({DT::datatable(df_exceptions,colnames = c("Exception","Frequency (%)","Examples"), rownames=FALSE, escape = FALSE)})
      write.csv(df_exceptions, file = paste0(app_dir,"negations-exceptions.csv"))
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
  
  observeEvent(input$addExampleButton_click,{
    for(i in 1:nrow(df_simclins))
      df_simclins[i,'Examples']<<- paste0("<button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"lastClickSimilar_termId\",this.id);' id='next_examples_for_",df_simclins[i,'Simclins'],"#0'><i class='icon-left'></i>Examples</button>")
    refreshTable('simclins')
  })  
  
  ######################
  # 1. Build the model
  #####################   

    
  observeEvent(input$buildModel_click, {
    #clean memory
    #rm(list=ls())
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
    
    model=train_word2vec(paste0(app_dir,"train2.txt"),paste0(app_dir,"train.bin"),vectors=200,threads=4,window=input$setting_window,iter=5,negative_samples=0,force=TRUE)

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
  df_similar_terms <- read.csv(paste0(app_dir,"similar_terms.csv"),header = TRUE, col.names = c('Similar_term','Distance','By_simclins','Examples'),stringsAsFactors=FALSE)
  output$similar_terms_table = DT::renderDataTable({DT::datatable(df_similar_terms,filter = 'top',options = list(pageLength = 100,order=list(1,'desc'),columnDefs = list(list(targets = c(1,2,3), searchable = FALSE))),colnames = c('Similar term','Distance','By simclins','Examples'),rownames=FALSE, escape = FALSE)})
  
  # Add new simclin by user
  observeEvent(input$addNewWord_click, {
    
    if (addNewSimclin(input$newWord_input))
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
                 } else if (input$lastClickId%like%"negation_for_")
                 {
                   negation_str=gsub("negation_for_","",input$lastClickId)
                   dataModal(getExamples_negatedNotes(negation_str))
                 }
                 
                 else if (input$lastClickId%like%"irrelevant_term_for_")
                 {
                   irrelevant_term_str=gsub("irrelevant_term_for_","",input$lastClickId)
                   dataModal(getExamples_irrelevantNotes(irrelevant_term_str))
                 }

               }
  )
  
  

  # Delete selected simclins - TODO: 1.get user confirmation 
  observeEvent(input$deleteSimclin_click, {
   
    if(!is.null(input$simclins_table_rows_selected)){
      # save selected simclins in irrelevant similar terms
      df_selected_rows = data.table (Similar_term=df_simclins[input$simclins_table_rows_selected,"Simclins"],Frequency=NA,Examples=NA)
      deleted_simclins_str = paste0(as.character(nrow(df_selected_rows))," simclins deleted: ",paste(df_selected_rows[['Similar_term']],collapse=" "))
      logAction (userId = currentUserId, operation = "Delete simclins", valueAfter = deleted_simclins_str)          
      df_irrelevant_terms <<- rbind(df_irrelevant_terms, df_selected_rows)
      rm(df_selected_rows)
      #exclude duplicates in list
      df_irrelevant_terms<<-df_irrelevant_terms[ order(df_irrelevant_terms$Similar_term, na.last = TRUE, decreasing = FALSE), ]
      df_irrelevant_terms<<- df_irrelevant_terms[ !duplicated(df_irrelevant_terms$Similar_term), ] 
      refreshTable('irrelevant_terms')

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
  
  progress$set(message = 'Reading a word2vec binary file')
  
  if (!file.exists(paste0(app_dir,"train.bin"))) 
    showModal(modalDialog(title = "Error message",  "There is no model! Please, build the model before.",easyClose = TRUE))
    
  else {

    # for each new simclin get num closest words  
    df_new_simclins=df_simclins[df_simclins[,'Processed']==FALSE,] 
    if (nrow(df_new_simclins)>0) {

     if(is.null(model))
        model <<- read.vectors(paste0(app_dir,"train.bin"))

     progress$set(message = 'Find Similar terms for:')  
     for(i in 1:nrow(df_new_simclins)) {
          start_time <- Sys.time()
          row <- df_new_simclins[i,]
          simclin_str <- as.character(row$Simclins)
          simclin_str <- gsub(" ","_",simclin_str)
  
          progress$set(value = i, detail =simclin_str)        
          # get num closest words 
          df_new_similar_terms = closest_to(model,simclin_str,input$setting_similar_terms_count,FALSE)
          
          # exclude empty rows 
          df_new_similar_terms <-  df_new_similar_terms[!(is.na(df_new_similar_terms$word) | df_new_similar_terms$word==""), ] 
          if (nrow(df_new_similar_terms)>0) {
            # prepare new dataframe for rbind command (adding to list of similar terms)   - create the same structure as df_similar_terms
              # rename column with distance
            colnames(df_new_similar_terms)[2] <-"Distance"
              # create column with simclin name
            df_new_similar_terms$By_simclins <- simclin_str
              # change col name Word to Similar_terms  
            colnames(df_new_similar_terms)[1] <-"Similar_term"
              # change type of first colm  
            factor_cols <- sapply(df_new_similar_terms, is.factor)
            df_new_similar_terms[factor_cols] <- lapply(df_new_similar_terms[factor_cols], as.character)
            df_new_similar_terms[,'Distance']=round(as.numeric(df_new_similar_terms[,'Distance']),2)
              # create column with examples
            df_new_similar_terms$Examples <- ""
            #how many new similar terms were found 
            new_similar_terms_count<-nrow(subset(df_new_similar_terms, !(df_new_similar_terms$Similar_term %in% df_similar_terms$Similar_term) & !(df_new_similar_terms$Similar_term %in% df_irrelevant_terms$Similar_term)))  
            #how many similar terms were found before
            duplicated_similar_terms_count<-nrow(subset(df_new_similar_terms, (df_new_similar_terms$Similar_term %in% df_similar_terms$Similar_term)))  
            #how many similar terms were deleted by user before
            irrelevant_similar_terms_count <- nrow(subset(df_new_similar_terms, (df_new_similar_terms$Similar_term %in% df_irrelevant_terms$Similar_term)))   
            #add all closest words to the  list of Similar terms
            df_similar_terms <<- rbind(df_similar_terms, df_new_similar_terms)
            
            refreshSystemEfficacyMetric ('suggestedSimilarTerms',new_similar_terms_count)  
            new_similar_terms_str <- paste0(as.character(new_similar_terms_count)," similar terms found ( ",as.character(irrelevant_similar_terms_count)," were deleted before by user as irrelevant, ",as.character(duplicated_similar_terms_count)," are duplicates of obtained before): ",paste(df_new_similar_terms[['Similar_term']],collapse=" "))          
          } else {
            new_similar_terms_str <- "0 similar terms found."
          }
          # mark this simclin as processed and update number of found similar_terms
          df_simclins[df_simclins[,'Simclins']==simclin_str,'Processed'] <<-TRUE
  
          rm(df_new_similar_terms)

          finish_time <-Sys.time()
          
          logAction(actionDataTime <- start_time,userId = currentUserId,operation = 'System search similar_terms for simclin',parameters = paste0("Simclin: ",simclin_str),valueAfter = new_similar_terms_str,actionDuration = round(finish_time-start_time,2))
     }
      refreshTable('simclins')
      rm(df_new_simclins)
      
      progress$close()
      search_start_time <-Sys.time()
      
      if (nrow(df_similar_terms)>0) {
  
          #exclude duplicates in updated list of similar_terms
          df_similar_terms<<-df_similar_terms[ order(df_similar_terms$Distance, na.last = TRUE, decreasing = TRUE), ]
          df_similar_terms$Examples <<-NULL
          df_similar_terms<<-aggregate(. ~ Similar_term,data = df_similar_terms,toString)
          ## restore examples after aggregation        
          for(i in 1:nrow(df_similar_terms)) {
            df_similar_terms[i,'Examples'] <<- paste0("<button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"lastClickSimilar_termId\",this.id);' id='next_examples_for_",df_similar_terms[i,'Similar_term'],"#0'><i class='icon-left'></i>Examples</button>")
          }  
          
          #exclude duplicates with simclins
          df_similar_terms<<- subset(df_similar_terms, !(df_similar_terms$Similar_term %in% df_simclins$Simclins))
          
          #delete similar_terms which are in irrelevant
          df_similar_terms<<- df_similar_terms[!(df_similar_terms$Similar_term %in% df_irrelevant_terms$Similar_term),]
          # order distances for each similar_term
          if (nrow(df_similar_terms)>0)
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
        search_duration <- Sys.time() - search_start_time
        showModal(modalDialog(title = "Similar terms search","Similar terms` search has completed!",easyClose = TRUE))
      } else {
       showModal(modalDialog(
         title = "Error message",
         "There are no new simclins for search!",
         easyClose = TRUE
       ))
      }
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
    # save unselected similar_terms into the list of irrelevant terms
    if(!is.null(input$similar_terms_table_rows_selected))
      df_unselected_similar_terms = data.table (Similar_term=df_similar_terms[-input$similar_terms_table_rows_selected,"Similar_term"],Frequency=NA,Examples=NA)
    else 
      df_unselected_similar_terms <<- data.table (Similar_term=df_similar_terms[,"Similar_term"],Frequency=NA,Examples=NA)
    
    deleted_similar_terms_str = paste0(as.character(nrow(df_unselected_similar_terms))," similar terms deleted: ",paste(df_unselected_similar_terms[['Similar_term']],collapse=" "))
    logAction (userId = currentUserId, operation = "Delete similar terms", valueAfter = deleted_similar_terms_str)    
    
    df_irrelevant_terms <<- rbind(df_irrelevant_terms, df_unselected_similar_terms)
    rm(df_unselected_similar_terms)
    #exclude duplicates in list
    df_irrelevant_terms<<-df_irrelevant_terms[ order(df_irrelevant_terms$Similar_term, na.last = TRUE, decreasing = FALSE), ]
    df_irrelevant_terms<<- df_irrelevant_terms[ !duplicated(df_irrelevant_terms$Similar_term), ] 

    write.csv(df_irrelevant_terms, file = paste0(app_dir,"irrelevant_terms.csv"))
    #output$irrelevant_similar_terms_table = DT::renderDataTable({DT::datatable(df_irrelevant_terms,colnames = c("Similar term","Frequency (%)","Examples"),rownames=FALSE, escape = FALSE)})
    output$irrelevant_similar_terms_table = DT::renderDataTable({DT::datatable(df_irrelevant_terms,rownames=FALSE, escape = FALSE)})    

    # clear unselected similar_terms
    df_similar_terms <<- df_similar_terms[input$similar_terms_table_rows_selected,]
    
    refreshTable('irrelevant_terms')
    refreshTable('similar_terms')
    
  })
  
  ######################
  #  3. Search EHRs by simclins
  ##################### 
  
  observeEvent(input$makeLabels_click, {
    #clean memory
    rm(list=ls())
    #get list of twinclins
    inFile_ehr <- input$fileEHR_input
    if(is.null(inFile_ehr)) {
     
      showModal(modalDialog(title = "Error message",  paste0("Please, specify the file name!"),easyClose = TRUE))
      
    } else {
      
      
      df_simclins <-  read.csv(paste0(app_dir,'simclins.csv'),header = TRUE, stringsAsFactors=FALSE)
      v_simclins <-data.frame(df_simclins$Simclins,stringsAsFactors=FALSE)
      
      df_results <- data.frame(Note1=character(),Label1 = logical(),stringsAsFactors=FALSE)     
      
      pattern_str <- ""
      for (i in 1:nrow(df_simclins)){
        pattern_str <- paste0(pattern_str,"|\\b(",df_simclins$Simclins[i],")\\b")
      }  
      
      if (length(pattern_str)>0)
        pattern_str <- substr(pattern_str,2,nchar(pattern_str))
      
      progress <- shiny::Progress$new(min=1, max=3)

      progress$set(message  = "Labeling by regular expressions based on simclins` list", detail = "Reading the file..." ,value = 1)       
      df_allNotes <- read.csv(inFile_ehr$datapath,header = TRUE, col.names = c("Note"),stringsAsFactors=FALSE,comment.char = "", colClasses = "character" )

      progress$set(detail = paste0("Labeling ",nrow(df_allNotes)," notes from ",Sys.time()) ,value = 2) 

      df_allNotes$Label<- grepl(pattern_str, df_allNotes$Note, ignore.case = T)
      fileName <- paste0(app_dir,"labeled-data-",Sys.Date(),".csv")
      write.csv(df_allNotes, file = fileName)
   
      progress$close()
      df_positiveLabels <- data.frame(Note = (df_allNotes[df_allNotes[,'Label']==TRUE,'Note']),stringsAsFactors=FALSE)
      total_notes <- nrow(df_allNotes)
      
      rm(df_allNotes)

      # get pattern for pre-negations search
      pattern_pre_negations <- ""
      df_pre_negations <-data.table(df_negations[df_negations[,'Type']=='before',])
      list_pre_negations <- paste(df_pre_negations$Negation, collapse = '|', sep="")
      pattern_pre_negations <- paste0("(\\b(",list_pre_negations,")\\b)")        
      
      rm(df_pre_negations)
      
      # get pattern for post-negations search
      df_post_negations <-data.table(df_negations[df_negations[,'Type']=='after',])
      pattern_post_negations <- ""
      list_post_negations <- paste(df_post_negations$Negation, collapse = '|', sep="")
      pattern_post_negations <- paste0("(\\b(",list_post_negations,")\\b)")        
      
      rm(df_post_negations)
      
      progress <- shiny::Progress$new(min = 1, max = nrow(df_positiveLabels))
      on.exit(progress$close())
      progress$set(message  = "Examine true labeled for negations") 
      
      df_false_positiveLabels <- vector(mode = "logical", length = 0)
      df_irrelevant_positiveLabels <- vector(mode = "logical", length = 0)
      fl_negation_type <-"" # pre/post
      
      if(nrow(df_positiveLabels)>0)
      for(i in 1:nrow(df_positiveLabels)){
        
        curr_note = df_positiveLabels[i,"Note"]
        noteOfLabel = FALSE
        #check all simclins for negation
        progress$set(value = i,detail=paste0(i,"/",nrow(df_positiveLabels)))  
        #get simclins 
        list_simclins = stri_locate_all(curr_note, regex = pattern_str)
        prev_pos_start = 1
        prev_pos_end = 0

        #search simclins in note
        for (j in 1:nrow(list_simclins[[1]])){
            if(noteOfLabel==FALSE){
              
            isSimclinIrrelevant = FALSE 
            isSimclinNegated = FALSE
            

            pos_start = list_simclins[[1]][j,'start']
            pos_end   = list_simclins[[1]][j,'end']            
            curr_simclin = substring(curr_note,pos_start,pos_end)
            
            progress$set(value = i,detail=paste0(i,"/",nrow(df_positiveLabels)," simclin:",curr_simclin))

            #simclin` relevance check
              
            #there are irrelevant expressions for current simclin
            if(length(grep(curr_simclin,df_irrelevant_terms$Similar_term))!=0){
                df_irrelevant_simclins <-df_irrelevant_terms[grepl(curr_simclin,df_irrelevant_terms$Similar_term),'Similar_term']
                pattern_irrelevant_terms = paste(df_irrelevant_simclins, collapse = '|', sep="")
                pattern_irrelevant_terms = paste(pattern_irrelevant_terms,"|",gsub("_"," ",pattern_irrelevant_terms), sep="")
                irr_expressions_in_note = stri_locate_all(curr_note, regex =  paste("(",pattern_irrelevant_terms,")", sep=""))
                
                #check everyirrelevant expression for current simclin
                for (i_irr_expr in 1:nrow(irr_expressions_in_note[[1]])){
                    start_pos_expression_in_note = irr_expressions_in_note[[1]][i_irr_expr,'start']
                    end_pos_expression_in_note = irr_expressions_in_note[[1]][i_irr_expr,'end']
                    if(!is.na(start_pos_expression_in_note)){
                      curr_irrelevant_expression = substring(curr_note,start_pos_expression_in_note,end_pos_expression_in_note)
                      pos_simclin_in_expression = grep(curr_simclin,curr_irrelevant_expression)
                      #is it current simclin? pos_start - pos of current simclin in note
                      if(length(pos_simclin_in_expression)>0)
                        for(i_simclin in 1:length(pos_simclin_in_expression)){
                            if(pos_start>=start_pos_expression_in_note[i_simclin]+pos_simclin_in_expression-1)  
                              isSimclinIrrelevant = TRUE
                        }
                    }
                }
              }
              
              # Negations check (if the simclin is relevant)
              
              if(!isSimclinIrrelevant){
            
                substring_before_simclin =  substring(curr_note,prev_pos_end+1,pos_end)     
                substring_after_simclin =   stri_extract_first(substring(curr_note,pos_start),regex = paste0("(?:\\W+\\w+){0",const_distance_between_simclin_and_negation,"}"))      
                #get pattern for pre-negations
                pattern_pre_negations_with_distance <- paste0(pattern_pre_negations,stri_dup("(\\w*)\\W*",const_distance_between_simclin_and_negation),"(\\b(",curr_simclin,")\\b)")
                # find all negations near the current simclin 
                negations_of_curr_simclin = stri_locate_all(substring_before_simclin, regex = pattern_pre_negations_with_distance)
                # if there are negations - check it for exceptions (pseudo and terminations later) 
                for (i_negation in 1:nrow(negations_of_curr_simclin[[1]])){
                  isNegationPseudo = FALSE
                  if(!isSimclinNegated){
                      pos_start_curr_negation = negations_of_curr_simclin[[1]][i_negation,'start']
                      pos_end_curr_negation = negations_of_curr_simclin[[1]][i_negation,'end']
                      
                      if(!is.na(pos_start_curr_negation)){
                      
                        curr_simclin_with_negation = substring(curr_note,pos_start_curr_negation,pos_end_curr_negation)
                        curr_negation = stri_extract_first(curr_simclin_with_negation, regex = pattern_pre_negations)
                        # for every negation - get list of its exceptions
                        exceptions_pattern_str <-df_exceptions[grepl(curr_negation,df_exceptions$Exception),'Exception']
                        # find these exceptions near the current simclin
                        exceptions_near_curr_simclin = stri_locate_all(substring_before_simclin, regex = paste("(",paste(exceptions_pattern_str, collapse = '|', sep=""),")", sep=""))
                        
                        # for every exception near simclin  
                        
                        for (i_exception in 1:nrow(exceptions_near_curr_simclin[[1]])){
                            pos_start_exception = exceptions_near_curr_simclin[[1]][i_exception,'start']
                            pos_end_exception = exceptions_near_curr_simclin[[1]][i_exception,'end']
                            if(!is.na(pos_start_exception)){
                                curr_exception = substring(substring_before_simclin,pos_start_exception,pos_end_exception)
                                pos_negation_in_exception = grep(curr_negation,curr_exception)
                                #is it current negation? 
                                if(length(pos_negation_in_exception)>0)
                                  for(i_negation_in_exception in 1:length(pos_negation_in_exception)){
                                    if(pos_start_curr_negation>=pos_negation_in_exception[i_negation_in_exception]+pos_start_exception-1)  
                                      isNegationPseudo = TRUE
                                  }
                            }
                        }
                        if(!isNegationPseudo) 
                          isSimclinNegated = TRUE
                      }
                    } #f(!isSimclinNegated){ 
                }#for (i_negation 
              
                #if negation was not found - check for post-negations
                if(!isSimclinNegated)
                {
                  #get pattern for post-negations
                  pattern_post_negations_with_distance<-paste0("(\\b(",curr_simclin,")\\b)")
                  pattern_post_negations_with_distance <- paste0(pattern_post_negations_with_distance,stri_dup("\\W*(\\w*)",const_distance_between_simclin_and_negation),"\\W*",pattern_post_negations)
                  
                  # find all negations near the current simclin 
                  negations_of_curr_simclin = stri_locate_all(substring_after_simclin, regex = pattern_post_negations_with_distance)
  
                  for (i_negation in 1:nrow(negations_of_curr_simclin[[1]])){
                    isNegationPseudo = FALSE
                    if(!isSimclinNegated){
                        pos_start_curr_negation = negations_of_curr_simclin[[1]][i_negation,'start']
                        pos_end_curr_negation = negations_of_curr_simclin[[1]][i_negation,'end']
                        
                        if(!is.na(pos_start_curr_negation)) {
                          curr_simclin_with_negation = substring(curr_note,pos_start_curr_negation,pos_end_curr_negation)
                              curr_negation = stri_extract_first(curr_simclin_with_negation, regex = pattern_post_negations)
                              # for every negation - get list of its exceptions
                              exceptions_pattern_str <-df_exceptions[grepl(df_exceptions$Exception,curr_negation),'Exception']
                              # find these exceptions near the current simclin
                              exceptions_near_curr_simclin = stri_locate_all(substring_after_simclin, regex = paste("(",paste(exceptions_pattern_str, collapse = '|', sep=""),")", sep=""))
                              
                              # for every exception near simclin  
                              
                              for (i_exception in 1:nrow(exceptions_near_curr_simclin[[1]])){
                                pos_start_exception = exceptions_near_curr_simclin[[1]][i_exception,'start']
                                pos_end_exception = exceptions_near_curr_simclin[[1]][i_exception,'end']
                                curr_exception = substring(substring_after_simclin,pos_start_exception,pos_end_exception)
                                pos_negation_in_exception = grep(curr_negation,curr_exception)
                                #is it current negation? 
                                for(i_negation_in_exception in 1:length(pos_negation_in_exception)){
                                  if(pos_start_curr_negation>=pos_negation_in_exception[i_negation_in_exception]+pos_start_exception-1)  
                                    isNegationPseudo = TRUE
                                  }
                                } #for (i_exception 
                              if(!isNegationPseudo) 
                                isSimclinNegated = TRUE
                          }#if(!is.na(pos_start_curr_negation)) { 
                     }#if(!isSimclinNegated){
                    }#for (i_negation 
                  }#if(!isSimclinNegated) then check post negations
              } #if(!isSimclinIrrelevant) then check negations
            prev_pos_start  <- pos_start
            prev_pos_end    <- pos_end
            }#if(noteOfLabel==FALSE){
          }  # loop over simclins

          #if simclin is not negated - the patient's note is labeled as TRUE  
          if (!isSimclinNegated & !isSimclinIrrelevant) {
            noteOfLabel = TRUE
            df_positiveLabels$Note[i]<- paste0(substring(df_positiveLabels$Note[i],1,pos_start-1),"<span class='true-simclin'>",curr_simclin,"</span>",substring(df_positiveLabels$Note[i],pos_end+1))
          }  
          else { 
            if(isSimclinNegated){
              df_positiveLabels$Note[i]<-gsub(curr_simclin_with_negation,paste0("<span class='false-simclin'>",curr_simclin_with_negation,"</span>"),df_positiveLabels$Note[i]) 
            } else if(isSimclinIrrelevant){
              df_positiveLabels$Note[i]<-gsub(curr_irrelevant_expression,paste0("<span class='false-simclin'>",curr_irrelevant_expression,"</span>"),df_positiveLabels$Note[i]) 
            }
            
          }  
          
          if (noteOfLabel==FALSE){
            #it's the negated positive note
            if(isSimclinNegated){
              df_false_positiveLabels = append(df_false_positiveLabels,trimws(curr_negation))
              df_irrelevant_positiveLabels = append(df_irrelevant_positiveLabels,NA)
            } else if(isSimclinIrrelevant){
              df_false_positiveLabels = append(df_false_positiveLabels,NA)
              df_irrelevant_positiveLabels = append(df_irrelevant_positiveLabels,trimws(curr_irrelevant_expression))
            }
          } else {
            df_irrelevant_positiveLabels = append(df_irrelevant_positiveLabels,NA)
            df_false_positiveLabels = append(df_false_positiveLabels,NA)
          }  
      
      } # loop over notes
      
      df_negatedPosLabels <<- data.table(Note = df_positiveLabels[!is.na(df_false_positiveLabels),"Note"],Negation = df_false_positiveLabels[!is.na(df_false_positiveLabels)] )
      write.csv(df_negatedPosLabels, file = paste0(app_dir,"pos_negated_labeled_data.csv"))          
        
      df_irrelevantPosLabels <<- data.table(Note = df_positiveLabels[!is.na(df_irrelevant_positiveLabels),"Note"],Similar_term = df_irrelevant_positiveLabels[!is.na(df_irrelevant_positiveLabels)] )
      write.csv(df_irrelevantPosLabels, file = paste0(app_dir,"pos_irrelevant_labeled_data.csv"))          
        
      df_positiveLabels <- data.table(Note = df_positiveLabels[is.na(df_false_positiveLabels)&is.na(df_irrelevant_positiveLabels),])
      write.csv(df_positiveLabels, file = paste0(app_dir,"pos_labeled_data.csv"))

      logAction(userId = currentUserId, operation = "Labeling",parameters = "totalNotes", valueAfter=total_notes)      
      
      total_positive_notes <-nrow(df_positiveLabels)      
      logAction(userId = currentUserId, operation = "Labeling",parameters = "totalPositiveNotes", valueAfter=total_positive_notes)            
      
      total_irrelevant_notes <- nrow(df_irrelevantPosLabels)
      logAction(userId = currentUserId, operation = "Labeling",parameters = "totalIrrelevantPositiveNotes", valueAfter=total_irrelevant_notes)            

      total_negated_notes <- nrow(df_negatedPosLabels)
      logAction(userId = currentUserId, operation = "Labeling",parameters = "totalNegatedPositiveNotes", valueAfter=total_negated_notes)            

      total_negative_notes <- total_notes - total_positive_notes - total_negated_notes
      logAction(userId = currentUserId, operation = "Labeling",parameters = "totalNegativeNotes", valueAfter=total_negative_notes)            
      
      rm(df_false_positiveLabels)
      rm(df_irrelevant_positiveLabels)
      
      # statistics calculation            
      for (i in 1:nrow(df_negations)){
        curr_negatin_count <-nrow(df_negatedPosLabels[df_negatedPosLabels$Negation==df_negations[i,'Negation'],])
        curr_freq <- round(curr_negatin_count/total_negated_notes*100,2)
        df_negations[i,'Frequency'] <<- curr_freq
        if(curr_negatin_count>0)
          df_negations[i,'Examples']  <<- paste0("<button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"lastClickId\",this.id);' id='negation_for_",df_negations[i,'Negation'],"'><i class='icon-left'></i>Examples</button>")
        else  df_negations[i,'Examples']  <<- NA       
      }
      refreshTable('negations')
        
      for (i in 1:nrow(df_irrelevant_terms)){
        curr_irrelevant_term_count <-nrow(df_irrelevantPosLabels[df_irrelevantPosLabels$Similar_term==df_irrelevant_terms[i,'Similar_term'],])
        curr_freq <- round(curr_irrelevant_term_count/total_irrelevant_notes*100,2)
        df_irrelevant_terms[i,'Frequency'] <<- curr_freq
        if(curr_irrelevant_term_count>0)
          df_irrelevant_terms[i,'Examples']  <<- paste0("<button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"lastClickId\",this.id);' id='irrelevant_term_for_",df_irrelevant_terms[i,'Similar_term'],"'><i class='icon-left'></i>Examples</button>")
        else df_irrelevant_terms[i,'Examples']  <<- NA
      }
      refreshTable('irrelevant_terms')      
      
      output$posLabeledData_table = DT::renderDataTable({DT::datatable(df_positiveLabels,rownames=FALSE, escape = FALSE)})
      output$posNegatedLabeledData_table = DT::renderDataTable({DT::datatable(df_negatedPosLabels,rownames=FALSE, escape = FALSE)})            
      output$posIrrelativeLabeledData_table = DT::renderDataTable({DT::datatable(df_negatedPosLabels,colnames = c("Note","Similar term"),rownames=FALSE, escape = FALSE)})            
      
      loadLabelingStatistics()
    } #inFile_ehr is specified
  })
  
  ######################
  #  Log
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
  
  addNewIrrSimilarTerm <- function (irr_similar_term_str) {
    
    irr_similar_term_str = trimws(irr_similar_term_str)
    #irr_similar_term_str = gsubub(" ","_",irr_similar_term_str)
    #check for duplicates
    if(nrow(df_irrelevant_terms)>0 && any(df_irrelevant_terms$Similar_term==irr_similar_term_str)) {
      showModal(modalDialog(title = "Error message",  paste0("The term \"",irr_similar_term_str,"\" is in the list already!"),easyClose = TRUE))
    } else {
      #add to data frame          
      newterm_row=data.frame(Similar_term = irr_similar_term_str,Frequency = NA, Examples = NA,stringsAsFactors=FALSE)
      df_irrelevant_terms<<-rbind(df_irrelevant_terms,newterm_row)
      refreshTable('irrelevant_terms')
      # save user action in log file
      logAction(userId = currentUserId, operation = "Add user irrelevant similar term",valueAfter=paste0(irr_similar_term_str))
    }
    
  }
  
  # Save selected similar terms as simclins 
  observeEvent(input$moveToSimclins_click, {
    
    if(!is.null(input$irrelevant_similar_terms_table_rows_selected)){
      
      df_selected_rows <-  data.table (df_irrelevant_terms[input$irrelevant_similar_terms_table_rows_selected,])
      for(i in 1:nrow(df_selected_rows)){
        new_simclin = as.character(df_selected_rows[i,1])
        addNewSimclin(new_simclin, FALSE)
        
        # save user action in log file
        logAction(userId = currentUserId, operation = "Select simclin from irrelevant similar terms",valueAfter=new_simclin)
      }
      
      df_irrelevant_terms <<- data.table(df_irrelevant_terms[-input$irrelevant_similar_terms_table_rows_selected,])
      refreshTable('irrelevant_terms')
      rm(df_selected_rows)

    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected similar terms!",easyClose = TRUE))
    }
  })
  
  observeEvent(input$addNewIrrSimilarTerm_click, {
    if (!is.na(input$newIrrSimilarTerm_input) && input$newIrrSimilarTerm_input!=""){
      addNewIrrSimilarTerm(input$newIrrSimilarTerm_input);
      #clean the input control
      updateTextInput(session,"newIrrSimilarTerm_input", value = NA)
    }  
  })  
  
  # Delete selected irrelevant similar terms - TODO: 1.get user confirmation 
  observeEvent(input$deleteIrrSimilarTerm_click, {
    
    if(!is.null(input$irrelevant_similar_terms_table_rows_selected)){
      # save selected negations       
      df_selected_rows = data.table (Similar_term=df_irrelevant_terms[input$irrelevant_similar_terms_table_rows_selected,"Similar_term"])
      deleted_terms_str = paste0(as.character(nrow(df_selected_rows))," irrelevant similar terms deleted: ",paste(df_selected_rows[['Similar_term']],collapse=", "))
      logAction (userId = currentUserId, operation = "Delete irrelevant similar terms", valueAfter = deleted_terms_str)          
      
      df_irrelevant_terms <<- data.table(df_irrelevant_terms[-input$irrelevant_similar_terms_table_rows_selected,])
      refreshTable('irrelevant_terms')
      rm(df_selected_rows)
    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected terms to delete!",easyClose = TRUE))
    }
    
  })
  ######################
  # Negations
  #####################  
  # Add new negation by user

  addNewNegation <- function (negation_str,negation_type) {
    
    # trim the new negation
    negation_str = trimws(negation_str)
    #check for duplicates
    if(nrow(df_negations)>0 && any(df_negations$Negation==negation_str)) {
      showModal(modalDialog(title = "Error message",  paste0("The negation \"",negation_str,"\" is in the list already!"),easyClose = TRUE))
    } else {
      #add negation to data frame          
      newnegation_row=data.frame(Negation = negation_str,Type = negation_type,Frequency = NA, Examples = NA, stringsAsFactors=FALSE)
      df_negations<<-rbind(df_negations,newnegation_row)
      refreshTable('negations')
      # save user action in log file
      logAction(userId = currentUserId, operation = "Add user negation",valueAfter=paste0(negation_str," (",negation_type,")"))
    }
    
  }
  
  observeEvent(input$addNewNegation_click, {
    if (!is.na(input$newPrepNegation_input) && input$newPrepNegation_input!=""){
      addNewNegation(input$newPrepNegation_input,'before');
      #clean the input control
      updateTextInput(session,"newPrepNegation_input", value = NA)  
    }  
    if (!is.na(input$newPostNegation_input) && input$newPostNegation_input!="") {
      addNewNegation(input$newPostNegation_input,'after');
      #clean the input control
      updateTextInput(session,"newPostNegation_input", value = NA)  
    }  
  })  
  
  # Delete selected negations - TODO: 1.get user confirmation 
  observeEvent(input$deleteNegation_click, {
    
    if(!is.null(input$negations_table_rows_selected)){
      # save selected negations       
      df_selected_rows = data.table (Negation=df_negations[input$negations_table_rows_selected,"Negation"])
      deleted_negations_str = paste0(as.character(nrow(df_selected_rows))," negations deleted: ",paste(df_selected_rows[['Negation']],collapse=" "))
      logAction (userId = currentUserId, operation = "Delete negations", valueAfter = deleted_negations_str)          

      df_negations <<- data.table(df_negations[-input$negations_table_rows_selected,])
      refreshTable('negations')
      rm(df_selected_rows)
    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected negations to delete!",easyClose = TRUE))
    }
    
  })
  
  ######################
  # Exceptions
  #####################  
  # Add new exception by user
  
  addNewException <- function (exception_str) {
    
    # trim the new exception
    exception_str = trimws(exception_str)
    #check for duplicates
    if(nrow(df_exceptions)>0 && any(df_exceptions['Exception']==exception_str)) {
      showModal(modalDialog(title = "Error message",  paste0("The exception \"",exception_str,"\" is in the list already!"),easyClose = TRUE))
    } else {
      #add exception to data frame          
      newexception_row=data.frame(Exception = exception_str,Frequency = NA, Examples = NA, stringsAsFactors=FALSE)
      df_exceptions<<-rbind(df_exceptions,newexception_row)
      refreshTable('exceptions')
      # save user action in log file
      logAction(userId = currentUserId, operation = "Add user exception",valueAfter=exception_str)
    }
    
  }
  
  observeEvent(input$addNewException_click, {
    if (!is.na(input$newException_input) && input$newException_input!=""){
      addNewException(input$newException_input);
      #clean the input control
      updateTextInput(session,"newException_input", value = NA)
    }  
  })  
  
  # Delete selected exceptions - TODO: 1.get user confirmation 
  observeEvent(input$deleteException_click, {
    
    if(!is.null(input$exceptions_table_rows_selected)){
      # save selected exceptions       
      df_selected_rows = data.table (Exception=df_exceptions[input$exceptions_table_rows_selected,"Exception"])
      deleted_exceptions_str = paste0(as.character(nrow(df_selected_rows))," exceptions deleted: ",paste(df_selected_rows[['Exception']],collapse=" "))
      logAction (userId = currentUserId, operation = "Delete exceptions", valueAfter = deleted_exceptions_str)          
      
      df_exceptions <<- data.table(df_exceptions[-input$exceptions_table_rows_selected,])
      refreshTable('exceptions')
      rm(df_selected_rows)
    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected exceptions to delete!",easyClose = TRUE))
    }
    
  })
  
}

shinyApp(ui = ui, server = server)