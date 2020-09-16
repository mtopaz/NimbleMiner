# NimbleMiner: a software that allows clinicians to interact with word embedding models (skip-gram models - word2vec by package wordVectors and GloVe) to rapidly create lexicons of similar terms.
# version: 0.52 (Models building, Search of similar terms, Negations (with exceptions), Irrelevant terms, Machine learning by SVM and LSTM)
#####################################

library(shiny)
library(stringi)
library(data.table)
library(DT)
library(shinythemes)
library(ggplot2)
library(readr)
library(wordVectors)
library(shinyTree)
library(shinyjs)
library(RTextTools)
library(tm)
library(xtable)
library(keras)
library(rword2vec)
library(text2vec)
library(tokenizers)
library(NimbleMiner)

# User interface
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  includeCSS("styles.css"),
  useShinyjs(),

  mainPanel(
    width = 12,
    div(id = "header-div","NimbleMiner"),
    navbarPage(id="headerNavBar", "NimbleMiner",
               tabPanel("Change the category", icon = icon("folder-open"),
                        wellPanel(
                          textInput(inputId = 'newCategory_input', label = 'Enter the new category name'),
                          actionButton("addSiblinsCategory_click", "Add category", icon = icon("plus")),
                          actionButton("addSubCategory_click", "Add subcategory", icon = icon("plus")),
                          hr(),
                          shinyTree("simclins_tree_settings",  theme="proton"),
                          tags$hr(),
                          actionButton("renameCategory_click", "Rename selected category", icon = icon("edit")),
                          actionButton("deleteCategory_click", "Delete selected category", icon = icon("trash")),
                          actionButton("deleteCategoryWithSimclins_click", "Delete selected category with simclins", icon = icon("trash"))
                        )
               ),
               tabPanel("1. Model builder",

                        tags$h3(paste0('1. Please, upload train.txt file with one column data to the NimbleMiner folder.')),
                        img(src = "folder_with_txt_file.JPG"),
                        tags$hr(),
                        tags$h3(paste0
                                ('2. Please, check the setting.')),
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

                        fluidRow(
                          column(8,
                                selectInput('buildModel_startStep_input', 'Choose the start step:', c("1. Text preprocessing (train.txt file is expected in app folder)" = "TP", "2. Vocabulary bulding  (train1.txt file is expected in app folder)" = "VB"), width = "100%")
                            )
                        ),
                        tags$hr(),
                        actionButton("buildModel_word2vec_click", "Build word2vec model", icon = icon("play")),
                        actionButton("buildModel_GloVe_click", "Build GloVe model", icon = icon("play")),
                        tags$hr(),
                        #actionButton("ExportVocabulary_click", "Export vocabulary", icon = icon("save")),
                        icon = icon("cogs")
               ),

               tabPanel("2. Simclin explorer",


                        wellPanel(
                          navbarPage("Simclins",

                                     tabPanel("Enter new simclin",
                                              textInput(inputId = 'newWord_input', label = 'Enter new simclin'),
                                              actionButton("addNewWord_click", "Add", icon = icon("plus")),
                                              icon = icon("keyboard-o")
                                     ),
                                     tabPanel("Load new simclins from file",
                                              fileInput("loadNewSimclinsFromCSV", label = "Please, upload .csv file with one column of simclins for every category. Specify category title in the column",accept = c("text/csv",".csv")),
                                              actionButton("loadNewSimclinsFromCSV_click", "Upload", icon = icon("upload")),
                                              icon = icon("file-text-o")
                                     )
                          ),
                          hr(),
                          wellPanel(
                            DT::dataTableOutput(outputId = 'simclins_table')
                          ),


                          fluidRow(
                            column(8,
                                   selectInput("select_model_method", label = h4("Select method of search"),
                                               choices = list("Keep model in the memory (faster simclin search but memory demanding)" = 1, "Keep  model on disk (slower simclin search but no expected memory issues)  " = 2),
                                               selected = 1))
                          ),
                          fluidRow(
                            column(4,actionButton("findSimilar_terms_click", "Find similar terms for new simclins", icon = icon("search-plus"))),
                            column(4,actionButton("deleteSimclin_click", "Delete selected simclins", icon = icon("trash")))
                          ),
                          verbatimTextOutput("selectedCategory")
                        ),

                        wellPanel(
                          div(id="div_similar_terms",
                              h1('New similar terms'),
                              hr(),
                              actionButton("selectAllSimilar_terms_click", "Select all", icon = icon("list")),
                              actionButton("deselectAllSimilar_terms_click", "Deselect all", icon = icon("bars")),
                              actionButton("selectLexicalVariants_click", "Select lexical variants of simclins", icon = icon("list")),
                              hr(),
                              DT::dataTableOutput(outputId = 'similar_terms_table')
                          ),
                          h5("Preview of selected terms:"),
                          verbatimTextOutput('selected_terms_label'),
                          verbatimTextOutput('selected_terms'),
                          actionButton("saveAsSimclins_click", "Save selected similar terms as simclins", icon = icon("arrow-up")),
                          actionButton("clearSimilar_terms_click", "Clear all unselected similar terms", icon = icon("trash")),
                          hr(),
                          actionButton("nextSearch_click", "Save & search again", icon = icon("search-plus")),
                          textInput( inputId = 'lastClickId', label = 'lastClickId' ),
                          textInput( inputId = 'lastClickSimilarTermId', label = 'lastClickSimilarTermId' )

                        ),
                        icon = icon("search-plus", class = NULL, lib = "font-awesome")

               ),

               tabPanel("3. Irrelevant similar terms explorer",


                        wellPanel(
                          h1('New irrelevant similar terms'),
                          textInput(inputId = 'newIrrSimilarTerm_input', label = 'Enter new irrelevant term'),
                          actionButton("addNewIrrSimilarTerm_click", "Add irrelevant term", icon = icon("plus")),
                          hr(),
                          wellPanel(DT::dataTableOutput(outputId = 'irrelevant_similar_terms_table')),
                          hr(),
                          actionButton("moveToSimclins_click", "Move selected irrelevant terms to simclins\' list", icon = icon("undo")),
                          actionButton("deleteIrrSimilarTerm_click", "Delete selected irrelevant terms", icon = icon("eraser")),
                          tags$div(class="clear-both")
                        )
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
                                              tabsetPanel( id = "negationCategory",
                                                           tabPanel('General',
                                                                    hr(),
                                                                    DT::dataTableOutput(outputId = 'negations_table'),
                                                                    actionButton("deleteNegation_click", "Delete selected negations", icon = icon("eraser"))
                                                           ),

                                                           tabPanel('For current category',
                                                                    hr(),
                                                                    DT::dataTableOutput(outputId = 'curr_category_negations_table'),
                                                                    actionButton("deleteCurrCategoryNegation_click", "Delete selected negations", icon = icon("eraser"))
                                                           )

                                              ),
                                              hr(),
                                              sliderInput(inputId = "distance_between_simclin_and_negation",
                                                          label = "Distance to negation:",
                                                          value = 2, min = 0, max = 10)
                                     ),
                                     tabPanel("Exceptions",
                                              h1('New exception'),
                                              textInput(inputId = 'newException_input', label = 'Enter new exception:'),
                                              actionButton("addNewException_click", "Add exception", icon = icon("plus")),
                                              hr(),
                                              tabsetPanel( id = "exceptionCategory",
                                                           tabPanel('General',
                                                                    hr(),
                                                                    DT::dataTableOutput(outputId = 'exceptions_table'),
                                                                    actionButton("deleteException_click", "Delete selected exceptions", icon = icon("eraser"))
                                                           ),
                                                           tabPanel('For current category',
                                                                    hr(),
                                                                    DT::dataTableOutput(outputId = 'curr_category_exceptions_table'),
                                                                    actionButton("deleteCurrCategoryException_click", "Delete selected exceptions", icon = icon("eraser"))
                                                           )
                                              )

                                     )
                          )
                        )
                        ,icon = icon("times-circle")
               ),

               tabPanel("5. Assign and review labels",
                        wellPanel(
                          h1('Data labeling'),
                          fileInput(inputId = 'fileEHR_input','Please, upload .csv file with column "Note":',accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), multiple = FALSE),
                          fluidRow(
                            column(4,selectInput("language", label = h4("Select language of notes"), choices = list("English" = 1, "Hebrew" = 2), selected = 1, width = "200px")),
                            column(4,selectInput("unit_type_to_label", label = h4("Select unit type"), choices = list("Document" = 1, "Paragraph" = 2, "Sentence" = 3), selected = 1, width = "200px"))
                          ),
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
                                                actionButton("exportToHTML_click","Export to HTML",icon = icon("code")),
                                                hr(),
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

               tabPanel("6. Machine learning",

                        wellPanel(
                          h1("1. Train corpus from labeled data"),
                          helpText("Please, specify the number of notes from every class (positive, negative and negated) in the corpus:"),
                          fluidRow(
                            column(4,sliderInput(inputId = "notes_of_positive_class", label = "Positive class:", value = 50, min = 10, max = 100)),
                            column(4,sliderInput(inputId = "notes_of_negative_class",label = 'Negative class:', value = 30, min = 10, max = 100)),
                            column(4,sliderInput(inputId = "notes_of_negated_class",label = 'Negated class:', value = 20, min = 10, max = 100))
                          ),
                          actionButton("generateCorpus_click", "Generate train corpus", icon = icon("play"))
                        ),

                        # Sidebar with a slider input
                        wellPanel(
                          h1("2. Learn model"),
                          selectInput('mlModel_input', 'Choose the algorithm:', c("Support Vector Machine (SVM)" = "SVM", "Neural network (LSTM)" = "NNET")),
                          hr(),
                          actionButton("learnModel_click", "Learn model", icon = icon("play")),
                          hr(),
                          h3("Model testing summary:"),
                          DT::dataTableOutput(outputId = 'algorythms_summary_table')
                        ),

                        # Show a plot of the generated distribution
                        wellPanel(
                          h1("3. Predict labels"),
                          selectInput('mlModelToPredict_input', 'Choose the algorithm:', c("Support Vector Machine (SVM)" = "SVM", "Neural network (LSTM)" = "NNET")),
                          fileInput(inputId = 'notesToPredictFile','Please, upload .csv file with column "note":',accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), multiple = FALSE),
                          actionButton("predictNotes_click", "Predict", icon = icon("search-plus")),
                          h3("Predicted results:"),
                          DT::dataTableOutput(outputId = 'predicted_results_table')
                        ),
                        icon = icon("bolt", class = NULL, lib = "font-awesome")
               ),
               tabPanel("Settings",

                        navbarPage("Settings",
                                   tabPanel("User data",

                                            tags$hr(),
                                            actionButton("clearData_click", "Clear all previous simclins' data", icon = icon("eraser")),
                                            tags$hr(),

                                            icon = icon("search-plus")

                                   )
                        ),icon = icon("cogs")

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

)


# Server part

server <- function(input, output, session) {

  ##########################################################################################
  # Function getSavedSelectedCategory - return selected category from saved categories tree
  ##########################################################################################
  getSavedSelectedCategory <- function(){
    #read the current tree as dataframe
    filename <- paste0(app_dir,"simclins_tree.csv")
    simclins_tree_json <-  readLines(filename,encoding="UTF-8")
    simclins_tree_df<-jsonlite::fromJSON(simclins_tree_json)

    if(nrow(simclins_tree_df)>0)
      for(i in 1:nrow(simclins_tree_df))
        if (simclins_tree_df[i,'state']$selected==TRUE)
          return(simclins_tree_df[i,'text'])

    return ("")
  }
  ############################################################################################################
  #  Function getNewSelectedCategory - returns the just selected category from the tree structure
  #                                    (is called after user clicked any node of the tree)
  ############################################################################################################
  getNewSelectedCategory <- function(treeId =  input$simclins_tree_settings){
    if (is.null(treeId)){
      selectedCategory_str <- ""
    } else{
      selectedCategory_ls <- get_selected(treeId)
      if(length(selectedCategory_ls)>0){
        selectedCategory_str <- unlist(selectedCategory_ls)
      } else{
        selectedCategory_str <- ""
      }
    }
    return(selectedCategory_str)
  }

  #############################################################################################################
  # Function getSelectedCategory - returns the current selected category from the global variable
  #############################################################################################################
  getSelectedCategory <- function(treeId =  input$simclins_tree_settings){
    return (userSettings$selectedCategory)
  }

  #############################################################################################################
  # Function closestByLevenstein - returns vector of of elements (indeces) of argument vector_of_words,
  #                                which are closed by Levenstein maximum distance <= argument max to argument word
  #############################################################################################################
  closestByLevenstein <- function(word,vector_of_words,max,min_word_length = 4){
    if (nchar(word)<min_word_length)
      return (character(0))
    else {
      distance_matrix <- adist(word,vector_of_words,ignore.case = TRUE)
      return (distance_matrix<=max)
    }
  }

  #############################################################################################################
  # Function getDuplicatedTerms - check terms from vector df_new_terms for there duplicates in  df_simclins (if check_simclins = TRUE),
  #                               df_similar_terms (if check_similar_terms = TRUE) and df_irrelevant_terms (if check_irrelevant_terms = TRUE).
  #                               Returns vector of indexes of duplicated elements from df_new_terms
  #############################################################################################################
  getDuplicatedTerms<-function(df_new_terms,check_simclins = TRUE,check_similar_terms = TRUE, check_irrelevant_terms = TRUE){

    result_list = vector()
    if (nrow(df_new_terms)>0)
      for(i in 1:nrow(df_new_terms)){

        result_list[i] <- FALSE
        curr_similar_term <- df_new_terms[i,'Similar_term']
        curr_category <- df_new_terms[i,'Category']


        if (check_simclins & nrow(df_simclins)>0){
          if (nrow(df_simclins[df_simclins$Simclins==curr_similar_term & df_simclins$Category==curr_category,] )>0) {
            result_list[i] <-TRUE
            next
          }
        }

        if (!result_list[i] & check_similar_terms & nrow(df_similar_terms)>0){
          if (nrow(df_similar_terms[df_similar_terms$Similar_term==curr_similar_term & df_similar_terms$Category==curr_category,])>0) {
            result_list[i] <-TRUE
            next
          }
        }


        if (!result_list[i] & check_irrelevant_terms & nrow(df_irrelevant_terms)>0){
          if (nrow(df_irrelevant_terms[df_irrelevant_terms$Similar_term==curr_similar_term & df_irrelevant_terms$Category==curr_category,])>0) {
            result_list[i] <-TRUE
            next
          }
        }

      }
    result_list
  }

  #############################################################################################################
  # Function tree2df - read json from csv-file and convert to the list structure (via data frame)
  #############################################################################################################
  tree2df <-function(){
    filename <- paste0(app_dir,"simclins_tree.csv")
    simclins_tree_json <- readLines(filename,encoding="UTF-8")
    simclins_tree_df<-jsonlite::fromJSON(simclins_tree_json)
    result_list<-treedf2list(simclins_tree_df)
    result_list
  }

  #############################################################################################################
  # Function saveCategoryTree_settings - save list structure of tree to the csv - file
  #############################################################################################################
  saveCategoryTree_settings <- function(tree_list){
    filename <- paste0(app_dir,"simclins_tree.csv")
    treeAsJson<-as.character(jsonlite::toJSON(get_flatList(tree_list), auto_unbox = T))
    treeAsJson<-enc2utf8(treeAsJson)
    writeLines(treeAsJson,filename,useBytes = TRUE)
  }

  #############################################################################################################
  # Function loadSystemMetrics - read system metrics of simclins search from the log and display it
  #############################################################################################################

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


  #############################################################################################################
  # Function loadLabelingStatistics - read labeling statistics from the log and display it to user
  #############################################################################################################
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
  #############################################################################################################
  # Function readFileByChunks - read chars from the file by chunks
  # The function is used to set pointer in large text
  #############################################################################################################
  readFileByChunks <-function(in_file, nchar,length_of_text_chunk = 5e7){
    while (nchar>length_of_text_chunk){
      readChar(in_file,length_of_text_chunk)
      nchar <- nchar - length_of_text_chunk
    }
    if(nchar>1)
      readChar(in_file,nchar)
  }
  #############################################################################################################
  # Function setCursorInTextFile - set file pointer to the new_pos position
  # The function is used for preprocessing of large texts
  #############################################################################################################
  setCursorInTextFile<-function (in_file, fileName, curr_pos, new_pos, length_of_text_chunk = 5e7){

    if (curr_pos == new_pos)
      return()

    if(new_pos<curr_pos){
      close(in_file)
      in_file = file(fileName, "r")
      open(in_file)
      curr_pos <- 1
    }

    if (new_pos>1){
      if (new_pos>curr_pos)
        readFileByChunks(in_file,new_pos-curr_pos,length_of_text_chunk)
      else readFileByChunks(in_file,new_pos,length_of_text_chunk)
    }
  }
  #############################################################################################################
  # Function getExamples - returns string with 10 examples of appearance "pattern" string in "source_txt" text.
  # The search of matches begins from the "last_pos" position in source_txt in forward direction (direction=1) or reverse direction (direction=0)
  #############################################################################################################
  getExamples <- function(direction,pattern, last_pos,control_id,length_of_text_chunk=2e7){

    # txt file for examples
    if (n_grams==1)
      fileName <- paste0(app_dir,"train.txt")
    else if(n_grams==2)
      fileName <- paste0(app_dir,"train2.txt")
    else if(n_grams==4)
      fileName <- paste0(app_dir,"train4.txt")

    example_str <- NimbleMiner::getExamples(pattern, fileName, control_id, last_pos,direction,length_of_text_chunk)

    return (example_str)
  }


  #############################################################################################################
  # Function getExamples_negatedNotes - returns positive notes negated by this negation
  #############################################################################################################
  getExamples_negatedNotes <- function(negation){

    df_negatedNotes = df_negatedPosLabels[ grepl(negation,df_negatedPosLabels$Negation),]

    examples_str <- ""

    if (nrow(df_negatedNotes)<200)
      examples_count <- nrow(df_negatedNotes)
    else examples_count <- 200

    for (i in 1:examples_count){
      examples_str <- paste0(examples_str,"<div class='example-text'>",df_negatedNotes[i,"Note"],"</div>")
    }

    return (examples_str)
  }

  #############################################################################################################
  # Function getExamples_irrelevantNotes - returns notes with irrelevant simclins
  #############################################################################################################
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

  #############################################################################################################
  # Function logAction - adds the new record to the log
  #############################################################################################################
  logAction <- function(actionDataTime=Sys.time(),userId,operation,parameters="",valueBefore="",valueAfter="",actionDuration=""){

    valueBefore <- ifelse(is.null(valueBefore),"",valueBefore)
    valueAfter <- ifelse(is.null(valueAfter),"",valueAfter)

    df_new_record <- data.frame(DateTime=format(actionDataTime, "%Y-%m-%d %H:%M:%S"), UserId = userId, Operation = operation, Parameters = parameters, ValueBefore=valueBefore,
                                ValueAfter=valueAfter,Duration=actionDuration, stringsAsFactors=FALSE)
    df_log <<- rbind(df_log,df_new_record)
    refreshTable('log')
  }

  #############################################################################################################
  # Function addNewSimclin - adds the new record to the simclin's list
  #############################################################################################################
  addNewSimclin <- function(new_simclin_str,df_new_simclins,category = NULL,fl_show_msg_about_duplicates = TRUE, fl_from_file = FALSE ){

    original_simclin_str = new_simclin_str
    # trim the new simclin
    new_simclin_str = enc2utf8(new_simclin_str)
    new_simclin_str = gsub("_", " ", trimws(new_simclin_str))
    new_simclin_str = removePunctuation(new_simclin_str,preserve_intra_word_contractions = FALSE,preserve_intra_word_dashes = FALSE,ucp = TRUE)
    new_simclin_str = gsub("[[:space:]]", "_", trimws(new_simclin_str))
    new_simclin_str=stri_trans_tolower(new_simclin_str)


    if(nrow(df_simclins)>0 && any(df_simclins[df_simclins[,'Category']==category,]$Simclins==new_simclin_str)) {
      if (fl_show_msg_about_duplicates)
        showModal(modalDialog(title = "Error message",  paste0("The simclin \"",new_simclin_str,"\" is in the list already!"),easyClose = TRUE))
      print(paste0("Duplicated simclin: ",original_simclin_str," to existing simclin ", new_simclin_str," with same category ",category))
      return(FALSE)
    } else {


      if(new_simclin_str=="") {
        if (fl_show_msg_about_duplicates)
          showModal(modalDialog(title = "Error message",  paste0("The simclin is empty!"),easyClose = TRUE))
        return(FALSE)
      }

      df_simclins<<-NimbleMiner::addNewSimclin(new_simclin_str,df_simclins,category)
      #clean the input control
      if(!fl_from_file){
        updateTextInput(session,"newWord_input", value = " ")
        refreshTable('simclins')
      }
      return(TRUE)
    }
  }

  #############################################################################################################
  # Function getLexicalOrigin - check if current term (x[1]) includes substring from regex pattern (argument pattern).
  #                             For example, term "duodenal_perforation" is lexical variation of the "perforation". So, the
  #                             x[1] = "duodenal_perforation", pattern = "\b("perforation")|(...)(?!/w)"
  #                             argument x should be the vector with 2 elements - x[1] with term and x[2] with category,
  #                             the x[2] is optional - only for the case the argument selectedCategory = NULL
  #############################################################################################################

  getLexicalOrigin <- function(x,pattern,selectedCategory = NULL){

    curr_similar_term <- x[1]  #$Similar_term

    if (!is.null(selectedCategory)){
      curr_category <- as.character(x[2])  #$category
      if (curr_category!=selectedCategory) return(NA)
    }

    curr_similar_term <- enc2utf8(curr_similar_term)
    curr_similar_term<-gsub("_","",curr_similar_term)

    pattern <- enc2utf8(pattern)
    pattern <- gsub("\\+","",pattern)
    pattern<-gsub("_","",pattern)

    list_simclins = stri_locate_all(curr_similar_term, regex = pattern, opts_regex=stri_opts_regex(case_insensitive=TRUE))

    pos_start = list_simclins[[1]][1,'start']
    pos_end   = list_simclins[[1]][1,'end']

    if(!is.na(pos_start) & !is.na(pos_end)) {
      print(paste0(x[1]," - lexical variant of ",substring(curr_similar_term,pos_start,pos_end)))
      return(substring(curr_similar_term,pos_start,pos_end))
    } else return(NA)
  }

  #############################################################################################################
  # Function getLevensteinOrigin - check if current term (x[1]) is closed by Levenstein metrics to any item
  #                                from curr_simclins
  #############################################################################################################
  getLevensteinOrigin <- function(x,curr_simclins,selectedCategory,max){
    curr_similar_term <- x[1]  #$Similar_term
    if (!is.null(selectedCategory)){
      curr_category <- as.character(x[2])  #$category
      if (curr_category!=selectedCategory) return(NA)
    }
    curr_similar_term <- enc2utf8(curr_similar_term)
    curr_similar_term<-gsub("_","",curr_similar_term)

    closest_by_lv <- closestByLevenstein (curr_similar_term, gsub("_","",curr_simclins$Simclins), max = max)
    if (length(closest_by_lv)>0 & any(closest_by_lv)){
      print(paste0(x[1]," - close by Levenstein to: ",paste(curr_simclins[closest_by_lv,'Simclins'],collapse = ",")))
      if(is.na(x['Lexical_variant']))
        return(paste(curr_simclins[closest_by_lv,'Simclins'],collapse = ","))
      else if(grepl(paste0("\\b(",x['Lexical_variant'],")(?!\\w)"),curr_simclins[closest_by_lv,'Simclins'],ignore.case = T,perl=T))
        return(paste(curr_simclins[closest_by_lv,'Simclins'],collapse = ","))
      else return(paste0(x['Lexical_variant'],", ",paste(curr_simclins[closest_by_lv,'Simclins'],collapse = ",")))
    } else return(x['Lexical_variant'])
  }



  #############################################################################################################
  # Handler of event of click button 'Select lexical variants of simclins' (tab 2. SImclins explorer)
  # Select similar terms, which are close by Levenstein, or are lexical variants of current simclins
  #############################################################################################################
  observeEvent(input$selectLexicalVariants_click, {

    selectedCategory_str<- getSelectedCategory(input$simclins_tree_settings)
    df_simclins_of_cat <- df_simclins[df_simclins$Category==selectedCategory_str,]
    df_similar_terms_of_cat <- df_similar_terms[df_similar_terms$Category==selectedCategory_str,]

    pattern_str <- paste(df_simclins_of_cat$Simclins,collapse = "|")

    pattern_str <- gsub("_","",df_simclins_of_cat$Simclins)
    df_similar_terms_of_cat$Lexical_variant <- gsub("_","",df_similar_terms_of_cat$Lexical_variant)

    df_similar_terms_of_cat$Lexical_variant<-apply(df_similar_terms_of_cat,1,getLexicalOrigin,pattern_str,selectedCategory_str)
    df_similar_terms_of_cat$Lexical_variant<-apply(df_similar_terms_of_cat,1,getLevensteinOrigin, df_simclins_of_cat, selectedCategory_str, max = 2)

    is_lexical_variant_rownames <- rownames(df_similar_terms_of_cat[!is.na(df_similar_terms_of_cat$Lexical_variant),])
    df_similar_terms[is_lexical_variant_rownames,'Lexical_variant']<<-df_similar_terms_of_cat[is_lexical_variant_rownames,'Lexical_variant']

    is_lexical_variant_indx <- match(is_lexical_variant_rownames,rownames(df_similar_terms))

    # is_from_several_models_rownames <- rownames(df_similar_terms_of_cat[grepl(',',df_similar_terms_of_cat$Model),])
    # is_from_several_models_indx <- match(is_from_several_models_rownames,rownames(df_similar_terms))

    selected_rows <- c(is_lexical_variant_indx)

    selected_rows <- unique(selected_rows)
    selected_rows <-sort(selected_rows)

    output$similar_terms_table = DT::renderDataTable({DT::datatable(df_similar_terms,selection = list(target = 'row', selected=selected_rows),
                                                                    filter = list(position = 'top', clear = FALSE),
                                                                    options = list(order=list(list(4,'asc')),pageLength = 100,columnDefs = list(list(targets = c(2,3,4,5,6), searchable = FALSE))
                                                                                   ,searchCols = list(NULL, list(search = selectedCategory_str),NULL,NULL,NULL,NULL,NULL)

                                                                    )
                                                                    ,callback=DT::JS('table.on("page.dt",function() {var topPos = document.getElementById("div_similar_terms").offsetTop; window.scroll(0,topPos);})')
                                                                    ,colnames = c("Similar terms","Category","Distance","By simclins",'Lexical variant of',"Model","Examples"),rownames=FALSE, escape = FALSE)})
    write.csv(df_similar_terms, file = paste0(app_dir,"similar_terms.csv"))
  })


  #############################################################################################################
  # Function refreshTable - outputs data from dataframe to the table and save data to csv-file
  #############################################################################################################
  refreshTable <- function(tableName, saveSelection = FALSE){

    if (tableName=='similar_terms') {

      selectedCategory_str<- getSelectedCategory(input$simclins_tree_settings)
      df_simclins_of_cat <- df_simclins[df_simclins$Category==selectedCategory_str,]
      df_similar_terms_of_cat <- df_similar_terms[df_similar_terms$Category==selectedCategory_str,]

      #update value of "lexical variant of"
      pattern_str <- paste(df_simclins_of_cat$Simclins,collapse = "|")
      df_similar_terms_of_cat$Lexical_variant<-apply(df_similar_terms_of_cat,1,getLexicalOrigin,pattern_str,selectedCategory_str)
      df_similar_terms_of_cat$Lexical_variant<-apply(df_similar_terms_of_cat,1,getLevensteinOrigin, df_simclins_of_cat, selectedCategory_str, max = 2)

      is_lexical_variant_rownames <- character(0)
      is_lexical_variant_indx <- integer(0)

      # if it's the first view of the search results - get rownames for selecting
      if(fl_first_view_of_search_results) {
        if(nrow(df_similar_terms_of_cat)>0){
          is_lexical_variant_rownames <- rownames(df_similar_terms_of_cat[!is.na(df_similar_terms_of_cat$Lexical_variant),])
          df_similar_terms[is_lexical_variant_rownames,'Lexical_variant']<<-df_similar_terms_of_cat[is_lexical_variant_rownames,'Lexical_variant']
          is_lexical_variant_indx <- match(is_lexical_variant_rownames,rownames(df_similar_terms))
        }
      }


      if((saveSelection & !is.null(input$similar_terms_table_rows_selected)) | length(is_lexical_variant_indx)>0 | fl_selectAllSimilar_terms | fl_deselectAllSimilar_terms){

        if (fl_selectAllSimilar_terms){
          fl_selectAllSimilar_terms <<- FALSE
          selected_rows <-  match(rownames(df_similar_terms_of_cat),rownames(df_similar_terms))
        } else if (fl_deselectAllSimilar_terms){
          fl_deselectAllSimilar_terms <<- FALSE
          selected_rows <- NULL
        } else{
          if (fl_next_search_in_process)
            selected_rows <- c(is_lexical_variant_indx)
          else selected_rows <- c(input$similar_terms_table_rows_selected,is_lexical_variant_indx)

          selected_rows <- unique(selected_rows)
          selected_rows <-sort(selected_rows)
        }
        df_similar_terms$Category <- factor(df_similar_terms$Category)
        output$similar_terms_table = DT::renderDataTable(df_similar_terms,selection = list(target = 'row', selected=selected_rows),
                                                         filter = list(position = 'top', clear = FALSE),
                                                         options = list(order=list(list(4,'asc'),list(2,'desc')),pageLength = 100,columnDefs = list(list(targets = c(2,3,4,5,6), searchable = FALSE))
                                                                        ,searchCols = list(NULL, list(search = paste0('["',selectedCategory_str,'"]')),NULL,NULL,NULL,NULL,NULL)

                                                         )
                                                         ,callback=DT::JS('table.on("page.dt",function() {var topPos = document.getElementById("div_similar_terms").offsetTop; window.scroll(0,topPos);})')
                                                         ,colnames = c("Similar terms","Category","Distance","By simclins",'Lexical variant of',"Model","Examples"),rownames=FALSE, escape = FALSE)
        write.csv(df_similar_terms, file = paste0(app_dir,"similar_terms.csv"))
      } else {
        df_similar_terms$Category <- factor(df_similar_terms$Category)
        output$similar_terms_table = DT::renderDataTable(df_similar_terms,
                                                         filter = list(position = 'top', clear = FALSE),
                                                         options = list(order=list(list(4,'asc'),list(2,'desc')),pageLength = 100,columnDefs = list(list(targets = c(2,3,4,5,6), searchable = FALSE))
                                                                        ,searchCols = list(NULL, list(search = paste0('["',selectedCategory_str,'"]')),NULL,NULL,NULL,NULL,NULL)

                                                         )
                                                         ,callback=DT::JS('table.on("page.dt",function() {var topPos = document.getElementById("div_similar_terms").offsetTop; window.scroll(0,topPos);})')
                                                         ,colnames = c("Similar terms","Category","Distance","By simclins",'Lexical variant of',"Model","Examples"),rownames=FALSE, escape = FALSE)

        write.csv(df_similar_terms, file = paste0(app_dir,"similar_terms.csv"))
      }
    }

    else if (tableName=='simclins') {

      selectedCategory_str<- getSelectedCategory(input$simclins_tree_settings)

      # update tables

      if(saveSelection & !is.null(input$simclins_table_rows_selected)){
        df_simclins$Category <- factor(df_simclins$Category)
        output$simclins_table = DT::renderDataTable(df_simclins,selection = list(selected=input$simclins_table_rows_selected),
                                                    filter = list(position = 'top', clear = FALSE),
                                                    options = list(order=list(0, 'asc'),pageLength = 10,columnDefs = list(list(targets = c(2,3), searchable = FALSE))
                                                                   ,searchCols = list(NULL, list(search = paste0('["',selectedCategory_str,'"]')),NULL,NULL)
                                                    ),colnames = c("Simclins","Category","Processed","Examples"),rownames=FALSE, escape = FALSE)
      }  else {
        df_simclins$Category <- factor(df_simclins$Category)
        output$simclins_table = DT::renderDataTable(df_simclins,
                                                    filter = list(position = 'top', clear = FALSE),
                                                    options = list(order=list(0, 'asc'),pageLength = 10,columnDefs = list(list(targets = c(2,3), searchable = FALSE))
                                                                   ,searchCols = list(NULL, list(search =paste0('["',selectedCategory_str,'"]')),NULL,NULL)
                                                    ),colnames = c("Simclins","Category","Processed","Examples"),rownames=FALSE, escape = FALSE)

      }


      write.csv(df_simclins, file = paste0(app_dir,"simclins.csv"), fileEncoding = "UTF-8")

    } else if (tableName=='log'){
      output$log_table <- DT::renderDataTable({DT::datatable(df_log,options = list(order=list(0,'desc')),colnames = c("Date & time","User Id","Operation","Parameters","Value before","Value after","Duration"),rownames=FALSE, escape = FALSE)})
      write.csv(df_log, file = paste0(app_dir,"log.csv"), fileEncoding = "UTF-8")
    } else if (tableName=='irrelevant_terms'){
      selectedCategory_str<- getSelectedCategory(input$simclins_tree_settings)
      #df_irrelevant_terms <<- df_irrelevant_terms[!is.na(df_irrelevant_terms$Similar_term),]
      df_irrelevant_terms$Category <- factor(df_irrelevant_terms$Category)
      if(saveSelection & !is.null(input$irrelevant_similar_terms_table_rows_selected)){
        output$irrelevant_similar_terms_table = DT::renderDataTable(df_irrelevant_terms,selection = list(selected=input$irrelevant_similar_terms_table_rows_selected),
                                                                    filter = list(position = 'top', clear = FALSE),
                                                                    options = list(order=list(0, 'asc'),pageLength = 10,columnDefs = list(list(targets = c(2,3), searchable = FALSE))
                                                                                   ,searchCols = list(NULL, list(search = paste0('["',selectedCategory_str,'"]')),NULL,NULL)
                                                                    ),colnames = c("Similar terms","Category","Frequency","Examples"),rownames=FALSE, escape = FALSE)
      } else {

        output$irrelevant_similar_terms_table = DT::renderDataTable(df_irrelevant_terms,
                                                                    filter = list(position = 'top', clear = FALSE),
                                                                    options = list(order=list(0, 'asc'),pageLength = 10,columnDefs = list(list(targets = c(2,3), searchable = FALSE))
                                                                                   ,searchCols = list(NULL, list(search = paste0('["',selectedCategory_str,'"]')),NULL,NULL)
                                                                    ),colnames = c("Similar terms","Category","Frequency","Examples"),rownames=FALSE, escape = FALSE)
      }

      write.csv(df_irrelevant_terms, file = paste0(app_dir,"irrelevant_terms.csv"), fileEncoding = "UTF-8")
    } else if (tableName=='negations'){
      output$negations_table <- DT::renderDataTable(df_negations,
                                                    filter = list(position = 'top', clear = FALSE),
                                                    options = list(order=list(3,'desc'),pageLength = 10,columnDefs = list(list(targets = c(0,1,3,4), searchable = FALSE))
                                                                   ,searchCols = list(NULL,NULL,list(search = "General"),NULL,NULL)),
                                                    colnames = c('Negation','Type','Category','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)

      output$curr_category_negations_table <- DT::renderDataTable(df_negations,
                                                                  filter = list(position = 'top', clear = FALSE),
                                                                  options = list(order=list(3,'desc'),pageLength = 10,columnDefs = list(list(targets = c(0,1,3,4), searchable = FALSE))
                                                                                 ,searchCols = list(NULL,NULL,list(search = userSettings$selectedCategory),NULL,NULL)),
                                                                  colnames = c('Negation','Type','Category','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)
      write.csv(df_negations, file = paste0(app_dir,"negations.csv"), fileEncoding = "UTF-8")
    } else if (tableName=='exceptions'){
      output$exceptions_table  <- DT::renderDataTable(df_exceptions,
                                                      filter = list(position = 'top', clear = FALSE),
                                                      options = list(order=list(2,'desc'),pageLength = 10,columnDefs = list(list(targets = c(0,2,3), searchable = FALSE))
                                                                     ,searchCols = list(NULL,list(search = "General"),NULL,NULL)),
                                                      colnames = c('Exception','Category','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)
      output$curr_category_exceptions_table  <- DT::renderDataTable(df_exceptions,
                                                                    filter = list(position = 'top', clear = FALSE),
                                                                    options = list(order=list(2,'desc'),pageLength = 10,columnDefs = list(list(targets = c(0,2,3), searchable = FALSE))
                                                                                   ,searchCols = list(NULL,list(search = userSettings$selectedCategory),NULL,NULL)),
                                                                    colnames = c('Exception','Category','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)


      write.csv(df_exceptions, file = paste0(app_dir,"negations-exceptions.csv"), fileEncoding = "UTF-8")
    }
  }

  #############################################################################################################
  # Function refreshSystemEfficacy - updates system efficacy and save it in log
  #############################################################################################################

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
  #############################################################################################################
  # function addNode2TreeList - add new category to the tree
  #############################################################################################################
  addNode2TreeList <- function(new_node,tree_df){
    #add new element to list
    tree_list <- list()
    node_text_str <- new_node[1,'text']
    tree_list <- c(tree_list,node_text_str)
    names(tree_list)[length(tree_list)]<-node_text_str

    node_children_df <- tree_df[tree_df[,'parent']==new_node[1,'id'],]

    if(nrow(node_children_df)>0){
      children_list <- list()
      for(j in 1:nrow(node_children_df)){
        children_list <- c(children_list, addNode2TreeList(node_children_df[j,],tree_df))
      }

      structure_ls <- structure(children_list,stselected = new_node[1,'state']$selected,stopened = new_node[1,'state']$opened)
    } else {
      structure_ls <- structure("",stselected = new_node[1,'state']$selected,stopened = new_node[1,'state']$opened)
    }
    tree_list[length(tree_list)] <- list(structure_ls)

    tree_list
  }
  #############################################################################################################
  # function addNode2TreeList - add new category to the tree
  #############################################################################################################
  treedf2list <- function(df){
    df_root<-df[df[,'parent']=='#',]
    res_list <- list()

    for(i in 1:nrow(df_root)){
      res_list <- c(res_list,addNode2TreeList(df_root[i,],df))
    }
    res_list
  }
  #############################################################################################################
  # function get_flatList - convert categories from tree structure to flat structure
  #############################################################################################################
  get_flatList <- function(nestedList, flatList = NULL, parent = "#") {
    for (name in names(nestedList)) {
      flatList = c(flatList,
                   list(
                     list(
                       id = as.character(length(flatList) + 1),
                       text = name,
                       parent = parent,
                       state = list(
                         opened   = isTRUE(attr(nestedList[[name]], "stopened")),
                         selected = isTRUE(attr(nestedList[[name]], "stselected"))
                       )
                     )
                   ))
      if (is.list(nestedList[[name]]))
        flatList =
          get_flatList(nestedList[[name]], flatList, parent = as.character(length(flatList)))
    }
    flatList
  }
  #############################################################################################################
  # 1. Build the model
  #############################################################################################################
  #############################################################################################################
  # Function buildModels - call functions for training all models
  #############################################################################################################
  buildModels <- function(model_names = c('word2vec'), app_dir,n_grams = 2, layers = 100 , window, min_count = 20){

    # create a progress object
    progress <- shiny::Progress$new(min=1, max=4)
    on.exit(progress$close())

    #pre-process the data
    progress$set(message = "Building the model", detail = "data pre-processing...", value = 1)
    fileName_source <- paste0(app_dir,"train.txt")
    if(input$buildModel_startStep_input == "TP" &  (!file.exists(fileName_source) | file.info(fileName_source)$size==0)){
      showModal(modalDialog(title = "Error message",  paste0("The file for pre-processing ",fileName_source," was not found."),easyClose = TRUE))
      return()
    }


    fileName_clean <- paste0(app_dir,"train1.txt")

    if(input$buildModel_startStep_input == "TP")
      NimbleMiner::TextPreprocess(fileName_in = fileName_source, fileName_out = fileName_clean)

    if(!file.exists(fileName_clean) | file.info(fileName_clean)$size==0){
      showModal(modalDialog(title = "Error message",  paste0("The file for model training ",fileName_clean," was not found."),easyClose = TRUE))
      return()
    }

    start_time <- Sys.time()
    progress$set(detail = "training model...", value = 3)

    if('word2vec' %in% model_names)
      # building word2vec word embedding model
      buildModel_word2vec(app_dir,n_grams, layers, window, min_count)

    if('GloVe' %in% model_names)
      # building Glove word embedding model (Assignment #1)
      #buildModel_Glove(app_dir,layers, min_count)
      buildModel_GloVe("train1.txt",n_grams = n_grams, const_layer_size = layers,  window = window, min_count = min_count)
    #if('Elmo' %in% model_names)
    # building elmo word embedding model  (Assignment #1)
    # buildModel_Elmo()
    #if('WS' %in% model_names)
    # building word similarity  embedding model (Assignment #2)
    # buildModel_WS()

    progress$set(detail = "the model is ready!", value = 4)

    process_duration <- as.character(round((difftime(Sys.time(),start_time,units = "mins")),2))
    logAction(actionDataTime = start_time, userId = currentUserId, operation = "Build the model",parameters = paste0("Model name:",paste(unlist(model_names),sep = ", ")," n-grams: ",n_grams,"; window: ",input$setting_window,"; min number: ",input$setting_min_count,"; layers number: ", const_layer_size,
                                                                                                                     "; source file: ", input$file_input['name'],"."), actionDuration = process_duration )

    showModal(modalDialog(title = "Model building",  paste0("The model was built within ",process_duration," minutes."),easyClose = TRUE))

    return()
  }

  #############################################################################################################
  # Handler event of button click 'Build the model' (tab 1. Model builder)
  # Build the word2vec model
  #############################################################################################################
  observeEvent(input$buildModel_word2vec_click, {

    buildModels('word2vec',app_dir,n_grams, const_layer_size , input$setting_window, input$setting_min_count)

  })

  #############################################################################################################
  # Function fun_word2vec - for Windows call C-function from dll/word2vec.dll.
  #                         for other OS call word2vec from rword2vec package
  #############################################################################################################
  fun_word2vec <- function(input_filename, output_filename,
                           binary=1, # output format, 1-binary, 0-txt
                           cbow=0, # skip-gram (0) or continuous bag of words (1)
                           num_threads = 4, # num of workers
                           num_features = 100, # word vector dimensionality
                           window = 10, # context / window size
                           min_count = 20, # minimum word count
                           sample = 0.001, # downsampling of frequent words
                           classes = 0 # if >0 make k-means clustering
  )
  {
      wordVectors::train_word2vec(train_file = input_filename,output_file = output_filename,cbow = 0, threads = num_threads,window = window,vectors = num_features,min_count = min_count,classes = classes, force = TRUE)
  }
  #############################################################################################################
  # Function fun_word2phrase - for Windows call C-function from dll/word2phrase.dll.
  #                         for other OS call word2phrase from rword2vec package
  #############################################################################################################
  fun_word2phrase <- function(input_filename, output_filename,
                              min_count = 20, # minimum word count
                              threshold=100) # The <float> value represents threshold for forming the phrases (higher means less phrases); default 100                        )
  {
      wordVectors::word2phrase(train_file = input_filename,output_file = output_filename,min_count = min_count, threshold = threshold, force = TRUE)
  }
  #############################################################################################################
  # Function buildModel_word2vec - build word2vec word embedding model
  #############################################################################################################
  buildModel_word2vec <- function(model_dir,n_grams =2, const_layer_size = 100, window = 10, min_count = 10){

    #create bigram model- the file path here needs to be without spaces- otheriwise it doesn't work!!!
    if(n_grams>1){
      cat("creating bigram model...")
      #create bigram trained word2vec here
      fun_word2phrase(paste0(model_dir,"train1.txt"),paste0(model_dir,"train2.txt"),min_count)

      if(n_grams==2){
        fun_word2vec(paste0(model_dir,"train2.txt"), paste0(model_dir,"train.bin"), num_features = const_layer_size, window = window,  min_count = min_count)
      } else  if(n_grams==4){
        #create up to four gram model
        cat("creating up to four gram model...")
        fun_word2phrase(paste0(model_dir,"train2.txt"),paste0(model_dir,"train4.txt"), min_count=min_count)

        cat("creating final model...")
        fun_word2vec(paste0(model_dir,"train4.txt"), paste0(model_dir,"train.bin"),num_features = const_layer_size, window = window, min_count = min_count)
      }
    } else {
      cat("creating unigram model...")
      fun_word2vec(paste0(model_dir,"train1.txt"), paste0(model_dir,"train.bin"),num_features = const_layer_size, window = window, min_count = min_count)
    }

  }
  #############################################################################################################
  # Function buildModel_GloVe - build GloVe word embedding model
  #############################################################################################################
  buildModel_GloVe <- function(model_filename,n_grams =2, const_layer_size = 100,  window = 10, min_count = 10){

    library(text2vec)

    if(!file.exists(paste0(app_dir,model_filename))) {
      cat('Error:The file with text ',paste0(app_dir,model_filename),' is not found!')
      return('')
    }

    file_size = file.info(paste0(app_dir,model_filename))$size
    in.file = file(model_filename, "r")

    start_time <- Sys.time()
    length_of_text_chunk = 5e7
    train_text = ""
    curr_chunk <- readChar(in.file,length_of_text_chunk)
    length_of_curr_chunk <- nchar(curr_chunk)
    i=1
    tokens = c()
    while( length_of_curr_chunk >0  ) {
      print(paste("Tokenizing ", i," part of text..."))
      if(length_of_curr_chunk>0){

        #includes whole last word to the current chunk
        length_end_of_word = 1
        while (length_end_of_word>0 & grepl("\\w",substring(curr_chunk,length_of_curr_chunk,length_of_curr_chunk))) {
          end_of_word <- readChar(in.file,1)
          length_end_of_word <- ifelse(length(end_of_word)==0,0,nchar(end_of_word))
          curr_chunk <- paste0(curr_chunk,end_of_word)
          length_of_curr_chunk = length_of_curr_chunk+1
        }

        tokens = c(tokens,word_tokenizer(curr_chunk))
        i=i+1
        curr_chunk <- readChar(in.file,length_of_text_chunk)
        length_of_curr_chunk <- ifelse(length(curr_chunk)==0,0,nchar(curr_chunk))
      }
    }
    close( in.file )

    print(paste0("word_tokenizer time:",as.character(round((difftime(Sys.time(),start_time,units = "secs")),2))))

    start_time <- Sys.time()
    it = itoken(tokens)
    print(paste0("itoken time:",as.character(round((difftime(Sys.time(),start_time,units = "secs")),2))))

    stopword <- tm::stopwords("SMART")
    start_time <- Sys.time()
    v = create_vocabulary(it,stopwords=stopword, ngram = c(1, n_grams), sep_ngram = "_")
    print(paste0("create_vocabulary time:",as.character(round((difftime(Sys.time(),start_time,units = "secs")),2))))
    v =  prune_vocabulary(v, term_count_min = min_count)

    # create co-occurrence vectorizer
    start_time <- Sys.time()
    vectorizer = vocab_vectorizer(v)
    print(paste0("create co-occurrence vectorizer time:",as.character(round((difftime(Sys.time(),start_time,units = "secs")),2))))

    start_time <- Sys.time()
    tcm = create_tcm(it, vectorizer, skip_grams_window = window, skip_grams_window_context  = "symmetric")
    print(paste0("create tcm time:",as.character(round((difftime(Sys.time(),start_time,units = "secs")),2))))


    glove_model = GloVe$new(word_vectors_size  = const_layer_size, vocabulary = v, x_max = 10, learning_rate = .25)

    start_time <- Sys.time()
    word_vectors_main = glove_model$fit_transform(tcm, n_iter = 10)
    print(paste0("fit GloVe model time:",as.character(round((difftime(Sys.time(),start_time,units = "secs")),2))))

    word_vectors_context = glove_model$components
    word_vectors = word_vectors_main + t(word_vectors_context)

    start_time <- Sys.time()
    saveRDS(word_vectors, paste0(app_dir,"train_Glove.rds" ))
    print(paste0("saveRDS time:",as.character(round((difftime(Sys.time(),start_time,units = "secs")),2))))

  }
  #############################################################################################################
  # Handler event of button click 'Build GloVe model' (tab 1. Model builder)
  # Build the GloVe model
  #############################################################################################################
  observeEvent(input$buildModel_GloVe_click, {

    buildModels('GloVe',app_dir,n_grams, const_layer_size , input$setting_window, input$setting_min_count)

  })

  #############################################################################################################
  # 2. Simclins explorer
  #############################################################################################################
  #############################################################################################################
  # Function loadCategoryTree - Simclins categories tree upload
  #############################################################################################################
  loadCategoryTree <- function (){

    output$simclins_tree_settings <- renderTree({
      userSettings$selectedCategory <- getSavedSelectedCategory()
      tree2df()
    })
  }

  #############################################################################################################
  # Handler event of button click 'Add' (tab. 2. Simclin explorer, table Simclins)
  # Add new simclin by user enter
  #############################################################################################################
  observeEvent(input$addNewWord_click, {
    category_str = getSelectedCategory(input$simclins_tree_settings)
    if (category_str!=""){
      if (addNewSimclin(input$newWord_input,TRUE,category_str))
        # save user action in log file
        logAction(userId = currentUserId, operation = "Add user simclin",valueAfter=input$newWord_input)

    } else {
      showModal(modalDialog(title = "Error Message",  "Please select the category for the new simclin!",easyClose = TRUE))
    }


  })

  #############################################################################################################
  # Function getColumnToUpload - define column with simclins in specified for simclins upload file
  # Define column in dataframe data by name columnName (case insensitive) or by type columnTypes
  #############################################################################################################
  getColumnToUpload <- function(data,columnName,columnTypes){
    colnames(data)<-tolower(colnames(data))
    columnIndex <- match (tolower(columnName),colnames(data),nomatch=0)
    if(columnIndex==0)
      columnIndex <- match(columnTypes,sapply(data, class),nomatch=0)
    return(columnIndex)
  }
  #############################################################################################################
  # Handler event of button click 'Upload' (tab. 2. Simclin explorer, table Simclins, section Load new simclins from file)
  # Load new simclins from specified file
  #############################################################################################################
  observeEvent(input$loadNewSimclinsFromCSV_click, {

    newSimclinsFile <- input$loadNewSimclinsFromCSV

    if (is.null(newSimclinsFile)) {
      showModal(modalDialog(title = "Error Message",  "Please specify the file with the new simclins!",easyClose = TRUE))
    } else {
      progress <- shiny::Progress$new(min=1, max=3)
      on.exit(progress$close())
      progress$set(message = 'Reading a file with simclins',value = 1)

      df_new_simclins <-  read.csv(newSimclinsFile$datapath,header = TRUE, stringsAsFactors=FALSE)
      #simclins_column_index <- getColumnToUpload(df_new_simclins,'simclins',c('factor','character'))

      if(nrow(df_new_simclins)==0){
        showModal(modalDialog(title = "Error Message",  "There are no any simclin in the specified file!",easyClose = TRUE))
        return()
      }

      fl_new_simclins <- c()
      for (i in 1:ncol(df_new_simclins)) {

        category_str <- addNewCategoryToTree(colnames(df_new_simclins)[i],FALSE,TRUE)

        if (category_str!=""){
          progress$set(message = paste0('Loading simclins to category ',category_str,'...'),value = 3)
          fl_new_uploaded_simclins <- lapply(df_new_simclins[,i],addNewSimclin,fl_show_msg_about_duplicates = FALSE,category = category_str)
          fl_new_simclins <- c(fl_new_uploaded_simclins,fl_new_simclins)
        }
      }

      if(any(fl_new_simclins)){
        logAction(userId = currentUserId, operation = "Load user simclins from csv file",parameters = newSimclinsFile$name, valueAfter = paste0(as.character(sum(as.numeric(fl_new_simclins)))," simclins were loaded from the file."))
        #update the category tree in the settings section
        filename <- paste0(app_dir,"simclins_tree.csv")
        simclins_tree_json <-  readLines(filename,encoding="UTF-8")
        simclins_tree_df<-jsonlite::fromJSON(simclins_tree_json)
        result_list<-treedf2list(simclins_tree_df)
        updateTree(session,"simclins_tree_settings",result_list)
      }

      showModal(modalDialog(title = "Information",  paste0("The ",as.character(sum(as.numeric(fl_new_simclins)))," simclins were loaded from the file!"),easyClose = TRUE))
    }
  })
  #############################################################################################################
  # Function dataModal - display modal window with specified htmlContent content
  # Used to display examples of simclins in train file
  #############################################################################################################
  dataModal <- function(htmlContent, failed = FALSE) {
    showModal(modalDialog(inputId = 'dialog_examples', title = "Examples", HTML(htmlContent),easyClose = TRUE, footer = modalButton("Close") ))
  }
  #############################################################################################################
  # Handler event of click button Example in the row of simclins table
  # Call functions to display examples for current simclin, negation or irrelevant term
  #############################################################################################################
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
  #############################################################################################################
  # Handler event of click button 'Delete selected simclins' (tab 2. Simclins explorer)
  # Move deleted simclins to the list of irrelevant terms
  #############################################################################################################
  observeEvent(input$deleteSimclin_click, {

    category_str = getSelectedCategory (input$simclins_tree_settings)
    if (category_str!=""){

      if(!is.null(input$simclins_table_rows_selected)){
        # save selected simclins in irrelevant similar terms
        df_selected_rows = data.table (Similar_term=df_simclins[input$simclins_table_rows_selected,"Simclins"],Category=category_str,Frequency=NA,Examples=NA)
        deleted_simclins_str = paste0(as.character(nrow(df_selected_rows))," simclins deleted: ",paste(df_selected_rows[['Similar_term']],collapse=" ")," from the category ",category_str,".")
        logAction (userId = currentUserId, operation = "Delete simclins", valueAfter = deleted_simclins_str)
        df_irrelevant_terms <<- rbind(df_irrelevant_terms, df_selected_rows)
        rm(df_selected_rows)
        #exclude duplicates in list
        df_irrelevant_terms<<-df_irrelevant_terms[ order(df_irrelevant_terms$Similar_term, na.last = TRUE, decreasing = FALSE), ]
        df_irrelevant_terms<<- df_irrelevant_terms[ !duplicated(df_irrelevant_terms$Similar_term), ]
        refreshTable('irrelevant_terms')

        # df_simclins <<- df_simclins[-input$simclins_table_rows_selected,]
        df_simclins <<- df_simclins[!(df_simclins$Simclins %in% df_simclins[input$simclins_table_rows_selected,"Simclins"] &  df_simclins$Category==category_str),]

        refreshTable('simclins')
      } else {
        showModal(modalDialog(title = "Error message",  "There are no any selected simclins to delete!",easyClose = TRUE))
      }
    } else {
      showModal(modalDialog(title = "Error message",  "Please select the category of simclins to delete!",easyClose = TRUE))
    }

  })
  #############################################################################################################
  # Function addNewSimilarTerms - add new similar term (by function of search similar terms from model)
  #
  #############################################################################################################
  addNewSimilarTerms<- function(df_new_similar_terms){

    # exclude empty rows
    df_new_similar_terms <-  df_new_similar_terms[!(is.na(df_new_similar_terms$word) | df_new_similar_terms$word==""), ]

    if (nrow(df_new_similar_terms)>0) {
      # prepare new dataframe for rbind command (adding to list of similar terms)   - create the same structure as df_similar_terms
      # rename column with distance
      colnames(df_new_similar_terms)[2] <-"Distance"
      # create column with flag if this similar term is lexical variant of any simclin
      df_new_similar_terms$Lexical_variant <- NA

      # change col name Word to Similar_terms
      colnames(df_new_similar_terms)[1] <-"Similar_term"
      # change type of first colm
      # factor_cols <- sapply(df_new_similar_terms, is.factor)
      # df_new_similar_terms[factor_cols] <- lapply(df_new_similar_terms[factor_cols], as.character)

      if(!is.na(df_new_similar_terms[1,'Distance'])){


        # create column with examples
        df_new_similar_terms$Examples <- ""
        # create column with lexical variant
        df_new_similar_terms$Lexical_variant <- ""
        # create column with categries
        df_new_similar_terms$Category <- getSelectedCategory(input$simclins_tree_settings)

        df_new_similar_terms <- df_new_similar_terms[,c("Similar_term","Category","Distance","By_simclins","Lexical_variant","Model","Examples")]
        #how many these similar terms were found before
        # print(paste0('0.1 - Before getDuplicatedTerms',Sys.time()))
        duplicated_similar_terms_indx <-getDuplicatedTerms(df_new_similar_terms)
        # print(paste0('0.2 - After getDuplicatedTerms',Sys.time()))
        duplicated_similar_terms_count<-sum(as.numeric(duplicated_similar_terms_indx))

        new_similar_terms_count <- nrow(df_new_similar_terms)-duplicated_similar_terms_count
        #add all closest words (with duplicated terms) to the  list of Similar terms
        df_similar_terms <<- rbind(df_similar_terms, df_new_similar_terms)

        refreshSystemEfficacyMetric ('suggestedSimilarTerms',new_similar_terms_count)
        new_similar_terms_str <- paste0(as.character(new_similar_terms_count)," similar terms found ( ",as.character(duplicated_similar_terms_count)," were deleted before by user as irrelevant or duplicate of obtained before): ",paste(df_new_similar_terms[['Similar_term']],collapse=" "))
      } else new_similar_terms_str <- "0 similar terms found."
    } else  new_similar_terms_str <- "0 similar terms found."

    new_similar_terms_str
  }

  #############################################################################################################
  # Function updateSimilarTerms - update list of similar terms after edition
  # Merge same similar terms to one string, update lexical variants column, exclude terms which are in simclins
  # or irrelevant terms list already
  #############################################################################################################
  updateSimilarTerms <- function(){
    if (nrow(df_similar_terms)>0) {

      #exclude duplicates in updated list of similar_terms
      df_similar_terms<<-df_similar_terms[ order(df_similar_terms$Distance, na.last = TRUE, decreasing = TRUE), ]
      df_similar_terms$Lexical_variant <<- ""
      df_similar_terms<<-aggregate(. ~ Similar_term+Category,data = df_similar_terms,toString)
      df_similar_terms$Lexical_variant <<- NA
      ## restore examples after aggregation
      for(i in 1:nrow(df_similar_terms)) {
        df_similar_terms[i,'Examples'] <<- paste0("<button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"lastClickSimilar_termId\",this.id);' id='next_examples_for_",df_similar_terms[i,'Similar_term'],"#0'><i class='icon-left'></i>Examples</button>")
      }
      #df_similar_terms$Category <<- getSelectedCategory(input$simclins_tree)
      df_similar_terms <<- df_similar_terms[,c("Similar_term","Category","Distance","By_simclins","Lexical_variant","Model","Examples")]

      #exclude duplicates with simclins and are in the list of irrelevant terms
      duplicated_indx <- getDuplicatedTerms(df_similar_terms,TRUE,FALSE,TRUE)
      df_similar_terms <<- df_similar_terms[!duplicated_indx,]

      # order distances for each similar_term
      if (nrow(df_similar_terms)>0)
        for(i in 1:nrow(df_similar_terms)) {
          distance_vector = unlist(strsplit(as.character(df_similar_terms[i,'Distance']), split=", "))
          simclins_vector = unlist(strsplit(as.character(df_similar_terms[i,'By_simclins']), split=", "))
          distances_of_synonim_df <- data.frame(distance=distance_vector, simclins=simclins_vector)
          distances_of_synonim_df<-distances_of_synonim_df[ order(distances_of_synonim_df$distance, na.last = TRUE, decreasing = TRUE), ]
          df_similar_terms[i,'Distance']<<-toString(distances_of_synonim_df$distance)
          df_similar_terms[i,'By_simclins']<<-toString(distances_of_synonim_df$simclins)
          rm(distances_of_synonim_df)
        }

      refreshTable('similar_terms')

    }
  }

  #############################################################################################################
  # Handler event of button click 'Find similar terms for new simclins' (tab 2. Similar explorer, table Simclins)
  # Find similar terms by the simclin
  #############################################################################################################
  observeEvent(input$findSimilar_terms_click, {
    category_str = getSelectedCategory(input$simclins_tree_settings)
    df_new_simclins=df_simclins[df_simclins$Processed==FALSE & df_simclins$Category==category_str,]

    if (nrow(df_new_simclins)>0) {

      df_similar_terms_as_result = NimbleMiner::findNewSimilarTerms(df_new_simclins, category_str, models_files, similar_terms_count=input$setting_similar_terms_count,load_model_to_memory = (input$select_model_method==1))
      refreshTable('simclins')

      if(nrow(df_similar_terms_as_result)>0){
        new_similar_terms_str <- addNewSimilarTerms(df_similar_terms_as_result)
        fl_first_view_of_search_results <<- TRUE
        updateSimilarTerms()
        fl_first_view_of_search_results <<- FALSE

      } else new_similar_terms_str = ""

      logAction(actionDataTime <- Sys.time(),userId = currentUserId,operation = 'System search similar terms for simclins',parameters = paste0("Simclins: ", paste(as.character(df_new_simclins$Simclin), collapse=", ")," from category ",category_str),valueAfter = new_similar_terms_str)

      showModal(modalDialog(title = "Similar terms search","Similar terms` search has completed!",easyClose = TRUE))

    } else {
      showModal(modalDialog(title = "Error message","There are no new simclins for search!",easyClose = TRUE))
    }
  })



  #############################################################################################################
  # Handler event of click button Example in the row of similar terms table
  # Call functions to display examples for current similar term
  #############################################################################################################
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


  #############################################################################################################
  # Function saveAsSimclins  - save selected similar terms as simclins
  #############################################################################################################
  saveAsSimclins<-function() {
    if(!is.null(input$similar_terms_table_rows_selected)){
      df_selected_similar_terms <<- df_similar_terms[input$similar_terms_table_rows_selected,]
      df_selected_similar_terms$By_simclins <- NULL
      df_selected_similar_terms$Lexical_variant <- NULL
      df_selected_similar_terms$Model <- NULL
      df_selected_similar_terms$Processed <- FALSE
      colnames(df_selected_similar_terms)[1] <-"Simclins"
      df_selected_similar_terms$Distance <- NULL

      category_str = getSelectedCategory(input$simclins_tree_settings)
      if (category_str!=""){
        fl_root <- identical(attr(category_str,'ancestry'), character(0))

        df_selected_similar_terms$Category <- category_str
        df_selected_similar_terms$Examples <- gsub('lastClickSimilarTermId', 'lastClickId', df_selected_similar_terms$Examples)
        if(nrow(df_selected_similar_terms[is.na(df_selected_similar_terms$Simclins),])>0){
          logAction (userId = currentUserId, operation = "NA simclins while selecting similar term as simclin", parameters = paste0("Selected similar term: ",paste(df_selected_similar_terms[,'Simclins'],collapse = " ")," class of selected rows:",class(input$similar_terms_table_rows_selected)," selected rows:",paste(input$similar_terms_table_rows_selected,collapse=" ")))
          df_selected_similar_terms<-df_selected_similar_terms[!is.na(df_selected_similar_terms$Simclins),]
        }
        df_simclins <<- rbind(df_simclins, df_selected_similar_terms)
        for (i in 1:nrow(df_selected_similar_terms))
          logAction (userId = currentUserId, operation = "Select similar term as simclin", parameters = paste0("Selected similar term: ",df_selected_similar_terms[i,'Simclins']))

        refreshSystemEfficacyMetric('trueSimilarTerms',nrow(df_selected_similar_terms))

        rm(df_selected_similar_terms)
        # rownames
        if(identical(class(input$similar_terms_table_rows_selected),"character"))
          df_similar_terms <<- df_similar_terms[!(rownames(df_similar_terms) %in% input$similar_terms_table_rows_selected),]
        else #row indeces
          df_similar_terms <<- df_similar_terms[-input$similar_terms_table_rows_selected,]

        refreshTable('similar_terms')
        refreshTable('simclins')

      } else {
        showModal(modalDialog(title = "Error Message",  "Please select the category for the new simclin!",easyClose = TRUE))
      }
    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected similar terms!",easyClose = TRUE))
    }
  }


  #############################################################################################################
  # Handler event of click button 'Save selected similar terms as simclins' (tab 2. Simclins explorer, table Similar terms)
  # Save selected similar terms as simclins
  #############################################################################################################
  observeEvent(input$saveAsSimclins_click, {
    saveAsSimclins()
  })
  #############################################################################################################
  # Function clearSimilarTerms - move unselected similar terms to list of irrelevant terms
  #############################################################################################################
  clearSimilarTerms<-function() {

    category_str = getSelectedCategory(input$simclins_tree_settings)
    if (category_str!=""){

      # save unselected similar_terms into the list of irrelevant terms
      if(!is.null(input$similar_terms_table_rows_selected)){
        df_unselected_similar_terms <<- df_similar_terms[-input$similar_terms_table_rows_selected,]
        df_unselected_similar_terms <<- df_unselected_similar_terms[df_unselected_similar_terms$Category==category_str,]
      }
      else
        df_unselected_similar_terms <<- df_similar_terms[df_similar_terms$Category==category_str,]

      df_unselected_similar_terms$By_simclins<<-NULL
      df_unselected_similar_terms$Lexical_variant<<-NULL
      df_unselected_similar_terms$Model<<-NULL
      df_unselected_similar_terms$Distance<<-NULL
      if (nrow(df_unselected_similar_terms)>0)
        df_unselected_similar_terms$Frequency<<-NA
      else df_unselected_similar_terms$Frequency<<-integer(0)

      deleted_similar_terms_str = paste0(as.character(nrow(df_unselected_similar_terms))," similar terms deleted: ",paste(df_unselected_similar_terms[['Similar_term']],collapse=" ")," from the category ",category_str,".")
      logAction (userId = currentUserId, operation = "Delete similar terms", valueAfter = deleted_similar_terms_str)

      df_irrelevant_terms <<- rbind(df_irrelevant_terms, df_unselected_similar_terms)
      df_irrelevant_terms<<-df_irrelevant_terms[!duplicated(df_irrelevant_terms[,c("Similar_term","Category")]),]

      write.csv(df_irrelevant_terms, file = paste0(app_dir,"irrelevant_terms.csv"))
      output$irrelevant_similar_terms_table = DT::renderDataTable({DT::datatable(df_irrelevant_terms,rownames=FALSE, escape = FALSE)})

      # clear unselected similar_terms
      if(!is.null(input$similar_terms_table_rows_selected)){
        df_similar_terms <<- df_similar_terms[!(rownames(df_similar_terms) %in% rownames(df_unselected_similar_terms)),]
      }
      else
        df_similar_terms <<- df_similar_terms[df_similar_terms$Category!=category_str,]


      refreshTable('irrelevant_terms')
      refreshTable('similar_terms')
    } else {
      showModal(modalDialog(title = "Error Message",  "Please select the category for the clear the similar terms!",easyClose = TRUE))
    }

  }
  #############################################################################################################
  # Handler event of click button '"Clear all unselected similar terms' (tab 2. Simclins explorer, table Similar terms)
  # Move unselected similar terms to list of irrelevant terms
  #############################################################################################################
  observeEvent(input$clearSimilar_terms_click, {
    clearSimilarTerms()
  })

  #############################################################################################################
  # Handler event of click button '"Clear all unselected similar terms' (tab 2. Simclins explorer, table Similar terms)
  # Move unselected similar terms to list of irrelevant terms
  #############################################################################################################
  observeEvent(input$nextSearch_click, {
    fl_next_search_in_process <<- TRUE # trick till issue https://github.com/rstudio/DT/issues/615 will be fixed
    saveAsSimclins()
    clearSimilarTerms()
    category_str = getSelectedCategory(input$simclins_tree_settings)
    df_new_simclins=df_simclins[df_simclins$Processed==FALSE & df_simclins$Category==category_str,]

    if (nrow(df_new_simclins)>0) {

      df_similar_terms_as_result = NimbleMiner::findNewSimilarTerms(df_new_simclins, category_str, models_files, similar_terms_count=input$setting_similar_terms_count,load_model_to_memory = (input$select_model_method==1))
      refreshTable('simclins')

      if(nrow(df_similar_terms_as_result)>0){
        new_similar_terms_str <- addNewSimilarTerms(df_similar_terms_as_result)
        fl_first_view_of_search_results <<- TRUE
        updateSimilarTerms()
        fl_first_view_of_search_results <<- FALSE

      } else new_similar_terms_str = ""

      logAction(actionDataTime <- Sys.time(),userId = currentUserId,operation = 'System search similar terms for simclins',parameters = paste0("Simclins: ", paste(as.character(df_new_simclins$Simclin), collapse=", ")," from category ",category_str),valueAfter = new_similar_terms_str)

      showModal(modalDialog(title = "Similar terms search","Similar terms` search has completed!",easyClose = TRUE))

    } else {
      showModal(modalDialog(title = "Error message","There are no new simclins for search!",easyClose = TRUE))
    }

    fl_next_search_in_process <<- FALSE
  })

  #############################################################################################################
  # Handler event of click button 'Select All' (tab 2. Simclins explorer, table Similar terms)
  # Select all terms in the table
  #############################################################################################################
  observeEvent(input$selectAllSimilar_terms_click, {
    fl_selectAllSimilar_terms <<- TRUE
    refreshTable('similar_terms')
  })

  #############################################################################################################
  # Handler event of click button 'Deselect All' (tab 2. Simclins explorer, table Similar terms)
  # Clear selection of all terms in the table
  #############################################################################################################
  observeEvent(input$deselectAllSimilar_terms_click, {
    fl_deselectAllSimilar_terms <<- TRUE
    refreshTable('similar_terms')
  })

  #############################################################################################################
  #  3. Assign and review labels
  #############################################################################################################


  #############################################################################################################
  #  Function updateStatisticsByLabeledData - After labeling user should generate text corpus from labeled data
  #  for machine learning. This function set initial settings for corpus generating based on results of labeling -
  #  equal number of positive and negative labelled notes
  #############################################################################################################
  updateStatisticsByLabeledData <- function(){

    positive_labeled_notes <-nrow(fread("pos_labeled_data.csv", select = 1L))
    negated_labeled_notes <-nrow(fread("pos_negated_labeled_data.csv", select = 1L))
    negative_labeled_data_notes <-nrow(fread("negative_labeled_data.csv", select = 1L))

    # set 50 - 30 - 20, by default
    value_of_positive_notes <-  positive_labeled_notes

    # sum_of_all_false_notes <- negative_labeled_data_notes+irrelevant_labeled_data_notes+negated_labeled_notes
    sum_of_all_false_notes <- negative_labeled_data_notes+negated_labeled_notes

    if((sum_of_all_false_notes)<value_of_positive_notes)
      value_of_positive_notes <- sum_of_all_false_notes
    else sum_of_all_false_notes<-value_of_positive_notes

    value_of_negative_notes <- round(sum_of_all_false_notes*0.6,0)

    if (value_of_negative_notes < negative_labeled_data_notes)
      value_of_negative_notes <- negative_labeled_data_notes

    value_of_negated_notes <- sum_of_all_false_notes - value_of_negative_notes

    if(value_of_negated_notes<negated_labeled_notes){
      value_of_negated_notes<-negated_labeled_notes
      value_of_negative_notes<-sum_of_all_false_notes-negated_labeled_notes
    }


    updateSliderInput(session, "notes_of_positive_class", value = value_of_positive_notes, min = 0, max = positive_labeled_notes)
    updateSliderInput(session, "notes_of_negated_class",  value = value_of_negated_notes, min = 0, max = negated_labeled_notes)
    updateSliderInput(session, "notes_of_negative_class", value = value_of_negative_notes, min = 0, max = negative_labeled_data_notes)

  }

  #############################################################################################################
  #  Function getCategoriesList - return vector of all categories
  #############################################################################################################
  getCategoriesList<-function(){
    filename <- paste0(app_dir,"simclins_tree.csv")
    simclins_tree_json <- readLines(filename,encoding="UTF-8")
    simclins_tree_df<-jsonlite::fromJSON(simclins_tree_json)
    return(simclins_tree_df$text)
  }


  #############################################################################################################
  #  Function assigneLabels - classification text notes from filename file by simclins list, negations list and exceptions list
  #  Save labeled data in csv files: pos_labeled_data.csv,pos_negated_labeled_data.csv,pos_negative_labeled_data.csv, pos_irrelevant_labeled_data.csv
  #  and all data are saved in labeled-data-",format(Sys.time(), "%Y-%m-%d_%H-%M"),".csv
  #  Simclins, negations and irrelevant terms are marked in text by html tags, spaces in ngrams are replaced by underscore sign.
  #  So use these files for machine learning only with preprocessing (tags and underscore removing removing)
  #############################################################################################################
  assigneLabels <- function(filename, df_simclins, categories_list, app_dir, df_negations = NULL, df_exceptions = NULL, utf8_language = FALSE){

    # create pattern for regex from simclins
    # divide pattern by substrings from 50000 chars because of the limit by length for the pattern string
    pattern_limit <- 5000
    pattern_list = c()
    pattern_str <- ""

    df_simclins_for_search <- df_simclins[!duplicated(df_simclins$Simclins) & (df_simclins$Category %in% categories_list), ]

    if(nrow(df_simclins_for_search)>0)
      for (i in 1:nrow(df_simclins_for_search)){
        df_simclins_for_search$Simclins[i]<- gsub("\\+","",df_simclins_for_search$Simclins[i])
        df_simclins_for_search$Simclins[i]<- gsub("_"," ",df_simclins_for_search$Simclins[i])
        pattern_str <- paste0(pattern_str,"|",df_simclins_for_search$Simclins[i],"")
        #if the length of current substring is limit over - create the new substring
        if(nchar(pattern_str)>=pattern_limit-50){
          pattern_str <- substr(pattern_str,2,nchar(pattern_str))
          pattern_str <- paste0("\\b(",pattern_str,")\\b")
          pattern_list <- c(pattern_list,pattern_str)
          pattern_str <- ""

        }
      }

    if (nchar(pattern_str)>0) {
      pattern_str <- substr(pattern_str,2,nchar(pattern_str))
      pattern_str <- paste0("\\b(",pattern_str,")\\b")
      pattern_list <- c(pattern_list,pattern_str)
    }

    if(utf8_language){
      pattern_list<- enc2utf8(pattern_list)
    }

    print("Reading text file...")

    df_corpus_to_label <- read.csv(filename,header = TRUE, stringsAsFactors=FALSE,comment.char = "", colClasses = "character",fileEncoding = "UTF-8")

    notes_column_index <- getColumnToUpload(df_corpus_to_label,"Note",c("character","factor"))

    if(notes_column_index==0){
      return(list("error_msg" = "The column \'Note\' or any character/factor column not found in the specified the file. Please, upload the file with column \'Note\'."))
    }

    colnames(df_corpus_to_label)[notes_column_index]<-'Note'


    if(input$unit_type_to_label==2){
      colnames(df_corpus_to_label)[notes_column_index]<-'Source_note'
      df_allNotes <- data.frame(matrix(ncol = ncol(df_corpus_to_label)+1, nrow = 0))
      colnames(df_allNotes)<-c(colnames(df_corpus_to_label),'Note')
      for(indx_note in 1:nrow(df_corpus_to_label)){
        paragraphs_of_curr_note <-unlist(tokenize_paragraphs(df_corpus_to_label[indx_note,'Source_note']))
        rows_of_curr_note <- rep(df_corpus_to_label[indx_note,], each =  length(paragraphs_of_curr_note))
        merged_rows_of_curr_note <- cbind(rows_of_curr_note,Note = paragraphs_of_curr_note)
        df_allNotes <- rbind(df_allNotes,merged_rows_of_curr_note)
      }
      info_to_user<-paste0(nrow(df_allNotes)," paragraphs from ",nrow(df_corpus_to_label)," notes")
    } else if(input$unit_type_to_label==3){
      colnames(df_corpus_to_label)[notes_column_index]<-'Source_note'
      df_allNotes <- data.frame(matrix(ncol = ncol(df_corpus_to_label)+1, nrow = 0))
      colnames(df_allNotes)<-c(colnames(df_corpus_to_label),'Note')
      for(indx_note in 1:nrow(df_corpus_to_label)){
        sentences_of_curr_note <-unlist(tokenize_sentences(df_corpus_to_label[indx_note,'Source_note']))
        rows_of_curr_note <- rep(df_corpus_to_label[indx_note,], each =  length(sentences_of_curr_note))
        merged_rows_of_curr_note <- cbind(rows_of_curr_note,Note = sentences_of_curr_note)
        df_allNotes <- rbind(df_allNotes,merged_rows_of_curr_note)
      }
      info_to_user<-paste0(nrow(df_allNotes)," sentences from ",nrow(df_corpus_to_label)," notes")
    } else {
      df_allNotes <- df_corpus_to_label
      info_to_user<-paste0(nrow(df_allNotes)," notes")
    }

    rm(df_corpus_to_label)

    # pre-processing of data for labeling
    df_allNotes$Note <- tolower(df_allNotes$Note)
    # df_allNotes$Note=gsub("[[:punct:]]", " ", df_allNotes$Note) - follow to errors in Hebrew corpus
    df_allNotes$Note = removePunctuation(df_allNotes$Note,preserve_intra_word_contractions = FALSE,preserve_intra_word_dashes = FALSE,ucp = TRUE)

    df_allNotes$NimbleMiner_ID <- seq.int(nrow(df_allNotes))

    # labeling all notes by regex with simclins
    df_allNotes$Label <- FALSE
    if(length(pattern_list)>0 & nrow(df_allNotes)>0)
      for(i in 1:length(pattern_list)){
        print(paste0("Filtering positive items from ",info_to_user," by ",i,"/",length(pattern_list)," part of simclins list from ",Sys.time()))
        start_time <- Sys.time()
        new_values <- grepl(pattern_list[i], df_allNotes$Note, ignore.case = TRUE)
        df_allNotes$Label <- ifelse(new_values == TRUE, TRUE,df_allNotes$Label)
        print(paste0('Duration: ', round((difftime(Sys.time(),start_time,units = "min")),2)))

      }

    write.csv(df_allNotes, file = paste0(app_dir,"raw_labeled_data_utf8.csv"), fileEncoding = "UTF-8")


    fileName_all_labeled_data <- paste0(app_dir,"labeled-data-",format(Sys.time(), "%Y-%m-%d_%H-%M"),".csv")
    write.csv(df_allNotes[df_allNotes[,'Label']==FALSE,], file = paste0(app_dir,"negative_labeled_data.csv"), fileEncoding = "UTF-8")
    write.csv(df_allNotes[df_allNotes[,'Label']==FALSE,], file = fileName_all_labeled_data, na = "", fileEncoding = "UTF-8")

    orignal_source_colnames <- colnames(df_allNotes)

    df_positiveLabels <- data.frame(df_allNotes[df_allNotes[,'Label']==TRUE,],stringsAsFactors=FALSE)
    if (nrow(df_positiveLabels)>0)
      df_positiveLabels$Simclins <-""
    else df_positiveLabels$Simclins <-character(0)

    total_notes <- nrow(df_allNotes)
    total_negated_notes <-0

    rm(df_allNotes)

    # get pattern for pre-negations search by categories

    pattern_pre_negations_list<-vector("list",length(categories_list)+1)
    names(pattern_pre_negations_list)<-c("General",categories_list)

    pattern_pre_negations <- ""
    df_pre_negations <-data.table(df_negations[df_negations$Type=='before',])

    # get pattern for general negations
    df_pre_negations_curr_cat <- df_pre_negations[df_pre_negations$Category=="General",]
    if(nrow(df_pre_negations_curr_cat)>0){
      list_pre_negations <- ""
      for(neg_indx in 1:nrow(df_pre_negations_curr_cat))
        list_pre_negations <- paste0(list_pre_negations,"|",df_pre_negations_curr_cat[neg_indx,"Negation"])
      if(nchar(list_pre_negations)>0) {
        list_pre_negations <- substring(list_pre_negations,2,nchar(list_pre_negations))
        pattern_pre_negations <- gsub("_"," ",list_pre_negations)
        if(utf8_language) pattern_pre_negations<-enc2utf8(pattern_pre_negations)
        pattern_pre_negations <- paste0("(\\b(",pattern_pre_negations,")\\b)")
      }
      pattern_pre_negations_list[['General']]<-pattern_pre_negations
    }

    # get patterns for all categories
    if(length(categories_list)>0)
      for(i in 1:length(categories_list)){
        pattern_pre_negations <- ""
        list_pre_negations <- ""
        df_pre_negations_curr_cat <- df_pre_negations[df_pre_negations$Category==categories_list[i],]

        if(nrow(df_pre_negations_curr_cat)>0){
          for(neg_indx in 1:nrow(df_pre_negations_curr_cat))
            list_pre_negations <- paste0(list_pre_negations,"|",df_pre_negations_curr_cat[neg_indx,"Negation"])
          # pattern_pre_negations <- paste0("(\\b(",list_pre_negations,gsub("_"," ",list_pre_negations),")\\b)")
          if(nchar(list_pre_negations)>0) {
            list_pre_negations <- substring(list_pre_negations,2,nchar(list_pre_negations))
            pattern_pre_negations <- gsub("_"," ",list_pre_negations)
            if(utf8_language) pattern_pre_negations<-enc2utf8(pattern_pre_negations)
            pattern_pre_negations <- paste0("(\\b(",pattern_pre_negations,")\\b)")
          }
          pattern_pre_negations_list[[categories_list[i]]] <- pattern_pre_negations
        }
      }
    rm(df_pre_negations)

    pattern_post_negations <- ""
    df_post_negations <-data.table(df_negations[df_negations$Type=='after',])

    pattern_post_negations_list<-vector("list",length(categories_list)+1)
    names(pattern_post_negations_list)<-c("General",categories_list)

    # get pattern for general negations
    df_post_negations_curr_cat <- df_post_negations[df_post_negations$Category=="General",]
    if(nrow(df_post_negations_curr_cat)>0){
      list_post_negations <- ""
      for(neg_indx in 1:nrow(df_post_negations_curr_cat))
        list_post_negations <- paste0(list_post_negations,"|",df_post_negations_curr_cat[neg_indx,"Negation"])
      if(nchar(list_post_negations)>0) {
        list_post_negations <- substring(list_post_negations,2,nchar(list_post_negations))
        pattern_post_negations <- paste0(list_post_negations,"|",gsub("_"," ",list_post_negations))
        if(utf8_language) pattern_post_negations<-enc2utf8(pattern_post_negations)
        pattern_post_negations <- paste0("(\\b(",pattern_post_negations,")\\b)")
      }
      pattern_post_negations_list[['General']]<-pattern_post_negations
    }

    # get patterns for specific negations for categories
    if(length(categories_list)>0)
      for(i in 1:length(categories_list)){
        pattern_post_negations <- ""
        list_post_negations <- ""
        df_post_negations_curr_cat <- df_post_negations[df_post_negations$Category==categories_list[i],]

        if(nrow(df_post_negations_curr_cat)>0){
          for(neg_indx in 1:nrow(df_post_negations_curr_cat))
            list_post_negations <- paste0(list_post_negations,"|",df_post_negations_curr_cat[neg_indx,"Negation"])
          # pattern_post_negations <- paste0("(\\b(",list_post_negations,gsub("_"," ",list_post_negations),")\\b)")
          if(nchar(list_post_negations)>0) {
            list_post_negations <- substring(list_post_negations,2,nchar(list_post_negations))
            pattern_post_negations <- paste0(list_post_negations,"|",gsub("_"," ",list_post_negations))
            if(utf8_language) pattern_post_negations<-enc2utf8(pattern_post_negations)
            pattern_post_negations <- paste0("(\\b(",pattern_post_negations,')\\b)')
          }
          pattern_post_negations_list[[categories_list[i]]] <- pattern_post_negations
        }
      }
    rm(df_post_negations)


    df_false_positiveLabels <- vector(mode = "logical", length = 0)
    df_irrelevant_positiveLabels <- vector(mode = "logical", length = 0)
    categories_cols_names <-"Simclins"

    if(nrow(df_positiveLabels)>0){

      if(length(categories_list)>0)
        for (cat_indx in 1:length(categories_list)){
          category_column_simclins_name <- paste0(categories_list[cat_indx],' (simclins)')
          category_column_simclins_count_name <- paste0(categories_list[cat_indx],' (# of simclins)')
          df_positiveLabels[,category_column_simclins_name]=""
          df_positiveLabels[,category_column_simclins_count_name]=0
          categories_cols_names <- paste0(categories_cols_names,",",category_column_simclins_name,",",category_column_simclins_count_name)
        }


      if (nrow(df_positiveLabels)>1)
        pb <- txtProgressBar(1,nrow(df_positiveLabels), style = 3)

      for(i in 1:nrow(df_positiveLabels)){

        curr_note = df_positiveLabels[i,"Note"]

        if(utf8_language) curr_note <- enc2utf8(curr_note)

        noteOfLabel = FALSE

        all_simclins_of_note <-c()
        curr_note_negations <- c()
        curr_note_irrelevant_terms <- c()

        tags_simclins <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(tags_simclins)<-c('word','start','end')
        tags_negated_simclins <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(tags_negated_simclins)<-c('word','start','end')
        tags_irrelevant_simclins <- data.frame(matrix(ncol = 3, nrow = 0))
        colnames(tags_negated_simclins)<-c('word','start','end')

        #check all simclins for negation
        #get simclins
        for(pattern_list_indx in 1:length(pattern_list)){

          list_simclins = stri_locate_all(curr_note, regex = pattern_list[pattern_list_indx], opts_regex=stri_opts_regex(case_insensitive=TRUE))
          prev_pos_end = 0

          #search simclins in note
          for (j in 1:nrow(list_simclins[[1]])){

            pos_start = list_simclins[[1]][j,'start']
            pos_end   = list_simclins[[1]][j,'end']

            if(!is.na(pos_start) & !is.na(pos_end)) {

              isSimclinIrrelevant = FALSE
              isSimclinNegated = FALSE

              curr_simclin = substring(curr_note,pos_start,pos_end)

              curr_simclin_categories <- df_simclins_for_search[df_simclins_for_search$Simclin==stri_trans_tolower(curr_simclin),'Category']

              #simclin` relevance check

              #there are irrelevant expressions for current simclin

              df_irrelevant_terms_of_simclin_cat <- df_irrelevant_terms[df_irrelevant_terms$Category %in% curr_simclin_categories,'Similar_term']

              if(length(grep(curr_simclin,df_irrelevant_terms_of_simclin_cat,ignore.case = TRUE))!=0){
                df_irrelevant_simclins <-df_irrelevant_terms_of_simclin_cat[grepl(curr_simclin,df_irrelevant_terms_of_simclin_cat,ignore.case = TRUE)]

                pattern_irrelevant_terms = paste(df_irrelevant_simclins, collapse = '|', sep="")
                pattern_irrelevant_terms = paste(pattern_irrelevant_terms,"|",gsub("_"," ",pattern_irrelevant_terms), sep="")

                if(utf8_language) pattern_irrelevant_terms <- enc2utf8(pattern_irrelevant_terms)
                irr_expressions_in_note = stri_locate_all(curr_note, regex =  paste("(",pattern_irrelevant_terms,")", sep=""), opts_regex=stri_opts_regex(case_insensitive=TRUE))

                #check every irrelevant expression for current simclin
                for (i_irr_expr in 1:nrow(irr_expressions_in_note[[1]])){
                  start_pos_expression_in_note = irr_expressions_in_note[[1]][i_irr_expr,'start']
                  end_pos_expression_in_note = irr_expressions_in_note[[1]][i_irr_expr,'end']
                  if(!is.na(start_pos_expression_in_note)){
                    curr_irrelevant_expression = substring(curr_note,start_pos_expression_in_note,end_pos_expression_in_note)
                    pos_simclin_in_expression = grep(curr_simclin,curr_irrelevant_expression,ignore.case = TRUE)
                    #is it current simclin? pos_start - pos of current simclin in note
                    if(length(pos_simclin_in_expression)>0)
                      for(i_simclin in 1:length(pos_simclin_in_expression)){
                        if(pos_start>=start_pos_expression_in_note+pos_simclin_in_expression[i_simclin]-1)
                          isSimclinIrrelevant = TRUE
                      }
                  }
                }
              }

              # Negations check (if the simclin is relevant)
              if(!isSimclinIrrelevant){
                pattern_pre_negations <- pattern_pre_negations_list[['General']]
                if(length(curr_simclin_categories)>0)
                  for (cat_indx in 1:length(curr_simclin_categories)){
                    pattern_pre_negations <- paste(pattern_pre_negations,pattern_pre_negations_list[[curr_simclin_categories[cat_indx]]],sep = "")
                  }

                substring_before_simclin =  substring(curr_note,1,pos_end)
                substring_after_simclin =   substring(curr_note,pos_start)

                if(length(pattern_pre_negations)>0){

                  #get pattern for pre-negations

                  pattern_pre_negations_with_distance <- paste0(pattern_pre_negations,"\\W*",stri_dup("(\\w*)\\W*",as.character(input$distance_between_simclin_and_negation)),"(\\b(",curr_simclin,")(?!\\w))")
                  # find all negations near the current simclin
                  negations_of_curr_simclin = stri_locate_all(substring_before_simclin, regex = pattern_pre_negations_with_distance, opts_regex=stri_opts_regex(case_insensitive=TRUE))
                  pattern_current_pre_negation <- paste0(pattern_pre_negations,"(?=(\\W*",stri_dup("(\\w*)\\W*",as.character(input$distance_between_simclin_and_negation)),"(\\b(",curr_simclin,")(?!\\w))))")
                  # if there are negations - check it for exceptions (pseudo and terminations later)
                  if(nrow(negations_of_curr_simclin[[1]])>0 & !is.na(negations_of_curr_simclin[[1]][1,'start']))
                    for (i_negation in nrow(negations_of_curr_simclin[[1]]):1){
                      isNegationPseudo = FALSE

                      if(!isSimclinNegated){
                        pos_start_curr_negation = negations_of_curr_simclin[[1]][i_negation,'start']
                        pos_end_curr_negation = negations_of_curr_simclin[[1]][i_negation,'end']

                        post_start_curr_negation_in_note = pos_start_curr_negation
                        post_end_curr_negation_in_note = pos_end_curr_negation

                        if(!is.na(pos_start_curr_negation) & pos_end_curr_negation>=pos_end){

                          curr_simclin_with_negation = substring(substring_before_simclin,pos_start_curr_negation,pos_end_curr_negation)


                          curr_negation = stri_extract_first(curr_simclin_with_negation, regex = pattern_current_pre_negation, opts_regex=stri_opts_regex(case_insensitive=TRUE))
                          curr_negation <- gsub(x = curr_negation,pattern = " ",replacement = "_")

                          if (nrow(df_exceptions)>0 & length(df_exceptions[df_exceptions$Category %in% curr_simclin_categories,'Exception'])>0) {
                            # for every negation - get list of its exceptions
                            exceptions_pattern_str <-df_exceptions[grepl(curr_negation,df_exceptions[df_exceptions$Category %in% curr_simclin_categories,'Exception'],ignore.case = TRUE),'Exception']

                            # find these exceptions near the current simclin
                            exceptions_near_curr_simclin = stri_locate_all(substring_before_simclin, regex = paste("(",paste(exceptions_pattern_str, collapse = '|', sep=""),")", sep=""), opts_regex=stri_opts_regex(case_insensitive=TRUE))

                            # for every exception near simclin
                            if(nrow(exceptions_near_curr_simclin[[1]])>0)
                              for (i_exception in 1:nrow(exceptions_near_curr_simclin[[1]])){
                                pos_start_exception = exceptions_near_curr_simclin[[1]][i_exception,'start']
                                pos_end_exception = exceptions_near_curr_simclin[[1]][i_exception,'end']
                                if(!is.na(pos_start_exception)){
                                  curr_exception = substring(substring_before_simclin,pos_start_exception,pos_end_exception)
                                  pos_negation_in_exception = grep(curr_negation,curr_exception)
                                  #is it current negation?
                                  if(length(pos_negation_in_exception)>0)
                                    for(i_negation_in_exception in 1:length(pos_negation_in_exception)){
                                      if(pos_start_curr_negation>=pos_negation_in_exception[i_negation_in_exception]+pos_start_exception-1) {
                                        isNegationPseudo = TRUE
                                      }

                                    }
                                }
                              }
                          }
                          if(!isNegationPseudo) isSimclinNegated = TRUE
                        }
                      } #f(!isSimclinNegated){
                    }#for (i_negation
                } #if(nchar(pattern_pre_negations)>0)

                #if negation was not found - check for post-negations
                if(!isSimclinNegated){

                  pattern_post_negations <- pattern_post_negations_list[['General']]
                  if(length(curr_simclin_categories)>0)
                    for (cat_indx in 1:length(curr_simclin_categories)){
                      pattern_post_negations <- paste(pattern_post_negations,pattern_post_negations_list[[curr_simclin_categories[cat_indx]]],sep = "")
                    }

                  if(length(pattern_post_negations)>0){
                    #get pattern for post-negations
                    pattern_post_negations_with_distance<-paste0("(\\b(",curr_simclin,")(?!\\w))")
                    pattern_post_negations_with_distance <- paste0(pattern_post_negations_with_distance,stri_dup("\\W*(\\w*)",as.character(input$distance_between_simclin_and_negation)),"\\W*",pattern_post_negations)

                    # find all negations near the current simclin
                    negations_of_curr_simclin = stri_locate_all(substring_after_simclin, regex = pattern_post_negations_with_distance, opts_regex=stri_opts_regex(case_insensitive=TRUE))
                    if(nrow(negations_of_curr_simclin[[1]])>0 & !is.na(negations_of_curr_simclin[[1]][1,'start']))
                      for (i_negation in 1:nrow(negations_of_curr_simclin[[1]])){
                        isNegationPseudo = FALSE

                        if(!isSimclinNegated){
                          pos_start_curr_negation = negations_of_curr_simclin[[1]][i_negation,'start']
                          pos_end_curr_negation = negations_of_curr_simclin[[1]][i_negation,'end']

                          post_start_curr_negation_in_note = pos_start_curr_negation + pos_start - 1
                          post_end_curr_negation_in_note = pos_end_curr_negation + pos_start - 1

                          if(!is.na(pos_start_curr_negation)) {
                            curr_simclin_with_negation = substring(substring_after_simclin,pos_start_curr_negation,pos_end_curr_negation)


                            curr_negation = stri_extract_first(curr_simclin_with_negation, regex = pattern_post_negations, opts_regex=stri_opts_regex(case_insensitive=TRUE))
                            df_exceptions_for_current_category <- df_exceptions[df_exceptions$Category %in% curr_simclin_categories,]
                            if (nrow(df_exceptions_for_current_category)>0){
                              # for every negation - get list of its exceptions
                              exceptions_pattern_str <-df_exceptions_for_current_category[grepl(df_exceptions_for_current_category$Exception,curr_negation,ignore.case = TRUE),'Exception']
                              # find these exceptions near the current simclin
                              exceptions_near_curr_simclin = stri_locate_all(substring_after_simclin, regex = paste("(",paste(exceptions_pattern_str, collapse = '|', sep=""),")", sep=""), opts_regex=stri_opts_regex(case_insensitive=TRUE))

                              # for every exception near simclin

                              for (i_exception in 1:nrow(exceptions_near_curr_simclin[[1]])){
                                pos_start_exception = exceptions_near_curr_simclin[[1]][i_exception,'start']
                                pos_end_exception = exceptions_near_curr_simclin[[1]][i_exception,'end']

                                if(!is.na(pos_start_exception)){
                                  curr_exception = substring(substring_after_simclin,pos_start_exception,pos_end_exception)
                                  pos_negation_in_exception = grep(curr_negation,curr_exception)
                                  #is it current negation?
                                  for(i_negation_in_exception in 1:length(pos_negation_in_exception)){
                                    if(pos_start_curr_negation>=pos_negation_in_exception[i_negation_in_exception]+pos_start_exception-1)
                                      isNegationPseudo = TRUE
                                  }
                                }
                              } #for (i_exception
                            }
                            if(!isNegationPseudo)
                              isSimclinNegated = TRUE
                          }#if(!is.na(pos_start_curr_negation)) {
                        }#if(!isSimclinNegated){
                      }#for (i_negation
                  } # check post negations
                } #if(!isSimclinNegated)
              } #if(!isSimclinIrrelevant) then check negations

              if (!isSimclinNegated & !isSimclinIrrelevant) {
                noteOfLabel = TRUE
                curr_simclin <- gsub(x = curr_simclin,pattern = " ",replacement = "_")
                tags_simclins<-rbind(tags_simclins,data.frame("word" = curr_simclin, "start" = pos_start, "end" = pos_end+1))
                if(!(curr_simclin %in% all_simclins_of_note)){
                  all_simclins_of_note <- c(all_simclins_of_note,curr_simclin)
                  if(length(curr_simclin_categories)>0) {
                    df_positiveLabels[i,paste0(curr_simclin_categories[1],' (simclins)')] <- paste(c(unlist(strsplit(df_positiveLabels[i,paste0(curr_simclin_categories[1],' (simclins)')],split = ", ")),curr_simclin),collapse = ", ")
                    df_positiveLabels[i,paste0(curr_simclin_categories[1],' (# of simclins)')] <- (as.integer(df_positiveLabels[i,paste0(curr_simclin_categories[1],' (# of simclins)')]))+1

                  }
                }

              } else {
                if(isSimclinNegated){
                  curr_simclin_with_negation<-sub("\\(","\\\\(",curr_simclin_with_negation)
                  curr_simclin_with_negation<-sub("\\)","\\\\(",curr_simclin_with_negation)
                  curr_note_negations <- append(curr_note_negations,stri_trans_tolower(curr_negation))
                  tags_negated_simclins<-rbind(tags_negated_simclins,data.frame("word" = curr_simclin, "start" = post_start_curr_negation_in_note, "end" = post_end_curr_negation_in_note+1))
                }
                else if(isSimclinIrrelevant){
                  curr_note_irrelevant_terms<-sub("\\(","\\\\(",curr_note_irrelevant_terms)
                  curr_note_irrelevant_terms<-sub("\\)","\\\\(",curr_note_irrelevant_terms)
                  curr_note_irrelevant_terms <- append(curr_note_irrelevant_terms,stri_trans_tolower(gsub(" ","_",trimws(curr_irrelevant_expression))))
                  tags_irrelevant_simclins<-rbind(tags_irrelevant_simclins,data.frame("word" = curr_simclin, "start" = start_pos_expression_in_note, "end" = end_pos_expression_in_note+1))
                }
              }

            } #if(!is.na(pos_start)&!is.na(pos_end))
            prev_pos_end = pos_end
          }#loop over simclins

        }#loop over pattern list


        #check simclins if they are part of other negated and irrelevant expressions (by coordinates) or other simclins
        if(nrow(tags_simclins)>0) {
          for(tags_indx_simclins in 1:nrow(tags_simclins)){
            if(nrow(tags_negated_simclins)>0)
              for(tags_indx_negated_simclins in 1:nrow(tags_negated_simclins))
                if (as.numeric(tags_simclins[tags_indx_simclins,'start'])>=as.numeric(tags_negated_simclins[tags_indx_negated_simclins,'start'])
                    & as.numeric(tags_simclins[tags_indx_simclins,'end'])<=as.numeric(tags_negated_simclins[tags_indx_negated_simclins,'end'])){
                  tags_simclins[tags_indx_simclins,'start']<- 0
                  tags_simclins[tags_indx_simclins,'end']<- 0
                }
            if(nrow(tags_irrelevant_simclins)>0)
              for(tags_indx_irrelevant_simclins in 1:nrow(tags_irrelevant_simclins))
                if (as.numeric(tags_simclins[tags_indx_simclins,'start'])>=as.numeric(tags_irrelevant_simclins[tags_indx_irrelevant_simclins,'start'])
                    & as.numeric(tags_simclins[tags_indx_simclins,'end'])<=as.numeric(tags_irrelevant_simclins[tags_indx_irrelevant_simclins,'end'])){
                  tags_simclins[tags_indx_simclins,'start']<- 0
                  tags_simclins[tags_indx_simclins,'end']<- 0
                }
            #remove simclins which re part of other simclins
            for(tags_indx_simclins2 in 1:nrow(tags_simclins))
              if (rownames(tags_simclins)[tags_indx_simclins]!=rownames(tags_simclins)[tags_indx_simclins2] & as.numeric(tags_simclins[tags_indx_simclins,'start'])>=as.numeric(tags_simclins[tags_indx_simclins2,'start'])
                  & as.numeric(tags_simclins[tags_indx_simclins,'end'])<=as.numeric(tags_simclins[tags_indx_simclins2,'end'])){
                tags_simclins[tags_indx_simclins,'start']<- 0
                tags_simclins[tags_indx_simclins,'end']<- 0
              }
          }
          tags_simclins <- tags_simclins[tags_simclins$start>0 & tags_simclins$end>0,]
          all_simclins_of_note <- paste(sort(unlist(lapply(unique(tags_simclins$word),as.character))),collapse = ', ')
        }
        curr_note_negations <- paste(sort(unique(curr_note_negations)),collapse = ', ')
        curr_note_irrelevant_terms <- paste(sort(unique(curr_note_irrelevant_terms)),collapse = ', ')

        #join all tags with their positions
        tags_all <- data.frame(matrix(ncol = 2, nrow = 0))
        colnames(tags_all) <- c("pos","tag")

        if(nrow(tags_simclins)>0){
          tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_simclins$start),"tag" = "<span class='true-simclin'>"))
          tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_simclins$end),"tag" = "</span>"))
        }
        if(nrow(tags_negated_simclins)>0){
          tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_negated_simclins$start),"tag" = "<span class='false-simclin'>"))
          tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_negated_simclins$end),"tag" = "</span>"))
        }
        if(nrow(tags_irrelevant_simclins)){
          tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_irrelevant_simclins$start),"tag" = "<span class='false-simclin'>"))
          tags_all <- rbind(tags_all,data.frame("pos" = as.numeric(tags_irrelevant_simclins$end),"tag" = "</span>"))
        }

        #sort tags by their positions in desc order
        tags_all <- tags_all[with(tags_all, order(pos,decreasing = TRUE)),]

        #insert tags
        if(nrow(tags_all)>0)
          for(tag_indx in (1:nrow(tags_all))){
            curr_note <- paste0(substring(curr_note,1,tags_all$pos[tag_indx]-1),tags_all$tag[tag_indx],substring(curr_note,as.numeric(tags_all$pos[tag_indx])))
          }

        #if there is at least one not negated and not irrelevant simclin - the patient's note is labeled as TRUE
        if (nrow(tags_simclins) >0 ) {

          df_irrelevant_positiveLabels = append(df_irrelevant_positiveLabels,NA)
          df_false_positiveLabels = append(df_false_positiveLabels,NA)
          df_positiveLabels[i,'Simclins'] <- all_simclins_of_note

        }
        else {
          if(isSimclinNegated){
            df_false_positiveLabels = append(df_false_positiveLabels,paste(curr_note_negations, collapse = ", "))
            df_irrelevant_positiveLabels = append(df_irrelevant_positiveLabels,NA)
            total_negated_notes<-total_negated_notes+1
          } else if(isSimclinIrrelevant){
            df_false_positiveLabels = append(df_false_positiveLabels,NA)
            df_irrelevant_positiveLabels = append(df_irrelevant_positiveLabels,paste(curr_note_irrelevant_terms, collapse = ", "))
          }

        }
        df_positiveLabels[i,"Note"]<-curr_note
        if (nrow(df_positiveLabels)>1)
          setTxtProgressBar(pb, i)
      }# loop over notes
    }

    
    df_labeled_data <- read.csv(fileName_all_labeled_data,col.names = c(orignal_source_colnames),stringsAsFactors = FALSE,fileEncoding = "UTF-8")

    df_negatedPosLabels <<- data.table(df_positiveLabels[!is.na(df_false_positiveLabels),orignal_source_colnames])
    df_negatedPosLabels$Label <<- FALSE
    df_labeled_data <- rbind(df_labeled_data,df_negatedPosLabels)
    df_negatedPosLabels$Label <<- NULL
    df_negatedPosLabels$Negation <<- df_false_positiveLabels[!is.na(df_false_positiveLabels)]
    write.csv(df_negatedPosLabels[,c("Note","Negation")], file = paste0(app_dir,"pos_negated_labeled_data.csv"),fileEncoding = "UTF-8")



    df_irrelevantPosLabels <<- data.table(df_positiveLabels[!is.na(df_irrelevant_positiveLabels),orignal_source_colnames])
    df_irrelevantPosLabels$Label <<- FALSE
    df_labeled_data <- rbind(df_labeled_data,df_irrelevantPosLabels)
    df_irrelevantPosLabels$Label <<- NULL
    df_irrelevantPosLabels$Similar_term <<- df_irrelevant_positiveLabels[!is.na(df_irrelevant_positiveLabels)]
    write.csv(df_irrelevantPosLabels[,c("Note","Similar_term")], file = paste0(app_dir,"pos_irrelevant_labeled_data.csv"),fileEncoding = "UTF-8")


    df_positiveLabels <- df_positiveLabels[is.na(df_false_positiveLabels)&is.na(df_irrelevant_positiveLabels),]
    write.csv(df_positiveLabels[ , names(df_positiveLabels) != "NimbleMiner_ID" ] , file = paste0(app_dir,"pos_labeled_data.csv"),fileEncoding = "UTF-8")

    if(nrow(df_positiveLabels)>0)
      df_positiveLabels$Label<- TRUE
    else df_positiveLabels$Label<- logical(0)
    
    df_labeled_data <- rbind(df_positiveLabels[,orignal_source_colnames],df_labeled_data, fill = TRUE)
    df_labeled_data<-df_labeled_data[with(df_labeled_data, order(NimbleMiner_ID)), ]
    df_labeled_data$NimbleMiner_ID<-NULL
    df_labeled_data$Note <- gsub("<span class='true-simclin'>", "", df_labeled_data$Note)
    df_labeled_data$Note <- gsub("<span class='false-simclin'>", "", df_labeled_data$Note)
    df_labeled_data$Note <- gsub("</span>", "", df_labeled_data$Note)
    print("Saving all labeled data...")
    write.csv(df_labeled_data, file = fileName_all_labeled_data, fileEncoding = "UTF-8", na = "")

    total_positive_notes <-nrow(df_positiveLabels)
    total_irrelevant_notes <- nrow(df_irrelevantPosLabels)
    total_negated_notes <- nrow(df_negatedPosLabels)
    total_negative_notes <- total_notes - total_positive_notes - total_negated_notes
    
    rm(df_false_positiveLabels)
    rm(df_irrelevant_positiveLabels)
    print("Lebeling is completed!")
    return (list("error_msg" = "","total_notes" = total_notes, "total_positive_notes" = total_positive_notes,"total_irrelevant_notes" = total_irrelevant_notes,"total_negated_notes" = total_negated_notes,"total_negative_notes"=total_negative_notes))
  }


  #############################################################################################################
  #  Handler event of button click 'Assign labels' (tab. 5 Assigne and review labels)
  #  Call function for assigning labeles by simclins
  #############################################################################################################

  observeEvent(input$makeLabels_click, {
    #clean memory
    rm(list=ls())
    #get list of twinclins
    inFile_ehr <- input$fileEHR_input
    if(is.null(inFile_ehr)) {
      showModal(modalDialog(title = "Error message",  paste0("Please, specify the file name!"),easyClose = TRUE))

    } else {

      startLabelingTime <- Sys.time()
      progress <- shiny::Progress$new(min = 1, max = 2)
      progress$set(message  = "Labeling in process...")


      categories_list <- getCategoriesList()

      labeling_results<-assigneLabels(inFile_ehr$datapath, df_simclins, categories_list, app_dir, df_negations, df_exceptions, utf8_language = (input$language == 2))

      if(labeling_results[['error_msg']]!=""){
        showModal(modalDialog(title = "Error message", labeling_results[['error_msg']],easyClose = TRUE))
        return()
      }

      total_notes <-labeling_results[['total_notes']]
      total_positive_notes <-labeling_results[['total_positive_notes']]
      total_irrelevant_notes <-labeling_results[['total_irrelevant_notes']]
      total_negated_notes <-labeling_results[['total_negated_notes']]
      total_negative_notes <-labeling_results[['total_negative_notes']]

      # open file with positive labeled data
      df_positiveLabels <<-  read.csv(paste0(app_dir,"pos_labeled_data.csv"),header = TRUE,  stringsAsFactors=FALSE,comment.char = "",fileEncoding = "UTF-8")
      output$posLabeledData_table <- DT::renderDataTable(df_positiveLabels[,c("Note","Simclins")],rownames=FALSE, escape = FALSE,filter = 'top',options = list(pageLength = 100,columnDefs = list(list(targets = c(0), width = "70%"))))

      # open file with negated simclins
      df_negatedPosLabels <<-  read.csv(paste0(app_dir,"pos_negated_labeled_data.csv"),header = TRUE, stringsAsFactors=FALSE,colClasses = "character",comment.char = "",fileEncoding = "UTF-8")
      output$posNegatedLabeledData_table <- DT::renderDataTable({DT::datatable(df_negatedPosLabels,rownames=FALSE, colnames = c('Note','Negation'),  escape = FALSE,filter = 'top',options = list(pageLength = 100))})

      # open file with irrelevant simclins
      df_irrelevantPosLabels <<-  read.csv(paste0(app_dir,"pos_irrelevant_labeled_data.csv"),header = TRUE, col.names = c('Note','Similar_term'), stringsAsFactors=FALSE,colClasses = c("character","character","character"),comment.char = "",fileEncoding = "UTF-8")
      output$posIrrelevantLabeledData_table <- DT::renderDataTable({DT::datatable(df_irrelevantPosLabels,colnames = c('Note','Similar term'),rownames=FALSE, escape = FALSE,filter = 'top',options = list(pageLength = 100))})

      logAction(userId = currentUserId, operation = "Labeling",parameters = "totalNotes", valueAfter=total_notes, actionDuration = round((Sys.time() - startLabelingTime),2))
      logAction(userId = currentUserId, operation = "Labeling",parameters = "totalPositiveNotes", valueAfter=total_positive_notes)
      logAction(userId = currentUserId, operation = "Labeling",parameters = "totalIrrelevantPositiveNotes", valueAfter=total_irrelevant_notes)
      logAction(userId = currentUserId, operation = "Labeling",parameters = "totalNegatedPositiveNotes", valueAfter=total_negated_notes)
      logAction(userId = currentUserId, operation = "Labeling",parameters = "totalNegativeNotes", valueAfter=total_negative_notes)

      negations_freq_table <- data.table()
      total_negations_in_note <-0

      if(nrow(df_negatedPosLabels)>0){
        used_negations_list <- strsplit(paste(df_negatedPosLabels$Negation, collapse = ", "), split=", ")
        if(length(used_negations_list)>0){
          negations_freq_table <-table(used_negations_list[[1]])
          total_negations_in_note <-sum(negations_freq_table)
        }
      }

      # statistics calculation
      if(nrow(df_negations)>0 & nrow(df_negatedPosLabels)>0)
        for (i in 1:nrow(df_negations)){
          curr_negatin_count <- ifelse(!is.na(negations_freq_table[df_negations[i,'Negation']]),as.integer(negations_freq_table[df_negations[i,'Negation']]),0)
          df_negations[i,'Frequency'] <<- round(curr_negatin_count/total_negations_in_note*100,2)
          df_negations[i,'Examples']  <<- ifelse(curr_negatin_count>0, paste0("<button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"lastClickId\",this.id);' id='negation_for_",df_negations[i,'Negation'],"'><i class='icon-left'></i>Examples</button>"),NA)
          refreshTable('negations')
        }

      irrelevant_terms_freq_table <- data.table()
      total_irrelevant_terms_in_note <-0

      if(nrow(df_irrelevantPosLabels)>0){
        used_irrelevant_terms_list <- strsplit(paste(df_irrelevantPosLabels$Similar_term, collapse = ", "), split=", ")
        if(length(used_irrelevant_terms_list)>0){
          irrelevant_terms_freq_table <-table(used_irrelevant_terms_list[[1]])
          total_irrelevant_terms_in_note <-sum(irrelevant_terms_freq_table)
        }
      }

      # statistics calculation
      if(nrow(df_irrelevant_terms)>0 & nrow(df_irrelevantPosLabels)>0){
        for (i in 1:nrow(df_irrelevant_terms)){
          curr_irrelevant_terms_count <- ifelse(!is.na(irrelevant_terms_freq_table[df_irrelevant_terms[i,'Similar_term']]),as.integer(irrelevant_terms_freq_table[df_irrelevant_terms[i,'Similar_term']]),0)
          df_irrelevant_terms[i,'Frequency'] <<- round(curr_irrelevant_terms_count/total_irrelevant_terms_in_note*100,2)
          df_irrelevant_terms[i,'Examples']  <<- ifelse(curr_irrelevant_terms_count>0, paste0("<button type='button' class='btn btn-secondary load' onclick='Shiny.onInputChange(\"lastClickId\",this.id);' id='irrelevant_term_for_",df_irrelevant_terms[i,'Similar_term'],"'><i class='icon-left'></i>Examples</button>"),NA)
        }
        refreshTable('irrelevant_terms')
      }


      loadLabelingStatistics()
      updateStatisticsByLabeledData()


      progress$set(message  = "Labeling completed", value = 2)
      progress$close()

    }
  })
  #############################################################################################################
  #  Handler event of button click 'Export to HTML' (tab 5.Assign and review labels, table Labeled data woth negations)
  #############################################################################################################
  observeEvent(input$exportToHTML_click, {
    html.head <- paste("<head>" ,
                       '<style>
                       .true-simclin{
                       font-weight: bold;
                       color: green;
                       }
                       .false-simclin{
                       font-weight: bold;
                       color: red;
                       }
                       td {
                       direction:rtl;
                       }
                       </style>',
                       "</head>",sep='\n')
    ## the html body
    html.table <- paste(print(xtable(df_negatedPosLabels),type="html",file=paste0(app_dir,"negatedPosLabels.html"),sanitize.text.function=function(x){x}),
                        collapse = "\n")

    html.body <- paste("<body>", html.table,"</body>")
    ## the html file
    write(paste(html.head,html.body,sep='\n'),paste0(app_dir,"negatedPosLabels.html"))

  })

  #############################################################################################################
  #  Log
  #############################################################################################################
  #############################################################################################################
  #  Handler event of button click 'Clear log' (tab Log)
  #############################################################################################################
  observeEvent(input$clearLog_click, {
    df_log <<- df_log[0,]
    refreshTable('log')
    updateTextInput(session,"trueSimilarTerms",label = "Number of true similar terms", value = 0)
    updateTextInput(session,"suggestedSimilarTerms",label = "Number of suggested similar terms", value = 0)
    updateTextInput(session,"systemEfficacy",label = 'System Efficacy', value = 0)
  })

  #############################################################################################################
  #  Handler event of button click 'Save log in file' (tab Log)
  #  Save current log to the file logs/log-<current data and time>.csv
  #############################################################################################################
  observeEvent(input$saveAsFile_click, {
    fileName <- paste0(app_dir,"logs/log-",format(Sys.time(), "%Y%m%d-%H%M%S"),".csv")
    write.csv(df_log, file = fileName)
    showModal(modalDialog(title = "Save log as file",  paste0("The log was saved in the file ",fileName,"."),easyClose = TRUE))
  })

  #############################################################################################################
  # Handler event of button click 'Clear all previous simclins' data' (tab Settings)
  # Clear data. For current category only - simclins, similar terms, irrelevant terms. For all categories - log,
  # results of labeling
  #############################################################################################################
  observeEvent(input$clearData_click, {

    currentCategory <- getSelectedCategory()

    df_log <<- df_log[0,]
    refreshTable('log')
    updateTextInput(session,"trueSimilarTerms",label = "Number of true similar terms", value = 0)
    updateTextInput(session,"suggestedSimilarTerms",label = "Number of suggested similar terms", value = 0)
    updateTextInput(session,"systemEfficacy",label = 'System Efficacy', value = 0)


    df_simclins <<-df_simclins[df_simclins$Category!=currentCategory,]
    refreshTable('simclins')

    df_similar_terms <<-df_similar_terms[df_similar_terms$Category!=currentCategory,]
    refreshTable('similar_terms')

    df_irrelevant_terms <<-df_irrelevant_terms[df_irrelevant_terms$Category!=currentCategory,]
    refreshTable('irrelevant_terms')

    #Clear results of labeling

    df_negatedPosLabels <<- df_negatedPosLabels[0,]
    write.csv(df_negatedPosLabels, file = paste0(app_dir,"pos_negated_labeled_data.csv"))

    df_irrelevantPosLabels <<- df_irrelevantPosLabels[0,]
    write.csv(df_irrelevantPosLabels, file = paste0(app_dir,"pos_irrelevant_labeled_data.csv"))

    df_positiveLabels <<- df_positiveLabels[0,]
    write.csv(df_positiveLabels, file = paste0(app_dir,"pos_labeled_data.csv"))

    output$posLabeledData_table <- DT::renderDataTable(df_positiveLabels[,c("Note","Simclins")],rownames=FALSE, escape = FALSE,filter = 'top',options = list(pageLength = 100,columnDefs = list(list(targets = c(0), width = "70%"))))
    output$posNegatedLabeledData_table = DT::renderDataTable({DT::datatable(df_negatedPosLabels,rownames=FALSE, escape = FALSE)})
    output$posIrrelativeLabeledData_table = DT::renderDataTable({DT::datatable(df_negatedPosLabels,colnames = c("Note","Similar term"),rownames=FALSE, escape = FALSE)})

    loadLabelingStatistics()

    showModal(modalDialog(title = "Model building",  "Previous data has been deleted successfully!",easyClose = TRUE))
  })

  #############################################################################################################
  # 4. Irrelevant similar terms
  #############################################################################################################
  #############################################################################################################
  # Function addNewIrrSimilarTerm - add new irrelevant similar terms.
  # Could be called by user enter or when simclins or similar terms deleted
  #############################################################################################################
  addNewIrrSimilarTerm <- function (irr_similar_term_str) {

    original_irr_similar_term_str = irr_similar_term_str
    # trim the new simclin
    irr_similar_term_str = enc2utf8(irr_similar_term_str)
    irr_similar_term_str = gsub("_", " ", trimws(irr_similar_term_str))
    irr_similar_term_str = removePunctuation(irr_similar_term_str,preserve_intra_word_contractions = FALSE,preserve_intra_word_dashes = FALSE,ucp = TRUE)
    irr_similar_term_str = gsub("[[:space:]]", "_", trimws(irr_similar_term_str))
    irr_similar_term_str=stri_trans_tolower(irr_similar_term_str)

    category_str <- getSelectedCategory(input$simclins_tree_settings)

    if(category_str!="") {
      if(nrow(df_irrelevant_terms)>0 && nrow(df_irrelevant_terms[ which( df_irrelevant_terms$Similar_term==irr_similar_term_str & df_irrelevant_terms$Category==category_str) , ])>0)  {
        showModal(modalDialog(title = "Error message",  paste0("The term \"",original_irr_similar_term_str,"\"  (",irr_similar_term_str,") is in this category already!"),easyClose = TRUE))
      } else {
        #add to data frame
        newterm_row=data.frame(Similar_term = irr_similar_term_str,Category = category_str,Frequency = NA, Examples = NA,stringsAsFactors=FALSE)
        df_irrelevant_terms<<-rbind(df_irrelevant_terms,newterm_row)
        refreshTable('irrelevant_terms')
        # save user action in log file
        logAction(userId = currentUserId, operation = "Add user irrelevant similar term",valueAfter=paste0(irr_similar_term_str,', to the category ',category_str,'.'))
      }
    } else {
      showModal(modalDialog(title = "Error Message",  "Please select the category for the new irrelevant term!",easyClose = TRUE))
    }
  }

  #############################################################################################################
  # Handler event of button click "Add irrelevant similar term" (tab 3. Irrelevant similar terms explorer)
  # Add irrelevant similar term by user enter
  #############################################################################################################
  observeEvent(input$addNewIrrSimilarTerm_click, {
    if (!is.na(input$newIrrSimilarTerm_input) && input$newIrrSimilarTerm_input!=""){
      addNewIrrSimilarTerm(input$newIrrSimilarTerm_input);
      #clean the input control
      updateTextInput(session,"newIrrSimilarTerm_input", value = NA)
    }
  })
  #############################################################################################################
  # Handler event of button click "Move selected similar terms to simclins' list" (tab 3. Irrelevant similar terms explorer)
  # Move selected irrelevant terms to simclins list (e.g. it was marked as irrelevant by fault)
  #############################################################################################################
  observeEvent(input$moveToSimclins_click, {

    if(!is.null(input$irrelevant_similar_terms_table_rows_selected)){

      df_selected_rows <-  data.table (df_irrelevant_terms[input$irrelevant_similar_terms_table_rows_selected,])

      for(i in 1:nrow(df_selected_rows)){
        new_simclin = as.character(df_selected_rows[i,1])
        category_str = as.character(df_selected_rows[i,2])
        if (addNewSimclin(new_simclin, FALSE,category_str)){
          # save user action in log file
          logAction(userId = currentUserId, operation = "Select simclin from irrelevant similar terms",valueAfter=new_simclin)
        }
      }

      df_irrelevant_terms <<- data.table(df_irrelevant_terms[-input$irrelevant_similar_terms_table_rows_selected,])
      refreshTable('irrelevant_terms')
      rm(df_selected_rows)

    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected similar terms!",easyClose = TRUE))
    }
  })

  #############################################################################################################
  # Handler event of button click "Delete selected irrelevant terms" (tab 3. Irrelevant similar terms explorer)
  # Remove selected irrelevant terms from system (removes these terms only for current category)
  #############################################################################################################
  observeEvent(input$deleteIrrSimilarTerm_click, {

    if(!is.null(input$irrelevant_similar_terms_table_rows_selected)){
      # save selected negations
      df_selected_rows = data.table (Similar_term=paste(df_irrelevant_terms[input$irrelevant_similar_terms_table_rows_selected,"Similar_term"],df_irrelevant_terms[input$irrelevant_similar_terms_table_rows_selected,"Category"]))
      deleted_terms_str = paste0(as.character(nrow(df_selected_rows))," irrelevant similar terms deleted: ",paste(df_selected_rows[['Similar_term']],collapse=", "))
      logAction (userId = currentUserId, operation = "Delete irrelevant similar terms", valueAfter = deleted_terms_str)

      df_irrelevant_terms <<- data.table(df_irrelevant_terms[-input$irrelevant_similar_terms_table_rows_selected,])
      refreshTable('irrelevant_terms')
      rm(df_selected_rows)
    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected terms to delete!",easyClose = TRUE))
    }

  })
  #############################################################################################################
  # 4. Negations explorer
  #############################################################################################################
  #############################################################################################################
  # Function addNewNegation - add new negation negation_str with type (before or after) to category (general or for current category only)
  #############################################################################################################
  addNewNegation <- function (negation_str,negation_type,negationCategory = "General") {

    # trim the new negation
    negation_str = trimws(negation_str)



    #check for duplicates

    if(nrow(df_negations)>0 && nrow(df_negations[ which( df_negations$Negation==negation_str & df_negations$Type==negation_type & df_negations$Category==negationCategory) , ])>0)  {
      showModal(modalDialog(title = "Error message",  paste0("The negation \"",negation_str,"\" is in the list already!"),easyClose = TRUE))
    } else {
      #add negation to data frame
      newnegation_row=data.frame(Negation = negation_str,Type = negation_type,Category = negationCategory, Frequency = 0, Examples = NA, stringsAsFactors=FALSE)
      df_negations<<-rbind(df_negations,newnegation_row)
      refreshTable('negations')
      # save user action in log file
      logAction(userId = currentUserId, operation = "Add user negation",valueAfter=paste0(paste0(negationCategory,": ",negation_str," (",negation_type,")")))
    }

  }
  #############################################################################################################
  # Handler event of button click "Add negation(s)" (tab 4. Negations explorer)
  # Add irrelevant similar term by user enter
  #############################################################################################################
  observeEvent(input$addNewNegation_click, {

    if(input$negationCategory=="General")
      negationCategory = "General"
    else negationCategory = getSelectedCategory()

    if (!is.na(input$newPrepNegation_input) && input$newPrepNegation_input!=""){
      addNewNegation(input$newPrepNegation_input,'before',negationCategory);
      #clean the input control
      updateTextInput(session,"newPrepNegation_input", value = NA)
    }
    if (!is.na(input$newPostNegation_input) && input$newPostNegation_input!="") {
      addNewNegation(input$newPostNegation_input,'after',negationCategory);
      #clean the input control
      updateTextInput(session,"newPostNegation_input", value = NA)
    }
  })
  #############################################################################################################
  # Handler event of button click "Delete selected negations" (tab 4. Negations explorer, table ''General)
  # Remove selected negations from system
  #############################################################################################################
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

  #############################################################################################################
  # Handler event of button click "Delete selected negations" (tab 4. Negations explorer, table 'For current category')
  # Remove selected negations of current category from system
  #############################################################################################################
  observeEvent(input$deleteCurrCategoryNegation_click, {

    if(!is.null(input$curr_category_negations_table_rows_selected)){
      # save selected negations
      df_selected_rows = data.table (Negation=df_negations[input$curr_category_negations_table_rows_selected,"Negation"])
      deleted_negations_str = paste0(as.character(nrow(df_selected_rows))," negations deleted: ",paste(df_selected_rows[['Negation']],collapse=" "))
      logAction (userId = currentUserId, operation = "Delete negations", valueAfter = deleted_negations_str)

      df_negations <<- data.table(df_negations[-input$curr_category_negations_table_rows_selected,])
      refreshTable('negations')
      rm(df_selected_rows)
    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected negations to delete!",easyClose = TRUE))
    }

  })

  #############################################################################################################
  # Exceptions from negations
  #############################################################################################################
  #############################################################################################################
  # Function addNewException - add new exception exception_str to category (general or for current category only)
  #############################################################################################################
  addNewException <- function (exception_str, exceptionCategory = "General") {

    # trim the new exception
    exception_str = trimws(exception_str)

    #check for duplicates
    if(nrow(df_exceptions)>0 && nrow(df_exceptions[ which( df_exceptions$Exception==exception_str & df_exceptions$Category==exceptionCategory) , ])>0)  {
      showModal(modalDialog(title = "Error message",  paste0("The exception \"",exception_str,"\" is in the list already!"),easyClose = TRUE))
    } else {
      #add exception to data frame
      newexception_row=data.frame(Exception = exception_str,Category=exceptionCategory, Frequency = 0, Examples = NA, stringsAsFactors=FALSE)
      df_exceptions<<-rbind(df_exceptions,newexception_row)
      refreshTable('exceptions')
      # save user action in log file
      logAction(userId = currentUserId, operation = "Add user exception",valueAfter=paste0(exceptionCategory,": ",exception_str))
    }

  }
  #############################################################################################################
  # Handler event of button click "Add new exception" (tab 4. Negations explorer, subtab 'Exceptions')
  # Add exception by user enter
  #############################################################################################################
  observeEvent(input$addNewException_click, {
    if (!is.na(input$newException_input) && input$newException_input!=""){

      if(input$exceptionCategory=="General")
        exceptionCategory = "General"
      else exceptionCategory = getSelectedCategory()

      addNewException(input$newException_input,exceptionCategory)
      #clean the input control
      updateTextInput(session,"newException_input", value = NA)
    }
  })

  #############################################################################################################
  # Handler event of button click "Delete selected exceptions" (tab 4. Negations explorer, subtab 'Exceptions', table 'General')
  # Remove selected general exceptions from system
  #############################################################################################################
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
  #############################################################################################################
  # Handler event of button click "Delete selected exceptions" (tab 4. Negations explorer, subtab 'Exceptions', table 'For current category')
  # Remove selected exceptions of current category from system
  #############################################################################################################
  observeEvent(input$deleteCurrCategoryException_click, {

    if(!is.null(input$curr_category_exceptions_table_rows_selected)){
      # save selected exceptions
      df_selected_rows = data.table (Exception=df_exceptions[input$curr_category_exceptions_table_rows_selected,"Exception"])
      deleted_exceptions_str = paste0(as.character(nrow(df_selected_rows))," exceptions deleted: ",paste(df_selected_rows[['Exception']],collapse=" "))
      logAction (userId = currentUserId, operation = "Delete exceptions", valueAfter = deleted_exceptions_str)

      df_exceptions <<- data.table(df_exceptions[-input$curr_category_exceptions_table_rows_selected,])
      refreshTable('exceptions')
      rm(df_selected_rows)
    } else {
      showModal(modalDialog(title = "Error message",  "There are no any selected exceptions to delete!",easyClose = TRUE))
    }

  })

  #############################################################################################################
  # Change category - edit categories tree and select current active category
  #############################################################################################################

  checkCategoryName<-function(new_category_str) {
    error_msg  = ""
    if (is.na(new_category_str) | new_category_str==""){
      error_msg  = "Please, specify the category name!"
    } else if(nchar(new_category_str)<3){
      error_msg  =  "The category name must be at least 3 characters!"
    }
    return(error_msg)
  }
  #############################################################################################################
  # Function addNewCategoryToTree - Add new node to the list structure of tree, save it to the scv-file
  # Returns name of the new added category or the existent category with this name
  #############################################################################################################
  addNewCategoryToTree <- function (categoryName, fl_child, fl_background_mode = FALSE){

    result <- ""

    new_category_str = trimws(categoryName)

    error_msg<- checkCategoryName(categoryName)
    # get title of the new category

    if (error_msg==""){
      # get current selected node of the tree
      selected_node <- get_selected(input$simclins_tree_settings,format = "classid")

      if (length(selected_node)>0) {

        #read the current tree as dataframe
        filename <- paste0(app_dir,"simclins_tree.csv")
        simclins_tree_json <- readLines(filename,encoding="UTF-8")

        simclins_tree_df<-jsonlite::fromJSON(simclins_tree_json)

        df_existed_category <- simclins_tree_df[tolower(simclins_tree_df$text)==tolower(new_category_str),]

        if(nrow(df_existed_category)>0) {

          if (!fl_background_mode)
            showModal(modalDialog(title = "Error message",  paste0("The category \"",new_category_str,"\" is in the tree already!"),easyClose = TRUE))
          else result <-df_existed_category$text

          return(result)

        } else {

          # get the text of selected node
          selected_node_text <- selected_node[[1]][1]

          # get the parent of the new node
          if(fl_child){
            new_parent_id <- simclins_tree_df[simclins_tree_df[,'text']==selected_node_text,'id']
          }
          else {
            new_parent_id <- simclins_tree_df[simclins_tree_df[,'text']==selected_node_text,'parent']
          }

          #get the id of the new node
          new_id <-  max(as.numeric(simclins_tree_df[,'id'])) +1

          # add new node to the dataframe
          new_category_row <-jsonlite::fromJSON(paste0('[{"id":"',new_id,'","text":"',new_category_str,'","parent":"',new_parent_id,'","state":{"opened":true,"selected":false}}]'))
          row.names(new_category_row)<-new_id
          row.names(new_category_row$state)<-new_id
          simclins_tree_df <- rbind(simclins_tree_df,new_category_row)

          #select the new node
          if (!fl_background_mode){
            simclins_tree_df$state['selected']$selected <- FALSE
            selected_node_id <- simclins_tree_df[simclins_tree_df[,'text']==new_category_str,'id']
            if(!identical(selected_node_id, character(0)))
              simclins_tree_df[selected_node_id,'state']<-t(c(TRUE,TRUE))
          }

          #update the category tree in the settings section

          result_list<-treedf2list(simclins_tree_df)
          if (!fl_background_mode){
            updateTree(session,"simclins_tree_settings",result_list)
          }

          #clean the input control
          if (!fl_background_mode){
            updateTextInput(session,"newCategory_input", value = NA)
          }

          # save updated tree as dataframe to csv-file
          saveCategoryTree_settings(result_list)


          result <- new_category_str
        }
      } else {
        if (!fl_background_mode)
          showModal(modalDialog(title = "Error message",  "Please, select the category of the tree!",easyClose = TRUE))
      }


    } else {
      if (!fl_background_mode)
        showModal(modalDialog(title = "Error message",  error_msg ,easyClose = TRUE))
    }

    return(result)

  }
  #############################################################################################################
  # Function renameCategoryInTree  - Rename selected category in category tree (tab 'Change category')
  # Returns new name of successfully renamed category
  #############################################################################################################
  renameCategoryInTree <- function (prev_categoryName, new_categoryName){

    result <- ""

    new_category_str = trimws(new_categoryName)

    error_msg<- checkCategoryName(new_category_str)

    if (error_msg==""){

      #read the current tree as dataframe
      filename <- paste0(app_dir,"simclins_tree.csv")
      simclins_tree_json <- readLines(filename,encoding="UTF-8")

      simclins_tree_df<-jsonlite::fromJSON(simclins_tree_json)

      df_existed_category <- simclins_tree_df[tolower(simclins_tree_df$text)==tolower(new_category_str),]

      if(nrow(df_existed_category)>0) {

        showModal(modalDialog(title = "Error message",  paste0("The category \"",new_category_str,"\" is in the tree already!"),easyClose = TRUE))
        return(result)

      } else {


        # rename node in the dataframe
        simclins_tree_df[simclins_tree_df[,'text']==prev_categoryName,'text'] = new_category_str

        #update the category tree in the settings section
        result_list<-treedf2list(simclins_tree_df)

        updateTree(session,"simclins_tree_settings",result_list)

        # save updated tree as dataframe to csv-file
        saveCategoryTree_settings(result_list)

        result <- new_category_str
      }

    } else {
      showModal(modalDialog(title = "Error message",  error_msg ,easyClose = TRUE))
    }

    return(result)

  }

  #############################################################################################################
  # Handler event of button click "Add the category" (tab Change category)
  # Add new category as sibling of active category
  #############################################################################################################
  observeEvent(input$addSiblinsCategory_click, {
    addNewCategoryToTree(input$newCategory_input,FALSE)
  })
  #############################################################################################################
  # Handler event of button click "Add the subcategory" (tab Change category)
  # Add new category as child of active category
  #############################################################################################################
  observeEvent(input$addSubCategory_click, {
    addNewCategoryToTree(input$newCategory_input,TRUE)
  })
  #############################################################################################################
  # Handler event of button click "Delete selected category" (tab Change category)
  # Delete active category with following limits:
  # - Category should be empty (without simclins).
  # - The last category cant be deleted
  # - The category with subcategories cant be deleted
  #############################################################################################################
  observeEvent(input$deleteCategory_click, {

    # get current selected node of the tree
    selected_node <- get_selected(input$simclins_tree_settings,format = "classid")

    if (length(selected_node)==0) {
      showModal(modalDialog(title = "Error message",  "Please, select the category of the tree!",easyClose = TRUE))
    } else if(nrow(df_simclins[which(df_simclins$Category ==selected_node[[1]][1]),])>0){
      showModal(modalDialog(title = "Error message",  "Please, delete simclins from the category before removing this category!",easyClose = TRUE))
    } else {
      #read the current tree as dataframe
      filename <- paste0(app_dir,"simclins_tree.csv")
      simclins_tree_json <-  readLines(filename,encoding="UTF-8")
      simclins_tree_df<-jsonlite::fromJSON(simclins_tree_json)
      if (nrow(simclins_tree_df)==1){
        showModal(modalDialog(title = "Error message",  "This is the last category in the tree and we cant delete it. Please, add the other category before removing this category!",easyClose = TRUE))
      } else {
        # get the text of selected node
        selected_node_text <- selected_node[[1]][1]
        selected_node_id <- simclins_tree_df[simclins_tree_df[,'text']==selected_node_text,'id']
        selected_node_parent_id <- simclins_tree_df[simclins_tree_df[,'text']==selected_node_text,'parent']

        if(nrow(simclins_tree_df[simclins_tree_df[,'parent']==selected_node_id,])>0) {
          showModal(modalDialog(title = "Error message",  paste0("The category \"",selected_node_text,"\" has subcategories. Please, delete its subcategories before."),easyClose = TRUE))
          return(FALSE)
        } else {

          simclins_tree_df <- simclins_tree_df[simclins_tree_df[,'text']!=selected_node_text,]
          logAction (userId = currentUserId, operation = "Delete simclins category", valueAfter = selected_node_text)

          #select the parent node
          simclins_tree_df$state['selected']$selected <- FALSE

          if(identical(selected_node_parent_id, character(0)) | selected_node_parent_id=='#'){
            simclins_tree_df[min(simclins_tree_df[,'id']),'state']<-t(c(TRUE,TRUE))
          } else {
            simclins_tree_df[selected_node_parent_id,'state']<-t(c(TRUE,TRUE))
          }

          #update the category tree in the settings section
          result_list<-treedf2list(simclins_tree_df)
          updateTree(session,"simclins_tree_settings",result_list)

          # save updated tree as dataframe to csv-file
          saveCategoryTree_settings(result_list)
          updateTree(session,"simclins_tree_settings",result_list)
        }
      }
    }
  })
  #############################################################################################################
  # Handler event of click button 'Delete selected category with simclins' (tab 'Change category').
  # Call confirmation message box
  #############################################################################################################
  observeEvent(input$deleteCategoryWithSimclins_click, {
    showModal(modalDialog(
      tagList(
        tags$p("Are you sure you want to delete this category with its simclins and subcategories?",class="ext-danger")
      ),
      title="Delete selected category with simclins and subcategories",
      footer = tagList(actionButton("confirm_deleteCategoryWithSimclins", "Yes, delete this category with its simclins and subcategories",class = "btn-danger"),
                       modalButton("Cancel")
      )
    ))

  })
  #############################################################################################################
  # Handler event of click button 'Yes, delete this category with its simclins and subcategories' in confirmation message box (tab 'Change category').
  # Call function to delete category
  #############################################################################################################
  observeEvent(input$confirm_deleteCategoryWithSimclins, {
    deleteCategory(get_selected(input$simclins_tree_settings,format = "classid"), fl_delete_simclins = TRUE, fl_delete_subcategories = TRUE)
    removeModal()
  })

  #############################################################################################################
  # Handler event of click button 'Rename selected category' (tab 'Change category').
  # Call message box
  #############################################################################################################
  observeEvent(input$renameCategory_click, {
    showModal(
      modalDialog(
        textInput("new_category_name", "Category name", value = get_selected(input$simclins_tree_settings,format = "classid")),

        if (global_error_message!="")
          div(tags$b(global_error_message, style = "color: red;")),

        footer = tagList(
          actionButton("rename_category", "Rename"),
          modalButton("Cancel")
        )
      )
    )
  })

  #############################################################################################################
  # Handler event of click button 'Rename' in confirmation message box (tab 'Change category').
  # Call function to rename category
  #############################################################################################################
  observeEvent(input$rename_category, {
    # get current selected node of the tree
    selected_node <- get_selected(input$simclins_tree_settings,format = "classid")
    # get the text of selected node
    prev_category_name <- selected_node[[1]][1]
    new_category_name <- input$new_category_name

    if(prev_category_name==trimws(new_category_name)){
      showModal(modalDialog(title = "Error message",  paste0("The new name of category is same as before!"),easyClose = TRUE))
      return()
    }

    new_category_name <- renameCategoryInTree(prev_category_name,new_category_name)

    if (new_category_name!=""){

      if(nrow(df_simclins)>0)
        df_simclins[df_simclins$Category == prev_category_name,'Category'] <<- new_category_name
      if(nrow(df_irrelevant_terms)>0)
        df_irrelevant_terms[df_irrelevant_terms$Category == prev_category_name,'Category'] <<- new_category_name
      if(nrow(df_negations)>0)
        df_negations[df_negations$Category == prev_category_name,'Category'] <<- new_category_name
      if(nrow(df_exceptions)>0)
        df_exceptions[df_exceptions$Category == prev_category_name,'Category'] <<- new_category_name
      if(nrow(df_similar_terms)>0)
        df_similar_terms[df_similar_terms$Category == prev_category_name,'Category'] <<- new_category_name

      refreshTable('simclins')
      refreshTable('irrelevant_terms')
      refreshTable('negations')
      refreshTable('exceptions')
      refreshTable('similar_terms')

      logAction (userId = currentUserId, operation = "Rename category", valueBefore = prev_category_name, valueAfter = new_category_name)

      removeModal()
    }

  })
  #############################################################################################################
  # Function deleteCategory  - Delete selected category with simclins (tab 'Change category')
  #############################################################################################################
  deleteCategory <- function(category_name,fl_delete_simclins = FALSE, fl_delete_subcategories = FALSE)
  {
    # get current selected node of the tree
    selected_node_text  <- category_name

    if (length(selected_node_text)==0) {
      showModal(modalDialog(title = "Error message",  "Please, select the category of the tree!",easyClose = TRUE))
    } else {

      #read the current tree as dataframe
      filename <- paste0(app_dir,"simclins_tree.csv")
      simclins_tree_json <-  readLines(filename,encoding="UTF-8")
      simclins_tree_df<-jsonlite::fromJSON(simclins_tree_json)
      if (nrow(simclins_tree_df)==1){
        showModal(modalDialog(title = "Error message",  "This is the last category in the tree and we cant delete it. Please, add the other category before removing this category!",easyClose = TRUE))
      } else {
        # get the text of node to delete(category name is unique in the tree)

        selected_node_id <- simclins_tree_df[simclins_tree_df[,'text']==selected_node_text,'id']
        selected_node_subcategories <- simclins_tree_df[simclins_tree_df[,'parent']==selected_node_id,]

        if(nrow(selected_node_subcategories)>0) {
          if(fl_delete_subcategories)
            while (nrow(selected_node_subcategories)>0){
              deleteCategory(selected_node_subcategories[1,'text'], fl_delete_simclins, fl_delete_subcategories)
              simclins_tree_json <-  readLines(filename,encoding="UTF-8")
              simclins_tree_df<-jsonlite::fromJSON(simclins_tree_json)
              selected_node_subcategories <- simclins_tree_df[simclins_tree_df[,'parent']==selected_node_id,]
            }
          else {
            showModal(modalDialog(title = "Error message",  paste0("The category \"",selected_node_text,"\" has subcategories. Please, delete its subcategories before."),easyClose = TRUE))
            return(FALSE)
          }
        }

        selected_node_parent_id <- simclins_tree_df[simclins_tree_df[,'text']==selected_node_text,'parent']

        simclins_in_deleted_category<-nrow(df_simclins[which(df_simclins$Category ==selected_node_text),])
        fl_with_simclins <- (simclins_in_deleted_category>0)

        if(fl_with_simclins & !fl_delete_simclins){
          showModal(modalDialog(title = "Error message",  paste0("The category \"",selected_node_text,"\" has simclins. Please, delete its simclins before."),easyClose = TRUE))
          return(FALSE)
        } else {

          simclins_tree_df <- simclins_tree_df[simclins_tree_df[,'text']!=selected_node_text,]
          logAction (userId = currentUserId, operation = "Delete simclins of deleted category", valueAfter = as.character(selected_node_text))

          #select the parent node
          simclins_tree_df$state['selected']$selected <- FALSE

          if(identical(selected_node_parent_id, character(0)) | selected_node_parent_id=='#'){
            simclins_tree_df[min(simclins_tree_df[,'id']),'state']<-t(c(TRUE,TRUE))
          } else {
            simclins_tree_df[selected_node_parent_id,'state']<-t(c(TRUE,TRUE))
          }

          #update the category tree in the settings section
          result_list<-treedf2list(simclins_tree_df)
          updateTree(session,"simclins_tree_settings",result_list)

          # save updated tree as dataframe to csv-file
          saveCategoryTree_settings(result_list)
          updateTree(session,"simclins_tree_settings",result_list)


          if (fl_with_simclins){
            df_selected_rows = data.table (Similar_term=df_simclins[which(df_simclins$Category ==selected_node_text),"Simclins"])
            deleted_simclins_str = paste0(as.character(nrow(df_selected_rows))," simclins deleted: ",paste(df_selected_rows[['Similar_term']],collapse=" ")," from the category ",selected_node_text,".")
            logAction (userId = currentUserId, operation = "Delete simclins with category", valueAfter = deleted_simclins_str)
            rm(df_selected_rows)
            df_simclins <<- df_simclins[df_simclins$Category!=selected_node_text,]

            refreshTable('simclins')
            showModal(modalDialog(title = "Information",  paste0("The category ",selected_node_text," and ",simclins_in_deleted_category," its simclins were deleted!"),easyClose = TRUE))

          }
        }


      }
    }
  }
  #############################################################################################################
  # Function saveSelectedCategory  - Save selected category to the file
  #############################################################################################################
  saveSelectedCategory <- function(selected_node_text) {

    #read the current tree as dataframe
    filename <- paste0(app_dir,"simclins_tree.csv")
    simclins_tree_json <-  readLines(filename,encoding="UTF-8")
    simclins_tree_df<-jsonlite::fromJSON(simclins_tree_json)

    selected_node_id <- simclins_tree_df[simclins_tree_df[,'text']==selected_node_text,'id']

    #select the parent node
    simclins_tree_df$state['selected']$selected <- FALSE

    if(identical(selected_node_id, character(0))){
      simclins_tree_df[min(simclins_tree_df[,'id']),'state']<-t(c(TRUE,TRUE))
    } else {
      simclins_tree_df[selected_node_id,'state']<-t(c(TRUE,TRUE))
    }

    #update the category tree in the settings section
    result_list<-treedf2list(simclins_tree_df)

    # save updated tree as dataframe to csv-file
    saveCategoryTree_settings(result_list)

  }

  #############################################################################################################
  # Handler event of selecting category in the tree
  # Update filters in simclins and similar_terms tables by selected category in the tree
  #############################################################################################################

  observe({

    #get current selected category
    newSelectedCategory <- getNewSelectedCategory(input$simclins_tree_settings)
    if (newSelectedCategory!="")   userSettings$selectedCategory <-newSelectedCategory

    saveSelectedCategory(userSettings$selectedCategory)

    # update tables - without saving the selection
    df_simclins$Category <- factor(df_simclins$Category)
    output$simclins_table = DT::renderDataTable(df_simclins,
                                                filter = list(position = 'top', clear = FALSE),
                                                options = list(order=list(0, 'asc'),pageLength = 10,columnDefs = list(list(targets = c(2,3), searchable = FALSE))
                                                               ,searchCols = list(NULL, list(search =  paste0('["',userSettings$selectedCategory,'"]')),NULL,NULL)
                                                               # ,searchCols = list(NULL, list(regex = TRUE, caseInsensitive = FALSE, search =  '^swall$',smart = FALSE),NULL,NULL)
                                                ),colnames = c("Simclins","Category","Processed","Examples"),rownames=FALSE, escape = FALSE)
    df_similar_terms$Category <- factor(df_similar_terms$Category)
    output$similar_terms_table = DT::renderDataTable(df_similar_terms,
                                                     filter = list(position = 'top', clear = FALSE),
                                                     options = list(order=list(2,'desc') ,pageLength = 100,stateSave = TRUE,columnDefs = list(list(targets = c(2,3,4,5,6), searchable = FALSE))
                                                                    ,searchCols = list(NULL, list(search =  paste0('["',userSettings$selectedCategory,'"]')),NULL,NULL,NULL,NULL,NULL))
                                                     ,callback=DT::JS('table.on("page.dt",function() {var topPos = document.getElementById("div_similar_terms").offsetTop; window.scroll(0,topPos);})')
                                                     ,colnames = c("Similar terms","Category","Distance","By simclins",'Lexical variant of',"Model","Examples"),rownames=FALSE, escape = FALSE)

    html( html = paste0("Category: ", userSettings$selectedCategory), add = FALSE, selector = "span.navbar-brand:first")
    df_irrelevant_terms$Category <- factor(df_irrelevant_terms$Category)
    output$irrelevant_similar_terms_table = DT::renderDataTable(df_irrelevant_terms,
                                                                filter = list(position = 'top', clear = FALSE),
                                                                options = list(order=list(0, 'asc'),pageLength = 10,columnDefs = list(list(targets = c(2,3), searchable = FALSE))
                                                                               ,searchCols = list(NULL, list(search = paste0('["',userSettings$selectedCategory,'"]')),NULL,NULL)
                                                                ),colnames = c("Similar_terms","Category","Frequency","Examples"),rownames=FALSE, escape = FALSE)

    output$curr_category_negations_table = DT::renderDataTable(df_negations,
                                                               filter = list(position = 'top', clear = FALSE),
                                                               options = list(order=list(3, 'desc'),pageLength = 10,columnDefs = list(list(targets = c(0,1,3,4), searchable = FALSE))
                                                                              ,searchCols = list(NULL, NULL,list(search = userSettings$selectedCategory),NULL,NULL)
                                                               ),
                                                               colnames = c("Negations","Type","Category","Frequency (%)","Examples"),rownames=FALSE, escape = FALSE)

    output$curr_category_exceptions_table  <- DT::renderDataTable(df_exceptions,
                                                                  filter = list(position = 'top', clear = FALSE),
                                                                  options = list(order=list(2,'desc'),pageLength = 10,columnDefs = list(list(targets = c(0,2,3), searchable = FALSE))
                                                                                 ,searchCols = list(NULL, list(search = userSettings$selectedCategory),NULL,NULL)
                                                                  ),
                                                                  colnames = c('Exception','Category','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)

  })

  #############################################################################################################
  # 6. Machine learning
  #############################################################################################################
  #############################################################################################################
  # Function clear_corpus - clear labeled data from marks of simclins and undescore signs from ngrams
  #############################################################################################################
  clear_corpus <- function(corpus){
    corpus$Note <- gsub("[\r\n]", "", corpus$Note)
    corpus$Note <- gsub("<span class='true-simclin'>", "", corpus$Note)
    corpus$Note <- gsub("<span class='false-simclin'>", "", corpus$Note)
    corpus$Note <- gsub("</span>", "", corpus$Note)
    corpus$Note <- gsub("_", " ", corpus$Note)
    corpus$Note <- removePunctuation(corpus$Note,preserve_intra_word_contractions = FALSE,preserve_intra_word_dashes = FALSE,ucp = TRUE)
    corpus$Note <- gsub("[[:digit:]]", "", corpus$Note)
    corpus$Note <- tolower(corpus$Note)
    corpus
  }
  #############################################################################################################
  # Handler event of button click "Generate train corpus" (tab 6. Machine learning)
  # Generate corpus from labeled data
  #############################################################################################################

  observeEvent(input$generateCorpus_click, {

    df_new_rows <-  read.csv(paste0(app_dir,"pos_labeled_data.csv"),header = TRUE, stringsAsFactors=FALSE,comment.char = "",nrows = input$notes_of_positive_class)

    if (nrow(df_new_rows)==0){
      showModal(modalDialog(title = "Information",  "There are no positive labeled data in results of previous labels assignment.",easyClose = TRUE))
    } else {
      progress <- shiny::Progress$new(min=1, max=3)

      progress$set(message  = "Train corpus generation...", detail = "Reading the labeled data..." ,value = 1)

      df_corpus <- data.frame (Note = df_new_rows$Note, Label = TRUE, stringsAsFactors=FALSE)

      rm(df_new_rows)

      df_new_rows <-  read.csv(paste0(app_dir,"negative_labeled_data.csv"),header = TRUE, stringsAsFactors=FALSE,comment.char = "",nrows = input$notes_of_negative_class)
      if (nrow(df_new_rows)>0)
        df_corpus <- rbind(df_corpus, data.frame(Note = df_new_rows$Note, Label = FALSE, stringsAsFactors=FALSE))

      df_new_rows <-  read.csv(paste0(app_dir,"pos_negated_labeled_data.csv"),header = TRUE, stringsAsFactors=FALSE,comment.char = "",nrows = input$notes_of_negated_class)
      if (nrow(df_new_rows)>0)
        df_corpus <- rbind(df_corpus, data.frame(Note = df_new_rows$Note, Label = FALSE, stringsAsFactors=FALSE))


      df_corpus <- clear_corpus(df_corpus)

      df_corpus <- df_corpus[sample(nrow(df_corpus)),]
      progress$set(detail = "Writing corpus into the file..." ,value = 2)
      write.csv(df_corpus, file = paste0(app_dir,"corpus.csv"))
      progress$close()
      rm(df_new_rows)
      rm(df_corpus)

      showModal(modalDialog(title = "Information",  "The corpus is generated successfully!!",easyClose = TRUE))
    }
  })
  #############################################################################################################
  # Function create_matrix - runtime fix small error of create_matrix from RTextTools package
  #############################################################################################################
  create_matrix<- function (textColumns, language = "english", minDocFreq = 1,
                            maxDocFreq = Inf, minWordLength = 3, maxWordLength = Inf,
                            ngramLength = 1, originalMatrix = NULL, removeNumbers = FALSE,
                            removePunctuation = TRUE, removeSparseTerms = 0, removeStopwords = TRUE,
                            stemWords = FALSE, stripWhitespace = TRUE, toLower = TRUE,
                            weighting = weightTf)
  {
    library(tau)

    stem_words <- function(x) {
      split <- strsplit(x, " ")
      return(wordStem(unlist(split), language = language))
    }

    tokenize_ngrams <- function(x, n = ngramLength) return(rownames(as.data.frame(unclass(textcnt(x, method = "string", n = n)))))

    control <- list(bounds = list(local = c(minDocFreq, maxDocFreq)),
                    language = language, tolower = toLower, removeNumbers = removeNumbers,
                    removePunctuation = removePunctuation, stopwords = removeStopwords,
                    stripWhitespace = stripWhitespace, wordLengths = c(minWordLength,
                                                                       maxWordLength), weighting = weighting)
    if (ngramLength > 1) {
      control <- append(control, list(tokenize = tokenize_ngrams),
                        after = 7)
    }
    else {
      control <- append(control, list(tokenize = scan_tokenizer),
                        after = 4)
    }
    if (stemWords == TRUE && ngramLength == 1)
      control <- append(control, list(stemming = stem_words),
                        after = 7)
    trainingColumn <- apply(as.matrix(textColumns), 1, paste,
                            collapse = " ")
    trainingColumn <- sapply(as.vector(trainingColumn, mode = "character"),
                             iconv, to = "UTF8", sub = "byte")


    corpus <- VCorpus(VectorSource(trainingColumn),readerControl = list(language = language))
    library(stopwords)
    corpus <- tm_map(corpus, removeWords, stopwords("English"))
    #corpus <- tm_map(corpus, removeWords, stopwords::stopwords("he", source = "stopwords-iso"))
    matrix <- DocumentTermMatrix(corpus, control = control)

    if (removeSparseTerms > 0)
      matrix <- removeSparseTerms(matrix, removeSparseTerms)
    if (!is.null(originalMatrix)) {
      terms <- colnames(originalMatrix[, which(!colnames(originalMatrix) %in%
                                                 colnames(matrix))])

      weight <- 0
      if (attr(weighting, "acronym") == "tf-idf")
        weight <- 1e-09
      amat <- matrix(weight, nrow = nrow(matrix), ncol = length(terms))
      colnames(amat) <- terms
      rownames(amat) <- rownames(matrix)
      fixed <- as.DocumentTermMatrix(cbind(matrix[, which(colnames(matrix) %in%
                                                            colnames(originalMatrix))], amat), weighting = weighting)
      matrix <- fixed
    }
    matrix <- matrix[, sort(colnames(matrix))]
    gc()
    return(matrix)
  }

  #############################################################################################################
  # Handler event of button click "Learn model" (tab 6. Machine learning)
  # Learm model of Machine Learning (SVM and LSTM)
  #############################################################################################################

  observeEvent(input$learnModel_click, {
    progress <- shiny::Progress$new(min=1, max=5)

    progress$set(message  = "Learning model", detail = "Reading the labeled data..." ,value = 1)

    corpus_df <-  read.csv(paste0(app_dir,"corpus.csv"),header = TRUE, stringsAsFactors=FALSE, comment.char = "", fileEncoding = "UTF-8")
    corpus_df <- clear_corpus (corpus_df)
    trainingSize <- round(nrow(corpus_df)*0.75,0)

    progress$set(detail = "Train the model..." ,value = 2)

    dir.create(file.path(app_dir, "ml"), showWarnings = FALSE)

    # train a SVM Model (RTextTools package)
    if(input$mlModel_input=="SVM"){

      progress$set(detail = paste0(nrow(corpus_df)," notes were found in the file. DTM building...") ,value = 2)

      # fix error in package
      tmpfun <- get("create_matrix", envir = asNamespace("RTextTools"))
      environment(create_matrix) <- environment(tmpfun)
      assignInNamespace("create_matrix", create_matrix, ns = "RTextTools")

      dtMatrix <- create_matrix(corpus_df$Note, toLower = FALSE, removeNumbers = TRUE,language="he", removeStopwords = FALSE, removePunctuation=TRUE, stripWhitespace=TRUE, ngramLength =1)

      #debug info - print first Note and its most frequently terms
      freqTerms <- findFreqTerms(dtMatrix)
      print(corpus_df[1,'Note'])
      firtsDoc_freqTerms <- findMostFreqTerms(dtMatrix,10L)
      print(firtsDoc_freqTerms[[1]])

      write.csv(freqTerms,file=paste0(app_dir,"ml/freqTerms_",input$mlModel_input,".csv"), fileEncoding="UTF-8")
      write.csv(firtsDoc_freqTerms[[1]],file=paste0(app_dir,"ml/firtsDoc_freqTerms_",input$mlModel_input,".csv"), fileEncoding="UTF-8")

      # Configure the training data
      progress$set(detail = paste0("Configure the training data with ",trainingSize," notes.") ,value = 3)
      container <- create_container(dtMatrix, as.numeric(as.vector(corpus_df$Label)), trainSize=1:trainingSize, testSize =(trainingSize+1):nrow(corpus_df) , virgin=FALSE)


      svm_method_parameter <- "C-classification"
      svm_cost_parameter <- 4
      svm_gamma_parameter <- 0.5
      svm_kernel_parameter <- "radial"
      parameters_of_model <- paste0("Method: ",input$mlModel_input,", Cost = ",svm_cost_parameter,", Type =",svm_method_parameter,", Kernel = ",svm_kernel_parameter)
      progress$set(detail = paste0("Training the model...") ,value = 4)
      model <- train_model(container, "SVM", kernel=svm_kernel_parameter, cost = svm_cost_parameter, method = svm_method_parameter, gamma = svm_gamma_parameter)

      progress$set(detail = "Save the model..." ,value = 5)
      save(model,file=paste0(app_dir,"ml/trainedModel_",input$mlModel_input,".Rd"))
      save(dtMatrix,file=paste0(app_dir,"ml/originalMatrix.Rd"))

      predictionData <- corpus_df[(trainingSize+1):(nrow(corpus_df)),]
      rm(corpus_df)

      progress$set(detail = "Test the model..." ,value = 6)
      predictedLabels <- classify_model(container, model)
      predictionData$Predicted_labels <- predictedLabels


      # VIEW THE RESULTS BY CREATING ANALYTICS
      analytics <- create_analytics(container, predictedLabels)

      # RESULTS WILL BE REPORTED BACK IN THE analytics VARIABLE.
      # analytics@algorithm_summary: SUMMARY OF PRECISION, RECALL, F-SCORES, AND ACCURACY SORTED BY TOPIC CODE FOR EACH ALGORITHM
      # analytics@label_summary: SUMMARY OF LABEL (e.g. TOPIC) ACCURACY
      # analytics@document_summary: RAW SUMMARY OF ALL DATA AND SCORING
      # analytics@ensemble_summary: SUMMARY OF ENSEMBLE PRECISION/COVERAGE. USES THE n VARIABLE PASSED INTO create_analytics()

      # WRITE OUT THE DATA TO A CSV
      write.csv(analytics@algorithm_summary,paste0(app_dir,"ml/SampleData_AlgorithmSummary_",input$mlModel_input,".csv"))
      write.csv(analytics@label_summary,paste0(app_dir,"ml/SampleData_LabelSummary_",input$mlModel_input,"SVM.csv"))
      write.csv(predictionData,file = paste0(app_dir,"ml/prediction_test_results_",input$mlModel_input,".csv"))

      output$algorythms_summary_table  <- DT::renderDataTable(analytics@algorithm_summary,  escape = FALSE)
      print(analytics@algorithm_summary)
      precision_results <- paste0("TRUE -  ",analytics@algorithm_summary["1","SVM_PRECISION"],", FALSE - ",analytics@algorithm_summary["0","SVM_PRECISION"])
      recall_results <- paste0("TRUE -  ",analytics@algorithm_summary["1","SVM_RECALL"],", FALSE - ",analytics@algorithm_summary["0","SVM_RECALL"])

      test_results_of_model <- paste0("Precision: ",precision_results,"; Recall: ",recall_results)
      logAction(userId = currentUserId, operation = "Training the model", parameters = parameters_of_model, valueAfter=test_results_of_model)
      showModal(modalDialog(title = "Information",  paste0("The model is ready ! ",parameters_of_model,". Test results - ",test_results_of_model,". Labeled test data were saved in the file 'ml/prediction_test_results_",input$mlModel_input,".csv'."),easyClose = TRUE))


    }
    # train a NNET Model (keras package, see as source https://tensorflow.rstudio.com/keras/articles/examples/imdb_bidirectional_lstm.html)
    else if(input$mlModel_input=="NNET"){

      # should be installed python and keras on computer before


      # Define maximum number of input features
      max_features <- 1000

      # Cut texts after this number of words
      # (among top max_features most common words)
      maxlen <- 200

      batch_size <- 32

      progress$set(detail = paste0("Configure the training data with ",trainingSize," notes.") ,value = 3)
      # Define training and test sets
      x_train <- corpus_df[1:trainingSize,'Note']
      y_train <- corpus_df[1:trainingSize,'Label']
      corpus_df_test <- corpus_df[(trainingSize+1):nrow(corpus_df),]
      x_test <- corpus_df_test$Note
      y_test <- corpus_df_test$Label

      # Output lengths of testing and training sets
      cat(length(x_train), 'train sequences\n')
      cat(length(x_test), 'test sequences\n')

      # Pad sequences
      tok <- text_tokenizer(num_words=max_features)
      fit_text_tokenizer(tok,x_train)
      sequences = texts_to_sequences(tok,x_train)
      x_train = pad_sequences(sequences,maxlen=maxlen)
      test_sequences = texts_to_sequences(tok,x_test)
      x_test <- pad_sequences(test_sequences, maxlen = maxlen)

      # Output dimensions of training and test inputs
      cat('x_train shape:', dim(x_train), '\n')
      cat('x_test shape:', dim(x_test), '\n')

      # Initialize model

      model <- keras_model_sequential()
      model %>%
        # Creates dense embedding layer; outputs 3D tensor
        # with shape (batch_size, sequence_length, output_dim)
        layer_embedding(input_dim = max_features,
                        output_dim = 128,
                        input_length = maxlen) %>%
        bidirectional(layer_lstm(units = 64)) %>%
        layer_dropout(rate = 0.5) %>%
        layer_dense(units = 1, activation = 'sigmoid')

      model %>% compile(
        loss = 'binary_crossentropy',
        optimizer = 'adam',
        metrics = c('accuracy')
      )

      # Train model
      progress$set(detail = paste0("Training the model...") ,value = 4)
      model %>% keras::fit(
        x_train, y_train,
        batch_size = batch_size,
        epochs = 10,
        validation_data = list(x_test, y_test)
      )

      # Save model
      progress$set(detail = "Save the model..." ,value = 5)
      model %>% save_model_hdf5(paste0(app_dir,"ml/trainedModel_",input$mlModel_input,".h5"))

      # Test model
      progress$set(detail = "Test the model..." ,value = 6)
      predictedLabels <- model %>% predict_classes(
        x_test,
        batch_size = batch_size,
        verbose = 1
      )

      corpus_df_test$Predicted_label <- predictedLabels

      TP<- nrow(corpus_df_test[corpus_df_test$Label==TRUE & corpus_df_test$Predicted_label==1,])
      FP<- nrow(corpus_df_test[corpus_df_test$Label==FALSE & corpus_df_test$Predicted_label==1,])
      FN<- nrow(corpus_df_test[corpus_df_test$Label==TRUE & corpus_df_test$Predicted_label==1,])

      precision_results<-round(TP / (TP+FP),2)
      recall_results<-round(TP / (TP+FN),2)

      precision_results

      test_results_of_model <- paste0("Precision: ",precision_results,"; Recall: ",recall_results)
      parameters_of_model <- "Bidirectional LSTM, 1000 features, maxlen = 100, keras package"

      logAction(userId = currentUserId, operation = "Training the model", parameters = parameters_of_model, valueAfter=test_results_of_model)
      showModal(modalDialog(title = "Information",  paste0("The model is ready ! ",parameters_of_model,". Labeled test data were saved in the file 'ml/prediction_test_results_",input$mlModel_input,".csv'."),easyClose = TRUE))
    }

    progress$close()
  })

  #############################################################################################################
  # Function predictByNNET - classification of notes by LSTM method
  #############################################################################################################
  predictByNNET <- function(notes,batch_size = 32,max_features = 1000,maxlen = 200){


    modelFileName <- paste0(app_dir,"ml/trainedModel_NNET.h5")
    progress <- shiny::Progress$new(min=1, max=2)
    progress$set(message  = "Predicting data", detail = "Loading the model..." ,value = 1)
    model <- load_model_hdf5(modelFileName)

    # Pad sequences

    tok <- text_tokenizer(num_words=max_features)
    fit_text_tokenizer(tok,notes)
    sequences = texts_to_sequences(tok,notes)
    notes = pad_sequences(sequences,maxlen=maxlen)

    progress$set(message  = "Predicting data", detail = "New data classification..." ,value = 2)

    result <- predict_classes(
      model,
      notes,
      batch_size = batch_size,
      verbose = 1
    )

    progress$close()
    result
  }


  #############################################################################################################
  # Handler event of button click "Predict" (tab 6. Machine learning, section 3. Predict labels)
  # Predict labels by Machine Learning methods (SVM and LSTM)
  #############################################################################################################
  observeEvent(input$predictNotes_click, {
    notesToPredictFile <- input$notesToPredictFile

    if (is.null(notesToPredictFile)) {
      showModal(modalDialog(title = "Error Message",  "Please specify the file with the notes to predict the label!",easyClose = TRUE))
      return()
    }

    predictionData <- read.csv(notesToPredictFile$datapath,header = TRUE, stringsAsFactors=FALSE,comment.char = "", encoding = "UTF-8")
    colnames(predictionData)<-tolower(colnames(predictionData))

    if (is.null(predictionData$note)){
      showModal(modalDialog(title = "Error Message",  "There is no column 'note' in the uploaded file.",easyClose = TRUE))
      return()
    }

    # LSTM method
    if (input$mlModelToPredict_input=="NNET"){
      predictedLabels <- predictByNNET(predictionData$note,32)
    } else {

      # SVM method
      progress <- shiny::Progress$new(min=1, max=3)
      matrixFileName <- paste0(app_dir,"ml/originalMatrix.Rd")
      modelFileName <- paste0(app_dir,"ml/trainedModel_",input$mlModelToPredict_input,".Rd")

      if((!file.exists(modelFileName))) {
        showModal(modalDialog(title = "Error Message",  paste0("The model for the method ",input$mlModelToPredict_input," is not found! Please, train the model in previous item.",easyClose = TRUE)))
        return()
      }

      if((!file.exists(matrixFileName))) {
        showModal(modalDialog(title = "Error Message",  paste0("The DTM matrix is not found! Please, train the model in previous item.",easyClose = TRUE)))
        return()
      }

      progress$set(message  = "Predicting data", detail = "Reading notes, model and DTM..." ,value = 1)
      load(modelFileName)
      load(matrixFileName)
      progress$set(message  = "Predicting data", detail = "Creating a prediction document term matrix..." ,value = 2)

      # create a prediction document term matrix
      predMatrix <- create_matrix(predictionData$note, originalMatrix=dtMatrix, toLower = FALSE, language="en", removeNumbers = TRUE, removeStopwords = FALSE,removePunctuation=TRUE, stripWhitespace=TRUE,ngramLength =1)

      pred_freqTerms <- findFreqTerms(predMatrix)
      write.csv(pred_freqTerms,file=paste0(app_dir,"ml/pred_freqTerms_",input$mlModel_input,".csv"), fileEncoding="UTF-8")

      # create the corresponding container
      predSize = length(predictionData$note)
      predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

      # predict
      progress$set(message  = "Predicting data", detail = "New data classification..." ,value = 3)
      predictedLabels <- classify_model(predictionContainer, model)

      progress$close()
    } # predict by SVM (from RTextTool package)


    predictedLebel_colName <- paste0(toupper(input$mlModelToPredict_input),"_LABEL")

    predictionData[,predictedLebel_colName] <- predictedLabels
    predictionData[,predictedLebel_colName] <- ifelse(predictionData[,predictedLebel_colName]=="0",FALSE,TRUE)


    fileDateTime <- format(Sys.time(),"%Y-%m-%d_%H-%M")
    write.csv(predictionData,file = paste0(app_dir,"ml/prediction_results_",input$mlModelToPredict_input,"_",fileDateTime,".csv"),fileEncoding = "UTF-8")

    output$predicted_results_table  <- DT::renderDataTable(predictionData,  escape = FALSE)
    logAction(userId = currentUserId, operation = "Prediction", parameters = paste0("Method: ",input$mlModelToPredict_input))
    showModal(modalDialog(title = "Information",  paste0("The prediction is completed.  Detailed results were saved in the file 'ml/prediction_results_",input$mlModelToPredict_input,"_",fileDateTime,".csv'."),easyClose = TRUE))

  })

  InitializeApp <- function(){
    options(warn = -1)

    # Size limit for files (uploading) - 3Gb
    options(shiny.maxRequestSize=3000*1024^2)

    # set current apps directory as working directory
    app_dir <<- paste0(getSrcDirectory(function(x) {x}),"/");

    #in examples - chars before and after pattern
    const_side_chars <<- 20

    #n_grams of the model
    n_grams <<- 4

    #layer_size (features number) in word2vec algorithm
    const_layer_size <<- 100

    #TODO: user authentification
    currentUserId <<- 1

    Sys.setlocale("LC_ALL", "English")

    source_txt <<- ""

    models_files <<- list.files(path = app_dir, pattern = "\\.bin|\\.rds")
    models <<- vector("list", length(models_files))

    fl_first_view_of_search_results <<- FALSE
    fl_selectAllSimilar_terms <<- FALSE
    fl_deselectAllSimilar_terms <<- FALSE
    fl_selectAllNewFeatures <<- FALSE
    fl_next_search_in_process<<- FALSE

    global_error_message <<- ""

    userSettings <<- reactiveValues(selectedCategory = getSavedSelectedCategory())
    updateTabsetPanel(session, "headerNavBar", selected = "2. Simclin explorer")

    # open log file
    df_log <<-  read.csv(paste0(app_dir,"log.csv"),header = TRUE, col.names = c('DateTime','UserId','Operation','Parameters','ValueBefore','ValueAfter','Duration'),stringsAsFactors=FALSE, fileEncoding = "UTF-8")
    output$log_table <- DT::renderDataTable({DT::datatable(df_log,options = list(order=list(0,'desc')),colnames=c('Date&time','User Id','Operation','Parameters','Value before','Value after','Duration'),rownames=FALSE, escape = FALSE)})
    loadSystemMetrics()

    # open file with irrelevant similar_terms
    df_irrelevant_terms <<-  read.csv(paste0(app_dir,"irrelevant_terms.csv"),header = TRUE, col.names = c('Similar_term','Category','Frequency','Examples'),stringsAsFactors=FALSE, fileEncoding = "UTF-8")
    output$irrelevant_similar_terms_table = DT::renderDataTable(df_irrelevant_terms,
                                                                filter = list(position = 'top', clear = FALSE),
                                                                options = list(order=list(0, 'asc'),pageLength = 10,columnDefs = list(list(targets = c(2,3), searchable = FALSE))
                                                                               ,searchCols = list(NULL,NULL,NULL,NULL)
                                                                ),colnames = c("Similar terms","Category","Frequency","Examples"),rownames=FALSE, escape = FALSE)

    # open file with negations
    df_negations <<-  read.csv(paste0(app_dir,"negations.csv"),header = TRUE, col.names = c('Negation','Type','Category','Frequency','Examples'),stringsAsFactors=FALSE, fileEncoding = "UTF-8")
    output$negations_table <- DT::renderDataTable(df_negations,
                                                  filter = list(position = 'top', clear = FALSE),
                                                  options = list(order=list(3,'desc'),pageLength = 10,columnDefs = list(list(targets = c(0,1,3,4), searchable = FALSE))
                                                                 ,searchCols = list(NULL,NULL,list(search = "General"),NULL,NULL)),
                                                  colnames = c('Negation','Type','Category','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)

    output$curr_category_negations_table <- DT::renderDataTable(df_negations,
                                                                filter = list(position = 'top', clear = FALSE),
                                                                options = list(order=list(3,'desc'),pageLength = 10,columnDefs = list(list(targets = c(0,1,3,4), searchable = FALSE))
                                                                               ,searchCols = list(NULL,NULL,list(search = userSettings$selectedCategory),NULL,NULL)),
                                                                colnames = c('Negation','Type','Category','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)

    df_exceptions <<-  read.csv(paste0(app_dir,"negations-exceptions.csv"),header = TRUE, col.names = c('Exception','Category','Frequency','Examples'),stringsAsFactors=FALSE, fileEncoding = "UTF-8")

    output$exceptions_table  <- DT::renderDataTable(df_exceptions,
                                                    filter = list(position = 'top', clear = FALSE),
                                                    options = list(order=list(2,'desc'),pageLength = 10,columnDefs = list(list(targets = c(0,2,3), searchable = FALSE))
                                                                   ,searchCols = list(NULL,list(search = "General"),NULL,NULL)),
                                                    colnames = c('Exception','Category','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)
    output$curr_category_exceptions_table  <- DT::renderDataTable(df_exceptions,
                                                                  filter = list(position = 'top', clear = FALSE),
                                                                  options = list(order=list(2,'desc'),pageLength = 10,columnDefs = list(list(targets = c(0,2,3), searchable = FALSE))
                                                                                 ,searchCols = list(NULL,userSettings$selectedCategory,NULL,NULL)),
                                                                  colnames = c('Exception','Category','Frequency (%)','Examples'),rownames=FALSE, escape = FALSE)

    # open file with positive labeled data
    df_positiveLabels <<-  read.csv(paste0(app_dir,"pos_labeled_data.csv"),header = TRUE,  stringsAsFactors=FALSE,comment.char = "",fileEncoding = "UTF-8")
    output$posLabeledData_table <- DT::renderDataTable(df_positiveLabels[,c("Note","Simclins")],rownames=FALSE, escape = FALSE,filter = 'top',options = list(pageLength = 100,columnDefs = list(list(targets = c(0), width = "70%"))))

    # open file with negated simclins
    df_negatedPosLabels <<-  read.csv(paste0(app_dir,"pos_negated_labeled_data.csv"),header = TRUE, stringsAsFactors=FALSE,colClasses = "character",comment.char = "",fileEncoding = "UTF-8")
    output$posNegatedLabeledData_table <- DT::renderDataTable({DT::datatable(df_negatedPosLabels,rownames=FALSE, colnames = c('Note','Negation'),  escape = FALSE,filter = 'top',options = list(pageLength = 100))})

    # open file with irrelevant simclins
    df_irrelevantPosLabels <<-  read.csv(paste0(app_dir,"pos_irrelevant_labeled_data.csv"),header = TRUE, col.names = c('Note','Similar_term'), stringsAsFactors=FALSE,colClasses = c("character","character","character"),comment.char = "",fileEncoding = "UTF-8")
    output$posIrrelevantLabeledData_table <- DT::renderDataTable({DT::datatable(df_irrelevantPosLabels,colnames = c('Note','Similar term'),rownames=FALSE, escape = FALSE,filter = 'top',options = list(pageLength = 100))})

    loadLabelingStatistics()

    # Uploading data from files
    # Simclins upload

    df_simclins <<-  read.csv(paste0(app_dir,"simclins.csv"),header = TRUE, col.names = c('Simclins','Category','Processed','Examples'),stringsAsFactors=FALSE, fileEncoding = "UTF-8")
    output$simclins_table <- DT::renderDataTable({DT::datatable(df_simclins,options = list(pageLength = 10,stateSave = TRUE,order = list(list(1, 'asc'))),rownames=FALSE, escape = FALSE)})

    loadCategoryTree()

    # Similar_terms uploading
    df_similar_terms <<- read.csv(paste0(app_dir,"similar_terms.csv"),header = TRUE, col.names = c('Similar_term','Category','Distance','By_simclins','Lexical_variant','Model','Examples'),stringsAsFactors=FALSE)

    output$similar_terms_table = DT::renderDataTable(df_similar_terms,filter = list(position = 'top', clear = FALSE), options = list(pageLength = 100,stateSave = TRUE,order=list(list(5,'asc'),list(3,'desc')) ,columnDefs = list(list(targets = c(2,3,4,5,6), searchable = FALSE))
                                                                                                                                     ,searchCols = list(NULL, list(search = userSettings$selectedCategory),NULL,NULL,NULL,NULL,NULL))
                                                     ,colnames = c("Similar terms","Category","Distance","By simclins","Lexical variant","Model","Examples")
                                                     ,callback=DT::JS('table.on("page.dt",function() {var topPos = document.getElementById("div_similar_terms").offsetTop; window.scroll(0,topPos);})')
                                                     ,rownames=FALSE, escape = FALSE)

    previewOfSelectedTerms <- renderText({
      # paste0(as.character(length(input$similar_terms_table_rows_selected))," selected terms: ",
      #        paste(df_similar_terms[input$similar_terms_table_rows_selected,'Similar_term'], collapse = ", "))
      paste0(paste(df_similar_terms[input$similar_terms_table_rows_selected,'Similar_term'], collapse = ", "))


    })

    output$selected_terms_label = renderText(paste0(as.character(length(input$similar_terms_table_rows_selected))," selected terms:"))
    output$selected_terms = renderText(previewOfSelectedTerms())

    updateStatisticsByLabeledData()
  }

  InitializeApp()

  }

shinyApp(ui = ui, server = server)
