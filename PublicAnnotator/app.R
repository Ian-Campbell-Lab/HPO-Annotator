library(shiny)
library(dplyr)
library(purrr)
library(stringr)
library(ontologyIndex)
library(DT)
library(shinyjs)

###Configuration
PreviouslyAnnotatedIDs <- 1 #In case loading fails
PathToStringData <- "./PhysicalExamStrings.Rdata" 
#Download the hp.obo file
PathToHPOOBOFile <- "./hp.obo"
#Replace with backup driectory
PathToBackupDirectory <-"./Backup/"

ui <- fluidPage(
  # Application title
  titlePanel("HPO Annotation"),
  useShinyjs(),
  mainPanel(
    fluidRow(tags$script(HTML("$(function(){ 
                               $(document).keyup(function(e) {
                               if (e.which == 112) {
                                 $('#AddSpan_1').click()
                               }
                               });
                               })")),
             tags$script(HTML("$(function(){ 
                               $(document).keyup(function(e) {
                               if (e.which == 113) {
                                 $('#AddSpan_2').click()
                               }
                               });
                               })")),
             tags$script(HTML("$(function(){ 
                               $(document).keyup(function(e) {
                               if (e.which == 114) {
                                 $('#AddSpan_3').click()
                               }
                               });
                               })")),
             tags$script(HTML("$(function(){ 
                               $(document).keyup(function(e) {
                               if (e.which == 115) {
                                 $('#AddSpan_4').click()
                               }
                               });
                               })")),
             tags$script(HTML("$(function(){ 
                               $(document).keyup(function(e) {
                               if (e.which == 116) {
                                 $('#AddSpan_5').click()
                               }
                               });
                               })")),
             tags$script(HTML("$(function(){ 
                               $(document).keyup(function(e) {
                               if (e.which == 117) {
                                 $('#AddSpan_6').click()
                               }
                               });
                               })")),
             tags$script(HTML("$(function(){ 
                               $(document).keyup(function(e) {
                               if (e.which == 192) {
                                 $('#Next').click()
                               }
                               });
                               })")),
             textOutput("Remaining"),
    h4(htmlOutput("CurrentString2")),
    DTOutput("SuggestedTerms"),
    br(),
    h6("Annotated String:"),
    h4(htmlOutput("AnnotatedString")),
    h4(htmlOutput("CurrentString")),
    DTOutput("Results"),
    br()),fluidRow(column(width = 4),column(width = 4),column(
    selectizeInput("NewTerm",label = "Add Term", choices = NULL, multiple = TRUE),
    actionButton("Next","Next"),
    br(),
    textOutput("LastStringID"), width = 4),
    br(),
    br(),
    DTOutput("Details"),
    br(),
    br(),
    actionButton("Backup","Backup")),
    width = 12
  )
  
)

server <- function(session, input, output) {
  hpo <- get_ontology(file = PathToHPOOBOFile, extract_tags = "everything")
  load(file = PathToStringData)
  HighlightColors = c("251,247,25","255,193,203","57,255,20","0,255,255","191,64,191","0,0,255","255,0,0")
  shinyjs::runjs("document.body.addEventListener('mouseup', () => {
  var selection = window.getSelection();
  if(selection){
    Shiny.setInputValue('textStart',selection.anchorOffset);
    Shiny.setInputValue('textEnd',selection.focusOffset);
  }
  else{
    Shiny.setInputValue('textStart',NULL);
  }
  });")
  AddHighlight = function(String,Spans,Colors){
    Spans <- unlist(Spans, recursive = FALSE)
    CloseTargets <- sort(sapply(Spans,`[`,2),decreasing = TRUE)
    OpenTargets <- sort(sapply(Spans,`[`,1),decreasing = TRUE)
    OpenTargets <- sapply(OpenTargets,\(x)x + sum(CloseTargets <= x) * 7)
    ReplacedString <- Reduce(\(x,y)paste0(substr(x,0,y),"</span>",substr(x,y+1,nchar(x))),CloseTargets,init = String)
    ReplacedString <- Reduce(\(x,y)paste0(substr(x,0,y),"<span style='background-color:rgba(%s,0.5);'>",substr(x,y+1,nchar(x))),OpenTargets,init = ReplacedString)
    do.call(sprintf,c(fmt=str_replace_all(ReplacedString,"%(?!s,)","%%"), as.list(Colors)))
  }
  updateSelectizeInput(inputId =  'NewTerm', choices = setNames(hpo$id,hpo$name), server = TRUE)
  shinyInput <- function(FUN, len, id, ...) {inputs <- character(len); for (i in seq_len(len)) {inputs[i] <- as.character(FUN(paste0(id, i), ...))};inputs}

  ###Create Reactive Container
  CurrentItem <- reactiveValues(CurrentRow = data.frame(StringID = integer(0), System = character(0), Text = character(0), ID = list(), SuggestedTerms = list(), Terms = list(), ProcessedSuggestions = logical(0)), 
                                CurrentString = vector(mode = "character"), 
                                RenderText = vector(mode = "character"),
                                CurrentSystem = vector(mode = "character"), 
                                DataFrame = data.frame(Negated = factor(character(0),levels = c("Negated"),labels = c("X")),
                                                       Term = character(0), Description = character(0),
                                                       Decision = factor(character(0)),
                                                       Reject = character(0),
                                                       Erroneous = character(0),
                                                       Negate = character(0),
                                                       Details = character(0)),
                                Results = data.frame(Negated = factor(character(0),levels = c("Negated"),labels = c("X")),
                                                     Term = character(0),
                                                     Description = character(0)) %>%
                                             mutate(Spans = list(integer(0))),
                                Error = vector(mode = "character"),
                                LastStringID = vector(mode = "integer"),
                                DetailTerm = vector(mode = "character"),
                                DetailData = data.frame(Type = factor(character(0),levels = c("Super","Sub"),labels = c("Superclass","Subclass")), Term = character(0), Description = character(0)))
  ### Next Button
  observeEvent(input$Next,({
    #Write Current to DF
    if(length(CurrentItem$CurrentString) > 0){
      PEStrings[PEStrings$StringID == CurrentItem$CurrentRow$StringID,"Terms"][[1]] <<- list(list(Terms = CurrentItem$Results$Term, Negated = CurrentItem$Results$Negated, Spans = setNames(CurrentItem$Results$Spans,CurrentItem$Results$Term)))
      PEStrings[PEStrings$StringID == CurrentItem$CurrentRow$StringID,"ProcessedSuggestions"][[1]] <<- list(list(Terms = CurrentItem$DataFrame$Term, Negated = CurrentItem$DataFrame$Negated, Decision = CurrentItem$DataFrame$Decision))
      #browser()
      save(PEStrings,file = PathToStringData)
      CurrentItem$LastStringID <- CurrentItem$CurrentRow$StringID
      CurrentItem$Results <- data.frame(Negated = factor(character(0),levels = c("Negated"),labels = c("X")), Term = character(0), Description = character(0)) %>% mutate(Spans = list(integer(0)))
      CurrentItem$DetailTerm <- character(0)
      CurrentItem$DetailData <- data.frame(Type = factor(character(0),levels = c("Super","Sub"),labels = c("Superclass","Subclass")), Term = character(0), Description = character(0))
      output$Remaining <- renderText(str_c("Remaining Strings: ",nrow(filter(isolate(PEStrings), map_lgl(ProcessedSuggestions,~is.na(.)[1])))," Current ID: ", CurrentItem$CurrentRow$StringID))
    }
    #Get Next Row
    
    CurrentItem$CurrentRow <- filter(isolate(PEStrings), map_lgl(ProcessedSuggestions,~is.na(.)[1])) %>% 
                              #filter(System != "NAILS HAIR SKIN") %>% #Skin is a big waste of time
                              filter(if(sample(c(T,F),1) & sum(StringID %in% PreviouslyAnnotatedIDs)>1){StringID %in% PreviouslyAnnotatedIDs}else{!StringID %in% PreviouslyAnnotatedIDs}) %>%
                                  sample_n(1) #Make half of new annotations those that have been previously done to test inter-rater reliability
    CurrentItem$CurrentString <- CurrentItem$CurrentRow$Text
    CurrentItem$CurrentSystem <- CurrentItem$CurrentRow$System
    output$CurrentString <- renderText(CurrentItem$CurrentString)
    output$CurrentString2 <- renderText(CurrentItem$CurrentString)
    output$CurrentSystem <- renderText(CurrentItem$CurrentSystem)
    CurrentItem$DataFrame <- map_dfr(unlist(CurrentItem$CurrentRow$SuggestedTerms), ~ c(Term = .x,
                                                                                        Description = unname(get_term_property(hpo,"name",.x[[1]])),
                                                                                        Synonyms = get_term_property(hpo,"synonym",.x[[1]]) %>% str_extract("(?<=\")[^\"]+(?=\")") %>% str_c(collapse = "; "))) %>%
      mutate(Decision = rep(factor("Error",levels = c("Accept","Reject","Error","Negate")), nrow(cur_data())),
             Accept = shinyInput(actionButton, nrow(cur_data()), 'Accept_', label = "Accept", onclick = "Shiny.onInputChange(\"select_button\",  this.id, {priority: \"event\"})"),
             Reject = shinyInput(actionButton, nrow(cur_data()), 'Reject_', label = "Reject", onclick = "Shiny.onInputChange(\"select_button\",  this.id, {priority: \"event\"})"),
             Erroneous = shinyInput(actionButton, nrow(cur_data()), 'Error_', label = "Erroneous", onclick = "Shiny.onInputChange(\"select_button\",  this.id, {priority: \"event\"})"),
             Negate = shinyInput(actionButton, nrow(cur_data()), 'Negate_', label = "Negate", onclick = "Shiny.onInputChange(\"select_button\",  this.id, {priority: \"event\"})"),
             Details = shinyInput(actionButton, nrow(cur_data()), 'Details_', label = "Details", onclick = "Shiny.onInputChange(\"select_button\",  this.id, {priority: \"event\"})")) %>%
      mutate(Negated = rep(factor(NA,levels = c("Negated"),labels = c("X")),nrow(cur_data())), .before = Term) %>%
      mutate(Superclass = factor(c(NA,1)[1+map_int(Term,~.x %in% get_ancestors(hpo,filter(cur_data(),Term != .x) %>% pull(Term)))],labels = c("🔻")), .before = Decision)
    CurrentItem$RenderText <- CurrentItem$CurrentRow$Text
  }))
  ###Manage Events from Suggestions
  observeEvent(input$select_button,({
    MyAction <- strsplit(input$select_button,split = "_")[[1]][1]
    MyID <- as.integer(strsplit(input$select_button,split = "_")[[1]][2])
    CurrentItem$Error <- CurrentItem$DataFrame$Negated[[MyID]]
    if(MyAction == "Details"){CurrentItem$DetailTerm <- CurrentItem$DataFrame$Term[[MyID]]}
    if(MyAction == "Negate"){if(is.na(CurrentItem$DataFrame$Negated[[MyID]])){CurrentItem$DataFrame$Negated[[MyID]] <- factor("Negated",levels = c("Negated"),labels = c("X"))}
      else{CurrentItem$DataFrame$Negated[[MyID]] <- NA}}
    if(MyAction != "Details"){CurrentItem$DataFrame$Decision[[MyID]] <- MyAction}
    if(sum(!CurrentItem$DataFrame$Term[CurrentItem$DataFrame$Decision %in% c("Accept","Negate")] %in% CurrentItem$Results$Term)>0){
      Good <- CurrentItem$DataFrame$Term[CurrentItem$DataFrame$Decision %in% c("Accept","Negate") & !(CurrentItem$DataFrame$Term %in% CurrentItem$Results$Term)]
      GoodNegated <- CurrentItem$DataFrame$Negated[CurrentItem$DataFrame$Decision %in% c("Accept","Negate") & !(CurrentItem$DataFrame$Term %in% CurrentItem$Results$Term)]
      NewTerms <- tibble(Negated = GoodNegated, Term = Good) %>% mutate(Description = map_chr(Term,~unname(get_term_property(hpo,"name",.x))))
      CurrentItem$Results <- bind_rows(CurrentItem$Results, NewTerms)}
    if(length(CurrentItem$DetailTerm) >0){
      CurrentItem$DetailData <- bind_rows(tibble(Type = factor("Super",levels = c("Super","Sub"),labels = c("Superclass","Subclass")),Term = get_term_property(hpo,"parents",CurrentItem$DetailTerm)) %>% mutate(Description = map_chr(Term,~get_term_property(hpo,"name",.x))),
                                          tibble(Type = factor(NA,levels = c("Super","Sub"),labels = c("Superclass","Subclass")), Term = CurrentItem$DetailTerm, Description = get_term_property(hpo,"name",CurrentItem$DetailTerm)),
                                          tibble(Type = factor("Sub",levels = c("Super","Sub"),labels = c("Superclass","Subclass")),Term = get_term_property(hpo,"children",CurrentItem$DetailTerm)) %>% mutate(Description = map_chr(Term,~get_term_property(hpo,"name",.x))))
      CurrentItem$DetailData <- CurrentItem$DetailData %>% 
        mutate(Term = map_chr(seq_along(Term), ~ as.character(actionLink(str_c("DetailsLink_",.x), label = Term[.x], onclick = str_c("Shiny.onInputChange(\"details_link\",  \"",Term[.x],"\", {priority: \"event\"})")))),
               Add = case_when(is.na(Type) ~ map_chr(seq_along(Term), ~ as.character(actionButton(str_c("AddDetailed_",.x), label = "Add", onclick = str_c("Shiny.onInputChange(\"details_add\",  this.id, {priority: \"event\"})")))),
                               TRUE ~ NA_character_))
    }
  }))
  
  ###Manage Events from Results
  observeEvent(input$select_button_2,({
    MyAction2 <- strsplit(input$select_button_2,split = "_")[[1]][1]
    MyID2 <- as.integer(strsplit(input$select_button_2,split = "_")[[1]][2])
    if(MyAction2 == "Negate"){if(is.na(CurrentItem$Results$Negated[[MyID2]])){CurrentItem$Results$Negated[[MyID2]] <- factor("Negated",levels = c("Negated"),labels = c("X"))}
      else{CurrentItem$Results$Negated[[MyID2]] <- NA}}
    if(MyAction2 == "Details"){CurrentItem$DetailTerm <- CurrentItem$Results$Term[[MyID2]]}
    if(MyAction2 == "Remove"){CurrentItem$Results <- CurrentItem$Results[-MyID2,]}
    if(MyAction2 == "AddSpan" & input$textEnd != 0 & input$textStart != input$textEnd){if(is.null(CurrentItem$Results$Spans[[MyID2]])){
                                                                                           CurrentItem$Results$Spans[[MyID2]] <- list(c(input$textStart,input$textEnd))}
                                                                                       else{CurrentItem$Results$Spans[[MyID2]][[length(CurrentItem$Results$Spans[[MyID2]]) + 1]] <- c(input$textStart,input$textEnd)}}
    if(length(CurrentItem$DetailTerm) >0){
      CurrentItem$DetailData <- bind_rows(tibble(Type = factor("Super",levels = c("Super","Sub"),labels = c("Superclass","Subclass")),Term = get_term_property(hpo,"parents",CurrentItem$DetailTerm)) %>% mutate(Description = map_chr(Term,~get_term_property(hpo,"name",.x))),
                                          tibble(Type = factor(NA,levels = c("Super","Sub"),labels = c("Superclass","Subclass")), Term = CurrentItem$DetailTerm, Description = get_term_property(hpo,"name",CurrentItem$DetailTerm)),
                                          tibble(Type = factor("Sub",levels = c("Super","Sub"),labels = c("Superclass","Subclass")),Term = get_term_property(hpo,"children",CurrentItem$DetailTerm)) %>% mutate(Description = map_chr(Term,~get_term_property(hpo,"name",.x))))
      CurrentItem$DetailData <- CurrentItem$DetailData %>% 
        mutate(Term = map_chr(seq_along(Term), ~ as.character(actionLink(str_c("DetailsLink_",.x), label = Term[.x], onclick = str_c("Shiny.onInputChange(\"details_link\",  \"",Term[.x],"\", {priority: \"event\"})")))),
               Add = case_when(is.na(Type) ~ map_chr(seq_along(Term), ~ as.character(actionButton(str_c("AddDetailed_",.x), label = "Add", onclick = str_c("Shiny.onInputChange(\"details_add\",  this.id, {priority: \"event\"})")))),
                               TRUE ~ NA_character_))
    }
    if(any(sapply(CurrentItem$Results$Spans,\(x)length(x[[1]])) > 0)){
      CurrentItem$RenderText <- AddHighlight(String = CurrentItem$CurrentRow$Text,
                                             Spans = CurrentItem$Results$Spans, 
                                             Colors = HighlightColors[rep(seq_len(nrow(CurrentItem$Results)),times = sapply(CurrentItem$Results$Spans,length))])}
    else{CurrentItem$RenderText <- CurrentItem$CurrentRow$Text}
    output$AnnotatedString <- renderText(HTML(CurrentItem$RenderText))
  }))
  
  ###Manage Events from Details Hyperlinks
  observeEvent(input$details_link,({
    CurrentItem$DetailTerm <- input$details_link
    CurrentItem$DetailData <- bind_rows(tibble(Type = factor("Super",levels = c("Super","Sub"),labels = c("Superclass","Subclass")),Term = get_term_property(hpo,"parents",CurrentItem$DetailTerm)) %>% mutate(Description = map_chr(Term,~get_term_property(hpo,"name",.x))),
                                        tibble(Type = factor(NA,levels = c("Super","Sub"),labels = c("Superclass","Subclass")), Term = CurrentItem$DetailTerm, Description = get_term_property(hpo,"name",CurrentItem$DetailTerm)),
                                        tibble(Type = factor("Sub",levels = c("Super","Sub"),labels = c("Superclass","Subclass")),Term = get_term_property(hpo,"children",CurrentItem$DetailTerm)) %>% mutate(Description = map_chr(Term,~get_term_property(hpo,"name",.x))))
    CurrentItem$DetailData <- CurrentItem$DetailData %>% 
      mutate(Term = map_chr(seq_along(Term), ~ as.character(actionLink(str_c("DetailsLink_",.x), label = Term[.x], onclick = str_c("Shiny.onInputChange(\"details_link\",  \"",Term[.x],"\", {priority: \"event\"})")))),
             Add = case_when(is.na(Type) ~ map_chr(seq_along(Term), ~ as.character(actionButton(str_c("AddDetailed_",.x), label = "Add", onclick = str_c("Shiny.onInputChange(\"details_add\",  this.id, {priority: \"event\"})")))),
                             TRUE ~ NA_character_))
    
  }))
  
  ###Add new term from Selectize
  observeEvent(input$NewTerm,({
    CurrentItem$Results <- bind_rows(CurrentItem$Results, data.frame(Negated = factor(NA,levels = c("Negated"),labels = c("X")), Term = input$NewTerm, Description = unname(get_term_property(hpo,"name",input$NewTerm))))
    updateSelectInput(inputId = "NewTerm", selected = NA)
  }))
  
  ###Add new term from details
  observeEvent(input$details_add,({
    MyAction3 <- strsplit(input$details_add,split = "_")[[1]][1]
    MyID3 <- as.integer(strsplit(input$details_add,split = "_")[[1]][2])
    AddTerm <- CurrentItem$DetailTerm
    CurrentItem$Results <- bind_rows(CurrentItem$Results, data.frame(Negated = factor(NA,levels = c("Negated"),labels = c("X")), Term = AddTerm, Description = unname(get_term_property(hpo,"name",AddTerm))))
  }))
  
  ###Manage backup
  observeEvent(input$Backup,({
    filename <- str_c(PathToBackupDirectory,"Annotation Data Backup ",str_replace_all(Sys.time(),":",""),".Rdata")
    save(PEStrings,file = filename)
  }))

  ###Outputs
  output$CurrentString <- renderText(HTML(CurrentItem$RenderText))
  output$AnnotatedString <- renderText(HTML(CurrentItem$RenderText))
  output$CurrentSystem <- renderText(CurrentItem$CurrentRow$System)
  output$Details <- renderDT(CurrentItem$DetailData, server = FALSE, escape = FALSE, selection = 'none', options = list(dom = 't',ordering = FALSE, pageLength = 20, scrollY = TRUE))
  output$SuggestedTerms <- renderDT(CurrentItem$DataFrame, server = FALSE, escape = FALSE, selection = 'none', options = list(dom = 't',ordering = FALSE))
  output$Results <- renderDT(CurrentItem$Results %>% mutate(Term = map_chr(seq_along(Term),~paste0("<span style='background-color:rgba(",HighlightColors[.x],",0.5);'>",Term[.x],"</span")),
                                                            Spans = map_chr(Spans,~paste0(sapply(.x,paste0,collapse = "-"),collapse = ";")),
                                                            `Add Span` = shinyInput(actionButton, nrow(cur_data()), 'AddSpan_', label = "Add Span", onclick = "Shiny.onInputChange(\"select_button_2\",  this.id, {priority: \"event\"})"),
                                                            Remove = shinyInput(actionButton, nrow(cur_data()), 'Remove_', label = "Remove", onclick = "Shiny.onInputChange(\"select_button_2\",  this.id, {priority: \"event\"})"),
                                                            Negate = shinyInput(actionButton, nrow(cur_data()), 'Negate_', label = "Negate", onclick = "Shiny.onInputChange(\"select_button_2\",  this.id, {priority: \"event\"})"),
                                                            Details = shinyInput(actionButton, nrow(cur_data()), 'Details_', label = "Details", onclick = "Shiny.onInputChange(\"select_button_2\",  this.id, {priority: \"event\"})")),
                             server = FALSE, escape = FALSE, selection = 'none', options = list(dom = 't',ordering = FALSE, "pageLength" = 40))
  output$LastStringID <- renderText(str_c("Last StringID: ",CurrentItem$LastStringID))
}

shinyApp(ui = ui, server = server)
