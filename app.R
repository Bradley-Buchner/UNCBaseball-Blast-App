
library(shiny)
library(RMariaDB)
library(tidyverse)
library(bayesbio)
library(DT)
shiny::devmode()

userpassword <- "Remy2022"

source("app functions.R")

## load in data from db ##

# # load all of table query
# table_name = "blaser_joined"
# load_all_query <- paste0("SELECT * FROM ", table_name, ";")
# # print(load_all_query)
# blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
# rs = dbSendQuery(blastDb, load_all_query)
# loadedData <- dbFetch(rs)
# # Clear the result
# dbClearResult(rs)
# # Disconnect to clean up the connection to the database.
# dbDisconnect(blastDb)
# blaser_joined <- loadedData
# 
# # load all of table query
# table_name = "blaser_swings"
# load_all_query <- paste0("SELECT * FROM ", table_name, ";")
# # print(load_all_query)
# blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
# rs = dbSendQuery(blastDb, load_all_query)
# loadedData <- dbFetch(rs)
# # Clear the result
# dbClearResult(rs)
# # Disconnect to clean up the connection to the database.
# dbDisconnect(blastDb)
# blaser_swings <- loadedData

# read in athletes
# load all of table query
table_name = "athletes"
load_all_query <- paste0("SELECT * FROM ", table_name, ";")
# print(load_all_query)
blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
rs = dbSendQuery(blastDb, load_all_query)
loadedData <- dbFetch(rs)
# Clear the result
dbClearResult(rs)
# Disconnect to clean up the connection to the database.
dbDisconnect(blastDb)
athletes <- loadedData

# load all of table query
table_name = "tmBP"
load_all_query <- paste0("SELECT * FROM ", table_name, ";")
# print(load_all_query)
blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
rs = dbSendQuery(blastDb, load_all_query)
loadedData <- dbFetch(rs)
# Clear the result
dbClearResult(rs)
# Disconnect to clean up the connection to the database.
dbDisconnect(blastDb)
tmBP <- loadedData

hitters = unique(athletes$tm_name)


# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(column(4, h1("Blast and BP Data Viewer"))),
    fluidRow(column(4, selectInput("selecthitter", label = "", choices = sort(hitters), selected = "Alvarez, Patrick")), 
             column(2, img(height = 100, width = 235, src="carolina.svg.png"))),
    
    tabsetPanel(
        tabPanel("Home", 
                 br(),
                 em("View a summary of your most recent session compared with your season averages, 
                    as well as your most recent notes and swing focuses/adjustments"),
                 # textOutput("test"),
                 fluidRow(
                     column(9, h2("Most Recent Session"),
                            tableOutput("recent_session_summary")),
                     column(3, h2("Recent Notes"),
                            tableOutput("recent_notes"),
                            br(),
                            h2("Recent Focuses"),
                            tableOutput("recent_mechanics")
                            
                     )
                 )
                 ),
        tabPanel("Analysis",
                 br(),
                 selectInput("selectSession2", label = "Choose Session", choices = NULL),
                 fluidRow(
                     column(9, h2("Session Summary"),
                            tableOutput("session_summary")),
                     ############
                     column(3,  
                            selectInput("best_worst_metric", label = "Choose Metric", choices = c("Plane Score (20-80)", "Connection Score (20-80)", 
                                                                                                      "Rotation Score (20-80)", "Bat Speed (mph)", "Hand Speed (mph)", 
                                                                                                      "Time to Contact (sec)"), selected = "Bat Speed (mph)"),
                            h3("Best Swings"),
                            tableOutput("best_swings"),
                            h3("Worst Swings"),
                            tableOutput("worst_swings")) 
                 )
                 ),
        tabPanel("Notebook",
                 tags$style(
                     type = 'text/css',
                     # '.modal-dialog { width: fit-content !important; }',
                     '.modal-lg {
                         width: 1100px;
                     }'
                 ),
                 br(),
                 em("Leave notes on full sessions or individual swings"),
                 br(),
                 br(),
                 sidebarLayout(
                     sidebarPanel(width = 2,
                         selectInput("selectSession", label = "Choose Session", choices = NULL),
                         actionButton("make_swing_note", "Leave Swing Note"),
                         br(),
                         br(),
                         actionButton("make_session_note", "Leave Session Note"),
                         br(),
                         br(),
                         actionButton("make_edit", "Edit/Delete Note"),
                         em(h5("Refresh your browser to see edits"))
                     ),
                     mainPanel(
                         dataTableOutput("notebook_add")#,
                         # tableOutput("notebook_view")
                     )
                 )
                 ),
        tabPanel("Swing Mechanics",
                 tags$style(
                     type = 'text/css',
                     # '.modal-dialog { width: fit-content !important; }',
                     '.modal-lg {
                         width: 1100px;
                     }'
                 ),
                 br(),
                 em("Make note of your swing mechanics focuses and log swing adjustments"),
                 br(),
                 br(),
                 sidebarLayout(
                     sidebarPanel(width = 2,
                                  actionButton("log_adjustment", "Log a Focus"),
                                  br(),
                                  br(),
                                  actionButton("make_edit_adj", "Edit/Delete"),
                                  br(),
                                  br(),
                                  em(h5("Refresh your browser to see edits"))
                     ),
                     mainPanel(
                         # dataTableOutput("notebook_add")#,
                     )
                 )
        ),
        tabPanel("Logs",
                 fluidRow(column(1, ""),
                     column(5, h2("All Notes"), tableOutput("notes_history")),
                          column(6, h2("All Swing Focuses/Adjustments"), tableOutput("mechanics_history"))
                 )
                 )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    name <- reactive({athletes[which(athletes$tm_name == input$selecthitter), "blast_name"]})
    
    # updated_date <- eventReactive(input$selecthitter, {
    #     input$selectSession
    # })
    
    # output$test <- renderText({
    #     name()
    # })
    playerBP <- eventReactive(input$selecthitter, {
        # name = "horvath"
        # load all of table query
        table_name = paste0(name(), "_joinedBP")
        load_all_query <- paste0("SELECT * FROM ", table_name, ";")
        # print(load_all_query)
        blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
        rs = dbSendQuery(blastDb, load_all_query)
        loadedData <- dbFetch(rs)
        # Clear the result
        dbClearResult(rs)
        # Disconnect to clean up the connection to the database.
        dbDisconnect(blastDb)
        loadedData
    })

    observeEvent(input$selecthitter ,{
        updateSelectInput(inputId = "selectSession", choices = as.character(rev(unique(playerBP()$Date...2))))
        updateSelectInput(inputId = "selectSession2", choices = as.character(rev(unique(playerBP()$Date...2))))
    })
    
    # update_date <- eventReactive(input$selecthitter ,{
    #     updateSelectInput(inputId = "selectSession", choices = as.character(rev(unique(playerBP()$Date...2))))
    #     # updateSelectInput(inputId = "selectSession2", choices = as.character(rev(unique(playerBP()$Date...2))))
    # })

    output$recent_session_summary <- renderTable({
        data = recent.session.summary(name())
        data$`Time to Contact (sec)` <- formatC(data$`Time to Contact (sec)`, digits = 2)
        data
    }, rownames=T, digits=1, align = "r")
    
    output$session_summary <- renderTable({
        data = session.summary(name(), date = input$selectSession2)
        data$`Time to Contact (sec)` <- formatC(data$`Time to Contact (sec)`, digits = 2)
        data
    }, rownames=T, digits=1, align = "r")

    output$best_swings <- renderTable({
        head(best.swings(name(), metric = input$best_worst_metric) %>% na.omit(), 3)
    })

    output$worst_swings <- renderTable({
        head(worst.swings(name(), metric = input$best_worst_metric) %>% na.omit(), 3)
    })

    output$recent_notes <- renderTable({
        recent.notes(name())
    })

    output$recent_mechanics <- renderTable({
        df = data.frame(get.mechanics(name())) %>% arrange(desc(id))
        df %>% select(-id)
    })

    notebook1 <- reactive({
        notebook.table(name()) %>% filter(Date == input$selectSession) %>% select(-Date)
    })
    
    notebook2 <- reactive({
        get.notes(name()) %>% arrange(desc(id))
    })
    
    notebook3 <- reactive({
        get.mechanics(name()) %>% arrange(desc(id))
    })

    observeEvent(input$swing_submit, {
        note = as.character(input$swing_note_textbox)
        # name
        date = input$selectSession
        # data = notebook1()
        id = as.numeric(notebook1()[as.integer(Clicked()), "Swing ID"])
        add.swing.note(note = note, id = id, name = name(), date = date)
    })

    observeEvent(req(input$sess_submit), {
        note = as.character(input$session_note_textbox)
        # name
        date = req(input$selectSession)
        # data = notebook1()
        # print("good")
        add.session.note(note = note, name = name(), date = date)
    })

    observeEvent(req(input$edit_submit), {
        note = as.character(input$edit_note_textbox)
        # name
        date = req(input$selectSession)
        # data = notebook1()
        # print("good")
        id = as.numeric(notebook2()[as.integer(ClickedEdit()), "id"])
        edit.note(id = id, name = name(), note = note)
    })

    output$notebook_add <- renderDataTable({
        # date = input$selectSession
        # name = athletes[which(athletes$tm_name == input$selecthitter), "blast_name"]
        # notes.datatable(name(), date = input$selectSession)
        # df = notebook1() %>% filter(Date == input$selectSession) %>% select(-Date)
        datatable(notebook1(), selection = 'single', options = list(pageLength = 15, dom = "tpi"), rownames = F)
    })

    output$notebook_add_modal <- renderDataTable({
        # notebook1 <- notebook.add.table(name = name(), date = input$selectSession)
        # df = notebook1() %>% filter(Date == input$selectSession) %>% select(-Date)
        datatable(notebook1(), selection = 'single', options = list(pageLength = 5, dom = "tpi"), rownames = F)
    })
    
    output$notebook_edit_modal <- renderDataTable({
        # notebook1 <- notebook.add.table(name = name(), date = input$selectSession)
        # df = notebook1() %>% filter(Date == input$selectSession) %>% select(-Date)
        datatable(notebook2(), selection = 'single', options = list(pageLength = 5, dom = "tpi"), rownames = F)
    })
    
    output$notebook_edit_modal_adj <- renderDataTable({
        # notebook1 <- notebook.add.table(name = name(), date = input$selectSession)
        # df = notebook1() %>% filter(Date == input$selectSession) %>% select(-Date)
        datatable(notebook3(), selection = 'single', options = list(pageLength = 5, dom = "tpi"), rownames = F)
    })

    # output$Table <- renderDataTable({datatable(mtcars, selection = 'single')})

    Clicked <- eventReactive(input$notebook_add_modal_rows_selected,{
        input$notebook_add_modal_rows_selected
    })
    
    ClickedEdit <- eventReactive(input$notebook_edit_modal_rows_selected,{
        input$notebook_edit_modal_rows_selected
    })
    
    ClickedEditAdj <- eventReactive(input$notebook_edit_modal_adj_rows_selected,{
        input$notebook_edit_modal_adj_rows_selected
    })

    output$selected <- renderText({
        paste0("Selected Swing ID: ", notebook1()[as.integer(Clicked()), "Swing ID"])
        })
    
    output$selected_edit <- renderText({
        paste0("Selected Note ID: ", notebook2()[as.integer(ClickedEdit()), "id"])
    })
    
    output$selected_edit_adj <- renderText({
        paste0("Selected ID: ", notebook3()[as.integer(ClickedEditAdj()), "id"])
    })

    observeEvent(input$make_session_note, {
        showModal(modalDialog(h2("Leave Session Note"),
                              textAreaInput("session_note_textbox", value = "", label = NULL, placeholder = "leave note here", rows = 5),
                              footer = tagList(
                                  modalButton("Cancel"),
                                  actionButton("sess_submit", "Submit")
                              )
        ))
    })

    observeEvent(input$make_swing_note,{
        showModal(modalDialog(h2("Leave Swing Note"),
                              em("click the row you want to add a note to, then leave a note below and press submit"),
                              DT::dataTableOutput("notebook_add_modal"),
                              size = "l", br(),
                              textOutput("selected"),
                              textAreaInput("swing_note_textbox", value = "", label = NULL, placeholder = "leave note here", rows = 3),
                              footer = tagList(
                                  modalButton("Cancel"),
                                  actionButton("swing_submit", "Submit")
                              )
        ))
    })
    
    observeEvent(input$make_edit, {
        showModal(modalDialog(h2("Edit/Delete"),
                              em("click the row you want to edit/delete, then leave an updated note below and press submit or press delete"),
                              DT::dataTableOutput("notebook_edit_modal"), 
                              size = "l", br(),
                              textOutput("selected_edit"),
                              textAreaInput("edit_note_textbox", value = "", label = NULL, placeholder = "leave note here", rows = 3),
                              footer = tagList(
                                  modalButton("Cancel"),
                                  actionButton("edit_delete", "Delete"),
                                  actionButton("edit_submit", "Submit"),
                              )
        ))
    })

    observeEvent(input$swing_submit, {
        removeModal()
    })

    observeEvent(input$sess_submit, {
        removeModal()
    })
    
    observeEvent(input$edit_submit, {
        removeModal()
    })
    
    observeEvent(input$edit_delete, {
        id1 = as.numeric(notebook2()[as.integer(ClickedEdit()), "id"])
        id2 = as.numeric(notebook2()[as.integer(ClickedEdit()), "Swing ID"])
        delete.note(id1 = id1, id2 = id2, name = name())
        removeModal()
    })

    ## Swing Adjustments ##

    all_days = seq(as.Date("2022-09-05"), as.Date(Sys.Date()), by="days")

    observeEvent(input$log_adjustment, {
        showModal(modalDialog(h2("Log Swing Focus or Adjustment"),
                              selectInput("selectAdjSession", label = "Choose Date", choices = rev(all_days)),
                              textAreaInput("adjustment_desc_textbox", value = "", label = "Description", placeholder = "Describe your focus or swing adjustment", rows = 3),
                              textAreaInput("adjustment_reasoning_textbox", value = "", label = "Reasoning", placeholder = "What is your goal?", rows = 3),
                              footer = tagList(
                                  modalButton("Cancel"),
                                  actionButton("adj_submit", "Submit")
                              )
        ))
    })
    
    observeEvent(input$make_edit_adj, {
        showModal(modalDialog(h2("Edit/Delete"),
                              em("click the row you want to edit/delete, then update the fields below and press submit or press delete"),
                              DT::dataTableOutput("notebook_edit_modal_adj"), 
                              size = "l", br(),
                              textOutput("selected_edit_adj"),
                              br(),
                              textAreaInput("edit_adjustment_desc_textbox", value = "", label = "Description", placeholder = "Describe your focus or swing adjustment", rows = 3),
                              textAreaInput("edit_adjustment_reasoning_textbox", value = "", label = "Reasoning", placeholder = "What is your goal?", rows = 3),
                              br(),
                              footer = tagList(
                                  modalButton("Cancel"),
                                  actionButton("edit_adj_delete", "Delete"),
                                  actionButton("edit_adj_submit", "Submit"),
                              )
        ))
    })

    observeEvent(req(input$adj_submit), {
        desc = as.character(input$adjustment_desc_textbox)
        reas = as.character(input$adjustment_reasoning_textbox)
        # name = "blaser"
        date = req(input$selectAdjSession)
        # data = notebook1()
        # print("good")
        log.mechanics(desc = desc, reas = reas, name = name(), date = date)
    })
    
    observeEvent(req(input$edit_adj_submit), {
        desc = as.character(input$edit_adjustment_desc_textbox)
        reas = as.character(input$edit_adjustment_reasoning_textbox)
        # name
        # data = notebook1()
        # print("good")
        id = as.numeric(notebook3()[as.integer(ClickedEditAdj()), "id"])
        edit.mechanics(id, name(), desc, reas)
        removeModal()
    })
    
    observeEvent(input$edit_adj_delete, {
        id = as.numeric(notebook3()[as.integer(ClickedEditAdj()), "id"])
        delete.mechanics(id, name())
        removeModal()
    })

    observeEvent(input$adj_submit, {
        removeModal()
    })

    output$mechanics <- renderTable({
        df = data.frame(get.mechanics(name())) %>% arrange(desc(id))
        df %>% select(-id)
    })
    
    ## History ##
    output$notes_history <- renderTable({
        recent.notes(name())
    })
    
    output$mechanics_history <- renderTable({
        recent.mechanics(name())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
