homework_UI <- function(id) {
  tabPanel(title = "Homework"
           , fluidRow(
             box(width = 12
                 , title = "Homework Grades"
                 , rHandsontableOutput(NS(id, "homework_table"))
                 , actionBttn(NS(id, "save"), "Save", style = "material-flat")
             )
           )
  )
}

homework_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    
    
    output$homework_table <- renderRHandsontable({
      df_homework_table <- r$df_homework_table 
      rhandsontable(df_homework_table
                    , rowHeaders = NULL) %>%
        hot_heatmap()
    })  
    
    observeEvent(input$save,{
      df_hot <- hot_to_r(input$homework_table)
      r$df_homework_table  <- df_hot
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df_hot
        , sheet = "homework_table"
      )
      showNotification("Saved to remote.")
    })
    
  }) #end module server
}

review_UI <- function(id) {
  tabPanel(title = "Reviews"
           , fluidRow(
             box(width = 12, title = "View by student or review or select topics button/select"
                 , "Data table of information"
                 , "Graph of information"
                 , "ways to edit")
           )
  )
}

review_server <- function(id, r){
  moduleServer(id, function(input, output, session){
  })
}