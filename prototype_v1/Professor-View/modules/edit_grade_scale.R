#  UI-----
edit_grade_scale_UI <- function(id) {
  showModal(
    modalDialog(title = "Edit Grade Scale", size = "l"
                , fluidRow(
                  column(width = 12
                         , rHandsontableOutput(NS(id, "grade_scale_table"))
                  )
                )
                , footer = fluidRow(
                  column(width = 6
                         , actionBttn(
                           inputId = NS(id,"save")
                           , label = "Save Scale"
                           , style = "material-flat"
                           , block = T
                         )
                  )
                  , column(width = 6
                           , actionBttn(
                             inputId = NS(id, "close")
                             , label = "Close"
                             , style = "material-flat"
                             , block = T
                           )
                  )
                )
    )
  )
}

# Server ----
edit_grade_scale_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    output$grade_scale_table <- renderRHandsontable({
      df_grade_scale <- r$df_grade_scale %>%
        select(`Level` = level, Title = title)
      
      rhandsontable(
        df_grade_scale
        , rowHeaders = NULL
        , stretchH = 'all'
        , readOnly = F
      ) %>%
        hot_col(col = c("Level"), type = "numeric")
      
    })
    
    observeEvent(input$save, {
      grade_scale <- hot_to_r(input$grade_scale_table)
      r$df_grade_scale <- grade_scale %>%
        select(level = Level, title = Title)
      write_rds(r$df_grade_scale, "data/df_grade_scale,RDS")
      showNotification("Saved in session.")
      
      removeModal()
      
    })
    
    observeEvent(input$close, {
      removeModal()
    })
    
  })
}