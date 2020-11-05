course_info_UI <- function(id){
  box(width = 8, title = "Course Information", height = "22vh"
  , column(width = 6
      ,  uiOutput(NS(id, "course_info_out"))
  )
  , column(width = 6
        , uiOutput(NS(id, "web_link_out"))
  )
  )
  
}

course_info_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    course_info_ui <- reactive({
      if (nrow(r$df_course_info) > 0) {
        
        list <-  tagList()
        for (row in seq(1:nrow(r$df_course_info))){
          type = pull(r$df_course_info[row, 1])
          value = pull(r$df_course_info[row, 2])
          ui <- box(title = NULL
                    , fluidRow(
                      column(width = 12
                             , tags$b(paste0(type, ":"))
                             , value
                      )
                    )
          )
          list <- append(list,ui)
        }
        list[seq(3, length(list), 3)]
      }
      else{
        
      }
    })
    
    web_link_ui <- reactive({
      if (nrow(r$df_links) > 0) {
        
        list <-  tagList()
        for (row in seq(1:nrow(r$df_links))){
          type = pull(r$df_links[row, 1])
          value = pull(r$df_links[row, 2])
          ui <- box(title = NULL
                    , fluidRow(
                      column(width = 12
                             , div(tags$a(
                               type
                               , href =  value
                             )
                             )
                      )
                    )
          )
          list <- append(list,ui)
        }
        list[seq(3, length(list), 3)]
      }
      else{
        
      }
    })
    
    
    output$course_info_out <- renderUI({
      course_info_ui()
    })
    
    
    output$web_link_out <- renderUI({
      web_link_ui()
    })
    
  }) # End server
  
}