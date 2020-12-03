course_info_UI <- function(id){
  box(width = 4, title = "Course Information", status = "primary"
      , uiOutput(NS(id, "course_info_out"))
  )
}

course_link_UI <- function(id){
  box(width = 4, title = "Course Links", status = "primary"
      , uiOutput(NS(id, "web_link_out"))
  )
}

course_info_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    course_info_ui <- reactive({
      if (nrow(r$df_course_info) > 0) {
        list <-  tagList()
        for (row in seq(1:nrow(r$df_course_info))){
          type <-  r$df_course_info[row, 1]
          value <- r$df_course_info[row, 2]
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
    })
    
    
    output$course_info_out <- renderUI({
      course_info_ui()
    })
    
  }) # End server
}

course_link_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    web_link_ui <- reactive({
      if (nrow(r$df_links) > 0) {
        
        list <-  tagList()
        for (row in seq(1:nrow(r$df_links))){
          type <-  r$df_links[row, 1]
          value <- r$df_links[row, 2]
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
    })
    
    output$web_link_out <- renderUI({
      web_link_ui()
    })
    
  })
  }