
# Course information button module ----
course_information_button_UI <- function(id, r) {
  showModal(
    modalDialog(title = "Edit Course Information", size = "l"
                , fluidRow(
                  column(width = 6
                         , tags$b("Course Location: ")
                         , textInput(inputId = NS(id,"location")
                                     , label = NULL, placeholder = r$course_info$location)
                         , br()
                         , tags$b("Meeting times: ")
                         , textInput(inputId = NS(id,"meeting_times")
                                     , label = NULL, placeholder = r$course_info$meeting_times)
                         , br()
                         , tags$b("Office hours: ")
                         , textInput(inputId = NS(id,"office_hours")
                                     , label = NULL, placeholder = r$course_info$office_hours)
                  )
                  , column(width = 6
                           , column(width = 6
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = NS(id,"link1_url")
                                                , label = NULL, placeholder = r$course_info$link1_url)
                                    , br()
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = NS(id,"link2_url")
                                                , label = NULL, placeholder = r$course_info$link2_url)
                                    , br()
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = NS(id,"link3_url")
                                                , label = NULL, placeholder = r$course_info$link3_url)
                           )
                           , column(width = 6
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = NS(id,"link1_text")
                                                , label = NULL, placeholder = r$course_info$link1_text)
                                    , br()
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = NS(id,"link2_text")
                                                , label = NULL, placeholder = r$course_info$link2_text)
                                    , br()
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = NS(id,"link3_text")
                                                , label = NULL, placeholder = r$course_info$link3_text)
                           )
                  )
                )
                , footer = actionBttn(inputId = NS(id,"save_info"), label = "Save Information", block = T)
    )
  )
}

course_information_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    #Store inputs ----
    
    #TODO: Rethink this
    observeEvent(input$location, {
      if (input$location != "")
      r$df_course_info$location <- input$location
    })
    observeEvent(input$meeting_times, {
      if (input$meeting_times != "")
        r$df_course_info$meeting_times <- input$meeting_times
    })
    observeEvent(input$office_hours, {
      if (input$office_hours != "")
        r$df_course_info$office_hours <- input$office_hours
    })
    observeEvent(input$link1_url, {
      if (!is.null(input$link1_url))
        r$df_course_info$link1_url <- input$link1_url
    })
    observeEvent(input$link2_url, {
      if (input$link2_url != "")
        r$df_course_info$link2_url <- input$link2_url
    })
    observeEvent(input$link3_url, {
      if (input$link3_url != "")
        r$df_course_info$link3_url <- input$link3_url
    })
    observeEvent(input$link1_text, {
      if (input$link1_text != "")
        r$df_course_info$link1_text <- input$link1_text
    })
    observeEvent(input$link2_text, {
      if (input$link2_text != "")
        r$df_course_info$link2_text <- input$link2_text
    })
    observeEvent(input$link3_text, {
      if (input$link3_text != "")
        r$df_course_info$link3_text <- input$link3_text
    })
   
    observeEvent(input$save_info, {
      df_info_in <- reactive({
        df_info_in <- tibble("location" = r$df_course_info$location
                          ,"meeting_times" = r$df_course_info$meeting_times
                          , "office_hours" = r$df_course_info$office_hours
                          , "link1_url" = r$course_info$link1_url
                          , "link2_url" = r$course_info$link2_url
                          , "link3_url" = r$course_info$link3_url
                          , "link1_text" = r$course_info$link1_text
                          , "link2_text" = r$course_info$link2_text
                          , "link3_text" = r$course_info$link3_text
        )
        
      })
      
      # Write to sheet ----
      view(df_info_in())
      sheet_write(
        ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df_info_in()
        , sheet = "course_info"
      )
      removeModal()
    })
    
  })
}