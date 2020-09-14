
# Course information button module ----
course_information_button_UI <- function(id, r) {
  df <- r$df_course_info
  showModal(
    modalDialog(title = "Edit Course Information", size = "l"
                , fluidRow(
                  column(width = 6
                         , tags$b("Course Location: ")
                         , textInput(inputId = NS(id,"location")
                                     , label = NULL, value = df$location)
                         , br()
                         , tags$b("Meeting times: ")
                         , textInput(inputId = NS(id,"meeting_times")
                                     , label = NULL, value = df$meeting_times)
                         , br()
                         , tags$b("Office hours: ")
                         , textInput(inputId = NS(id,"office_hours")
                                     , label = NULL, value = df$office_hours)
                  )
                  , column(width = 6
                           , column(width = 6
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = NS(id,"link1_url")
                                                , label = NULL, value = df$link1_url)
                                    , br()
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = NS(id,"link2_url")
                                                , label = NULL, value = df$link2_url)
                                    , br()
                                    , tags$b("Link URL: ")
                                    , textInput(inputId = NS(id,"link3_url")
                                                , label = NULL, value = df$link3_url)
                           )
                           , column(width = 6
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = NS(id,"link1_text")
                                                , label = NULL, value = df$link1_text)
                                    , br()
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = NS(id,"link2_text")
                                                , label = NULL, value = df$link2_text)
                                    , br()
                                    , tags$b("Link Description: ")
                                    , textInput(inputId = NS(id,"link3_text")
                                                , label = NULL, value = df$link3_text)
                           )
                  )
                )
                , footer = fluidRow(actionBttn(inputId = NS(id,"save"), label = "Save Information", block = T)
                                    , actionBttn(inputId = NS(id, "close"), label = "Close", block = T))
    )
  )
}

course_information_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    observeEvent(input$save, {
      new_df <- tibble("location" = input$location
                          ,"meeting_times" = input$meeting_times
                          , "office_hours" = input$office_hours
                          , "link1_url" = input$link1_url
                          , "link2_url" = input$link2_url
                          , "link3_url" = input$link3_url
                          , "link1_text" = input$link1_text
                          , "link2_text" = input$link2_text
                          , "link3_text" = input$link3_text
        )
      
      # Write to sheet ----
        sheet_write(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
          , data = new_df
          , sheet = "course_info"
        )
      
      # Update reactive ----
      r$df_course_info <- new_df
      showNotification("Saved to remote.")
      removeModal()
      
      })
    observeEvent(input$close, {
      removeModal()
    })

      
    
  })
}