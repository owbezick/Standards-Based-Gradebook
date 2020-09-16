
# Course information button module ----
add_review_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Add Review", size = "l", easyClose = T
                , fluidRow(
                  column(width = 6
                         , fluidRow(
                           column(width = 6
                                  , numericInput(inputId = NS(id, "reviewNumber")
                                                 , label = "Review Number: "
                                                 , value = 0)
                           )
                           , column(width = 6
                                    , dateInput(inputId = NS(id, "reviewDate")
                                                , label = "Date: ")
                           )
                         )
                         , fluidRow(
                           column(width = 12
                                  , textAreaInput(inputId = NS(id, "reviewDescription")
                                                  , label = "Review Description: ")
                           )
                         )
                  )
                  , column(width = 6
                           , uiOutput(NS(id, "topics"))
                  )
                )
                , footer = fluidRow(
                  column(width = 12
                         , actionBttn(
                           inputId = NS(id,"save")
                           , label = "Save Review"
                           , style = "material-flat"
                           , block = T
                         )
                  )
                )
    )
  )
}

add_review_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    output$topics <-  renderUI({
      checkboxGroupInput(
        inputId = NS(id, "addReviewTopics")
        , label = "Select Topics: "
        , choices = r$df_topic$id
      )
    })
    
    observeEvent(input$save, {
      # Add review to df_review ----
      df_review <- r$df_review
      new_row <- tibble("id" = input$reviewNumber
                        , "date" = input$reviewDate
                        , "description" = input$reviewDescription
                        
      )
      # Write to sheet 
      if(input$reviewNumber %in% df_review$id){
        showNotification("Review number already exists.")
        removeModal()
      }else{
        sheet_append(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
          , data = new_row
          , sheet = "review"
          
        )      
        # Update reactive 
        new_df <- rbind(df_review, new_row)
        r$df_review <- new_df
        
      }
      # Add review to review_to_topic ----
      df_review_to_topic <- r$review_to_topic
      ls_student_ids <- r$df_student$id
      ls_topics_to_add <- input$addReviewTopics
      review_id <- input$reviewNumber
      # Replicate list of student id's  to as many topics there are:
      ls_student_ids_rep <- rep(ls_student_ids, length(ls_topics_to_add))
      # Replicate list of topics to as many students there are:
      ls_topic_ids_rep <- rep(ls_topics_to_add, length(ls_student_ids))
      # Replicate list of review numbers to as many students and topics there are:
      ls_review_id_rep <- rep(review_id, length(ls_student_ids_rep))
      # List of "NA"s for grade
      ls_grade_rep <- rep("NA", length(ls_review_id_rep))
      # Allll together now!
      df_review_to_topic_new <- tibble(
        "review_id" = ls_review_id_rep
        , "topic_id" = ls_topic_ids_rep
        , "student_id" = ls_student_ids_rep
        , "grade" = ls_grade_rep
      )
      # Write to sheet 
      if(input$reviewNumber %in% df_review_to_topic$id){
        showNotification("Review number already exists.")
        removeModal()
      }else{
        sheet_append(
          ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
          , data = df_review_to_topic_new
          , sheet = "review_to_topic"
          
        )      
        # Update reactive 
        new_df <- rbind(df_review_to_topic, df_review_to_topic_new)
        r$df_review_top_topic <- new_df
        showNotification("Saved to remote.")
        removeModal()
      }
    })
    
    observeEvent(input$close, {
      removeModal()
    })
    
  })
}