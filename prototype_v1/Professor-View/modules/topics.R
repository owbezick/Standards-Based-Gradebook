topics_UI <- function(id){
  box(width = 12, title = "Review Table", status = "primary"

      , rHandsontableOutput(NS(id, "topicTable"))
      , footer = "Right click to add a row to the table."

      , actionBttn(NS(id, "save"), "Save", style = "material-flat", block = T)

  )
}

topics_server <- function(id, r){
  moduleServer(id, function(input,output,session){
    # Topic Table Output ----
    output$topicTable <- renderRHandsontable({
      df <- r$df_review_table
      df <- df %>%
        mutate(`Review Date` = ymd(`Review Date`)
               , `Review ID` = as.character(`Review ID`))
      rhandsontable(
        df
        , rowHeaders = NULL
        , stretchH = 'all'
      ) %>%
        hot_context_menu(allowComments = T)
    })

    # Save topic table -----
    observeEvent(input$save, {
      df_hot <- hot_to_r(input$topicTable)
      # Save Review Table ----
      r$df_review_table <- df_hot
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df_hot
        , sheet = "review_table"
      )

      `%notin%` <- Negate(`%in%`)

      # Save review_grades ----
      df_review_grades <- r$df_review_grades

      df_student <- r$df_student %>%
        select(student_id)

      data_from_hot <- df_hot %>%
        pivot_longer(cols = c(4:ncol(df_hot))) %>%
        na.omit()

      review_topic_id_hot <- data_from_hot %>%
        filter(value == "TRUE")

      review_topic_id_hot <- review_topic_id_hot %>%
        mutate(topic_id = str_split_fixed(review_topic_id_hot$name, " ", 2)[,2]) %>%
        select(review_id = `Review ID`, topic_id)

      number_of_topics <- nrow(review_topic_id_hot)
      # replicate review_id and topic_id for as many students that are in the class
      new_review_topic_rep <- do.call("rbind", replicate(nrow(df_student), review_topic_id_hot, simplify = FALSE))

      # replicate student_id the for as many topics being added
      student_id <- do.call("rbind", replicate(nrow(review_topic_id_hot), df_student, simplify = FALSE)) %>%
        arrange(student_id)

      df_review_grades <- df_review_grades %>%
        filter(grade != "NA") %>%
        mutate(
          filter_id = paste(review_id, topic_id, student_id)
        )

      df_new_review_data <- new_review_topic_rep %>%
        mutate(student_id = student_id$student_id
               , grade = rep("NA", nrow(new_review_topic_rep))
               , filter_id = paste(review_id, topic_id, student_id)) %>%
        filter(filter_id %notin% df_review_grades$filter_id) %>%
        rbind(df_review_grades) %>%
        arrange(review_id, topic_id) %>%
        select(-c(filter_id))


      # Refresh and save data ----
      r$df_review_grades <- df_new_review_data
      sheet_write(
        ss =  "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
        , data = df_new_review_data
        , sheet = "review_grades"
      )
      showNotification("Saved to remote.")
    })




  })
}
