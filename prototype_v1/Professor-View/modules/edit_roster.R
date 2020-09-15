
# Course information button module ----
edit_roster_button_UI <- function(id) {
  showModal(
    modalDialog(title = "Roster", size = "l"
                , fluidRow(tabsetPanel(
                  tabPanel(
                    title = "Current Roster"
                    
                  )
                  , tabPanel(
                    title = "Add to Roster"
                    
                  )
                  , tabPanel(
                    title = "Remove from Roster"
                  )
                )
                )
    )
  )
}

edit_roster_button_Server <- function(id, r){
  moduleServer(id, function(input,output,session){
    
    # output$removeFromRoster <-  renderUI({
    #   checkboxGroupInput(
    #     inputId = NS(id, "removeFromRoster")
    #     , label = "Select Students to Remove: "
    #     , choices = r$df_student$id
    #   )
    # })
    
    # observeEvent(input$removeSave, {
    # })
    # 
    # observeEvent(input$addSave, {
    #   df_review <- r$df_review
    #   new_row <- tibble("id" = input$reviewNumber
    #                     , "date" = input$reviewDate
    #                     , "description" = input$reviewDescription
    #                     
    #   )
    #   
    #   # Write to sheet ----
    #   if(input$reviewNumber %in% df_review$id){
    #     showNotification("Review number already exists.")
    #     removeModal()
    #   }else{
    #     sheet_append(
    #       ss = "https://docs.google.com/spreadsheets/d/1xIC4pGhnnodwxqopHa45KRSHIVcOTxFSfJSEGPbQH20/edit#gid=2102408290"
    #       , data = new_row
    #       , sheet = "review"
    #     )      # Update reactive ----
    #     new_df <- rbind(df_review, new_row)
    #     r$df_review <- new_df
    #     
    #     #TODO: write to exam to topic sheet
    #     removeModal()
    #     showNotification("Saved to remote.")
    #   }
    # })
    # 
    # observeEvent(input$close, {
    #   removeModal()
    # })
    
  })
}