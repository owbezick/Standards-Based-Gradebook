# Page helper buttons ----
nextPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i + 1)), "next")
}
prevPage <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i - 1)), "prev")
}

wrapPage <- function(title, page, button_left = NULL, button_right = NULL) {
  tabPanel(
    title = title, 
    fluidRow(
      column(12, page)
    ), 
    fluidRow(
      column(6, button_left),
      column(6, button_right)
    )
  )
}

wizardUI <- function(id, pages, doneButton = NULL) {
  stopifnot(is.list(pages))
  n <- length(pages)
  
  wrapped <- vector("list", n)
  for (i in seq_along(pages)) {
    # First page only has next; last page only prev + done
    lhs <- if (i > 1) prevPage(id, i)
    rhs <- if (i < n) nextPage(id, i) else doneButton
    wrapped[[i]] <- wrapPage(paste0("page_", i), pages[[i]], lhs, rhs)
  }
  
  # Create tabsetPanel
  # https://github.com/rstudio/shiny/issues/2927
  wrapped$id <- NS(id, "wizard")
  wrapped$type <- "hidden"
  do.call("tabsetPanel", wrapped)
}

wizardServer <- function(id, n) {
  moduleServer(id, function(input, output, session) {
    changePage <- function(from, to) {
      observeEvent(input[[paste0("go_", from, "_", to)]], {
        updateTabsetPanel(session, "wizard", selected = paste0("page_", to))
      })  
    }
    ids <- seq_len(n)
    lapply(ids[-1], function(i) changePage(i, i - 1))
    lapply(ids[-n], function(i) changePage(i, i + 1))
  })
}


page1 <- tagList(
  box(title = "Add some students to your roster:"
      , "The roster and any other data can be edited within the app, too!"
  )
)
page2 <- tagList(
  numericInput("age", "How old are you?", 20)
)
page3 <- tagList(
  "Is this data correct?",
  verbatimTextOutput("info")
)

ui <- fluidPage(
  br()
  , br()
  , fluidRow(
    actionBttn("test", "test")
  )
)


server <- function(input, output, session) {
  wizardServer("demographics", 3)
  observeEvent(input$test, {
    showModal(
      modalDialog(
        wizardUI(
          id = "demographics", 
          pages = list(page1, page2, page3), 
          doneButton = actionButton("done", "Submit")
        )  
      )
    )
  })
  
  observeEvent(input$done, {
    showModal(
      modalDialog("Thank you!")
    )
  })
  
  output$info <- renderText(paste0(
    "Age: ", input$age, "\n",
    "Name: ", input$name, "\n"
  ))
}
shinyApp(ui = ui, server = server)
