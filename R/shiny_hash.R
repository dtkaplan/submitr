#' Shiny ui and server logic for the learnrhash submission system.
#'
#' @export
hash_controls <- function(url) {
  rclipboard::rclipboardSetup()
  shiny::tags$div(
    shiny::tags$h3(
      shiny::tags$a("Link to submit your work", href=url, target="_blank")
    ),
    shiny::tags$br(),
    uiOutput("clipboard"),
    htmlOutput("hash_output"),
    shiny::actionButton("hash_copy", "Start submission process."),

  )
}

#' @export
hash_logic <- function(input, output, session,
                       storage_actions = submitr::record_hash()) {
  # Set the learnr event recorder
  options(tutorial.event_recorder =
            make_a_recorder(storage_actions$write, "from_hash"))

  get_hash <- eventReactive(input$hash_copy, {
    learnrhash::encode_obj(storage_actions$get_events())
  })

  message_for_submit <- reactiveVal("")
  time_hash_created <- reactiveVal(Sys.time())

  # This was taken out when the clipbutton system was added
  # to shift the work to JavaScript
  # observeEvent(input$hash_copy, {
  #   clipr::write_clip(get_hash(), allow_non_interactive = TRUE)
  # })

  observe({
    invalidateLater(5000, session)
    if (input$hash_copy == 0 || as.numeric(Sys.time() - time_hash_created()) > 20)
      message_for_submit("")
    else if (as.numeric(Sys.time() - time_hash_created()) > 10)
      message_for_submit("<p>Turning off the message, but the clipboard still has the submissions to be uploaded to the above link.</p>")
  })

  observe({
    hash <- get_hash()
    res <- paste0(
      "<p>Your work has been copied to the clipboard as a compressed string. Follow ",
      "the link above and paste your clipboard contents at that site.</p>",
      "<p>The clipboard will look like <code>",
      substr(hash, 0, 20), "</code> with ",
      nchar(hash), " characters altogether.",
      "It currently contains ", nrow(storage_actions$get_events()), " submission events.</p>")
    message_for_submit(res)
    time_hash_created(Sys.time())
  })

  output$hash_output <- renderText({
    HTML(message_for_submit())
      })

    output$clipboard <- renderUI({
      message_for_submit() # for the dependency
      rclipboard::rclipButton("clipbtn", "Copy work to clipboard", get_hash(), icon("clipboard"))
    })
}

