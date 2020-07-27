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
    wrapped_verbatim_text_output("show_hash", placeholder = TRUE),
    # htmlOutput("hash_output"),
    #shiny::actionButton("hash_copy", "Start submission process."),
  )
}

# From learnrhash
wrapped_verbatim_text_output = function(outputId, placeholder = FALSE) {
  x = shiny::verbatimTextOutput(outputId, placeholder)
  x$attribs$style = "white-space: pre-wrap;"

  x
}

#' @export
hash_logic <- function(input, output, session,
                       storage_actions = submitr::record_hash()) {
  # Set the learnr event recorder
  options(tutorial.event_recorder =
            make_a_recorder(storage_actions$write, "from_hash"))


  get_hash <- reactive({
    invalidateLater(5000, session)
    learnrhash::encode_obj(storage_actions$get_events())
  })

  hash_contents_to_show <- reactiveVal("")

  observe({
    hash_contents_to_show(get_hash())
  })

  output$show_hash <- renderText({
    hash_contents_to_show()
  })
}

