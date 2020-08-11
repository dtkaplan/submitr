#' Shiny ui and server logic for the learnrhash submission system.
#'
#' @param url A URL to display indicating where students should hand-in their hash codes
#' @param authentication Flag (`FALSE`). Whether to display text-entry boxes for entering a user  ID and an authentication code.
#' @export
hash_controls <- function(url, authentication = FALSE) {
  if  (authentication)  {
    auth_boxes <- shiny::span(
      shiny::tags$input(type="text",  id="UserID",
                        size = 25,
                        placeholder="Your user ID (typically, email)"),
      shiny::tags$input(type="text",  id="Authentication",
                        size = 25,
                        placeholder="Your authentication code"),

      shiny::tags$strong(
        shiny::tags$a("Link to submit your work", href=url, target="_blank")
        ),
      shiny::tags$br()

    )
  } else {
    auth_boxes <- shiny::tags$h3(
      shiny::tags$a("Link to submit your work", href=url, target="_blank")
    )
  }
  rclipboard::rclipboardSetup()
  shiny::tags$div(
    auth_boxes,
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
    Events <- storage_actions$get_events()
    if ("UserID" %in% names(input))
      Events$user_id <- rep(input$UserID,  nrow(Events))
    if ("Authentication" %in% names(input))
      Events$authentication <- rep(input$Authentication, nrow(Events))
    learnrhash::encode_obj(Events)
  })

  hash_contents_to_show <- reactiveVal("")

  observe({
    hash_contents_to_show(get_hash())
  })

  output$show_hash <- renderText({
    hash_contents_to_show()
  })
}

