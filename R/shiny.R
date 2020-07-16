#' Login related shiny operations for submittr
#'
#' @export
login_controls <- function(){
  knitr::asis_output(
    #"<table><tr></td><div id=\"login_message\" class=\"shiny-text-output\"></td></tr><tr><td>ID:</td><td><input id=\"userID\" type=\"text\" class=\"form-control\" value=\"\"/></td><td>     Password:</td><td><input id=\"password\" type=\"password\" class=\"form-control\" value=\"\" /></td><td></td></tr><tr><td></td><td><div id=\"download\" class=\"shiny-html-output\"><div></td></tr></table>"
    paste(
      "<table><tr>",
      "<td colspan=3><span id=\"login_message\" class=\"shiny-text-output\"></td></tr>",
      "<tr><td>ID:</td><td><input id=\"userID\" type=\"text\" class=\"form-control\" value=\"\"/></td>",
      "<td>     Password:</td><td><input id=\"password\" type=\"password\" class=\"form-control\" value=\"\" /></td>",
      "<td>     <button id=\"flush\" type=\"button\" class=\"btn btn-default action-button\">Submit your work</button></td>",
      "<td></td></tr>",
      "<tr><td></td><td><div id=\"download\" class=\"shiny-html-output\"><div></td>",
      "</tr></table>",
      collapse = "")
      )
}

#' @export
shiny_logic <- function(input, output, session,
                        vfun, storage_actions) {

  observe({
    req(input$flush)
    storage_actions$flush()
    showModal(modalDialog(
      p("Your previous answers have been submitted."),
      p("You can wait until you are done with your present session before pressing 'Submit' again."),
      title = "Submitted work."
    ))
  })
  observe({
    if (vfun(input$userID,  "i", input$password)) {
      output$download <- renderUI(downloadLink("get_submissions",
                                               label="Download submissions"))
    } else {
      output$download <- renderUI(tagList())
    }
  }
  )

  valid_id <<- reactive({
    if (!isTruthy(input$userID)) return(FALSE)
    vfun(input$userID, "u")
  })
  valid_password <<- reactive({
    if (!isTruthy(input$userID)) return(FALSE)
    if (!isTruthy(input$password)) return(FALSE)
    vfun(input$userID, "p", input$password)
  })

  observe({
    if (valid_id()) {
      if (valid_password())  {
        user_identification <- isolate(input$userID)
      } else {
        user_identification <- paste("Unauthenticated", isolate(input$userID))
      }
      #  create the event recorder once the user has logged in
      options(tutorial.event_recorder =
                make_a_recorder(storage_actions$write,
                              user_identification))
    }
  })

  output$login_message <-
    renderText({
      if (valid_id() && valid_password()) {
        return("Login SUCCESSFUL.")
      }
      if (valid_id()) {
        return("Valid ID but password not yet correct")
      }
      if (nchar(input$userID) > 1) {
        return("Invalid user ID")
      }
      return("Please log in.")

    })

  # Link to download submissions ...
  output$get_submissions <- downloadHandler(
    filename = function() {
      paste0("submissions-", Sys.Date(), ".csv")
    },
    content = storage_actions$read_submissions
  )
}
