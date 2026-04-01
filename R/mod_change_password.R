#' Change Password Module
#'
#' Allows any logged-in user to change their own password.
#' Requires current password for verification before updating.
#'
#' @param id Module namespace ID
#' @param users_conn SQLite connection for users database
#' @param user_info Reactive returning logged-in user data.frame

change_password_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Change Password"),
      passwordInput(ns("current_password"), "Current Password"),
      passwordInput(ns("new_password"), "New Password"),
      passwordInput(ns("confirm_password"), "Confirm New Password"),
      tags$small(class = "text-muted", "Password must be at least 6 characters."),
      tags$br(), tags$br(),
      actionButton(ns("change_btn"), "Update Password",
                   class = "btn-primary w-100"),
      uiOutput(ns("feedback"))
    ),
    card(
      card_header("About Your Account"),
      uiOutput(ns("account_info"))
    )
  )
}

change_password_server <- function(id, users_conn, user_info) {
  moduleServer(id, function(input, output, session) {

    # Displaying account info
    output$account_info <- renderUI({
      req(user_info())
      u <- user_info()
      row <- DBI::dbGetQuery(
        users_conn,
        "SELECT username, role, created_at FROM users WHERE id = ?",
        params = list(u$id)
      )
      if (nrow(row) == 0) return(NULL)
      tags$dl(
        class = "row",
        tags$dt(class = "col-sm-4", "Username"),
        tags$dd(class = "col-sm-8", row$username),
        tags$dt(class = "col-sm-4", "Role"),
        tags$dd(class = "col-sm-8",
          tags$span(
            class = if (row$role == "admin") "badge bg-danger" else "badge bg-secondary",
            row$role
          )
        ),
        tags$dt(class = "col-sm-4", "Member since"),
        tags$dd(class = "col-sm-8",
          if (!is.na(row$created_at) && nzchar(row$created_at))
            format(as.POSIXct(row$created_at), "%B %d, %Y")
          else
            "—"
        )
      )
    })

    observeEvent(input$change_btn, {
      req(user_info())
      u <- user_info()
      current <- input$current_password
      new_pw <- input$new_password
      confirm <- input$confirm_password

      # Verifying current password
      row <- DBI::dbGetQuery(
        users_conn,
        "SELECT password_hash FROM users WHERE id = ?",
        params = list(u$id)
      )

      if (nrow(row) == 0 ||
          !sodium::password_verify(row$password_hash, as.character(current))) {
        output$feedback <- renderUI(
          tags$div(class = "alert alert-danger mt-3",
                   "Current password is incorrect.")
        )
        return()
      }

      if (nchar(new_pw) < 6) {
        output$feedback <- renderUI(
          tags$div(class = "alert alert-danger mt-3",
                   "New password must be at least 6 characters.")
        )
        return()
      }

      if (new_pw != confirm) {
        output$feedback <- renderUI(
          tags$div(class = "alert alert-danger mt-3",
                   "New passwords do not match.")
        )
        return()
      }

      if (new_pw == current) {
        output$feedback <- renderUI(
          tags$div(class = "alert alert-warning mt-3",
                   "New password must differ from the current one.")
        )
        return()
      }

      # Hashing and updating
      new_hash <- hash_password(new_pw)
      DBI::dbExecute(
        users_conn,
        "UPDATE users SET password_hash = ? WHERE id = ?",
        params = list(new_hash, u$id)
      )

      updateTextInput(session, "current_password", value = "")
      updateTextInput(session, "new_password",     value = "")
      updateTextInput(session, "confirm_password", value = "")

      output$feedback <- renderUI(
        tags$div(class = "alert alert-success mt-3",
                 icon("check-circle"), " Password updated successfully.")
      )
    })
  })
}
