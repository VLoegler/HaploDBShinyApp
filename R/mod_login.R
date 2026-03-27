#' Login Module
#'
#' Provides a modal-based login screen with logo and brand colors.
#' Uses shinyauthr for session management.
#'
#' @param id Module namespace ID
#' @param users_conn SQLite connection for users database

#' Login modal UI (shown as a modal dialog)
#'
#' @param id Module namespace ID
login_modal_ui <- function(id) {
  ns <- NS(id)
  modalDialog(
    title = NULL,
    size = "s",
    easyClose = FALSE,
    footer = NULL,
    tags$div(
      class = "login-container",
      tags$img(src = "haplologo.webp", class = "login-logo"),
      tags$h4("Sign in to HaploDB", class = "text-center mb-4",
              style = "color: #1B2A4A;"),
      textInput(ns("username"), "Username", placeholder = "Enter username"),
      passwordInput(ns("password"), "Password", placeholder = "Enter password"),
      tags$br(),
      actionButton(ns("login_btn"), "Sign In",
                   class = "btn btn-primary w-100",
                   style = "background-color: #2E6DAE; border-color: #2E6DAE;"),
      uiOutput(ns("login_error"))
    )
  )
}

#' Login server logic
#'
#' @param id Module namespace ID
#' @param users_conn SQLite connection for users database
#' @return Reactive list with logged_in (logical) and info (user data.frame)
login_server <- function(id, users_conn) {
  moduleServer(id, function(input, output, session) {
    credentials_rv <- reactiveValues(logged_in = FALSE, info = NULL, token = NULL)

    # Restore session from browser sessionStorage on page refresh
    observeEvent(input$restored_token, {
      if (!credentials_rv$logged_in) {
        user <- retrieve_session_token(input$restored_token)
        if (!is.null(user)) {
          credentials_rv$logged_in <- TRUE
          credentials_rv$info <- user
          credentials_rv$token <- input$restored_token
        }
      }
    }, once = TRUE)

    observeEvent(input$login_btn, {
      user <- check_credentials(input$username, input$password, users_conn)
      if (!is.null(user)) {
        token <- generate_session_token()
        store_session_token(token, user)
        credentials_rv$logged_in <- TRUE
        credentials_rv$info <- user
        credentials_rv$token <- token
        session$sendCustomMessage("save_session_token", token)
      } else {
        output$login_error <- renderUI({
          tags$div(
            class = "alert alert-danger mt-3",
            role = "alert",
            "Invalid username or password."
          )
        })
      }
    })

    reactive(list(
      logged_in = credentials_rv$logged_in,
      info = credentials_rv$info,
      token = credentials_rv$token
    ))
  })
}
