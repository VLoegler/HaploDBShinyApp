#' Admin Panel Module
#'
#' User management: view all accounts, add new users, delete users.
#' Only accessible to admin users. Cannot delete the last admin account.
#'
#' @param id Module namespace ID
#' @param users_conn SQLite connection for users database
#' @param user_info Reactive returning logged-in user data.frame

admin_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(8, 4),
    card(
      card_header("User Accounts"),
      DT::dataTableOutput(ns("users_table"))
    ),
    card(
      card_header("Add New User"),
      textInput(ns("new_username"), "Username"),
      passwordInput(ns("new_password"), "Password"),
      passwordInput(ns("confirm_password"), "Confirm Password"),
      selectInput(ns("new_role"), "Role", choices = c("basic", "admin")),
      actionButton(ns("add_user_btn"), "Add User", class = "btn-primary w-100"),
      uiOutput(ns("add_feedback")),
      tags$hr(),
      card_header("Delete User"),
      uiOutput(ns("delete_ui")),
      uiOutput(ns("delete_feedback"))
    )
  )
}

admin_server <- function(id, users_conn, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    users_data <- reactiveVal(NULL)

    load_users <- function() {
      df <- DBI::dbGetQuery(users_conn, "SELECT id, username, role, created_at FROM users ORDER BY id")
      users_data(df)
    }

    observe({
      req(is_admin(user_info()))
      load_users()
    })

    output$users_table <- DT::renderDataTable({
      req(users_data())
      DT::datatable(users_data(), selection = "none", rownames = FALSE,
                    options = list(pageLength = 20, dom = "tip"))
    })

    # Add user
    observeEvent(input$add_user_btn, {
      req(is_admin(user_info()))

      username <- trimws(input$new_username)
      password <- input$new_password
      confirm <- input$confirm_password
      role <- input$new_role

      if (nchar(username) < 1) {
        output$add_feedback <- renderUI(
          tags$div(class = "alert alert-danger mt-2", "Username is required.")
        )
        return()
      }

      if (nchar(password) < 6) {
        output$add_feedback <- renderUI(
          tags$div(class = "alert alert-danger mt-2", "Password must be at least 6 characters.")
        )
        return()
      }

      if (password != confirm) {
        output$add_feedback <- renderUI(
          tags$div(class = "alert alert-danger mt-2", "Passwords do not match.")
        )
        return()
      }

      existing <- DBI::dbGetQuery(
        users_conn, "SELECT COUNT(*) AS n FROM users WHERE username = ?",
        params = list(username)
      )$n

      if (existing > 0) {
        output$add_feedback <- renderUI(
          tags$div(class = "alert alert-danger mt-2", "Username already exists.")
        )
        return()
      }

      hashed <- hash_password(password)
      DBI::dbExecute(
        users_conn,
        "INSERT INTO users (username, password_hash, role) VALUES (?, ?, ?)",
        params = list(username, hashed, role)
      )

      updateTextInput(session, "new_username", value = "")
      updateTextInput(session, "new_password", value = "")
      updateTextInput(session, "confirm_password", value = "")

      output$add_feedback <- renderUI(
        tags$div(class = "alert alert-success mt-2",
                 sprintf("User '%s' created successfully.", username))
      )
      load_users()
    })

    # Delete user UI
    output$delete_ui <- renderUI({
      req(users_data())
      choices <- setNames(users_data()$id, users_data()$username)
      tagList(
        selectInput(ns("delete_user_id"), "Select user to delete", choices = choices),
        actionButton(ns("delete_user_btn"), "Delete User",
                     class = "btn-danger w-100")
      )
    })

    observeEvent(input$delete_user_btn, {
      req(is_admin(user_info()))

      target_id <- as.integer(input$delete_user_id)
      target <- users_data() %>% filter(id == target_id)

      if (nrow(target) == 0) return()

      # Cannot delete the last admin
      if (target$role == "admin") {
        admin_count <- DBI::dbGetQuery(
          users_conn, "SELECT COUNT(*) AS n FROM users WHERE role = 'admin'"
        )$n
        if (admin_count <= 1) {
          output$delete_feedback <- renderUI(
            tags$div(class = "alert alert-danger mt-2",
                     "Cannot delete the last admin account.")
          )
          return()
        }
      }

      DBI::dbExecute(users_conn, "DELETE FROM users WHERE id = ?", params = list(target_id))

      output$delete_feedback <- renderUI(
        tags$div(class = "alert alert-success mt-2",
                 sprintf("User '%s' deleted.", target$username))
      )
      load_users()
    })
  })
}
