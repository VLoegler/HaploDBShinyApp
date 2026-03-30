library(shiny)
library(bslib)
library(DT)
library(DBI)
library(RSQLite)
library(sodium)
library(config)
library(dplyr)
library(shinyjs)
library(colourpicker)
library(ggtree)

# Load configuration
app_config <- config::get(file = "config.yml")

# Database connections
main_conn <- create_db_conn(app_config$database$main_path)
pending_conn <- create_db_conn(app_config$database$pending_path)
users_conn <- create_db_conn(app_config$database$users_path)

# Ensure tables exist and seed admin
ensure_users_table(users_conn)
ensure_pending_tables(pending_conn)
seed_default_admin(users_conn, app_config$admin)

# Clean up on app stop
onStop(function() {
  DBI::dbDisconnect(main_conn)
  DBI::dbDisconnect(pending_conn)
  DBI::dbDisconnect(users_conn)
})

# Theme
app_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#1B2A4A",
  primary = "#2E6DAE",
  secondary = "#6C757D",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  font_scale = 0.95
)

# UI
ui <- fluidPage(
  theme = app_theme,
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        if (!window._haplodb_handlers) {
          Shiny.addCustomMessageHandler('save_session_token', function(token) {
            sessionStorage.setItem('haplodb_token', token);
          });
          Shiny.addCustomMessageHandler('clear_session_token', function(msg) {
            sessionStorage.removeItem('haplodb_token');
            sessionStorage.removeItem('haplodb_tab');
          });
          Shiny.addCustomMessageHandler('updateBadge', function(msg) {
            var $link = $('a[data-value=\"' + msg.tab + '\"]');
            var $badge = $link.find('.tab-badge');
            if (msg.count > 0) {
              if ($badge.length === 0) {
                $link.append(' <span class=\"badge bg-danger tab-badge\">' + msg.count + '</span>');
              } else {
                $badge.text(msg.count);
              }
            } else {
              $badge.remove();
            }
          });
          window._haplodb_handlers = true;
        }
        var token = sessionStorage.getItem('haplodb_token');
        if (token) {
          Shiny.setInputValue('login-restored_token', token, {priority: 'event'});
        }
        var tab = sessionStorage.getItem('haplodb_tab');
        if (tab) {
          Shiny.setInputValue('restored_tab', tab, {priority: 'event'});
        }
      });
      $(document).on('keypress', '#login-username, #login-password', function(e) {
        if (e.which === 13) {
          $('#login-username').trigger('change');
          $('#login-password').trigger('change');
          setTimeout(function() { $('#login-login_btn').click(); }, 50);
        }
      });
      $(document).on('shiny:inputchanged', function(e) {
        if (e.name === 'main_navbar') {
          sessionStorage.setItem('haplodb_tab', e.value);
        }
      });
    "))
  ),
  uiOutput("main_ui")
)

# Server
server <- function(input, output, session) {
  # Authentication
  credentials <- login_server("login", users_conn)

  user_info <- reactive({
    creds <- credentials()
    if (isTRUE(creds$logged_in)) creds$info else NULL
  })

  # Render the full page based on auth state
  output$main_ui <- renderUI({
    logged_in <- isTRUE(credentials()$logged_in)

    if (!logged_in) {
      # Login page
      tags$div(
        style = "min-height: 100vh; background: #F5F6F8; display: flex; align-items: center; justify-content: center;",
        card(
          style = "width: 380px;",
          tags$div(
            class = "text-center p-3",
            tags$img(src = "haplologo.webp", style = "max-width: 180px; margin-bottom: 1rem;"),
            tags$h4("Sign in to HaploDB", style = "color: #1B2A4A; margin-bottom: 1.5rem;"),
            textInput("login-username", "Username", placeholder = "Enter username"),
            passwordInput("login-password", "Password", placeholder = "Enter password"),
            tags$br(),
            actionButton("login-login_btn", "Sign In",
                         class = "btn btn-primary w-100",
                         style = "background-color: #2E6DAE; border-color: #2E6DAE;"),
            uiOutput("login-login_error")
          )
        )
      )
    } else {
      admin <- is_admin(user_info())

      tabs <- list(
        nav_panel("Home", value = "home", icon = icon("home"),
                  home_ui("home")),
        nav_panel("Browse", value = "browse", icon = icon("database"),
                  browse_ui("browse")),
        nav_panel("Add Entries", value = "add_entry", icon = icon("plus-circle"),
                  add_entry_ui("add_entry")),
        nav_panel(title = tags$span(icon("inbox"), "My Submissions"),
                  value = "submissions",
                  notifications_ui("notifications")),
        nav_panel("Account", value = "account", icon = icon("user-circle"),
                  change_password_ui("change_password"))
      )

      if (admin) {
        tabs <- c(tabs, list(
          nav_panel("Review", value = "review", icon = icon("check-circle"),
                    review_ui("review"))
        ))
      }

      tabs <- c(tabs, list(
        nav_panel("Tree Plot", value = "tree", icon = icon("tree"),
                  tree_ui("tree")),
        nav_panel("Name Conversion", value = "nameconv", icon = icon("exchange-alt"),
                  nameconv_ui("nameconv"))
      ))

      if (admin) {
        tabs <- c(tabs, list(
          nav_panel("Admin", value = "admin", icon = icon("users-cog"),
                    admin_ui("admin"))
        ))
      }

      tabs <- c(tabs, list(
        nav_spacer(),
        nav_item(tags$span(
          class = "navbar-text me-2",
          style = "color: white;",
          icon("user"), " ", user_info()$username
        )),
        nav_item(
          actionButton("logout_btn", "Logout", icon = icon("sign-out-alt"),
                       class = "btn btn-sm btn-outline-light")
        )
      ))

      do.call(page_navbar, c(
        list(
          id = "main_navbar",
          title = tags$img(src = "HaploDB.png", class = "navbar-logo"),
          theme = app_theme
        ),
        tabs
      ))
    }
  })

  # Logout
  observeEvent(input$logout_btn, {
    token <- credentials()$token
    if (!is.null(token)) remove_session_token(token)
    session$sendCustomMessage("clear_session_token", TRUE)
    session$reload()
  })

  # Restore active tab after session refresh
  observeEvent(credentials()$logged_in, {
    if (isTRUE(credentials()$logged_in)) {
      tab <- isolate(input$restored_tab)
      if (!is.null(tab) && nzchar(tab)) {
        session$onFlushed(function() {
          updateTabsetPanel(session, "main_navbar", selected = tab)
        })
      }
    }
  }, once = TRUE)

  # Navigation helper for module quick actions
  navigate <- function(tab) {
    updateTabsetPanel(session, "main_navbar", selected = tab)
  }

  # Module servers
  home_server("home", main_conn, user_info, navigate)
  browse_server("browse", main_conn, user_info)
  nameconv_server("nameconv", main_conn, user_info)
  add_entry_server("add_entry", main_conn, pending_conn, user_info)
  review_count <- review_server("review", main_conn, pending_conn, user_info)
  active_tab <- reactive(input$main_navbar)
  unread_count <- notifications_server("notifications", main_conn, pending_conn, user_info, active_tab)
  tree_server("tree", user_info)
  admin_server("admin", users_conn, user_info)
  change_password_server("change_password", users_conn, user_info)

  # Badge updates (req ensures navbar DOM exists before sending)
  observe({
    req(input$main_navbar)
    count <- unread_count()
    session$sendCustomMessage("updateBadge",
      list(tab = "submissions", count = count))
  })
  observe({
    req(input$main_navbar)
    count <- review_count()
    session$sendCustomMessage("updateBadge",
      list(tab = "review", count = count))
  })
}

shinyApp(ui, server)
