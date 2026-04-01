home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "home-container",
      tags$div(
        class = "text-center mb-4",
        tags$img(src = "haplologo.webp", class = "home-logo"),
        tags$h2("Welcome to HaploDB",
                 style = "color: var(--bs-body-color); font-weight: 700;"),
        tags$p("Yeast strain and sample database",
               style = "color: var(--bs-secondary-color); font-size: 1.1rem;")
      ),
      uiOutput(ns("stat_cards")),
      tags$div(
        class = "text-center mt-4",
        tags$h5("Quick Actions",
                 style = "color: var(--bs-body-color); margin-bottom: 1rem;"),
        tags$div(
          class = "d-flex justify-content-center gap-3 flex-wrap",
          actionButton(ns("go_browse"), "Browse Database",
                       icon = icon("database"),
                       class = "btn btn-outline-primary btn-lg"),
          actionButton(ns("go_add"), "Add Entries",
                       icon = icon("plus-circle"),
                       class = "btn btn-outline-primary btn-lg"),
          actionButton(ns("go_tree"), "Tree Plot",
                       icon = icon("tree"),
                       class = "btn btn-outline-primary btn-lg")
        )
      )
    )
  )
}

home_server <- function(id, main_conn, user_info, navigate) {
  moduleServer(id, function(input, output, session) {

    output$stat_cards <- renderUI({
      req(user_info())

      n_yjs <- DBI::dbGetQuery(
        main_conn,
        "SELECT COUNT(*) AS n FROM YJSnumbers
         WHERE YJS_NUMBER LIKE 'YJS%'"
      )$n
      n_strains <- DBI::dbGetQuery(
        main_conn, "SELECT COUNT(*) AS n FROM Strains"
      )$n
      n_species <- DBI::dbGetQuery(
        main_conn,
        "SELECT COUNT(DISTINCT SPECIES) AS n FROM YJSnumbers
         WHERE SPECIES IS NOT NULL AND SPECIES != ''"
      )$n

      tags$div(
        class = "row justify-content-center g-3",
        stat_card("YJS Samples", n_yjs, "vial", "#2E6DAE"),
        stat_card("Strains", n_strains, "dna", "#5B9BD5"),
        stat_card("Species", n_species, "leaf", "#27AE60")
      )
    })

    observeEvent(input$go_browse, navigate("browse"))
    observeEvent(input$go_add, navigate("add_entry"))
    observeEvent(input$go_tree, navigate("tree"))
  })
}

stat_card <- function(label, value, icon_name, color) {
  tags$div(
    class = "col-auto",
    tags$div(
      class = "stat-card text-center",
      style = paste0("border-top: 4px solid ", color, ";"),
      tags$div(
        icon(icon_name),
        style = paste0("font-size: 1.8rem; color: ", color, ";
                        margin-bottom: 0.5rem;")
      ),
      tags$h3(format(value, big.mark = ","),
              style = "margin: 0; font-weight: 700; color: var(--bs-body-color);"),
      tags$p(label, style = "margin: 0; color: var(--bs-secondary-color); font-size: 0.9rem;")
    )
  )
}
