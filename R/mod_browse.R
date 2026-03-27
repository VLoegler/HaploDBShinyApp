#' Browse Database Module
#'
#' Displays YJSnumbers LEFT JOIN Strains as a flat table with per-column filters,
#' column visibility toggle, global search, and TSV download.
#'
#' @param id Module namespace ID
#' @param main_conn Main SQLite DB connection
#' @param user_info Reactive returning logged-in user data.frame

browse_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#", ns("browse_table"),
      " thead td > div > input + span { display: none !important; }",
      "\n#", ns("col_select_panel"), " .checkbox-columns {",
      "  column-width: 200px;",
      "  column-gap: 1rem;",
      "}\n",
      "#", ns("col_select_panel"), " .checkbox-columns .checkbox {",
      "  break-inside: avoid;",
      "  margin-bottom: 2px;",
      "}"
    ))),
    tags$div(
      class = "d-flex justify-content-between align-items-center mb-3",
      tags$h5("Browse Database", class = "mb-0"),
      tags$div(
        class = "d-flex align-items-center gap-3",
        tags$div(
          class = "d-flex align-items-center gap-2",
          tags$span("Biological samples", style = "font-weight: 600;"),
          tags$div(
            class = "form-check form-switch mb-0",
            tags$input(type = "checkbox", class = "form-check-input",
                       id = ns("sample_toggle"), role = "switch")
          ),
          tags$span("Bioinformatics samples", style = "font-weight: 600;")
        ),
        tags$div(
          actionButton(ns("col_toggle_btn"), "Columns",
                       icon = icon("table-columns"),
                       class = "btn-sm btn-outline-info me-2"),
          actionButton(ns("refresh_btn"), "Refresh", icon = icon("sync"),
                       class = "btn-sm btn-outline-primary me-2"),
          downloadButton(ns("download_tsv"), "Download TSV",
                         class = "btn-sm btn-outline-secondary")
        )
      )
    ),
    tags$div(
      id = ns("col_select_panel"),
      style = "display: none;",
      class = "mb-3",
      bslib::card(
        bslib::card_body(
          class = "py-2 px-3",
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-2",
            tags$strong("Select columns to display"),
            tags$div(
              actionButton(ns("col_select_all"), "All",
                           class = "btn-sm btn-outline-secondary me-1"),
              actionButton(ns("col_reset"), "Reset",
                           class = "btn-sm btn-outline-primary")
            )
          ),
          uiOutput(ns("col_checkboxes"))
        )
      )
    ),
    DT::dataTableOutput(ns("browse_table"))
  )
}

browse_server <- function(id, main_conn, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    default_cols <- c("YJS_NUMBER", "SAMPLE_NAME", "SPECIES", "MATING_TYPE",
                      "BOX_NUMBER", "BOX_ROW", "BOX_COL",
                      "PLATE", "PLATE_ROW", "PLATE_COL",
                      "STOCKED_BY", "ID_STRAIN", "CLADE")

    browse_data <- reactiveVal(NULL)
    col_panel_visible <- reactiveVal(FALSE)

    load_data <- function() {
      query <- "
        SELECT y.*,
               s.ISOLATION, s.ECO_ORIGIN AS STRAIN_ECO_ORIGIN,
               s.GEO_ORIGIN, s.CONTINENT, s.COUNTRY, s.CLADE,
               s.SRR_ID, s.SPECIES AS STRAIN_SPECIES
        FROM YJSnumbers y
        LEFT JOIN Strains s ON y.ID_STRAIN = s.STRAIN
      "
      df <- DBI::dbGetQuery(main_conn, query)
      browse_data(df)
    }

    observe({
      req(user_info())
      load_data()
    })

    observeEvent(input$refresh_btn, {
      load_data()
    })

    # Column selector panel toggle
    observeEvent(input$col_toggle_btn, {
      col_panel_visible(!col_panel_visible())
      shinyjs::toggle("col_select_panel", anim = TRUE, animType = "slide",
                       time = 0.3)
    })

    # Render checkboxes when data is loaded
    output$col_checkboxes <- renderUI({
      req(browse_data())
      all_cols <- names(browse_data())
      selected <- if (!is.null(input$col_select)) input$col_select else default_cols
      tags$div(
        class = "checkbox-columns",
        checkboxGroupInput(ns("col_select"), label = NULL,
                           choices = all_cols, selected = selected)
      )
    })

    observeEvent(input$col_select_all, {
      req(browse_data())
      updateCheckboxGroupInput(session, "col_select",
                               selected = names(browse_data()))
    })

    observeEvent(input$col_reset, {
      updateCheckboxGroupInput(session, "col_select", selected = default_cols)
    })

    filtered_data <- reactive({
      req(browse_data())
      df <- browse_data()
      if (isTRUE(input$sample_toggle)) {
        df[grepl("^XTRA", df$YJS_NUMBER), , drop = FALSE]
      } else {
        df[grepl("^YJS", df$YJS_NUMBER), , drop = FALSE]
      }
    })

    display_data <- reactive({
      req(filtered_data())
      selected <- input$col_select
      all_cols <- names(filtered_data())
      if (is.null(selected)) {
        show <- intersect(all_cols, default_cols)
      } else {
        show <- intersect(all_cols, selected)
      }
      if (length(show) == 0) show <- all_cols
      filtered_data()[, show, drop = FALSE]
    })

    output$browse_table <- DT::renderDataTable({
      req(display_data())
      df <- display_data()
      cols <- names(df)
      for (sc in c("SPECIES", "STRAIN_SPECIES")) {
        if (sc %in% cols) {
          df[[sc]] <- ifelse(
            is.na(df[[sc]]) | df[[sc]] == "",
            df[[sc]],
            paste0("<em>", htmltools::htmlEscape(df[[sc]]), "</em>")
          )
        }
      }
      num_cols <- names(df)[sapply(df, is.numeric)]
      for (col in num_cols) df[[col]] <- as.character(df[[col]])
      species_col_idx <- which(cols %in% c("SPECIES", "STRAIN_SPECIES"))
      DT::datatable(
        df,
        filter = "top",
        rownames = FALSE,
        escape = setdiff(seq_along(cols), species_col_idx),
        extensions = "Scroller",
        options = list(
          scrollX = TRUE,
          scrollY = "calc(100vh - 260px)",
          scroller = TRUE,
          deferRender = TRUE,
          dom = "frtip"
        )
      )
    })

    output$download_tsv <- downloadHandler(
      filename = function() {
        paste0("haplodb_browse_", format(Sys.Date(), "%Y%m%d"), ".tsv")
      },
      content = function(file) {
        req(filtered_data())
        filtered_rows <- input$browse_table_rows_all
        df <- filtered_data()
        if (!is.null(filtered_rows)) {
          df <- df[filtered_rows, , drop = FALSE]
        }
        write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
  })
}
