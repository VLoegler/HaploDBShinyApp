#' Add Entry Module
#'
#' Provides sub-tabs for adding new entries to the database.
#' v1: YJS Samples and Strains. Rows go to SQLite pending tables for admin review.
#'
#' @param id Module namespace ID
#' @param main_conn Main SQLite DB connection
#' @param pending_conn SQLite connection for pending submissions
#' @param user_info Reactive returning logged-in user data.frame
#' @param species_list Character vector of allowed species

# -- Validation helpers (pure functions, testable) --

#' Validate YJS sample rows
#'
#' @param df Data frame of YJS rows to validate
#' @param main_conn Main SQLite DB connection
#' @param pending_conn SQLite connection
#' @return List with valid (logical) and errors (character vector)
validate_yjs <- function(df, main_conn, pending_conn) {
  errors <- character(0)

  existing_strains <- DBI::dbGetQuery(
    main_conn, "SELECT STRAIN FROM Strains"
  )$STRAIN
  pending_s <- DBI::dbGetQuery(
    pending_conn, "SELECT STRAIN FROM pending_strains"
  )$STRAIN
  all_strains <- c(existing_strains, pending_s)

  for (i in seq_len(nrow(df))) {
    row_prefix <- sprintf("Row %d: ", i)

    if (is.na(df$SAMPLE_NAME[i]) ||
        nchar(trimws(as.character(df$SAMPLE_NAME[i]))) == 0) {
      errors <- c(errors,
        paste0(row_prefix, "SAMPLE_NAME is required."))
    }

    has_box <- !is.na(df$BOX_NUMBER[i])
    has_plate <- !is.na(df$PLATE[i]) &&
                 !is.na(df$PLATE_ROW[i]) &&
                 !is.na(df$PLATE_COL[i])
    if (!has_box && !has_plate) {
      errors <- c(errors,
        paste0(row_prefix,
          "At least BOX_NUMBER or PLATE location ",
          "(PLATE + PLATE_ROW + PLATE_COL) is required."))
    }

    if (is.na(df$STOCKED_BY[i]) ||
        nchar(trimws(as.character(df$STOCKED_BY[i]))) == 0) {
      errors <- c(errors,
        paste0(row_prefix, "STOCKED_BY is required."))
    }

    if (!is.na(df$PLOIDY[i]) &&
        nchar(trimws(df$PLOIDY[i])) > 0 &&
        !grepl("^\\d+n$", df$PLOIDY[i])) {
      errors <- c(errors,
        paste0(row_prefix, "Invalid PLOIDY '",
               df$PLOIDY[i], "'. Expected format: <int>n"))
    }

    if (!is.na(df$ID_STRAIN[i]) &&
        nchar(trimws(df$ID_STRAIN[i])) > 0 &&
        !(df$ID_STRAIN[i] %in% all_strains)) {
      errors <- c(errors,
        paste0(row_prefix, "ID_STRAIN '",
               df$ID_STRAIN[i], "' not found."))
    }
  }

  list(valid = length(errors) == 0, errors = errors)
}

#' Validate strain rows
#'
#' @param df Data frame of strain rows to validate
#' @param main_conn Main SQLite DB connection
#' @param pending_conn SQLite connection
#' @return List with valid (logical) and errors (character vector)
validate_strains <- function(df, main_conn, pending_conn) {
  errors <- character(0)

  existing_strains <- DBI::dbGetQuery(
    main_conn, "SELECT STRAIN FROM Strains"
  )$STRAIN
  pending_s <- DBI::dbGetQuery(
    pending_conn, "SELECT STRAIN FROM pending_strains"
  )$STRAIN
  all_strains <- c(existing_strains, pending_s)

  for (i in seq_len(nrow(df))) {
    row_prefix <- sprintf("Row %d: ", i)

    if (df$STRAIN[i] %in% all_strains) {
      errors <- c(errors,
        paste0(row_prefix, "STRAIN '",
               df$STRAIN[i], "' already exists."))
    }
  }

  list(valid = length(errors) == 0, errors = errors)
}

#' Get next XTRA_ strain name by incrementing the 3-letter code
#'
#' @param main_conn Main SQLite DB connection
#' @param pending_conn SQLite connection
#' @param count Number of names to generate
#' @return Character vector of XTRA_XXX names
next_xtra_names <- function(main_conn, pending_conn, count = 1, extra_names = character(0)) {
  db_last <- DBI::dbGetQuery(
    main_conn,
    "SELECT STRAIN FROM Strains WHERE STRAIN LIKE 'XTRA_%' ORDER BY STRAIN DESC LIMIT 1"
  )$STRAIN

  sqlite_last <- DBI::dbGetQuery(
    pending_conn,
    "SELECT STRAIN FROM pending_strains WHERE STRAIN LIKE 'XTRA_%' ORDER BY STRAIN DESC LIMIT 1"
  )$STRAIN

  candidates <- c(db_last, sqlite_last, extra_names)
  if (length(candidates) == 0) {
    last_name <- "XTRA_AAA"
    idx1 <- 1; idx2 <- 1; idx3 <- 0
  } else {
    last_name <- sort(candidates, decreasing = TRUE)[1]
    idx1 <- match(substr(last_name, 6, 6), LETTERS)
    idx2 <- match(substr(last_name, 7, 7), LETTERS)
    idx3 <- match(substr(last_name, 8, 8), LETTERS)
  }

  names <- character(count)
  for (i in seq_len(count)) {
    if (idx3 < 26) {
      idx3 <- idx3 + 1
    } else if (idx2 < 26) {
      idx3 <- 1
      idx2 <- idx2 + 1
    } else {
      idx3 <- 1
      idx2 <- 1
      idx1 <- idx1 + 1
    }
    names[i] <- paste0("XTRA_", LETTERS[idx1], LETTERS[idx2], LETTERS[idx3])
  }
  names
}

# YJS column definitions for the form and template
yjs_columns <- function() {
  c("SAMPLE_NAME", "SPECIES", "MATING_TYPE", "PLOIDY",
    "GENOTYPE", "SPORULATION", "EXTERNAL_ORIGIN", "ECO_ORIGIN",
    "COMMENTS_ORIGIN", "PARENTAL_ORIGIN", "PUBLICATION",
    "STRAINS_GROUP", "OLD_BOX", "BOX_NUMBER", "BOX_ROW", "BOX_COL",
    "PLATE", "PLATE_ROW", "PLATE_COL",
    "NOTES", "STOCKED_BY", "COMMENTS", "ID_STRAIN",
    "SAMPLE_TYPE", "COLLECTION")
}

strain_columns <- function() {
  c("STRAIN_NAME", "ISOLATION", "ECO_ORIGIN", "GEO_ORIGIN",
    "CONTINENT", "COUNTRY", "CLADE", "SRR_ID", "SPECIES")
}

# -- UI --

add_entry_ui <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    id = ns("subtab"),
    title = "Add Entries",
    nav_panel("YJS Samples", value = "yjs",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Add YJS Sample"),
          radioButtons(ns("yjs_mode"), "Input Mode",
                       choices = c("Manual" = "manual", "File Upload" = "file"),
                       inline = TRUE),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("yjs_mode")),
            tags$div(
              class = "alert alert-info py-2 mb-3",
              icon("info-circle"),
              "YJS numbers are automatically assigned upon approval."
            ),
            textInput(ns("yjs_sample_name"), "Sample Name *"),
            textInput(ns("yjs_stocked_by"), "Stocked By *"),
            tags$p(
              style = "color: #0d6efd; margin-bottom: 0.5rem;",
              "At least box number or plate location (Plate + Row + Col) must be filled."
            ),
            textInput(ns("yjs_box_number"), "Box Number"),
            textInput(ns("yjs_box_row"), "Box Row"),
            textInput(ns("yjs_box_col"), "Box Column"),
            textInput(ns("yjs_plate"), "Plate"),
            textInput(ns("yjs_plate_row"), "Plate Row"),
            textInput(ns("yjs_plate_col"), "Plate Column"),
            selectizeInput(
              ns("yjs_species"), "Species", choices = c(""),
              options = list(
                create = TRUE,
                render = I("{
                  option: function(item, escape) {
                    return '<div><em>' + escape(item.label) + '</em></div>';
                  },
                  item: function(item, escape) {
                    return '<div><em>' + escape(item.label) + '</em></div>';
                  }
                }")
              )
            ),
            selectizeInput(
              ns("yjs_mating_type"), "Mating Type",
              choices = c("", "MATa/alpha", "MATa", "MATalpha"),
              options = list(create = TRUE)
            ),
            selectizeInput(
              ns("yjs_ploidy"), HTML("Ploidy <small class='text-muted'>(format: Nn, e.g. 2n)</small>"),
              choices = c("", "1n", "2n", "3n", "4n", "5n", "6n"),
              options = list(create = TRUE)
            ),
            textInput(ns("yjs_genotype"), "Genotype"),
            textInput(ns("yjs_sporulation"), "Sporulation"),
            textInput(ns("yjs_external_origin"), "External Origin"),
            textInput(ns("yjs_eco_origin"), "Ecological Origin"),
            textInput(ns("yjs_comments_origin"), "Comments Origin"),
            textInput(ns("yjs_parental_origin"), "Parental Origin"),
            textInput(ns("yjs_publication"), "Publication"),
            textInput(ns("yjs_strains_group"), "Strains Group"),
            textInput(ns("yjs_old_box"), "Old Box"),
            textInput(ns("yjs_notes"), "Notes"),
            textInput(ns("yjs_comments"), "Comments"),
            textInput(ns("yjs_id_strain"), "ID Strain (must exist)"),
            textInput(ns("yjs_sample_type"), "Sample Type"),
            selectizeInput(
              ns("yjs_collection"), "Collection", choices = c(""),
              options = list(create = TRUE)
            )
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'file'", ns("yjs_mode")),
            fileInput(ns("yjs_file"), "Upload TSV/CSV file",
                      accept = c(".tsv", ".csv", ".txt")),
            tags$small(
              class = "text-muted",
              "YJS numbers are auto-assigned on approval.",
              "Any YJS_NUMBER column in the file will be ignored."
            )
          ),
          tags$hr(),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("yjs_mode")),
            actionButton(ns("yjs_add_btn"), "Add to List", class = "btn-outline-primary me-2"),
            actionButton(ns("yjs_update_btn"), "Update Selected", class = "btn-outline-warning me-2")
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'file'", ns("yjs_mode")),
            actionButton(ns("yjs_preview_btn"), "Preview", class = "btn-outline-primary me-2")
          ),
          actionButton(ns("yjs_submit_btn"), "Submit", class = "btn-primary"),
          tags$div(class = "mt-2",
            downloadButton(ns("yjs_template"), "Download Template", class = "btn-sm btn-outline-secondary")
          )
        ),
        card(
          card_header("Submission List"),
          uiOutput(ns("yjs_feedback")),
          DT::dataTableOutput(ns("yjs_preview_table")),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("yjs_mode")),
            tags$div(class = "mt-2",
              actionButton(ns("yjs_remove_btn"), "Remove Selected", class = "btn-outline-danger btn-sm")
            )
          )
        )
      )
    ),
    nav_panel("Strains", value = "strains",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Add Strain"),
          radioButtons(ns("strain_mode"), "Input Mode",
                       choices = c("Manual" = "manual", "File Upload" = "file"),
                       inline = TRUE),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("strain_mode")),
            textInput(ns("strain_name"), HTML("Strain Name (e.g., RM11) &mdash; Standardized ID will be automatically generated *")),
            textInput(ns("strain_isolation"), "Isolation"),
            textInput(ns("strain_eco_origin"), "Ecological Origin"),
            textInput(ns("strain_geo_origin"), "Geographic Origin"),
            textInput(ns("strain_continent"), "Continent"),
            textInput(ns("strain_country"), "Country"),
            textInput(ns("strain_clade"), "Clade"),
            textInput(ns("strain_srr_id"), "SRR ID"),
            selectizeInput(
              ns("strain_species"), "Species", choices = c(""),
              options = list(
                create = TRUE,
                render = I("{
                  option: function(item, escape) {
                    return '<div><em>' + escape(item.label) + '</em></div>';
                  },
                  item: function(item, escape) {
                    return '<div><em>' + escape(item.label) + '</em></div>';
                  }
                }")
              )
            )
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'file'", ns("strain_mode")),
            fileInput(ns("strain_file"), "Upload TSV/CSV file",
                      accept = c(".tsv", ".csv", ".txt")),
            tags$small(class = "text-muted",
                       "File must have a STRAIN_NAME column. XTRA_XXX names are auto-assigned.")
          ),
          tags$hr(),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("strain_mode")),
            actionButton(ns("strain_add_btn"), "Add to List", class = "btn-outline-primary me-2"),
            actionButton(ns("strain_update_btn"), "Update Selected", class = "btn-outline-warning me-2")
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'file'", ns("strain_mode")),
            actionButton(ns("strain_preview_btn"), "Preview", class = "btn-outline-primary me-2")
          ),
          actionButton(ns("strain_submit_btn"), "Submit", class = "btn-primary"),
          tags$div(class = "mt-2",
            downloadButton(ns("strain_template"), "Download Template", class = "btn-sm btn-outline-secondary")
          )
        ),
        card(
          card_header("Submission List"),
          uiOutput(ns("strain_feedback")),
          DT::dataTableOutput(ns("strain_preview_table")),
          conditionalPanel(
            sprintf("input['%s'] == 'manual'", ns("strain_mode")),
            tags$div(class = "mt-2",
              actionButton(ns("strain_remove_btn"), "Remove Selected", class = "btn-outline-danger btn-sm")
            )
          )
        )
      )
    )
  )
}

# -- Server --

add_entry_server <- function(id, main_conn, pending_conn, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dynamic choices for species, ploidy, mating type
    get_species <- function() {
      c("", get_field_choices(pending_conn, main_conn, "species"))
    }
    get_ploidy <- function() {
      c("", get_field_choices(pending_conn, main_conn, "ploidy"))
    }
    get_mating <- function() {
      c("", get_field_choices(pending_conn, main_conn, "mating_type"))
    }
    get_collection <- function() {
      c("", get_field_choices(pending_conn, main_conn, "collection"))
    }

    refresh_choices <- function() {
      updateSelectizeInput(
        session, "yjs_species",
        choices = get_species(), server = FALSE
      )
      updateSelectizeInput(
        session, "yjs_ploidy",
        choices = get_ploidy(), server = FALSE
      )
      updateSelectizeInput(
        session, "yjs_mating_type",
        choices = get_mating(), server = FALSE
      )
      updateSelectizeInput(
        session, "yjs_collection",
        choices = get_collection(), server = FALSE
      )
      updateSelectizeInput(
        session, "strain_species",
        choices = get_species(), server = FALSE
      )
    }

    observe({
      req(input$subtab)
      refresh_choices()
    })

    # ---- YJS Samples ----

    yjs_preview_data <- reactiveVal(NULL)
    yjs_list_data <- reactiveVal(data.frame())

    # Helper to parse text input as integer (NA if empty or non-numeric)
    parse_int <- function(x) {
      if (is.null(x) || trimws(x) == "") return(NA_integer_)
      val <- suppressWarnings(as.integer(x))
      val
    }

    build_yjs_manual <- function() {
      data.frame(
        SAMPLE_NAME = trimws(input$yjs_sample_name),
        SPECIES = na_if_empty(input$yjs_species),
        MATING_TYPE = na_if_empty(input$yjs_mating_type),
        PLOIDY = na_if_empty(input$yjs_ploidy),
        GENOTYPE = na_if_empty(input$yjs_genotype),
        SPORULATION = na_if_empty(input$yjs_sporulation),
        EXTERNAL_ORIGIN = na_if_empty(input$yjs_external_origin),
        ECO_ORIGIN = na_if_empty(input$yjs_eco_origin),
        COMMENTS_ORIGIN = na_if_empty(input$yjs_comments_origin),
        PARENTAL_ORIGIN = na_if_empty(input$yjs_parental_origin),
        PUBLICATION = na_if_empty(input$yjs_publication),
        STRAINS_GROUP = na_if_empty(input$yjs_strains_group),
        OLD_BOX = parse_int(input$yjs_old_box),
        BOX_NUMBER = parse_int(input$yjs_box_number),
        BOX_ROW = parse_int(input$yjs_box_row),
        BOX_COL = parse_int(input$yjs_box_col),
        PLATE = parse_int(input$yjs_plate),
        PLATE_ROW = parse_int(input$yjs_plate_row),
        PLATE_COL = parse_int(input$yjs_plate_col),
        NOTES = na_if_empty(input$yjs_notes),
        STOCKED_BY = trimws(input$yjs_stocked_by),
        COMMENTS = na_if_empty(input$yjs_comments),
        ID_STRAIN = na_if_empty(input$yjs_id_strain),
        SAMPLE_TYPE = na_if_empty(input$yjs_sample_type),
        COLLECTION = na_if_empty(input$yjs_collection),
        stringsAsFactors = FALSE
      )
    }

    reset_yjs_form <- function() {
      updateTextInput(session, "yjs_sample_name", value = "")
      updateSelectizeInput(
        session, "yjs_species",
        choices = get_species(), selected = "", server = FALSE
      )
      updateSelectizeInput(
        session, "yjs_mating_type",
        choices = get_mating(), selected = "", server = FALSE
      )
      updateSelectizeInput(
        session, "yjs_ploidy",
        choices = get_ploidy(), selected = "", server = FALSE
      )
      updateTextInput(session, "yjs_genotype", value = "")
      updateTextInput(session, "yjs_sporulation", value = "")
      updateTextInput(session, "yjs_external_origin", value = "")
      updateTextInput(session, "yjs_eco_origin", value = "")
      updateTextInput(session, "yjs_comments_origin", value = "")
      updateTextInput(session, "yjs_parental_origin", value = "")
      updateTextInput(session, "yjs_publication", value = "")
      updateTextInput(session, "yjs_strains_group", value = "")
      updateTextInput(session, "yjs_old_box", value = "")
      updateTextInput(session, "yjs_box_number", value = "")
      updateTextInput(session, "yjs_box_row", value = "")
      updateTextInput(session, "yjs_box_col", value = "")
      updateTextInput(session, "yjs_plate", value = "")
      updateTextInput(session, "yjs_plate_row", value = "")
      updateTextInput(session, "yjs_plate_col", value = "")
      updateTextInput(session, "yjs_notes", value = "")
      updateTextInput(session, "yjs_stocked_by", value = "")
      updateTextInput(session, "yjs_comments", value = "")
      updateTextInput(session, "yjs_id_strain", value = "")
      updateTextInput(session, "yjs_sample_type", value = "")
      updateSelectizeInput(
        session, "yjs_collection",
        choices = get_collection(), selected = "", server = FALSE
      )
    }

    fill_yjs_form <- function(row) {
      updateTextInput(session, "yjs_sample_name",
                      value = na_to_empty(row$SAMPLE_NAME))
      sp <- get_species()
      sel_sp <- na_to_empty(row$SPECIES)
      if (nchar(sel_sp) > 0 && !(sel_sp %in% sp)) sp <- c(sp, sel_sp)
      updateSelectizeInput(session, "yjs_species",
                           choices = sp, selected = sel_sp,
                           server = FALSE)
      mt <- get_mating()
      sel_mt <- na_to_empty(row$MATING_TYPE)
      if (nchar(sel_mt) > 0 && !(sel_mt %in% mt)) mt <- c(mt, sel_mt)
      updateSelectizeInput(session, "yjs_mating_type",
                           choices = mt, selected = sel_mt,
                           server = FALSE)
      pl <- get_ploidy()
      sel_pl <- na_to_empty(row$PLOIDY)
      if (nchar(sel_pl) > 0 && !(sel_pl %in% pl)) pl <- c(pl, sel_pl)
      updateSelectizeInput(session, "yjs_ploidy",
                           choices = pl, selected = sel_pl,
                           server = FALSE)
      updateTextInput(session, "yjs_genotype", value = na_to_empty(row$GENOTYPE))
      updateTextInput(session, "yjs_sporulation", value = na_to_empty(row$SPORULATION))
      updateTextInput(session, "yjs_external_origin", value = na_to_empty(row$EXTERNAL_ORIGIN))
      updateTextInput(session, "yjs_eco_origin", value = na_to_empty(row$ECO_ORIGIN))
      updateTextInput(session, "yjs_comments_origin", value = na_to_empty(row$COMMENTS_ORIGIN))
      updateTextInput(session, "yjs_parental_origin", value = na_to_empty(row$PARENTAL_ORIGIN))
      updateTextInput(session, "yjs_publication", value = na_to_empty(row$PUBLICATION))
      updateTextInput(session, "yjs_strains_group", value = na_to_empty(row$STRAINS_GROUP))
      updateTextInput(session, "yjs_old_box", value = na_to_empty(row$OLD_BOX))
      updateTextInput(session, "yjs_box_number", value = na_to_empty(row$BOX_NUMBER))
      updateTextInput(session, "yjs_box_row", value = na_to_empty(row$BOX_ROW))
      updateTextInput(session, "yjs_box_col", value = na_to_empty(row$BOX_COL))
      updateTextInput(session, "yjs_plate", value = na_to_empty(row$PLATE))
      updateTextInput(session, "yjs_plate_row", value = na_to_empty(row$PLATE_ROW))
      updateTextInput(session, "yjs_plate_col", value = na_to_empty(row$PLATE_COL))
      updateTextInput(session, "yjs_notes", value = na_to_empty(row$NOTES))
      updateTextInput(session, "yjs_stocked_by", value = na_to_empty(row$STOCKED_BY))
      updateTextInput(session, "yjs_comments", value = na_to_empty(row$COMMENTS))
      updateTextInput(session, "yjs_id_strain", value = na_to_empty(row$ID_STRAIN))
      updateTextInput(session, "yjs_sample_type", value = na_to_empty(row$SAMPLE_TYPE))
      cl <- get_collection()
      sel_cl <- na_to_empty(row$COLLECTION)
      if (nchar(sel_cl) > 0 && !(sel_cl %in% cl)) cl <- c(cl, sel_cl)
      updateSelectizeInput(session, "yjs_collection",
                           choices = cl, selected = sel_cl,
                           server = FALSE)
    }

    build_yjs_from_file <- function() {
      req(input$yjs_file)
      file_path <- input$yjs_file$datapath
      ext <- tools::file_ext(input$yjs_file$name)

      df <- if (ext == "tsv" || ext == "txt") {
        read.delim(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      } else {
        read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      }

      expected <- yjs_columns()
      # Allow STRAIN_NAME as alias for ID_STRAIN
      if ("STRAIN_NAME" %in% names(df) && !("ID_STRAIN" %in% names(df))) {
        names(df)[names(df) == "STRAIN_NAME"] <- "ID_STRAIN"
      }

      # Drop YJS_NUMBER if present (auto-assigned on approval)
      if ("YJS_NUMBER" %in% names(df)) df$YJS_NUMBER <- NULL

      # Add missing columns as NA
      for (col in expected) {
        if (!col %in% names(df)) df[[col]] <- NA
      }
      df <- df[, expected, drop = FALSE]

      df
    }

    # Fill form when a row is selected in the table
    observeEvent(input$yjs_preview_table_rows_selected, {
      sel <- input$yjs_preview_table_rows_selected
      if (length(sel) == 1 && input$yjs_mode == "manual") {
        df <- yjs_list_data()
        if (sel >= 1 && sel <= nrow(df)) {
          fill_yjs_form(df[sel, ])
        }
      }
    })

    # Add to list (manual mode)
    observeEvent(input$yjs_add_btn, {
      tryCatch({
        df <- build_yjs_manual()
        result <- validate_yjs(df, main_conn, pending_conn)

        if (!result$valid) {
          output$yjs_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
          return()
        }

        current <- yjs_list_data()
        yjs_list_data(rbind(current, df))
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("Added '%s' to list. %d entry(ies) ready.",
                           df$SAMPLE_NAME[1], nrow(current) + 1))
        )
        reset_yjs_form()
      }, error = function(e) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    # Update selected entry (manual mode)
    observeEvent(input$yjs_update_btn, {
      sel <- input$yjs_preview_table_rows_selected
      if (length(sel) != 1) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-warning", "Select a row in the list to update.")
        )
        return()
      }

      tryCatch({
        df <- build_yjs_manual()
        current <- yjs_list_data()

        result <- validate_yjs(df, main_conn, pending_conn)
        if (!result$valid) {
          output$yjs_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
          return()
        }

        current[sel, ] <- df
        yjs_list_data(current)
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("Updated %s in list.", df$SAMPLE_NAME[1]))
        )
        reset_yjs_form()
      }, error = function(e) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    # Remove selected entry from YJS list
    observeEvent(input$yjs_remove_btn, {
      sel <- input$yjs_preview_table_rows_selected
      if (length(sel) != 1) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-warning", "Select a row in the list to remove.")
        )
        return()
      }
      current <- yjs_list_data()
      if (sel >= 1 && sel <= nrow(current)) {
        yjs_list_data(current[-sel, , drop = FALSE])
        reset_yjs_form()
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-info",
                   sprintf("Removed entry. %d entry(ies) in list.", nrow(current) - 1))
        )
      }
    })

    # Preview from file upload
    observeEvent(input$yjs_preview_btn, {
      tryCatch({
        df <- build_yjs_from_file()
        result <- validate_yjs(df, main_conn, pending_conn)
        yjs_preview_data(df)

        if (!result$valid) {
          output$yjs_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
        } else {
          output$yjs_feedback <- renderUI(
            tags$div(class = "alert alert-success",
                     sprintf("%d row(s) passed validation. Ready to submit.", nrow(df)))
          )
        }
      }, error = function(e) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    output$yjs_preview_table <- DT::renderDataTable({
      if (input$yjs_mode == "manual") {
        df <- yjs_list_data()
        req(nrow(df) > 0)
        DT::datatable(df, rownames = FALSE,
                      selection = list(mode = "single"),
                      options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
      } else {
        req(yjs_preview_data())
        DT::datatable(yjs_preview_data(), rownames = FALSE,
                      selection = "none",
                      options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
      }
    })

    observeEvent(input$yjs_submit_btn, {
      req(user_info())

      df <- if (input$yjs_mode == "manual") {
        yjs_list_data()
      } else {
        yjs_preview_data()
      }
      req(nrow(df) > 0)

      result <- validate_yjs(df, main_conn, pending_conn)

      if (!result$valid) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-danger",
                   tags$strong("Cannot submit — validation errors:"),
                   tags$ul(lapply(result$errors, tags$li)))
        )
        return()
      }

      df$YJS_NUMBER <- "PENDING"
      df$submitted_by <- user_info()$username
      df$submitted_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      tryCatch({
        # Save any new custom options
        for (val in unique(df$SPECIES[!is.na(df$SPECIES) & df$SPECIES != ""])) {
          save_custom_option(pending_conn, "species", val)
        }
        for (val in unique(df$PLOIDY[!is.na(df$PLOIDY) & df$PLOIDY != ""])) {
          save_custom_option(pending_conn, "ploidy", val)
        }
        for (val in unique(df$MATING_TYPE[!is.na(df$MATING_TYPE) & df$MATING_TYPE != ""])) {
          save_custom_option(pending_conn, "mating_type", val)
        }
        for (val in unique(df$COLLECTION[!is.na(df$COLLECTION) & df$COLLECTION != ""])) {
          save_custom_option(pending_conn, "collection", val)
        }

        DBI::dbWriteTable(pending_conn, "pending_yjs", df, append = TRUE, row.names = FALSE)
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("%d YJS sample(s) submitted for review.", nrow(df)))
        )
        yjs_preview_data(NULL)
        yjs_list_data(data.frame())
        reset_yjs_form()
        refresh_choices()
      }, error = function(e) {
        output$yjs_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Submission error:", e$message))
        )
      })
    })

    output$yjs_template <- downloadHandler(
      filename = function() "yjs_template.tsv",
      content = function(file) {
        template <- data.frame(matrix(ncol = length(yjs_columns()), nrow = 0))
        colnames(template) <- yjs_columns()
        write.table(template, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )

    # ---- Strains ----

    strain_preview_data <- reactiveVal(NULL)
    strain_list_data <- reactiveVal(data.frame())

    build_strain_manual <- function() {
      data.frame(
        STRAIN_NAME = trimws(input$strain_name),
        ISOLATION = na_if_empty(input$strain_isolation),
        ECO_ORIGIN = na_if_empty(input$strain_eco_origin),
        GEO_ORIGIN = na_if_empty(input$strain_geo_origin),
        CONTINENT = na_if_empty(input$strain_continent),
        COUNTRY = na_if_empty(input$strain_country),
        CLADE = na_if_empty(input$strain_clade),
        SRR_ID = na_if_empty(input$strain_srr_id),
        SPECIES = na_if_empty(input$strain_species),
        stringsAsFactors = FALSE
      )
    }

    reset_strain_form <- function() {
      updateTextInput(session, "strain_name", value = "")
      updateTextInput(session, "strain_isolation", value = "")
      updateTextInput(session, "strain_eco_origin", value = "")
      updateTextInput(session, "strain_geo_origin", value = "")
      updateTextInput(session, "strain_continent", value = "")
      updateTextInput(session, "strain_country", value = "")
      updateTextInput(session, "strain_clade", value = "")
      updateTextInput(session, "strain_srr_id", value = "")
      updateSelectizeInput(
        session, "strain_species",
        choices = get_species(), selected = "", server = FALSE
      )
    }

    fill_strain_form <- function(row) {
      updateTextInput(session, "strain_name", value = na_to_empty(row$original_name))
      updateTextInput(session, "strain_isolation", value = na_to_empty(row$ISOLATION))
      updateTextInput(session, "strain_eco_origin", value = na_to_empty(row$ECO_ORIGIN))
      updateTextInput(session, "strain_geo_origin", value = na_to_empty(row$GEO_ORIGIN))
      updateTextInput(session, "strain_continent", value = na_to_empty(row$CONTINENT))
      updateTextInput(session, "strain_country", value = na_to_empty(row$COUNTRY))
      updateTextInput(session, "strain_clade", value = na_to_empty(row$CLADE))
      updateTextInput(session, "strain_srr_id", value = na_to_empty(row$SRR_ID))
      sp <- get_species()
      sel_sp <- na_to_empty(row$SPECIES)
      if (nchar(sel_sp) > 0 && !(sel_sp %in% sp)) sp <- c(sp, sel_sp)
      updateSelectizeInput(session, "strain_species",
                           choices = sp, selected = sel_sp,
                           server = FALSE)
    }

    build_strain_from_file <- function() {
      req(input$strain_file)
      file_path <- input$strain_file$datapath
      ext <- tools::file_ext(input$strain_file$name)

      df <- if (ext == "tsv" || ext == "txt") {
        read.delim(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      } else {
        read.csv(file_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
      }

      expected <- strain_columns()
      for (col in expected) {
        if (!col %in% names(df)) df[[col]] <- NA
      }
      df <- df[, expected, drop = FALSE]
      df
    }

    assign_xtra_names <- function(df) {
      existing <- strain_list_data()
      existing_xtra <- if (nrow(existing) > 0 && "STRAIN" %in% names(existing)) {
        grep("^XTRA_", existing$STRAIN, value = TRUE)
      } else {
        character(0)
      }
      xtra_names <- next_xtra_names(main_conn, pending_conn, nrow(df),
                                    extra_names = existing_xtra)
      df$STRAIN <- xtra_names
      df$original_name <- df$STRAIN_NAME
      df$STRAIN_NAME <- NULL
      df
    }

    # Fill form when a row is selected in the table
    observeEvent(input$strain_preview_table_rows_selected, {
      sel <- input$strain_preview_table_rows_selected
      if (length(sel) == 1 && input$strain_mode == "manual") {
        df <- strain_list_data()
        if (sel >= 1 && sel <= nrow(df)) {
          fill_strain_form(df[sel, ])
        }
      }
    })

    # Add to list (manual mode)
    observeEvent(input$strain_add_btn, {
      tryCatch({
        df <- build_strain_manual()

        if (nrow(df) == 0 || all(is.na(df$STRAIN_NAME) | trimws(df$STRAIN_NAME) == "")) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger", "Strain name is required.")
          )
          return()
        }

        df_with_names <- assign_xtra_names(df)
        result <- validate_strains(df_with_names, main_conn, pending_conn)

        if (!result$valid) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
          return()
        }

        current <- strain_list_data()
        strain_list_data(rbind(current, df_with_names))
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("Added %s (%s) to list. %d entry(ies) ready to submit.",
                           df_with_names$STRAIN[1], df_with_names$original_name[1],
                           nrow(current) + 1))
        )
        reset_strain_form()
      }, error = function(e) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    # Update selected entry (manual mode)
    observeEvent(input$strain_update_btn, {
      sel <- input$strain_preview_table_rows_selected
      if (length(sel) != 1) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-warning", "Select a row in the list to update.")
        )
        return()
      }

      tryCatch({
        df <- build_strain_manual()

        if (nrow(df) == 0 || all(is.na(df$STRAIN_NAME) | trimws(df$STRAIN_NAME) == "")) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger", "Strain name is required.")
          )
          return()
        }

        current <- strain_list_data()
        # Keep the existing STRAIN (XTRA_XXX) name for the updated row
        existing_strain <- current$STRAIN[sel]
        df_updated <- df
        df_updated$STRAIN <- existing_strain
        df_updated$original_name <- df_updated$STRAIN_NAME
        df_updated$STRAIN_NAME <- NULL

        result <- validate_strains(df_updated, main_conn, pending_conn)
        if (!result$valid) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
          return()
        }

        current[sel, ] <- df_updated
        strain_list_data(current)
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("Updated %s in list.", existing_strain))
        )
        reset_strain_form()
      }, error = function(e) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    # Remove selected entry from strain list
    observeEvent(input$strain_remove_btn, {
      sel <- input$strain_preview_table_rows_selected
      if (length(sel) != 1) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-warning", "Select a row in the list to remove.")
        )
        return()
      }
      current <- strain_list_data()
      if (sel >= 1 && sel <= nrow(current)) {
        strain_list_data(current[-sel, , drop = FALSE])
        reset_strain_form()
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-info",
                   sprintf("Removed entry. %d entry(ies) in list.", nrow(current) - 1))
        )
      }
    })

    # Preview from file upload
    observeEvent(input$strain_preview_btn, {
      tryCatch({
        df <- build_strain_from_file()

        if (nrow(df) == 0 || all(is.na(df$STRAIN_NAME) | trimws(df$STRAIN_NAME) == "")) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger", "At least one strain name is required.")
          )
          return()
        }

        df_with_names <- assign_xtra_names(df)
        result <- validate_strains(df_with_names, main_conn, pending_conn)
        strain_preview_data(df_with_names)

        if (!result$valid) {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-danger",
                     tags$strong("Validation errors:"),
                     tags$ul(lapply(result$errors, tags$li)))
          )
        } else {
          output$strain_feedback <- renderUI(
            tags$div(class = "alert alert-success",
                     sprintf("%d strain(s) passed validation. Names assigned: %s",
                             nrow(df_with_names),
                             paste(df_with_names$STRAIN, collapse = ", ")))
          )
        }
      }, error = function(e) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Error:", e$message))
        )
      })
    })

    output$strain_preview_table <- DT::renderDataTable({
      if (input$strain_mode == "manual") {
        df <- strain_list_data()
        req(nrow(df) > 0)
        DT::datatable(df, rownames = FALSE,
                      selection = list(mode = "single"),
                      options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
      } else {
        req(strain_preview_data())
        DT::datatable(strain_preview_data(), rownames = FALSE,
                      selection = "none",
                      options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
      }
    })

    observeEvent(input$strain_submit_btn, {
      req(user_info())

      df <- if (input$strain_mode == "manual") {
        strain_list_data()
      } else {
        strain_preview_data()
      }
      req(nrow(df) > 0)

      result <- validate_strains(df, main_conn, pending_conn)

      if (!result$valid) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-danger",
                   tags$strong("Cannot submit — validation errors:"),
                   tags$ul(lapply(result$errors, tags$li)))
        )
        return()
      }

      df$submitted_by <- user_info()$username
      df$submitted_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

      tryCatch({
        # Save any new custom species options
        for (val in unique(df$SPECIES[!is.na(df$SPECIES) & df$SPECIES != ""])) {
          save_custom_option(pending_conn, "species", val)
        }

        DBI::dbWriteTable(pending_conn, "pending_strains", df, append = TRUE, row.names = FALSE)
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-success",
                   sprintf("%d strain(s) submitted for review.", nrow(df)))
        )
        strain_preview_data(NULL)
        strain_list_data(data.frame())
        reset_strain_form()
        refresh_choices()
      }, error = function(e) {
        output$strain_feedback <- renderUI(
          tags$div(class = "alert alert-danger", paste("Submission error:", e$message))
        )
      })
    })

    output$strain_template <- downloadHandler(
      filename = function() "strains_template.tsv",
      content = function(file) {
        template <- data.frame(matrix(ncol = length(strain_columns()), nrow = 0))
        colnames(template) <- strain_columns()
        write.table(template, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
  })
}

# Helper: convert empty string to NA
na_if_empty <- function(x) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && trimws(x) == "")) NA else x
}

# Helper: convert NA to empty string (for filling form fields)
na_to_empty <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) "" else as.character(x)
}
