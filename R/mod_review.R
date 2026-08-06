#' Review Entries Module
#'
#' Admin-only tab for reviewing pending submissions.
#' Supports approve (insert into main DB tables + delete from pending) and reject (delete from pending).
#' Admin cannot approve their own submissions (server-side enforced).
#'
#' @param id Module namespace ID
#' @param db_conn DB connection
#' @param user_info Reactive returning logged-in user data.frame

review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Review Pending Entries",
        actionButton(ns("refresh_btn"), "Refresh", icon = icon("sync"),
                     class = "btn-sm btn-outline-primary")
      ),
      layout_columns(
        col_widths = 12,
        selectInput(ns("pending_table"), "Pending Table",
                    choices = c("YJS Samples" = "pending_yjs",
                                "Strains" = "pending_strains"),
                    width = "300px")
      ),
      tags$div(
        class = "mb-3",
        actionButton(ns("select_all_btn"), "Select All",
                     class = "btn-sm btn-outline-secondary me-2"),
        actionButton(ns("unselect_all_btn"), "Unselect All",
                     class = "btn-sm btn-outline-secondary me-3"),
        actionButton(ns("approve_btn"), "Approve Selected",
                     class = "btn-success me-2", icon = icon("check")),
        actionButton(ns("reject_btn"), "Reject Selected",
                     class = "btn-danger", icon = icon("times"))
      ),
      uiOutput(ns("review_feedback")),
      DT::dataTableOutput(ns("pending_dt"))
    )
  )
}

review_server <- function(id, db_conn, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    pending_data <- reactiveVal(NULL)

    review_count <- reactiveVal(0)

    update_dropdown_counts <- function() {
      current_user <- user_info()$username
      n_yjs <- as.integer(DBI::dbGetQuery(db_conn,
        "SELECT COUNT(*) AS n FROM pending_yjs WHERE submitted_by != $1",
        params = list(current_user))$n)
      n_strains <- as.integer(DBI::dbGetQuery(db_conn,
        "SELECT COUNT(*) AS n FROM pending_strains WHERE submitted_by != $1",
        params = list(current_user))$n)
      review_count(n_yjs + n_strains)
      choices <- setNames(
        c("pending_yjs", "pending_strains"),
        c(sprintf("YJS Samples (%d)", n_yjs),
          sprintf("Strains (%d)", n_strains))
      )
      updateSelectInput(session, "pending_table", choices = choices,
                        selected = input$pending_table)
    }

    load_pending <- function() {
      req(is_admin(user_info()))
      table_name <- input$pending_table
      if (is.null(table_name)) table_name <- "pending_yjs"
      current_user <- user_info()$username
      df <- DBI::dbGetQuery(db_conn,
        sprintf("SELECT * FROM %s WHERE submitted_by != $1 ORDER BY submitted_at DESC",
                table_name),
        params = list(current_user))
      pending_data(df)
      update_dropdown_counts()
    }

    observe({ load_pending() })
    observeEvent(input$pending_table, { load_pending() })
    observeEvent(input$refresh_btn, { load_pending() })

    dt_proxy <- DT::dataTableProxy("pending_dt")

    output$pending_dt <- DT::renderDataTable({
      req(pending_data())
      df <- pending_data()
      if (nrow(df) == 0) {
        return(DT::datatable(df, rownames = FALSE,
                             options = list(dom = "t", language = list(emptyTable = "No pending entries."))))
      }
      DT::datatable(df, selection = "multiple", rownames = FALSE,
                    options = list(scrollX = TRUE, scrollY = "calc(100vh - 300px)", paging = FALSE, dom = "ti"))
    })

    observeEvent(input$select_all_btn, {
      req(pending_data(), nrow(pending_data()) > 0)
      DT::selectRows(dt_proxy, seq_len(nrow(pending_data())))
    })

    observeEvent(input$unselect_all_btn, {
      DT::selectRows(dt_proxy, NULL)
    })

    # Approve selected rows
    observeEvent(input$approve_btn, {
      req(is_admin(user_info()))
      selected <- input$pending_dt_rows_selected
      if (length(selected) == 0) {
        output$review_feedback <- renderUI(
          tags$div(class = "alert alert-warning mt-2", "No rows selected.")
        )
        return()
      }

      df <- pending_data()
      rows <- df[selected, , drop = FALSE]
      current_user <- user_info()$username
      table_name <- input$pending_table

      # Self-approval check
      own_rows <- rows$submitted_by == current_user
      if (any(own_rows)) {
        output$review_feedback <- renderUI(
          tags$div(class = "alert alert-danger mt-2",
                   sprintf("Cannot approve your own submissions. %d row(s) blocked (submitted by '%s').",
                           sum(own_rows), current_user))
        )
        return()
      }

      approved <- 0
      failed <- character(0)

      for (i in seq_len(nrow(rows))) {
        row <- rows[i, , drop = FALSE]
        row_id <- row$id

        tryCatch({
          if (table_name == "pending_yjs") {
            assigned_yjs <- approve_yjs_row(row, db_conn)
            create_notification(
              db_conn, row$submitted_by, "YJS Sample",
              row$sample_name, assigned_yjs, "approved", current_user,
              box = row$box_number, box_row = row$box_row, box_col = row$box_col,
              plate = row$plate, plate_row = row$plate_row, plate_col = row$plate_col
            )
          } else if (table_name == "pending_strains") {
            approve_strain_row(row, db_conn)
            create_notification(
              db_conn, row$submitted_by, "Strain",
              row$original_name, row$strain, "approved", current_user
            )
          }
          approved <- approved + 1
        }, error = function(e) {
          failed <<- c(failed, sprintf("Row %d: %s", row_id, e$message))
        })
      }

      msg <- sprintf("%d row(s) approved.", approved)
      if (length(failed) > 0) {
        msg <- paste(msg, "Failures:", paste(failed, collapse = "; "))
      }

      output$review_feedback <- renderUI(
        tags$div(class = if (length(failed) > 0) "alert alert-warning mt-2" else "alert alert-success mt-2",
                 msg)
      )
      load_pending()
    })

    # Reject selected rows
    observeEvent(input$reject_btn, {
      req(is_admin(user_info()))
      selected <- input$pending_dt_rows_selected
      if (length(selected) == 0) {
        output$review_feedback <- renderUI(
          tags$div(class = "alert alert-warning mt-2", "No rows selected.")
        )
        return()
      }

      df <- pending_data()
      ids <- df$id[selected]
      table_name <- input$pending_table

      rows <- df[selected, , drop = FALSE]
      current_user <- user_info()$username

      for (i in seq_len(nrow(rows))) {
        row <- rows[i, , drop = FALSE]
        entry_name <- if (table_name == "pending_yjs") row$sample_name else row$original_name
        entry_type <- if (table_name == "pending_yjs") "YJS Sample" else "Strain"
        if (table_name == "pending_yjs") {
          create_notification(
            db_conn, row$submitted_by, entry_type,
            entry_name, NA_character_, "rejected", current_user,
            box = row$box_number, box_row = row$box_row, box_col = row$box_col,
            plate = row$plate, plate_row = row$plate_row, plate_col = row$plate_col
          )
        } else {
          create_notification(
            db_conn, row$submitted_by, entry_type,
            entry_name, NA_character_, "rejected", current_user
          )
        }
      }

      placeholders <- paste0("$", seq_along(ids), collapse = ",")
      DBI::dbExecute(
        db_conn,
        sprintf("DELETE FROM %s WHERE id IN (%s)", table_name, placeholders),
        params = as.list(ids)
      )

      output$review_feedback <- renderUI(
        tags$div(class = "alert alert-info mt-2",
                 sprintf("%d row(s) rejected and removed.", length(ids)))
      )
      load_pending()
    })

    return(review_count)
  })
}

# -- Approval helpers --

#' Approve a YJS row: auto-assign YJS number, insert into main DB, delete from pending
#' @return The assigned YJS number
approve_yjs_row <- function(row, db_conn) {
  assigned_yjs <- get_next_yjs_number(db_conn, 1)

  db_yjs_cols <- c("yjs_number", "sample_name", "species", "mating_type", "ploidy",
                   "genotype", "sporulation", "external_origin", "eco_origin",
                   "comments_origin", "parental_origin", "publication", "strains_group",
                   "old_box", "box_number", "box_row", "box_col",
                   "plate", "plate_row", "plate_col",
                   "notes", "stocked_by", "comments", "id_strain",
                   "sample_type", "collection")

  insert_data <- row[, db_yjs_cols, drop = FALSE]
  names(insert_data) <- db_yjs_cols
  insert_data$yjs_number <- assigned_yjs
  placeholders <- paste0("$", seq_along(db_yjs_cols), collapse = ", ")
  col_names <- paste(db_yjs_cols, collapse = ", ")
  sql <- sprintf("INSERT INTO yjs_numbers (%s) VALUES (%s)", col_names, placeholders)

  DBI::dbWithTransaction(db_conn, {
    DBI::dbExecute(db_conn, sql, params = unname(as.list(insert_data)))
  })

  DBI::dbExecute(db_conn,
                 "DELETE FROM pending_yjs WHERE id = $1",
                 params = list(row$id))

  assigned_yjs
}

#' Approve a Strain row: insert into main DB Strains + alt_names, delete from pending
approve_strain_row <- function(row, db_conn) {
  strain_cols <- c("strain", "isolation", "eco_origin", "geo_origin",
                   "continent", "country", "clade", "srr_id", "species")
  db_strain_cols <- c("strain", "isolation", "eco_origin", "geo_origin",
                      "continent", "country", "clade", "srr_id", "species")

  insert_data <- row[, strain_cols, drop = FALSE]
  names(insert_data) <- db_strain_cols
  placeholders <- paste0("$", seq_along(strain_cols), collapse = ", ")
  col_names <- paste(db_strain_cols, collapse = ", ")
  sql <- sprintf("INSERT INTO strains (%s) VALUES (%s)", col_names, placeholders)

  DBI::dbWithTransaction(db_conn, {
    DBI::dbExecute(db_conn, sql, params = unname(as.list(insert_data)))

    if (!is.na(row$original_name) && nchar(trimws(row$original_name)) > 0) {
      DBI::dbExecute(db_conn,
                     "INSERT INTO alt_names (strain, alt_name) VALUES ($1, $2)",
                     params = list(row$strain, row$original_name))
    }
  })

  DBI::dbExecute(db_conn,
                 "DELETE FROM pending_strains WHERE id = $1",
                 params = list(row$id))
}
