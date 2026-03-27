#' My Submissions Module
#'
#' Shows all submissions by the current user: pending, approved, and rejected.
#'
#' @param id Module namespace ID

notifications_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#", ns("submissions_dt"),
      " thead td > div > input + span { display: none !important; }",
      " #", ns("submissions_dt"),
      " table.dataTable tbody tr { --bs-table-bg-type: transparent !important;",
      " --bs-table-striped-bg: transparent !important; }"
    ))),
    tags$div(
      class = "d-flex justify-content-between align-items-center mb-3",
      tags$h5("My Submissions", class = "mb-0"),
      tags$div(
        class = "d-flex align-items-center gap-3",
        checkboxInput(ns("show_new"), "Show newly resolved entries (0)",
                      value = FALSE),
        actionButton(ns("refresh_btn"), "Refresh", icon = icon("sync"),
                     class = "btn-sm btn-outline-primary"),
        downloadButton(ns("download_csv"), "Download CSV",
                       class = "btn-sm btn-outline-secondary")
      )
    ),
    DT::dataTableOutput(ns("submissions_dt"))
  )
}

notifications_server <- function(id, main_conn, pending_conn, user_info, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    submissions <- reactiveVal(data.frame())

    empty_subs <- function() {
      data.frame(
        name = character(), type = character(), status = character(),
        submitted_at = character(), assigned_number = character(),
        reviewer = character(),
        box = character(), box_row = character(), box_col = character(),
        plate = character(), plate_row = character(), plate_col = character(),
        is_read = character(),
        stringsAsFactors = FALSE
      )
    }

    coerce_subs <- function(df) {
      if (nrow(df) == 0) return(empty_subs())
      df$name <- as.character(df$name)
      df$type <- as.character(df$type)
      df$status <- as.character(df$status)
      df$submitted_at <- as.character(df$submitted_at)
      df$assigned_number <- as.character(df$assigned_number)
      df$reviewer <- as.character(df$reviewer)
      df$box <- as.character(df$box)
      df$box_row <- as.character(df$box_row)
      df$box_col <- as.character(df$box_col)
      df$plate <- as.character(df$plate)
      df$plate_row <- as.character(df$plate_row)
      df$plate_col <- as.character(df$plate_col)
      df$is_read <- as.character(df$is_read)
      df
    }

    load_submissions <- function() {
      req(user_info())
      username <- user_info()$username

      pending_yjs <- tryCatch(
        coerce_subs(DBI::dbGetQuery(pending_conn,
          "SELECT SAMPLE_NAME AS name, 'YJS Sample' AS type, 'pending' AS status,
                  submitted_at, '' AS assigned_number, '' AS reviewer,
                  CAST(BOX_NUMBER AS TEXT) AS box,
                  CAST(BOX_ROW AS TEXT) AS box_row,
                  CAST(BOX_COL AS TEXT) AS box_col,
                  CAST(PLATE AS TEXT) AS plate,
                  CAST(PLATE_ROW AS TEXT) AS plate_row,
                  CAST(PLATE_COL AS TEXT) AS plate_col,
                  '1' AS is_read
           FROM pending_yjs WHERE submitted_by = ?",
          params = list(username))),
        error = function(e) empty_subs()
      )

      pending_strains <- tryCatch(
        coerce_subs(DBI::dbGetQuery(pending_conn,
          "SELECT original_name AS name, 'Strain' AS type, 'pending' AS status,
                  submitted_at, '' AS assigned_number, '' AS reviewer,
                  '' AS box, '' AS box_row, '' AS box_col,
                  '' AS plate, '' AS plate_row, '' AS plate_col,
                  '1' AS is_read
           FROM pending_strains WHERE submitted_by = ?",
          params = list(username))),
        error = function(e) empty_subs()
      )

      resolved <- tryCatch(
        coerce_subs(DBI::dbGetQuery(pending_conn,
          "SELECT entry_name AS name, entry_type AS type, status,
                  created_at AS submitted_at, assigned_number, reviewer,
                  COALESCE(box, '') AS box,
                  COALESCE(box_row, '') AS box_row,
                  COALESCE(box_col, '') AS box_col,
                  COALESCE(plate, '') AS plate,
                  COALESCE(plate_row, '') AS plate_row,
                  COALESCE(plate_col, '') AS plate_col,
                  CAST(is_read AS TEXT) AS is_read
           FROM notifications WHERE username = ?
           ORDER BY created_at DESC",
          params = list(username))),
        error = function(e) empty_subs()
      )

      # Fallback: fill box/row/col from main DB for approved YJS entries missing location
      if (nrow(resolved) > 0) {
        yjs_idx <- which(resolved$type == "YJS Sample" &
                         !is.na(resolved$assigned_number) &
                         resolved$assigned_number != "" &
                         (resolved$box == "" | is.na(resolved$box)))
        if (length(yjs_idx) > 0) {
          yjs_nums <- resolved$assigned_number[yjs_idx]
          placeholders <- paste(rep("?", length(yjs_nums)), collapse = ",")
          loc <- tryCatch(
            DBI::dbGetQuery(main_conn, sprintf(
              "SELECT YJS_NUMBER,
                      CAST(BOX_NUMBER AS TEXT) AS box,
                      CAST(BOX_ROW AS TEXT) AS box_row,
                      CAST(BOX_COL AS TEXT) AS box_col,
                      CAST(PLATE AS TEXT) AS plate,
                      CAST(PLATE_ROW AS TEXT) AS plate_row,
                      CAST(PLATE_COL AS TEXT) AS plate_col
               FROM YJSnumbers WHERE YJS_NUMBER IN (%s)", placeholders),
              params = as.list(yjs_nums)),
            error = function(e) data.frame()
          )
          if (nrow(loc) > 0) {
            m <- match(resolved$assigned_number[yjs_idx], loc$YJS_NUMBER)
            resolved$box[yjs_idx] <- ifelse(is.na(m), "", loc$box[m])
            resolved$box_row[yjs_idx] <- ifelse(is.na(m), "", loc$box_row[m])
            resolved$box_col[yjs_idx] <- ifelse(is.na(m), "", loc$box_col[m])
            resolved$plate[yjs_idx] <- ifelse(is.na(m), "", loc$plate[m])
            resolved$plate_row[yjs_idx] <- ifelse(is.na(m), "", loc$plate_row[m])
            resolved$plate_col[yjs_idx] <- ifelse(is.na(m), "", loc$plate_col[m])
          }
        }
      }

      all_subs <- dplyr::bind_rows(pending_yjs, pending_strains, resolved)
      if (nrow(all_subs) > 0) {
        all_subs <- all_subs[order(all_subs$status != "pending",
                                   as.POSIXct(all_subs$submitted_at),
                                   decreasing = c(FALSE, TRUE)), ]
      }
      if (nrow(all_subs) == 0) all_subs <- empty_subs()

      submissions(all_subs)
    }

    observe({ load_submissions() })
    observeEvent(input$refresh_btn, { load_submissions() })

    has_visited <- reactiveVal(FALSE)

    observeEvent(active_tab(), {
      if (identical(active_tab(), "submissions")) {
        if (has_visited()) {
          load_submissions()
        }
        has_visited(TRUE)
      } else if (has_visited()) {
        username <- user_info()$username
        tryCatch(
          DBI::dbExecute(pending_conn,
            "UPDATE notifications SET is_read = 1
             WHERE username = ? AND is_read = 0",
            params = list(username)),
          error = function(e) NULL
        )
        unread_count(0)
      }
    })

    unread_count <- reactiveVal(0)

    observe({
      df <- submissions()
      n_new <- sum(df$is_read == "0")
      unread_count(n_new)
      updateCheckboxInput(session, "show_new",
        label = paste0("Show newly resolved entries (", n_new, ")"))
    })

    output$submissions_dt <- DT::renderDT({
      df <- submissions()
      if (isTRUE(input$show_new)) {
        df <- df[df$is_read == "0", , drop = FALSE]
      }
      if (nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No submissions yet."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }

      display <- data.frame(
        Name = df$name,
        Type = df$type,
        Status = df$status,
        `Assigned Number` = ifelse(df$assigned_number == "" | is.na(df$assigned_number),
                                   "\u2014", df$assigned_number),
        Box = ifelse(df$box == "" | is.na(df$box), "\u2014", df$box),
        `Box Row` = ifelse(df$box_row == "" | is.na(df$box_row), "\u2014", df$box_row),
        `Box Col` = ifelse(df$box_col == "" | is.na(df$box_col), "\u2014", df$box_col),
        Plate = ifelse(df$plate == "" | is.na(df$plate), "\u2014", df$plate),
        `Plate Row` = ifelse(df$plate_row == "" | is.na(df$plate_row), "\u2014", df$plate_row),
        `Plate Col` = ifelse(df$plate_col == "" | is.na(df$plate_col), "\u2014", df$plate_col),
        `Reviewed By` = ifelse(df$reviewer == "" | is.na(df$reviewer),
                               "\u2014", df$reviewer),
        Date = df$submitted_at,
        is_read = df$is_read,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      DT::datatable(
        display, rownames = FALSE, selection = "none",
        class = "display",
        filter = "top",
        options = list(
          scrollX = TRUE, scrollY = "calc(100vh - 300px)", paging = FALSE, dom = "ti",
          columnDefs = list(list(visible = FALSE, targets = 12)),
          rowCallback = htmlwidgets::JS(
            "function(row, data, displayNum, displayIndex, dataIndex) {
              var status = data[2];
              var isRead = String(data[12]);
              var color = '';
              if (status === 'approved') {
                if (isRead === '0') color = '#86A9D5';
                else if (isRead === '2') color = '#A4BFDF';
                else color = '#DEEAF6';
              }
              if (status === 'rejected') {
                if (isRead === '0') color = '#F4978E';
                else if (isRead === '2') color = '#F6A7A2';
                else color = '#FDE2DF';
              }
              if (color) { row.style.setProperty('background-color', color, 'important'); $('td', row).each(function() { this.style.setProperty('background-color', color, 'important'); }); }
              if (isRead === '0') { $('td', row).css({'font-weight': '900', 'text-shadow': '0 0 0.5px currentColor', 'color': '#ffffff'}); }
            }")
        )
      )
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("my_submissions_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- submissions()
        if (nrow(df) == 0) {
          write.csv(data.frame(Message = "No submissions yet."),
                    file, row.names = FALSE)
        } else {
          filtered_rows <- input$submissions_dt_rows_all
          if (!is.null(filtered_rows)) df <- df[filtered_rows, ]
          export <- data.frame(
            Name = df$name, Type = df$type, Status = df$status,
            Assigned_Number = df$assigned_number,
            Box = df$box, Box_Row = df$box_row, Box_Col = df$box_col,
            Plate = df$plate, Plate_Row = df$plate_row, Plate_Col = df$plate_col,
            Reviewed_By = df$reviewer, Date = df$submitted_at,
            stringsAsFactors = FALSE
          )
          write.csv(export, file, row.names = FALSE)
        }
      }
    )

    return(unread_count)
  })
}
