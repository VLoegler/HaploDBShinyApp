#' Name Conversion Module
#'
#' Unified search across YJSnumbers, Strains, AltYJS, and AltNames tables.
#' Exact match for YJS numbers and Strain IDs, fuzzy match for names.
#' Results displayed in tabbed layout with inline expandable detail rows.
#'
#' @param id Module namespace ID
#' @param main_conn Main SQLite DB connection

nameconv_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      td.details-control {
        cursor: pointer;
        font-weight: bold;
        font-size: 1.2em;
        color: var(--bs-primary, #0d6efd);
        text-align: center;
        width: 30px;
      }
      td.details-control:hover { color: var(--bs-link-hover-color, #0a58ca); }
      tr.child-row td {
        padding: 12px 16px;
        background-color: var(--bs-tertiary-bg, #f8f9fa);
      }
      .child-detail { max-width: 700px; }
      .child-detail .detail-row {
        display: flex;
        padding: 2px 0;
      }
      .child-detail .detail-label {
        flex: 0 0 160px;
        font-weight: 600;
        color: var(--bs-secondary-color, #6c757d);
        text-align: right;
        padding-right: 12px;
      }
      .child-detail .detail-value { flex: 1; }
      .child-detail .detail-section {
        font-weight: 600;
        margin-top: 8px;
        margin-bottom: 4px;
        padding-bottom: 2px;
        border-bottom: 1px solid #dee2e6;
      }
    ")),
    tags$script(HTML(sprintf(
      "$(document).on('keypress', '#%s', function(e) {
         if (e.which === 13) {
           e.preventDefault();
           $(this).trigger('change');
           var btn = $('#%s');
           setTimeout(function() { btn.click(); }, 100);
         }
       });",
      ns("search_input"), ns("search_btn")
    ))),
    tags$div(
      class = "mb-4",
      tags$h5("Name Conversion", class = "mb-3"),
      tags$div(
        class = "d-flex align-items-center gap-2",
        tags$div(
          style = "flex: 1; min-width: 0;",
          textInput(ns("search_input"), NULL, width = "100%",
                    placeholder = "Search by YJS number, strain ID, or name...")
        ),
        actionButton(ns("search_btn"), "Search", icon = icon("search"),
                     class = "btn-primary",
                     style = "height: 38px; margin-top: -19px; display: flex; align-items: center; gap: 0.4em; white-space: nowrap;")
      ),
      tags$small(class = "text-muted",
                 "Exact match for YJS/Strain IDs. Partial match for names.",
                 tags$br(),
                 "Examples: AAA, BAM, SACE_GAL, RM11, YJS1359")
    ),
    uiOutput(ns("results"))
  )
}

nameconv_server <- function(id, main_conn, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    search_data <- reactiveVal(NULL)

    observeEvent(input$search_btn, {
      query_val <- trimws(input$search_input)
      if (nchar(query_val) == 0) {
        search_data(NULL)
        return()
      }
      search_data(collect_search_results(main_conn, query_val))
    })

    output$results <- renderUI({
      data <- search_data()
      if (is.null(data)) return(NULL)

      n_strains <- nrow(data$strains)
      n_samples <- nrow(data$samples)

      if (n_strains == 0 && n_samples == 0) {
        return(tags$div(
          class = "alert alert-warning mt-3",
          icon("exclamation-triangle"),
          paste0(" No match found for '", data$query, "'")
        ))
      }

      alert_ui <- lapply(data$alerts, function(msg) {
        tags$div(class = "alert alert-info mt-3 mb-1",
                 icon("arrow-right"), msg)
      })

      selected_tab <- if (n_strains > 0 && n_strains >= n_samples) {
        "strains"
      } else {
        "samples"
      }

      tagList(
        alert_ui,
        if (!is.null(data$fuzzy_header)) {
          tags$h6(class = "mt-3 mb-2 text-muted", data$fuzzy_header)
        },
        navset_card_tab(
          id = ns("results_tabs"),
          selected = selected_tab,
          nav_panel(
            title = paste0("Strains (", n_strains, ")"),
            value = "strains",
            if (n_strains > 0) {
              DT::DTOutput(ns("strains_table"))
            } else {
              tags$p(class = "text-muted mt-3",
                     "No matching strains.")
            }
          ),
          nav_panel(
            title = paste0("Samples (", n_samples, ")"),
            value = "samples",
            if (n_samples > 0) {
              DT::DTOutput(ns("samples_table"))
            } else {
              tags$p(class = "text-muted mt-3",
                     "No matching samples.")
            }
          )
        )
      )
    })

    output$strains_table <- DT::renderDT({
      data <- search_data()
      req(data, nrow(data$strains) > 0)
      build_strain_dt(data$strains)
    })

    output$samples_table <- DT::renderDT({
      data <- search_data()
      req(data, nrow(data$samples) > 0)
      build_sample_dt(data$samples)
    })
  })
}

# --- JS callback for child row toggle ---
# format_fn_body: JS function body that takes `d` (row data array)
# and returns HTML string for the child row content
child_row_callback <- function(format_fn_body) {
  DT::JS(paste0(
    "var formatDetail = ", format_fn_body, ";",
    "table.on('click', 'td.details-control', function() {",
    "  var td = $(this);",
    "  var tr = td.closest('tr');",
    "  var row = table.row(tr);",
    "  if (row.child.isShown()) {",
    "    row.child.hide();",
    "    td.html('+');",
    "  } else {",
    "    row.child(formatDetail(row.data())).show();",
    "    tr.next('tr').addClass('child-row');",
    "    td.html('\\u2212');",
    "  }",
    "});"
  ))
}

# --- Build strain DT with expandable child rows ---
build_strain_dt <- function(strains_df) {
  # Prepend empty column for expand toggle
  df <- cbind(` ` = rep("", nrow(strains_df)), strains_df)

  # Visible: toggle, STRAIN, SPECIES, CLADE, ECO_ORIGIN, COUNTRY, Match Source
  # Hidden: GEO_ORIGIN(7), CONTINENT(8), ISOLATION(9), SRR_ID(10),
  #         ALT_NAMES(11), LINKED_YJS(12)
  hidden_cols <- which(names(df) %in%
    c("GEO_ORIGIN", "CONTINENT", "ISOLATION", "SRR_ID",
      "ALT_NAMES", "LINKED_YJS")) - 1  # 0-indexed

  format_fn <- paste0(
    "function(d) {",
    "  function row(label, val) {",
    "    var v = (val === null || val === '') ? '<span style=\"color:#adb5bd\">&mdash;</span>' : val;",
    "    return '<div class=\"detail-row\"><div class=\"detail-label\">' + label + '</div><div class=\"detail-value\">' + v + '</div></div>';",
    "  }",
    "  var h = '<div class=\"child-detail\">';",
    "  h += row('Geo Origin', d[7]);",
    "  h += row('Continent', d[8]);",
    "  h += row('Isolation', d[9]);",
    "  h += row('SRR ID', d[10]);",
    "  h += row('Alternative Names', d[11]);",
    "  if (d[12] && d[12] !== '') {",
    "    h += '<div class=\"detail-section\">Linked YJS Samples</div>';",
    "    h += '<div style=\"padding-left:172px\">' + d[12] + '</div>';",
    "  }",
    "  h += '</div>';",
    "  return h;",
    "}"
  )

  DT::datatable(
    df,
    rownames = FALSE,
    escape = FALSE,
    selection = "none",
    callback = child_row_callback(format_fn),
    options = list(
      dom = "tip",
      pageLength = 10,
      columnDefs = list(
        list(
          targets = 0,
          orderable = FALSE,
          className = "details-control",
          render = DT::JS(
            "function(data, type, row, meta) { return '+'; }"
          )
        ),
        list(targets = hidden_cols, visible = FALSE)
      )
    ),
    class = "compact stripe"
  )
}

# --- Build sample DT with expandable child rows ---
build_sample_dt <- function(samples_df) {
  df <- cbind(` ` = rep("", nrow(samples_df)), samples_df)

  # Visible: toggle, YJS_NUMBER, SAMPLE_NAME, SPECIES, ID_STRAIN,
  #          SAMPLE_TYPE, PLOIDY, Match Source
  # Hidden: MATING_TYPE(8), STRAINS_GROUP(9), GENOTYPE(10),
  #         COLLECTION(11), OLD_YJS(12), STRAIN_SPECIES(13),
  #         CLADE(14), ECO_ORIGIN(15), GEO_ORIGIN(16),
  #         COUNTRY(17), ISOLATION(18), ALT_NAMES(19)
  hidden_cols <- which(names(df) %in%
    c("MATING_TYPE", "STRAINS_GROUP", "GENOTYPE", "COLLECTION",
      "OLD_YJS", "STRAIN_SPECIES", "CLADE", "ECO_ORIGIN",
      "GEO_ORIGIN", "COUNTRY", "ISOLATION", "ALT_NAMES")) - 1

  format_fn <- paste0(
    "function(d) {",
    "  function row(label, val) {",
    "    var v = (val === null || val === '') ? '<span style=\"color:#adb5bd\">&mdash;</span>' : val;",
    "    return '<div class=\"detail-row\"><div class=\"detail-label\">' + label + '</div><div class=\"detail-value\">' + v + '</div></div>';",
    "  }",
    "  var h = '<div class=\"child-detail\">';",
    "  h += row('Mating Type', d[8]);",
    "  h += row('Strains Group', d[9]);",
    "  h += row('Genotype', d[10]);",
    "  h += row('Collection', d[11]);",
    "  h += row('Old YJS Numbers', d[12]);",
    "  if (d[4] && d[4] !== '') {",
    "    h += '<div class=\"detail-section\">Linked Strain</div>';",
    "    h += row('Species', d[13]);",
    "    h += row('Clade', d[14]);",
    "    h += row('Eco Origin', d[15]);",
    "    h += row('Geo Origin', d[16]);",
    "    h += row('Country', d[17]);",
    "    h += row('Isolation', d[18]);",
    "    h += row('Alternative Names', d[19]);",
    "  }",
    "  h += '</div>';",
    "  return h;",
    "}"
  )

  DT::datatable(
    df,
    rownames = FALSE,
    escape = FALSE,
    selection = "none",
    callback = child_row_callback(format_fn),
    options = list(
      dom = "tip",
      pageLength = 10,
      columnDefs = list(
        list(
          targets = 0,
          orderable = FALSE,
          className = "details-control",
          render = DT::JS(
            "function(data, type, row, meta) { return '+'; }"
          )
        ),
        list(targets = hidden_cols, visible = FALSE)
      )
    ),
    class = "compact stripe"
  )
}

# --- Normalization: strip punctuation, lowercase ---
normalize_str <- function(x) {
  gsub("[^[:alnum:]]", "", tolower(x))
}

# --- Input classification ---
classify_input <- function(query) {
  q <- trimws(query)
  if (grepl("^YJS\\d+$", q, ignore.case = TRUE)) return("yjs")
  if (grepl("^(SACE_|XTRA_)?[A-Z]{3}$", q, ignore.case = TRUE)) return("strain")
  "freetext"
}

# --- Empty result template ---
empty_result <- function(query) {
  list(
    strains = data.frame(
      STRAIN = character(0), SPECIES = character(0),
      CLADE = character(0), ECO_ORIGIN = character(0),
      COUNTRY = character(0), `Match Source` = character(0),
      GEO_ORIGIN = character(0), CONTINENT = character(0),
      ISOLATION = character(0), SRR_ID = character(0),
      ALT_NAMES = character(0), LINKED_YJS = character(0),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    samples = data.frame(
      YJS_NUMBER = character(0), SAMPLE_NAME = character(0),
      SPECIES = character(0), ID_STRAIN = character(0),
      SAMPLE_TYPE = character(0), PLOIDY = character(0),
      `Match Source` = character(0),
      MATING_TYPE = character(0), STRAINS_GROUP = character(0),
      GENOTYPE = character(0), COLLECTION = character(0),
      OLD_YJS = character(0), STRAIN_SPECIES = character(0),
      CLADE = character(0), ECO_ORIGIN = character(0),
      GEO_ORIGIN = character(0), COUNTRY = character(0),
      ISOLATION = character(0), ALT_NAMES = character(0),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    alerts = list(),
    query = query,
    fuzzy_header = NULL
  )
}

# --- Main search dispatcher (returns structured data) ---
collect_search_results <- function(main_conn, query) {
  input_type <- classify_input(query)
  result <- empty_result(query)

  if (input_type == "yjs") {
    result <- search_yjs_exact(main_conn, query, result)
  } else if (input_type == "strain") {
    result <- search_strain_exact(main_conn, query, result)
  }

  if (nrow(result$strains) == 0 && nrow(result$samples) == 0) {
    result <- search_fuzzy(main_conn, query, result)
  }

  result
}

# --- Exact YJS search ---
search_yjs_exact <- function(main_conn, query, result) {
  q_upper <- toupper(trimws(query))

  yjs_data <- collect_yjs_data(main_conn, q_upper)
  if (!is.null(yjs_data)) {
    result$samples <- rbind(result$samples, yjs_data$sample)
    if (!is.null(yjs_data$strain)) {
      result$strains <- rbind(result$strains, yjs_data$strain)
    }
    return(result)
  }

  redirect <- DBI::dbGetQuery(main_conn,
    "SELECT YJS_NUMBER FROM AltYJS WHERE OLD_YJS_NUMBER = ?",
    params = list(q_upper)
  )
  if (nrow(redirect) > 0) {
    current_yjs <- redirect$YJS_NUMBER[1]
    result$alerts <- c(result$alerts, list(
      paste0("Redirected from old YJS number ", q_upper,
             " \u2192 ", current_yjs)
    ))
    yjs_data <- collect_yjs_data(main_conn, current_yjs)
    if (!is.null(yjs_data)) {
      result$samples <- rbind(result$samples, yjs_data$sample)
      if (!is.null(yjs_data$strain)) {
        result$strains <- rbind(result$strains, yjs_data$strain)
      }
    }
  }

  result
}

# --- Exact Strain search ---
search_strain_exact <- function(main_conn, query, result) {
  q_upper <- toupper(trimws(query))

  strain_data <- collect_strain_data(main_conn, q_upper, "Exact match")
  if (!is.null(strain_data)) {
    result$strains <- rbind(result$strains, strain_data$strain)
    if (nrow(strain_data$samples) > 0) {
      result$samples <- rbind(result$samples, strain_data$samples)
    }
    return(result)
  }

  alt_hit <- DBI::dbGetQuery(main_conn,
    "SELECT STRAIN FROM AltNames WHERE UPPER(ALT_NAME) = ?",
    params = list(q_upper)
  )
  if (nrow(alt_hit) > 0) {
    resolved_strain <- alt_hit$STRAIN[1]
    result$alerts <- c(result$alerts, list(
      paste0("Found via alternative name '", q_upper,
             "' for strain ", resolved_strain)
    ))
    strain_data <- collect_strain_data(
      main_conn, resolved_strain, "Via alt name"
    )
    if (!is.null(strain_data)) {
      result$strains <- rbind(result$strains, strain_data$strain)
      if (nrow(strain_data$samples) > 0) {
        result$samples <- rbind(result$samples, strain_data$samples)
      }
    }
  }

  result
}

# --- Fuzzy search ---
search_fuzzy <- function(main_conn, query, result) {
  norm_query <- normalize_str(query)
  if (nchar(norm_query) == 0) return(result)

  max_results <- 50
  total_count <- 0

  # Fuzzy on YJSnumbers.SAMPLE_NAME
  yjs_all <- DBI::dbGetQuery(main_conn,
    "SELECT YJS_NUMBER, SAMPLE_NAME FROM YJSnumbers
     WHERE SAMPLE_NAME IS NOT NULL AND SAMPLE_NAME != ''"
  )
  if (nrow(yjs_all) > 0) {
    yjs_all$norm <- normalize_str(yjs_all$SAMPLE_NAME)
    hits <- yjs_all[grepl(norm_query, yjs_all$norm, fixed = TRUE),
                    , drop = FALSE]
    for (i in seq_len(min(nrow(hits), max_results - total_count))) {
      yjs_data <- collect_yjs_data(
        main_conn, hits$YJS_NUMBER[i],
        match_source = paste0("Sample name '",
                              hits$SAMPLE_NAME[i], "'")
      )
      if (!is.null(yjs_data)) {
        result$samples <- rbind(result$samples, yjs_data$sample)
        if (!is.null(yjs_data$strain)) {
          if (!yjs_data$strain$STRAIN %in% result$strains$STRAIN) {
            result$strains <- rbind(result$strains, yjs_data$strain)
          }
        }
        total_count <- total_count + 1
      }
    }
  }

  # Fuzzy on Strains.STRAIN
  if (total_count < max_results) {
    strains_all <- DBI::dbGetQuery(main_conn,
      "SELECT STRAIN FROM Strains"
    )
    if (nrow(strains_all) > 0) {
      strains_all$norm <- normalize_str(strains_all$STRAIN)
      hits <- strains_all[grepl(norm_query, strains_all$norm,
                                fixed = TRUE), , drop = FALSE]
      for (i in seq_len(min(nrow(hits),
                            max_results - total_count))) {
        if (!hits$STRAIN[i] %in% result$strains$STRAIN) {
          strain_data <- collect_strain_data(
            main_conn, hits$STRAIN[i],
            paste0("Strain ID '", hits$STRAIN[i], "'")
          )
          if (!is.null(strain_data)) {
            result$strains <- rbind(result$strains,
                                    strain_data$strain)
            new_samples <- strain_data$samples[
              !strain_data$samples$YJS_NUMBER %in%
                result$samples$YJS_NUMBER, , drop = FALSE]
            if (nrow(new_samples) > 0) {
              result$samples <- rbind(result$samples, new_samples)
            }
            total_count <- total_count + 1
          }
        }
      }
    }
  }

  # Fuzzy on AltNames.ALT_NAME
  if (total_count < max_results) {
    alt_all <- DBI::dbGetQuery(main_conn,
      "SELECT STRAIN, ALT_NAME FROM AltNames"
    )
    if (nrow(alt_all) > 0) {
      alt_all$norm <- normalize_str(alt_all$ALT_NAME)
      hits <- alt_all[grepl(norm_query, alt_all$norm, fixed = TRUE),
                      , drop = FALSE]
      for (i in seq_len(nrow(hits))) {
        if (total_count >= max_results) break
        strain_id <- hits$STRAIN[i]
        if (strain_id %in% result$strains$STRAIN) next
        strain_data <- collect_strain_data(
          main_conn, strain_id,
          paste0("Alt name '", hits$ALT_NAME[i], "'")
        )
        if (!is.null(strain_data)) {
          result$strains <- rbind(result$strains,
                                  strain_data$strain)
          new_samples <- strain_data$samples[
            !strain_data$samples$YJS_NUMBER %in%
              result$samples$YJS_NUMBER, , drop = FALSE]
          if (nrow(new_samples) > 0) {
            result$samples <- rbind(result$samples, new_samples)
          }
          total_count <- total_count + 1
        }
      }
    }
  }

  total <- nrow(result$strains) + nrow(result$samples)
  if (total > 0) {
    result$fuzzy_header <- paste0(
      "Fuzzy matches (", nrow(result$strains), " strain",
      if (nrow(result$strains) != 1) "s", ", ",
      nrow(result$samples), " sample",
      if (nrow(result$samples) != 1) "s", ")"
    )
  }

  result
}

# --- Collect data for a single YJS number ---
collect_yjs_data <- function(main_conn, yjs_number,
                             match_source = "Exact match") {
  yjs_row <- DBI::dbGetQuery(main_conn,
    "SELECT y.YJS_NUMBER, y.SAMPLE_NAME, y.SPECIES, y.ID_STRAIN,
            y.SAMPLE_TYPE, y.PLOIDY, y.MATING_TYPE,
            y.STRAINS_GROUP, y.GENOTYPE, y.COLLECTION,
            s.STRAIN, s.SPECIES AS STRAIN_SPECIES, s.CLADE,
            s.ECO_ORIGIN, s.GEO_ORIGIN, s.CONTINENT, s.COUNTRY,
            s.ISOLATION, s.SRR_ID
     FROM YJSnumbers y
     LEFT JOIN Strains s ON y.ID_STRAIN = s.STRAIN
     WHERE y.YJS_NUMBER = ?",
    params = list(yjs_number)
  )
  if (nrow(yjs_row) == 0) return(NULL)

  row <- yjs_row[1, ]

  # Alt YJS numbers
  alt_yjs <- DBI::dbGetQuery(main_conn,
    "SELECT OLD_YJS_NUMBER FROM AltYJS WHERE YJS_NUMBER = ?",
    params = list(yjs_number)
  )
  old_yjs_str <- if (nrow(alt_yjs) > 0) {
    paste(alt_yjs$OLD_YJS_NUMBER, collapse = ", ")
  } else ""

  # Alt names for the linked strain
  alt_names_str <- ""
  strain_id <- row$ID_STRAIN
  if (!is.na(strain_id) && nchar(as.character(strain_id)) > 0) {
    alt_names <- DBI::dbGetQuery(main_conn,
      "SELECT ALT_NAME FROM AltNames WHERE STRAIN = ?",
      params = list(strain_id)
    )
    if (nrow(alt_names) > 0) {
      alt_names_str <- paste(alt_names$ALT_NAME, collapse = ", ")
    }
  }

  # Column order must match empty_result()$samples and JS indices
  sample_df <- data.frame(
    YJS_NUMBER = row$YJS_NUMBER,
    SAMPLE_NAME = na_to_empty(row$SAMPLE_NAME),
    SPECIES = na_to_empty(row$SPECIES),
    ID_STRAIN = na_to_empty(row$ID_STRAIN),
    SAMPLE_TYPE = na_to_empty(row$SAMPLE_TYPE),
    PLOIDY = na_to_empty(row$PLOIDY),
    `Match Source` = match_source,
    MATING_TYPE = na_to_empty(row$MATING_TYPE),
    STRAINS_GROUP = na_to_empty(row$STRAINS_GROUP),
    GENOTYPE = na_to_empty(row$GENOTYPE),
    COLLECTION = na_to_empty(row$COLLECTION),
    OLD_YJS = old_yjs_str,
    STRAIN_SPECIES = na_to_empty(row$STRAIN_SPECIES),
    CLADE = na_to_empty(row$CLADE),
    ECO_ORIGIN = na_to_empty(row$ECO_ORIGIN),
    GEO_ORIGIN = na_to_empty(row$GEO_ORIGIN),
    COUNTRY = na_to_empty(row$COUNTRY),
    ISOLATION = na_to_empty(row$ISOLATION),
    ALT_NAMES = alt_names_str,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  strain_df <- NULL
  if (!is.na(row$STRAIN) && nchar(as.character(row$STRAIN)) > 0) {
    # Linked YJS for strain
    linked_yjs <- DBI::dbGetQuery(main_conn,
      "SELECT YJS_NUMBER FROM YJSnumbers WHERE ID_STRAIN = ?",
      params = list(row$STRAIN)
    )
    linked_str <- if (nrow(linked_yjs) > 0) {
      paste(linked_yjs$YJS_NUMBER, collapse = ", ")
    } else ""

    # Column order must match empty_result()$strains and JS indices
    strain_df <- data.frame(
      STRAIN = row$STRAIN,
      SPECIES = na_to_empty(row$STRAIN_SPECIES),
      CLADE = na_to_empty(row$CLADE),
      ECO_ORIGIN = na_to_empty(row$ECO_ORIGIN),
      COUNTRY = na_to_empty(row$COUNTRY),
      `Match Source` = match_source,
      GEO_ORIGIN = na_to_empty(row$GEO_ORIGIN),
      CONTINENT = na_to_empty(row$CONTINENT),
      ISOLATION = na_to_empty(row$ISOLATION),
      SRR_ID = na_to_empty(row$SRR_ID),
      ALT_NAMES = alt_names_str,
      LINKED_YJS = linked_str,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }

  list(sample = sample_df, strain = strain_df)
}

# --- Collect data for a single Strain ---
collect_strain_data <- function(main_conn, strain_id,
                                match_source = "Exact match") {
  strain_row <- DBI::dbGetQuery(main_conn,
    "SELECT * FROM Strains WHERE STRAIN = ?",
    params = list(strain_id)
  )
  if (nrow(strain_row) == 0) return(NULL)

  sr <- strain_row[1, ]

  alt_names <- DBI::dbGetQuery(main_conn,
    "SELECT ALT_NAME FROM AltNames WHERE STRAIN = ?",
    params = list(strain_id)
  )
  alt_names_str <- if (nrow(alt_names) > 0) {
    paste(alt_names$ALT_NAME, collapse = ", ")
  } else ""

  linked_yjs <- DBI::dbGetQuery(main_conn,
    "SELECT YJS_NUMBER FROM YJSnumbers WHERE ID_STRAIN = ?",
    params = list(strain_id)
  )
  linked_str <- if (nrow(linked_yjs) > 0) {
    paste(linked_yjs$YJS_NUMBER, collapse = ", ")
  } else ""

  strain_df <- data.frame(
    STRAIN = sr$STRAIN,
    SPECIES = na_to_empty(sr$SPECIES),
    CLADE = na_to_empty(sr$CLADE),
    ECO_ORIGIN = na_to_empty(sr$ECO_ORIGIN),
    COUNTRY = na_to_empty(sr$COUNTRY),
    `Match Source` = match_source,
    GEO_ORIGIN = na_to_empty(sr$GEO_ORIGIN),
    CONTINENT = na_to_empty(sr$CONTINENT),
    ISOLATION = na_to_empty(sr$ISOLATION),
    SRR_ID = na_to_empty(sr$SRR_ID),
    ALT_NAMES = alt_names_str,
    LINKED_YJS = linked_str,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  # Linked samples with enriched detail columns
  yjs_rows <- DBI::dbGetQuery(main_conn,
    "SELECT YJS_NUMBER, SAMPLE_NAME, SPECIES, ID_STRAIN,
            SAMPLE_TYPE, PLOIDY, MATING_TYPE,
            STRAINS_GROUP, GENOTYPE, COLLECTION
     FROM YJSnumbers WHERE ID_STRAIN = ?",
    params = list(strain_id)
  )

  if (nrow(yjs_rows) > 0) {
    yjs_rows[is.na(yjs_rows)] <- ""
    yjs_rows$`Match Source` <- paste0("Strain ", strain_id)

    # Add alt YJS for each sample
    yjs_rows$OLD_YJS <- ""
    if (nrow(yjs_rows) > 0) {
      old_yjs_map <- get_old_yjs_map(main_conn, yjs_rows$YJS_NUMBER)
      yjs_rows$OLD_YJS <- unname(
        old_yjs_map[yjs_rows$YJS_NUMBER]
      )
      yjs_rows$OLD_YJS[is.na(yjs_rows$OLD_YJS)] <- ""
    }

    # Add strain detail columns for child row display
    yjs_rows$STRAIN_SPECIES <- na_to_empty(sr$SPECIES)
    yjs_rows$CLADE <- na_to_empty(sr$CLADE)
    yjs_rows$ECO_ORIGIN <- na_to_empty(sr$ECO_ORIGIN)
    yjs_rows$GEO_ORIGIN <- na_to_empty(sr$GEO_ORIGIN)
    yjs_rows$COUNTRY <- na_to_empty(sr$COUNTRY)
    yjs_rows$ISOLATION <- na_to_empty(sr$ISOLATION)
    yjs_rows$ALT_NAMES <- alt_names_str

    # Reorder to match expected column order
    samples_df <- yjs_rows[, c(
      "YJS_NUMBER", "SAMPLE_NAME", "SPECIES", "ID_STRAIN",
      "SAMPLE_TYPE", "PLOIDY", "Match Source",
      "MATING_TYPE", "STRAINS_GROUP", "GENOTYPE", "COLLECTION",
      "OLD_YJS", "STRAIN_SPECIES", "CLADE", "ECO_ORIGIN",
      "GEO_ORIGIN", "COUNTRY", "ISOLATION", "ALT_NAMES"
    ), drop = FALSE]
  } else {
    samples_df <- empty_result("")$samples
  }

  list(strain = strain_df, samples = samples_df)
}

# --- Helpers ---

na_to_empty <- function(x) {
  if (is.null(x) || is.na(x)) "" else as.character(x)
}

get_old_yjs_map <- function(main_conn, yjs_numbers) {
  if (length(yjs_numbers) == 0) return(character(0))
  placeholders <- paste(rep("?", length(yjs_numbers)),
                        collapse = ", ")
  alt_yjs <- DBI::dbGetQuery(main_conn,
    paste0("SELECT YJS_NUMBER, OLD_YJS_NUMBER FROM AltYJS ",
           "WHERE YJS_NUMBER IN (", placeholders, ")"),
    params = as.list(yjs_numbers)
  )
  if (nrow(alt_yjs) > 0) {
    tapply(alt_yjs$OLD_YJS_NUMBER, alt_yjs$YJS_NUMBER,
           paste, collapse = ", ")
  } else {
    stats::setNames(rep("", length(yjs_numbers)), yjs_numbers)
  }
}

add_old_yjs_column <- function(yjs_df, main_conn) {
  if (nrow(yjs_df) == 0) return(yjs_df)
  old_map <- get_old_yjs_map(main_conn, yjs_df$YJS_NUMBER)
  yjs_df$Old_YJS <- unname(old_map[yjs_df$YJS_NUMBER])
  yjs_df$Old_YJS[is.na(yjs_df$Old_YJS)] <- ""
  yjs_df
}
