#' Name Conversion Module
#'
#' Unified search across YJSnumbers, Strains, alt_yjs, and alt_names tables.
#' Exact match for YJS numbers and Strain IDs, fuzzy match for names.
#' Results displayed in tabbed layout with inline expandable detail rows.
#'
#' @param id Module namespace ID
#' @param main_conn DB connection

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

nameconv_server <- function(id, db_conn, user_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    search_data <- reactiveVal(NULL)

    observeEvent(input$search_btn, {
      query_val <- trimws(input$search_input)
      if (nchar(query_val) == 0) {
        search_data(NULL)
        return()
      }
      t0 <- Sys.time()
      result <- collect_search_results(db_conn, query_val)
      message("TOTAL SEARCH TIME: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3), " sec")
      search_data(result)
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

  # Visible: toggle, strain, species, clade, eco_origin, country, Match Source
  # Hidden: geo_origin(7), continent(8), isolation(9), srr_id(10),
  #         alt_names(11), linked_yjs(12)
  hidden_cols <- which(names(df) %in%
    c("geo_origin", "continent", "isolation", "srr_id",
      "alt_names", "linked_yjs")) - 1  # 0-indexed

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

  # Visible: toggle, yjs_number, sample_name, species, id_strain,
  #          sample_type, ploidy, Match Source
  # Hidden: mating_type(8), strains_group(9), genotype(10),
  #         collection(11), old_yjs(12), strain_species(13),
  #         clade(14), eco_origin(15), geo_origin(16),
  #         country(17), isolation(18), alt_names(19)
  hidden_cols <- which(names(df) %in%
    c("mating_type", "strains_group", "genotype", "collection",
      "old_yjs", "strain_species", "clade", "eco_origin",
      "geo_origin", "country", "isolation", "alt_names")) - 1

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
      strain = character(0), species = character(0),
      clade = character(0), eco_origin = character(0),
      country = character(0), `Match Source` = character(0),
      geo_origin = character(0), continent = character(0),
      isolation = character(0), srr_id = character(0),
      alt_names = character(0), linked_yjs = character(0),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    samples = data.frame(
      yjs_number = character(0), sample_name = character(0),
      species = character(0), id_strain = character(0),
      sample_type = character(0), ploidy = character(0),
      `Match Source` = character(0),
      mating_type = character(0), strains_group = character(0),
      genotype = character(0), collection = character(0),
      old_yjs = character(0), strain_species = character(0),
      clade = character(0), eco_origin = character(0),
      geo_origin = character(0), country = character(0),
      isolation = character(0), alt_names = character(0),
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    alerts = list(),
    query = query,
    fuzzy_header = NULL
  )
}

# --- Main search dispatcher (returns structured data) ---
collect_search_results <- function(db_conn, query) {
  input_type <- classify_input(query)
  result <- empty_result(query)

  if (input_type == "yjs") {
    result <- search_yjs_exact(db_conn, query, result)
  } else if (input_type == "strain") {
    result <- search_strain_exact(db_conn, query, result)
  }

  if (nrow(result$strains) == 0 && nrow(result$samples) == 0) {
    result <- search_fuzzy(db_conn, query, result)
  }

  result
}

# --- Exact YJS search ---
search_yjs_exact <- function(db_conn, query, result) {
  q_upper <- toupper(trimws(query))

  yjs_data <- collect_yjs_data(db_conn, q_upper)
  if (!is.null(yjs_data)) {
    result$samples <- rbind(result$samples, yjs_data$sample)
    if (!is.null(yjs_data$strain)) {
      result$strains <- rbind(result$strains, yjs_data$strain)
    }
    return(result)
  }

  redirect <- DBI::dbGetQuery(db_conn,
    "SELECT yjs_number FROM alt_yjs WHERE old_yjs_number = $1",
    params = list(q_upper)
  )
  if (nrow(redirect) > 0) {
    current_yjs <- redirect$yjs_number[1]
    result$alerts <- c(result$alerts, list(
      paste0("Redirected from old YJS number ", q_upper,
             " \u2192 ", current_yjs)
    ))
    yjs_data <- collect_yjs_data(db_conn, current_yjs)
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
search_strain_exact <- function(db_conn, query, result) {
  q_upper <- toupper(trimws(query))

  strain_data <- collect_strain_data(db_conn, q_upper, "Exact match")
  if (!is.null(strain_data)) {
    result$strains <- rbind(result$strains, strain_data$strain)
    if (nrow(strain_data$samples) > 0) {
      result$samples <- rbind(result$samples, strain_data$samples)
    }
    return(result)
  }

  alt_hit <- DBI::dbGetQuery(db_conn,
    "SELECT strain FROM alt_names WHERE UPPER(alt_name) = $1",
    params = list(q_upper)
  )
  if (nrow(alt_hit) > 0) {
    resolved_strain <- alt_hit$strain[1]
    result$alerts <- c(result$alerts, list(
      paste0("Found via alternative name '", q_upper,
             "' for strain ", resolved_strain)
    ))
    strain_data <- collect_strain_data(
      db_conn, resolved_strain, "Via alt name"
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
search_fuzzy <- function(db_conn, query, result) {
  norm_query <- normalize_str(query)
  if (nchar(norm_query) == 0) return(result)

  max_results <- 50
  total_count <- 0

  # Fuzzy on YJSnumbers.sample_name
  yjs_all <- DBI::dbGetQuery(db_conn,
    "SELECT yjs_number, sample_name FROM yjs_numbers
     WHERE sample_name IS NOT NULL AND sample_name != ''"
  )
  if (nrow(yjs_all) > 0) {
    names(yjs_all) <- c("yjs_number", "sample_name")
    yjs_all$norm <- normalize_str(yjs_all$sample_name)
    hits <- yjs_all[grepl(norm_query, yjs_all$norm, fixed = TRUE),
                    , drop = FALSE]
    for (i in seq_len(min(nrow(hits), max_results - total_count))) {
      yjs_data <- collect_yjs_data(
        db_conn, hits$yjs_number[i],
        match_source = paste0("Sample name '",
                              hits$sample_name[i], "'")
      )
      if (!is.null(yjs_data)) {
        result$samples <- rbind(result$samples, yjs_data$sample)
        if (!is.null(yjs_data$strain)) {
          if (!yjs_data$strain$strain %in% result$strains$strain) {
            result$strains <- rbind(result$strains, yjs_data$strain)
          }
        }
        total_count <- total_count + 1
      }
    }
  }

  # Fuzzy on Strains.strain
  if (total_count < max_results) {
    strains_all <- DBI::dbGetQuery(db_conn,
      "SELECT strain FROM strains"
    )
    if (nrow(strains_all) > 0) {
      names(strains_all) <- "strain"
      strains_all$norm <- normalize_str(strains_all$strain)
      hits <- strains_all[grepl(norm_query, strains_all$norm,
                                fixed = TRUE), , drop = FALSE]
      for (i in seq_len(min(nrow(hits),
                            max_results - total_count))) {
        if (!hits$strain[i] %in% result$strains$strain) {
          strain_data <- collect_strain_data(
            db_conn, hits$strain[i],
            paste0("Strain ID '", hits$strain[i], "'")
          )
          if (!is.null(strain_data)) {
            result$strains <- rbind(result$strains,
                                    strain_data$strain)
            new_samples <- strain_data$samples[
              !strain_data$samples$yjs_number %in%
                result$samples$yjs_number, , drop = FALSE]
            if (nrow(new_samples) > 0) {
              result$samples <- rbind(result$samples, new_samples)
            }
            total_count <- total_count + 1
          }
        }
      }
    }
  }

  # Fuzzy on alt_names.alt_name
  if (total_count < max_results) {
    alt_all <- DBI::dbGetQuery(db_conn,
      "SELECT strain, alt_name FROM alt_names"
    )
    if (nrow(alt_all) > 0) {
      alt_all$norm <- normalize_str(alt_all$alt_name)
      hits <- alt_all[grepl(norm_query, alt_all$norm, fixed = TRUE),
                      , drop = FALSE]
      for (i in seq_len(nrow(hits))) {
        if (total_count >= max_results) break
        strain_id <- hits$strain[i]
        if (strain_id %in% result$strains$strain) next
        strain_data <- collect_strain_data(
          db_conn, strain_id,
          paste0("Alt name '", hits$alt_name[i], "'")
        )
        if (!is.null(strain_data)) {
          result$strains <- rbind(result$strains,
                                  strain_data$strain)
          new_samples <- strain_data$samples[
            !strain_data$samples$yjs_number %in%
              result$samples$yjs_number, , drop = FALSE]
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
collect_yjs_data <- function(db_conn, yjs_number,
                             match_source = "Exact match") {
  yjs_row <- DBI::dbGetQuery(db_conn,
    "SELECT y.yjs_number, y.sample_name, y.species AS species, y.id_strain,
            y.sample_type, y.ploidy, y.mating_type,
            y.strains_group, y.genotype, y.collection,
            s.strain, s.species AS strain_species, s.clade,
            s.eco_origin, s.geo_origin, s.continent, s.country,
            s.isolation, s.srr_id
     FROM yjs_numbers y
     LEFT JOIN strains s ON y.id_strain = s.strain
     WHERE y.yjs_number = $1",
    params = list(yjs_number)
  )
  if (nrow(yjs_row) == 0) return(NULL)

  row <- yjs_row[1, ]

  # Alt YJS numbers
  alt_yjs <- DBI::dbGetQuery(db_conn,
    "SELECT old_yjs_number FROM alt_yjs WHERE yjs_number = $1",
    params = list(yjs_number)
  )
  old_yjs_str <- if (nrow(alt_yjs) > 0) {
    paste(alt_yjs$old_yjs_number, collapse = ", ")
  } else ""

  # Alt names for the linked strain
  alt_names_str <- ""
  strain_id <- row$id_strain
  if (!is.na(strain_id) && nchar(as.character(strain_id)) > 0) {
    alt_names <- DBI::dbGetQuery(db_conn,
      "SELECT alt_name FROM alt_names WHERE strain = $1",
      params = list(strain_id)
    )
    if (nrow(alt_names) > 0) {
      alt_names_str <- paste(alt_names$alt_name, collapse = ", ")
    }
  }

  # Column order must match empty_result()$samples and JS indices
  sample_df <- data.frame(
    yjs_number = row$yjs_number,
    sample_name = na_to_empty(row$sample_name),
    species = na_to_empty(row$species),
    id_strain = na_to_empty(row$id_strain),
    sample_type = na_to_empty(row$sample_type),
    ploidy = na_to_empty(row$ploidy),
    `Match Source` = match_source,
    mating_type = na_to_empty(row$mating_type),
    strains_group = na_to_empty(row$strains_group),
    genotype = na_to_empty(row$genotype),
    collection = na_to_empty(row$collection),
    old_yjs = old_yjs_str,
    strain_species = na_to_empty(row$strain_species),
    clade = na_to_empty(row$clade),
    eco_origin = na_to_empty(row$eco_origin),
    geo_origin = na_to_empty(row$geo_origin),
    country = na_to_empty(row$country),
    isolation = na_to_empty(row$isolation),
    alt_names = alt_names_str,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  strain_df <- NULL
  if (!is.na(row$strain) && nchar(as.character(row$strain)) > 0) {
    # Linked YJS for strain
    linked_yjs <- DBI::dbGetQuery(db_conn,
      "SELECT yjs_number FROM yjs_numbers WHERE id_strain = $1",
      params = list(row$strain)
    )
    linked_str <- if (nrow(linked_yjs) > 0) {
      paste(linked_yjs$yjs_number, collapse = ", ")
    } else ""

    # Column order must match empty_result()$strains and JS indices
    strain_df <- data.frame(
      strain = row$strain,
      species = na_to_empty(row$strain_species),
      clade = na_to_empty(row$clade),
      eco_origin = na_to_empty(row$eco_origin),
      country = na_to_empty(row$country),
      `Match Source` = match_source,
      geo_origin = na_to_empty(row$geo_origin),
      continent = na_to_empty(row$continent),
      isolation = na_to_empty(row$isolation),
      srr_id = na_to_empty(row$srr_id),
      alt_names = alt_names_str,
      linked_yjs = linked_str,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }

  list(sample = sample_df, strain = strain_df)
}

# --- Collect data for a single Strain ---
collect_strain_data <- function(db_conn, strain_id,
                                match_source = "Exact match") {
  strain_row <- DBI::dbGetQuery(db_conn,
    "SELECT strain, isolation, eco_origin,
            geo_origin, continent, country,
            clade, srr_id, species
     FROM strains WHERE strain = $1",
    params = list(strain_id)
  )
  if (nrow(strain_row) == 0) return(NULL)

  sr <- strain_row[1, ]

  alt_names <- DBI::dbGetQuery(db_conn,
    "SELECT alt_name FROM alt_names WHERE strain = $1",
    params = list(strain_id)
  )
  alt_names_str <- if (nrow(alt_names) > 0) {
    paste(alt_names$alt_name, collapse = ", ")
  } else ""

  linked_yjs <- DBI::dbGetQuery(db_conn,
    "SELECT yjs_number FROM yjs_numbers WHERE id_strain = $1",
    params = list(strain_id)
  )
  linked_str <- if (nrow(linked_yjs) > 0) {
    paste(linked_yjs$yjs_number, collapse = ", ")
  } else ""

  strain_df <- data.frame(
    strain = sr$strain,
    species = na_to_empty(sr$species),
    clade = na_to_empty(sr$clade),
    eco_origin = na_to_empty(sr$eco_origin),
    country = na_to_empty(sr$country),
    `Match Source` = match_source,
    geo_origin = na_to_empty(sr$geo_origin),
    continent = na_to_empty(sr$continent),
    isolation = na_to_empty(sr$isolation),
    srr_id = na_to_empty(sr$srr_id),
    alt_names = alt_names_str,
    linked_yjs = linked_str,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  # Linked samples with enriched detail columns
  yjs_rows <- DBI::dbGetQuery(db_conn,
    "SELECT yjs_number, sample_name, species, id_strain,
            sample_type, ploidy, mating_type,
            strains_group, genotype, collection
     FROM yjs_numbers WHERE id_strain = $1",
    params = list(strain_id)
  )

  if (nrow(yjs_rows) > 0) {
    yjs_rows[is.na(yjs_rows)] <- ""
    yjs_rows$`Match Source` <- paste0("Strain ", strain_id)

    # Add alt YJS for each sample
    yjs_rows$old_yjs <- ""
    if (nrow(yjs_rows) > 0) {
      old_yjs_map <- get_old_yjs_map(db_conn, yjs_rows$yjs_number)
      yjs_rows$old_yjs <- unname(
        old_yjs_map[yjs_rows$yjs_number]
      )
      yjs_rows$old_yjs[is.na(yjs_rows$old_yjs)] <- ""
    }

    # Add strain detail columns for child row display
    yjs_rows$strain_species <- na_to_empty(sr$species)
    yjs_rows$clade <- na_to_empty(sr$clade)
    yjs_rows$eco_origin <- na_to_empty(sr$eco_origin)
    yjs_rows$geo_origin <- na_to_empty(sr$geo_origin)
    yjs_rows$country <- na_to_empty(sr$country)
    yjs_rows$isolation <- na_to_empty(sr$isolation)
    yjs_rows$alt_names <- alt_names_str

    # Reorder to match expected column order
    samples_df <- yjs_rows[, c(
      "yjs_number", "sample_name", "species", "id_strain",
      "sample_type", "ploidy", "Match Source",
      "mating_type", "strains_group", "genotype", "collection",
      "old_yjs", "strain_species", "clade", "eco_origin",
      "geo_origin", "country", "isolation", "alt_names"
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

get_old_yjs_map <- function(db_conn, yjs_numbers) {
  if (length(yjs_numbers) == 0) return(character(0))
  placeholders <- paste0("$", seq_along(yjs_numbers), collapse = ", ")
  alt_yjs <- DBI::dbGetQuery(db_conn,
    paste0("SELECT yjs_number, old_yjs_number FROM alt_yjs ",
           "WHERE yjs_number IN (", placeholders, ")"),
    params = as.list(yjs_numbers)
  )
  if (nrow(alt_yjs) > 0) {
    tapply(alt_yjs$old_yjs_number, alt_yjs$yjs_number,
           paste, collapse = ", ")
  } else {
    stats::setNames(rep("", length(yjs_numbers)), yjs_numbers)
  }
}

add_old_yjs_column <- function(yjs_df, db_conn) {
  if (nrow(yjs_df) == 0) return(yjs_df)
  old_map <- get_old_yjs_map(db_conn, yjs_df$yjs_number)
  yjs_df$old_yjs <- unname(old_map[yjs_df$yjs_number])
  yjs_df$old_yjs[is.na(yjs_df$old_yjs)] <- ""
  yjs_df
}
